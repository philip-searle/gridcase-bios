
INT13_AT	PROGRAM	OutFile=build/int13_at.obj

		include	"macros.inc"
		include	"segments/bda.inc"
		include	"segments/ivt.inc"
		include	"cmos.inc"
		include	"hdc_at.inc"
		include	"int13.inc"
		include	"pic.inc"

		EXTERN	kBdaSegment
		EXTERN	ReadCmos, WriteCmos
		EXTERN	SetCriticalErr
		EXTERN	FixedDiskParams
		EXTERN	ConString_Inline

		PUBLIC	HdAtInit
		PUBLIC	HdAtInt13
		PUBLIC	HdAtDecideError
		PUBLIC	HdAtWaitTask
		PUBLIC	HdAtMakeRegPack
		PUBLIC	HdAtSendRegPack
		PUBLIC	HdAtOpComplete

; Constant used to locate the interrupt vector table.
; The int13 code has its own copy of this constant, probably because it was
; written seperately from the main BIOS codebase before being merged in.
kIvtSegment2	dw	0

; ===========================================================================
; GetCmosFdsPtr
; Returns a pointer to the fixed disk parameter table for a given drive.
;
; On entry:
;   AH = CMOS register number for extended hard drive type
;   AL = hard drive type in lower nibble
; On return:
;   CS:SI -> fixed disk parameter table entry
;   ZF set if no drive type set
; ===========================================================================
GetCmosFdsPtr	PROC
		and	al, 0Fh		; mask off drive type nibble
		jz	.leaveFunction
		cmp	al, 0Fh		; extended drive type needed?
		jnz	.haveDriveType	; skip it if not

		mov_	al, ah		; get extended drive type from CMOS
		call	ReadCmos

		; Special-case two drive types:
		; type 2 skips the range check (not sure why)
		; type E0 has a hardcoded parameter table entry that lives
		; outside the normal 47 entries (again, unsure why)
		cmp	al, 02h
		jz	.checkRange
		cmp	al, 0E0h
		jnz	.checkRange
		mov	si, FDS_E0	; special entry for type E0
		jmp	.haveFdsPtr
		FillerNop

.checkRange	cmp	al, 47
		ja	.diskConfigErr

.haveDriveType	xor_	ah, ah
		shl	ax, 4		; multiply by sizeof(FDS_AT_INSTANCE)
		jz	.diskConfigErr
		mov_	bp, ax		; need indexed addressing mode
		; index into table offset by one entry because the drive type
		; number is one-based (zero is used for "no drive").
		lea	si, [bp+(FixedDiskParams - SIZE#FDS_E0)]

.haveFdsPtr	cmp	[cs:si], 0,DATA=WORD	; cylinder count == 0 for unused table entries
		jz	.diskConfigErr

.leaveFunction	retn

.diskConfigErr	Inline	ConString,'Hard disk configuration error',0Dh,0Ah,0
		call	SetCriticalErr
		xor_	al, al
		retn
		ENDPROC	GetCmosFdsPtr

; ===========================================================================
; Special fixed disk parameter table entry for drive type E0
; ===========================================================================
FDS_E0		DS FIXED_DISK_PARMS_AT,	\
		.cylinders=771,		\
		.heads=8,		\
		.writePreComp=0,	\
		.control=0,		\
		.landingZone=0,		\
		.sectorsPerTrk=33

; ===========================================================================
; HdAtInit
; Initializes the int13 hard disk system for an AT-compatible controller.
; ===========================================================================
HdAtInit	PROC
		; Determine hard disk type and get drive parameter table pointer
		; BL contains the diagnostic byte throughout this proc, updated as the init proceeds
		mov	al, CMOS_STATUS_DIAG | NMI_DISABLE
		call	ReadCmos
		mov_	bl, al
		test	al, 0C0h		; invalid config or checksum bad?
		jnz	.noHdConfigured

		mov	al, CMOS_HD_TYPE | NMI_DISABLE
		call	ReadCmos
		mov_	bh, al
		shr	al, 4			; isolate drive C nibble
		mov	ah, CMOS_HD_EXTTYPE1 | NMI_DISABLE
		call	GetCmosFdsPtr
		jnz	.haveFdsPtr

.noHdConfigured	jmp	.leaveFunction

.haveFdsPtr	; Install interrupt handlers
		cli
		mov	ds, [cs:kIvtSegment2]
		mov	ax, [IvtInt13]		; relocate original int13 handler to int40
		mov	[IvtInt40], ax
		mov	ax, [IvtInt13+2]
		mov	[IvtInt40+2], ax

		mov	[IvtInt13], HdAtInt13,DATA=WORD		; install int13 handler
		mov	[IvtInt13+2], cs
		mov	[IvtInt76], HdAtOpComplete,DATA=WORD	; install disk controller callback handler
		mov	[IvtInt76+2], cs
		mov	[IvtHd0Parms+2], cs		; set segment for hd0 parameter table (offset set later)
		mov	[IvtHd1Parms], FixedDiskParams,DATA=WORD	; set hd1 param table to dummy value
		mov	[IvtHd1Parms+2], cs

		; Initialize BDA fields
		mov	es, [cs:kBdaSegment]
		mov	[es:HdLastOpStatus], 0
		mov	[es:HdCount], 1
		mov	[es:HdControl], 0
		mov	[es:HdIoOffset], 0

		; Set offset for hd0 parameters (from GetCmosFdsPtr call above)
		mov	[IvtHd0Parms], si

		; Determine whether we have a second hard disk and set fields accordingly
		mov_	al, bh
		and	al, 0Fh			; isolate drive D nibble
		mov	ah, CMOS_HD_EXTTYPE2 | NMI_DISABLE
		call	GetCmosFdsPtr
		jz	.checkedDrive2
		mov	[IvtHd1Parms], si
		inc	[es:HdCount]

.checkedDrive2	or	bl, 8		; set "drive C or controller failed initialization" bit
					; we'll clear it once we know we've init'd OK

		; Run disk controller internal diagnostics
		mov	dl, 80h		; first hard disk
		mov	ah, 14h		; fixed disk diagnostics
		int	13h
		jnb	.hdcDiagOk
		Inline	ConString,'Hard disk controller failure',0Dh,0Ah,0
		call	SetCriticalErr
		jmp	.updateCmosDiag

.hdcDiagOk	; Loop over all hard disks and intialize them
		mov	dl, 0

.initDriveLoop	cmp	dl, [es:HdCount]	; run out of disk drives?
		jnb	.updateCmosDiag		; exit loop if so

		add	dl, 80h			; convert to int13 drive number

		mov	ax, [es:SoftResetFlag]
		and	al, ~CRITICAL_ERR_FLAG	; mask off 'critical error' flag
		cmp	ax, SOFT_RESET_FLAG	; soft reset?
		jz	.initDriveType		; skip waiting for drive to become ready if so

		mov	cx, 0			; loop maximum number of times waiting for drive to become ready
.testDriveReady	mov	ah, 10h			; check if drive is ready
		int	13h
		jnb	.initDriveType		; if successful, move on
		cmp	[es:TimerTicks], 30*18,DATA=WORD	; have we been waiting more than 30secs?
		ja	.diskFailure		; declare it a lost cause if so
		loop	.testDriveReady		; otherwise keep trying
		jmps	.diskFailure		; 64K attempts is a failure as well

.initDriveType	mov	ah, 9			; init controller from drive params
		int	13h
		jb	.diskFailure

		mov	cx, 10h			; only recalibrate 16 times, it should succeed within that
.calibrateDrive	mov	ah, 11h
		int	13h
		jnb	.calibrationOk
		mov	ah, 0			; reset disk system on calibration fail and try again
		int	13h
		loop	.calibrateDrive
		jmps	.diskFailure
		FillerNop

.calibrationOk	mov_	bh, dl			; save drive number, next int13 destroys DL
		mov	ah, 8			; get current drive parameters
		int	13h
		mov_	dl, bh			; restore drive number
		mov	al, 1			; verify one sector
		mov	ah, 4			; verify sectors
		int	13h			; head, sector, and cylinder (DH/CL/CH) from previous int13h call
		jnb	.driveOk
		cmp	ah, INT13STAT_BADSECTOR	; errors below this are fatal
		jb	.diskFailure
		cmp	ah, INT13STAT_DATACORRECTED
		jbe	.driveOk		; errors up to this one are bad sector data but track OK
						; errors above this are fatal

.diskFailure	Inline	ConString,'Hard disk failure',0Dh,0Ah,0
		call	SetCriticalErr
		jmp	.nextDrive

.driveOk	and	bl, 0F7h		; clear init failed bit

.nextDrive	sub	dl, 7Fh			; return to drive index, incremented by one
		jmp	.initDriveLoop

.updateCmosDiag	; Once all drives have been initialized (or failed) we update
		; the CMOS diagnostic byte to reflect the HD init status.
		mov	ah, CMOS_STATUS_DIAG | NMI_ENABLE
		mov_	al, bl
		call	WriteCmos

.leaveFunction	sti
		retn
		ENDPROC	HdAtInit

; ===========================================================================
; HdAtOpComplete
; Interrupt handler called by hard disk controller when it has processed a
; request and wants us to give it some attention.  Sets the HdTaskComplete
; flag in the BDA and clears the interrupt before calling the int15 OS hook.
; ===========================================================================
HdAtOpComplete	PROC
		push	ds
		mov	ds, [cs:kBdaSegment]
		mov	[HdTaskComplete], 0FFh	; flag task as complete
		pop	ds

		push	ax
		mov	al, 20h			; end-of-interrupt
		out	PORT_PIC2_CTRL, al
		out	PORT_PIC1_CTRL, al

		mov	ax, 9100h		; OS hook - disk busy
		int	15h
		pop	ax

		iret
		ENDPROC	HdAtOpComplete

; ===========================================================================
; HdAtInt13
; Provides int13 hard disk services using an AT-compatible disk controller.
; ===========================================================================
HdAtInt13	PROC

		sti			; need interrupts for hard disk controller
		cmp	dl, 80h		; 80h is first hard disk drive
		jnb	.notFloppyDrive
		int	40h		; send to redirected floppy disk int13h handler
		retf	2		; retf instead of iret because int40 call needs the
					; flags preserved for its return value

.notFloppyDrive	push	ds

		; Handle the "last status" command specially up-front, since it
		; is the only command that unconditionally clears the last status
		; after returning its value.
		cmp	ah, INT13CMD_LASTSTATUS
		jnz	.notStatusCmd
		mov	ds, [cs:kBdaSegment]
		; [Compat] We return the last operation's status in AL, not AH
		;          as RBIL documents most BIOSes to do.  In AH we instead
		;          return the status of this operation which is, of course,
		;          unconditionally successful.
		mov	al, [HdLastOpStatus]
		mov	ah, INT13STAT_OK
		mov	[HdLastOpStatus], ah	; clear last op status
		pop	ds
		retf	2			; retf instead of iret to preserve flags

.notStatusCmd	; Prepare to dispatch to command handler
		pusha			; preserve all registers we may change
		push	es
		pushf
		cld			; ensure string ops increment
		sub	dl, 80h		; convert int13 drive number to 0-based index
		mov	ds, [cs:kBdaSegment]

		; Clear last operation status, we'll update it later
		mov	[HdLastOpStatus], INT13STAT_OK

		; Range check and dispatch through jump table
		cmp	ah, (.int13HandlersEnd - .int13Handlers) / 2
		jnb	.invalidCommand
		mov_	bp, ax
		shr	bp, 7		; effectively same as BP=AH*2
		and	bp, 0FEh	; ensure value is even
		cmp	dl, [HdCount]	; several command handlers want to compare
					; against HdCount so do it here and they
					; can all reuse the flags
		jmp	[cs:.int13Handlers+bp]


; Jump table for Int13 command handlers.
; Note that some command handlers expect to be entered with BP equal
; to their jump table offset so their shared code can perform different
; actions depending on how it was called.  This is not nice!
; The macro here generates constants so we don't have to keep track of
; which command handler is at what offset manually.
JumpTableEntry	%macro
		; Suppress "unused symbol" warning for tableOffset label
		; We normally only use a couple of them
		EUROASM	PUSH, NOWARN=2101
%1_tableOffset	equ	$ - .int13Handlers
		dw	%1
		EUROASM	POP
		%endmacro

.int13Handlers	JumpTableEntry	HdAtReset
		dw		.invalidCommand
		JumpTableEntry	HdAtRead
		JumpTableEntry	HdAtWrite
		JumpTableEntry	HdAtVerify
		JumpTableEntry	HdAtFormat
		dw		.invalidCommand
		dw		.invalidCommand
		JumpTableEntry	HdAtDiskParms
		JumpTableEntry	HdAtInitPair
		JumpTableEntry	HdAtReadLong
		JumpTableEntry	HdAtWriteLong
		JumpTableEntry	HdAtSeek
		JumpTableEntry	HdAtReset2
		dw		.invalidCommand
		dw		.invalidCommand
		JumpTableEntry	HdAtTestReady
		JumpTableEntry	HdAtCalibrate
		dw		.invalidCommand
		dw		.invalidCommand
		JumpTableEntry	HdAtDiagnostic
		JumpTableEntry	HdAtReadDasd
.int13HandlersEnd

.invalidCommand	mov	ah, INT13STAT_BADCMD
		; fall-through into HdAtCmdDone
		ENDPROC	HdAtInt13

; ===========================================================================
; HdAtCmdDone
; Shared function tail for HdAtInt13 and most Int13 command handlers.
; Updates HdLastOpStatus (if it's not already an error code) and cleans up
; the stack frame setup by HdAtInt13.
;
; On return:
; 	AH == HdLastOpStatus
;	CF set on error
; ===========================================================================
HdAtCmdDone	PROC
		sti			; command handler may have disabled interrupts
		cmp	[HdLastOpStatus], INT13STAT_OK
		jnz	.L1
		mov	[HdLastOpStatus], ah

.L1		popf
		pop	es
		popa

		mov	ah, 0
		cmp	ah, [HdLastOpStatus]	; set CF on error
		mov	ah, [HdLastOpStatus]

		pop	ds
		retf	2			; retf instead of iret to preserve flags
		ENDPROC	HdAtCmdDone

; ===========================================================================
; HdAtReadDasd [TechRef 10-9]
; Int13 command handler that returns details about direct attached storage
; devices (DASD -- disk drives).  Unlike other int13 command handlers, this
; has a non-standard return: AH is not a status/error code.
;
; On return;
; 	AH == 00 if drive not present
;	AH == 03 if hard drive present
;	         CX:DX == number of 512 byte blocks drive supports
; ===========================================================================
HdAtReadDasd	PROC
		jnb	.badDrive	; reuse flags from caller
		call	GetIvtFdsPtr
		mov	al, [es:si+FIXED_DISK_PARMS_AT.sectorsPerTrk]
		mul	[es:si+FIXED_DISK_PARMS_AT.heads]
		mov	dx, [es:si+FIXED_DISK_PARMS_AT.cylinders]
		dec	dx
		mul	dx

		mov_	bp, sp
		mov	[ss:bp+Int13HdAtStack.cx], dx
		mov	[ss:bp+Int13HdAtStack.dx], ax
		mov	[ss:bp+Int13HdAtStack.ax], (DASD_HD << 8) + 00h

		popf
		pop	es
		popa
		pop	ds
		retf	2

.badDrive	popf
		pop	es
		popa
		mov	ax, 0	; drive not present
		mov_	cx, ax
		and_	dx, ax
		pop	ds
		retf	2
		ENDPROC	HdAtReadDasd

; ===========================================================================
; HdAtReset2 [TechRef 10-8], [TechRef 10-9]
; Variant of HdAtReset that does not reset the floppy disk subsystem.
; ===========================================================================
HdAtReset2	PROC
		; Reuse flags from caller
		jb	HdAtReset.performReset

		; If drive number not valid, return an error
		mov	ah, INT13STAT_BADCMD
		jmp	HdAtCmdDone
		ENDPROC	HdAtReset2

; ===========================================================================
; HdAtReset [TechRef 10-8], [TechRef 10-9]
; Resets the disk drive subsystem.
; ===========================================================================
HdAtReset	PROC
		; Call the relocated int13 handler so it can reset floppy subsystem
		int	40h

		; Is this a reset request for a valid drive?
		cmp	dl, [HdCount]
		jnb	.resetDone

.performReset	; Alternate function entrypoint used by HdAtReset2

		; Place controller in reset until next command is issued
		mov	al, 4		; set reset bit in digital output register
		mov	dx, PORT_HD_AT_DIGOUT
		out	dx, al

		; Reset all drives
		mov	dl, 0
.resetDriveLoop	cmp	dl, [HdCount]	; run out of drives?
		jnb	.resetSuccess

		add	dl, 80h		; convert index to int13 drive ID

		; simulate int13 with a near call (AX is pushed where flags
		; would normally go... OK since int13 handler never pops
		; the flags and always ends with retf 2)
		mov	ah, INT13CMD_INITPAIR
		push	ax
		push	cs
		calln	HdAtInt13

		; Another simulated int13 to recalibrate
		mov	ah, INT13CMD_RECALIBRATE
		push	ax
		push	cs
		calln	HdAtInt13

		; Restore DL to a 0-based index, incremented for the next iteration
		sub	dl, 7Fh
		jmp	.resetDriveLoop


.resetSuccess	mov	ah, INT13STAT_OK
		mov	[HdLastOpStatus], ah
.resetDone	jmp	HdAtCmdDone
		ENDPROC	HdAtReset

; ===========================================================================
; HdAtReadLong [TechRef 10-7]
; As HdAtRead but additionally reads four bytes of ECC data per sector.
; ===========================================================================
HdAtReadLong	PROC
		mov	ah, 22h		; [TechRef 10-17] Read sectors (long)
		jmp	HdAtRead.performRead
		ENDPROC	HdAtReadLong

; ===========================================================================
; HdAtRead [TechRef 10-6]
; Reads sectors from disk.
; ===========================================================================
HdAtRead		PROC
		mov	ah, 20h		; [TechRef 10-17] Read sectors

.performRead	; Alternate function entrypoint used by HdAtReadLong

		call	ValidHdXferBuf
		jb	.readDone
		call	HdAtMakeRegPack
		jb	.readDone
		jz	.L1		; do we want retries disabled?
		or	[HdAtRegPacket+HD_AT_REG_PACK.command], 1,DATA=BYTE
.L1		call	HdAtSendRegPack
		jb	.readDone

.copySector	; Wait for the disk controller to have data for us and
		; copy it all into the user's buffer
		call	HdAtWaitTask
		jb	.readDone
		call	HdAtWaitData
		jb	.readDone
		mov	dx, PORT_HD_AT_DATA
		mov	cx, HD_AT_DATA_SECTOR_SIZE / 2
		rep insw

		; Was this a long read?  Copy ECC bytes if so.
		; They must be transferred individually in 8-bit transfers.
		cmp	bp, HdAtReadLong_tableOffset
		jnz	.readDataDone
		mov	cx, HD_AT_DATA_ECC_SIZE
.readEccBytes	call	HdAtWaitData
		jb	.readDone
		mov	dx, PORT_HD_AT_DATA
		insb
		loop	.readEccBytes

.readDataDone	call	HdAtDecideError
		jb	.readDone
		call	DecSectorCount
		jb	.readDone
		jnz	.copySector		; go again if we've got multiple sectors to transfer
		mov	ah, INT13STAT_OK	; otherwise declare success and move on

.readDone	jmp	HdAtCmdDone
		ENDPROC	HdAtRead

; ===========================================================================
; HdAtWriteLong [TechRef 10-8]
; As HdAtWrite but additionally writes four bytes of ECC data per sector.
; ===========================================================================
HdAtWriteLong	PROC
		mov	ah, 32h		; [TechRef 10-17] Write sectors (long)
		jmp	HdAtWrite.performWrite
		ENDPROC	HdAtWriteLong

; ===========================================================================
; HdAtWrite [TechRef 10-6]
; Writes sectors to disk.
; ===========================================================================
HdAtWrite	PROC
		mov	ah, 30h		; [TechRef 10-17] Write sectors

.performWrite	; Alternate function entrypoint used by HdAtWriteLong

		call	ValidHdXferBuf
		jb	.writeDone
		call	HdAtMakeRegPack
		jb	.writeDone
		jz	.L1		; Do we want retries disabled?
		or	[HdAtRegPacket+HD_AT_REG_PACK.command], 1,DATA=BYTE
.L1		call	HdAtSendRegPack
		jb	.writeDone

.copySector	; Wait for the disk controller to be ready to accept data
		; and then send the sector data from the user's buffer.
		call	HdAtWaitData
		jb	.writeDone
		mov	dx, PORT_HD_AT_DATA
		mov	cx, HD_AT_DATA_SECTOR_SIZE / 2
		push	ds
		mov	si, es		; need ES:SI for outsw to work
		mov	ds, si
		mov_	si, di
		rep outsw
		pop	ds

		; Was this a long write?  Copy ECC bytes if so.
		; Must be done individually in 8-bit transfers.
		cmp	bp, HdAtWriteLong_tableOffset
		jnz	.writeDataDone
		mov	cx, HD_AT_DATA_ECC_SIZE
.writeEccBytes	call	HdAtWaitData
		jb	.writeDone
		mov	dx, PORT_HD_AT_DATA
		outs	dx, [es:si],DATA=BYTE
		loop	.writeEccBytes

.writeDataDone	mov_	di, si		; update sector pointer
		call	HdAtWaitTask
		jb	.writeDone
		call	HdAtDecideError
		jb	.writeDone
		call	DecSectorCount
		jb	.writeDone
		jnz	.copySector		; go again if it's a multi-sector write
		mov	ah, INT13STAT_OK	; otherwise declare success and continue

.writeDone	jmp	HdAtCmdDone
		ENDPROC	HdAtWrite

; ===========================================================================
; HdAtVerify [TechRef 10-6]
; Verifies that disk sectors are valid
; ===========================================================================
HdAtVerify	PROC
		mov	ah, 40h		; [TechRef 10-17] Read verify sectors
		call	HdAtMakeRegPack
		jb	.verifyDone
		jz	.L1		; do we want retries disabled?
		or	[HdAtRegPacket+HD_AT_REG_PACK.command], 1,DATA=BYTE
.L1		call	HdAtSendRegPack
		jb	.verifyDone
		call	HdAtWaitTask
		jb	.verifyDone
		call	HdAtDecideError
		jb	.verifyDone

		; If we made it through all that then we succeeded
		mov	ah, INT13STAT_OK

.verifyDone	jmp	HdAtCmdDone
		ENDPROC	HdAtVerify

; ===========================================================================
; HdAtFormat [TechRef 10-7]
; Formats a single track
; ===========================================================================
HdAtFormat	PROC
		call	ValidHdXferBuf

		; Force number of sectors to match the disk parameter table
		push	es
		call	GetIvtFdsPtr
		mov	al, [es:si+FIXED_DISK_PARMS_AT.sectorsPerTrk]
		pop	es

		; Sector number isn't used by the disk controller for this
		; command but we force it to one anyway
		and	cl, 0C0h	; preserve upper two bits of track number
		inc	cl		; force sector number to one

		mov	ah, 50h		; [TechRef 10-17] Format track
		call	HdAtMakeRegPack
		jb	.formatDone
		call	HdAtSendRegPack
		jb	.formatDone

		; Wait for disk controller to be ready for the sector buffer
		; (which contains sector IDs and good/bad/assign-spare status,
		; see [TechRef 10-24] for details).
		call	HdAtWaitData
		jb	.formatDone
		mov	cx, HD_AT_DATA_SECTOR_SIZE / 2
		push	ds
		mov	si, es		; need DS:SI for outsw to work
		mov	ds, si
		mov_	si, di
		mov	dx, PORT_HD_AT_DATA
		rep outsw
		pop	ds

		call	HdAtWaitTask
		jb	.formatDone
		call	HdAtDecideError

.formatDone	jmp	HdAtCmdDone
		ENDPROC	HdAtFormat

; ===========================================================================
; HdAtDiskParms [TechRef 10-7]
; Returns disk drive parameters from the current fixed disk parameter table.
; ===========================================================================
HdAtDiskParms	PROC
		mov_	bp, sp		; access parent stack frame
		cmp	dl, 2
		jb	.validDrive	; first two hard disks?

		; Invalid drive number, return empty result
		mov	[ss:bp+Int13HdAtStack.dx], 0,DATA=WORD
		mov	[ss:bp+Int13HdAtStack.cx], 0,DATA=WORD
		mov	[ss:bp+Int13HdAtStack.ax], 0,DATA=BYTE
		mov	ah, INT13STAT_BADPARAMTBL
		jmp	HdAtCmdDone

.validDrive	call	GetIvtFdsPtr

		mov	dh, [es:si+FIXED_DISK_PARMS_AT.heads]
		dec	dh
		mov	dl, [HdCount]
		mov	[ss:bp+Int13HdAtStack.dx], dx

		mov	cx, [es:si+FIXED_DISK_PARMS_AT.cylinders]
		dec	cx
		dec	cx
		xchg	ch, cl
		shl	cl, 6
		add	cl, [es:si+FIXED_DISK_PARMS_AT.sectorsPerTrk]
		mov	[ss:bp+Int13HdAtStack.cx], cx

		mov	[ss:bp+Int13HdAtStack.ax], 0

		popf
		pop	es
		popa
		pop	ds
		retf	2
		ENDPROC	HdAtDiskParms

; ===========================================================================
; HdAtInitPair [TechRef 10-7], [TechRef 10-10]
; Initializes the disk controller using the contents of the current fixed
; disk parameter table
; ===========================================================================
HdAtInitPair	PROC
		jb	.validDrive	; reuse caller's flags
		mov	ah, INT13STAT_BADCMD
		jmp	HdAtCmdDone

.validDrive	call	GetIvtFdsPtr
		mov	dh, [es:si+FIXED_DISK_PARMS_AT.heads]
		dec	dh
		mov	al, [es:si+FIXED_DISK_PARMS_AT.sectorsPerTrk]
		mov	ah, 91h		; [TechRef 10-26] Initialise drive parameters
		call	HdAtMakeRegPack
		jb	.initDone
		call	HdAtSendRegPack
		jb	.initDone
		call	HdAtWaitTask
		jb	.initDone
		call	HdAtDecideError

.initDone	jmp	HdAtCmdDone
		ENDPROC	HdAtInitPair

; ===========================================================================
; HdAtCalibrate [TechRef 10-8]
; Seeks to track 0
; ===========================================================================
HdAtCalibrate	PROC
		mov	ah, 10h		; [TechRef 10-21] Recalibrate
		jmp	HdAtSeek.performSeek
		ENDPROC	HdAtCalibrate

; ===========================================================================
; HdAtSeek [TechRef 10-8]
; Seeks to any track
; ===========================================================================
HdAtSeek	PROC
		mov	ah, 70h		; [TechRef 10-25] Seek

.performSeek	; Alternate function entrypoint used by HdAtCalibrate

		; Force sector number to one (it's not used) but
		; preserve the upper two bits of the cylinder number
		and	cl, 0C0h
		inc	cl

		call	HdAtMakeRegPack
		jb	.seekDone
		call	HdAtSendRegPack
		jb	.seekDone
		call	HdAtWaitTask
		jb	.seekDone
		call	HdAtDecideError

.seekDone	jmp	HdAtCmdDone
		ENDPROC	HdAtSeek

; ===========================================================================
; HdAtTestReady [TechRef 10-8]
; Checks whether a drive is ready for use
; ===========================================================================
HdAtTestReady	PROC
		mov	ah, 0		; Not a valid controller command, will be ignored
		call	HdAtMakeRegPack
		jb	.testReadyDone
		call	HdAtSendRegPack
		jb	.testReadyDone

		; If we made it through that without any errors then drive must be ready
		mov	ah, INT13STAT_OK

.testReadyDone	jmp	HdAtCmdDone
		ENDPROC	HdAtTestReady

; ===========================================================================
; HdAtDiagnostic [TechRef 10-8]
; Triggers disk controller internal diagnostics
; ===========================================================================
HdAtDiagnostic	PROC
		jb	.validDrive	; reuse caller's flags
		mov	ah, INT13STAT_BADCMD
		jmp	HdAtCmdDone

.validDrive	; Disable interrupts while we trigger the diagnostics.
		; The controller will signal an interrupt as part of it's
		; internal tests which we want to ignore.
		cli
		mov	al, 90h		; [TechRef 10-26] Execute drive diagnostics
		mov	dx, PORT_HD_AT_COMMAND
		out	dx, al

		; Delay while the disk controller starts it's diagnostics.
		; This short delay is enough for the controller to set the BSY
		; bit in it's status register while the tests are run.
		; Eventually it will clear the BSY flag and signal an interrupt
		; (which we ignore).
		Delay	4

		; Read controller status register.  If it's not busy, then
		; assume it's broken since diagnostics shouldn't complete so fast.
		in	al, dx
		mov	[HdStatus], al
		test	al, HD_AT_STATUS_BSY
		mov	ah, INT13STAT_BADCONTROLLER
		jb	.diagDone		; BUG? Should be jz since test doesn't set CF?

		; Wait for controller to complete diagnostics
		call	HdAtWaitTask
		jb	.diagDone

		; Reread status register to find out result.
		in	al, dx
		mov	[HdStatus], al
		mov	ah, INT13STAT_OK

		test	al, HD_AT_STATUS_ERR
		jz	.diagDone		; Success if ERR clear

		; If ERR flag was set then read the error register.
		; After diagnostics it doesn't have the usual bitfield,
		; it's just a result byte:
		;   01 = OK, 03 = Sector buffer error, 8x = reserved
		mov	dx, PORT_HD_AT_ERROR
		in	al, dx
		mov	[HdError], al
		mov	ah, INT13STAT_BADCONTROLLER
		stc

.diagDone	jmp	HdAtCmdDone
		ENDPROC	HdAtDiagnostic

; ===========================================================================
; GetIvtFdsPtr
; Loads the fixed disk parameters table pointer from IVT entries 41h and 46h.
;
; On entry:
; 	DL == hard disk drive index (0/1)
;
; On return:
; 	ES:SI -> fixed disk parameter table entry
; ===========================================================================
GetIvtFdsPtr	PROC
		mov	es, [cs:kIvtSegment2]
		mov	al, IvtHd1Parms - IvtHd0Parms
		mul	dl
		mov_	si, ax
		les	si, [es:si+IvtHd0Parms]
		retn
		ENDPROC	GetIvtFdsPtr

; ===========================================================================
; ValidHdXferBuf
;
; On entry:
; 	ES:BX -> disk transfer buffer start
; 	AL == count of sectors
;
; On return:
; 	ES_DI -> normalized disk transfer buffer start
; 	AH = error code
; 	       zero if all OK
; 	       non-zero error code if:
; 	         sector count > 80h
; 	         or buffer would cross a 64K DMA boundary
;
; ===========================================================================
ValidHdXferBuf	PROC
		push	bx	; normalize ES:BX into ES:DI
		mov	di, es
		shr	bx, 4
		add_	di, bx
		mov	es, di
		pop	di
		; EuroASM consistently misassembles this:
		;   |81 E7 0F 00| and di, 0Fh,DATA=WORD
		; as this:
		;   |83 E7 0F| add di, 0Fh
		; not sure why, so I've just hand assembled it for now...
		db	81h, 0E7h, 0Fh, 00h

		push	ax	; save registers we'll use
		push	dx

		; Ugly code ahead!  We want to calculate the buffer size
		; required for our disk operation.  Normally this would be:
		;   buf_size = sector_size * sector_count
		;            = 512 * AL
		;
		; BUT, int13 operations 09h and 0Ah (read long/write long)
		; require an extra four bytes per sector for the ECC codes.
		; We account for this by adding on:
		;   (BP & 10h) >> 2
		;
		; This works because BP contains the int13 AH service code
		; multiplied by two (see comment by the int13 jump table).
		; The only int13 services >= 08h are the "read/write long"
		; services.
		;
		; (actually not true because "read/write drive parameters"
		; are >= 08h but they use ES:BX as a pointer, not a transfer
		; buffer and so do not call this method and it all Just Works).
		mov	ah, 0
		mov_	dx, bp

		; EuroASM consistently misassembles this:
		;   |81 E2 10 00| and dx, 10h,DATA=WORD
		; as this:
		;   |83 E2 10| add dx, 10h
		; not sure why, so I've just hand assembled it for now...
		db	81h, 0E2h, 10h, 00h

		shr	dx, 2
		add	dx, HD_AT_DATA_SECTOR_SIZE

		; AX is now count of sectors
		; DX is now sector transfer size
		mul	dx

		; DX:AX is now total transfer size.
		; DX will be 0000h if AL was < 80h (CF clear in this case).
		; DX will be 0001h if AL was >= 80h (CF is set in this ccase).
		; dec will set ZF if DX was 0001, clear it otherwise.
		; dec does not affect CF which remains set by the mul.
		dec	dx

		jnz	.L1	; skip extra checks if AL was < 80h

		; If DX was 0001 (sector count >= 80h) then add the transfer
		; buffer base and see if it pushes us over the 64K DMA limit
		or_	ax, di
		cmp_	dx, ax

.L1		; Flags are now set appropriately for function return

		pop	dx	; restore modified registers
		pop	ax

		jnb	.done	; set error code if calculated size was over 64K
		mov	ah, INT13STAT_DMABOUNDARY

.done		retn
		ENDPROC	ValidHdXferBuf

; ===========================================================================
; HdAtWaitTask
; Waits for the disk controller to signal an interrupt by checking the
; HdTaskComplete flag in the BDA.
;
; On return:
; 	CF set on timeout
; 	CF clear on interrupt before timeout
; ===========================================================================
HdAtWaitTask	PROC
		mov	[HdTaskComplete], 0

		in	al, PORT_PIC2_MASK
		and	al, ~IRQ_HDC		; unmask fixed disk interrupt
		Delay	1
		out	PORT_PIC2_MASK, al

		in	al, PORT_PIC1_MASK
		and	al, ~IRQ_CASCADE	; unmask slave PIC IRQ
		Delay	1
		out	PORT_PIC1_MASK, al

		sti				; ready for disk controller interrupt

		; OS hook BIOS callback
		mov	ax, 9000h
		int	15h
		mov	ah, 80h			; ???
		jb	.leave1			; OS callback didn't want us to wait?

		mov	cx, 100			; outer timeout loop
.outerLoop	push	cx
		mov	cx, 0			; maximum wait for inner loop
.innerLoop	cmp	[HdTaskComplete], 0FFh
		jnb	.leave2
		loop	.innerLoop
		pop	cx
		loop	.outerLoop

.leave1		retn
.leave2		pop	cx
		retn
		ENDPROC	HdAtWaitTask

; ===========================================================================
; HdAtMakeRegPack
; Converts a sector address from int13 input format to a block of memory
; suitable for sending to the AT hard disk controller registers.  Register
; values are stored in ascending order by port number.
;
; On entry:
; 	AH == disk controller command [TechRef 10-17]
; 	AL == count of sectors
; 	CH == low eight bits of cylinder number
; 	CL == sector number 1-63 (bits 0-5)
; 	      upper two bits of cylinder (bits 6-7)
; 	DH == head number
; 	DL == drive number (only lowest bit used)
;
; On successful returnL
; 	CF clear
; 	ZF set if retry-on-disk-error wanted
; 	BH == count of sectors
; 	Register packet assembled at BDA:HdAtRegPacket for SendAtRegPack to use
;
; On failure:
; 	CF set
; 	AH == int13 error code
; ===========================================================================
HdAtMakeRegPack	PROC
		push	es

		mov	bx, HdAtRegPacket
		mov	[bx+HD_AT_REG_PACK.command], ah
		mov	[bx+HD_AT_REG_PACK.sectorCount], al

		call	GetIvtFdsPtr
		mov_	al, cl
		and	al, 3Fh
		mov	[bx+HD_AT_REG_PACK.sectorNumber], al

		shr	cl, 6
		xchg	ch, cl
		mov	[bx+HD_AT_REG_PACK.cylinderLo], cx

		cmp	dl, [HdCount]
		jb	.validDrive		; odd to check this so late...

		; Invalid drive number, report error
		mov	ah, INT13STAT_BADCMD
		stc
		pop	es
		retn

.validDrive	shl	dl, 4
		and	dh, 0Fh
		add_	dl, dh
		or	dl, 0A0h		; ??? sets two reserved bits
		mov	[bx+HD_AT_REG_PACK.driveHead], dl

		mov	ax, [es:si+FIXED_DISK_PARMS_AT.writePreComp]
		shr	ax, 2			; ??? why shift here
		mov	[bx+HD_AT_REG_PACK.writePrecomp], al

		mov	al, [es:si+FIXED_DISK_PARMS_AT.control]
		mov	[HdControl], al
		test	al, 0C0h		; check 'disable retries on errors' bits
						; do not change ZF flag after this instruction, we need to return it!
		mov	bh, [bx+HD_AT_REG_PACK.sectorCount]

		clc
		pop	es
		retn
		ENDPROC	HdAtMakeRegPack

; ===========================================================================
; HdAtSendRegPack
; Sends the register packet previously assembled by HdAtMakeRegPack to the AT
; hard disk controller.
;
; On return:
; 	CF set on error, AH = error code
; ===========================================================================
HdAtSendRegPack	PROC
		mov	al, [HdControl]
		mov	dx, PORT_HD_AT_DIGOUT
		out	dx, al			; ensure disk controller is out of reset

		mov	bl, 2			; retry seek twice if error occurs
.attemptSeek	call	HdAtWaitNotBusy
		jb	.returnBad

		; Start loading HDC registers from the packet (skip the padding byte at the start)
		lea	si, [HdAtRegPacket+HD_AT_REG_PACK.writePrecomp]
		mov	dx, PORT_HD_AT_WRPRECOMP
		mov	cx, 6
.writeRegister	lodsb
		out	dx, al
		inc	dx
		loop	.writeRegister

		mov	ah, 6			; wait for 6x0FFFFh loops for seek to complete
.waitSeek	in	al, dx			; dx == PORT_HD_AT_STATUS
		mov	[HdStatus], al
		and	al, HD_AT_STATUS_DSC | HD_AT_STATUS_DWF | HD_AT_STATUS_DRDY
		cmp	al, HD_AT_STATUS_DSC | HD_AT_STATUS_DRDY
		jz	.seekComplete
		loop	.waitSeek
		dec	ah
		jnz	.waitSeek
		jmp	.seekFailed

.seekComplete	; Drive is positioned over the correct cylinder, now issue the command
		lodsb
		cmp	al, 0
		jz	.returnOk		; command==0 meant the seek was what we really wanted
		out	dx, al			; dx == PORT_HD_AT_COMMAND

		; Clear the interrupt flag.  The disk controller will issue
		; an interrupt when the command is complete but we want to
		; delay responding to it until after we've returned.
		; BUG?  Should this be before the command register is written
		; in case the controller responds very quickly?
		cli

.returnOk	clc
		retn

.seekFailed	dec	bl			; try again?
		jnz	.attemptSeek
		mov	ah, INT13STAT_NOTREADY
		stc
.returnBad	retn
		ENDPROC	HdAtSendRegPack

; ===========================================================================
; DecSectorCount
; Decrements the sector count in BH, checking for hard disk controller errors
; along the way.
;
; On return:
; 	BH decremented
; 	CF set on controller error, AH is error code
; 	CF clear on success, ZF set according to new BH value
; ===========================================================================
DecSectorCount	PROC
		mov	dx, PORT_HD_AT_STATUS
		in	al, dx
		mov	[HdStatus], al

		; Different behaviour depending on drive being busy/waiting on data transfer.
		; TODO: figure this out...
		test	al, HD_AT_STATUS_DRQ | HD_AT_STATUS_BSY
		jnz	.bsyOrDrq

		dec	bh
		jz	.leaveFunction
		jmp	.checkError

.bsyOrDrq	dec	bh
		jnz	.leaveFunction

.checkError	call	HdAtDecideError
		jb	.leaveFunction
		mov	ah, INT13STAT_BADCONTROLLER
		stc

.leaveFunction	retn
		ENDPROC	DecSectorCount

; ===========================================================================
; HdAtWaitNotBusy
; Waits for the AT hard disk controller to not be busy, updating the
; controller status byte in the BDA.
;
; On return:
; 	CF clear on success
; 	AH = int13 error code, CF set if controller still busy after timeout
; ===========================================================================
HdAtWaitNotBusy	PROC
		mov	ah, 40		; 40x0FFFFh timeout loops
		mov	cx, 0
		mov	dx, PORT_HD_AT_STATUS
.checkNotBusy	in	al, dx
		mov	[HdStatus], al
		test	al, HD_AT_STATUS_BSY
		jz	.done
		loop	.checkNotBusy
		sub	ah, 1
		jnb	.checkNotBusy

		; Still busy after all those loops means an error
		mov	ah, INT13STAT_BADCONTROLLER

.done		retn
		ENDPROC	HdAtWaitNotBusy

; ===========================================================================
; HdAtWaitData
; Waits for the AT hard disk controller to report that it is ready for a data
; transfer to take place.
;
; On return:
; 	CF clear on success
; 	CF set on failure, AH = error code
; 	Interrupts disabled
; ===========================================================================
HdAtWaitData	PROC
		push	cx
		mov	dx, PORT_HD_AT_STATUS
		mov	cx, 200			; 200 loops before we declare failure
.waitForData	in	al, dx
		mov	[HdStatus], al
		test	al, HD_AT_STATUS_DRQ	; ready for data?
		loope	.waitForData

		jnz	.done
		call	HdAtDecideError		; if timed out then find out why
		jb	.done
		mov	ah, INT13STAT_BADCONTROLLER
		stc

.done		pop	cx
		cli
		retn
		ENDPROC	HdAtWaitData

; ===========================================================================
; HdAtDecideError
; Examines the AT hard disk controller and updates the status and error bytes
; in the BDA.
;
; On return:
; 	if error determined:
; 	  CF set
; 	  AH == BDA HD error byte
; 	otherwise:
; 	  CF clear
; 	  AH = BDA HD error byte if error, zero otherwise
; ===========================================================================
HdAtDecideError	PROC
		; Default error code if we can't find a more specific one
		mov	ah, INT13STAT_BADCONTROLLER

		; Update BDA hard disk status
		mov	dx, PORT_HD_AT_STATUS
		in	al, dx
		mov	[HdStatus], al

		; Busy or waiting for data transfer, no more specific error
		test	al, HD_AT_STATUS_BSY | HD_AT_STATUS_DRQ
		jnz	.returnOk

		; Previous command didn't report an error?
		test	al, HD_AT_STATUS_ERR
		jz	.prevCmdOk

		; Update BDA hard disk error code
		mov	dx, PORT_HD_AT_ERROR
		in	al, dx
		mov	[HdError], al

		; Bad block mark in sector ID?
		mov	ah, INT13STAT_BADSECTOR
		test	al, HD_AT_ERROR_BBK
		jnz	.returnError

		; Sector not found?
		mov	ah, INT13STAT_NOSECTOR
		test	al, HD_AT_ERROR_IDNF
		jnz	.returnError

		; Data address mark not found?
		mov	ah, INT13STAT_BADADDRMARK
		test	al, HD_AT_ERROR_DAMNF
		jnz	.returnError

		; Uncorrectable data error?
		mov	ah, INT13STAT_BADECC
		test	al, HD_AT_ERROR_UNC
		jnz	.returnError

		; Track 0 not found?
		mov	ah, INT13STAT_RESETFAIL
		test	al, HD_AT_ERROR_TK0
		jnz	.returnError

		; Command completed OK?  Undefined error, if so
		mov	ah, INT13STAT_UNDEFINED
		test	al, HD_AT_ERROR_ABRT
		jz	.returnError

		; Command did not complete successfully so examine the
		; status register to figure out why
		mov	dx, PORT_HD_AT_STATUS
		in	al, dx
		mov	[HdStatus], al

		; Drive not ready?
		mov	ah, INT13STAT_NOTREADY
		test	al, HD_AT_STATUS_DRDY
		jz	.returnError

		; Drive write fault?
		mov	ah, INT13STAT_WRITEFAULT
		test	al, HD_AT_STATUS_DWF
		jnz	.returnError

		; Seek not complete?
		mov	ah, INT13STAT_BADSEEK
		test	al, HD_AT_STATUS_DSC
		jz	.returnError

		; If we make it here we have no idea what the error was!
		mov	ah, INT13STAT_UNDEFINED
		jmp	.returnError

.prevCmdOk	; ECC corrected data?
		test	al, HD_AT_STATUS_CORR
		jz	.prevCmdSuccess
		mov	ah, INT13STAT_DATACORRECTED

.returnError	stc
		retn

.prevCmdSuccess	mov	ah, INT13STAT_OK

.returnOk	retn
		ENDPROC	HdAtDecideError

ENDPROGRAM	INT13_AT
