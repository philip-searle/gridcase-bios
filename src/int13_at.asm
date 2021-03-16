
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

		mov	al, ah,CODE=LONG; get extended drive type from CMOS
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
		nop			; assembler-inserted nop

.checkRange	cmp	al, 47
		ja	.diskConfigErr

.haveDriveType	xor	ah, ah,CODE=LONG
		shl	ax, 4		; multiply by sizeof(FDS_INSTANCE)
		jz	.diskConfigErr
		mov	bp, ax,CODE=LONG	; need indexed addressing mode
		; index into table offset by one entry because the drive type
		; number is one-based (zero is used for "no drive").
		lea	si, [bp+(FixedDiskParams - SIZE#FDS_E0)]

.haveFdsPtr	cmp	[cs:si], 0,DATA=WORD	; cylinder count == 0 for unused table entries
		jz	.diskConfigErr

.leaveFunction	retn

.diskConfigErr	Inline	WriteString,'Hard disk configuration error',0Dh,0Ah,0
		call	SetSoftResetFlag
		xor	al, al,CODE=LONG
		retn
		ENDPROC	GetCmosFdsPtr

; ===========================================================================
; Special fixed disk parameter table entry for drive type E0
; ===========================================================================
FDS_E0		DS FIXED_DISK_PARMS,	\
		.cylinders=771,		\
		.heads=8,		\
		.writePreComp=0,	\
		.control=0,		\
		.landingZone=0,		\
		.sectorsPerTrk=33

; ===========================================================================
; HdcAtInit
; Initializes the int13 hard disk system for an AT-compatible controller.
; ===========================================================================
HdcAtInit	PROC
		; Determine hard disk type and get drive parameter table pointer
		; BL contains the diagnostic byte throughout this proc, updated as the init proceeds
		mov	al, CMOS_STATUS_DIAG | NMI_DISABLE
		call	ReadCmos
		mov	bl, al,CODE=LONG
		test	al, 0C0h		; invalid config or checksum bad?
		jnz	.noHdConfigured

		mov	al, CMOS_HD_TYPE | NMI_DISABLE
		call	ReadCmos
		mov	bh, al,CODE=LONG
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

		mov	[IvtInt13], Int13At,DATA=WORD		; install int13 handler
		mov	[IvtInt13+2], cs
		mov	[IvtInt76], HdcOpComplete,DATA=WORD	; install disk controller callback handler
		mov	[IvtInt76+2], cs
		mov	[IvtHd0Parms+2], cs		; set segment for hd0 parameter table (offset set later)
		mov	[IvtHd1Parms], FixedDiskParams,DATA=WORD	; set hd1 param table to dummy value
		mov	[IvtHd1Parms+2], cs

		; Initialize BDA fields
		mov	es, [cs:kBdaSegment]
		mov	[es:LastHdOpStatus], 0
		mov	[es:HdCount], 1
		mov	[es:HdControl], 0
		mov	[es:HdIoOffset], 0

		; Set offset for hd0 parameters (from GetCmosFdsPtr call above)
		mov	[IvtHd0Parms], si

		; Determine whether we have a second hard disk and set fields accordingly
		mov	al, bh,CODE=LONG
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
		Inline	WriteString,'Hard disk controller failure',0Dh,0Ah,0
		call	SetSoftResetFlag
		jmp	.updateCmosDiag

.hdcDiagOk	; Loop over all hard disks and intialize them
		mov	dl, 0

.initDriveLoop	cmp	dl, [es:HdCount]	; run out of disk drives?
		jnb	.updateCmosDiag		; exit loop if so

		add	dl, 80h			; convert to int13 drive number

		SEGES	mov	ax, [es:SoftResetFlag]
		and	al, 0FEh		; mask off 'important' flag
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
		nop				; assembler-inserted nop

.calibrationOk	mov	bh, dl,CODE=LONG	; save drive number, next int13 destroys DL
		mov	ah, 8			; get current drive parameters
		int	13h
		mov	dl, bh,CODE=LONG	; restore drive number
		mov	al, 1			; verify one sector
		mov	ah, 4			; verify sectors
		int	13h			; head, sector, and cylinder (DH/CL/CH) from previous int13h call
		jnb	.driveOk
		cmp	ah, INT13STAT_BADSECTOR	; errors below this are fatal
		jb	.diskFailure
		cmp	ah, INT13STAT_DATACORRECTED
		jbe	.driveOk		; errors up to this one are bad sector data but track OK
						; errors above this are fatal

.diskFailure	Inline	WriteString,'Hard disk failure',0Dh,0Ah,0
		call	SetSoftResetFlag
		jmp	.nextDrive

.driveOk	and	bl, 0F7h		; clear init failed bit

.nextDrive	sub	dl, 7Fh			; return to drive index, incremented by one
		jmp	.initDriveLoop

.updateCmosDiag	; Once all drives have been initialized (or failed) we update
		; the CMOS diagnostic byte to reflect the HD init status.
		mov	ah, CMOS_STATUS_DIAG | NMI_ENABLE
		mov	al, bl,CODE=LONG
		call	WriteCmos

.leaveFunction	sti
		retn
		ENDPROC	HdcAtInit

; ===========================================================================
; HdcOpComplete
; Interrupt handler called by hard disk controller when it has processed a
; request and wants us to give it some attention.  Sets the HdcTaskComplete
; flag in the BDA and clears the interrupt before calling the int15 OS hook.
; ===========================================================================
HdcOpComplete	PROC
		push	ds
		mov	ds, [cs:kBdaSegment]
		mov	[HdcTaskComplete], 0FFh	; flag task as complete
		pop	ds

		push	ax
		mov	al, 20h			; end-of-interrupt
		out	PORT_PIC2_CTRL, al
		out	PORT_PIC1_CTRL, al

		mov	ax, 9100h		; OS hook - disk busy
		int	15h
		pop	ax

		iret
		ENDPROC	HdcOpComplete

; ===========================================================================
; Int13At
; Provides int13 hard disk services using an AT-compatible disk controller.
; ===========================================================================
Int13At		PROC
		ENDPROC	Int13At

