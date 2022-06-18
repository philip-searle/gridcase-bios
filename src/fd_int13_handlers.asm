
FD_INT13_HANDLE	PROGRAM	OutFile=build/fd_int13_handlers.obj

		include	"macros.inc"
		include	"segments/bda.inc"
		include	"cmos.inc"
		include	"fdc.inc"
		include	"int13.inc"
		include	"isr.inc"

		EXTERN	ReadCmos
		EXTERN	FdGetDriveType, FdGetDriveType2
		EXTERN	FdGetDriveInfo, FdUpdateDrvInfo
		EXTERN	FdCountDrives
		EXTERN	FdDiskChange
		EXTERN	FdGetMediaState, FdGetMediaState2
		EXTERN	FdSetMediaState, FdSetMediaState2
		EXTERN	FdResetMediaState
		EXTERN	FdUpdateMotor
		EXTERN	FdTryNextMediaType
		EXTERN	FdSyncDtr, FdSetupDma
		EXTERN	FdWriteByte, FdWriteByte2
		EXTERN	FdWriteHdUs, FdLoadDpt
		EXTERN	FdWriteDptBytes, FdWriteDptBytes2
		EXTERN	FdHeadDelay, FdWaitInt, FdConvResult
		EXTERN	FdCompatSeek, FdResetSelect
		EXTERN	Dpb525_360K, Dpb525_12M, Dpb35_14M
		EXTERN	UnmakeIsrStack
		EXTERN	sub_FACA1

		PUBLIC	FdChangeLine
		PUBLIC	FdDasdType
		PUBLIC	FdDiskParms
		PUBLIC	FdFormat
		PUBLIC	FdPerformOp
		PUBLIC	FdReset
		PUBLIC	FdSetDasdType
		PUBLIC	FdSetMediaType
		PUBLIC	FdStatus
		PUBLIC	FdUnsupported

; Different gap lengths when writing to disks, depending on sectors-per-track
WRITE_GAP_8SPT	equ	2Ah
WRITE_GAP_9SPT	equ	1Bh

; ---------------------------------------------------------------------
; FdDasdType [TechRef 9-7]
; Returns the type of drive attached to the computer.
; Expects to be jumped into from Int13Fd_Actual.
; ---------------------------------------------------------------------
FdDasdType	PROC
		; Reuse flags from caller to validate drive index
		jc	FdChangeLine

		; Get drive type from CMOS into AL
		push	ax
		call	FdGetDriveType

		; Do we know about the drives already?
		test	[FdDriveInfo], FDDI_DRIVES_OK
		jz	.drivesUnknown

		; Get floppy media state into AH
		mov_	bl, dl		; move drive index to BX
		xor_	bh, bh
		call	FdGetMediaState2

		; 250K data transfer rate implies 5.25" drive
		cmp	ah, FDMS_DTR_250K
		jnz	.not250KDTR
		cmp	al, FD_5_1M	; any 5.25" drive?
		jmp	.check525Drive

.drivesUnknown	cmp	al, FD_5_360K	; DD 5.25" drive?

.check525Drive	; Drive type maps directly to DASD_* constants
		; for 5.25" drives
		jbe	.leaveFunction
		xor_	al, al		; map to DASD_NO_DRIVE
		jmp	.leaveFunction

.not250KDTR	; 720KB 3.5" drive has no change line
		cmp	al, FD_3_720K
		jz	.haveFloppy

		; No media state means drive not present?
		or_	ah, ah
		mov	al, DASD_NO_DRIVE
		jz	.leaveFunction
		; Check for low-density disk in low-density drive
		and	ah, FDMS_LAST_ACCESS
		cmp	ah, FDMS_LA_K360M_360D

.haveFloppy	mov	al, DASD_FLOPPY
		jz	.leaveFunction
		mov	al, DASD_FLOPPY_CL

.leaveFunction	mov_	cl, al
		pop	ax
		mov_	ah, cl
		clc
		jmp	FdRet
		ENDPROC	FdDasdType

; ---------------------------------------------------------------------
; FdChangeLine [TechRef 9-7]
; Returns the disk change line status.  The change line is reset, so a
; disk change will only be detected once.
; Expects to be jumped into from Int13Fd_Actual.
; ---------------------------------------------------------------------
FdChangeLine	PROC
		; Reuse caller's flags to validate drive index
		jc	FdSetDasdType

		; Do we know the drive types already?
		test	[FdDriveInfo], FDDI_DRIVES_OK
		jnz	.drivesKnown

		; If we don't know about the drive, try to find out
		call	FdGetDriveType

		; no drive present?
		mov	[FdScratch.returnAh], FD_CL_ERROR
		jz	.leaveFunction

		; drive present but CMOS config known to be bad?
		mov	al, CMOS_STATUS_DIAG | NMI_ENABLE
		call	ReadCmos
		sti
		test	al, 0C0h	; power fail or checksum bad?
		jnz	.leaveFunction

.changeLineOn	; Change line either known to be set or drive does not
		; support change line detection.  Report a disk change event.
		mov	[FdScratch.returnAh], FD_CL_CHANGED

.leaveFunction	; Floppy disk routines below reference this so they can
		; use short jumps despite being >128 bytes from the
		; sharedRetAh label
		jmp	FdRetAh

.drivesKnown	; If the drive doesn't support change line detection,
		; then assume that the disk has changed.
		mov	bl, [bp+IsrStackAx.dx]
		xor_	bh, bh
		call	FdGetDriveInfo
		test	al, FDDI_CHANGELINE
		nop			; why nop twice?
		nop
		jz	.changeLineOn

		; Drive supports change line detection.  Turn on the
		; motor to ensure we get accurate change detection.
		call	FdUpdateMotor
		mov	dx, PORT_FDC_DIGIN
		in	al, dx
		test	al, FDC_DIGIN_CHANGELINE
		mov	[FdScratch.returnAh], FD_CL_UNCHANGED
		jz	.gotChangeLine

		; Change line detected a disk change, clear any cached state
		call	FdGetMediaState
		and	al, ~ FDMS_KNOWN_MEDIA
		inc	al		; adjust up so bit testing works
		test	al, 4		; media in 'known' range?
		jz	.L1
		sub	al, 3		; move media from 'known' to 'trying'
.L1		dec	al		; adjust back down
		call	FdSetMediaState

		mov	[FdScratch.returnAh], FD_CL_CHANGED

.gotChangeLine	jmp	FdRetMotor
		ENDPROC	FdChangeLine

; ---------------------------------------------------------------------
; FdSetDasdType [TechRef 9-7]
; int13h / AH=17h
; Expects to be jumped into from Int13Fd_Actual.
; ---------------------------------------------------------------------
FdSetDasdType	PROC
		; Reuse caller's flags to validate drive index
		jc	FdUnsupported2

		or_	al, al		; max sectors supplied?
		jz	FdUnsupported2

		; Assume the best and set a successful return code
		mov	[FdScratch.returnAh], 0

		; Drives known?
		test	[FdDriveInfo], FDDI_DRIVES_OK
		jz	FdChangeLine.leaveFunction

		; Maintain disk motor state
		push	ax
		call	FdUpdateMotor
		pop	ax

		; Convert AL value (01-04) to media state byte

		; Try 360K disk in 360K drive
		dec	al
		mov	ah, FDMS_DTR_250K | FDMS_KNOWN_MEDIA | FDMS_LA_K360M_360D
		jz	.gotMediaState

		; Try 360K disk in 1.2M drive
		dec	al
		mov	ah, FDMS_DTR_300K | FDMS_DOUBLE_STEP | FDMS_KNOWN_MEDIA | FDMS_LA_K360M_12D
		jz	.checkMedia

		; Try 1.2M disk in 1.2M drive
		dec	al
		mov	ah, FDMS_DTR_500K | FDMS_KNOWN_MEDIA | FDMS_LA_K12M_12D
		jz	.checkMedia

		; Try 1.44M disk in 1.44M drive
		dec	al
		mov	ah, FDMS_DTR_250K | FDMS_KNOWN_MEDIA | FDMS_LA_35
		jz	.gotMediaState

		; Other AL values not valid
		jmp	FdUnsupported2

.checkMedia	; Access the disk to verify media type
		push	ax
		call	FdDiskChange
		pop	ax
		jnc	.gotMediaState
		mov	ah, FDMS_DTR_250K | FDMS_LA_35

.gotMediaState	; Store new media state
		mov	bl, [bp+IsrStackAx.dx]
		xor_	bh, bh
		call	FdSetMediaState2
		jmp	FdRetMotor
		ENDPROC	FdSetDasdType

; ---------------------------------------------------------------------
; FdUnsupported2
; Shared thunk to FdUnsupported to allow short jmp in surrounding procs
; ---------------------------------------------------------------------
FdUnsupported2	PROC
		jmp	FdUnsupported
		ENDPROC	FdUnsupported2

; Constant used by FdPerformOp for the success AH return code
kFdZero		d b	0

; ---------------------------------------------------------------------
; FdPerformOp
; Shared handler for read/write/verify operations
; Expects to be jumped into from Int13Fd_Actual.
; ---------------------------------------------------------------------
FdPerformOp	PROC
		; Reuse callers flags to validate drive index
		jc	FdUnsupported2

		; Maintain disk motor state and pre-set return code
		call	FdUpdateMotor
		mov	[FdScratch.returnAh], 0

.tryPerformOp	; Retry operation until we get the right media type
		call	FdTryNextMediaType
		jc	.opComplete

		call	sub_FACA1		; ???
		jc	.opComplete

		; Start setting up registers for disk transfer
		call	FdSyncDtr
		call	FdSetupDma
		jc	.opComplete

		; Seek to desired track and wait for heads to settle
		mov	ch, [bp+IsrStackAx.ch]
		call	FdCompatSeek
		jc	.opComplete
		call	FdHeadDelay

		; Determine correct command to send to FDC
		cmp	[bp+IsrStackAx.ah], INT13CMD_WRITE
		mov	al, FDC_CMD_WRITE | FDC_CMD_MT | FDC_CMD_MF
		jz	.gotFdcCmd
		mov	al, FDC_CMD_READ | FDC_CMD_MT | FDC_CMD_MF | FDC_CMD_SK
.gotFdcCmd	call	FdWriteByte
		jc	.opComplete

		; Write head/unit select byte, cylinder number,
		; head number, and sector ID from input parameters
		call	FdWriteHdUs
		mov	al, [bp+IsrStackAx.ch]
		call	FdWriteByte2		; cylinder
		mov	al, [bp+IsrStackAx.dh]
		call	FdWriteByte2		; head
		mov	al, [bp+IsrStackAx.cl]
		call	FdWriteByte2		; sector
		jc	.opComplete

		; Write sector length and end-of-track sector ID from DPT
		mov	bx, DISKETTE_PARMS.sectorSize
		call	FdWriteDptBytes2
		jc	.opComplete

		; Determine gap length from media state byte
		mov	bl, [bp+IsrStackAx.dl]
		xor_	bh, bh
		push	ax
		call	FdGetMediaState
		mov_	bl, al
		pop	ax

		test	bl, FDMS_DATA_TRANSFER_RATE
		mov	al, WRITE_GAP_9SPT
		jz	.gotWriteGap
		mov	al, WRITE_GAP_8SPT
.gotWriteGap	call	FdWriteByte		; write gap
		jc	.opComplete

		; Write data length (DTL, ignored)
		mov	bx, DISKETTE_PARMS.dtl
		mov	cx, 1
		call	FdWriteDptBytes
		jc	.opComplete

		; Previous write completed the command and triggered the FDC
		; to start processing it.  Wait for the FDC to signal that it
		; has completed the command before continuing.
		call	FdWaitInt
		jc	.opComplete

		; Convert result from FDC and, if not successful, try again
		; with a different media type (if possible).
		call	FdConvResult
		call	FdUpdateDrvInfo
		jc	.tryPerformOp

		; Successfully completed the command!  Shift the actual
		; number of processed sectors into CL so we can calculate
		; the number of sectors processed
		mov	cl, [bp+IsrStackAx.cl]

.opComplete	; Operation complete (possibly unsuccessfully).  Re-enable
		; interrupts and set AL to count of sectors.
		sti
		mov	al, [FdScratch.fdcSector]
		cmp	al, 1
		jnz	.L1		; why special-case single sector count?
		call	FdLoadDpt
		mov	al, [es:si+DISKETTE_PARMS.lastSectorId]
		inc	al
.L1		sub_	al, cl		; ??? is this right

		; fallthrough to FdRetMotor
		ENDPROC	FdPerformOp

; ---------------------------------------------------------------------
; FdRetMotor
; Shared function tail for floppy disk int13 handlers that need to
; update the drive motor state before returning.  Also takes care of
; resetting the intE communication bit.
; ---------------------------------------------------------------------
FdRetMotor	PROC
		; Extend disk motor timeout to the minimum if needed
		call	FdLoadDpt
		mov	ah, [es:si+DISKETTE_PARMS.motorWaitTime]
		cmp	[FdMotorTimeout], FD_MIN_MOTOR_TIMEOUT
		jbe	.timeoutSet
		add	ah, [FdMotorTimeout]
		sub	ah, FD_MIN_MOTOR_TIMEOUT
.timeoutSet	mov	[FdMotorTimeout], ah
		and	[FdCalibStatus], ~ FDCAL_INT
		; fallthrough to FdRetAh
		ENDPROC	FdRetMotor


; ---------------------------------------------------------------------
; Shared function tail for floppy disk int13 handlers that need to set
; the AH return value from the FdScratch space before unmaking the
; stack setup by Int13_Actual.
; ---------------------------------------------------------------------
FdRetAh		PROC
		; set return value and flags
		mov	ah, [FdScratch.returnAh]
		cmp	[cs:kFdZero], ah
		; fallthrough to FdRet
		ENDPROC	FdRetAh

; ---------------------------------------------------------------------
; Shared function tail for floppy disk int13 handlers that need to undo
; the stack layout setup by Int13_Actual.
; ---------------------------------------------------------------------
FdRet		PROC
		; Replace flags on the ISR stack so the int13 caller gets them
		pushf
		pop	[bp+IsrStackAx.returnFlags]
		; Remove the AX we added at the start of the int13 code
		add	sp, 2
		jmp	UnmakeIsrStack
		ENDPROC	FdRet

; ---------------------------------------------------------------------
; FdUnsupported
; Called for int13 floppy disk requests we don't support, e.g. drive
; index out of range.
; Expects to be jumped into from Int13Fd_Actual.
; ---------------------------------------------------------------------
FdUnsupported	PROC
		mov	[FdScratch.returnAh], INT13STAT_BADCMD
		jmp	FdRetAh
		ENDPROC	FdUnsupported

; ---------------------------------------------------------------------
; FdFormat
; Formats a single track.
; Expects to be jumped into from Int13Fd_Actual.
; ---------------------------------------------------------------------
FdFormat	PROC
		; Reuse caller's flags to validate drive index
		jc	FdUnsupported

		; Ensure motor is running and try operation
		mov	[FdScratch.returnAh], 0
		call	FdUpdateMotor
		call	FdTryNextMediaType
		jc	.opComplete
		call	FdSyncDtr
		call	FdSetupDma
		jc	.opComplete
		mov	ch, [bp+IsrStackAx.ch]
		call	FdCompatSeek
		jc	.opComplete
		call	FdHeadDelay

		mov	al, FDC_CMD_FORMAT | FDC_CMD_MF
		call	FdWriteByte		; format command
		jc	.opComplete
		call	FdWriteHdUs		; drive/head select
		jc	.opComplete
		mov	bx, DISKETTE_PARMS.sectorSize
		call	FdWriteDptBytes2	; sector size/count
		jc	.opComplete
		mov	bx, DISKETTE_PARMS.formatGapLen
		call	FdWriteDptBytes2	; gap length/fill byte
		jc	.opComplete
		call	FdWaitInt
		jc	.opComplete
		call	FdConvResult

.opComplete	mov	cl, 1			; ??? expect one sector
		jmp	FdPerformOp.opComplete
		ENDPROC	FdFormat

; ---------------------------------------------------------------------
; FdReset [TechRef 9-4]
; Resets the floppy disk controller and adjusts BDA variables to match.
; Expects to be jumped into from Int13Fd_Actual.
; ---------------------------------------------------------------------
FdReset		PROC
		mov	[FdScratch.returnAh], INT13STAT_OK
		call	FdResetSelect
		xor_	al, al
.retAhThunk	jmp	FdRetAh
		ENDPROC	FdReset

; ---------------------------------------------------------------------
; FdStatus [TechRef 9-4]
; Returns the AH status code from the most recent operation.
; Expects to be jumped into from Int13Fd_Actual.
; ---------------------------------------------------------------------
FdStatus	PROC
		mov	al, [FdScratch.returnAh]
		; Not sure why we use a thunk here -- we're 116 bytes away
		; from FdRetAh and could have used a short jmp directly.
		; Possibly there was more code between us and our destination
		; in previous BIOS versions?
		jmp	FdReset.retAhThunk
		ENDPROC	FdStatus

; ---------------------------------------------------------------------
; FdSetMediaType [TechRef 9-8]
; Expects to be jumped into from Int13Fd_Actual.
; ---------------------------------------------------------------------
FdSetMediaType	PROC
		; Reuse caller's flags to validate drive index
		jc	FdUnsupported

		; Get drive type, fail call if no drive is present
		mov_	bl, dl
		xor_	bh, bh
		call	FdGetDriveType2
		jc	.leaveFailure
		jz	.leaveFailure

		; Convert BDA drive type nibble into the appropriate offset
		; in the DPB array
		dec	al		; BDA nibble     -> DPB index
		jz	.typeAdjusted	; 1 (5.25" 360K) -> 0
		cmp	al, 3
		sbb	al, 0FEh	; 2 (5.25" 1.2M) -> 2
					; 3 (3.5"  720K) -> 3
					; 4 (3.5"  1.4M) -> 5

.typeAdjusted	; Convert the DPB index into a pointer in DI
		mov	ah, SIZE#DPB_MEDIA
		nop			; ???
		nop
		mul	ah
		mov	di, Dpb525_360K
		add_	di, ax

.checkMedia	; If sector/track count doesn't match what was requested,
		; we may be able to try an alternate DPB for media of a
		; different density (e.g. 360K disk in a 1.2M drive).
		cmp	ch, [cs:di+DPB_MEDIA.maxTracks],DISP=WORD
		jnz	.tryAltDpb
		cmp	cl, [cs:di+DPB_MEDIA.dpb.lastSectorId],DISP=WORD
		jnz	.tryAltDpb

		; Found a matching DPB, decide if we need double stepping
		mov	ah, [cs:di+DPB_MEDIA.mediaType],DISP=WORD
		cmp	di, Dpb525_360K
		jz	.setMediaType
		cmp	ch, 40
		jnb	.setMediaType
		or	ah, FDMS_DOUBLE_STEP

.setMediaType	call	FdResetMediaState
		xor_	ah, ah
		mov	[bp+IsrStackAx.di], di
		mov	[bp+IsrStackAx.es], cs

.fdRetThunk	jmp	FdRet

.tryAltDpb	; Only two drive types support lower-density media
		cmp	di, Dpb525_12M	; try 360K media in 1.2M drive
		jz	.tryPrevDpb
		cmp	di, Dpb35_14M	; try 720K media in 1.44M drive
		jnz	.leaveFailure

.tryPrevDpb	; The DPB media entry immediately preceding the current
		; one is for the same drive but lower-density media.
		sub	di, SIZE#DPB_MEDIA,IMM=WORD
		jmp	.checkMedia

.leaveFailure	mov	ax, (INT13STAT_BADMEDIA << 8) | 00h
		stc
		jmp	.fdRetThunk
		ENDPROC	FdSetMediaType

; ---------------------------------------------------------------------
; FdDiskParms [TechRef 9-6]
; Complex logic to return data from the appropriate DPB...
; Expects to be jumped into from Int13Fd_Actual.
; TODO: figure out why this is doing all the crazy bit-twidling and
;       comment the code.
; ---------------------------------------------------------------------
FdDiskParms	PROC
		; Strange extra check for hard drive indexes ???
		pushf
		cmp	dl, 80h
		jb	.validDrive
		popf
		jmp	FdUnsupported
.validDrive	popf

		; If drive number not valid, return total drive count
		mov	bx, 0
		jb	.countDrives

		; ???
		mov	dl, 1
		call	FdGetDriveType2
		mov	dh, 0FFh
		jc	.L1
		mov_	bl, al
		jz	.countDrives
		cmp_	al, ah
		adc	dl, 0
		call	FdCountDrives

		mov	dh, 1
		dec	al
		jz	.L1

		mov	dh, 6
		dec	al
		jz	.L1

		mov	dh, 8
		dec	al
		jz	.L1

		mov	dh, 30h
		dec	al
		jz	.L1

		mov	dh, 0FFh

.L1		; ??? bit masking to get a value which we shift left
		; until a one drops into CF.  Number of shifts is the
		; index into the DPB array from the end (actually from
		; three past the end since the top three bits in the
		; bitmask aren't used).  Is this really the simplest
		; way to do this?
		mov	ah, 3Fh
		call	sub_FABBF
		and_	dh, ah
		jz	.L2
		and_	ah, dh
.L2		or_	ah, ah
		jz	.countDrives
		mov	di, Dpb35_14M + (SIZE#DPB_MEDIA * 3)
.locateDpb	sub	di, SIZE#DPB_MEDIA,IMM=WORD
		rol	ah, 1
		jnc	.locateDpb

		; Copy data out the DPB to return to our caller
		mov	ch, [cs:di+DPB_MEDIA.maxTracks],DISP=WORD
		mov	dh, 1		; maxHeads always = 1
		mov	cl, [cs:di+DPB_MEDIA.dpb.lastSectorId],DISP=WORD
		mov	ax, cs
		jmp	.returnValues

.countDrives	xor_	dx, dx
		mov	[bp+IsrStackAx.dl], 0
		call	FdGetDriveType
		; If no drives configured, don't try to count them
		jz	.clearRegs
		mov	dl, 1
		cmp_	al, ah		; is this resetting CF?
		adc	dl, 0
		call	FdCountDrives

.clearRegs	xor_	bx, bx
		xor_	cx, cx
		xor_	di, di
		xor_	ax, ax

.returnValues	mov	[bp+IsrStackAx.cx], cx
		mov	[bp+IsrStackAx.dx], dx
		mov	[bp+IsrStackAx.bx], bx
		mov	[bp+IsrStackAx.di], di
		mov	[bp+IsrStackAx.es], ax
		xor_	ax, ax
		jmp	FdSetMediaType.fdRetThunk
		ENDPROC	FdDiskParms

; ---------------------------------------------------------------------
; sub_FABBF
; Helper for FdDiskParms.
; ??? Masks the value in AH according to floppy drive type?
; ---------------------------------------------------------------------
sub_FABBF	PROC
		call	FdGetDriveInfo
		shr	al, 1		; move change line detect into CF
		jc	.haveChangeLine
		and	ah, 1
		jmp	.L1

.haveChangeLine	and	ah, 3Eh

.L1		or_	al, al		; drive type determined?
		jnz	.L2
		and	ah, 9
.L2		dec	al
		jnz	.L3
		and	ah, 6
.L3		dec	al
		jnz	.leaveFunction
		and	ah, 3Bh
.leaveFunction	retn
		ENDPROC	sub_FABBF

ENDPROGRAM	FD_INT13_HANDLE
