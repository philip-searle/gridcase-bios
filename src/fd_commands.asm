
FD_COMMANDS	PROGRAM	OutFile=build/fd_commands.obj

		include	"macros.inc"
		include	"segments/bda.inc"
		include	"fdc.inc"
		include	"int13.inc"
		include	"isr.inc"

		EXTERN	FdGetCylinder, FdSetCylinder
		EXTERN	FdGetMediaState
		EXTERN	FdWriteByte
		EXTERN	FdWriteHdUs
		EXTERN	FdWaitInt, FdSenseInt
		EXTERN	FdReadResult

		PUBLIC	FdCalibrate
		PUBLIC	FdCompatSeek
		PUBLIC	FdDoSeek
		PUBLIC	FdSeek

; ---------------------------------------------------------------------
; As FdSeek, but also adjusts the requested cylinder index if the
; drive/media combination requires double-stepping.
FdCompatSeek	PROC
		; Double the cylinder value if media state indicates
		; double-stepping is required
		mov	bl, [bp+IsrStackAx.dl]
		xor_	bh, bh
		push	ax
		call	FdGetMediaState
		test	al, FDMS_DOUBLE_STEP
		pop	ax
		jz	.cylAdjusted
		shl	ch, 1			; double it

.cylAdjusted	; Strange check here for AH having it's high bit set.
		; Maybe remnants of support for int13/AH=D4 function
		; that is explicitly checked for in int13 handler?
		; All it does is delay a minimum 900h loops before
		; continuing with the same code path...
		mov	ah, [bp+IsrStackAx.ah]
		shl	ah, 1
		jnc	FdSeek		; delegate to main seek code

		push	cx
		mov	ch, 9			; strange delay
		loop	$
		pop	cx
		; fall-through into FdSeek
		ENDPROC	FdCompatSeek

; ---------------------------------------------------------------------
; Calibrates the drive if necessary and issues a seek command if the
; current cylinder does not match the desired one.
; On entry:
;   CH == cylinder number
FdSeek		PROC
		xor_	dx, dx

		; Convert current drive index to a one-hot binary value
		; (0 -> 001b, 1 -> 010b, 2 -> 100b)
		mov	al, [bp+IsrStackAx.dl]
		inc	al
		cmp	al, 3
		jnz	.checkCalib
		inc	al

.checkCalib	; Test whether 'drive calibrated' bit is set in the BDA
		; and re-calibrate if it isn't.
		and	al, [FdCalibStatus]
		jnz	.isCalibrated
		push	cx
		call	FdCalibrate
		pop	cx
		jc	FdRetn

.isCalibrated	; See if we're already on the desired cylinder.  If so,
		; then we can return immediately, else we fall-through
		; into FdDoSeek.
		mov	bl, [bp+IsrStackAx.dl]
		xor_	bh, bh
		push	ax
		call	FdGetCylinder
		cmp_	al, ch
		pop	ax
		jz	FdRetn
		; fall-through into FdDoSeek
		ENDPROC	FdSeek

; ---------------------------------------------------------------------
; Issues seek command to the FDC and stores desired cylinder in the BDA
; Waits for the FDC to indicate command completion using an interrupt.
; On entry:
;   CH == desired cylinder
FdDoSeek	PROC
		mov	al, FDC_CMD_SEEK
		call	FdWriteByte
		jc	FdRetn
		call	FdWriteHdUs		; select head/unit
		jc	FdRetn

		; store new cylinder index in BDA
		mov_	al, ch
		mov	bl, [bp+IsrStackAx.dl]
		xor_	bh, bh
		push	ax
		mov_	al, ch
		call	FdSetCylinder
		pop	ax
		call	FdWriteByte		; final cmd byte: cylinder
		jc	FdRetn
		; fall-through into FdWaitCmdDone
		ENDPROC	FdDoSeek

; ---------------------------------------------------------------------
; Waits for the FDC to signal command completion and the selected unit
; to match that used by the last issued command.
; On return:
;   AH == FDC ST0 register
;   CF set on error
FdWaitCmdDone	PROC
		; Wait for FDC interrupt handler to signal it has been run
		call	FdWaitInt
		jc	FdRetn

		; Issue "sense interrupt status" command to FDC to get
		; the ST0 status register so we can check which unit
		; it thinks it has selected.  If it's not for the unit
		; we most recently issued a command for, then keep
		; waiting - it may be a spurious interrupt or from a
		; previous command?
		call	FdSenseInt
		jc	FdRetn
		mov_	al, ah
		and	al, FDC_ST0_UNIT_SELECT
		cmp	al, [bp+IsrStackAx.dl]
		jnz	FdWaitCmdDone

		; Report any non-normal command termination
		test	ah, FDC_ST0_INTERRUPT_CODE
		jz	FdRetn
		or	[FdScratch.returnAh], INT13STAT_BADSEEK
		; fall-through into FdRetnFailure
		ENDPROC	FdWaitCmdDone

; ---------------------------------------------------------------------
; Shared function tail for seek/calibrate functions.
; Sets the carry flag then returns.
FdRetnFailure	PROC
		stc
		; fall-through into FdRetn
		ENDPROC	FdRetnFailure

; ---------------------------------------------------------------------
; Shared function tail for seek/calibrate functions.
; Just a near return.
FdRetn		PROC
		retn
		ENDPROC	FdRetn

; ---------------------------------------------------------------------
; Issues a recalibrate command to the FDC, retrying once if needed,
; then updates the drive calibration status in the BDA.
FdCalibrate	PROC
		mov	al, FDC_CMD_RECALIBRATE
		call	FdWriteByte
		jc	FdRetn
		call	FdWriteHdUs		; cmd byte 2: head/unit
		jc	FdRetn
		call	FdWaitCmdDone
		jnc	.calibrateDone

		; Recalibrate failed, see if it was because we couldn't
		; locate track zero in 77 steps. If so it might just be
		; that the drive needs a few more steps - retry once.
		cmp	[FdScratch.returnAh], INT13STAT_BADSEEK
		jnz	FdRetnFailure
		and	ah, FDC_ST0_INTERRUPT_CODE | FDC_ST0_EQUIP_CHECK
		cmp	ah, FDC_IC_INCOMPLETE | FDC_ST0_EQUIP_CHECK
		jnz	FdRetnFailure

		; Issue another recalibrate command
		mov	[FdScratch.returnAh], 0
		mov	al, FDC_CMD_RECALIBRATE
		call	FdWriteByte
		jc	FdRetn
		call	FdWriteHdUs		; cmd byte 2: head/unit
		jc	FdRetn
		call	FdWaitCmdDone
		jc	FdRetn

.calibrateDone	; Make sure track 0 was found, report error if not
		test	ah, FDC_ST0_EQUIP_CHECK
		jz	.calibrateOk
		or	[FdScratch.returnAh], INT13STAT_BADSEEK
		stc
		retn

.calibrateOk	; Update drive calibration bit in BDA
		mov	al, [bp+IsrStackAx.dl]
		inc	al			; convert drive index to
		cmp	al, 3			; bitmask in calibration byte
		jnz	.L1
		inc	al
.L1		or	[FdCalibStatus], al	; set drive calibrated bit

		; Update current cylinder in BDA to zero
		mov	bl, [bp+IsrStackAx.dl]
		xor_	bh, bh
		push	ax
		mov	al, 0
		call	FdSetCylinder
		pop	ax
		retn
		ENDPROC	FdCalibrate

ENDPROGRAM	FD_COMMANDS
