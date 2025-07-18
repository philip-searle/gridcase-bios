
GRID_FD		PROGRAM	OutFile=grid_fd.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"segments/bda.inc"
		include	"grid.inc"
		include	"parallel.inc"

		PUBLIC	FdGetMediaState, FdGetMediaState2
		PUBLIC	FdSetMediaState, FdSetMediaState2
		PUBLIC	FdCountDrives
		PUBLIC	FdGetOpStartState, FdSetOpStartState
		PUBLIC	FdGetCylinder, FdSetCylinder
		PUBLIC	FdExternalType

; ===========================================================================
; FdGetMediaState
; Replacement for direct reads from [DS:Fd0MediaState+BX] but also supporting
; the GRiD external floppy/tape drive.
;
; On entry:
;   BX == drive index (0-2)
; On return:
;   AL == drive media state
; ===========================================================================
FdGetMediaState	PROC
		pushf
		mov	al, [ds:Fd0MediaState+bx]
		cmp	bl, 2
		jnz	.leaveFunction
		mov	al, [ds:Fd2MediaState]
.leaveFunction	popf
		retn
		ENDPROC	FdGetMediaState

; ===========================================================================
; FdGetMediaState2
; As FdGetMediaState, except output is in AH not AL.
; ===========================================================================
FdGetMediaState2	PROC
		xchg	al, ah
		call	FdGetMediaState
		xchg	al, ah
		retn
		ENDPROC	FdGetMediaState2

; ===========================================================================
; FdSetMediaState
; Replacement for direct writes to [DS:Fd0MediaState+BX] but also supporting
; the GRiD external floppy/tape drive.
; ===========================================================================
FdSetMediaState	PROC
		pushf
		cmp	bl, 2
		jz	.externalDrive
		mov	[ds:Fd0MediaState+bx], al
		popf
		retn

.externalDrive	mov	[ds:Fd2MediaState], al
		popf
		retn
		ENDPROC	FdSetMediaState

; ===========================================================================
; As FdSetMediaState, except input is in AH not AL.
; ===========================================================================
FdSetMediaState2	PROC
		xchg	al, ah
		call	FdSetMediaState
		xchg	al, ah
		retn
		ENDPROC	FdSetMediaState2

; ===========================================================================
; FdCountDrives
; Uses the drive backplane ID and external drive detect pins to calculate the
; number of floppy disk drives attached to the system.
;
; On return:
;   DL == count of floppy drives
; ===========================================================================
FdCountDrives	PROC
		pushf
		push	ax
		push	dx

		mov	dx, PORT_ROM_SUBSYSTEM1
		in	al, dx
		and	al, GRID_BKPL_MASK
		mov	ah, 0			; assume no floppy drives to start with

		cmp	al, GRID_BKPL_90	; backplane 90 has one floppy drive
		jz	.backplaneHasFd
		cmp	al, GRID_BKPL_30
		ja	.gotInternalFds		; backplanes 40-90 have no floppy drives

.backplaneHasFd	inc	ah			; backplane has one floppy drive
		cmp	al, GRID_BKPL_20
		jnz	.gotInternalFds
		inc	ah			; backplane 20 is the one one with two floppy drives

.gotInternalFds	mov	dx, PORT_PRINTER_STATUSC
		in	al, dx
		and	al, PRINTER_STATC_EFLOPPY | PRINTER_STATC_TAPE
		cmp	al, PRINTER_STATC_EFLOPPY | PRINTER_STATC_TAPE
		jnz	.gotExternalFds
		inc	ah			; count the external drive

.gotExternalFds	pop	dx
		mov_	dl, ah
		pop	ax
		popf
		retn
		ENDPROC	FdCountDrives

; ===========================================================================
; FdGetOpStartState
; Replacement for direct reads from [DS:Fd0OpStartState+BX] but also
; supporting the GRiD external drive.
; ===========================================================================
FdGetOpStartState	PROC
		pushf
		mov	al, [ds:Fd0OpStartState+bx]
		cmp	bl, 2
		jnz	.leaveFunction
		mov	al, [ds:Fd2OpStartState]
.leaveFunction	popf
		retn
		ENDPROC	FdGetOpStartState

; ===========================================================================
; FdSetOpStateState
; Replacement for direct writes to [DS:Fd0OpStartState+BX] but also
; supporting the GRiD external drive.
; ===========================================================================
FdSetOpStartState	PROC
		pushf
		cmp	bl, 2
		jz	.externalDrive
		mov	[ds:Fd0OpStartState+bx], al
		popf
		retn
.externalDrive	mov	[ds:Fd2OpStartState], al
		popf
		retn
		ENDPROC	FdSetOpStartState

; ===========================================================================
; FdGetCylinder
; Replacement for direct reads from [DS:Fd0Cylinder+BX] but also supporting
; the the GRiD external drive.
; ===========================================================================
FdGetCylinder	PROC
		pushf
		mov	al, [ds:Fd0Cylinder+bx]
		cmp	bl, 2
		jnz	.leaveFunction
		mov	al, [ds:Fd2Cylinder]
.leaveFunction	popf
		retn
		ENDPROC	FdGetCylinder

; ===========================================================================
; FdSetCylinder
; Replacement for direct writes to [DS:Fd0Cylinder+BX] but also supporting
; the GRiD external drive.
; ===========================================================================
FdSetCylinder	PROC
		pushf
		cmp	bl, 2
		jz	.externalDrive
		mov	[ds:Fd0Cylinder+bx], al
		popf
		retn
.externalDrive	mov	[ds:Fd2Cylinder], al
		popf
		retn
		ENDPROC	FdSetCylinder

; ===========================================================================
; FdExternalType
; Returns the CMOS drive type ID for the external disk drive.
; ===========================================================================
FdExternalType	PROC
		pushf
		push	dx
		push	ax
		mov	ah, 0			; assume no external drive to start
		mov	dx, PORT_PRINTER_STATUSC
		in	al, dx
		push	ax
		and	al, PRINTER_STATC_EFLOPPY | PRINTER_STATC_TAPE
		cmp	al, PRINTER_STATC_EFLOPPY | PRINTER_STATC_TAPE
		pop	ax
		jnz	.driveTypeKnown

		; Any bit set indicates some type of external drive is present
		mov	ah, 1
		test	al, PRINTER_STATC_HDDRIVE
		jnz	.densityChecked
		inc	ah

.densityChecked	mov	dx, PORT_PRINTER_STATUSB
		in	al, dx
		test	al, PRINTER_STATB_3OR5
		jz	.3or5Checked
		add	ah, 2			; bit 2 set indicates 3.5" drive

.3or5Checked	test	al, PRINTER_STATB_BORA
		jnz	.driveTypeKnown
		mov	ah, 4			; bit 0 set indicates B: or E:, unset indcates A:

.driveTypeKnown	mov_	dl, ah
		pop	ax
		mov_	al, dl
		pop	dx
		popf
		retn
		ENDPROC	FdExternalType

ENDPROGRAM	GRID_FD
