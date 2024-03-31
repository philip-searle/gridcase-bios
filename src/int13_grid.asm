
INT13_GRID	PROGRAM	OutFile=build/int13_grid.obj

		include	"macros.inc"
		include	"segments/bda.inc"
		include	"grid.inc"
		include	"hdc_at.inc"
		include	"int13.inc"

		EXTERN	DriveIdentify
		EXTERN	kBdaSegment
		EXTERN	HdAtMakeRegPack, HdAtSendRegPack
		EXTERN	HdAtDecideError
		EXTERN	HdAtWaitTask

		PUBLIC	HdAtSpinDown

; ===========================================================================
; HdAtSpinDown [TechRef 3-25]
; GRiD-specific BIOS call that controls hard disk spin down behaviour.
; ===========================================================================
HdAtSpinDown	PROC
		call	DriveIdentify
		cmp	ax, GRID_HD_5
		jz	.supportedDrive
		cmp	ax, GRID_HD_6
		jz	.supportedDrive
		cmp	ax, GRID_HD_9
		jz	.supportedDrive

		; Not a drive for which we support spin down
		mov	ah, GRID_INT15_ERROR
		stc
		retn

.supportedDrive	cmp	dh, 0FFh
		jz	.infoRequest

		; save registers we will modify
		mov_	al, dl
		push	bx
		push	cx
		push	dx
		push	si
		push	ds
		push	di
		push	es

		or_	al, al		; force motor off?
		jz	.turnOffMotor
		cmp	al, 0FFh	; force motor on?
		jz	.turnOnMotor

		; AL is non-zero but not FFh, so enable auto power-off
		; using AL as the spin-down timeout (in 5-second intervals)
		mov	ah, 0E2h	; [TechRef 10-34] Standby mode with auto power-off
		cmp	dh, 1
		ja	.leaveFunction	; only 0/1 are valid values
		add_	ah, dh		; potential adjust by one to get:
					; [TechRef 10-34] Idle mode with auto power-off
		jmp	.sendHdCmd

.turnOnMotor	; AL is zero so enter idle mode with auto power-off disabled
		mov	ah, 0E3h	; [TechRef 10-34] Idle mode with auto power-off
		mov	al, 0		; auto power-off disabled
		jmp	.sendHdCmd2

.turnOffMotor	; AL is FFh so enter standby mode immediately
		mov	ah, 0E0h	; [TechRef 10-34] Standby mode
		mov	al, 1		; why set sector count here?
					; command E0h doesn't need it...

.sendHdCmd2	mov	cx, 1		; why set sector number here?
					; none of the power commands need it...
.sendHdCmd	mov	dx, 0
		mov	ds, [cs:kBdaSegment]
		call	HdAtMakeRegPack
		jb	.leaveFunction
		call	HdAtSendRegPack
		jb	.leaveFunction
		call	HdAtDecideError
		jb	.leaveFunction
		xor_	ah, ah		; report success

.leaveFunction	pop	es
		pop	di
		pop	ds
		pop	si
		pop	dx
		pop	cx
		pop	bx
		xor_	al, al		; clear CF
		retn

.infoRequest	; Return information about power saving potential if
		; hard disk spin-down is enabled.
		; This is not as smart as [TechRef 3-26] makes it seem.
		; Actually we hardcode all the values!
		mov	bx, 20		; estimated number of start/stops the
					; drive is expected to survives before failure, divided by 1000
		mov	cx, 220		; maximum number of 5-second intervals supported for timeout
		mov	dx, 12		; minimum number of 5-second intervals supported for timeout
		xor_	ax, ax		; report success
		retn
		ENDPROC	HdAtSpinDown

; ===========================================================================
; HdAtIdentify
; Requests identifying information from the hard disk drive.
; ===========================================================================
HdAtIdentify	PROC
		; Allocate buffer for drive identity (in BP)
		push	bp
		mov_	bp, sp
		sub	sp, 46h
		push	bp
		sub	bp, 44h

		; Preserve registers we will modify
		push	ax
		push	bx
		push	cx
		push	dx
		push	si
		push	ds

		; Set ES:DI to point to drive identity buffer
		mov	ax, ss
		mov	es, ax
		mov_	di, bp
		push	di	; store original buffer pointer

		; Setup for AT disk controller command
		mov	ds, [cs:kBdaSegment]
		mov	[HdLastOpStatus], 0, DATA=BYTE
		sti

		mov	bl, 2	; ??? unused, possible this code originally
				; looped over two disk drives?

		; Wait for the disk controller to not be busy
		mov	ah, 200	; retry count for HDC to not be busy
		mov	cx, 0	; maximum loop count for inner timeout loop
		mov	dx, PORT_HD_AT_STATUS
.waitNotBusy	in	al, dx
		test	al, HD_AT_STATUS_BSY
		jz	.notBusy
		loop	.waitNotBusy
		sub	ah, 1
		jnb	.waitNotBusy

.notBusy	jb	.failure

		; Send 'identify drive' command
		mov	dx, PORT_HD_AT_COMMAND
		mov	al, 0ECh		; [TechRef 10-30] Identify drive
		out	dx, al
		call	HdAtWaitTask
		jb	.failure

		; Wait for drive to have data for us
		mov	dx, PORT_HD_AT_STATUS
		mov	cx, 200	; retry count for drive to have data
.waitForData	in	al, dx
		test	al, HD_AT_STATUS_DRQ
		loope	.waitForData
		jnz	.haveData
		; If no data available in time, set error code and carry
		; flag and fallthrough
		mov	ah, INT13STAT_BADCONTROLLER
		stc

.haveData	cli	; disable interrupts while we read the data
		jb	.failure

		; Read (and ignore) the first 1Bh words
		mov	dx, PORT_HD_AT_DATA
		mov	cx, 1Bh
.readLoop1	in	ax, dx
		loop	.readLoop1

		; Read (and store) the next 11h words
		mov	cx, 11h
.readLoop2	in	ax, dx
		xchg	ah, al	; disk controller reports data byteswapped
		stosw
		loop	.readLoop2

		; Read (and ignore) the remainder of the sector buffer
		mov	cx, 0D4h
.readLoop3	in	ax, dx
		loop	.readLoop3

		; Was the read successful?
		call	HdAtDecideError
		jnb	.leaveFunction

.failure	; Reset the disk controller and report an error
		mov	al, HD_AT_DIGOUT_SRST
		mov	dx, PORT_HD_AT_DIGOUT
		out	dx, al	; place in reset
		Delay	2	; wait a bit
		xor_	al, al	; take out of reset
		out	dx, al
		stc		; report error in CF

.leaveFunction	sti		; safe to have interrupts now
		pop	di
		pop	ds
		pop	si
		pop	dx
		pop	cx
		pop	bx
		pop	ax
		pop	sp
		pop	bp
		retn
		ENDPROC	HdAtIdentify

ENDPROGRAM	INT13_GRID
