
POST_TIMER	PROGRAM	OutFile=build/post/timer.obj

		include	"macros.inc"
		include	"segments/bda.inc"

		EXTERN	kBdaSegment

		PUBLIC	InitTimerTicks

; =====================================================================
; POST utilities related to the timer interrupt.
; =====================================================================

; Multiplication factors for converting RTC time to binary
kBcdRadix	d b	10
kMinSecRadix	d w	60

; Magic constants for converting count of seconds to timer tick count
; TODO: figure out how these work
kMagic1		d w	13532
kMagic2		d w	18

; =====================================================================
; InitTimerTicks
; Converts the current RTC value to count of timer ticks since midnight
; and stores this in the BIOS Data Area for the Int1A handler to update
; =====================================================================
InitTimerTicks	PROC
		mov	ds, [cs:kBdaSegment]
		mov	ah, 2
		int	1Ah		; read RTC (BCD result)
					; CH = hours, CL = minutes, DH = seconds

; ---------------------------------------------------------------------
; Check all time components are in range before conversion
		cmp	ch, 24h
		jnb	.leaveProc
		cmp	cl, 60h
		jnb	.leaveProc
		cmp	dh, 60h
		jnb	.leaveProc

; ---------------------------------------------------------------------
; Convert BCD values to binary
		mov_	al, ch
		call	BcdToBinary
		mul	[cs:kMinSecRadix],DATA=BYTE
		; AX = hours * 60

		xchg	ax, cx
		call	BcdToBinary
		mov	ah, 0
		add_	ax, cx
		; AX = (hours * 60) + minutes

		mov_	bl, dh		; preserve seconds
		mul	[cs:kMinSecRadix]
		mov_	cx, ax
		; CX = ((hours * 60) + minutes) * 60

		mov_	al, bl		; restore seconds
		call	BcdToBinary
		mov	ah, 0
		add_	ax, cx		; add seconds to running tota;
		jnb	.l1		; addition carried?
		inc	dx		; propagate to upper word if so
.l1		; AX = (((hours * 60) + minutes) * 60) + seconds

; ---------------------------------------------------------------------
; Magic maths code to convert count of seconds to count of timer ticks
; TODO: figure this out
		mov	[TimerTicks+2], 0,DATA=WORD
		push	dx
		push	ax
		mul	[cs:kMagic1]
		mov	[TimerTicks], dx
		pop	ax
		mul	[cs:kMagic2]
		add	[TimerTicks], ax
		adc	[TimerTicks+2], dx
		pop	ax
		push	ax
		mul	[cs:kMagic1]
		add	[TimerTicks], ax
		adc	[TimerTicks+2], dx
		pop	ax
		mul	[cs:kMagic2],DATA=BYTE
		add	[TimerTicks+2], ax
		stc

.leaveProc	retn
		ENDPROC	InitTimerTicks

; =====================================================================
; BcdToBinary
; Converts BCD byte to its binary equivalent.
;
; On entry:
;   AL == BCD input
;
; On return:
;   AL == binary output
;   BH destroyed
; =====================================================================
BcdToBinary	PROC
		mov_	bh, al
		and	bh, 0Fh
		shr	al, 4
		mul	[cs:kBcdRadix]
		add_	al, bh
		retn
		ENDPROC	BcdToBinary

ENDPROGRAM	POST_TIMER
