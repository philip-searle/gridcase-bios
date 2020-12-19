
; Number of times to toggle the speaker for a fatal beep
FATAL_BEEP_LENGTH	equ	300

; ===========================================================================
; FatalBeeps
;
; Produces three groups of beeps using the value in AL, outputs AL to the
; diagnostic port, and then halts the CPU and does not return.
;
; On entry:
;   AL == Beep code: 8-bit value "xxaabbcc"
;         xx = Don't care
;         aa = Count of beeps minus one, first group
;         bb = Count of beeps minus one, second group
;         cc = Count of beeps minus one, second group
; ===========================================================================
FatalBeeps:
		out	PORT_DIAGNOSTICS, al
		jmp	.l1
		nop				; Mystery nop to jump over?
.l1		mov	bh, [cs:BeepFactor]	; cs override because ds may not be valid
		mov	dh, al			; move beep code to dh; al will be used for port writes
		mov	al, 0Ch			; setup KBC port B write: both parity checks disabled
		rol	dh, 2			; pre-rotate beep code for main loop
		mov	bl, 3			; three groups of beeps

; ---------------------------------------------------------------------------
.beepGroup	rol	dh, 2			; rotate next beep group to lower bits of dh
		mov	dl, dh			; copy beep group and isolate lower bits
		and	dl, 3
		inc	dl			; beep codes are stored excess-one so adjust for true value

; ---------------------------------------------------------------------------
.beep		mov	si, FATAL_BEEP_LENGTH
.l2		xor	al, 2			; toggle speaker
		out	PORT_KBC_PORTB, al

		mov	cl, bh			; loop 4xbeep factor between toggles
		mov	ch, 0
		add	cx, cx
		add	cx, cx
		loop	$			; 4 clock loop

		dec	si
		jnz	.l2

; ---------------------------------------------------------------------------
		mov	ch, bh			; delay 256xbeep factor between beeps
.beepGap	Delay	1
		loop	.beepGap

		dec	dl			; beep completed
		jnz	.beep			; loop if more in this group

; ---------------------------------------------------------------------------
		mov	ch, bh			; delay 1024xbeep factor between beep groups
.beepGroupGap	Delay	4
		loop	.beepGroupGap

		dec	bl			; beep group completed
		jnz	.beepGroup		; loop if not last group

		hlt				; halt computer

