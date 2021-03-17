
POST18_Timer2	PROC
		; Enter via fall-through from previous POST code

; ---------------------------------------------------------------------------
; Test that programmable interval timer channel 2 is advancing correctly
; See post_05_initpit.asm for a description of the test algorithm.
		mov	al, CHECKPOINT_TIMER2
		out	PORT_DIAGNOSTICS, al

		; Save state before we run the test
		mov	dx, PORT_KBC_PORTB
		push	dx		; save port for later
		in	al, dx
		push	ax		; save original port value

		; Disable timer 2 gate to speaker while we test it
		and	al, 0FCh	; timer 2 gate+data off
		or	al, 1		; timer 2 gate off, data on?
		out	dx, al

		mov	bx, 00FFh	; initial test pattern
		xor_	cx, cx		; max loops while timer is tested
.testCounter	mov	dx, PORT_PIT_MODE
		mov	al, 80h		; PIT: select timer 2, latch counter value
		out	dx, al
		dec	dx		; decrement to PORT_PIT_COUNTER2

		Delay	2
		in	al, dx		; read counter lobyte
		or_	bh, al		; set/reset bits in test pattern
		and_	bl, al

		Delay	2
		in	al, dx		; read counter hibyte
		or_	bh, al		; set/reset bits in test pattrn
		and_	bl, al

		cmp	bx, 0FF00h	; all bits changed state?
		jz	.timer2Checked	; continue if so
		loop	.testCounter	; go again if not

		; If we reach here then not all bits in the timer changed
		; state within the timeout, so assume failure and report to
		; the user in whatever way we can
		test	[InterruptFlag], 20h
		jnz	.l1
		mov	al, BEEP_TIMER2_FAIL
		jmp	FatalBeeps
.l1		Inline	WriteString,'Timer chip counter 2 failed',0Dh,0Ah,0
		call	SetSoftResetFlag

.timer2Checked	pop	ax		; restore original port value
		pop	dx		; restore port number
		out	dx, al		; restore timer 2 gate state

		; Exit via fall-through to next POST procedure
		ENDPROC POST18_Timer2

