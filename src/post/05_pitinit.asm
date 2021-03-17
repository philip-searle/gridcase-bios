
POST05_PitInit	PROC
		; Enter via fall-through from previous POST code

; ---------------------------------------------------------------------------
; Initialize programmable interval timer
		mov	al, CHECKPOINT_TIMER
		out	PORT_DIAGNOSTICS, al

		; Start RAM refreshing from 0 when timer starts
		mov	al, 0
		out	PORT_DMA_RAM_REFRESH_ADDRESS, al

		; Initialize channel 1
		mov	dx, PORT_PIT_MODE
		mov	al, 54h			; PIT mode: counter 1 select, lobyte only access mode,
						;           mode 2 select (rate generator), 16bit counter
		out	dx, al

		mov	ss, [cs:kBdaSegment]	; why reset SS here? SP is still set from the ROM checksum earlier...
		pusha				; why pusha here?

		dec	dx			; adjust port downwards to PORT_PIT_COUNTER1
		dec	dx
		mov	al, 12h			; PIT channel 1: divisor = 12h (18)
		out	dx, al

		; Initialize channel 0
		inc	dx			; adjust port back to 43h (PORT_PIT_MODE)
		inc	dx
		mov	al, 36h			; PIT mode: counter 0 select, lobyte/hibyte access mode,
						;           mode 3 (square wave generator), 16bit counter
		out	dx, al

		dec	dx			; adjust port downwards to PORT_PIT_COUNTER0
		dec	dx
		dec	dx
		mov	al, 0			; maximum divisor for counter 0: (1)0000h == 18.2Hz clock tick
		out	dx, al			; write lobyte of divisor

		xchg	bh, bl			; possible bug? meant to exchange AL,AH instead but still works because AX is 0?
		xor_	cx, cx			; setup maximum loop count for timer test below
		Delay	1			; only one jmp needed because of xchg/xor above?
		out	dx, al

		; Test channel 0 is counting before we use it for RAM refresh.
		; No RAM refresh setup yet so interrupts can't be used. To test the timer, we
		; repeatedly latch it's value, using it to set bits in BH and clear bits in
		; BL. If we loop more than FFFFh times without encountering each bit at least
		; once then we assume the timer has failed.
		; Note that BX still contains AA55h from the CPU register test above, so the
		; AND/OR on high and low bytes still tests each bit.
.timerTest	mov	dx, PORT_PIT_MODE
		cmp	bx, 0FF00h		; reached target value?
		jz	.timerTestOk		; continue if so

		; latch counter 0 and read it
		mov	al, 0			; PIT mode: counter 0 select, latch count value command
		out	dx, al
		dec	dx			; adjust port downwards to PORT_PIT_COUNTER0
		dec	dx
		dec	dx
		Delay	1			; prev 3 'dec' is clocks equivalent to one 'jmp short' so we only need one delay here
		in	al, dx			; read timer counter 0 latched value (lobyte)
		or_	bh, al			; set bits 
		and_	bl, al			; clear bit
		in	al, dx			; read timer counter 0 latched value (hibyte)
		or_	bh, al			; set bits
		and_	bl, al			; clear bits

		loop	.timerTest		; keep looping until target bit pattern reached
		mov	al, BEEP_TIMER_FAIL	; if loop exits here we call it a failure
		jmp	FatalBeeps

.timerTestOk
		; Initialize channel 2
		mov	al, 0B6h		; PIT mode: counter 2 select, lobyte/hibyte access,
						;           mode 3 select (square wave)
		out	dx, al

		dec	dx			; adjust port downwards to PORT_PIT_CHANNEL2
		mov	al, 33h			; divisor lobyte
		Delay	2
		out	dx, al
		mov	al, 5h			; divisor hibyte
		Delay	2
		out	dx, al			; counter 2 divisor == 533h

		; Exit via fall-through to next POST procedure
		ENDPROC POST05_PitInit

