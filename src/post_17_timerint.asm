
POST17_TimerInt	PROC
		; Enter via fall-through from previous POST code

; ---------------------------------------------------------------------------
; Test that the timer interrupt is firing as expected.
		mov	al, CHECKPOINT_TIMER_INT
		out	PORT_DIAGNOSTICS, al

		cli			; disable all interrupts
		mov	al, 0FEh	; unmask PIC timer input
		out	PORT_PIC1_MASK, al
		mov	cx, 0		; set timer IVT entry to test success handler
		mov	es, cx
		mov	[es:IvtInt08], .timerTickOk,DATA=WORD
		sti			; re-enable interrupts

		; Delay loop: FFFEh*(5+8) + 1*(5+4) = 851,951 clocks = 0.07sec @ 12MHz
		; A working 18.2ms timer interrupt should fire ~3 times before this loop exits
.waitForTimer	rol	al, 1Fh		; 5 clocks
		loop	.waitForTimer	; 8 clocks (nojmp=4)
		cli

		; If we reach here then the timer didn't fire in time - report failure.
		test	[InterruptFlag], 20h
		jnz	.haveDisplay
		mov	al, BEEP_TIMER_INT
		jmp	FatalBeeps
.haveDisplay	Inline	WriteString,'No timer tick interrupt',0Dh,0Ah,0
		call	SetSoftResetFlag
		jmp	.skipShutdown

		; This is our temporary handler for the timer tick.  There's
		; no need to IRET here as we're going to immediately trigger
		; a reset via the keyboard controller to test the shutdown
		; recovery handler feature.
.timerTickOk	mov	al, 20h		; reset PIC with non-specific EOI
		out	PORT_PIC1_CTRL, al

; ---------------------------------------------------------------------------
; Test the the keyboard controller can successfully reset the CPU
		mov	al, CHECKPOINT_SHUTDOWN
		out	PORT_DIAGNOSTICS, al

		; Reset with the right shutdown reason in CMOS will jmpf to
		; AdapterRomAddress.  If it doesn't do that before the next
		; timer tick then something is wrong.
		mov	es, [cs:kIvtSegment]
		mov	[es:IvtInt08], .shutdownFail,DATA=WORD
		mov	[AdapterRomOffset], .shutdownOk
		mov	[AdapterRomSegment], cs
		mov	ah, CMOS_SHUTDOWN_REASON | NMI_DISABLE
		mov	al, SD_JMP_WITHOUT_INT
		call	WriteCmos

		; Ask KBC to pulse the reset line
		mov	al, 0FEh
		out	PORT_KBC_CMD, al
		sti
		hlt

.shutdownFail	; If we reach here then the reset never took place
		; Notify the user any way we can
		mov	al, 20h		; reset PIC with non-specific EOI
		out	PORT_PIC1_CTRL, al
		test	[InterruptFlag], 20h
		jnz	.haveDisplay2
		mov	al, BEEP_SHUTDOWN
		jmp	FatalBeeps
.haveDisplay2	Inline	WriteString,'Shutdown failure',0Dh,0Ah,0
		mov	[SoftResetFlag], 1235h	; ???
		mov	[es:IvtInt08], Int8_Compat,DATA=WORD
		jmp	loc_F885D	; why jump so far ahead?

.skipShutdown	; A non-fatal failure in timer tick test ends up here.
.shutdownOk	; So does a successful shutdown/reset rest.

		; Exit via fall-through to next POST procedure
		ENDPROC POST17_TimerInt

