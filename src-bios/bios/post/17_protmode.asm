
; ---------------------------------------------------------------------------
; Tests that use protected mode, plus their prerequisites:
;  - Test timer interrupt works.
;  - Test KBC reset works (using timer to detect failure).
;  - Switch to protected mode (testing the A20 gate on the way).
;  - Detect expanded memory size.
;  - Return to real mode.
POST17_ProtMode	PROC
		; Enter via fall-through from previous POST code

; ---------------------------------------------------------------------------
; Test that the timer interrupt is firing as expected.
		mov	al, CHECKPOINT_TIMER_INT
		out	PORT_DIAGNOSTICS, al

		cli			; disable all interrupts
		mov	al, ~IRQ_TIMER	; unmask PIC timer input
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
.haveDisplay	Inline	ConString,'No timer tick interrupt',0Dh,0Ah,0
		call	SetCriticalErr
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
		mov	al, KBC_CMD_PULSEP2
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
.haveDisplay2	Inline	ConString,'Shutdown failure',0Dh,0Ah,0
		mov	[SoftResetFlag], SOFT_RESET_FLAG | CRITICAL_ERR_FLAG
		mov	[es:IvtInt08], Int8_Compat,DATA=WORD
		; If the KBC can't reset the CPU then we must skip all
		; tests that use protected mode since we won't be able
		; to exit from real-mode without the KBC's help.
		; In practice this means expanded memory size checks
		; and, since they share the same helper code, the base
		; memory size checks as well.
		jmp	.pastPmodeTests
		%IF	BIOS_VERSION = 19880912
			; Older BIOS wasn't as optimized
			FillerNop
		%ENDIF

.skipShutdown	; A non-fatal failure in timer tick test ends up here.
		; (Why?  Maybe we can still reset via the KBC and give
		; the user some additional useful diagnostics perhaps).
.shutdownOk	; A successful shutdown/reset also ends up here.
		; Reset the timer interrupt handler to the normal one.
		mov	[es:IvtInt08], Int8_Compat,DATA=WORD

		; TODO: check whether the shutdown handler modifies
		; InterruptFlag to indicate a successful reset...
		test	[InterruptFlag], 20h
		jnz	.detectMemSize

		; Flag not set; mark minimum amount of memory?
		; Note that DetectMemSize below will also set MemorySizeKb
		test	[InterruptFlag], 10h
		mov	bx, 200h	; 512KB
		jnz	.l1
		mov	bx, 100h	; 256KB
.l1		mov	[MemorySizeKb], bx
		jmp	.pastPmodeTests

; ---------------------------------------------------------------------------
; Detect the amount of extended memory the computer has fitted
; Different methods depending on BIOS version
.detectMemSize
		%IF	BIOS_VERSION = 19880912
			; 1988 BIOS just detects memory size
			call	DetectMemSize
		%ELSE
			; Later BIOS also needs to detect and configure memory
			; controller (256K/1M SIMMs)
			mov	dx, PORT_PAR_PORTB_W
			mov	al, 1		; ??? enable all address lines
			out	dx, al
			Delay	2
			inc	dx		; ??? increment to 0FFEh
			out	dx, al
			call	DetectMemSize

			; CMOS mem size bytes have been updated, so use the
			; new values to configure the memory controller
			mov	al, CMOS_EXPMEM_HIBYTE | NMI_DISABLE
			call	ReadCmos
			mov_	ah, al
			mov	al, CMOS_EXPMEM_LOBYTE | NMI_DISABLE
			call	ReadCmos
			mov_	bx, ax			; got expansion memory size
			mov	al, CMOS_BASEMEM_HIBYTE | NMI_DISABLE
			call	ReadCmos
			mov_	ah, al
			mov	al, CMOS_BASEMEM_LOBYTE | NMI_DISABLE
			call	ReadCmos
			add_	bx, ax			; add on base memory size

			; Configure the memory controller based on the detected memory size
			call	DetectMemC
			mov	al, 0
			jnb	.ramc1M		; 1MB or 256KB memory controller?

.ramc256K		cmp	bx, 200h	; <= 512KB?
			jbe	.configureRamc
			inc	al
			cmp	bx, 400h	; <= 1MB?
			jbe	.configureRamc
			inc	al
			cmp	bx, 600h	; <= 1.5MB?
			jbe	.configureRamc
			inc	al		; must be 2MB then
			jmp	.configureRamc

.ramc1M			cmp	bx, 800h	; <= 2MB?
			jbe	.configureRamc
			inc	al
			cmp	bx, 1000h	; <= 4MB?
			jbe	.configureRamc
			inc	al
			cmp	bx, 1800h	; <= 6MB?
			jbe	.configureRamc
			inc	al		; must be 8MB then

.configureRamc		mov	dx, PORT_PAR_PORTB_W
			out	dx, al		; ??? 0-3 number of populated RAM banks
			Delay	2
			inc	dx		; ??? increment to 0FFEh
			shr	al, 1
			out	dx, al		; ??? 0-1
		%ENDIF

.pastPmodeTests	; Exit via fall-through to next POST procedure
		ENDPROC POST17_ProtMode

