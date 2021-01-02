
POST11_LoadIvt	PROC
		; Enter via fall-through from previous POST code

; ---------------------------------------------------------------------------
; Initialize the interrupt vector table and enable interrupts
		mov	al, CHECKPOINT_IVT
		out	PORT_DIAGNOSTICS, al

		mov	di, 0			; point ES:DI at IVT start
		mov	es, di
		mov	bx, cs			; BX is BIOS code segment
		mov	ds, bx
		mov	dx, DummyIsr
		push	dx			; why save DX here?

		; Set int 00h - 1Eh to DummyIsr
		mov	cx, 78h
.clearIvt	mov	ax, dx,CODE=LONG
		stosw				; store ISR offset
		mov	ax, bx,CODE=LONG
		stosw				; store ISR segment
		loop	.clearIvt

		; Set int 60h - 67h to 0000:0000
		mov	ax, cx,CODE=LONG	; CX was 0 from loop above
		mov	di, 180h		; start filling from int 30
		mov	cx, 0Eh			; fill 7x2 words
		rep stosw

		; Set int 08h - int 1Eh
		mov	si, InitialIvt
		mov	di, 20h
		mov	cx, 18h
		pop	dx			; why pop an unmodified value?
.loadIvt1	lodsw				; load ISR offset
		cmp	ax, dx,CODE=LONG	; ISR offset is DummyIsr?
		jnz	.loadIvtEntry1
		add	di, 4			; skip if so
		jmp	.loadIvtLoop1
.loadIvtEntry1	stosw				; store ISR offset
		mov	ax, cs
		stosw				; store ISR segment
.loadIvtLoop1	loop	.loadIvt1

		; Set int 1Fh to 0000:0000 (InitialIvt sets the offset to 0
		; but the segment was set to CS, so we must correct it)
		mov	[es:7Eh], 0,DATA=WORD

		; Set int 70h to 78h
		; DI is still pointing to InitialIvt table from previous loop
		mov	di, 1C0h		; start filling in from int 70h
		mov	cx, 8
.loadIvt2	lodsw				; load ISR offset
		cmp	ax, dx,CODE=LONG	; ISR offset is DummyIsr?
		jnz	.loadIvtEntry2
		add	di, 4			; skip if so
		jmp	.loadIvtLoop2
.loadIvtEntry2	stosw				; store ISR offset
		mov	ax, cs
		stosw				; store ISR segment
.loadIvtLoop2	loop	.loadIvt2

		; Set a few other ICT entries manually
		; [Compat] is this required because the InitalIvt table
		;          must keep the same items in the same order?
		mov	[es:08h], IntNmi_Compat,DATA=WORD
		mov	[es:0Ah], cs
		mov	[es:14h], PrntScrn_Compat,DATA=WORD
		mov	[es:16h], cs

		; Enable interrupts
		mov	ds, [kBdaSegment]
		mov	al, 0FAh		; PIC1: set interrupt mask:
						;   IRQ0 - timer (disabled)
						;   IRQ1 - keyboard
						;   IRQ2 - cascade (disabled)
						;   IRQ3 - serial port, COM2
						;   IRQ4 - modem, COM1
						;   IRQ5 - reserved
						;   IRQ6 - floppy drive
						;   IRQ7 - parallel port
		out	PORT_PIC1_MASK, al
		sti

		; Exit via fall-through to next POST procedure
		ENDPROC POST11_LoadIvt

