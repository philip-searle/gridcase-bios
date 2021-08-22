
POST11_LoadIvt	PROC
		; Enter via fall-through from previous POST code

; ---------------------------------------------------------------------------
; Initialize the interrupt vector table and enable interrupts
		mov	al, CHECKPOINT_IVT
		out	PORT_DIAGNOSTICS, al

		mov	di, IVT_SEGMENT		; point ES:DI at IVT start
		mov	es, di
		mov	bx, cs			; BX is BIOS code segment
		mov	ds, bx
		mov	dx, DummyIsr
		push	dx			; why save DX here?

		; Set int 00h - 78h to DummyIsr
		mov	cx, 78h
.clearIvt	mov_	ax, dx
		stosw				; store ISR offset
		mov_	ax, bx
		stosw				; store ISR segment
		loop	.clearIvt

		; Set int 60h - 67h to 0000:0000
		mov_	ax, cx			; CX was 0 from loop above
		mov	di, IvtInt60		; start filling from int 60
		mov	cx, (IvtInt67 - IvtInt60) / 2
		rep stosw

		; Set int 08h - int 14h
		mov	si, InitialIvt
		mov	di, IvtInt08
		mov	cx, (IvtInt14 - IvtInt08) / 2
		pop	dx			; why pop an unmodified value?
.loadIvt1	lodsw				; load ISR offset
		cmp_	ax, dx			; ISR offset is DummyIsr?
		jnz	.loadIvtEntry1
		add	di, 4			; skip if so
		jmp	.loadIvtLoop1
.loadIvtEntry1	stosw				; store ISR offset
		mov	ax, cs
		stosw				; store ISR segment
.loadIvtLoop1	loop	.loadIvt1

		; Set int 1Fh to 0000:0000 (InitialIvt sets the offset to 0
		; but the segment was set to CS, so we must correct it)
		mov	[es:IvtVidGrapFont + 2], 0,DATA=WORD

		; Set int 70h to 78h
		; DI is still pointing to InitialIvt table from previous loop
		mov	di, IvtInt70		; start filling in from int 70h
		mov	cx, 78h - 70h
.loadIvt2	lodsw				; load ISR offset
		cmp_	ax, dx			; ISR offset is DummyIsr?
		jnz	.loadIvtEntry2
		add	di, 4			; skip if so
		jmp	.loadIvtLoop2
.loadIvtEntry2	stosw				; store ISR offset
		mov	ax, cs
		stosw				; store ISR segment
.loadIvtLoop2	loop	.loadIvt2

		; Set a few other IVT entries manually
		; [Compat] is this required because the InitalIvt table
		;          must keep the same items in the same order?
		mov	[es:IvtInt02], IntNmi_Compat,DATA=WORD
		mov	[es:IvtInt02+2], cs
		mov	[es:IvtInt05], PrntScrn_Compat,DATA=WORD
		mov	[es:IvtInt05+2], cs

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

