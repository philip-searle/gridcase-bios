
ISR		PROGRAM	OutFile=build/isr.obj

		include	"macros.inc"
		include	"segments/bda.inc"
		include	"pic.inc"

		EXTERN	LoadBdaToDs

		PUBLIC	UnexpectedInt

; ===========================================================================
; UnexpectedInt
; Handler for unexpected interrupts.  Since we don't have a dedicated handler
; for these interrupts we don't know how to handle them.  So we just
; acknowledge the interrupt and then mask it off so it won't occur again.
; ===========================================================================
UnexpectedInt	PROC
		push	ax
		push	ds
		push	dx

		mov	ax, 0FF0Bh	; 0Bh: OCW3 - read IRR
		mov	dx, PORT_PIC1_CTRL
		out	dx, al
		call	LoadBdaToDs
		in	al, dx		; read interrupt request register
		cmp_	dh, al		; set CF if AL is non-zero
		adc_	ah, al		; combine IRR bit with CF to get either:
					;   IRR bit set, CF clear; or
					;   zero, CF set if no hardware interrupt occurred
		mov	[LastInterrupt], ah
		jnb	.leaveFunction

		test	al, 4		; interrupt was on cascade line?
		jz	.disableIrq	; if so, ack PIC1 and switch to PIC2
		mov	al, NONSPECIFIC_EOI
		out	dx, al		; acknowledge master PIC
		mov	al, 0Bh		; 0Bh: oCW3 - read IRR
		mov	dx, PORT_PIC2_CTRL
		out	dx, al
		Delay	4
		in	al, dx		; read interrupt request register
		mov_	ah, al

.disableIrq	inc	dx		; increment to PIC interrupt mask register
		in	al, dx		; read IMR
		or_	al, ah		; disable the IRQ that just came in
		Delay	2
		out	dx, al
		dec	dx		; decrement to PIC control register
		mov	al, NONSPECIFIC_EOI
		out	dx, al

.leaveFunction	pop	dx
		pop	ds
		pop	ax
		iret
		ENDPROC	UnexpectedInt

ENDPROGRAM	ISR
