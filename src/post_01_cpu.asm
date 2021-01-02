
POST01_Cpu	PROC
		; Enter via fall-through from reset handler

; ---------------------------------------------------------------------------
; CPU register test
; Seems to be left over from the original Phoenix BIOS, since we've already
; used a bunch of registers checking for protected mode recovery.  I suppose
; it might catch a CPU that's bad in one or two bits we've not touched yet...
		mov	al, CHECKPOINT_CPU_REGS
		out	PORT_DIAGNOSTICS, al

		mov	ax, 55AAh		; load alternating bit pattern to test registers
.testRegisters	mov	ss, ax			; cycle the test pattern through all registers
		mov	si, ss
		mov	bx, si,CODE=LONG
		mov	ds, bx
		mov	di, ds
		mov	cx, di,CODE=LONG
		mov	es, cx
		mov	bp, es
		mov	dx, bp,CODE=LONG
		mov	sp, dx,CODE=LONG
		cmp	ax, sp,CODE=LONG	; did the test pattern survive all register transfers?
		jz	.registersOk		; if so, continue with POST
		hlt				; if not, we can't continue.
						; since interrupts are disabled the system is soft-locked
						; and the user will see an apparently 'dead' computer

.registersOk	not	ax			; invert the test pattern
		cmp	ax, 55AAh		; do we have the original value (i.e. inverted twice)?
		jnz	.testRegisters		; if not, redo the test to check registers can hold inverted bits

		; Exit via fall-through to next POST procedure
		ENDPROC	POST01_Cpu

