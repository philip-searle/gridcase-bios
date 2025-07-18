
POST09_DmaReg	PROC
		; Enter via fall-through from previous POST code

; ---------------------------------------------------------------------------
; Test DMA controllers
; Assumes that all channel registers for a controller are at regular offsets
; from each other.  Does not test the first channel because it is either used
; for RAM refresh (DMA1) or the cascade feature (DMA2).

		; Test DMA1 registers
		mov	al, CHECKPOINT_DMA1
		out	PORT_DIAGNOSTICS, al
		mov	dx, PORT_DMA_CHAN1_BASE
		mov	cx, 6			; 2 registers x 3 channels
		mov	si, PORT_DMA_CHAN2_BASE - PORT_DMA_CHAN1_WC
		call	TestDmaRegs
		jnb	.testDma1Ok
		mov	al, BEEP_DMA1_REG
		jmp	FatalBeeps

		; Test DMA2 registers
.testDma1Ok	mov	al, CHECKPOINT_DMA2
		out	PORT_DIAGNOSTICS, al
		mov	dx, PORT_DMA_CHAN5_BASE
		mov	cx, 6			; 2 registers x 3 channels
		mov	si, PORT_DMA_CHAN6_BASE - PORT_DMA_CHAN5_WC
		call	TestDmaRegs
		jnb	.testDma2Ok
		mov	al, BEEP_DMA2_REG
		jmp	FatalBeeps

.testDma2Ok	; Exit via fall-through to next POST procedure
		ENDPROC POST09_DmaReg

