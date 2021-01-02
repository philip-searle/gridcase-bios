
POST06_InitDma	PROC
		; Enter via fall-through from previous POST code

; ---------------------------------------------------------------------------
; Initialize DMA controllers and test channel 0 registers
		mov	al, CHECKPOINT_DMA
		out	PORT_DIAGNOSTICS, al

		; Any value written to a master clear port resets the controller
		out	PORT_DMA1_MASTER_CLEAR, al
		out	PORT_DMA2_MASTER_CLEAR, al

		; Setup DMA controllers: memory-to-memory transfer disabled,
		; channel 0 address hold disabled, controller enabled, normal
		; timing, fixed priority, late write selection, DREQ sense
		; active high, DACK sense active low
		mov	al, 0
		out	PORT_DMA1_COMMAND, al
		out	PORT_DMA2_COMMAND, al

		; Test that DMA channel 0 can write/read 0 and 1 bits in all
		; locations for current and base address registers.
		mov	dx, PORT_DMA_CHAN0_CUR
		mov	bx, 8000h		; test pattern: 1 bit set
		mov	cx, 40h			; 40h iterations: test bits into 2x16bit registers
						; (DMA channel 0 current address+base) normal and inverted

.dmaZeroTest	test	cl, 1
		jz	.l4			; odd iterations test the base address
		inc	dx			; so increment port to PORT_DMA_CHAN0_BASE

.l4		mov	al, bh,CODE=LONG
		out	dx, al			; output test pattern for register upper byte
		xchg	ax, bx			; xchg to get test pattern lower byte into AL
		Delay	2
		out	dx, al			; output test pattern for register lower byte
		xchg	ax, bx			; xchg to get test pattern back into BX for next loop
		Delay	2

		in	al, dx			; read back register value hibyte
		xchg	ah, al			; shift into AH
		Delay	2
		in	al, dx			; read back register value lobyte
		cmp	bx, ax,CODE=LONG	; value read back matches test pattern?
		jnz	.dmaZeroFail		; if not, report failure

		test	cl, 1			; odd iteration (DMA base register)?
		jz	.l5
		dec	dx			; reset port for next iteration after testing base register
		sar	bx, 1			; shift test pattern over by 1 bit

.l5		cmp	cl, 21h			; are we halfway through the test cycle?
		jnz	.l6
		mov	bh, 7Fh			; if so, switch to inverted test pattern

.l6		loop	.dmaZeroTest		; loop until all bits tested
		jmp	.dmaZeroOk		; then continue successfully

.dmaZeroFail	mov	al, BEEP_DMA_FAIL
		jmp	FatalBeeps

; ---------------------------------------------------------------------------
; Set DMA channel modes
.dmaZeroOk	mov	dx, PORT_DMA1_MODE

.setDmaModes	mov	al, 40h			; DMA mode: channel 0, verify mode, single transfer mode
		mov	cx, 4			; process all four channels on the DMA controller

.setDmaMode	out	dx, al			; set mode for one channel
		inc	al			; advance mode to next channel
		loop	.setDmaMode

		; Advance port to update DMA2 controller modes
		add	dl, PORT_DMA2_MODE - PORT_DMA1_MODE
		jnb	.setDmaModes		; go again for second controller

		; Setup cascade from second DMA controller
		mov	al, 0C0h		; DMA2 mode: channel 4 to cascade mode
		out	PORT_DMA2_MODE, al
		mov	al, 0			; DMA2: enable channel 4
		out	PORT_DMA2_MASK, al

; ---------------------------------------------------------------------------
; Set DMA channel address registers
		mov	al, CHECKPOINT_DMA_PR
		out	PORT_DIAGNOSTICS, al

		jmp	.l7			; jump over data array
.dmaPagePorts	db	PORT_DMA_PAGE0
		db	PORT_DMA_PAGE1
		db	PORT_DMA_PAGE2
		db	PORT_DMA_PAGE3
		; 	PORT_DMA_PAGE4 not set because it's used for the cascade feature
		db	PORT_DMA_PAGE5
		db	PORT_DMA_PAGE6
		db	PORT_DMA_PAGE7

		; Set all page registers, then verify they read back OK
.l7		mov	dx, cs
		mov	ds, dx
		mov	bl, 0FFh		; set address registers to all-ones
.pagePortsTest	mov	di, .setPageReg

.pagePortsLoop	mov	si, .dmaPagePorts
		mov	cx, 7			; seven page registers to set
		mov	dh, 0			; clear upper half of port number
.nextPageReg	lodsb				; load page register port
		mov	dl, al,CODE=LONG	; merge into DX
		jmp	di			; jump to test handler

.setPageReg	mov	al, bl,CODE=LONG
		out	dx, al			; set DMA page register
		jmp	.l8

.checkPageReg	in	al, dx			; read DMA page register
		cmp	al, bl,CODE=LONG			; matches what we wrote?
		jz	.l8			; continue if so
		mov	al, BEEP_DMA_PR_FAIL	; report error if not
		jmp	FatalBeeps

.l8		loop	.nextPageReg		; loop over all page registers

		cmp	di, .checkPageReg	; are we on the verification pass?
		jz	.dmaPageCheckOk		; continue with POST if so
		mov	di, .checkPageReg	; rerun loop with verification instead of setting if not
		jmp	.pagePortsLoop

.dmaPageCheckOk	xor	bl, 0FFh		; invert test pattern
		jz	.pagePortsTest		; rerun tests with all-zero pattern before continuing

		; Exit via fall-through to next POST procedure
		ENDPROC POST06_InitDma

