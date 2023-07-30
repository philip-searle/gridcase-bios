
POST10_PicReg	PROC
		; Enter via fall-through from previous POST code
; ---------------------------------------------------------------------------
; Test PIC mask registers

		; Test PIC1 mask register
		mov	al, CHECKPOINT_PIC1
		out	PORT_DIAGNOSTICS, al
		mov	dx, PORT_PIC1_MASK
		call	TestPicMaskReg
		jz	.testPic1Ok
		mov	al, BEEP_PIC1_REG
		jmp	FatalBeeps

		; Test PIC2 mask register
.testPic1Ok	mov	al, CHECKPOINT_PIC2
		out	PORT_DIAGNOSTICS, al
		mov	dx, PORT_PIC2_MASK
		call	TestPicMaskReg
		jz	.testPic2Ok
		mov	al, BEEP_PIC2_REG
		jmp	FatalBeeps

.testPic2Ok	; Exit via fall-through to next POST procedure
		ENDPROC POST10_PicReg

