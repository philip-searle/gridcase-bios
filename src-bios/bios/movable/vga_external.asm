; ---------------------------------------------------------------------------
; VgaIsExternal
; On return:
;   AX == bit 0 inverted if external display is enabled
; ---------------------------------------------------------------------------
VgaIsExternal	PROC
		push	dx
		push	ax
		mov	dx, PORT_VID_ROMSTATUS
		in	al, dx
		pop	dx
		test	al, VID_TYPE_EXTERNAL
		jnz	.isExternal
		xor	dl, 1
.isExternal	mov_	al, dl
		pop	dx
		retn
		ENDPROC	VgaIsExternal
