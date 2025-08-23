POST00_Vga	PROC
		; Enter via fall-through from reset handler
		%IF	BIOS_VERSION = 19891025
		; Only latest BIOS supports VGA module

; ---------------------------------------------------------------------------
; VGA module reset code


		mov	ax, VGA_ROM_SEG
		mov	es, ax
		xor_	di, di
		cmp	[es:di], 0AA55h,DATA=WORD	; Option ROM present?
		jnz	.vgaDone			; Assume VGA module if so

		mov	bx, PORT_VGA_ADAPTOR_ENABLE
		mov_	dx, bx
		mov	al, VGA_ADAPTOR_SETUP | VGA_ADAPTOR_ENABLE | VGA_ADAPTOR_UNKNOWN1 | VGA_ADAPTOR_UNKNOWN2
		out	dx, al

		mov	al, POS_SYSTEM_BOARD_UNKNOWN1
		mov	dx, PORT_POS_SYSTEM_BOARD
		out	dx, al

		mov	dx, PORT_VGA_SUBSYS_ENABLE
		mov	al, VGA_SUBSYS_DISABLE
		out	dx, al

		mov	dx, PORT_VGA_GLOBAL_ENABLE
		mov	al, VGA_GLOBAL_DISABLE
		out	dx, al

		mov_	dx, bx			; reset to PORT_VGA_ADAPTOR_ENABLE
		out	dx, al			; write VGA_ADAPTOR_DISABLE

.vgaDone	mov	ax, BDA_SEGMENT
		mov	ds, ax
		mov	[VgaByteUnknown1], 0

		%ENDIF
		; Exit via fall-through to next POST procedure
		ENDPROC	POST00_Vga
