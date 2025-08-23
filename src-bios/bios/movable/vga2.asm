; vga2.asm
; VGA routines related to option ROM identification

; ---------------------------------------------------------------------------
; VgaRomLocation
; Returns the location and length of the VGA option ROM.
; On return:
;   ES:DI -> VGA option ROM start
;      CX == Option ROM length
; ---------------------------------------------------------------------------
VgaRomLocation	PROC
		; Hardcode ROM location for GRiD VGA board
		mov	ax, VGA_ROM_SEG
		mov	es, ax
		xor_	di, di
		mov	cx, VGA_ROM_LEN
		retn
		ENDPROC	VgaRomLocation

; ---------------------------------------------------------------------------
; VgaCheckPh
; Checks whether the VGA option ROM has the expected 'Ph' signature.
; On return:
;   ZF set if ROM signature matches
; ---------------------------------------------------------------------------
VgaCheckPh	PROC
		mov	ax, VGA_ROM_SEG
		mov	es, ax
		mov	di, 2Dh	; ??? which ROM is this checking for?
		cmp	[es:di], 'Ph',DATA=WORD
		retn
		ENDPROC	VgaCheckPh
