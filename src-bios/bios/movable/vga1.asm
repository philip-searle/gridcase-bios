; ===========================================================================
; vga1.asm
; VGA identify functions
; ===========================================================================

; ---------------------------------------------------------------------------
; VgaIdentify
; On return:
;   CF set == no VGA present
;   CF clear:
;     AX == VGA type
; ---------------------------------------------------------------------------
VgaIdentify	PROC
		push	cx
		push	si
		push	di
		push	ds
		push	es
		pushf

		; Check for presence of option ROM
		mov	ax, VGA_ROM_SEG
		mov	ds, ax
		xor_	si, si
		cmp	[si], 0AA55h,DATA=WORD	; VGA option ROM present?
		jz	.builtInVgaRom

		; No VGA option ROM present
		mov	ax, VGA_TYPE_0
		popf
		jmp	.leaveFunction
		FillerNop

.builtInVgaRom	; Find start of copyright string
		call	VgaRomLocation
		cld
.findCopyright	; Scan ROM for "Cirrus Logic" text
		mov	al, 'C'
		repne scasb
		jnz	.notCirrusLogic
		push	cx
		push	di
		push	cs
		pop	ds	; DS == CS
		mov	si, kCirrusLogic
		mov	cx, SIZE#kCirrusLogic
		FillerNop
		repe cmpsb
		pop	di
		pop	cx
		jnz	.findCopyright

.isType4	mov	al, VGA_TYPE_4
		jmp	.knowType

.notCirrusLogic	; Not a Cirrus Logic BIOS, but maybe a Phoenix variant?
		call	VgaCheckPh
		jnz	.notType4
		jmp	.isType4

.notType4	; Not type four; check for types one and two
		call	VgaRomLocation
		cld
.findGridId	mov	ax, 1FEh	; GRiD VGA BIOS ID starts with magic number
		repne scasw
		jnz	.isType3	; No GRiD ID?  Must be type three

		; Magic number found, now check for GRiD text
		push	cx
		push	di
		push	cs
		pop	ds	; DS == CS
		mov	si, kGridVgaText
		mov	cx, SIZE#kGridVgaText
		FillerNop
		repe cmpsb
		pop	di
		pop	cx
		jnz	.findGridId

		; GRiD text found, now see if the ROM
		; also contains the text '545 VGA '.
		mov_	cx, di
		xor_	di, di
		mov	ax, '45'
.checkType2	repne scasw
		jnz	.isType1
		cmp	[es:di], '5 ',DATA=WORD
		jnz	.checkType2
		cmp	[es:di+2], 'VG',DATA=WORD
		jnz	.checkType2
		cmp	[es:di+4], 'A ',DATA=WORD
		jnz	.checkType2
		mov	al, VGA_TYPE_2
		jmp	.knowType

.isType1	mov	al, VGA_TYPE_1
		jmp	.knowType

.isType3	mov	al, VGA_TYPE_3
		; fallthrough to knowType

.knowType	popf
		xor_	ah, ah
.leaveFunction	clc			; assume success
		cmp	al, VGA_TYPE_0	; did we fail?
		jnz	.l1
		stc			; flag if so
.l1		pop	es
		pop	ds
		pop	di
		pop	si
		pop	cx
		retn
		ENDPROC	VgaIdentify

kGridVgaText	d b	'GRiD Systems Corp'
kCirrusLogic	d b	'irrus Logic'

; ---------------------------------------------------------------------------
; VgaRetnGuard
; Checks whether the VGA controller supports LCD colour mapping, and if not,
; tweaks the stack so the caller is returned from immediately.
; ---------------------------------------------------------------------------
VgaRetnGuard	PROC
		push	ax
		push	dx
		call	VgaGetFlags
		test	dl, VGA_FLAGS_08
		pop	dx
		pop	ax
		jz	.supportsColMap
		add	sp, 2		; Return to caller's caller
		retn
.supportsColMap	retn
		ENDPROC

; ---------------------------------------------------------------------------
; sub_FE38A
; ??? Unused?
; On entry:
;   DL == 1 Enable external display
; ---------------------------------------------------------------------------
sub_FE38A	PROC
		push	ax
		call	VgaIdentify
		cmp	al, VGA_TYPE_4
		pop	ax
		jz	.isType4
		retn

.isType4	push	bx
		push	dx
		push	ds
		cmp	dl, 1
		mov	dx, PORT_VID_EXTERNAL
		jz	.switchInternal

		; Switch to external display
		mov	al, VID_EXTERNAL_ENABLE
		out	dx, al
		mov	ax, 1201h	; Cirrus Logic BIOS - LCD panel - SWITCH DISPLAY
		mov	bl, 92h
		int	10h

.updateInverse	mov	ds, [cs:kBdaSegment]
		mov	al, [VgaByteUnknown3]
		call	VgaIsExternal
		mov	ah, 12h		; Cirrus Logic BIOS - LCD panel - EN/DISABLE REVERSE VIDEO MODE
		mov	bl, 89h
		int	10h

		pop	ds
		pop	dx
		pop	bx
		xor_	ax, ax		; return success
		add	sp, 2		; skip caller and return to their caller
		retn

.switchInternal	mov	al, 1
		out	dx, al
		mov	ax, 1200h	; Cirrus Logic BIOS - LCD panel - SWITCH DISPLAY
		mov	bl, 92h
		int	10h
		jmp	.updateInverse
		ENDPROC	sub_FE38A

; ---------------------------------------------------------------------------
; sub_FE3CF
; ??? Unused?
; ---------------------------------------------------------------------------
sub_FE3CF	PROC
		push	ax
		call	VgaIdentify
		cmp	al, VGA_TYPE_4
		pop	ax
		jz	.isType4
		retn

.isType4	push	ax
		push	dx
		mov	dx, PORT_VID_ROMSTATUS
		in	al, dx
		mov	dl, 1		; enable external display
		test	al, VID_TYPE_EXTERNAL
		jz	.l1
		mov	dl, 0		; disable external display
.l1		mov	ax, .l2		; compensate for sub_FE38A skipping
		push	ax		; a level in the return stack (???)
		call	sub_FE38A
.l2		pop	dx
		pop	ax
		add	sp, 2		; skip caller and return to their caller
		retn
		ENDPROC	sub_FE3CF
