
POST14_VidInit	PROC
		; Enter via fall-through from previous POST code

; ---------------------------------------------------------------------------
; Determine initial video mode and initialize adapter
		mov	ds, [cs:kBdaSegment]
		mov	al, CHECKPOINT_VIDEO
		out	PORT_DIAGNOSTICS, al

		; Assume default mode based on InterruptFlag?
		; TODO: how is this set?
		mov	[EquipmentWord], 30h,DATA=BYTE	; 80x25 monochrome
		test	[InterruptFlag], 40h	; ???
		jnz	.l1
		mov	[EquipmentWord], 20h,DATA=BYTE	; 80x25 colour
.l1		; EquipmentWord now set

		%IF	BIOS_VERSION = 19891025
			; VGA module support
			push	ax
			call	VgaIdentify
			cmp	al, VGA_TYPE_2
			jz	.removeInt10Isr
			cmp	al, VGA_TYPE_4
			jz	.removeInt10Isr
			cmp	al, VGA_TYPE_0
			jz	.vgaHandled
			pop	ax
			jmp	.l2
			jmp	.findVidRom	; Unnecessary jmp???

.removeInt10Isr		push	ds
			xor_	ax, ax
			mov	ds, ax
			mov	[IvtInt10], SoftwareIret,DATA=WORD
			pop	ds
.vgaHandled		pop	ax
		%ENDIF

		; Check for an option ROM present anywhere between C000:0000
		; and C8000:0000, at 2KB boundaries.  If found, then assume it's
		; an EGA+ video adapter and update the default video mode.
		mov	si, 0C000h
		mov	cx, 10h
.findVidRom	mov	ds, si
		add	si, 80h
		cmp	[0], 0AA55h,DATA=WORD	; check for option ROM signature
		loopne	.findVidRom
		mov	ds, [cs:kBdaSegment]
		jnz	.l2
		and	[EquipmentWord], 0CFh,DATA=BYTE	; set video equipment to EGA+

.l2		mov	al, CMOS_STATUS_DIAG | NMI_DISABLE
		call	ReadCmos
		mov_	bl, al
		test	al, 0C0h		; invalid config or CMOS checksum bad?
		jnz	.loc_F8664

		; Update CMOS equipment byte with video mode
		mov	al, CMOS_EQUIPMENT | NMI_DISABLE
		call	ReadCmos
		mov_	bh, al
		and	al, 30h			; isolate CMOS equipment video bits
		cmp	[EquipmentWord], al
		jz	.loc_F8653
		cmp	al, 10h
		jnz	.loc_F865B
		cmp	[EquipmentWord], 20h,DATA=BYTE
		jnz	.loc_F865B
		and	[EquipmentWord], 0CFh,DATA=BYTE
		or	[EquipmentWord], al
.loc_F8653	test	bh, 1
		jnz	.loc_F8664
		jmp	.loc_F8669
		FillerNop

.loc_F865B	mov_	al, bl
		or	al, 20h
		mov	ah, CMOS_STATUS_DIAG | NMI_DISABLE
		call	WriteCmos

.loc_F8664	or	[EquipmentWord], 1,DATA=BYTE

.loc_F8669	sti
		test	[InterruptFlag], 20h
		jnz	.loc_F8674
		jmp	.vidInitFail
		FillerNop

		; Init the display now we know what type it is
.loc_F8674	mov	al, CHECKPOINT_VIDEO_INIT
		out	PORT_DIAGNOSTICS, al

		; TODO: why is equipmentWord modified here?
		push	[EquipmentWord]		; save original equipment word
		mov	[EquipmentWord], 30h,DATA=BYTE	; set just reserved bits in equipment word
		xor_	ax, ax			; mode 0: 40x25 CGA text
		int	10h			; set video mode
		mov	[EquipmentWord], 10h,DATA=BYTE	; set just one reserved bit in equipment word
		mov	ax, 3			; mode 3: 80x25 CGA text
		int	10h			; set video mode
		pop	ax
		mov	[EquipmentWord], ax
		test	al, 30h			; check int10/00 return
		jz	.vidInitFail
		%IF	BIOS_VERSION = 19891025
			; VGA module support
			push	ax
			push	dx
			call	VgaGetFlags
			test	dl, VGA_FLAGS_10	; ??? VGA
			pop	dx
			pop	ax
			jnz	.vidInitDone
		%ENDIF

		call	VidTestMem
		jnb	.vidInitDone
		mov	al, [EquipmentWord]
		and	al, 10h			; create xor mask from CGA/MDA to MDA/CGA
		add	al, 10h
		xor	[EquipmentWord], al	; invert default video mode
		call	VidTestMem
		jb	.vidInitFail

		Inline	ConString,'Display adapter failed; using alternate',0Dh,0Ah,0
		call	SetCriticalErr
		jmp	.vidInitDone

.vidInitFail	xor_	ax, ax
		mov	ds, ax
		mov	[IvtInt10], SoftwareIret,DATA=WORD
		%IF	BIOS_VERSION = 19891025
			; Disable VGA???
			call	VgaUnknown2
		%ENDIF
.vidInitDone	; Exit via fall-through to next POST procedure
		ENDPROC POST14_VidInit

