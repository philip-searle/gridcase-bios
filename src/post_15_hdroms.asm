
POST15_HdOptRom	PROC
		; Enter via fall-through from previous POST code

; ---------------------------------------------------------------------------
; Locate any option ROMs in range C000:0000 to C800:0000 and call their
; initialization entrypoints if their checksum is valid.  Assume that any
; ROMs in this range are hard disk controllers.
		mov	ds, [cs:kBdaSegment]
		test	[interruptFlag], 20h	; do we have video output?
		jz	.romInitDone		; skip ROM init if not, it requires video for error messages

		mov	al, CHECKPOINT_HD_ROM
		out	PORT_DIAGNOSTICS, al

		mov	si, HD_ROM_SEG_LO
		mov	cx, HD_ROM_SEG_HI
		mov	dl, 1			; ???
		call	InitOptionRoms

.romInitDone	; Exit via fall-through to next POST procedure
		ENDPROC POST15_HdOptRom

