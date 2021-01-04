
POST16_VidInit	PROC
		; Enter via fall-through from previous POST code

; ---------------------------------------------------------------------------
; Initialize internal video display panel
		mov	ds, [cs:kBdaSegment]
		push	ax
		push	bx
		push	cx
		push	dx
		push	si
		push	di
		push	es
		push	ds
		call	VidInit
		call	VidInitBacklite
		pop	ds
		pop	es
		pop	di
		pop	si
		pop	dx
		pop	cx
		pop	bx
		pop	ax

		; Exit via fall-through to next POST procedure
		ENDPROC POST16_VidInit

