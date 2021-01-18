
POST02_VidOff	PROC
		; Enter via fall-through from previous POST code

; ---------------------------------------------------------------------------
; Disable video output on both MDA and CGA adaptors
		mov	al, 11h			; MDA: high resolution, disable display
						; CGA: 80x25 text mode, video disabled
		mov	dx, PORT_MDA_MODE_CTRL	; disable display until 6845 is initialized; the
						; IBM 5151 monitor can be damaged by out of range HSYNC input
						; (I know the GRiD 1520 doesn't have an MDA-compatible output,
						; but this is probably from the generic Phoenix BIOS)
		out	dx, al

		; Reuse the port number in DX and the value in AL to disable the CGA
		; adapter (upper byte of CGA port number is the same as the MDA port)
		mov	dl, PORT_CGA_MODE_CTRL & 0FFh
		out	dx, al

		; Delay loop (why is this needed? where did the loop count come from?)
		; Note that CL still has AAh in from the CPU test, so this actually
		; loops 51,370 times instead of 51,200.
		mov	ch, 200
		loop	$

		; Exit via fall-through to next POST procedure
		ENDPROC POST02_VidOff

