
Int10.handlers		dw	VidSetMode
			dw	VidSetCursor
			dw	VidSetCursorPos
			dw	VidGetCursorPos
			dw	VidReadLightPen
			dw	VidWinSelect
			dw	VidWinScrollUp
			dw	VidWinScrollDn
			dw	VidReadCell
			dw	VidWriteCell
			dw	VidWriteChar
			dw	VidSetPalette
			dw	VidWritePel
			dw	VidReadPel
			dw	VidTeletype
			dw	VidGetDispState
			dw	VidReserved
			dw	VidReserved
			dw	VidReserved
Int10.handlersLast	dw	VidWriteString

; ---------------------------------------------------------------------
; Int10_Actual [TechRef 7-10]
; Provides BIOS video services.
; ---------------------------------------------------------------------
Int10_Actual	PROC
		call	MakeIsrStack
		mov	di, Int10.handlersLast - Int10.handlers
		call	FuncToOffset
		jc	VidReserved
		cld
		jmp	[cs:di+Int10.handlers]
		ENDPROC	Int10_Actual

; ---------------------------------------------------------------------
; VidReadCell2
; Reads a character/attribute pair from display memory.  If using a CGA
; adaptor in 80x25 text mode, also waits for non-display time to avoid
; 'snow' on the display.
;
; On entry:
;   BH == page index
;   DH == cell row
;   DL == cell column
; On return:
;   AL == character code
;   AH == attribute code
;   DX == undefined
;   ES == refresh buffer segment
; ---------------------------------------------------------------------
VidReadCell2	PROC
		push	bx

		; Convert cursor position to offset in DI, get refresh
		; buffer segment in ES.
		call	VidCalcOffsets

		; Do we need to work around CGA 'snow'?
		; CGA lacks bandwidth in 80x25 text modes for CPU/display
		; interleaving of memory accesses to work, so in these modes
		; (modes 2 and 3) we must wait for the display hardware to
		; not be accessing the refresh buffer.
		mov	al, [VidActiveMode]
		and	al, ~1
		cmp	al, 2			; check for modes 2/3

		; Get video adapter base port and repoint DS to refresh buffer
		mov	dx, [VidBasePort]
		push	ds
		push	es
		pop	ds

		jnz	.readCell		; skip 'snow' code if we can

		; Wait for CGA to access refresh buffer then delay our memory
		; access until we detect it *not* accessing the refresh buffer.
		; This gives us the largest margin for our memory fetch to
		; occur without overlapping the next CGA access.
		add	dx, VID_STATUS_PORT_OFFSET
.waitCgaBusy	in	al, dx
		ror	al, 1			; move 'display active' to CF
		jc	.waitCgaBusy
		cli				; interrupts will only throw
						; off our timing, disable them
.waitCgaNotBusy	in	al, dx
		ror	al, 1			; get 'display active' bit
		jnc	.waitCgaNotBusy

.readCell	; Read from refresh buffer, re-enable interrupts
		mov	ax, [di]
		sti
		pop	ds
		pop	bx
		retn
		ENDPROC	VidReadCell2

; ---------------------------------------------------------------------
; VidReserved
; Do-nothing placeholder for video functions not supported by MDA/CGA.
; ---------------------------------------------------------------------
VidReserved	PROC
		jmp	UnmakeIsrStack
		ENDPROC	VidReserved

; ---------------------------------------------------------------------
; Constants for the video refresh buffer segments.
; ---------------------------------------------------------------------
kMdaRefreshSeg	dw	0B000h
kCgaRefreshSeg	dw	0B800h

; ---------------------------------------------------------------------
; VidReserved2
; Dead code - forwards to VidReserved, unreferenced.
; ---------------------------------------------------------------------
VidReserved2	PROC
		jmp	VidReserved
		ENDPROC	VidReserved2

