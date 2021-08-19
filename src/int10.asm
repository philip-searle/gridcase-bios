
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

; ---------------------------------------------------------------------
; VidSetMode [TechRef 7-11]
; Sets the video mode according to the value in AL.  Note that it does
; not clear the screen if the high bit of AL is set -- that behaviour
; was only introduced with the EGA.
; ---------------------------------------------------------------------
VidSetMode	PROC
		; Determine whether we have a CGA or MDA adaptor.
		mov	ah, [EquipmentWord]	; load low byte
		and	ah, 30h			; mask display type bits
		cmp	ah, 30h			; both set means MDA
		jnz	.notMda

		; MDA adapter only supports one video mode
		mov	al, VID_MDA_MODE
		jmp	.determinedMode
		FillerNop

.notMda		; We have two different init methods depending on whether
		; the caller requested a standard CGA mode or one of the
		; GRiD extended modes.
		cmp	al, VID_LAST_CGA_MODE
		jbe	.stdCgaMode
		cmp	al, VID_MODE_EXT_40
		jz	.extendedMode
		cmp	al, VID_MODE_EXT_48
		jz	.extendedMode
		cmp	al, VID_MODE_EXT_74
		jz	.extendedMode

		; Not a supported mode, so map it to the 'safest' standard
		; mode: 80x25, colorburst disabled (greyscale) .  This seems to
		; have been chosen based on an IBM PC's minimum useful
		; configuration: Cassette BASIC on a CGA's composite output.
		mov	al, VID_MODE_CGA_2
		jmp	.determinedMode
		FillerNop

.extendedMode	; The Yamaha V6366 video controller supports a 640x400 high
		; resolution mode.  Try initializing it if we have the
		; appropriate screen attached, otherwise fall back to the
		; highest standard CGA resolution.
		; Store the actual mode number in the BDA but keep AL set to
		; a CGA-compatible graphics mode so the rest of this function
		; doesn't need any changes (there is code that assumes AL>6
		; implies an MDA).
		mov	[VidActiveMode], al
		call	GridVidInitHi
		mov	al, VID_MODE_CGA_6
		jc	.determinedMode
		jmp	.vidInitDone
		FillerNop

.stdCgaMode	; Initialize the Yamaha V6366 video controller to support the
		; maximum CGA resolution of 640x200.
		mov	[VidActiveMode], al
		push	ax
		call	GridVidInit
		pop	ax
		cmp	al, VID_LAST_CGA_MODE
		jbe	.determinedMode
		; Above check is unnecessary -- we checked AL was a valid CGA
		; video mode earlier.  Likely left over from before support was
		; added for AT&T 6300/Toshiba T3100 extended video modes.
		; This fallback to a 'safe' mode is now dead code:
		mov	al, VID_MODE_CGA_2

.determinedMode	mov	[VidActiveMode], al
.vidInitDone	; Move CRTC address port number into DX and Mode Control port
		; value into AL.  Adjust depending on adapter type.
		cmp	al, VID_MDA_MODE	; are we an MDA controller?
		mov_	ah, al			; video mode in AH for later
		mov	dx, PORT_CGA_CRTC_ADDR
		mov	al, 35h			; CGA mode control byte
		jb	.gotCrtcPort
		mov	dl, (PORT_MDA_CRTC_ADDR & 0FFh)
		mov	al, 21h			; MDA mode control byte

.gotCrtcPort	; Save CRTC address port in BDA, initialize active page
		; variables in BDA, and calculate the new video mode's offset
		; in the video parameter table.
		mov	[VidBasePort], dx
		xor_	si, si
		mov	[VidPageOffset], si

		mov	es, si			; video param table ptr is in IVT
		les	si, [es:IvtVidParms]
		xor_	bx, bx			; BX will be our offset into the
						; refresh buffer length table

		cmp	ah, VID_MODE_CGA_2
		jb	.gotParmOffset
		add	bl, 2
		add	si, 16

		cmp	ah, VID_MODE_CGA_4
		jb	.gotParmOffset
		add	bl, 2
		add	si, 16

		cmp	ah, VID_MODE_MDA_7
		jnz	.gotParmOffset
		add	bl, 2
		add	si, 16

.gotParmOffset	; Set video Mode Control register
		push	dx
		add	dx, VID_MODE_PORT_OFFSET
		out	dx, al
		pop	dx

		; Entries in the video parameter table are stored in reverse
		; order, so advance our pointer to the end of the new mode's
		; entries and we will load them 'backwards'.
		mov	cx, 16
		add_	si, cx
		mov	[VidActivePage], ch	; set it to zero

		; Load all 16 values into the CRTC registers
.loadCrtcReg	mov_	al, cl			; load register index
		dec	al			; convert to ordinal
		out	dx, al			; select CRTC register
		inc	dx			; advance to CRTC data register
		dec	si			; next param table entry
		mov	al, [es:si]		; load table entry
		out	dx, al			; set CRTC register value
		dec	dx			; go back to CRTC address register
		loop	.loadCrtcReg

		; Now SI is pointing at the start of the new mode's video
		; parameter table entry, we can load the cursor shape.
		mov	cx, [es:si+10]		; load cursor shape regs
		xchg	ch, cl			; swap to x86 endian
		mov	[VidCursorShape], cx

		; Populate remaining mode-dependent BDA variables
		mov	cx, [cs:bx+VidRegenLengths]
		mov	[VidPageSize], cx
		mov_	bl, ah			; replace with mode number
		mov	cl, [cs:bx+VidColumns]
		xor_	ch, ch
		mov	[VidTextColumns], cx
		mov	al, [cs:bx+VidModeSets]
		mov	[VidModeCtrl], al

		; Setup ES:DI to point to the refresh buffer and AX with the
		; 'blank screen' attribute/character pair, then fill the screen
		; with it.
		mov	ax, 0720h		; white on black space char
		cmp	[VidActiveMode], VID_FIRST_CGA_TEXT_MODE
		jl	.L1
		xor_	ax, ax			; graphics mode: clear to black
.L1		mov	bx, CGA_REGEN_SEG
		mov	cx, 2000h		; 8k CGA refresh buffer
		cmp	[VidActiveMode], VID_MDA_MODE
		jbe	.L2
		shl	cx, 1			; extended graphics modes have
						; double the refresh buffer
.L2		cmp	[VidActiveMode], VID_MDA_MODE
		jnz	.L3
		mov	ax, 0720h
		mov	bh, (MDA_REGEN_SEG >> 8)
		mov	ch, 8			; 2k MDA refresh buffer

.L3		mov	es, bx
		xor_	di, di
		rep stosw			; clear the refresh buffer

		; Zero all page cursor positions
		mov	ax, ds
		mov	es, ax
		xor_	ax, ax
		mov	di, VidCursorPos
		mov	cx, 8
		rep stosw

		; If using a CGA mode, set the Color Control Register.
		; BUG: this seems to not set it if running in an extended
		; mode?
		add	dx, VID_CCR_PORT_OFFSET
		mov	al, 30h			; CGA CCR:
						;   magenta/cyan/white palette,
						;   bright foreground (320x200)
						;   black overscan (text)/black
						;   overscan+bg (320x200)
		cmp	[VidActiveMode], VID_LAST_CGA_MODE
		ja	.storePalette		; skip CCR write for MDA
		jl	.writeCgaCcr
		mov	al, 3Fh			; if 640x200 mode, white fg

.writeCgaCcr	out	dx, al
.storePalette	mov	[VidColorPalette], al
		dec	dx			; decrement to mode control reg

		; Very odd bit here: we check whether the 'display enable' bit
		; is zero with a timeout of 64k loops.  If not, we XOR the mode
		; control register to toggle the graphics/text, high-resolution,
		; and blink-enable bits.  We then have a VERY long delay before
		; resetting them to the desired values.
		; Maybe this is to work around some rare bug in the Yamaha
		; V6366 chip where it sometimes needs 'resetting' after a
		; mode change?
		push	dx
		add	dx, 2			; advance to status reg
		mov	cx, 0			; maximum loops
.waitDispEnOk	in	al, dx			; read status port
		test	al, 1			; display enable clear?
		jz	.dispEnOk
		loop	.waitDispEnOk

		pop	dx			; return to mode ctrl reg
		mov	al, [VidModeCtrl]
		mov	cl, 33h
		xor_	al, cl
		out	dx, al			; toggle some bits
		xor_	al, cl

		mov	cx, 0			; maximum delay
		loop	$
		mov	cx, 4000h		; even more delay
		loop	$
		jmp	.leaveFunction

.dispEnOk	pop	dx			; return to mode ctrl reg
		mov	al, [VidModeCtrl]

.leaveFunction	out	dx, al			; set mode ctrl reg
		jmp	VidRetn
		ENDPROC	VidSetMode

