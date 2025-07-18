
INT10		PROGRAM	OutFile=int10.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"segments/bda.inc"
		include	"segments/ivt.inc"
		include	"isr.inc"
		include	"video.inc"

		EXTERN	Beep, FuncToOffset
		EXTERN	MakeIsrStack, UnmakeIsrStack, UnmakeIsrStack2
		EXTERN	GridVidInitHi, GridVidInitLo
		EXTERN	VidRegenLengths, VidColumns, VidModeSets
		EXTERN	VidReadLightpen
		EXTERN	GraphicsChars
		EXTERN	kCrLf, ConString2

		PUBLIC	Int10_Actual
		PUBLIC	ConChar, ConCrLf, ConString
		PUBLIC	VidIsTextMode, VidWriteString

; =====================================================================

Int10.handlers		dw	VidSetMode
			dw	VidSetCursor
			dw	VidSetCursorPos
			dw	VidGetCursorPos
			dw	VidReadLightpen
			dw	VidPageSelect
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
		call	VidPageOffsets

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
		Unused	VidReserved2
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
		call	GridVidInitLo
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
		cmp	[VidActiveMode], VID_FIRST_CGA_GRAP_MODE
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

		; ??? Very odd bit here: we check whether the 'display enable'
		; bit is zero with a timeout of 64k loops.  If not, we XOR the
		; mode control register to toggle the graphics/text,
		; high-resolution, and blink-enable bits.  We then have a VERY
		; long delay before  resetting them to the desired values.
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
		jmp	VidOutRetn

.dispEnOk	pop	dx			; return to mode ctrl reg
		mov	al, [VidModeCtrl]
		; fallthrough to VidOutRetn
		ENDPROC	VidSetMode

; ---------------------------------------------------------------------
; VidOutRetn
; Shared function tail that outputs the value in AL to port DX before
; jumping to VidRetn.
; ---------------------------------------------------------------------
VidOutRetn	PROC
		out	dx, al
		jmp	VidRetn
		ENDPROC	VidOutRetn

; ---------------------------------------------------------------------
; VidSetPalette [TechRef 7-13]
; Sets either border/background color or graphics palette, depending on
; the value of BL.
; ---------------------------------------------------------------------
VidSetPalette	PROC
		mov	dx, [VidBasePort]
		add	dx, VID_CCR_PORT_OFFSET
		mov	al, [VidColorPalette]

		; BH determines subfunction
		or_	bh, bh
		jnz	.setPalette

		; Set border/background color from AL
		and	al, 0E0h	; mask existing border/bg color
		and	bl, 1Fh		; mask argument
		or_	al, bl		; merge in new value
		jmp	.updatePalette

.setPalette	and	al, 0DFh	; mask existing palette bit
		and	bl, 1		; mask argument
		or_	bl, bl
		jz	.updatePalette
		or	al, 20h		; add palette bit back in

.updatePalette	mov	[VidColorPalette], al
		jmp	VidOutRetn
		ENDPROC	VidSetPalette

; ---------------------------------------------------------------------
; VidSetCursor [TechRef 7-11]
; Sets the start and end lines on which the text mode cursor is visible
; as well as its blink rate.
; ---------------------------------------------------------------------
VidSetCursor	PROC
		mov	bx, 0A0Bh	; CRTC cursor shape registers
		call	VidWriteCrtc2	; write both from CH/CL
		mov	[VidCursorShape], cx
		ENDPROC	VidSetCursor

; ---------------------------------------------------------------------
; VidRetn
; Shared function tail that returns from the int10 interrupt routine.
; ---------------------------------------------------------------------
VidRetn		PROC
		jmp	UnmakeIsrStack
		ENDPROC	VidRetn

; ---------------------------------------------------------------------
; VidGetCursorPos [TechRef 7-11]
; Returns the cursor start/end lines in CH/CL and the cursor position
; in DH/DL for the video page in BH.
; ---------------------------------------------------------------------
VidGetCursorPos	PROC
		cmp	[VidActiveMode], VID_FIRST_CGA_GRAP_MODE
		jb	.textMode

		; Graphics modes only have one page
		xor_	bh, bh

.textMode	mov_	bl, bh
		call	VidPageCursorPos
		mov_	bp, sp
		mov	[bp+IsrStack.dx], dx
		mov	dx, [VidCursorShape]
		mov	[bp+IsrStack.cx], dx
		jmp	VidRetn
		ENDPROC	VidGetCursorPos

; ---------------------------------------------------------------------
; VidPageSelect [TechRef 7-11]
; Switches the active video page.
; ---------------------------------------------------------------------
VidPageSelect	PROC
		; Graphics mode and MDA do not support multiple pages
		cmp	[VidActiveMode], VID_FIRST_CGA_GRAP_MODE
		jnb	.leaveFunction

		mov	[VidActivePage], al	; store new page index

		; Multiply VidActivePage by VidPageSize manually.
		; TODO: check whether this is faster than mul.
		xor_	ah, ah
		mov	bx, [VidPageSize]
		xor_	cx, cx
		inc	ax
		sub_	cx, bx
.mulPageSize	add_	cx, bx
		dec	ax
		jnz	.mulPageSize

		; Store calculated page offset to BDA and CRTC (CRTC counts in
		; character units, not bytes so divide by two).
		mov	[VidPageOffset], cx
		sar	cx, 1
		mov	bx, CRTC_RPAIR_STARTADDR
		call	VidWriteCrtc2

		; Update the cursor position to account for possibly changed
		; column count in the new video mode
		mov	bl, [VidActivePage]
		call	VidPageCursorPos
		call	VidUpdCrtcCurs

.leaveFunction	jmp	UnmakeIsrStack
		ENDPROC	VidPageSelect

; ---------------------------------------------------------------------
; VidReadCell [TechRef 7-12]
; Reads character and attribute at cursor position.
; ---------------------------------------------------------------------
VidReadCell	PROC
.kGrapCellSize	equ	8		; height of character cell
.kCgaBankStride	equ	2000h		; bytes between bank origins

		call	VidIsTextMode
		jc	.graphicsMode

		; Text mode is simple: one byte for character,
		; one for the attributes.
		call	VidReadCell2
		jmp	UnmakeIsrStack

.graphicsMode	; Graphics mode is complicated: we need to read the bitmap
		; representing the character at the cursor position and compare
		; it to each character in the ROM font as well as any extended
		; graphics font loaded.

		; Convert current cursor position into an offset to the
		; cursor location's top left pixel in the refresh buffer.
		xor_	bl, bl		; only one page in graphics mode
		call	VidPageCursorPos
		call	VidGrapCursorOffset
		mov	es, [cs:kCgaRefreshSeg]	; ES:DI -> character

		; Reserve some space on the stack to store a copy of the
		; bitmap from the refresh buffer.
		; DH will store the highest color index used in the character
		; and will be returned to the caller in AH (even though that
		; is documented as only valid in text mode).
		mov	cx, .kGrapCellSize
		sub	sp, .kGrapCellSize+2	; ??? why plus two?
		mov_	bp, sp
		xor_	dh, dh
		mov	bl, [VidActiveMode]	; we use this throughout

.extractBitmap	; Load a word from the refresh buffer and jump to the
		; appropriate code for the current video mode's bit depth.
		mov	ax, [es:di]
		cmp	bl, VID_MODE_CGA_6
		jnb	.extract1Bpp

		; Two bits per pixel needs a loop to convert to 1bpp
		xchg	ah, al			; word reads on byte data needs swapping
		mov	bh, 8			; eight pixels per word
		xor_	dl, dl			; DL will store extracted row

.extract2Bpp	; Make room for the next bit in DL, then shift out two bits
		; from AX.  If either bit is set then mark it DL and record
		; the largest pixel value seen so far in DH.
		shl	dl, 1
		shl	ax, 1
		jnc	.hiBitDone2Bpp
		or	dx, 0201h
.hiBitDone2Bpp	shl	ax, 1
		jnc	.loBitDone2Bpp
		or	dx, 0101h
.loBitDone2Bpp	dec	bh			; count pixel as done
		jnz	.extract2Bpp		; loop until row done
		mov_	al, dl			; store extracted pixels
		jmp	.extractedRow

.extract1Bpp	; 1bpp is a simple copy
		or_	al, al			; evaluate row
		jz	.extractedRow
		mov	dh, 1			; any bit set -> attribute byte

.extractedRow	; Adjust DI to point to the next row.  The CGA memory layout
		; makes this awkward: we need to alternate between the low
		; bank (0000h) and the high bank (2000h) on each row.
		; We use the low bit of the row number (CX) to jump to the
		; adjustment code.
		; The extended graphic modes (40h/48h/74h) complicate things
		; further -- they have two additional memory banks at 4000h and
		; 6000h. Code seems to have been patched in to special-case this,
		; which makes the code look worse than it really is.
		add	di, .kCgaBankStride	; advance to next bank
		cmp	bl, VID_MODE_EXT_48
		jnz	.notMode48

		; Mode 48h uses all four memory banks and only needs the offset
		; resetting to the first bank after every four rows.
		dec	cx
		test	cl, 3
		jnz	.L1
		sub	di, 7FB0h		; reset to bank 0
.L1		inc	cx
		jmp	.nextRow

.notMode48	; Modes other than 48h are simpler.
		; TODO: comment this properly...
		cmp	bl, VID_MODE_EXT_40
		jb	.notExtMode
		add	di, .kCgaBankStride
.notExtMode	test	cx, 1
		jz	.nextRow
		sub	di, 3FB0h
		cmp	bl, VID_MODE_EXT_40
		jb	.nextRow
		sub	di, 2 * .kCgaBankStride

.nextRow	inc	bp			; save extracted pixels
		mov	[bp+0], al
		loop	.extractBitmap

		; Adjust BP back to the start of th extracted pixel data and
		; prepare to compare it to the ROM font data.
		sub	bp, 7
		mov_	ah, dh			; return attributes in AH
		xor_	al, al			; AL will hold returned char

		; Setup for comparison of extracted pixel data on stack via
		; DS:SI against ROM font data via ES:DI.
		push	cs
		pop	es
		mov	di, GraphicsChars
		push	ss
		pop	ds

.compareChar	mov_	si, bp
		mov	cx, 4			; compare by words
		repe	cmpsw
		jz	.leaveFunction		; char matched?
		add_	di, cx			; skip unmatched data if not
		add_	di, cx
		inc	al			; try next character
		jns	.compareChar

		; Setup for comparison of extracted pixel data on stack via
		; DS:SI against extended font data (if any) via ES:DI.
		xor_	di, di
		mov	es, di
		les	di, [es:IvtVidGrapFont]
		mov	cx, es
		or_	cx, di			; evaluate flags
		jz	.noCharMatched		; is there extended font data?

.compHiChar	mov_	si, bp
		mov	cx, 4			; compare by words
		repe	cmpsw
		jz	.leaveFunction		; char matched?
		add_	di, cx			; skip unmatched data if not
		add_	di, cx
		inc	al			; try next character
		jnz	.compHiChar

.noCharMatched	xor_	ax, ax

.leaveFunction	add	sp, 0Ah
		jmp	VidRetn2
		ENDPROC	VidReadCell

; ---------------------------------------------------------------------
; VidWritePel [TechRef 7-13]
; Writes the value in AL to the pixel located at row/column DX/CX.  If
; the high-bit of AL is set the value will be XOR-ed instead of
; replacing the existing value.
; ---------------------------------------------------------------------
VidWritePel	PROC
		; Convert row/column to refresh buffer pointer and mask/shift
		call	VidGrapPelMask
		jc	VidRetn2

		mov_	dh, dl		; duplicate pixel mask
		and_	dl, al		; convert mask to masked value
		shl	dx, cl		; shift mask and value
		or_	al, al		; xor bit set?
		js	.xorOp

		; Replace existing pixel
		not	dh		; invert mask
		and	[es:bx], dh	; clear pixel
		or	[es:bx], dl	; merge new pixel value
		jmp	VidRetn2

.xorOp		; Merge pixel value with existing
		xor	[es:bx], dl
		jmp	VidRetn2
		ENDPROC	VidWritePel

; ---------------------------------------------------------------------
; VidReadPel [TechRef 7-13]
; Reads the pixel located at row/column DX/CX to AL.
; ---------------------------------------------------------------------
VidReadPel	PROC
		; Convert row/column to refresh buffer pointer and mask/shift
		call	VidGrapPelMask
		jc	VidRetn2

		mov	al, [es:bx]	; read pixel byte
		shr	al, cl		; shift pixel to low bits
		and_	al, dl		; mask off other pixels
		; fallthrough to VidRetn2
		ENDPROC	VidReadPel

; ---------------------------------------------------------------------
; VidRetn2
; Shared function tail that forwards to UnmakeIsrStack.
; ---------------------------------------------------------------------
VidRetn2	PROC
		jmp	UnmakeIsrStack
		ENDPROC	VidRetn2

; ---------------------------------------------------------------------
; VidWinScrollDn [TechRef 7-12]
; ---------------------------------------------------------------------
VidWinScrollDn	PROC
		mov	bl, 1
		jmp	VidWinScroll
		ENDPROC	VidWinScrollDn

; ---------------------------------------------------------------------
; VidWinScrollUp [TechRef 7-11]
; ---------------------------------------------------------------------
VidWinScrollUp	PROC
		xor_	bl, bl
		ENDPROC

; ---------------------------------------------------------------------
; VidWinScroll
; Copies data around in video refresh buffer to scroll a rectangular area
; of the screen up or down.
; On entry:
;   AL == number of lines to scroll, 00 == blank window
;   BH == attributes to use for blank lines
;   BL == 0 (scroll up), 1 (scroll down)
;   CX == row,column of upper left corner of window
;   DX == row,column of lower right corner of scroll
; ---------------------------------------------------------------------
VidWinScroll	PROC
		; Move total number of lines on screen into AH
		mov	ah, 25
		cmp	[VidActiveMode], VID_MODE_EXT_48
		jnz	.gotMaxLines
		mov	ah, 50		; mode 48 is double-height

.gotMaxLines	cmp_	dh, ah		; window extends past last line?
		jb	.rowInRange
		mov_	dh, ah		; clamp if so
		dec	dh

.rowInRange	cmp	dl, [VidTextColumns]	; column too high?
		jb	.colInRange
		mov	dl, [VidTextColumns]	; clamp if so
		dec	dl

.colInRange	sub_	dl, cl		; dl <- window width
		push	ax		; convert stack to IsrStackAx layout

		mov_	ah, dh
		sub_	ah, ch		; ah <- window height

		inc	dl
		xchg	ch, cl
		xchg	dl, cl
		sub_	ah, al		; subtract lines to scroll
					; from window width
		jnb	.L1		; scrolling more than a
					; full window height?
		xor_	al, al		; scroll no lines if so
.L1		or_	al, al		; scrolling no lines?
		jnz	.L2
		mov	ah, -1
.L2		inc	ah
		call	VidScrollImpl
		jmp	VidRetn3
		ENDPROC	VidWinScroll

; ---------------------------------------------------------------------
; VidWriteCell [TechRef 7-12]
; Writes attribute+character at cursor location, CX times.
; ---------------------------------------------------------------------
VidWriteCell	PROC
		xor_	dl, dl
		jmp	VidWriteCell2
		ENDPROC	VidWriteCell

; ---------------------------------------------------------------------
; VidWriteChar [TechRef 7-12]
; Writes character at cursor location, CX times.
; ---------------------------------------------------------------------
VidWriteChar	PROC
		mov	dl, 1
		; fall-through to VidWriteCell2
		ENDPROC	VidWriteChar

; ---------------------------------------------------------------------
; VidWriteCell2
; Shared function tail for VidWriteCell and VidWriteChar.
; ---------------------------------------------------------------------
VidWriteCell2	PROC
		jcxz	VidRetn2	; early-out for no-op calls
		push	ax		; convert stack frame to IsrStackAx
		call	VidWriteCell3
		; fall-through to VidRetn3
		ENDPROC

; ---------------------------------------------------------------------
; VidRetn3
; Shared function tail.
; ---------------------------------------------------------------------
VidRetn3	PROC
		jmp	UnmakeIsrStack2
		ENDPROC	VidRetn3

; ---------------------------------------------------------------------
; VidTeletype [TechRef 7-13]
; Writes the character in AL to the screen, interpreting basic control
; characters to update the cursor and sound the speaker (BEL/CR/LF/BS).
; Attribute for character cells scrolled into view are taken from BL
; (for graphics mode) and from the previous content in text mode.
; ---------------------------------------------------------------------
VidTeletype	PROC
		push	ax		; convert stack layout to IsrStackAx

		; Get the current page (or 0 for graphics mode)
		mov_	cl, bl		; preserve BL around call
		call	VidGetPage
		mov_	bl, cl

		; Get cursor position into DH/DL
		push	ax
		mov	ah, 03h
		int	10h
		pop	ax

		; Perform character write and get new cursor position in DX
		mov	cl, 0Ah		; write with int10/0Ah (VidWriteChar)
		call	VidTeletypeChar

		; Update stored cursor location from DX
		mov	ah, 02h
		int	10h

		jmp	UnmakeIsrStack2
		ENDPROC	VidTeletype

; ---------------------------------------------------------------------
; VidTeletypeChar
; Backend for VidTeletype and VidWriteString.
; On entry:
;   AL == character to write
;   DH == cursor row
;   DL == cursor column
;   CL == int 10h function to use when writing non-control characters
; On return:
;   DX updated with new cursor position
; ---------------------------------------------------------------------
VidTeletypeChar	PROC
		; Apply control characters

		cmp	al, 07h		; ASCII BEL
		jnz	.notBEL
		jmp	Beep

.notBEL		cmp	al, 0Dh		; ASCII CR
		jz	.applyCR

		cmp	al, 08h		; ASCII BS
		jnz	.notBS
		sub	dl, 1		; move back a space
		jnb	.charWritten	; clamp to start of line
		jmp	.applyCR

.notBS		cmp	al, 0Ah		; ASCII LF
		jnz	.notLF
		inc	dh		; move to next line
		jmp	.charWritten

.notLF		; Write character to screen
		mov_	ah, cl
		mov	cx, 1
		int	10h

		; Advance cursor and move to next line if past last column
		inc	dx
		cmp	dl, [VidTextColumns]
		jnz	.charWritten
		inc	dh

.applyCR	xor_	dl, dl		; move back to start of line

.charWritten	; Check whether we've run off the end of the page and
		; scroll up by one line if so.
		mov	ah, 25
		cmp	[VidActiveMode], VID_MODE_EXT_48
		jnz	.checkScroll
		mov	ah, 50
.checkScroll	cmp_	dh, ah		; passed end of page?
		jnz	.leaveFunction
		dec	dh		; pull back to last valid row

		; Determine attribute for newly-visible line and then scroll
		; it into view.  Graphics modes need the attribute (foreground
		; color) to be specified by the caller, while text mode can
		; read fore/back-ground from the refresh buffer.
		push	dx
		push	bx
		call	VidIsTextMode
		mov	ah, 0		; graphics mode attribute
		jc	.gotAttribute
		mov	ah, 08h		; read attribute/char
		int	10h

.gotAttribute	; Perform the scroll
		mov_	bh, ah
		mov	ax, 0601h	; scroll one line
		mov	cx, 0		; scroll entire screen
		mov	dl, [VidTextColumns]
		dec	dx
		int	10h
		pop	bx
		pop	dx

.leaveFunction	retn
		ENDPROC	VidTeletypeChar

; ---------------------------------------------------------------------
; VidSetCursorPos [TechRef 7-11]
; ---------------------------------------------------------------------
VidSetCursorPos	PROC
		call	VidStoreCurs
		jmp	UnmakeIsrStack
		ENDPROC	VidSetCursorPos

; ---------------------------------------------------------------------
; VidGetDispState [TechRef 7-13]
; ---------------------------------------------------------------------
VidGetDispState	PROC
		; AX can be returned directly
		mov	ah, [VidTextColumns]
		mov	al, [VidActiveMode]

		; BH must be returned via the saved stack image
		mov	bh, [VidActivePage]
		mov_	bp, sp
		mov	[bp+IsrStack.bh], bh

		jmp	UnmakeIsrStack
		ENDPROC	VidGetDispState

; ---------------------------------------------------------------------
; VidGrapCursorOffset
; Takes cursor position in DX and returns the offset in the refresh
; buffer where the top-left pixel of the character is located.
; ---------------------------------------------------------------------
kCgaGrapStride	db	80 * 2

VidGrapCursorOffset	PROC
		push	ax		; save trashed registers
		push	dx

		; Convert cursor row in DH to byte offset to top-left pixel
		; of row in refresh buffer.  Since CGA uses a split memory
		; layout, half the pixels are stored in the upper memory bank
		; and can be ignored.  We just need to double the row number
		; to account for the lower memory bank's rows (except for
		; video mode 48h which has four memory banks and doesn't
		; double-up rows in bank 0).
		mov_	al, dh		; get cursor row
		cmp	[VidActiveMode], VID_MODE_EXT_48
		jz	.L1
		add_	al, al		; double row number
.L1		mul	[cs:kCgaGrapStride]
		mov_	di, ax		; accumulate offset in DI

		; Calculate the column offset and add it to DI.
		; Video modes < 6 have 2bpp and need twice as many bytes
		; as video modes >= 6 do.
		xor_	al, al
		cmp	[VidActiveMode], VID_MODE_CGA_6
		adc	al, 1
		mul	dl
		add_	di, ax		; accumulate

		pop	dx		; restore trashed registers
		pop	ax
		retn
		ENDPROC	VidGrapCursorOffset

; ---------------------------------------------------------------------
; VidGetPage
; Returns VidActivePage in BH, or 0 if graphics/MDA mode.  Destroys BL.
; ---------------------------------------------------------------------
VidGetPage	PROC
		mov	bl, [VidActivePage]
		cmp	[VidActiveMode], VID_FIRST_CGA_GRAP_MODE
		jl	.return
		xor_	bl, bl
.return		mov_	bh, bl
		retn
		ENDPROC	VidGetPage

; ---------------------------------------------------------------------
; VidStoreCurs
; Updates BDA and CRTC with new cursor position from DX. For text modes
; the current video page is taken from BH.
; ---------------------------------------------------------------------
VidStoreCurs	PROC
		cmp	[VidActiveMode], VID_FIRST_CGA_GRAP_MODE
		jb	.gotPage
		xor_	bh, bh		; graphics mode has only one page

.gotPage	; Store new cursor position to BDA
		mov_	bl, bh
		xor_	bh, bh
		shl	bx, 1
		mov	[bx+VidCursorPos], dx
		sar	bx, 1

		; If we're updating the cursor for the current video page
		; then let the CRTC know where it should be rendering it.
		cmp	[VidActivePage], bl
		jnz	VidRetn4
		; fall-through to VidUpdCrtcCurs
		ENDPROC	VidStoreCurs

; ---------------------------------------------------------------------
; VidUpdCrtcCurs
; Writes DH and DL to the CRTC cursor position registers.
; ---------------------------------------------------------------------
VidUpdCrtcCurs	PROC
		push	ax		; store trashed register

		; Convert row/column to refresh buffer character offset
		mov_	al, dh
		mul	[VidTextColumns],DATA=BYTE
		xor_	dh, dh
		add_	ax, dx
		mov	cx, [VidPageOffset]
		shr	cx, 1
		add_	cx, ax

		; Fall-through to VidWriteCrtc2
		mov	bx, CRTC_RPAIR_CURSORADDR
		pop	ax
		ENDPROC	VidUpdCrtcCurs

; ---------------------------------------------------------------------
; VidWriteCrtc2
; Writes the bytes in CH/CL to CRTC the register numbers in BH/BL.
; ---------------------------------------------------------------------
VidWriteCrtc2	PROC
		push	ax
		mov	dx, [VidBasePort]

		mov_	al, bh		; save second address
		out	dx, al		; set address
		inc	dx		; move to data port
		mov_	al, ch		; get first data
		Delay	2
		out	dx, al		; write data

		dec	dx		; back to address port
		xchg	ax, bx		; retrieve second address
		Delay	2
		out	dx, al		; set second address
		xchg	ax, bx
		inc	dx		; move to data port
		xchg	ax, cx
		Delay	2
		out	dx, al		; write second data
		xchg	ax, cx

		pop	ax
		; fall-through to VidRetn4
		ENDPROC	VidWriteCrtc2

; ---------------------------------------------------------------------
; VidRetn4
; Shared function tail for normal returns.
; ---------------------------------------------------------------------
VidRetn4	PROC
		retn
		ENDPROC	VidRetn4

; ---------------------------------------------------------------------
; VidOffsets
; Calculates offsets into refresh buffer for active video page.
; On entry:
;   DH == cursor row
;   DL == cursor column
; On return:
;   As VidCalcOffsets
; ---------------------------------------------------------------------
VidOffsets	PROC
		mov	bl, [VidActivePage]
		jmp	VidCalcOffsets
		ENDPROC	VidOffsets

; ---------------------------------------------------------------------
; VidPageOffsets
; Calculates offsets into refresh buffer for specified video page using
; its current cursor position.
; On entry:
;   BH == page index
; On return:
;   As VidCalcOffsets
; ---------------------------------------------------------------------
VidPageOffsets	PROC
		mov_	bl, bh
		call	VidPageCursorPos
		; fall-through to VidCalcOffsets
		ENDPROC	VidPageOffsets

; ---------------------------------------------------------------------
; VidCalcOffsets
; Calculates offsets into refresh buffer for the specfied position on a
; specific video page.
; On entry:
;   BL == page index
;   DH == row
;   DL == column
; On return:
;   BX == offset into video page
;   DI == offset into video refresh buffer
; ---------------------------------------------------------------------
VidCalcOffsets	PROC
		push	ax		; save modified register

		xor_	di, di		; accumulate offset in ES:DI
		mov	es, [cs:kMdaRefreshSeg]

		; Adjust segment if we're not using CGA
		mov	al, [EquipmentWord]
		or	al, 0CFh
		inc	al
		jz	.gotPageBase	; MDA?  Nothing to do if so.
		mov	es, [cs:kCgaRefreshSeg]

		; Adjust page base if we're not on page zero
		cmp	[VidActiveMode], VID_FIRST_CGA_GRAP_MODE
		jnb	.gotPageBase	; Graphics mode has only one page
		mov	ax, [VidPageSize]
		xor_	bh, bh
		push	dx
		mul	bx		; multiply page index by page size
		pop	dx
		mov_	di, ax		; DI == page start offset

.gotPageBase	; Add columns to offset
		mov_	al, dh
		mul	[VidTextColumns],DATA=BYTE
		add_	al, dl
		adc	ah, 0
		shl	ax, 1		; mul by 2 for attributes
		mov_	bx, ax		; BX == offset into page
		add_	di, ax		; DI == offset into refresh buffer
		pop	ax
		retn
		ENDPROC	VidCalcOffsets

; ---------------------------------------------------------------------
; VidGrapScroll
; Scrolls the screen in graphics mode.  Complicated by the CGA memory
; layout.  TODO: figure out how this works and comment it.
; ---------------------------------------------------------------------
VidGrapScroll	PROC
		cmp	[VidActiveMode], VID_LAST_CGA_MODE
		mov_	dl, ch
		jnb	.L1
		shl	cx, 1		; extended video modes are 2x vres

.L1		call	VidGrapCursorOffset
		mov	es, [cs:kCgaRefreshSeg]
		xor_	ch, ch
		mov	dx, 2000h
		sub_	dx, cx
		mov	bp, 1FB0h
		add_	bp, cx
		push	cx
		cmp	[VidActiveMode], VID_MODE_EXT_48
		jnz	.L2
		shl	ah, 1
		mov	cx, 0A0h
		or_	bl, bl
		jz	.L3
		add	di, 50h
		add	bp, 0A0h
		neg	cx
		jmp	.L3

.L2		shl	ah, 1
		shl	ah, 1
		mov	cx, 140h
		or_	bl, bl
		jz	.L3
		add	di, 0F0h
		add	bp, 0A0h
		neg	cx

.L3		or_	al, al
		jnz	.L4
		mov	al, [VidActiveMode]
		cmp	al, VID_MODE_EXT_74
		jnz	.L5
		mov	al, 40h

.L5		jmp	.L6
		FillerNop

.L4		mov_	si, di
.L7		add_	si, cx
		dec	al
		jnz	.L7
		mov	al, [VidActiveMode]
		cmp	al, VID_MODE_EXT_74
		jnz	.L8
		mov	al, 40h

.L8		pop	cx
		mov	ds, [cs:kCgaRefreshSeg]

.L9		push	cx
		shr	cx, 1
		jnb	.L10
		movsb
.L10		rep movsw
		sti
		pop	cx
		add_	si, dx
		add_	di, dx
		push	cx
		shr	cx, 1
		jnb	.L11
		movsb
.L11		rep	movsw
		sti
		pop	cx
		cmp	al, 6
		jbe	.L12
		add_	si, dx
		add_	di, dx
		push	cx
		shr	cx, 1
		jnb	.L13
		movsb
.L13		rep movsw
		sti
		pop	cx
		add_	si, dx
		add_	di, dx
		push	cx
		shr	cx, 1
		jnb	.L14
		movsb
.L14		rep	movsw
		sti
		pop	cx
		sub	si, 4000h
		sub	di, 4000h
.L12		sub_	si, bp
		sub_	di, bp
		dec	ah
		jnz	.L9
		push	cx
.L6		pop	cx
		mov_	ah, bh
		pop	bx
		inc	bh
		sub_	bh, bl
		shl	bh, 1
		mov_	bl, al
		mov_	al, ah
		cmp	bl, 40h
		ja	.L15
		shl	bh, 1
.L15		mov_	si, cx
.L16		shr	cx, 1
		jnb	.L17
		stosb
.L17		rep stosw
		sti
		mov_	cx, si
		add_	di, dx
		shr	cx, 1
		jnb	.L18
		stosb
.L18		rep	stosw
		sti
		cmp	bl, 6
		jbe	.L19
		mov_	cx, si
		add_	di, dx
		shr	cx, 1
		jnb	.L20
		stosb
.L20		rep stosw
		sti
		mov_	cx, si
		add_	di, dx
		shr	cx, 1
		jnb	.L21
		stosb
.L21		rep	stosw
		sti
		sub	di, 4000h
.L19		mov_	cx, si
		sub_	di, bp
		dec	bh
		jnz	.L16
		retn
		ENDPROC	VidGrapScroll

; ---------------------------------------------------------------------
; VidGrapScroll2
; Seemingly unnecessary thunk to VidGrapScroll.
; ---------------------------------------------------------------------
VidGrapScroll2	PROC
		jmp	VidGrapScroll
		ENDPROC	VidGrapScroll2

; ---------------------------------------------------------------------
; VidScrollImpl
; TODO: figure out how this works and document it.
; ---------------------------------------------------------------------
VidScrollImpl	PROC
		mov_	si, dx
		add_	dl, ah
		push	dx
		mov_	dx, si
		or_	bl, bl		; scrolling down?
		jnz	.L1
		mov_	dh, dl
.L1		call	VidIsTextMode
		jc	VidGrapScroll2	; handle graphics mode separately

		mov_	dl, ch
		push	bx
		call	VidOffsets	; DI == offset into refresh buffer
		pop	bx
		xor_	ch, ch
		push	cx
		shl	cx, 1
		mov_	dx, cx
		mov	bp, [VidTextColumns]
		shl	bp, 1
		mov_	cx, bp
		sub_	bp, dx
		or_	bl, bl
		jz	.L2
		sub_	bp, cx
		sub_	bp, cx
		neg	cx

.L2		mov	bl, [VidActiveMode]
		and	bl, 0FEh
		cmp	bl, 2
		jnz	.L3
		push	dx		; disable CGA display for copy?
		push	ax
		mov	dx, [VidBasePort]
		add	dx, 4
		mov	al, [VidModeCtrl]
		and	al, 0F7h
		out	dx, al
		pop	ax
		pop	dx

.L3		or_	al, al
		jz	.L4
		mov_	si, di

.L5		add_	si, cx
		dec	al
		jnz	.L5
		pop	cx
		push	ds
		push	es
		pop	ds
.L6		push	cx
		rep movsw
		pop	cx
		add_	si, bp
		add_	di, bp
		dec	ah
		jnz	.L6
		pop	ds
		push	cx
.L4		pop	cx
		mov_	ah, bh
		mov	al, ' '
		pop	bx
		inc	bh
		sub_	bh, bl
		mov_	si, cx
.L7		rep stosw
		sti
		mov_	cx, si
		add_	di, bp
		dec	bh
		jnz	.L7

		mov	bl, [VidActiveMode]
		and	bl, 0FEh
		cmp	bl, 2
		jnz	.L8
		mov	dx, [VidBasePort]
		add	dx, 4
		mov	al, [VidModeCtrl]
		out	dx, al		; re-enable CGA display?

.L8		retn
		ENDPROC	VidScrollImpl

; ---------------------------------------------------------------------
; VidWriteCell3
; Writes a character to the screen, optionally writing the attribute
; byte as well (for text modes).
; On entry:
;   AL == character to display
;   BH == page number (text mode)
;   BL == attribute (text mode) or colour (graphics mode)
;   CX == number of times to write character
;   DL == 0 to write char+attributes, 1 to write char only
; On return:
;   Interrupts enabled
; ---------------------------------------------------------------------
VidWriteCell3	PROC
		mov_	ah, bl			; start assembling AX into char+attribute
		call	VidIsTextMode
		jc	.graphicsMode

		; --------------------- Text mode write
		push	dx
		call	VidPageOffsets		; calulate refresh buffer write offset
		pop	dx

		mov_	si, ax			; unnecessary save of AX?
		mov	bl, [VidActiveMode]
		and	bl, 0FEh		; why mask the video mode here? nothing uses it?
		or_	dl, dl			; test attribute byte write
		mov	dx, [VidBasePort]	; unnecessary load? nothing uses DX after this
		jnz	.textCharOnly

		; --------------------- Text mode char+attribute write
		mov_	ax, si			; restore AX from unnecessary save
		repne	stosw			; write cells
		sti
		retn

		; --------------------- Text mode char only write
.textCharOnly	mov_	ax, si			; restore AX from unnecessary save
.copyTextChar	stosb				; copy char
		inc	di			; skip attribute byte
		loop	.copyTextChar
		retn

		; --------------------- Graphics mode write
		; TODO: figure out how all this works and document it
.graphicsMode	xor_	bl, bl			; only one page in graphics mode
		call	VidPageCursorPos
		; convert cursor position to refresh buffer offset
		mov_	bp, ax			; save char and colour
		mov_	al, dh			; get cursor column positon
		mov	ah, 40			; multiply row position by row length
		mul	ah
		xor_	bh, bh
		mov_	bl, dl
		cmp	[VidActiveMode], VID_80COL_CGA_GRAP_MODE
		jb	.1
		shl	ax, 1
.1		add_	bx, ax
		mov_	ax, bp			; restore char and colour

		; Convert character code into ES:SI pointer to font data
		and	ah, 0BFh
		or_	al, al
		jns	.lowerHalfChar
		; Characters > 80h must use the soft-font data located by IVT pointer
		sub	al, 80h
		xor_	si, si
		mov	es, si
		les	si, [es:IvtVidGrapFont]
		mov	bx, es
		or_	bx, si			; did we have soft-font data loaded?
		jnz	.gotFontPtr
		or	ah, 40h			; adjust colour and display the equivalent
						; character from the lower-half of the font?

.lowerHalfChar	; Characters <= 80h can use the font data from ROM
		mov	si, GraphicsChars
		push	cs
		pop	es

.gotFontPtr	; Index into font data by character code
		mov_	bl, al
		xor_	bh, bh
		shl	bx, 1			; multiply by 8 (character height)
		shl	bx, 1
		shl	bx, 1
		add_	si, bx

		; ----------------------------- Graphics mode copy loop
		; TODO: figure out how all this works and document it
.c1		push	cx
		push	dx
		push	si
		call	VidGrapCursorOffset
		mov	dl, [VidActiveMode]
		cmp	dl, VID_MODE_EXT_74
		jnz	.c2
		mov	dl, VID_MODE_EXT_40
.c2		mov	bp, 8
.c3		push	ax
		mov	al, [es:si]
		test	ah, 40h
		jz	.c4
		not	al
.c4		mov_	dh, ah
		cmp	dl, VID_80COL_CGA_GRAP_MODE
		jnb	.c5
		mov	cx, 8
		and	ah, 3
		xor_	bx, bx
.c6		shl	bx, 1			; BUG?
		shl	bx, 1			; Should this be 3 shifts
		shl	al, 1			; of BX?
		jnb	.c7
		or_	bl, ah
.c7		loop	.c6
		mov_	al, bh
		mov_	ah, bl

.c5		push	es
		mov	es, [cs:kCgaRefreshSeg]
		or_	dh, dh
		js	.c8
		cmp	dl, 6
		jnb	.c9
		mov	[es:di], ax
		jmp	.c10
.c11		jmp	.c1

.c9		mov	[es:di], al
		cmp	dl, 40h
		jnz	.c10
		mov	[es:di+CGA_REGEN_LEN], al
		jmp	.c10

.c8		cmp	dl, 6
		jnb	.c12
		xor	[es:di], ax
		jmp	.c10

.c12		xor	[es:di], al
		cmp	dl, 40h
		jnz	.c10
		xor	[es:di+CGA_REGEN_LEN], al

.c10		pop	es
		add	di, CGA_REGEN_LEN
		cmp	dl, 48h
		jnz	.c13
		dec	bp
		test	bp, 3
		jnz	.c14
		sub	di, 7FB0h		; reset to bank 0

.c14		inc	bp
		jmp	.c15

.c13		cmp	dl, 6
		jbe	.c16
		add	di, CGA_REGEN_LEN

.c16		test	bp, 1
		jz	.c15
		sub	di, 3FB0h		; reset to bank 0
		cmp	dl, 6
		jbe	.c15
		sub	di, 4000h		; back one bank?

.c15		pop	ax
		inc	si			; next row of font data
		dec	bp
		jz	.c17
		jmp	.c3

.c17		pop	si
		pop	dx
		pop	cx
		inc	dx
		cmp	[VidActiveMode], 6
		jnb	.c18
		cmp	dl, 40
		jmp	.c19
.c18		cmp	dl, 80
.c19		jb	.c20
		xor_	dl, dl
		inc	dh
.c20		loop	.c21
		retn
.c21		jmp	.c11
		ENDPROC	VidWriteCell3

; ---------------------------------------------------------------------
; VidPageCursorPos
; Returns the cursor position for a text page.
; On entry:
;   BL == page index
; On return:
;   DX == cursor position
; ---------------------------------------------------------------------
VidPageCursorPos	PROC
		push	bx
		xor_	bh, bh
		shl	bx, 1			; load cursor position from array
		mov	dx, [VidCursorPos+bx]
		pop	bx
		retn
		ENDPROC	VidPageCursorPos

; ---------------------------------------------------------------------
; VidIsTextMode
; Sets CF if the current video mode is not a text mode.
; ---------------------------------------------------------------------
VidIsTextMode	PROC
		; MDA is always text mode
		cmp	[VidActiveMode], VID_MDA_MODE
		jz	.leaveFunction
		; Below the first graphics mode?  Must be text mode.
		cmp	[VidActiveMode], VID_FIRST_CGA_GRAP_MODE
		cmc
.leaveFunction	retn
		ENDPROC	VidIsTextMode

; ---------------------------------------------------------------------
; VidGrapPelMask
; Prepares registers for writing to the screen in graphics mode.
; On entry:
;   AL == pixel colour
;   CX == column
;   DX == row
; On return:
;   ES:BX -> pixel offset in refresh buffer
;   CL == shift for pixel data
;   DL == mask for bits to be written (unshifted)
;   CF set on failure
; ---------------------------------------------------------------------
VidGrapPelMask	PROC
		; Determine numebr of rows for current video mode
		mov	ah, [VidActiveMode]
		mov	bx, 200
		cmp	ah, VID_LAST_CGA_MODE
		jbe	.gotRowCount			; extended graphics mode have
		mov	bx, 400				; double vertical resolution

.gotRowCount	cmp_	dx, bx				; out of range row?
		jb	.validRow
		jmp	.leaveFailure

.validRow	mov	bx, 640				; load max column count
		cmp	ah, VID_MODE_EXT_40
		jnb	.extendedMode			; all extended modes have 640 cols
		cmp	ah, VID_LAST_CGA_MODE
		jz	.checkCol			; only one CGA mode supports 640 cols
		ja	.leaveFailure			; MDA doesn't support graphics at all
		cmp	ah, VID_FIRST_CGA_GRAP_MODE
		jb	.leaveFunction			; BUG? Text modes don't set CF on return, although
							; we shouldn't be called in text mode anyway...
		shr	bx, 1				; all other modes have 320 cols

.checkCol	cmp_	cx, bx				; out of range column?
		jnb	.leaveFailure
		xchg	ax, si				; save AX

		mov	es, [cs:kCgaRefreshSeg]
		xor_	bx, bx
		ror	dx, 1				; odd row?
		jnb	.cgaBankSet
		mov	bh, 20h				; odd rows are offset by 2000h bytes

.cgaBankSet	shl	dx, 1				; restore row with low bit cleared
		mov_	ax, dx				; accumulate refresh buffer offset in AX
		shl	ax, 1				; mul by 40 with shl/add combo
		shl	ax, 1
		add_	ax, dx
		shl	ax, 1
		shl	ax, 1
		shl	ax, 1

.c1		add_	bx, ax				; add to CGA bank base
		mov_	ax, cx				; get column
		shr	ax, 1				; div 4?
		shr	ax, 1
		not	cl
		cmp	[VidActiveMode], VID_80COL_CGA_GRAP_MODE
		jb	.not80ColMode
		shr	ax, 1
		mov	dl, 1
		jmp	.c2
.not80ColMode	shl	cl, 1
		mov	dl, 3

.c2		add_	bx, ax
		and	cl, 7
		xchg	ax, si
		retn

.extendedMode	cmp_	cx, bx				; out of range column?
		jnb	.leaveFailure
		xchg	ax, si				; save AX
		mov	es, [cs:kCgaRefreshSeg]
		xor_	bx, bx
		ror	dx, 1
		jnb	.cgaBankSet2
		add	bh, 20h				; odd rows are offset by 2000h
.cgaBankSet2	ror	dx, 1
		jnb	.cgaBankSet3
		add	bh, 40h				; two extra banks in hi-res modes

.cgaBankSet3	shl	dx, 1				; restore row with low bits cleared
		shl	dx, 1
		mov_	ax, dx
		shl	ax, 1				; mu by 20 using shift/add
		shl	ax, 1
		add_	ax, dx
		shl	ax, 1
		shl	ax, 1
		jmp	.c1

.leaveFailure	stc
.leaveFunction	retn
		ENDPROC	VidGrapPelMask

; ---------------------------------------------------------------------
; VidWriteString [TechRef 7-14]
; TechRef incorrectly documents CS:BP as a pointer to the string to be
; written; it should be ES:BP.
; ---------------------------------------------------------------------
VidWriteString	PROC
		push	ax
		test	al, 0FCh			; reserved bits set?
		jnz	.leaveFunction
		jcxz	.leaveFunction			; early-out if no data to write

		mov_	si, ax				; copy AL (string type)
		xchg	bl, bh				; move page index to lobyte, we're going to use it as an offset
		mov_	di, bx				; copy to DX, we'll use it to double the offset
		lea	di, [bx+di+VidCursorPos]	; load cursor location for page
		and	di, 0FFh
		xchg	bl, bh				; restore page to BH
		push	[di]				; save cursor location

.processChar	push	cx
		mov	[di], dx			; update saved cursor location
		mov	al, [es:bp+0]			; load character
		inc	bp

		test	si, 2				; caller wants to write attributes?
		jz	.writeChar
		; [TechRef 7-14] documents ASCII BEL, BKSP, CR, and LF are treated as commands and
		; not as printable characters.  Apparently this means they shouldn't have an
		; attribute byte after them in the input string.
		cmp	al, 07h
		jz	.writeChar
		cmp	al, 08h
		jz	.writeChar
		cmp	al, 0Ah
		jz	.writeChar
		cmp	al, 0Dh
		jz	.writeChar
		mov	bl, [es:bp+0]			; load new attribute value
		inc	bp

.writeChar	mov	cl, 9				; perform write using VidWriteCel
		call	VidTeletypeChar
		pop	cx
		loop	.processChar

		pop	[di]				; load saved cursor location
		test	si, 1				; caller wants cursor to move?
		jz	.leaveFunction
		mov	ah, 2				; set cursor position
		int	10h

.leaveFunction	jmp	UnmakeIsrStack2
		ENDPROC	VidWriteString

; ---------------------------------------------------------------------
; ConChar
; Writes a single character to the screen, as VidTeletype.
; On entry:
;   AL == character to write
; ---------------------------------------------------------------------
ConChar		PROC
		push	bx				; save display window
		mov	bl, 82h				; fixed colours for graphics mode
		mov	ah, 0Eh				; write using VidTeletype
		int	10h
		pop	bx
		retn
		ENDPROC	ConChar

; ---------------------------------------------------------------------
; ConCrLf
; Writes a carriage-return/line-feed pair to the screen.
; ---------------------------------------------------------------------
ConCrLf	PROC
		mov	si, kCrLf
		; fall-through into ConString
		ENDPROC	ConCrLf

; ---------------------------------------------------------------------
; ConString
; Writes a nul-terminated string to the screen.
; On entry:
;   CS:SI -> string
; On return:
;   CS:SI -> nul-terminator
; ---------------------------------------------------------------------
ConString	PROC
		push	cx
		call	ConString2
		pop	cx
		retn
		ENDPROC	ConString

ENDPROGRAM	INT10
