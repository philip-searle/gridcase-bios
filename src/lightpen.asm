
LIGHTPEN	PROGRAM	OutFile=build/lightpen.obj

		include	"macros.inc"
		include	"segments/bda.inc"
		include	"isr.inc"
		include	"video.inc"

		EXTERN	VidIsTextMode
		EXTERN	UnmakeIsrStack2

; Horizontal adjustment for lightpen x-coordinates per video mode
; BUG: no support for extended modes > 7
LightpenAdjX	db	3	; CGA Text 40x25
		db	3	; CGA Text 40x25
		db	5	; CGA Text 80x25
		db	5	; CGA Text 80x25
		db	3	; CGA Graphics 40x25
		db	3	; CGA Graphics 40x25
		db	3	; CGA Graphics 80x25
		db	4	; MDA Text 80x25

VidReadLightpen	PROC
		; Use BP as frame base so IsrStack offsets work
		mov_	bp, sp
		xor_	ah, ah
		push	ax

		; Read video status port and decide if lightpen has been triggered
		mov	dx, [VidBasePort]
		add	dx, VID_STATUS_PORT_OFFSET
		in	al, dx
		inc	dx			; inc to lightpen port
		test	al, 4			; lightpen switch pressed?
		jnz	.clearAndLeave
		test	al, 2			; lightpen triggered?
		jz	.leaveFunction

		; Read lightpen position
		sub	dx, VID_LPEN_PORT_OFFSET
		mov	al, CRTC_REG_LPEN_HI
		out	dx, al
		mov	bl, [VidActiveMode]
		xor_	bh, bh
		inc	dx			; inc to crtc data port
		Delay	1
		in	al, dx			; read lightpen hi
		mov_	ah, al
		dec	dx			; dec to crtc address port
		mov	al, CRTC_REG_LPEN_LO
		out	dx, al
		mov	bl, [cs:bx+LightpenAdjX]
		inc	dx			; inc to crtc data port
		Delay	1
		in	al, dx			; read lightpen lo
		add	dx, 6			; inc to lightpen port
		out	dx, al			; clear lightpen latch
		sub_	ax, bx			; adjust x position
		jnb	.1			; too far left?
		xor_	ax, ax			; reset position if so

.1		; Convert lightpen counter to row/column position
		mov	cl, 3			; CL is our divisor
		call	VidIsTextMode

		; Convert graphics mode character position
		jnc	.textMode
		mov	bh, 40			; assume 40 cols in graphics mode
		div	bh
		xchg	ah, al
		mov_	bx, ax			; column position in BX
		shl	ah, 1
		cmp	[VidActiveMode], VID_80COL_CGA_GRAP_MODE
		jnz	.2
		inc	cx			; 80-col mode has double the rows
		shl	bl, 1
.2		shr	bh, 1
		shr	bh, 1
		mov	[bp+IsrStack.dx], bx	; DH,DL = character row/column
		jmp	.storeResults

.textMode	; Convert text mode character position
		div	[VidTextColumns],DATA=BYTE
		xchg	ah, al
		mov	[bp+IsrStack.dx], ax	; DH,DL = character row/column
		shl	ah, cl

.storeResults	; Store pixel row/column position
		mov	[bp+IsrStack.cx+1], ah
		xor_	ah, ah
		shl	ax, cl
		mov	[bp+IsrStack.bx], ax
		mov	[bp-1], 1,DATA=BYTE	; modify AX on stack to set lightpen trigger flag
		add	dx, 6			; inc to lightpen reset port

.clearAndLeave	out	dx, al			; clear lightpen latch
.leaveFunction	jmp	UnmakeIsrStack2
		ENDPROC	VidReadLightpen

ENDPROGRAM	LIGHTPEN
