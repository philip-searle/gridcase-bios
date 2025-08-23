; =====================================================================
; GRiD-specific video routines that deal with the V6366 colour maps
; =====================================================================

GRID_VID2	PROGRAM	OutFile=grid_vid2.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"segments/bda.inc"
		include	"bios-version.inc"
		include	"grid.inc"
		include	"v6366.inc"

		PUBLIC	VidDecColMap, VidIncColMap
		PUBLIC	VidLoadColMapExpRegs
		PUBLIC	VidInitColMap

		%IF	BIOS_VERSION = 19891025
			EXTERN	VgaRetnGuard
		%ENDIF
; ---------------------------------------------------------------------
; Video expansion register bytes for video modes 0-5 (0 is used for
; modes > 5).  First 32 bytes are colour palette register values.
; Last byte is expansion register 24h value.
;			0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F  10  11  12  13  14  15  16  17  18  19  1A  1B  1C  1D  1E  1F  20
VidExpRegTable	db	0,  0,  0,  0,  2,20h,  2,20h,  2,  0,  2,  0,  1,  0,  1,  0,  0,  0,  0,  0,  2,20h,  2,20h,  2,  0,  2,  0,  1,  0,  1,  0,  2
		db	0,  0,  2,20h,  2,20h,  2,20h,  2,  0,  2,  0,  1,  0,  1,  0,  0,  0,  2,20h,  2,20h,  2,20h,  2,  0,  2,  0,  1,  0,  1,  0,  2
		db	0,  0,  2,20h,  2,  0,  2,  0,  2,20h,  2,20h,  1,  0,  1,  0,  0,  0,  2,20h,  2,  0,  2,  0,  2,20h,  2,20h,  1,  0,  1,  0,  2
		db	0,  0,  0,  0,  2,  0,  2,  0,  2,20h,  2,20h,  1,  0,  1,  0,  0,  0,  0,  0,  2,  0,  2,  0,  2,20h,  2,20h,  1,  0,  1,  0,  2
		db	0,  0,  0,  0,  2,20h,  2,20h,  2,  0,  2,  0,  1,  0,  1,  0,  0,  0,  0,  0,  2,20h,  2,20h,  2,  0,  2,  0,  1,  0,  1,  0,82h
		db	0,  0,  0,  0,  2,20h,  2,20h,  2,  0,  2,  0,  1,  0,  1,  0,  0,  0,  0,  0,  2,20h,  2,20h,  2,  0,  2,  0,  1,  0,  1,  0,0C2h

; =====================================================================
; VidLoadExpRegs
; Loads all 16 colour palettes into the 6366 video controller via the
; expansion registers.
;
; On entry:
;   DS:SI -> palette register data (20h bytes) followed by
;            expansion register 24h (1 byte)
;            [TechRef 7-26, TechRef 7-38]
; =====================================================================
VidLoadExpRegs	PROC
		push	ax
		push	dx

		; Enable access to 6366 expansion registers and Mode
		; Control Register bit 0.
		mov	al, 81h
		mov	dx, PORT_V6366_CONTROLID
		out	dx, al

		; Copy palette data into expansion registers 00-1F
		mov	al, 0
		mov	dx, PORT_V6366_BANK_ADDR
		out	dx, al
		inc	dx		; advance to PORT_V6366_BANK_DATA
		cld
		mov	cx, 20h		; 32 palette registers
.loadPaletteReg	lodsb
		out	dx, al
		Delay	3
		loop	.loadPaletteReg

		; Load expansion register 24h (MON, ULE, GRM2-GRM0, FON2-FON0)
		mov	dx, PORT_V6366_BANK_ADDR
		mov	al, 24h
		out	dx, al
		push	ds
		mov	ax, BDA_SEGMENT
		mov	ds, ax
		mov	ah, [VidActiveMode]	; Get current video mode
		pop	ds
		lodsb				; Load exp reg 24h value
		cmp	ah, VID_MODE_EXT_40
		jb	.L1
		or	al, 20h			; Extended video modes need 4-bank video memory
.L1		mov	dx, PORT_V6366_BANK_DATA
		out	dx, al			; Set exp reg 24h

		; Disable access to expansion registers
		mov	dx, PORT_V6366_CONTROLID
		mov	al, 1
		out	dx, al

		pop	dx
		pop	ax
		retn
		ENDPROC	VidLoadExpRegs

; =====================================================================
; VidDecColMap
; Decrements VidColorMap in the BDA, wrapping around to the maximum
; colour map index (5).
; =====================================================================
VidDecColMap	PROC
		%IF	BIOS_VERSION = 19891025
			call	VgaRetnGuard
		%ENDIF
		push	ds
		push	ax
		mov	ax, BDA_SEGMENT
		mov	ds, ax

		call	VidIsInternal
		jz	VidIncColMap.sharedReturn
		dec	[VidColorMap]
		cmp	[VidColorMap], 0FFh	; Need to wrap?
		jnz	VidIncColMap.sharedLoad
		mov	[VidColorMap], 5	; Wrap colour map index
		jmp	VidIncColMap.sharedLoad
		FillerNop
		ENDPROC	VidDecColMap

; =====================================================================
; VidIncColMap
; Increments VidColorMap in the BDA, wrapping around to the minimum
; colour map index (0).  Also houses the shared expansion register
; loading and return code for VidDecColMap.
; =====================================================================
VidIncColMap	PROC
		%IF	BIOS_VERSION = 19891025
			call	VgaRetnGuard
		%ENDIF
		push	ds
		push	ax
		mov	ax, BDA_SEGMENT
		mov	ds, ax

		call	VidIsInternal
		jz	.sharedReturn
		inc	[VidColorMap]
		cmp	[VidColorMap], 6	; Need to wrap?
		jb	.sharedLoad
		mov	[VidColorMap], 0	; Wrap colour map index

.sharedLoad	call	VidLoadColMapExpRegs
.sharedReturn	pop	ax
		pop	ds
		retn
		ENDPROC	VidIncColMap

; =====================================================================
; VidIsInternal
; Clears ZF if the display is currently set to the internal panel.
; Sets ZF if the display is set to the external port.
; =====================================================================
VidIsInternal	PROC
		push	dx
		push	ax

		mov	dx, PORT_VID_ROMSTATUS
		in	al, dx
		and	al, VID_TYPE_EXTERNAL

		pop	ax
		pop	dx
		retn
		ENDPROC	VidIsInternal

; =====================================================================
; VidLoadColMapExpRegs
; Loads the appropriate palette data into V6366 expansion registers
; for the currently selected colour map (VidColorMap from BDA).
; =====================================================================
VidLoadColMapExpRegs	PROC
		push	ds
		push	ax
		push	cx
		push	si
		mov	ax, BDA_SEGMENT
		mov	ds, ax

		xor_	ah, ah
		mov	al, [VidColorMap]
		cmp	al, 5		; range check max colour map index
		jbe	.validColourMap
		mov	[VidColorMap], 0
		xor_	al, al

.validColourMap	; Calculate correct address of palette register data
		mov_	si, ax
		mov	cl, 5		; 2^5 == 32 palette bytes
		shl	ax, cl
		add_	si, ax
		add	si, VidExpRegTable
		mov	ax, cs
		mov	ds, ax
		call	VidLoadExpRegs

		pop	si
		pop	cx
		pop	ax
		pop	ds
		retn
		ENDPROC	VidLoadColMapExpRegs

; =====================================================================
; VidInitColMap
; Initialises BDA variable VidColorMap and loads the palette.
; =====================================================================
VidInitColMap	PROC
		push	ds
		mov	ax, BDA_SEGMENT
		mov	ds, ax
		mov	[VidColorMap], 0
		pop	ds
		call	VidLoadColMapExpRegs
		retn
		ENDPROC	VidInitColMap

ENDPROGRAM	GRID_VID2
