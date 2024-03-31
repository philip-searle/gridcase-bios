
GRID_VID	PROGRAM	OutFile=build/grid_vid.obj

		include	"macros.inc"
		include	"segments/bda.inc"
		include	"grid.inc"
		include	"v6366.inc"

		EXTERN	VidLoadColMapExpRegs, VidInitV6366
		EXTERN	TenthMilliDelay, WaitHdSpinUp

		PUBLIC	GridVidInitLo, GridVidInitHi
		PUBLIC	GridToggleExt
		PUBLIC	VidInit
		PUBLIC	VidInit_Internal, VidInit_External

; Default V6366 secondary register values
kVidSecRegValues	\
		d b	71h	; Horiz. total		= 113 chars
		d b	50h	; Horiz. displayed	=  80 chars
		d b	5Ah	; Horiz. sync position	=  90 char
		d b	0Ah	; Sync pulse width	=  10 chars horiz, 16 lines vert
		d b	1Fh	; Vert. total		=  31 chars
		d b	06h	; Total raster adj	=   6 lines
		d b	19h	; Vert. displayed	=  25 chars
		d b	1Ch	; Vert. sync position	=  28 char
		d b	02h	; Interlace mode	= ignored
		d b	07h	; Max raster address	=   8 line characters
		d b	06h	; Cursor start+mode
		d b	07h	; Cursor end
		d b	0, 0	; Start address
		d b	0, 0	; Cursor address

; Expansion registers 20h-40h, plus 27h for LCD displays, single drive 640x200
kVidExpRegs_LCD_Lo	\
		d b	63h	; 20h: dual-panel DUTY value = 1/100
		d b	9Fh	; 21h: HSIZE = 640 (dual-drive, two-screen = (n/4)-1)
		d b	07h	; 22h: VADJ = 7 lines
		d b	81h	; 23h: SCT=1 (high-speed memory timing), SCU=0 (16-cycle cursor blink), HADJ=1 pixel
		d b	02h	; 24h: MON=0 (colour mode), ULE=0 (no underline), GRM=640x1 2color CGA banking, FON=8pels wide
		d b	0B0h	; 25h: SYTH=1 (negative sync pulse), HERC=0 (two-bank mode), SRAM=1 (static RAM), 8BUS=1 (8-bit display data bus), 16CP=0 (8-bit CPU bus), BINH=0 (display border), CLK=14.318MHz CGA clock
		d b	80h	; 26h: RA4=AC conversion signal M, A15=0 (A15 line ignored), EXP=0 (IBM compatible mode), PB/PAG=no extra pages used
		d b	8Ah	; 27h: LS=4-bit parallel transmission, IEN=four gray shades, EXTS=no external sync, IINH=inhibit display off, ACM=ignored, CPLE=enable color palettes, PRE=disable preset data registers
		d b	6Eh	; 28h: SWC=normal sync pulse width, VST=delay vsync 1us, SSY=negative polarity sync pulses, SSCK=output clock only during display, H/E=output ECK signal for LCD, R/M=RA4 is AC conversion signal M, SC=dual-drive two-screen LCD
		d b	53h	; 29h: STBY=no standby, RREG=read control regs, ESIO=no external sync, EH=19 (???)
		d b	0	; 2Ah: TST=ignored, SS=no vertical pel scroll
		d b	0	; 2Bh: control data register (ignored)
				; 2Ch-2Fh not present
		d b	0, 0, 0, 0
				; 30h-37h: preset values for alphanumeric modes
		d b	55h, 0F8h, 0FDh, 0Ah, 19h, 0, 07h, 0
				; 38h-3Fh: preset values for graphical modes
		d b	2Ah, 0FCh, 0F4h, 0Ah, 67h, 0, 01h, 07h
				; Rewrite 27h: enable preset value usage
		d b	8Bh

; Expansion registers 20h-40h, plus 27h for plasma/EL displays, 640x200
kVidExpRegs_1Panel_Lo	\
		d b	0	; 20h: DUTY cycle not used
		d b	4Fh	; 21h: HSIZE=640 (non-LCD panel)
		d b	07h	; 22h: VADJ=7 lines
		d b	9Dh	; 23h: SCT=high speed memory timing, SCU=16 field cursor blink, HADJ=29 pixels
		d b	02h	; 24h: MON=0 (colour mode), ULE=0 (no underline), GRM=640x1 2color CGA banking, FON=8pels wide
		d b	0B0h	; 25h: SYTH=1 (negative sync pulse), HERC=0 (two-bank mode), SRAM=1 (static RAM), 8BUS=1 (8-bit display data bus), 16CP=0 (8-bit CPU bus), BINH=0 (display border), CLK=14.318MHz CGA clock
		d b	0	; 26h: RA4=input, A15=0 (A15 line ignored), EXP=0 (IBM compatible mode), PB/PAG=no extra pages used
		d b	0Ah	; 27h: LS=1-bit serial transmission, IEN=four gray shades, EXTS=no external sync, IINH=inhibit display off, ACM=ignored, CPLE=enable color palettes, PRE=disable preset data registers
		d b	11h	; 28h: SWC=normal sync pulse width, VST=normal vsync timing, SSY=positive polarity sync pulses, SSCK=constant output clock, H/E=hsync signal output, R/M=RA4 is raster address 4, SC=one-screen flat panel
		d b	40h	; 29h: STBY=no standby, RREG=read control regs, ESIO=no external sync, EH=0 (???)
		d b	0	; 2Ah: TST=ignored, SS=no vertical pel scroll
		d b	0	; 2Bh: control data register (ignored)
				; 2Ch-2Fh not present
		d b	0, 0, 0, 0
				; 30-37h: preset values for alphanumeric modes
		d b	71h, 0F6h, 0FCh, 0Ah, 18h, 0, 07h, 0
				; 38h-3Fh: preset values for graphical modes
		d b	38h, 0FBh, 0F3h, 0Ah, 63h, 06h, 01h, 07h
				; Rewrite 27h: enable preset value usage
		d b	0Bh

; Expansion registers 20h-40h, plus 27h for plasma/EL displays, 640x400
kVidExpRegs_1Panel_Hi	\
		d b	0	; 20h: DUTY cycle not used
		d b	4Fh	; 21h: HSIZE=640 (non-LCD panel)
		d b	10h	; 22h: VADJ=16 lines
		d b	81h	; 23h: SCT=high speed memory timing, SCU=16 field cursor blink, HADJ=1 pixel
		d b	0E2h	; 24h: MON=1 (mono mode), ULE=1 (underline enable), GRM=640x1 1color Hercules banking, FON=8pels wide
		d b	0F5h	; 25h: SYTH=1 (negative sync pulse), HERC=1 (four-bank mode), SRAM=1 (static RAM), 8BUS=1 (8-bit display data bus), 16CP=0 (8-bit CPU bus), BINH=1 (inhibit display border), CLK=17.745MHz 640x400 clock
		d b	0	; 26h: RA4=input, A15=0 (A15 line ignored), EXP=0 (IBM compatible mode), PB/PAG=no extra pages used
		d b	0Ah	; 27h: LS=1-bit serial transmission, IEN=four gray shades, EXTS=no external sync, IINH=inhibit display off, ACM=ignored, CPLE=enable color palettes, PRE=disable preset data registers
		d b	11h	; 28h: SWC=normal sync pulse width, VST=normal vsync timing, SSY=positive polarity sync pulses, SSCK=constant output clock, H/E=hsync signal output, R/M=RA4 is raster address 4, SC=one-screen flat panel
		d b	40h	; 29h: STBY=no standby, RREG=read control regs, ESIO=no external sync, EH=0 (???)
		d b	0	; 2Ah: TST=ignored, SS=no vertical pel scroll
		d b	0	; 2Bh: control data register (ignored)
				; 2Ch-2Fh not present
		d b	0, 0, 0, 0
				; 30-37h: preset values for alphanumeric modes
		d b	53h, 0F4h, 0FDh, 0Ah, 19h, 0, 07h, 0
				; 38h-3Fh: preset values for graphical modes
		d b	29h, 0FAh, 0F4h, 0Ah, 67h, 01h, 03h, 07h
				; Rewrite 27h: enable preset value usage
		d b	0Bh

; Expansion registers 20h-40h, plus 27h for LCD displays, dual drive 640x200
kVidExpRegs_2Panel_Lo	\
		d b	0C7h	; 20h: DUTY cycle = 1/200
		d b	9Fh	; 21h: HSIZE=640 (dual-drive two-panel screen)
		d b	0Fh	; 22h: VADJ=15 lines
		d b	81h	; 23h: SCT=high speed memory timing, SCU=16 field cursor blink, HADJ=1 pixel
		d b	02h	; 24h: MON=0 (colour mode), ULE=0 (no underline), GRM=640x1 2color CGA banking, FON=8pels wide
		d b	0B1h	; 25h: SYTH=1 (negative sync pulse), HERC=0 (two-bank mode), SRAM=1 (static RAM), 8BUS=1 (8-bit display data bus), 16CP=0 (8-bit CPU bus), BINH=0 (display border), CLK=17.745MHz 640x400 clock
		d b	80h	; 26h: RA4=output AC conversion M, A15=0 (A15 line ignored), EXP=0 (IBM compatible mode), PB/PAG=no extra pages used
		d b	8Ah	; 27h: LS=4-bit parallel transmission, IEN=four gray shades, EXTS=no external sync, IINH=inhibit display off, ACM=ignored, CPLE=enable color palettes, PRE=disable preset data registers
		d b	66h	; 28h: SWC=normal sync pulse width, VST=delay vsync 1us, SSY=negative polarity sync pulses, SSCK=output clock only during display, H/E=hsync signal output, R/M=RA4 is AC conversion signal M, SC=dual-drive dual-screen LCD
		d b	40h	; 29h: STBY=no standby, RREG=read control regs, ESIO=no external sync, EH=0 (???)
		d b	0	; 2Ah: TST=ignored, SS=no vertical pel scroll
		d b	0	; 2Bh: control data register (ignored)
				; 2Ch-2Fh not present
		d b	0, 0, 0, 0
				; 30-37h: preset values for alphanumeric modes
		d b	55h, 0F8h, 0FDh, 0Ah, 19h, 0, 07h, 60h
				; 38h-3Fh: preset values for graphical modes
		d b	2Ah, 0FCh, 0F4h, 0Ah, 67h, 0, 01h, 67h
				; Rewrite 27h: enable preset value usage
		d b	8Bh

; Expansion registers 20h-40h, plus 27h for LCD displays, dual drive 640x400
kVidExpRegs_2Panel_Hi	\
		d b	0C7h	; 20h: DUTY cycle = 1/200
		d b	9Fh	; 21h: HSIZE=640 (dual-drive two-panel screen)
		d b	0Fh	; 22h: VADJ=15 lines
		d b	81h	; 23h: SCT=high speed memory timing, SCU=16 field cursor blink, HADJ=1 pixel
		d b	0E2h	; 24h: MON=1 (mono mode), ULE=1 (underline support), GRM=640x1 1color Hercules banking, FON=8pels wide
		d b	75h	; 25h: SYTH=0 (positive sync pulse), HERC=1 (four-bank mode), SRAM=1 (static RAM), 8BUS=1 (8-bit display data bus), 16CP=0 (8-bit CPU bus), BINH=1 (inhibit display border), CLK=17.745MHz 640x400 clock
		d b	80h	; 26h: RA4=output AC conversion M, A15=0 (A15 line ignored), EXP=0 (IBM compatible mode), PB/PAG=no extra pages used
		d b	8Ah	; 27h: LS=4-bit parallel transmission, IEN=four gray shades, EXTS=no external sync, IINH=inhibit display off, ACM=ignored, CPLE=enable color palettes, PRE=disable preset data registers
		d b	66h	; 28h: SWC=normal sync pulse width, VST=delay vsync 1us, SSY=negative polarity sync pulses, SSCK=output clock only during display, H/E=hsync signal output, R/M=RA4 is AC conversion signal M, SC=dual-drive dual-screen LCD
		d b	40h	; 29h: STBY=no standby, RREG=read control regs, ESIO=no external sync, EH=0 (???)
		d b	0	; 2Ah: TST=ignored, SS=no vertical pel scroll
		d b	0	; 2Bh: control data register (ignored)
				; 2Ch-2Fh not present
		d b	0, 0, 0, 0
				; 30-37h: preset values for alphanumeric modes
		d b	55h, 0F8h, 0FDh, 0Ah, 19h, 0, 07h, 60h
				; 38h-3Fh: preset values for graphical modes
		d b	2Ah, 0FCh, 0F4h, 0Ah, 67h, 0, 03h, 07h
				; Rewrite 27h: enable preset value usage
		d b	8Bh

; Expansion registers 20h-40h, plus 27h for external CRT displays
kVidExpRegs_External	\
		d b	0	; 20h: DUTY cycle disabled
		d b	0	; 21h: HSIZE=0 (no panel)
		d b	0	; 22h: VADJ=0 lines
		d b	80h	; 23h: SCT=high speed memory timing, SCU=16 field cursor blink, HADJ=0 pixels
		d b	02h	; 24h: MON=0 (colour mode), ULE=0 (no underline), GRM=640x1 2color CGA banking, FON=8pels wide
		d b	30h	; 25h: SYTH=0 (positive sync pulse), HERC=0 (two-bank mode), SRAM=1 (static RAM), 8BUS=1 (8-bit display data bus), 16CP=0 (8-bit CPU bus), BINH=0 (display border), CLK=14.318MHz CGA clock
		d b	0h	; 26h: RA4=input, A15=0 (A15 line ignored), EXP=0 (IBM compatible mode), PB/PAG=no extra pages used
		d b	08h	; 27h: LS=1-bit serial transmission, IEN=four gray shades, EXTS=no external sync, IINH=inhibit display off, ACM=ignored, CPLE=disable color palettes, PRE=disable preset data registers
		d b	0	; 28h: SWC=normal sync pulse width, VST=normal vsync timing, SSY=positive polarity sync pulses, SSCK=output clock only during display, H/E=hsync signal output, R/M=RA4 is raster address 4, SC=no flat panel
		d b	40h	; 29h: STBY=no standby, RREG=read control regs, ESIO=no external sync, EH=0 (???)
		d b	0	; 2Ah: TST=ignored, SS=no vertical pel scroll
		d b	0	; 2Bh: control data register (ignored)
				; 2Ch-2Fh not present
		d b	0, 0, 0, 0
				; 30-37h: preset values for alphanumeric modes
		d b	0, 0, 0, 0, 0, 0, 0, 0
				; 38h-3Fh: preset values for graphical modes
		d b	0, 0, 0, 0, 0, 0, 0, 07h
				; Rewrite 27h: enable preset value usage
		d b	08h

; ===========================================================================
; VidOpenV6366
; Disables the video display and returns with the secondary video controller
; registers unlocked.
;
; On return:
;   DX == 3D0h (PORT_V6366_CADDR1)
;   AL == previous video mode control register value
; ===========================================================================
VidOpenV6366	PROC
		mov	dx, PORT_V6366_CONTROLID
		mov	al, 81h		; unlock expansion registers
		out	dx, al
		sub	dx, (PORT_V6366_CONTROLID - PORT_V6366_MODE_CTRL)
		in	al, dx		; read mode control port
		push	ax
		xor_	al, al		; mode control: 40x25 text mode, display disabled
		out	dx, al
		pop	ax
		sub	dx, (PORT_V6366_MODE_CTRL - PORT_V6366_CADDR1)
		retn
		ENDPROC	VidOpenV6366

; ===========================================================================
; VidWriteSecReg
; Writes the value in AL to the video controller secondary register whose
; index is in BL.
;
; On entry:
;   BL == secondary register index
;   AL == value to write
;   DX == 3D0h (PORT_V6366_CADDR1)
; ===========================================================================
VidWriteSecReg	PROC
		push	ax
		mov_	al, bl		; set secondary register index
		out	dx, al
		inc	dx		; advance to data port
		pop	ax
		out	dx, al		; write secondary register value
		dec	dx		; reset port number
		retn
		ENDPROC	VidWriteSecReg

; ===========================================================================
; VidInitSecRegs
; Initialises the V6366 secondary registers to default values.
;
; On entry:
;   DX == 3D0h (PORT_V6366_CADDR1)
; ===========================================================================
VidInitSecRegs	PROC
		mov	si, kVidSecRegValues
		mov	bl, 0
		mov	cx, 12
.writeSecReg	lodsb
		call	VidWriteSecReg
		inc	bl
		loop	.writeSecReg
		retn
		ENDPROC	VidInitSecRegs

; ===========================================================================
; VidSetModeReg
; Sets the mode control register to the value in AL.
;
; On entry:
;   DX == 3D0h (PORT_V6366_CADDR1)
;   AL == new mode control value
; ===========================================================================
VidSetModeReg	PROC
		add	dx, (PORT_V6366_MODE_CTRL - PORT_V6366_CADDR1)
		out	dx, al
		sub	dx, (PORT_V6366_MODE_CTRL - PORT_V6366_CADDR1)
		retn
		ENDPROC	VidSetModeReg

; ===========================================================================
; VidSetColorSelectReg
; Sets video colour select register: black border, normal intensity for
; colours 1-3 in four-bit graphics mode, normal two-bit graphics mode colours.
;
; On entry:
;   DX == 3D0h (PORT_V6366_CADDR1)
; ===========================================================================
VidSetColorSelectReg	PROC
		Unused	VidSetColorSelectReg
		mov	al, 0
		add	dx, (PORT_V6366_COLOR_SELECT - PORT_V6366_CADDR1)
		out	dx, al
		sub	dx, (PORT_V6366_COLOR_SELECT - PORT_V6366_CADDR1)
		retn
		ENDPROC	VidSetColorSelectReg

; ===========================================================================
; VidWriteExpReg
; Writes the value in AL to the video controller expansion register whose
; index is in BL.
;
; On entry:
;   DX == 3D0h (PORT_V6366_CADDR1)
;   BL == expansion register index
;   AL == value to write
; ===========================================================================
VidWriteExpReg	PROC
		push	ax
		mov_	al, bl
		add	dx, (PORT_V6366_BANK_ADDR - PORT_V6366_CADDR1)
		out	dx, al		; write expansion register index
		inc	dx		; advance to bank data port
		pop	ax
		out	dx, al		; write expansion register data
		sub	dx, (PORT_V6366_BANK_DATA - PORT_V6366_CADDR1)
		retn
		ENDPROC	VidWriteExpReg

; ===========================================================================
; VidInitExpRegs
; Initialises video controller expansion registers 20h-40h, plus 27h (again)
; from data stored at DS:SI.
;
; On entry:
;   DX == 3D0h (PORT_V6366_CADDR1)
;   SI -> 21h bytes (exp regs 20h-40, 27h)
; ===========================================================================
VidInitExpRegs	PROC
		; Enable setting full values in 6845 preset registers
		mov	al, 3Fh		; expansion reg 3Fh
		add	dx, (PORT_V6366_BANK_ADDR - PORT_V6366_CADDR1)
		out	dx, al
		inc	dx		; advance to PORT_V6366_BANK_DATA
		mov	al, 81h		; exp reg 3Fh: enable 6845 register MSBs
		out	dx, al
		sub	dx, (PORT_V6366_BANK_DATA - PORT_V6366_CADDR1)
		mov	cx, 20h		; 32 expansion registers to write
		mov	bl, 20h		; starting from register 20h (after the palette regs)

		; Write expansion registers
.writeExpReg	lodsb
		call	VidWriteExpReg
		inc	bl
		loop	.writeExpReg

		; Rewrite expansion reg 27h
		lodsb
		mov	bl, 27h
		call	VidWriteExpReg
		retn
		ENDPROC	VidInitExpRegs

; ===========================================================================
; VidCloseV6366
; Enables the display and locks the secondary display controller registers.
;
; On entry:
;   DX == 3D0h (PORT_V6366_CADDR1)
; ===========================================================================
VidCloseV6366	PROC
		Unused	VidCloseV6366	; Never called???
		mov	al, 1		; reverse VidOpenV6366 changes
		add	dx, (PORT_V6366_CONTROLID - PORT_V6366_CADDR1)
		out	dx, al
		sub	dx, (PORT_V6366_CONTROLID - PORT_V6366_CADDR1)
		retn
		ENDPROC	VidCloseV6366

; ===========================================================================
; VidInit_External
; Initializes the video controller to drive an external CRT display.
; ===========================================================================
VidInit_External	PROC
		push	ds
		mov	ax, cs
		mov	ds, ax

		; Reset ROM subsystem -- why ???
		mov	dx, PORT_ROM_SUBSYSTEM1
		xor_	al, al
		out	dx, al

		call	VidOpenV6366
		push	ax
		mov	si, kVidExpRegs_External
		call	VidInitExpRegs
		pop	ax
		call	VidSetModeReg
		call	VidLoadColMapExpRegs
		; call	VidCloseV6366	; should call this but don't ???

		xor_	al, al		; VID_EXTERNAL_ENABLE
		mov	dx, PORT_VID_EXTERNAL
		out	dx, al
		pop	ds
		retn
		ENDPROC	VidInit_External

; ===========================================================================
; VidInit_LCD
; Initializes the video controller to drive an internal 640x200 single screen
; single-drive LCD panel.
; ===========================================================================
VidInit_LCD	PROC
		push	ds
		mov	ax, cs
		mov	ds, ax

		; Reset ROM subsystem -- why ???
		mov	dx, PORT_ROM_SUBSYSTEM1
		xor_	al, al
		out	dx, al

		call	VidOpenV6366
		push	ax
		mov	si, kVidExpRegs_LCD_Lo
		call	VidInitExpRegs
		pop	ax
		call	VidSetModeReg
		call	VidLoadColMapExpRegs
		; call	VidCloseV6366	; should call this but don't ???

		mov	al, VID_EXTERNAL_DISABLE
		mov	dx, PORT_VID_EXTERNAL
		out	dx, al
		pop	ds
		retn
		ENDPROC	VidInit_LCD

; ===========================================================================
; VidInit_1Panel_Lo
; Initializes the video controller to drive an internal 640x200 single-panel
; single-drive plasma display.
; ===========================================================================
VidInit_1Panel_Lo	PROC
		push	ds
		mov	ax, cs
		mov	ds, ax

		; Reset ROM subsystem -- why ???
		mov	dx, PORT_ROM_SUBSYSTEM1
		xor_	al, al
		out	dx, al

		call	VidOpenV6366
		push	ax
		mov	si, kVidExpRegs_1Panel_Lo
		call	VidInitExpRegs
		pop	ax
		call	VidSetModeReg
		call	VidLoadColMapExpRegs
		; call	VidCloseV6366	; should call this but don't ???

		mov	al, VID_EXTERNAL_DISABLE
		mov	dx, PORT_VID_EXTERNAL
		out	dx, al
		pop	ds
		retn
		ENDPROC	VidInit_1Panel_Lo

; ===========================================================================
; VidInit_1Panel_Hi
; Initializes the video controller to drive an internal 640x400 single panel
; single-drive plasma display.
; ===========================================================================
VidInit_1Panel_Hi	PROC
		push	ds
		mov	ax, cs
		mov	ds, ax

		; Reset ROM subsystem -- why ???
		mov	dx, PORT_ROM_SUBSYSTEM1
		xor_	al, al
		out	dx, al

		call	VidOpenV6366
		push	ax
		mov	si, kVidExpRegs_1Panel_Hi
		call	VidInitExpRegs
		pop	ax
		call	VidSetModeReg
		call	VidLoadColMapExpRegs
		; call	VidCloseV6366	; should call this but don't ???

		mov	al, VID_EXTERNAL_DISABLE
		mov	dx, PORT_VID_EXTERNAL
		out	dx, al
		pop	ds
		retn
		ENDPROC	VidInit_1Panel_Hi

; ===========================================================================
; VidInit_2Panel_Lo
; Initializes the video controller to drive an internal 640x200 dual-screen
; dual-drive LCD display.
; ===========================================================================
VidInit_2Panel_Lo	PROC
		push	ds
		mov	ax, cs
		mov	ds, ax

		; Reset ROM subsystem -- why ???
		mov	dx, PORT_ROM_SUBSYSTEM1
		xor_	al, al
		out	dx, al

		call	VidOpenV6366
		push	ax
		mov	si, kVidExpRegs_2Panel_Lo
		call	VidInitExpRegs
		pop	ax
		call	VidSetModeReg
		call	VidLoadColMapExpRegs
		; call	VidCloseV6366	; should call this but don't ???

		mov	al, VID_EXTERNAL_DISABLE
		mov	dx, PORT_VID_EXTERNAL
		out	dx, al
		pop	ds
		retn
		ENDPROC	VidInit_2Panel_Lo


; ===========================================================================
; VidInit_2Panel_Hi
; Initializes the video controller to drive an internal 640x400 dual-screen
; dual-drive LCD display
; ===========================================================================
VidInit_2Panel_Hi	PROC
		push	ds
		mov	ax, cs
		mov	ds, ax

		; Reset ROM subsystem -- why ???
		mov	dx, PORT_ROM_SUBSYSTEM1
		xor_	al, al
		out	dx, al

		call	VidOpenV6366
		push	ax
		mov	si, kVidExpRegs_2Panel_Hi
		call	VidInitExpRegs
		pop	ax
		call	VidSetModeReg
		call	VidLoadColMapExpRegs
		; call	VidCloseV6366	; should call this but don't ???

		mov	al, VID_EXTERNAL_DISABLE
		mov	dx, PORT_VID_EXTERNAL
		out	dx, al
		pop	ds
		retn
		ENDPROC	VidInit_2Panel_Hi

; ===========================================================================
; VidInit_Internal
; Examines the current video mode and video hardware, then calls the
; appropriate GRiD-specific initialization code to initialize the internal
; video panel.
; ===========================================================================
VidInit_Internal	PROC
		; Get current video mode into AH
		push	ds
		mov	ax, BDA_SEGMENT
		mov	ds, ax
		mov	bx, VidActiveMode
		mov	ah, [bx]
		pop	ds

		; Get video hardware type into AL
		mov	dx, PORT_VID_ROMSTATUS
		in	al, dx
		and	al, VID_TYPE_INTERNAL_MASK

		; Plasma 640x400?
		cmp	al, VID_TYPE_PLASMA_400
		jnz	.notPlasma400
		cmp	ah, VID_MODE_EXT_40
		jb	.plasma400Lo
		call	VidInit_1Panel_Hi
		jmp	.leaveFunction
.plasma400Lo	call	VidInit_1Panel_Lo
		jmp	.leaveFunction

.notPlasma400	; LCD 640x400?
		cmp	al, VID_TYPE_LCD_400
		jnz	.notLcd400
		cmp	ah, VID_MODE_EXT_40
		jb	.lcd400Lo
		call	VidInit_2Panel_Hi
		jmp	.leaveFunction
.lcd400Lo	call	VidInit_2Panel_Lo
		jmp	.leaveFunction

.notLcd400	; Plasma 640x200?
		cmp	al, VID_TYPE_PLASMA_200
		jnz	.notPlasma200
		call	VidInit_1Panel_Lo
		jmp	.leaveFunction

.notPlasma200	; LCD 640x200?
		cmp	al, VID_TYPE_LCD_200
		jnz	.leaveFunction
		call	VidInit_LCD

.leaveFunction	retn
		ENDPROC	VidInit_Internal

; ===========================================================================
; GridVidInitHi
; Checks whether the video system supports the GRiD-specific 640x400
; hi-resolution mode and if so, initializes the video controller.
; ===========================================================================
GridVidInitHi	PROC
		mov	dx, PORT_ROM_SUBSYSTEM1
		xor_	al, al
		out	dx, al		; reset subsystem???

		mov	dx, PORT_VID_ROMSTATUS
		in	al, dx		; read video hardware type

		test	al, VID_TYPE_EXTERNAL
		jz	.leaveFailure	; external display doesn't support hi-res

		and	al, VID_TYPE_INTERNAL_MASK
		cmp	al, VID_TYPE_PLASMA_400
		jnz	.notPlasma400
		call	VidInit_1Panel_Hi
		jmp	.leaveSuccess
.notPlasma400	cmp	al, VID_TYPE_LCD_400
		jnz	.leaveFailure	; other displays don't support hi-res
		call	VidInit_2Panel_Hi
		jmp	.leaveSuccess

.leaveFailure	stc
		retn
.leaveSuccess	clc
		retn
		ENDPROC	GridVidInitHi

; ===========================================================================
; GridVidInitLo
; Initialize the video controller for low-resolution modes.
; ===========================================================================
GridVidInitLo	PROC
		mov	dx, PORT_ROM_SUBSYSTEM1
		xor_	al, al
		out	dx, al		; reset subsystem???

		mov	dx, PORT_VID_ROMSTATUS
		in	al, dx		; read video hardware type

		test	al, VID_TYPE_EXTERNAL
		jnz	.notExternal
		call	VidInit_External
		jmp	.leaveSuccess

.notExternal	and	al, VID_TYPE_INTERNAL_MASK
		cmp	al, VID_TYPE_PLASMA_400
		jnz	.notPlasma400
		call	VidInit_1Panel_Lo
		jmp	.leaveSuccess
.notPlasma400	cmp	al, VID_TYPE_LCD_400
		jnz	.leaveSuccess
		call	VidInit_2Panel_Lo
		jmp	.leaveSuccess

.leaveSuccess	clc
		retn
		ENDPROC	GridVidInitLo

; ===========================================================================
; GridToggleExt
; Toggles between the internal display and an external monitor.
; ===========================================================================
GridToggleExt	PROC
		push	ax
		push	bx
		push	cx
		push	dx
		push	si
		push	di
		push	ds
		push	es

		mov	dx, PORT_ROM_SUBSYSTEM1
		in	al, dx
		push	ax

		mov	dx, PORT_VID_ROMSTATUS
		in	al, dx
		and	al, VID_TYPE_EXTERNAL
		jnz	.gotoExternal

		; Active internal panel
		call	VidInit_Internal
		jmp	.leaveFunction

.gotoExternal	; Activate external screen
		call	VidInit_External

.leaveFunction	; Restore subsystem state???
		pop	ax
		mov	dx, PORT_ROM_SUBSYSTEM1
		out	dx, al

		pop	es
		pop	ds
		pop	di
		pop	si
		pop	dx
		pop	cx
		pop	bx
		pop	ax
		retn
		ENDPROC	GridToggleExt

; Values for video secondary registers 0-1 (colour palette 0) and delay values.
; Only used for plasma screens.
kVidBootColors	d b	0, 20h		; Display off
		d b	0, 40h		; 1/2 grayscale
		d b	1, 0		; 1/1 grayscale
kVidBootDelays	d w	1000
		d w	125
		d w	125
		d w	2500

; ===========================================================================
; VidInit
; ===========================================================================
VidInit		PROC
		push	si
		push	bx
		push	es
		push	ds
		mov	ax, cs
		mov	ds, ax

		; Reset ROM subsystem -- why ???
		mov	dx, PORT_ROM_SUBSYSTEM1
		xor_	al, al
		out	dx, al

		call	VidInitV6366

		mov	dx, PORT_V6366_CONTROLID
		mov	al, 81h		; unlock expansion registers
		out	dx, al

		sub	dx, (PORT_V6366_CONTROLID - PORT_V6366_BANK_ADDR)
		mov	al, 29h		; select V6366 expansion register 29h
		out	dx, al
		inc	dx		; increment to PORT_V6366_BANK_DATA
		mov	al, 40h		; set RREG=1 to enable access to control
					; functions from V6366 Control/IO register
		out	dx, al
		mov	dl, (PORT_V6366_MODE_CTRL & 0FFh)
		in	al, dx
		push	ax		; save V6366 mode control register

		mov	dx, PORT_V6366_CADDR1
		xor_	al, al
		call	VidSetModeReg
		call	VidSetColorSelectReg
		call	VidInitSecRegs
		call	VidInit_Internal

		pop	ax		; restore V6366 mode control register
		mov	dx, PORT_V6366_CADDR1
		call	VidSetModeReg
		mov	dx, PORT_VID_ROMSTATUS
		in	al, dx
		and	al, VID_TYPE_INTERNAL_MASK
		cmp	al, VID_TYPE_PLASMA_400
		jz	.plasmaScreen
		cmp	al, VID_TYPE_PLASMA_200
		jz	.plasmaScreen
		jmp	.leaveFunction
		FillerNop

.plasmaScreen	call	WaitHdSpinUp	; ???

		mov	ax, CGA_REGEN_SEG
		mov	es, ax
		mov	bx, 0

		; ??? Display an underscore for a short while.
		;     Is this trying to emulate a cursor?
		; BUG: the underscore is black-on-black.  Bug or hastily removed feature?
		mov	ah, 0		; black-on-black text...
		mov	al, '_'		; ...display an underscore
		mov	si, 0		; SI will be unsed to index various arrays
		mov	cx, [cs:si+kVidBootDelays]
		mov	[es:bx], ax	; write char to top-left of screen
.delay1		call	TenthMilliDelay
		loop	.delay1

		; Remove underscore from the screen
		mov	ah, 7		; white-on-black text...
		mov	al, ' '		; ...replace underscore with space
		mov	[es:bx], ax

		; Unlock V6366 secondary registers
		mov	dx, PORT_V6366_CONTROLID
		mov	al, 81h
		out	dx, al

		; Run through the color/delay arrays, setting palette 0 so the entire
		; screen gradually brightens.
		; ??? Is this to ensure max power draw (fullbright screen plus HD spinning)
		;     to try to diagnose dodgy power supplies?
		mov	si, 0
.plasmaLoop	mov	dx, PORT_V6366_BANK_ADDR
		mov	al, 0		; expansion register 0: palette 0
		out	dx, al
		mov	dx, PORT_V6366_BANK_DATA
		mov	al, [cs:si+kVidBootColors]
		out	dx, al
		Delay	2		; inc/mov provides the third delay
		inc	si
		mov	al, [cs:si+kVidBootColors]
		out	dx, al
		inc	si
		mov	cx, [cs:si+kVidBootDelays]
		dec	si
.delay2		call	TenthMilliDelay
		loop	.delay2
		inc	si
		cmp	si, 5
		jbe	.plasmaLoop

		; Reset palette back to normal
		mov	dx, PORT_V6366_BANK_ADDR
		mov	al, 0
		out	dx, al
		mov	dx, PORT_V6366_BANK_DATA
		out	dx, al
		Delay	3
		out	dx, al

		; Lock V6366 secondary registers
		mov	dx, PORT_V6366_CONTROLID
		mov	al, 1
		out	dx, al

.leaveFunction	pop	ds
		pop	es
		pop	bx
		pop	si
		retn
		ENDPROC	VidInit

ENDPROGRAM	GRID_VID
