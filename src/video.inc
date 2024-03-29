
; ---------------------------------------------------------------------------
; Monochrome display adapter ports
PORT_MDA_CRTC_ADDR	equ	3B4h
PORT_MDA_CRTC_DATA	equ	3B5h
PORT_MDA_MODE_CTRL	equ	3B8h
PORT_MDA_STATUS		equ	3BAh
PORT_MDA_CLEAR_LIGHTPEN	equ	3BBh

; ---------------------------------------------------------------------------
; Colour graphics adapter ports
; There is code in the BIOS that assumes the high-byte of these port numbers
; is the same as the equivalent MDA register (i.e. ports are 20h apart and the
; same code can work on both adaptors by swapping out the low-byte depending
; on the adapter type).
PORT_CGA_CRTC_ADDR	equ	3D4h
PORT_CGA_CRTC_DATA	equ	3D5h
PORT_CGA_MODE_CTRL	equ	3D8h
PORT_CGA_COLOR_CTRL	equ	3D9h
PORT_CGA_STATUS		equ	3DAh
PORT_CGA_CLEAR_LIGHTPEN	equ	3DBh

; ---------------------------------------------------------------------------
; (R-) GRiD video board status
; Mentioned in [TechRef 3-31] as "7F8 - 7FF: Write video ROM, read video status"
; but no further documentation.  May be a single port with incomplete decoding.
; Update, 2022-10-29:
;   Int15/E4/22h (set video font) writes the font index (0-3) to ports
;   7FBh (lo bit) and 7FCh (hi bit).  Value is ANDed with 1 before writing.
;   TODO: Investigate whether there are other bits in these registers that do anything...
PORT_VID_ROMSTATUS	equ	7F8h

; Values read from PORT_VID_ROMSTATUS
VID_TYPE_UNKNOWN_MASK	equ	0F0h
VID_TYPE_EXTERNAL	equ	08h	; 0=external active, 1=internal active
VID_TYPE_INTERNAL_MASK	equ	07h
VID_TYPE_PLASMA_200	equ	01h	; Plasma/EL 640x200
;				02h-04h	  Not used
VID_TYPE_PLASMA_400	equ	05h	; Plasma/EL 640x400
VID_TYPE_LCD_200	equ	06h	; LCD 640x200
VID_TYPE_LCD_400	equ	07h	; LCD 640x400

; ---------------------------------------------------------------------------
; (-W) GRiD video board font
; Two-bit value spread over two ports.
; TODO: documented in the hardware service student course???
PORT_VID_FONT_LO	equ	7FBh	; only lo bit valid
PORT_VID_FONT_HI	equ	7FCh	; only lo bit valid

; ---------------------------------------------------------------------------
; (-W) External display toggle
; Switches video output from the V6366 chip to the external D-sub connector
PORT_VID_EXTERNAL	equ	7FDh

; Values written to PORT_VID_EXTERNAL
; Bits other than bit0 are assumed undefined
VID_EXTERNAL_ENABLE	equ	0
VID_EXTERNAL_DISABLE	equ	1

; ---------------------------------------------------------------------------
; Supported BIOS video modes
;			text/ text pixel   pixel   colors disply scrn notes
;			grph resol  box  resolution       pages  addr
VID_MODE_CGA_0	equ 00h	; T  40x25  8x8   320x200  16gray    8   B800
VID_MODE_CGA_1	equ 01h	; T  40x25  8x8   320x200  16        8   B800
VID_MODE_CGA_2	equ 02h	; T  80x25  8x8   640x200  16gray    4   B800
VID_MODE_CGA_3	equ 03h	; T  80x25  8x8   640x200  16        4   B800
VID_MODE_CGA_4	equ 04h	; G  40x25  8x8   320x200  4         .   B800
VID_MODE_CGA_5	equ 05h	; G  40x25  8x8   320x200  4gray     .   B800
VID_MODE_CGA_6	equ 06h	; G  80x25  8x8   640x200  2         .   B800
VID_MODE_MDA_7	equ 07h	; T  80x25  9x14  720x350  mono     var  B000 *1
VID_MODE_EXT_40	equ 40h	; G  80x25  8x16  640x400  2         1   B800 *2
VID_MODE_EXT_48	equ 48h	; G  80x50  8x8   640x400  2         .   B800 *2
VID_MODE_EXT_74	equ 74h	; G    .     .    640x400  2         .   B800 *2

; Note 1: "With an IBM-type monochrome adapter,the video display begins at
;          address B0000h.  However, the GRiDCase 1500 Series computers do not
;          support the IBM monochrome adapter so the starting address is
;          B8000h for all display modes." [TechRef 7-2]
;          TODO: check whether mode 7 works and whether B0000h mirrors B8000h.
;
; Note 2: "The 640x400 graphics mode display is compatible at the ROM-BIOS
;          level with both the AT&T 6300 and Toshiba T3100 computers."
;         [TechRef 7-8]
;         The AT&T 6300 is also known as the Olivetti M24.

; Some code checks the video mode to determine CGA/MDA
VID_FIRST_CGA_GRAP_MODE	equ	VID_MODE_CGA_4
VID_LAST_CGA_MODE	equ	VID_MODE_CGA_6
VID_80COL_CGA_GRAP_MODE	equ	VID_MODE_CGA_6
VID_MDA_MODE		equ	VID_MODE_MDA_7

; ---------------------------------------------------------------------------
; 6845 CRT Controller register numbers.
; The Yamaha V6366 used in the GRiDCase 1500 series video boards has
; 6854-compatible registers mapped at the same addresses as the IBM CGA/MDA.

CRTC_REG_HTOTAL		equ	00h	; Horizontal total
CRTC_REG_HDISPLAY	equ	01h	; Horizontal display
CRTC_REG_HSYNC_POS	equ	02h	; Horizontal sync position
CRTC_REG_SYNC_PULSE	equ	03h	; Sync pulse width
CRTC_REG_VTOTAL		equ	04h	; Vertical total
CRTC_REG_VTOTAL_ADJ	equ	05h	; Vertical total adjust
CRTC_REG_VDISPLAY	equ	06h	; Vertical display
CRTC_REG_VSYNC_POS	equ	07h	; Vertical sync position
CRTC_REG_INTERLACE	equ	08h	; Interlace
CRTC_REG_MAX_RASTER	equ	09h	; Maximum raster address
CRTC_REG_CURSOR_START	equ	0Ah	; Cursor start raster
CRTC_REG_CURSOR_END	equ	0Bh	; Cursor end raster
CRTC_REG_STARTADDR_HI	equ	0Ch	; Start address high-byte
CRTC_REG_STARTADDR_LO	equ	0Dh	; Start address low-byte
CRTC_REG_CURSORADDR_HI	equ	0Eh	; Cursor address high-byte
CRTC_REG_CURSORADDR_LO	equ	0Fh	; Cursor address low-byte
CRTC_REG_LPEN_HI	equ	10h	; Light-pen high-byte
CRTC_REG_LPEN_LO	equ	11h	; Light-pen low-byte

; Some code addresses CRTC registers in pairs, with two register numbers stored
; in a single 16-bit register.
CRTC_RPAIR_STARTADDR	equ	(CRTC_REG_STARTADDR_HI << 8) | CRTC_REG_STARTADDR_LO
CRTC_RPAIR_CURSORADDR	equ	(CRTC_REG_CURSORADDR_HI << 8) | CRTC_REG_CURSORADDR_LO

; ---------------------------------------------------------------------------
; Misc video constants

MDA_REGEN_SEG		equ	0B000h
MDA_REGEN_LEN		equ	0800h

CGA_REGEN_SEG		equ	0B800h
CGA_REGEN_LEN		equ	2000h

; Offset from MDA/CGA register index port to mode control port
VID_MODE_PORT_OFFSET	equ	4

; Offset from CGA register index port to color control port
VID_CCR_PORT_OFFSET	equ	5

; Offset from MDA/CGA register index port to status port
VID_STATUS_PORT_OFFSET	equ	6

; Offset from MDA/CGA register index port to lightpen port
VID_LPEN_PORT_OFFSET	equ	7
