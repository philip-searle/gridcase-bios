
; ---------------------------------------------------------------------------
; Monochrome display adapter ports
PORT_MDA_MODE_CTRL	equ	3B8h

; ---------------------------------------------------------------------------
; Colour graphics adapter ports
PORT_CGA_MODE_CTRL	equ	3D8h

; ---------------------------------------------------------------------------
; Video controller ports

; Mentioned in [TechRef 3-31] as "7F8 - 7FF: Write video ROM, read video status"
; but no further documentation.  May be a single port with incomplete decoding.
PORT_VID_ROMSTATUS	equ	7F8h

; ---------------------------------------------------------------------------
; Supported BIOS video modes
;			text/ text pixel   pixel   colors disply scrn notes
;			grph resol  box  resolution       pages  addr
VID_MODE_0	equ 00h	; T  40x25  8x8   320x200  16gray    8   B800
VID_MODE_1	equ 01h	; T  40x25  8x8   320x200  16        8   B800
VID_MODE_2	equ 02h	; T  80x25  8x8   640x200  16gray    4   B800
VID_MODE_3	equ 03h	; T  80x25  8x8   640x200  16        4   B800
VID_MODE_4	equ 04h	; G  40x25  8x8   320x200  4         .   B800
VID_MODE_5	equ 05h	; G  40x25  8x8   320x200  4gray     .   B800
VID_MODE_6	equ 06h	; G  80x25  8x8   640x200  2         .   B800
VID_MODE_7	equ 07h	; T  80x25  9x14  720x350  mono     var  B000 *1
VID_MODE_40	equ 40h	; G  80x25  8x16  640x400  2         1   B800 *2
VID_MODE_48	equ 48h	; G  80x50  8x8   640x400  2         .   B800 *2
VID_MODE_74	equ 74h	; G    .     .    640x400  2         .   B800 *2

; Note 1: "With an IBM-type monochrome adapter,the video display begins at
;          address B0000h.  However, the GRiDCase 1500 Series computers do not
;          support the IBM monochrome adapter so the starting address is
;          B8000h for all display modes." [TechRef 7-2]
;          TODO: check whether mode 7 works and whether B0000h mirrors B8000h.
;
; Note 2: "The 640x400 graphics mode display is compatible at the ROM-BIOS
;          level with both the AT&T 6300 and Toshiba T3100 computers."
;         [TechRef 7-8]

; Some code checks the video mode to determine CGA/MDA
VID_LAST_CGA_MODE	equ	VID_MODE_6
VID_MDA_MODE		equ	VID_MODE_7

; ---------------------------------------------------------------------------
; Misc video constants

MDA_REGEN_SEG		equ	0B000h
MDA_REGEN_LEN		equ	0800h

CGA_REGEN_SEG		equ	0B800h
CGA_REGEN_LEN		equ	2000h

; Offset from MDA/CGA register index port to mode control port
VID_MODE_PORT_OFFSET	equ	4

; Offset from MDA/CGA register index port to status port
VID_STATUS_PORT_OFFSET	equ	6

