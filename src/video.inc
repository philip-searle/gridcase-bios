
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
; Misc video constants

MDA_REGEN_SEG		equ	0B000h
MDA_REGEN_LEN		equ	0800h

CGA_REGEN_SEG		equ	0B800h
CGA_REGEN_LEN		equ	2000h

; Offset from MDA/CGA register index port to status port
VID_STATUS_PORT_OFFSET	equ	6

