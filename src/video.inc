
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

