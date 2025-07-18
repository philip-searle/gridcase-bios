
		include	"video.inc"

; ---------------------------------------------------------------------------
; (-W) Controller address registers
; Write secondary register index here to make it available in the
; corresponding PORT_V6366_CDATA port.  Each address/data port pair operates
; identically but independently.
; The 1520 BIOS always uses the first address/data pair to access the V6366
; secondary registers because the third pair is mapped to the PC-compatible CGA
; ports and may be used by non-BIOS code in unpredictable ways.
PORT_V6366_CADDR1	equ	3D0h
PORT_V6366_CADDR2	equ	3D2h
PORT_V6366_CADDR3	equ	3D4h
PORT_V6366_CADDR4	equ	3D6h
	AssertEqual	PORT_V6366_CADDR3, PORT_CGA_CRTC_ADDR

; ---------------------------------------------------------------------------
; (RW) Controller data registers
; Secondary registers are made available here, depending on the value written
; to the corresponding controller address register.
PORT_V6366_CDATA1	equ	3D1h
PORT_V6366_CDATA2	equ	3D3h
PORT_V6366_CDATA3	equ	3D5h
PORT_V6366_CDATA4	equ	3D9h
	AssertEqual	PORT_V6366_CDATA3, PORT_CGA_CRTC_DATA

; ---------------------------------------------------------------------------
; (RW) Mode control
PORT_V6366_MODE_CTRL	equ	3D8h
	AssertEqual	PORT_V6366_MODE_CTRL, PORT_CGA_MODE_CTRL

; ---------------------------------------------------------------------------
; (RW) Colour select
PORT_V6366_COLOR_SELECT	equ	3D9h
	AssertEqual	PORT_V6366_COLOR_SELECT, PORT_CGA_COLOR_CTRL

; ---------------------------------------------------------------------------
; (R-) Status
PORT_V6366_STATUS	equ	3DAh
	AssertEqual	PORT_V6366_STATUS, PORT_CGA_STATUS

; ---------------------------------------------------------------------------
; (-W) Light pen strobe clear
; [TechRef 7-18] documents this as "not supported"
PORT_V6366_LP_CLEAR	equ	3DBh
	AssertEqual	PORT_V6366_LP_CLEAR, PORT_CGA_CLEAR_LIGHTPEN

; ---------------------------------------------------------------------------
; (-W) Light pen strobe set
; [TechRef 7-18] documents this as "not supported"
PORT_V6366_LP_SET	equ	3DCh

; ---------------------------------------------------------------------------
; (-W) Expansion register bank address
PORT_V6366_BANK_ADDR	equ	3DDh

; ---------------------------------------------------------------------------
; (RW) Expansion register bank data
PORT_V6366_BANK_DATA	equ	3DEh

; ---------------------------------------------------------------------------
; (RW) Control/ID register
PORT_V6366_CONTROLID	equ	3DFh
