; ===========================================================================
; Ports used by the GRiD VGA module (I think)
; ===========================================================================

; ---------------------------------------------------------------------------
; (RW) IBM onboard VGA - GLOBAL ENABLE REGISTER
; Note: Only accessible in setup mode (port 46E8h bit 4 = 1).
PORT_VGA_GLOBAL_ENABLE		equ	102h
VGA_GLOBAL_DISABLE		equ	000h
VGA_GLOBAL_ENABLE		equ	001h
VGA_GLOBAL_ENABLE_RESERVED	equ	0FEh

; ---------------------------------------------------------------------------
; (RW) IBM VGA adaptor - VIDEO ADAPTOR ENABLE
PORT_VGA_ADAPTOR_ENABLE		equ	46E8h
VGA_ADAPTOR_DISABLE		equ	000h	; disable ROM, display memory, all IO except 102h/46E8h
VGA_ADAPTOR_SETUP		equ	010h	; enable IO ports 102h/46E8h, and BIOS ROM but disable display RAM
VGA_ADAPTOR_ENABLE		equ	008h	; enable display adaptor IO access
VGA_ADAPTOR_UNKNOWN1		equ	004h	; ???
VGA_ADAPTOR_UNKNOWN2		equ	002h	; ???
VGA_ADAPTOR_RESERVED		equ	0E1h

; ---------------------------------------------------------------------------
; (RW) VGA video subsystem enable
PORT_VGA_SUBSYS_ENABLE		equ	3C3h
VGA_SUBSYS_DISABLE		equ	000h	; vga adaptor disabled
VGA_SUBSYS_ENABLE_03C3		equ	001h	; select video subsystem (address 03C3h)
VGA_SUBSYS_ENABLE_46E8		equ	008h	; select video subsystem (address 46E8h)
VGA_SUBSYS_ENABLE_RESERVED	equ	0F6h

; ---------------------------------------------------------------------------
; (-W) PS/2 programable option select - system board enable/setup register
PORT_POS_SYSTEM_BOARD		equ	094h
POS_SYSTEM_BOARD_UNKNOWN1	equ	0DFh	; ??? might be VGA setup enable?

; ---------------------------------------------------------------------------
; VGA module type identifiers
VGA_TYPE_0			equ	0	; No VGA present, fallback to CGA int10 handler
VGA_TYPE_1			equ	1	; VGA with BIOS containing "GRiD Systems Corp" only
VGA_TYPE_2			equ	2	; VGA with BIOS containing "GRiD Systems Corp" + "545 VGA "
VGA_TYPE_3			equ	3	; Unknown VGA option ROM
VGA_TYPE_4			equ	4	; VGA BIOS containing "Cirrus Logic" or "Ph" signature
