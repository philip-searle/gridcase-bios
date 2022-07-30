
; ---------------------------------------------------------------------------
; Programmable Interrupt Controllers
PORT_PIC1_CTRL	equ	20h
PORT_PIC1_MASK	equ	21h

PORT_PIC2_CTRL	equ	0A0h
PORT_PIC2_MASK	equ	0A1h

; Command to issue to PIC to complete IRQ
NONSPECIFIC_EOI	equ	20h

; ---------------------------------------------------------------------------
; IRQ assignments, PIC1 mask values
IRQ_TIMER	equ	0y00000001
IRQ_KEYBOARD	equ	0y00000010
IRQ_CASCADE	equ	0y00000100	; not a real IRQ
;			0y00001000 not used
;			0y00010000 not used
;			0y00100000 not used
IRQ_FDC		equ	0y01000000
;			0y10000000 not used

; ---------------------------------------------------------------------------
; IRQ assignments, PIC2 mask values
; See comment in reset.asm for why IRQ_NPU_BUG exists
IRQ_RTC		equ	0y00000001
IRQ_NPU_BUG	equ	0y00000010
;			0y00000100 not used
;			0y00001000 not used
;			0y00010000 not used
IRQ_NPU		equ	0y00100000
IRQ_HDC		equ	0y01000000
;			0y10000000 not used
