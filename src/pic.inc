
; ---------------------------------------------------------------------------
; Programmable Interrupt Controllers
PORT_PIC1_CTRL	equ	20h
PORT_PIC1_MASK	equ	21h

PORT_PIC2_CTRL	equ	0A0h
PORT_PIC2_MASK	equ	0A1h

; Command to issue to PIC to complete IRQ
NONSPECIFIC_EOI	equ	20h

; ---------------------------------------------------------------------------
; IRQ assignments, PIC1 mask values [TechRef 5-3]
IRQ_TIMER	equ	0y00000001
IRQ_KEYBOARD	equ	0y00000010
IRQ_CASCADE	equ	0y00000100	; not a real IRQ
;			0y00001000 COM2
;			0y00010000 COM1 (modem)
;			0y00100000 reserved
IRQ_FDC		equ	0y01000000
;			0y10000000 parallel port

; ---------------------------------------------------------------------------
; IRQ assignments, PIC2 mask values [TechRef 5-3]
; See comment in reset.asm for why IRQ_NPU_BUG exists
IRQ_RTC		equ	0y00000001
IRQ_NPU_BUG	equ	0y00000010	; actually wired to IRQ2 line in hardware
;			0y00000100 reserved
;			0y00001000 reserved
;			0y00010000 reserved
IRQ_NPU		equ	0y00100000
IRQ_HDC		equ	0y01000000
;			0y10000000 reserved
