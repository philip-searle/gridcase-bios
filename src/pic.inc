
; ---------------------------------------------------------------------------
; Programmable Interrupt Controllers
PORT_PIC1_CTRL	equ	20h
PORT_PIC1_MASK	equ	21h

PORT_PIC2_CTRL	equ	0A0h
PORT_PIC2_MASK	equ	0A1h

; Command to issue to PIC to complete IRQ
NONSPECIFIC_EOI	equ	20h

