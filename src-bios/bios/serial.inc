
; ---------------------------------------------------------------------------
; Serial port subsystem [TechRef 12-5]
; Note that THR/RBR/IER/IIR and BDL registers share the same address and
; cannot be accessed at the same time.  Ref Divisor Latch Access Bit in the
; Line Control register.
PORT_SER1_THR		equ	3F8h	; (-W) transmitter holding register
PORT_SER1_RBR		equ	3F8h	; (R-) receiver buffer register
PORT_SER1_BDL_LSB	equ	3F8h	; (RW) baud divsior latch LSB
PORT_SER1_BDL_MSB	equ	3F9h	; (RW) baud divisor latch MSB
PORT_SER1_IE		equ	3F9h	; (RW) interrupt enable register
PORT_SER1_II		equ	3FAh	; (RW) interrupt identification
PORT_SER1_LCR		equ	3FBh	; (RW) line control register
PORT_SER1_MCR		equ	3FCh	; (RW) modem control register
PORT_SER1_LSR		equ	3FDh	; (R-) line status register
PORT_SER1_MSR		equ	3FEh	; (R-) modem status register
PORT_SER1_ScRATCH	equ	3FFh	; (RW) scratchpad register

PORT_SER2_THR		equ	2F8h	; (-W) transmitter holding register
PORT_SER2_RBR		equ	2F8h	; (R-) receiver buffer register
PORT_SER2_BDL_LSB	equ	2F8h	; (RW) baud divsior latch LSB
PORT_SER2_BDL_MSB	equ	2F9h	; (RW) baud divisor latch MSB
PORT_SER2_IE		equ	2F9h	; (RW) interrupt enable register
PORT_SER2_II		equ	2FAh	; (RW) interrupt identification
PORT_SER2_LCR		equ	2FBh	; (RW) line control register
PORT_SER2_MCR		equ	2FCh	; (RW) modem control register
PORT_SER2_LSR		equ	2FDh	; (R-) line status register
PORT_SER2_MSR		equ	2FEh	; (R-) modem status register
PORT_SER2_ScRATCH	equ	2FFh	; (RW) scratchpad register

; LCR registers probed by POST code for COM2/3.
; Note that these do not exist on the GRiDCase 1500 hardware
; unless provided externally.
PORT_SER3_LCR		equ	3EBH	; (RW) line control register
PORT_SER4_LCR		equ	2EBh	; (RW) line control register

