
; ---------------------------------------------------------------------------
; Parallel port subsystem [TechRef 11-3]
PORT_PRINTER_DATA_R	equ	378h
PORT_PRINTER_STATUSB	equ	379h
PORT_PRINTER_STATUSC	equ	37Ah
PORT_PRINTER_37B	equ	37Bh	; Documented as 'undefined'
PORT_PRINTER_DATA_W	equ	37Ch
PORT_PRINTER_37D	equ	37Dh	; Documented as 'no operation'
PORT_PRINTER_CTRL	equ	37Eh
PORT_PRINTER_37F	equ	37Fh	; Documented as 'undefined'
PORT_PAR_PORTA_R	equ	0FF8h
PORT_PAR_PORTB_R	equ	0FF9h
PORT_PAR_PORTC_R	equ	0FFAh
PORT_PAR_FFB		equ	0FFBh	; Documented as 'undefined'
PORT_PAR_FFC		equ	0FFCh	; Documented as 'not used'
PORT_PAR_PORTB_W	equ	0FFDh

; Ports which are probed by POST code for LPT1/2/3.
; Note that only 378h actually exists on the GRiDCase 1500.
PORT_PAR_CANDIDATE1	equ	3BCh
PORT_PAR_CANDIDATE2	equ	378h
PORT_PAR_CANDIDATE3	equ	278h

