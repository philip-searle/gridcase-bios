
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

; PORT_PRINTER_CTRL bits [TechRef 11-4]
PRINTER_CTRL_STROBE	equ	01h	; Enables valid data onto the bus
PRINTER_CTRL_AFDXT	equ	02h	; Autofeeds one line of paper after printing has stopped
PRINTER_CTRL_INIT	equ	04h	; Initializes the printer
PRINTER_CTRL_SLCTIN	equ	08h	; Allows printer to accept data
PRINTER_CTRL_IRQEN	equ	10h	; Enables IRQ7
					; Bits 5-7 not used

; PORT_PRINTER_STATUSB bits [TechRef 11-5]
PRINTER_STATB_BORA	equ	01h	; External disk drive address (0=A, 1=B)
PRINTER_STATB_MAINPWRLO	equ	02h	; Battery voltage < 10.5VDC when set
PRINTER_STATB_3OR5	equ	04h	; External floppy disk type (0=5.25", 1=3.5")
PRINTER_STATB_ERROR	equ	08h	; Printer error has occurred
PRINTER_STATB_SLCT	equ	10h	; Printer is selected
PRINTER_STATB_PE	equ	20h	; Printer is out of paper
PRINTER_STATB_ACK	equ	40h	; Printer is ready to accept data
PRINTER_STATB_BUSY	equ	80h	; Printer is not ready to accept data
PRINTER_STATB_GRIDBITS	equ	07h	; Mask for GRiD-specific bits

; PORT_PRINTER_STATUSC bits [TechRef 11-6]
PRINTER_STATC_STROBE	equ	01h	; Indicates valid data on the printer data bus
PRINTER_STATC_AFDXT	equ	02h	; Indicates autofeed is on
PRINTER_STATC_INIT	equ	04h	; Indicates printer is initializing
PRINTER_STATC_SLCTIN	equ	08h	; Indicates printer has accepted data
PRINTER_STATC_IRQEN	equ	10h	; Indicates IRQ7 is enabled
PRINTER_STATC_EFLOPPY	equ	20h	; 0=External floppy drive attached, 1=No external floppy
PRINTER_STATC_TAPE	equ	40h	; 0=External tape drive attached, 1=No external tape
PRINTER_STATC_HDDRIVE	equ	80h	; 0=External drive is high density, 1=low density

