
; ---------------------------------------------------------------------------
; Programmable Interrupt Controllers
PORT_PIC1_CTRL	equ	20h
PORT_PIC1_MASK	equ	21h

PORT_PIC2_CTRL	equ	0A0h
PORT_PIC2_MASK	equ	0A1h

; ---------------------------------------------------------------------------
; Programmable interval timer
PORT_PIT_COUNTER0	equ	40h
PORT_PIT_COUNTER1	equ	41h
PORT_PIT_COUNTER2	equ	42h
PORT_PIT_MODE		equ	43h

; ---------------------------------------------------------------------------
; Keyboard controller, port B
PORT_KBC_PORTB	equ	61h
; Keyboard controller, status port
PORT_KBC_STATUS	equ	64h

; Bitmasks for PORT_KBC_STATUS reads
KBC_STATUS_PERR	equ	80h	; keyboard data parity error
KBC_STATUS_RxTO	equ	40h	; recieve timeout
KBC_STATUS_TxTO	equ	20h	; transmit timeout
KBC_STATUS_INH	equ	10h	; keyboard comms inhibit
KBC_STATUS_A2	equ	08h	; A2 address line for last port write
KBC_STATUS_SYS	equ	04h	; Last reset type: 0=cold boot, 1=warm boot
KBC_STATUS_IBF	equ	02h	; input buffer full
KBC_STATUS_OBF	equ	01h	; output buffer full

; ---------------------------------------------------------------------------
; CMOS address port (with NMI enable in bit 7)
; Needs setting after each data port access
PORT_CMOS_ADDRESS	equ	70h
; CMOS data port
PORT_CMOS_DATA		equ	71h

; OR with register index to form value for CMOS_ADDRESS_PORT
NMI_ENABLE		equ	00h
NMI_DISABLE		equ	80h

; ---------------------------------------------------------------------------
; Manufacturer's diagnostic output port
PORT_DIAGNOSTICS equ	80h

; Checkpoint values written to diagnostic port during POST
CHECKPOINT_CPU_REGS	equ	01h
CHECKPOINT_CMOS_RW	equ	02h
CHECKPOINT_BIOS_CHECKSUM	equ	03h
CHECKPOINT_TIMER	equ	04h
CHECKPOINT_DMA		equ	05h
CHECKPOINT_DMA_PR	equ	06h
CHECKPOINT_RAM_REFRESH	equ	08h
CHECKPOINT_RAM		equ	09h
CHECKPOINT_RAM_PARITY	equ	0Dh

; Beep codes used for fatal errors before display is initialized
; See FatalBeeps for meaning of these values
BEEP_CMOS_FAIL		equ	02h
BEEP_BIOS_CHECKSUM	equ	03h
BEEP_TIMER_FAIL		equ	04h
BEEP_DMA_FAIL		equ	05h
BEEP_DMA_PR_FAIL	equ	06h
BEEP_RAM_REFRESH	equ	08h
BEEP_RAM_MULTIPLE	equ	0Ah	; 1st 64K, multiple bits
BEEP_RAM_OE		equ	0Bh	; 1st 64K, even/odd logic
BEEP_RAM_ADDRESS_LINE	equ	0Ch	; 1st 64K, address line
BEEP_RAM_PARITY		equ	0Dh	; 1st 64K, parity

; ---------------------------------------------------------------------------
; DMA controller ports [TechRef 4-1]

; DMA page registers
PORT_DMA_PAGE0		equ	87h
PORT_DMA_PAGE1		equ	83h
PORT_DMA_PAGE2		equ	81h
PORT_DMA_PAGE3		equ	82h
; PORT_DMA_PAGE4 used for cascade feature
PORT_DMA_PAGE5		equ	8Bh
PORT_DMA_PAGE6		equ	89h
PORT_DMA_PAGE7		equ	8Ah
PORT_DMA_RAM_REFRESH_ADDRESS	equ	8Fh

; DMA channel 0-3 control registers
PORT_DMA1_STATUS	equ	08h	; (R-)
PORT_DMA1_COMMAND	equ	08h	; (-W)
PORT_DMA1_WRITE_REQUEST	equ	09h	; (-W)
PORT_DMA1_MASK		equ	0Ah	; (RW)
PORT_DMA1_MODE		equ	0Bh	; (-W)
PORT_DMA1_CLEAR_FF	equ	0Ch	; (-W)
PORT_DMA1_MASTER_CLEAR	equ	0Dh	; (-W)

; DMA channel 4-7 control registers
PORT_DMA2_STATUS	equ	0D0h	; (R-)
PORT_DMA2_COMMAND	equ	0D0h	; (-W)
PORT_DMA2_WRITE_REQUEST	equ	0D2h	; (-W)
PORT_DMA2_MASK		equ	0D4h	; (RW)
PORT_DMA2_MODE		equ	0D6h	; (-W)
PORT_DMA2_CLEAR_FF	equ	0D8h	; (-W)
PORT_DMA2_MASTER_CLEAR	equ	0DAh	; (-W)

; DMA controller 1 channels
PORT_DMA_CHAN0_CUR	equ	00h	; (R-) current address
PORT_DMA_CHAN0_BASE	equ	00h	; (-W) base address
PORT_DMA_CHAN0_WC	equ	01h	; (RW) word count
PORT_DMA_CHAN1_CUR	equ	02h	; (R-) current address
PORT_DMA_CHAN1_BASE	equ	02h	; (-W) base address
PORT_DMA_CHAN1_WC	equ	03h	; (RW) word count
PORT_DMA_CHAN2_CUR	equ	04h	; (R-) current address
PORT_DMA_CHAN2_BASE	equ	04h	; (-W) base address
PORT_DMA_CHAN2_WC	equ	05h	; (RW) word count
PORT_DMA_CHAN3_CUR	equ	06h	; (R-) current address
PORT_DMA_CHAN3_BASE	equ	06h	; (-W) base address
PORT_DMA_CHAN3_WC	equ	07h	; (RW) word count

; DMA controller 2 channels
PORT_DMA_CHAN4_CUR	equ	0C0h	; (R-) current address
PORT_DMA_CHAN4_BASE	equ	0C0h	; (-W) base address
PORT_DMA_CHAN4_WC	equ	0C2h	; (RW) word count
PORT_DMA_CHAN5_CUR	equ	0C4h	; (R-) current address
PORT_DMA_CHAN5_BASE	equ	0C4h	; (-W) base address
PORT_DMA_CHAN5_WC	equ	0C6h	; (RW) word count
PORT_DMA_CHAN6_CUR	equ	0C8h	; (R-) current address
PORT_DMA_CHAN6_BASE	equ	0C8h	; (-W) base address
PORT_DMA_CHAN6_WC	equ	0CAh	; (RW) word count
PORT_DMA_CHAN7_CUR	equ	0CCh	; (R-) current address
PORT_DMA_CHAN7_BASE	equ	0CCh	; (-W) base address
PORT_DMA_CHAN7_WC	equ	0CEh	; (RW) word count

; ---------------------------------------------------------------------------
; 80287 FPU/NPU control ports
; Ports 0E8h-0EFh and 0F8h-0FFh are reserved for 286/287 private ocmmunication.
PORT_287RESET	equ	0F1h	; See FE3001A manual, section 2.9 "Coprocessor Interface"

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

; ---------------------------------------------------------------------------
; Monochrome display adapter ports
PORT_MDA_MODE_CTRL	equ	3B8h

; ---------------------------------------------------------------------------
; Colour graphics adapter ports
PORT_CGA_MODE_CTRL	equ	3D8h

; ---------------------------------------------------------------------------
; Mystery ports!
; No idea what these do yet.
PORT_UNKNOWN_426	equ	0426h	; set to 1 then 0 in POST
PORT_UNKNOWN_FFF	equ	0FFFh	; set to 1 in POST
PORT_UNKNOWN_6F8	equ	06F8h	; set to 0 in POST
PORT_UNKNOWN_7FD	equ	07FDh	; set to 0 in POST

