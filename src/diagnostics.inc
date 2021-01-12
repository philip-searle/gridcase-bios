
; ---------------------------------------------------------------------------
; Manufacturer's diagnostic output port
PORT_DIAGNOSTICS equ	80h

; Checkpoint values written to diagnostic port during POST
CHECKPOINT_CPU_REGS	equ	01h
CHECKPOINT_CMOS_RW	equ	02h
CHECKPOINT_BIOS_CSUM	equ	03h
CHECKPOINT_TIMER	equ	04h
CHECKPOINT_DMA		equ	05h
CHECKPOINT_DMA_PR	equ	06h
;				07h not used
CHECKPOINT_RAM_REFRESH	equ	08h
CHECKPOINT_RAM		equ	09h
;				0Ah - 0Ch not used
CHECKPOINT_RAM_PARITY	equ	0Dh
;				0Eh - 1Fh not used
CHECKPOINT_DMA1		equ	20h
CHECKPOINT_DMA2		equ	21h
CHECKPOINT_PIC1		equ	22h
CHECKPOINT_PIC2		equ	23h
;				24h not used
CHECKPOINT_IVT		equ	25h
;				26h not used
CHECKPOINT_KEYBOARD	equ	27h
CHECKPOINT_CMOS_CSUM	equ	28h
CHECKPOINT_VIDEO	equ	29h
CHECKPOINT_VIDEO_INIT	equ	2Ch
;				2Dh not used
CHECKPOINT_HD_ROM	equ	2Eh
;				2Fh - 33h not used
CHECKPOINT_TIMER_INT	equ	34h
CHECKPOINT_SHUTDOWN	equ	35h
;				36h - 39h not used
CHECKPOINT_TIMER2	equ	3Ah

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
BEEP_DMA1_REG		equ	20h
BEEP_DMA2_REG		equ	21h
BEEP_PIC1_REG		equ	22h
BEEP_PIC2_REG		equ	23h
BEEP_KBC		equ	27h
;				28h - 33h not used
BEEP_TIMER_INT		equ	34h
BEEP_SHUTDOWN		equ	35h
;				36h - 39h not used
BEEP_TIMER2_FAIL	equ	3Ah

