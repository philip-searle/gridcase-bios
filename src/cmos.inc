
; ---------------------------------------------------------------------------
; CMOS address port (with NMI enable in bit 7)
; Needs setting after each data port access
PORT_CMOS_ADDRESS      equ     70h
; CMOS data port
PORT_CMOS_DATA         equ     71h

; OR with register index to form value for CMOS_ADDRESS_PORT
NMI_ENABLE             equ     00h
NMI_DISABLE            equ     80h

; ---------------------------------------------------------------------------
; CMOS register indexes
CMOS_STATUS_A		equ	0Ah
CMOS_STATUS_B		equ	0Bh
CMOS_STATUS_C		equ	0Ch
CMOS_STATUS_D		equ	0Dh
CMOS_STATUS_DIAG	equ	0Eh
CMOS_SHUTDOWN_REASON	equ	0Fh
CMOS_FD_TYPE		equ	10h
CMOS_EQUIPMENT		equ	14h
CMOS_BASEMEM_LOBYTE	equ	15h
CMOS_BASEMEM_HIBYTE	equ	16h
CMOS_EXPMEM_LOBYTE	equ	17h
CMOS_EXPMEM_HIBYTE	equ	18h
CMOS_EXPMEM2_LOBYTE	equ	30h
CMOS_EXPMEM2_HIBYTE	equ	31h

; ---------------------------------------------------------------------------
; Valid values for the CMOS_SHUTDOWN_REASON register
SD_SOFT_RESET		equ	00h
SD_AFTER_MEMSIZE	equ	01h
SD_AFTER_MEMTEST	equ	02h
SD_AFTER_MEMERROR	equ	03h
SD_BOOTLOADER_REQ	equ	04h
SD_JMP_WITH_INT		equ	05h
SD_PM_TEST7_PASS	equ	06h
SD_PM_TEST7_FAIL	equ	07h
SD_PM_TEST1_FAIL	equ	08h
SD_BLOCK_MOVE		equ	09h
SD_JMP_WITHOUT_INT	equ	0Ah

; Unknown shutdown reason, checked for in POST
SD_UNKNOWN_FC		equ	0FCh

