
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
CMOS_INFORMATION	equ	33h
CMOS_GRIDFLAGS		equ	3Fh	; [TechRef 6-17] documented as 'reserved'

; ---------------------------------------------------------------------------
; Valid values for the CMOS_SHUTDOWN_REASON register
SD_SOFT_RESET		equ	00h	; Soft reset, skip POST
SD_AFTER_MEMSIZE	equ	01h	; Reset after mem size determined
SD_AFTER_MEMTEST	equ	02h	; Reset after mem tests performed
SD_AFTER_MEMERROR	equ	03h	; Reset after parity check
SD_BOOTLOADER_REQ	equ	04h	; Boot loader request ???
SD_JMP_WITH_INT		equ	05h	; Reset (JMP to [0:0467h] after resetting PIC)
SD_PM_TEST7_PASS	equ	06h	; 
SD_PM_TEST7_FAIL	equ	07h	; 
SD_PM_TEST1		equ	08h	; Reset after protected-mode test 1
SD_BLOCK_MOVE		equ	09h	; Reset after performing block move
SD_JMP_WITHOUT_INT	equ	0Ah	; Reset (JMP to [0:0467h] without resetting PIC)

; Unknown shutdown reason, checked for in POST
SD_UNKNOWN_FC		equ	0FCh

; ---------------------------------------------------------------------------
; Flags stored in CMOS_INFORMATION register [TechRef 6-17]
CMOS_INFO_128K		equ	80h	; Top 128K memory installed
CMOS_INFO_FIRST_START	equ	40h	; Setup utility first use message?
CMOS_INFO_RESERVED	equ	3Fh	; Reserved

; ---------------------------------------------------------------------------
; Flags stored in CMOS_GRIDFLAGS.
; Not officially documented.
GF_EMS			equ	01h	; use EMS instead of XMS

