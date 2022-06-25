
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
;				11h reserved
CMOS_HD_TYPE		equ	12h
;				13h reserved
CMOS_EQUIPMENT		equ	14h
CMOS_BASEMEM_LOBYTE	equ	15h
CMOS_BASEMEM_HIBYTE	equ	16h
CMOS_EXPMEM_LOBYTE	equ	17h
CMOS_EXPMEM_HIBYTE	equ	18h
CMOS_HD_EXTTYPE1	equ	19h
CMOS_HD_EXTTYPE2	equ	1Ah
;				1Bh-2Dh reserved
CMOS_CHECKSUM_LO	equ	2Eh
CMOS_CHECKSUM_HI	equ	2Fh
CMOS_EXPMEM2_LOBYTE	equ	30h
CMOS_EXPMEM2_HIBYTE	equ	31h
CMOS_INFORMATION	equ	33h
CMOS_GRIDFLAGS		equ	3Fh	; [TechRef 6-17] documented as 'reserved'

; ---------------------------------------------------------------------------
; Valid values for the CMOS_SHUTDOWN_REASON register
; [Compat] These differ from the values used in the IBM BIOS!  The values
;          used after return from protected-mode are swapped around.
SD_SOFT_RESET		equ	00h	; Soft reset, skip POST
SD_AFTER_MEMSIZE	equ	01h	; Reset after mem size determined
SD_AFTER_MEMTEST	equ	02h	; Reset after mem tests performed
SD_AFTER_MEMERROR	equ	03h	; Reset after parity check
SD_BOOTLOADER_REQ	equ	04h	; Boot loader request ???
SD_JMP_WITH_INT		equ	05h	; Reset (JMP to [0:0467h] after resetting PIC)
SD_UNKNOWN_6		equ	06h	;
SD_PM_BLOCK_MOVE	equ	07h	; Reset after performing block move
SD_PM_TEST1		equ	08h	; Reset after protected-mode test 1
SD_UNKNOWN_9		equ	09h	;
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

; ---------------------------------------------------------------------------
; Valid values for the CMOS_FD_TYPE register (upper/lower nibble = FD0/FD1)
; Note that [TechRef 6-15] documents all values greater than two as 'reserved',
; despite FdDasdType checking values up to four.
FD_NONE			equ	00h
FD_5_360K		equ	01h	; 360KB 5.25" drive
FD_5_1M			equ	02h	; 1.2MB 5.25" drive
FD_3_720K		equ	03h	; 720KB 3.5" drive
FD_3_1M			equ	04h	; 1.44MB 3.5" drive

