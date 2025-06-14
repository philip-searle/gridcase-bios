
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
CMOS_RTC_SECONDS	equ	00h
CMOS_ALARM_SECONDS	equ	01h
CMOS_RTC_MINUTES	equ	02h
CMOS_ALARM_MINUTES	equ	03h
CMOS_RTC_HOURS		equ	04h
CMOS_ALARM_HOURS	equ	05h
CMOS_RTC_DAY_OF_WEEK	equ	06h
CMOS_RTC_DAY_OF_MONTH	equ	07h
CMOS_RTC_MONTH		equ	08h
CMOS_RTC_YEAR		equ	09h

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
CMOS_CENTURY		equ	32h
CMOS_INFORMATION	equ	33h

CMOS_GRID_PASSWORD	equ	34h	; \
;				35h	; | Not documented in [TechRef], but locations 34h
;				36h	; | through 3Ch hold the eight-byte encrypted BIOS
;				37h	; | boot password.
;				38h	; |
;				39h	; |
;				3Ah	; |
;				3Bh	; /
;				3Ch	; ??? Not used
;				3Dh	; ??? Not used
CMOS_GRIDBACKLITE	equ	3Eh	; Not documented in [TechRef], but contains
					; the backlight timeout for LCD systems

CMOS_GRIDFLAGS		equ	3Fh	; [TechRef 6-17] documents this as 'reserved'
					; [TechRef 2-9] has the correct description (EMS flag)

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
; Specific to the GRiD 1520 BIOS. [TechRef 2-9] only doucments lowest bit.
GF_EMS			equ	01h	; use EMS instead of XMS
GF_PASSWORD		equ	02h	; boot password enabled if set
GF_PW_LANG_MASK		equ	0Ch	; two bits store password prompt language

; ---------------------------------------------------------------------------
; Valid values for the CMOS_GRIDFLAGS, bits GF_PW_LANG_MASK, shifted right by two.
GF_PW_LANG_EN		equ	00h
GF_PW_LANG_DE		equ	01h
GF_PW_LANG_FR		equ	02h

; ---------------------------------------------------------------------------
; Valid values for the CMOS_FD_TYPE register (upper/lower nibble = FD0/FD1)
; Note that [TechRef 6-15] documents all values greater than two as 'reserved',
; despite FdDasdType checking values up to four.
FD_NONE			equ	00h
FD_5_360K		equ	01h	; 360KB 5.25" drive
FD_5_1M			equ	02h	; 1.2MB 5.25" drive
FD_3_720K		equ	03h	; 720KB 3.5" drive
FD_3_1M			equ	04h	; 1.44MB 3.5" drive
