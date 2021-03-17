
; ---------------------------------------------------------------------------
; AT-compatible hard disk controller ports

; [TechRef 10-12] (RW) Data Register
PORT_HDC_DATA		equ	1F0h
HDC_DATA_SECTOR_SIZE	equ	512	; bytes per sector
HDC_DATA_ECC_SIZE	equ	4	; additional bytes for long read/write

; [TechRef 10-13] (R-) Error Register
PORT_HDC_ERROR		equ	1F1h
HDC_ERROR_RESERVED	equ	28h	; Documented as "not used"
HDC_ERROR_DAMNF		equ	01h	; Data address mark not found
					;   [TechRef 10-13] documents this as "not used" but it is checked in the BIOS.
					;   RBIL documents it as "DAM not found (always 0 for CP-3022)"
HDC_ERROR_TK0		equ	02h	; Track0 not found during recalibrate
HDC_ERROR_ABRT		equ	04h	; Command aborted
HDC_ERROR_IDNF		equ	10h	; Requested sector ID not found
HDC_ERROR_UNC		equ	40h	; ECC failed to correct data error
HDC_ERROR_BBK		equ	80h	; Bad block mark found in sector ID field

; [TechRef 10-14] (-W) Write Precompensation Register
PORT_HDC_WRPRECOMP	equ	1F1h

; [TechRef 10-14] (RW) Sector Count Register
PORT_HDC_SECTORC	equ	1F2h

; [TechRef 10-14] (RW) Sector Number Register
PORT_HDC_SECTORN	equ	1F3h

; [TechRef 10-14] (RW) Cylinder Low/High Registers
PORT_HDC_CYL_LO		equ	1F4h
PORT_HDC_CYL_HI		equ	1F5h

; [TechRef 10-15] (RW) Select Drive/Head Register
PORT_HDC_SDH		equ	1F6h
HDC_SDH_HEAD		equ	00Fh	; Head select bits
HDC_SDH_DRV		equ	010h	; Drive select bit
HDC_SDH_RESERVED	equ	0E0h	; Documented as "reserved"

; [TechRef 10-15] (R-) Status Register
PORT_HDC_STATUS		equ	1F7h
HDC_STATUS_ERR		equ	01h	; Previous command ended with error
HDC_STATUS_IXD		equ	02h	; Index pulse from disk
HDC_STATUS_CORR		equ	04h	; Correctable error has been corrected
HDC_STATUS_DRQ		equ	08h	; Drive is ready for data transfer via PORT_HDC_DATA
HDC_STATUS_DSC		equ	10h	; Drive seek complete
HDC_STATUS_DWF		equ	20h	; Drive write fault
HDC_STATUS_DRDY		equ	40h	; Drive is up to speed and ready to accept commands
HDC_STATUS_BSY		equ	80h	; Drive busy, all other status bits invalid


; [TechRef 10-16] (-W) Command Register
PORT_HDC_COMMAND	equ	1F7h

; [TechRef 10-18] (R-) Alternate Status Register
; Same as PORT_HDC_STATUS but does not clear pending interrupt when read.
PORT_HDC_ALTSTATUS	equ	3F6h

; [TechRef 10-18] (-W) Digital Output Register
PORT_HDC_DIGOUT		equ	3F6h
HDC_DIGOUT_UNDEFINED	equ	0F9h	; Documented as "not used"
HDC_DIGOUT_IEN		equ	002h	; Interrupt enable
HDC_DIGOUT_SRST		equ	004h	; System software reset

; [TechRef 10-18] (--) Drive Address Register
; Documented as "not used by the GRiDCase 1500 Series computers".
; No definition given for its contents.
PORT_HDC_DRIVEADDR	equ	3F7h

