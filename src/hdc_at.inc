
; ---------------------------------------------------------------------------
; AT-compatible hard disk controller ports

; [TechRef 10-12] (RW) Data Register
PORT_HD_AT_DATA		equ	1F0h
HD_AT_DATA_SECTOR_SIZE	equ	512	; bytes per sector
HD_AT_DATA_ECC_SIZE	equ	4	; additional bytes for long read/write

; [TechRef 10-13] (R-) Error Register
PORT_HD_AT_ERROR	equ	1F1h
HD_AT_ERROR_RESERVED	equ	28h	; Documented as "not used"
HD_AT_ERROR_DAMNF	equ	01h	; Data address mark not found
					;   [TechRef 10-13] documents this as "not used" but it is checked in the BIOS.
					;   RBIL documents it as "DAM not found (always 0 for CP-3022)"
HD_AT_ERROR_TK0		equ	02h	; Track0 not found during recalibrate
HD_AT_ERROR_ABRT	equ	04h	; Command aborted
HD_AT_ERROR_IDNF	equ	10h	; Requested sector ID not found
HD_AT_ERROR_UNC		equ	40h	; ECC failed to correct data error
HD_AT_ERROR_BBK		equ	80h	; Bad block mark found in sector ID field

; [TechRef 10-14] (-W) Write Precompensation Register
PORT_HD_AT_WRPRECOMP	equ	1F1h

; [TechRef 10-14] (RW) Sector Count Register
PORT_HD_AT_SECTORC	equ	1F2h

; [TechRef 10-14] (RW) Sector Number Register
PORT_HD_AT_SECTORN	equ	1F3h

; [TechRef 10-14] (RW) Cylinder Low/High Registers
PORT_HD_AT_CYL_LO	equ	1F4h
PORT_HD_AT_CYL_HI	equ	1F5h

; [TechRef 10-15] (RW) Select Drive/Head Register
PORT_HD_AT_SDH		equ	1F6h
HD_AT_SDH_HEAD		equ	00Fh	; Head select bits
HD_AT_SDH_DRV		equ	010h	; Drive select bit
HD_AT_SDH_RESERVED	equ	0E0h	; Documented as "reserved"

; [TechRef 10-15] (R-) Status Register
PORT_HD_AT_STATUS	equ	1F7h
HD_AT_STATUS_ERR	equ	01h	; Previous command ended with error
HD_AT_STATUS_IXD	equ	02h	; Index pulse from disk
HD_AT_STATUS_CORR	equ	04h	; Correctable error has been corrected
HD_AT_STATUS_DRQ	equ	08h	; Drive is ready for data transfer via PORT_HD_AT_DATA
HD_AT_STATUS_DSC	equ	10h	; Drive seek complete
HD_AT_STATUS_DWF	equ	20h	; Drive write fault
HD_AT_STATUS_DRDY	equ	40h	; Drive is up to speed and ready to accept commands
HD_AT_STATUS_BSY	equ	80h	; Drive busy, all other status bits invalid


; [TechRef 10-16] (-W) Command Register
PORT_HD_AT_COMMAND	equ	1F7h

; [TechRef 10-18] (R-) Alternate Status Register
; Same as PORT_HD_AT_STATUS but does not clear pending interrupt when read.
PORT_HD_AT_ALTSTATUS	equ	3F6h

; [TechRef 10-18] (-W) Digital Output Register
PORT_HD_AT_DIGOUT	equ	3F6h
HD_AT_DIGOUT_UNDEFINED	equ	0F9h	; Documented as "not used"
HD_AT_DIGOUT_IEN	equ	002h	; Interrupt enable
HD_AT_DIGOUT_SRST	equ	004h	; System software reset

; [TechRef 10-18] (--) Drive Address Register
; Documented as "not used by the GRiDCase 1500 Series computers".
; No definition given for its contents.
PORT_HD_AT_DRIVEADDR	equ	3F7h
