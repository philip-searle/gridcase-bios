
; ---------------------------------------------------------------------------
; XT-compatible hard disk controller ports

; [TechRef 9-41] (RW) Data Register
PORT_HD_XT_DATA		equ	320h

; [TechRef 9-42] (R-) Status Register
PORT_HD_XT_STATUS	equ	321h
HD_XT_STATUS_READY	equ	001h	; 1=Ready for byte transfer
HD_XT_STATUS_DIR	equ	002h	; 0=Computer->HDC, 1=HDC->Computer
HD_XT_STATUS_CONTROL	equ	004h	; 0=Data tranfer, 1=Command/status transfer
HD_XT_STATUS_BUSY	equ	008h	; 0=HDC idle, 1=HDC busy
HD_XT_STATUS_DRQ3	equ	010h	; 0=No DMA request, 1=HDC ready for DMA
HD_XT_STATUS_IRQ14	equ	020h	; 0=No interrupt pending, 1=HDC interrupt pending
HD_XT_STATUS_RESERVED	equ	0C0h
HD_XT_STATUS_TXIDLE	equ	00Fh	; Mask for detecting HDC ready for xfer

; [TechRef 9-41] (-W) Reset HDC
PORT_HD_XT_RESET	equ	321h	; Any value resets HDC

; [TechRef 9-42] (R-) Configuration Register
PORT_HD_XT_CONFIG	equ	322h

; [TechRef 9-42] (-W) Control Select Register
PORT_HD_XT_SELECT	equ	322h

; [TechRef 9-42] (-W) Interrupt and DMA Enable Register
PORT_HD_XT_DMAINT	equ	323h
HD_XT_DMAINT_DRQ3	equ	001h	; Enable DMA request
HD_XT_DMAINT_IRQ14	equ	002h	; Enable interrupt request
HD_XT_DMAINT_RESERVED	equ	0FCh

; HDC Command bytes [TechRef 9-44]
HD_XT_CMD_TEST_READY	equ	000h
HD_XT_CMD_RECALIBRATE	equ	001h
;				002h	; not used
HD_XT_CMD_STATUS	equ	003h
HD_XT_CMD_FORMAT_DRIVE	equ	004h
HD_XT_CMD_VERIFY	equ	005h	; checks cylinder for sector ID, does not read data
HD_XT_CMD_FORMAT_TRACK	equ	006h
HD_XT_CMD_FORMAT_BTRACK	equ	007h	; as 006h but sets "bad-sector flag" and does not write data
HD_XT_CMD_READ_DATA	equ	008h
;				009h	; not used
HD_XT_CMD_WRITE_DATA	equ	00Ah
HD_XT_CMD_SEEK		equ	00Bh
HD_XT_CMD_INIT		equ	00Ch
HD_XT_CMD_LAST_ECC_LEN	equ	00Dh
;				00Eh	; [TechRef 9-45] documents this as "Read Sector Buffer" but BIOS uses 10h instead?
HD_XT_CMD_WRITE_SEC_BUF	equ	00Fh
HD_XT_CMD_READ_SEC_BUF	equ	010h
HD_XT_CMD_DIAG_RAM	equ	0E0h
;				0E1h	; not used
;				0E2h	; not used
HD_XT_CMD_DIAG_DRIVE	equ	0E3h
HD_XT_CMD_DIAG_HDC	equ	0E4h
HD_XT_CMD_READ_LONG	equ	0E5h
HD_XT_CMD_WRITE_LONG	equ	0E6h

; loops to wait after writing to PORT_HD_XT_RESET
HD_XT_RESET_TIMEOUT	equ	2000

; Timer ticks to wait for drive to be ready after running controller diagnostics
HD_XT_DIAG_TIMEOUT	equ	50 * 18.2

; Timer ticks to wait for drive to finish initializing
HD_XT_INIT_TIMEOUT	equ	30
