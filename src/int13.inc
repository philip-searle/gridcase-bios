
; ---------------------------------------------------------------------------
; Status returns from Int13 BIOS calls.
; ---------------------------------------------------------------------------
INT13STAT_OK		equ	00h	; No error
INT13STAT_BADCMD	equ	01h	; Bad command passed to driver
INT13STAT_ADDRMARK	equ	02h	; Address mark not found
INT13STAT_WRPROT	equ	03h	; Write protect error
INT13STAT_NOSECTOR	equ	04h	; Sector not found
INT13STAT_RESETFAIL	equ	05h	; Fixed disk reset failed
INT13STAT_DISKCHANGE	equ	06h	; Diskette changed or removed
INT13STAT_BADPARAMTBL	equ	07h	; Bad fixed disk parameter table
INT13STAT_DMAOVERRUN	equ	08h	; DMA overrun
INT13STAT_DMABOUNDARY	equ	09h	; DMA crossed 64K boundary

