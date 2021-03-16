
; ---------------------------------------------------------------------------
; Fixed disk parameter table entry
; ---------------------------------------------------------------------------

FIXED_DISK_PARMS	struc
.cylinders	dw	0
.heads		db	0
.unused1	dw	0
.writePreComp	dw	0
.unused2	db	0
.control	db	0
.unused3	db	0, 0, 0
.landingZone	dw	0
.sectorsPerTrk	db	0
.reserved	db	0
		endstruc

FDS_INSTANCE	%macro	; (cylinders, heads, writePreComp, control, landingZone, sectorsPerTrack)
	dw	%1	; cylinders
	db	%2	; heads
	dw	0	; unused1
	dw	%3	; writePreComp,
	db	0	; unused2
	db	%4	; control
	db	0,0,0	; unused3
	dw	%5	; landingZone
	db	%6	; sectorsPerTrack
	db	0	; reserved
%endmacro

; ---------------------------------------------------------------------------
; Status returns from Int13 BIOS calls.
; [TechRef 10-9]
; ---------------------------------------------------------------------------
INT13STAT_OK		equ	000h	; No error detected
INT13STAT_BADCMD	equ	001h	; Bad command passed to disk I/O
INT13STAT_BADADDRMARK	equ	002h	; Address mark not found
INT13STAT_WRPROT	equ	003h	; Write protect error
INT13STAT_NOSECTOR	equ	004h	; Sector not found
INT13STAT_RESETFAIL	equ	005h	; Fixed disk reset failed
INT13STAT_DISKCHANGE	equ	006h	; Diskette changed or removed
INT13STAT_BADPARAMTBL	equ	007h	; Bad fixed disk parameter table
INT13STAT_DMAOVERRUN	equ	008h	; DMA overrun
INT13STAT_DMABOUNDARY	equ	009h	; DMA crossed 64K boundary
INT13STAT_BADSECTOR	equ	00Ah	; Bad sector flag detected
INT13STAT_NOTUSED	equ	00Bh	; "Not used"
;				00Ch-00Fh	Omitted from TechRef
INT13STAT_BADECC	equ	010h	; Uncorrectable data error
INT13STAT_DATACORRECTED	equ	011h	; ECC corrected data error
;				012h-01Fh	Omitted from TechRef
INT13STAT_BADCONTROLLER	equ	020h	; Controller failed
;				021h-03Fh	Omitted from TechRef
INT13STAT_BADSEEK	equ	040h	; Seek operation failed
;				041h-07Fh	Omitted from TechRef
INT13STAT_TIMEOUT	equ	080h	; Attachment failed to respond
;				081h-0A9h	Omitted from TechRef
INT13STAT_NOTREADY	equ	0AAh	; Drive not ready
;				0ABh-0BAh	Omitted from TechRef
INT13STAT_UNDEFINED	equ	0BBh	; Undefined error occurred
;				0BC-0CBh	Omitted from TechRef
INT13STAT_WRITEFAULT	equ	0CCh	; Write fault on selected drive
;				0CDh-0DFh	Omitted from TechRef
INT13STAT_NOERROR	equ	0E0h	; "Status error/Error Register-0"
;				0E1h-0FFh	Omitted from TechRef

