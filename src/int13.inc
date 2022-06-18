
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
; Floppy diskette parameter table
; ---------------------------------------------------------------------------
DISKETTE_PARMS	struc
.specify1	db	0	; FDC specify cmd, first byte
.specify2	db	0	; FDC specify cmd, second bye
.motorWaitTime	db	0	; motor wait time in clock ticks
.sectorSize	db	0	; log2 of bytes per sector (2=512)
.lastSectorId	db	0	; last sector on track
.gapLength	db	0	; in bytes
.dtl		db	0	; disk data length (ignored)
.formatGapLen	db	0
.formatFillByte	db	0
.headSettleTime	db	0	; in milliseconds
.motorStartTime	db	0	; in 0.125 second intervals
		endstruc

; Extension of DISKETTE_PARMS that also stores media type and track count.
; Used by FdSetMediaType to get the correct DPB for a given sector/track
; count and drive type.
DPB_MEDIA	struc
.dpb		ds	DISKETTE_PARMS
.maxTracks	db	0
.mediaType	db	0
		endstruc

; ---------------------------------------------------------------------------
; Commands passed to Int13 in AH.
; [TechRef 9-4, 10-4]
; ---------------------------------------------------------------------------
INT13CMD_RESET		equ	00h	; Reset disk drive system
INT13CMD_LASTSTATUS	equ	01h	; Read disk drive status from last operation
INT13CMD_READ		equ	02h	; Read data from desired sectors into memory
INT13CMD_WRITE		equ	03h	; Write data from memory to desired sectors
INT13CMD_VERIFY		equ	04h	; Verify that disk sectors are valid
INT13CMD_FORMAT		equ	05h	; Format desired track on disk
;				06h	  Unused
;				07h	  Unused
INT13CMD_READPARAMS	equ	08h	; Read current disk drive parameters
INT13CMD_INITPAIR	equ	09h	; Initialize drive pair characteristics
INT13CMD_READLONG	equ	0Ah	; Read data plus ECC bytes
INT13CMD_WRITELONG	equ	0Bh	; Write data plus ECC bytes
INT13CMD_SEEK		equ	0Ch	; Seek to cylinder
INT13CMD_RESETHD	equ	0Dh	; Reset hard disk system only
;				0Eh	  Unused
;				0Fh	  Unused
INT13CMD_TESTREADY	equ	10h	; Test drive ready
INT13CMD_RECALIBRATE	equ	11h	; Recalibrate back to track zero
;				12h	  Unused
;				13h	  Unused
INT13CMD_CONTROLLERDIAG	equ	14h	; Controller internal diagnostic
INT13CMD_READDASDTYPE	equ	15h	; Read direct attached storage device (DASD) type
INT13CMD_CHANGELINE	equ	16h	; Read disk change line status
INT13CMD_SETDASDTYPE	equ	17h	; Set DASD type for format
INT13CMD_SETMEDIATYPE	equ	18h	; Set media type for format

; ---------------------------------------------------------------------------
; Status returns from Int13 BIOS calls.
; [TechRef 9-13, 10-9]
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
INT13STAT_BADMEDIA	equ	00Ch	; Unsupported track/invalid media
;				00Dh-00Fh	Omitted from TechRef
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

; Int13 hard-disk command handlers can rely on this stack layout to be in
; place when they are called and can poke data into it to return values to
; our caller.
Int13HdStack	struc
.flags		dw	0
.es		dw	0
.di		dw	0
.si		dw	0
.bp		dw	0
.sp		dw	0
.bx		dw	0
.dx		dw	0
.cx		dw	0
.ax		dw	0
		endstruc

; Return values from int13/15h (read DASD type)
DASD_NO_DRIVE	equ	0
DASD_FLOPPY	equ	1	; floppy drive without change line
DASD_FLOPPY_CL	equ	2	; floppy drive with change line available
DASD_HD		equ	3	; hard disk

; Return values from int13/16h (disk change line status)
FD_CL_ERROR	equ	80h
FD_CL_UNCHANGED	equ	00h
FD_CL_CHANGED	equ	06h

