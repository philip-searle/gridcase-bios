
GRID_AUTODETECT	PROGRAM	OutFile=grid_autodetect.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"segments/ivt.inc"
		include	"bios-version.inc"
		include	"cmos.inc"
		include	"grid.inc"
		include	"parallel.inc"

		EXTERN	ReadCmos, WriteCmos
		EXTERN	HdAtOpComplete
		EXTERN	DriveIdentify

		PUBLIC	GridAutodetect, HdDetect70, HdDetectNot70
		PUBLIC	MemSetXmsEms
		PUBLIC	VidInitBacklite

; ===========================================================================
; GridAutodetect
; Detects as much hardware configuration as possible using GRiD-specific
; methods and then updates the CMOS registers to match what is found.
; ===========================================================================
GridAutodetect	PROC
		pushf
		push	ax
		push	bx
		push	cx
		push	dx
		call	FdAutodetect
		call	HdDetectNot70
		call	MemAutodetect
		call	CmosResetCsum
		pop	dx
		pop	cx
		pop	bx
		pop	ax
		popf
		retn
		ENDPROC	GridAutodetect

; ===========================================================================
; MemAutodetect
; Sets CMOS registers to indicate installed memory < 1MB.
; Also clears the diagnostic CMOS register since the CMOS content cannot
; differ from the actual RAM count.
; Leaves NMI disabled.
; ===========================================================================
MemAutodetect	PROC
		; GRiD 1520 always has full base mmeory installed
.baseMemKB	equ	640

		mov	al, (.baseMemKB >> 0) & 0FFh
		mov	ah, CMOS_BASEMEM_LOBYTE | NMI_DISABLE
		call	WriteCmos
		mov	al, (.baseMemKB >> 8) & 0FFh
		mov	ah, CMOS_BASEMEM_HIBYTE | NMI_DISABLE
		call	WriteCmos

		mov	al, 0
		mov	ah, CMOS_STATUS_DIAG | NMI_DISABLE
		call	WriteCmos
		retn
		ENDPROC	MemAutodetect

; ===========================================================================
; FdAutodetect
; Sets the appropriate bits in the CMOS equipment byte to reflect the number
; of floppy disk drives present (internal and external).
; ??? Also forces the initial display mode to 80-column colour.
; ===========================================================================
FdAutodetect	PROC
		; Convert the disk drive backplane ID into a value from 0-3:
		; 0 == no floppy drives (maybe a hard drive)
		; 1 == one floppy drive, no hard drives
		; 2 == two floppy drives, no hard drive
		; 3 == one floppy drive, one hard drive
		; Backplane ID mapping is:
		; 00/40/50/60/70/80 -> 0
		; 10                -> 1
		; 20                -> 2
		; 30/90             -> 3
		mov	dx, PORT_ROM_SUBSYSTEM1
		in	al, dx			; read backplane ID
		mov_	ah, al
		mov	cl, 4
		shr	ah, cl			; shift hi-nibble to lo-nibble
		cmp	ah, GRID_BKPL_90 >> 4
		jnz	.1			; 90 goes straight to 3
		mov	ah, 3
.1		cmp	ah, GRID_BKPL_70 >> 4
		jz	.2			; 70 goes to 0 via 5
		cmp	ah, GRID_BKPL_80 >> 4
		jnz	.3			; 80 also goes to 0 but not via 5?
						; End result is the same, no need to special-case this one?
.2		mov	ah, 5
		; AH is now 0/1/2/3/4/5
.3		test	ah, 0Ch			; anything over 3 goes to 0
		jz	.internalDone		; why not compare to 3 though?
		xor_	ah, ah
.internalDone	; AH is now 0/1/2/3, as required

		; Adjust AH up by 32 if no external drive is present
		mov	dx, PORT_PRINTER_STATUSC
		in	al, dx
		push	ax
		and	al, PRINTER_STATC_EFLOPPY | PRINTER_STATC_TAPE
		cmp	al, PRINTER_STATC_EFLOPPY | PRINTER_STATC_TAPE
		pop	ax
		jz	.extDrvPresent
		add	ah, 20h
		jmp	.extDrvDone
		FillerNop

.extDrvPresent	; Adjust AH up by 16 if external drive should use the higher drive number (B instead of A)
		mov	dx, PORT_PRINTER_STATUSB
		in	al, dx
		test	al, PRINTER_STATB_BORA
		jz	.4
		add	ah, 10h

.4		; Adjust AH up by 8 if external drive is 5.25" instead of 3.5"
		test	al, PRINTER_STATB_3OR5
		jz	.5
		add	ah, 08h

.5		; Adjust AH up by 4 if external drive is low-density
		mov	dx, PORT_PRINTER_STATUSC
		in	al, dx
		test	al, PRINTER_STATC_HDDRIVE
		jnz	.extDrvDone
		add	ah, 04h

.extDrvDone	; AH is now an index into BackplFdTypes so load
		; the floppy drive type values from the table
		mov_	bl, ah
		xor_	bh, bh
		add	bx, BackplFdTypes
		mov	al, [cs:bx]

		; Store floppy drive types to CMOS config
		mov	ah, CMOS_FD_TYPE | NMI_DISABLE
		call	WriteCmos

		; Convert floppy drive type values into CMOS_EQUIPMENT bits (in AH)
		mov	ah, 00h				; assume no floppy drives to start
		cmp	al, (FD_NONE << 8) | FD_NONE
		jz	.equipByteReady			; skip remaining work if no floppy drives

		mov	ah, 01h				; any floppy drive implies IPL disk drive available
		test	al, 0Fh				; any second flopy drive available?
		jz	.equipByteReady			; skip remaining work if not
		mov	ah, 41h				; must increment FD count for second floppy drive

		; If the drive backplane supports two floppy drives then our FD count maxes out the CMOS_EQUIPMENT bits
		mov	dx, PORT_ROM_SUBSYSTEM1
		in	al, dx
		and	al, GRID_BKPL_MASK
		cmp	al, GRID_BKPL_20		; backplane supports two floppy drives?
		jnz	.equipByteReady			; if not, skip external drive checks

		; If the drive backplane supports two floppy drives then we must inspect the external disk drive
		; to see if it exists and include it in the floppy drive count if so (??? why not include the
		; drive count in the BackplFdTypes table -- would be easier since it's already indexed by
		; backplane, external drive presence, and type...)
		mov	dx, PORT_PRINTER_STATUSC
		in	al, dx
		push	ax
		and	al, PRINTER_STATC_EFLOPPY | PRINTER_STATC_TAPE
		cmp	al, PRINTER_STATC_EFLOPPY | PRINTER_STATC_TAPE
		pop	ax
		jnz	.equipByteReady
		mov	ah, 81h				; must increment FD count for third (external) floppy drive

.equipByteReady	; Merge the floppy drive bits into the CMOS equipment byte
		mov_	dl, ah
		push	dx
		mov	al, CMOS_EQUIPMENT | NMI_DISABLE
		call	ReadCmos
		pop	dx
		and	al, 0Eh
		or_	al, dl
		or	al, 20h				; ??? force 80-column colour initial display mode
		mov	ah, CMOS_EQUIPMENT | NMI_DISABLE
		call	WriteCmos
		retn
		ENDPROC	FdAutodetect

BackplFdType	%macro
		db	(%1 << 4) | %2
		%endmacro

; Table of values for the CMOS_FD_TYPE byte, indexed by:
;  1 * Backplane FD count (0 = none, 1 = one, 2 = two, 3 = 1 with HD?) ---+
;  4 * PRINTER_STATC_HDDRIVE -------------------------------------------+ |
;  8 * PRINTER_STATB_3OR5 --------------------------------------------+ | |
; 16 * PRINTER_STATB_BORA ------------------------------------------+ | | |
; 32 * PRINTER_STATC_EFLOPPY -------------------------------------+ | | | |
;                                                                 | | | | |
;                               Fd0             Fd1               v v v v v
BackplFdTypes	BackplFdType	FD_5_360K,	FD_NONE		; . . . . 0
		BackplFdType	FD_5_360K,	FD_3_1M		; . . . . 1
		BackplFdType	FD_5_360K,	FD_3_1M		; . . . . 2
		BackplFdType	FD_5_360K,	FD_3_1M		; . . . . 3
		BackplFdType	FD_5_1M,	FD_NONE		; . . . X 0
		BackplFdType	FD_5_1M,	FD_3_1M		; . . . X 1
		BackplFdType	FD_5_1M,	FD_3_1M		; . . . X 2
		BackplFdType	FD_5_1M,	FD_3_1M		; . . . X 3
		BackplFdType	FD_3_720K,	FD_NONE		; . . X . 0
		BackplFdType	FD_3_720K,	FD_3_1M		; . . X . 1
		BackplFdType	FD_3_720K,	FD_3_1M		; . . X . 2
		BackplFdType	FD_3_720K,	FD_3_1M		; . . X . 3
		BackplFdType	FD_3_1M,	FD_NONE		; . . X X 0
		BackplFdType	FD_3_1M,	FD_3_1M		; . . X X 1
		BackplFdType	FD_3_1M,	FD_3_1M		; . . X X 2
		BackplFdType	FD_3_1M,	FD_3_1M		; . . X X 3
		BackplFdType	FD_5_360K,	FD_NONE		; . X . . 0
		BackplFdType	FD_3_1M,	FD_5_360K	; . X . . 1
		BackplFdType	FD_3_1M,	FD_3_1M		; . X . . 2
		BackplFdType	FD_3_1M,	FD_5_360K	; . X . . 3
		BackplFdType	FD_5_1M,	FD_NONE		; . X . X 0
		BackplFdType	FD_3_1M,	FD_5_1M		; . X . X 1
		BackplFdType	FD_3_1M,	FD_3_1M		; . X . X 2
		BackplFdType	FD_3_1M,	FD_5_1M		; . X . X 3
		BackplFdType	FD_3_720K,	FD_NONE		; . X X . 0
		BackplFdType	FD_3_1M,	FD_3_720K	; . X X . 1
		BackplFdType	FD_3_1M,	FD_3_1M		; . X X . 2
		BackplFdType	FD_3_1M,	FD_3_720K	; . X X . 3
		BackplFdType	FD_3_1M,	FD_NONE		; . X X X 0
		BackplFdType	FD_3_1M,	FD_3_1M		; . X X X 1
		BackplFdType	FD_3_1M,	FD_3_1M		; . X X X 2
		BackplFdType	FD_3_1M,	FD_3_1M		; . X X X 3
		BackplFdType	FD_NONE,	FD_NONE		; X ? ? ? 0	PRINTER_STAT* values don't matter if PRINTER_STATC_EFLOPPY
		BackplFdType	FD_3_1M,	FD_NONE		; X ? ? ? 1	is 1 so there's only one copy of these values for each
		BackplFdType	FD_3_1M,	FD_3_1M		; X ? ? ? 2	backplane type.  Not sure why there are values past backplane
		BackplFdType	FD_3_1M,	FD_NONE		; X ? ? ? 3	type 3 though, I think they're not ever accessed???
		BackplFdType	FD_NONE,	FD_NONE		; X ? ? ? 4
		BackplFdType	FD_NONE,	FD_NONE		; X ? ? ? 5
		BackplFdType	FD_NONE,	FD_NONE		; X ? ? ? 6
		BackplFdType	FD_NONE,	FD_NONE		; X ? ? ? 7
		BackplFdType	FD_NONE,	FD_NONE		; X ? ? ? 8
		BackplFdType	FD_3_1M,	FD_NONE		; X ? ? ? 9
		BackplFdType	FD_NONE,	FD_NONE		; X ? ? ? A
		BackplFdType	FD_NONE,	FD_NONE		; X ? ? ? B
		BackplFdType	FD_NONE,	FD_NONE		; X ? ? ? C
		%IF		BIOS_VERSION = 19880912
				; 1988 BIOS has a floppy drive here?
		BackplFdType	FD_3_1M,	FD_NONE		; X ? ? ? D
		%ELSE
				; Later BIOSes have no drives here
		BackplFdType	FD_NONE,	FD_NONE		; X ? ? ? D
		%ENDIF

; ===========================================================================
; HdDetect70
; Attempts to auto-detect the type of the attached IDE drive if the drive
; backplane of type 70, updating the CMOS drive type bytes appropriately.
; Does nothing otherwise.
; ===========================================================================
HdDetect70	PROC
		push	ax
		push	dx
		mov	dx, PORT_ROM_SUBSYSTEM1
		in	al, dx
		and	al, GRID_BKPL_MASK
		cmp	al, GRID_BKPL_70
		pop	dx
		pop	ax
		jnz	.leaveFunction
		mov	bx, IVT_SEGMENT
		jmp	HdDetect

.leaveFunction	retn
		ENDPROC	HdDetect70

; ===========================================================================
; HdDetectNot70
; Attempts to auto-detect the type of the attached hard drive if the drive
; backplane is not type 70, updating the CMOS drive type bytes appropriately.
; Does nothing otherwise.
; ===========================================================================
HdDetectNot70	PROC
		mov	bx, IVT_SEGMENT
		mov	dx, PORT_ROM_SUBSYSTEM1
		in	al, dx
		and	al, GRID_BKPL_MASK
		cmp	al, GRID_BKPL_70
		jz	HdDetect.leaveFunction
		; Fall-through to HdDetect
		ENDPROC	HdDetectNot70

; ===========================================================================
; HdDetect
; Attempts to auto-detect the type of the attached drive, updating the CMOS
; drive type bytes appropriately.
;
; On entry:
;   BX == segment of IVT used to save/restore the IvtInt76 vector which is
;         modified during the auto-detect process
; ===========================================================================
HdDetect	PROC
		push	ds
		mov	ds, bx
		push	[IvtInt76]
		push	[IvtInt76+2]
		mov	[IvtInt76], HdAtOpComplete,DATA=WORD
		mov	[IvtInt76+2], cs
		call	DriveIdentify
		pop	[IvtInt76+2]
		pop	[IvtInt76]
		pop	ds
		; Couldn't auto-detect the drive type?  Just store anything...
		jb	.driveTypeInBx

		; Grid type 5 maps to BIOS type 02h
		cmp	ax, GRID_HD_5
		jz	.biosHdType02

		; Grid types 4, 6, 9 map to BIOS type 11h
		cmp	ax, GRID_HD_4
		jz	.biosHdType11
		%IF	BIOS_VERSION > 19880912
			; 1988 BIOS doesn't support drive backplane type nine
		cmp	ax, GRID_HD_9
		jz	.biosHdType11
		%ENDIF
		cmp	ax, GRID_HD_6
		jz	.biosHdType11
		%IF	BIOS_VERSION > 19880912
			; 1988 BIOS doesn't support drive backplane type nine
		cmp	ax, GRID_HD_9	; ??? second check for type 9
		jz	.biosHdType11
		%ENDIF

		; Grid type 7 maps to BIOS type E0h
		cmp	ax, GRID_HD_7
		jnz	.driveTypeInBx	; All other types are unknown

;.biosHdTypeE0
		mov	bx, 0F0E0h
		jmp	.driveTypeInBx

.biosHdType02	mov	bx, 2000h
		jmp	.driveTypeInBx

.biosHdType11	mov	bx, 0F011h

.driveTypeInBx	mov	ah, CMOS_HD_TYPE | NMI_DISABLE
		mov_	al, bh
		call	WriteCmos
		mov	ah, CMOS_HD_EXTTYPE1 | NMI_DISABLE
		mov_	al, bl
		call	WriteCmos

.leaveFunction	retn
		ENDPROC	HdDetect

; Some versions of the GRiD BIOS have DriveIdentify code here.
; In later versions this became too long to fit and it was relocated
; into slack space in the at_compat module.  Split the code into two
; segments so we can conditionally link it in the correct place.
[CODE2]	SEGMENT WIDTH=16, ALIGN=1, CLASS=CODE, PURPOSE=DATA|CODE, COMBINE=PUBLIC

; ===========================================================================
; CmosResetCsum
; Checksums the contents of CMOS RAM from 10h to 2Dh and stores them in the
; standard checksum locations 2Eh and 2Fh.
; ===========================================================================
CmosResetCsum	PROC
		mov	bx, 0		; accumulate checksum in BX
		mov	cx, (2Eh - 10h)
.csumLoop	mov_	al, cl
		add	al, 0Fh
		call	ReadCmos
		xor_	ah, ah
		add_	bx, ax
		loop	.csumLoop

		mov_	al, bh
		mov	ah, CMOS_CHECKSUM_LO | NMI_DISABLE
		call	WriteCmos
		mov_	al, bl
		mov	ah, CMOS_CHECKSUM_HI | NMI_DISABLE
		call	WriteCmos
		retn
		ENDPROC	CmosResetCsum

; ===========================================================================
; MemSetXmsEms
; Adjusts the CMOS extended memory size bytes if the system is configured to
; to use memory above 1MB as EMS instead of XMS.
; ===========================================================================
MemSetXmsEms	PROC
		pushf
		push	ax
		mov	al, CMOS_GRIDFLAGS | NMI_DISABLE
		call	ReadCmos
		test	al, GF_EMS
		jz	.done

		; Switch from extended to expanded memory mode
		push	dx
		mov	dx, PORT_XMS_ENABLE
		mov	al, 0
		out	dx, al
		pop	dx

		; Zero out "base expansion memory above 1MB" CMOS bytes.
		; Leave the "base expansion memory" bytes alone because
		; they apparently include the EMS memory.
		mov	ah, CMOS_EXPMEM2_LOBYTE | NMI_DISABLE
		mov	al, 0
		call	WriteCmos
		mov	ah, CMOS_EXPMEM2_HIBYTE | NMI_DISABLE
		mov	al, 0
		call	WriteCmos

.done		pop	ax
		popf
		retn
		ENDPROC	MemSetXmsEms

; ===========================================================================
; VidInitBacklite
; Sets the keyboard controller to control the screen backlight based on the
; CMOS backlight timeout byte.
; ===========================================================================
VidInitBacklite	PROC
		push	ax
		push	dx
		mov	al, CMOS_GRIDBACKLITE | NMI_ENABLE
		call	ReadCmos
		cmp	al, 0FFh
		jz	.setBacklite
		cmp	al, 60
		jbe	.setBacklite
		mov	al, 2		; reset invalid values to 2mins

.setBacklite	mov_	dl, al
		mov	ax, 0E423h	; GRiD: set backlight timeout
		int	15h
		pop	dx
		pop	ax
		retn
		ENDPROC	VidInitBacklite

ENDPROGRAM	GRID_AUTODETECT
