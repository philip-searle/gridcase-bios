
GRID_AUTODETECT	PROGRAM	OutFile=build/grid_autodetect.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"cmos.inc"
		include	"grid.inc"
		include	"parallel.inc"

		EXTERN	ReadCmos, WriteCmos

		PUBLIC	GridAutodetect

TODO           %macro
$              equ     %1 - 0CFA3h
%:             db      0
               %endmacro
CmosResetCsum	TODO	0D11Ah
HdDetectNot70	TODO	0D0B0h
$		equ	0

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
;								  | | | | |
;				Fd0		Fd1		  v v v v v
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
		BackplFdType	FD_NONE,	FD_NONE		; X ? ? ? D

ENDPROGRAM	GRID_AUTODETECT
