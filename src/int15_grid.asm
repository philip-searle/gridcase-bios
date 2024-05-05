
INT15_GRID	PROGRAM	OutFile=build/int15_grid.obj

		include	"macros.inc"
		include	"segments/bda.inc"
		include	"cmos.inc"
		include	"grid.inc"
		include	"keyboard.inc"
		include	"parallel.inc"
		include	"video.inc"

		EXTERN	ReadCmos, WriteCmos
		EXTERN	KbWaitIbe_Cli
		EXTERN	GridCpuFast, GridCpuSlow
		EXTERN	VidInit_External, VidInit_Internal
		EXTERN	VidLoadColMapExpRegs
		EXTERN	MachineId, GridSysId
		EXTERN	DriveIdentify, HdAtSpinDown
		EXTERN	PwBackdoor1

; =====================================================================
; Int15_Grid
; Main entrypoint for GRiD-specific int15/E4 services.
; =====================================================================
Int15_Grid	PROC
		push	bp		; allocate 3 words on stack
		push	bp
		push	bp

		; Previous code allocated three words on the stack.
		; This will be be used to setup a series of NEAR returns
		; to the desired int15/E4 handler, followed by the shared
		; cleanup code, followed by the original return address.
		; Original stack on the left, adjusted stack on the right:
		;
		; on_entry	after_adj
		; 		00 ? <- BP <- SP (SP moves down for scratch space)
		; 		02 r_handler
		; 		04 r_cleanup
		; 00 r <- SP	06 r
		mov_	bp, sp

		; Move address of shared cleanup code into r_cleanup
		mov	[bp+4], Grid15Cleanup,DATA=WORD

		; Save registers while we mess with the stack
		push	bx
		push	cx

		; Convert AL handler number to jump table index
		mov_	bl, al
		xor_	bh, bh
		mov	cl, 4		; isolate top nibble of subfunction so
		shr	bx, cl		; we can lookup the correct handler table
		add	bx, .handlers
		mov	bx, [cs:bx]
		mov	[bp+2], bx	; move handler into r_handler

		; Restore registers and kick things off with our constructed return stack
		pop	cx
		pop	bx
		pop	bp
		retn

.handlers	d w	Grid15Rom
		d w	Grid15Display
		d w	Grid15Config
		d	5 * w Grid15Unsup
		ENDPROC	Int15_Grid

; =====================================================================
; Grid15Unsup
; Sets AH to indicate an unsupported function was called.
; =====================================================================
Grid15Unsup	PROC
		mov	ah, GRID_INT15_ERROR
		retn
		ENDPROC	Grid15Unsup

; =====================================================================
; Grid15Cleanup
; Shared cleanup code for GRiD int15/E4 handlers.
; =====================================================================
Grid15Cleanup	PROC
		push	bp		; setup stack frame for BP-based access to saved regs
		mov_	bp, sp
		push	[bp+6]		; load saved flags from original interrupt
		popf
		pop	bp
		cmp	ah, 0		; handler set an error code?
		jz	.leaveFunction
		xor_	al, al		; if so, clear any return value and set CF
		stc
.leaveFunction	retf	2		; drop BP we pushed at start
		ENDPROC	Grid15Cleanup

; =====================================================================
; Grid15Handler
; Shared prolog code for most int15/E4 handlers that validates the
; subfunction index and calls via a lookup table.
; On entry:
;   AL == subfunction index
;   AH == max valid index + 1
;   BX -> subfunction lookup table
;   Stack has old BX value at top (dropped on return)
; =====================================================================
Grid15Handler	PROC
		and	al, 1Fh		; clamp to max valid index
		cmp_	al, ah		; exceeded max valid?
		jb	.callHandler
		pop	bx		; cleanup stack from Int15_Grid
		mov	ah, GRID_INT15_ERROR
		retn

.callHandler	xor_	ah, ah		; convert handler index to lookup table offset:
		shl	ax, 1		; mul by 2
		add_	bx, ax		; add lookup table base
		mov	ax, [cs:bx]	; load handler offset

		; Replace NEAR return with handler offset (equiv to tail
		; call optimisation -- but why?  Needlessly convoluted;
		; perhaps required due to limited stack space during int15?)
		pop	bx
		push	ax
		retn
		ENDPROC	Grid15Handler

; =====================================================================
; Grid15Display
; Handler for int15/E4, subfunctions 20h-3Fh
; =====================================================================
Grid15Display	PROC
		push	bx		; store BX for cleanup code to drop
		mov	bx, .handlers
		mov	ah, (.handlersEnd - .handlers) / 2
		FillerNop		; 2 NOPS ???
		FillerNop
		jmp	Grid15Handler

.handlers	d w	GridDisplay20
		d w	GridDisplay21
		d w	GridDisplay22
		d w	GridDisplay23
		d w	GridDisplay24
.handlersEnd	ENDPROC	Grid15Display

; =====================================================================
; GridDisplay20 [TechRef 3-19]
; Select internal or external display, or return internal panel type.
; =====================================================================
GridDisplay20	PROC
		cmp	dl, 0FFh	; query current display?
		jnz	.changeDisplay

		; Fetch internal panel type
		push	dx
		mov	dx, PORT_VID_ROMSTATUS
		in	al, dx
		pop	dx
		mov	dl, 0
		test	al, VID_TYPE_EXTERNAL
		jz	.returnDisplay
		and	al, VID_TYPE_INTERNAL_MASK
		mov_	dl, al
.returnDisplay	xor_	ax, ax
		retn

.changeDisplay	; Switch between internal and external displays
		push	bx		; store all regs
		push	cx		; vid init may use any of them
		push	dx
		push	si
		push	di
		push	ds
		push	es
		push	bp
		cmp	dl, 0		; want external display?
		jz	.enableExtVideo
		call	VidInit_Internal
		jmp	.displayChanged
		FillerNop
.enableExtVideo	call	VidInit_External
.displayChanged	pop	bp
		pop	es
		pop	ds
		pop	di
		pop	si
		pop	dx
		pop	cx
		pop	bx
		xor_	ax, ax		; return success
		retn
		ENDPROC	GridDisplay20

; =====================================================================
; GridDisplay21 [TechRef 3-20]
; Select or get color map.
; =====================================================================
GridDisplay21	PROC
		push	ds
		mov	ax, BDA_SEGMENT	; setup for BDA access
		mov	ds, ax

		cmp	dl, 0FFh	; BUG: [TechRef 3-20] incorrectly documents DL=0FFh as
					;      "the currently selected color map is activated".
					;      Actually, it returns the current color map index
					;      in DL.
		jnz	.setColorMap
		mov	dl, [VidColorMap]	; return current color map index
		jmp	.leaveFunction

.setColorMap	cmp	dl, 5		; compare to max color map index
		jbe	.validColorMap
		mov	ah, GRID_INT15_OUTOFRANGE
		pop	ds
		retn

.validColorMap	; Store new color map index
		; BUG: [TechRef 3-20] doesn't document that the color map index is
		;      stored but only applied if the internal panel is active.
		mov	[VidColorMap], dl
		push	dx
		mov	dx, PORT_VID_ROMSTATUS
		in	al, dx
		pop	dx
		test	al, VID_TYPE_EXTERNAL
		jz	.leaveFunction
		call	VidLoadColMapExpRegs	; apply new color map

.leaveFunction	pop	ds
		xor_	ax, ax
		retn
		ENDPROC	GridDisplay21

; =====================================================================
; GridDisplay23 [TechRef 3-21]
; Set backlight timeout.
; BUG: TechRef doesn't document that this entire subfunction only does
;      anything when the internal display panel is enabled.
; =====================================================================
GridDisplay23	PROC
		push	dx
		mov_	ah, dl
		mov	dx, PORT_VID_ROMSTATUS
		in	al, dx
		test	al, VID_TYPE_EXTERNAL	; do nothing if external display
		jz	.leaveFunction
		cmp	ah, 0FFh		; turn on backlight?
		jz	.adjustBacklite
		cmp	ah, 60			; timeout in range?
		jbe	.adjustBacklite
		mov	ah, 60			; clamp timeout to max 60mins

.adjustBacklite	mov	al, CMOS_GRIDBACKLITE
		xchg	al, ah			; store new timeout to CMOS
		call	WriteCmos
		xchg	al, ah
		pushf
		push	cx
		xor_	cx, cx
		call	KbWaitIbe_Cli
		jnz	.leaveFunction2
		mov	al, KBC_CMD_BKLITE
		out	PORT_KBC_CMD, al
		xor_	cx, cx
		call	KbWaitIbe_Cli
		jnz	.leaveFunction2
		mov_	al, ah
		out	PORT_KBC_DATA, al

.leaveFunction2	pop	cx
		popf
.leaveFunction	pop	dx
		xor_	ax, ax
		retn
		ENDPROC	GridDisplay23

; =====================================================================
; GridDisplay22 [TechRef 3-21]
; Select current display font.
; =====================================================================
GridDisplay22	PROC
		cmp	dl, 3		; only four valid fonts
		jbe	.setFont
		mov	ah, GRID_INT15_OUTOFRANGE
		retn

.setFont	push	dx
		mov_	al, dl
		push	ax
		and	al, 1		; write lo bit
		mov	dx, PORT_VID_FONT_LO
		out	dx, al
		pop	ax
		shr	al, 1
		and	al, 1		; write hi bit
		inc	dx
		out	dx, al
		pop	dx
		xor_	ax, ax
		retn
		ENDPROC	GridDisplay22

; =====================================================================
; GridDisplay24 [UNDOC]
; Undocumented GRiD BIOS service.
; ??? What does this do
; =====================================================================
GridDisplay24	PROC
		push	dx
		mov	al, 0
		cmp	dl, 0FFh
		jz	.query
		cmp	dl, 0
		jnz	.set
		mov	al, 0Fh		; default if not set

.set		mov	dx, PORT_ROM_SUBSYSTEM1
		out	dx, al
		pop	dx
		xor_	ax, ax
		retn

.query		mov	dx, PORT_ROM_SUBSYSTEM1
		in	al, dx
		and	al, 0Fh
		pop	dx
		mov_	dl, al
		xor_	ax, ax
		retn
		ENDPROC	GridDisplay24

; =====================================================================
; Grid15Config
; Handler for int15/E4, subfunctions 40h-5Fh
; =====================================================================
Grid15Config	PROC
		push	bx
		mov	bx, .handlers
		mov	ah, (.handlersEnd - .handlers) / 2
		FillerNop
		FillerNop
		jmp	Grid15Handler

.handlers	d w	GridConfig40
		d w	GridConfig41
		d w	GridConfig42
		d w	GridConfig43
		d w	GridConfig44
		d w	GridConfig45
		d w	GridConfig46
		d w	GridConfig47
		d w	GridConfig48
		d w	GridConfig49
		d w	GridConfig4A
		d w	GridConfig4B
.handlersEnd	ENDPROC	Grid15Config

; =====================================================================
; GridConfig40 [TechRef 3-22]
; Get EMS RAM size
; =====================================================================
GridConfig40	PROC
		pushf
		mov	al, CMOS_GRIDFLAGS
		call	ReadCmos
		test	al, GF_EMS	; configured for EMS?
		mov	cx, 0		; if not, report no EMS RAM
		jz	.leaveFunction

.readExpMemSize	mov	al, CMOS_EXPMEM_LOBYTE
		call	ReadCmos
		mov_	cl, al
		mov	al, CMOS_EXPMEM_HIBYTE
		call	ReadCmos
		mov_	ch, al

.leaveFunction	xor_	ax, ax
		popf
		retn
		ENDPROC	GridConfig40

; =====================================================================
; GridConfig41 [TechRef 3-23]
; Get XMS RAM size
; =====================================================================
GridConfig41	PROC
		pushf
		mov	al, CMOS_GRIDFLAGS
		call	ReadCmos
		test	al, GF_EMS	; configured for EMS?
		mov	cx, 0		; if so, report no XMS RAM
		jz	GridConfig40.readExpMemSize
		xor_	ax, ax
		popf
		retn
		ENDPROC	GridConfig41

; =====================================================================
; GridConfig42 [TechRef 3-23]
; Gets or sets the extra mmeory type (EMS/XMS)
; =====================================================================
GridConfig42	PROC
		pushf
		cmp	dl, 0FFh	; enquiry about RAM type?
		jz	.enquireRamType

		; change extra RAM type
		mov	al, CMOS_GRIDFLAGS
		call	ReadCmos
		mov_	ah, al
		and	ah, GF_PASSWORD	; preserve password enabled flag
		mov_	al, dl
		and	al, GF_EMS	; set new extra RAM type
		or_	al, ah		; merge in other flags
		mov	ah, CMOS_GRIDFLAGS
		call	WriteCmos
		xor_	ax, ax
		popf
		retn

.enquireRamType	mov	al, CMOS_GRIDFLAGS
		call	ReadCmos
		and	al, GF_EMS
		mov_	dl, al
		xor_	ax, ax
		popf
		retn
		ENDPROC	GridConfig42

; =====================================================================
; GridConfig43 [TechRef 3-24]
; Return external floppy drive number
; =====================================================================
GridConfig43	PROC
		push	dx
		mov	dx, PORT_PRINTER_STATUSC
		in	al, dx
		and	al, PRINTER_STATC_EFLOPPY | PRINTER_STATC_TAPE
		cmp	al, PRINTER_STATC_EFLOPPY | PRINTER_STATC_TAPE
		jz	.haveExtDrive

		; No external drive attached, report it would be drive 2
		pop	dx
		mov	ah, 2
		retn

.haveExtDrive	mov	dx, PORT_PRINTER_STATUSB
		in	al, dx
		and	al, PRINTER_STATB_BORA
		mov_	dl, al		; ext drive index in DL
		jz	.leaveFunction	; if it's A: then just return

		; If external drive is B: then check drive backplane in case it needs incrementing
		mov	dx, PORT_ROM_SUBSYSTEM1
		in	al, dx
		and	al, GRID_BKPL_MASK
		jz	.leaveFunction
		cmp	al, GRID_BKPL_20
		mov	dl, 2		; Backplane 20 has two drives
		jz	.leaveFunction
		dec	dl		; Backplanes 10/20/30/90 have one drive
		cmp	al, GRID_BKPL_90
		jz	.leaveFunction
		cmp	al, GRID_BKPL_40
		jb	.leaveFunction
		dec	dl		; All other backplanes have no drives

.leaveFunction	mov_	al, dl
		pop	dx
		mov_	dl, al
		xor_	ax, ax
		retn
		ENDPROC	GridConfig43

; =====================================================================
; GridConfig44 [TechRef 3-24]
; Returns whether a Pouch tape drive is installed
; =====================================================================
GridConfig44	PROC
		push	dx
		mov	dx, PORT_PRINTER_STATUSC
		in	al, dx
		and	al, PRINTER_STATC_EFLOPPY | PRINTER_STATC_TAPE
		cmp	al, PRINTER_STATC_EFLOPPY
		mov	ax, 0
		jz	.leaveFunction
		mov	ah, GRID_INT15_NOTAPEDRIVE
.leaveFunction	pop	dx
		retn
		ENDPROC	GridConfig44

; =====================================================================
; GridConfig45 [TechRef 3-24]
; Sets CPU clock speed hi/lo
; =====================================================================
GridConfig45	PROC
		cmp	dl, 0
		jnz	.wantFast
		call	GridCpuSlow
		jmp	.leaveFunction
		FillerNop
.wantFast	call	GridCpuFast
.leaveFunction	xor_	ax, ax
		retn
		ENDPROC	GridConfig45

; =====================================================================
; GridConfig46 [TechRef 3-25]
; Battery low check
; =====================================================================
GridConfig46	PROC
		push	dx
		mov	dx, PORT_PRINTER_STATUSB
		in	al, dx
		shr	al, 1		; isolate PRINTER_STATB_MAINPWRLO in lo bit
		and	al, 1
		xor	al, 1		; invert to match documented return
		pop	dx
		mov_	dl, al
		xor_	ax, ax
		retn
		ENDPROC	GridConfig46

; =====================================================================
; GridConfig47 [UNDOC]
; Undocumented BIOS function that... does something?
; Need more information on port 424.
; On entry:
;   DL == 0/1
; =====================================================================
GridConfig47	PROC
		push	dx
		mov_	al, dl
		and	al, 1		; ??? only low bit valid?
		mov	dx, PORT_424
		out	dx, al
		pop	dx
		xor_	ax, ax
		retn
		ENDPROC	GridConfig47

; =====================================================================
; GridConfig48 [UNDOC]
; Undocumented BIOS function that returns identifying details for the
; GRiD computer.
; On return:
;   AX == 0
;   CL == IBM model byte from F000:FFFE (0FCh = IBM AT)
;   CH == GRiD model byte from F000:DFFE [TechRef 3-26]
;   DL == GRiD drive backplane type
; =====================================================================
GridConfig48	PROC
		push	es
		push	di

		; Place drive backplane ID in DL
		mov	dx, PORT_ROM_SUBSYSTEM1
		in	al, dx
		shr	al, 1		; move backplane ID to lo nibble
		shr	al, 1
		shr	al, 1
		shr	al, 1
		and	al, 0Fh
		mov_	dl, al

		; Place model numbers in CX
		mov	ax, BIOS_SEGMENT
		mov	es, ax
		mov	di, MachineId
		mov	cl, [es:di]
		mov	di, GridSysId
		mov	ch, [es:di]

		pop	di
		pop	es
		xor_	ax, ax
		retn
		ENDPROC	GridConfig48

; =====================================================================
; GridConfig49 [UNDOC]
; Undocumented BIOS function that configures or queries the AT hard
; drive spindown powersaving feature.
; Actually implemented in the AT hard drive code; aliased here to keep
; the GridConfigXX naming pattern consistent.
; =====================================================================
GridConfig49	equ	HdAtSpinDown

; =====================================================================
; GridConfig4A [UNDOC]
; Undocumented BIOS function that uses the IDE IDENTIFY command to try
; to identify the size and type of the attached AT-compatible hard disk
; =====================================================================
GridConfig4A	PROC
		call	DriveIdentify
		mov_	cx, ax
		xor_	ax, ax
		clc
		retn
		ENDPROC	GridConfig4A

; =====================================================================
; GridBootRom
; Checks all available application ROMs and returns if none of them
; are bootable.  Loads ROM boot sector and jumps to it otherwise.
; =====================================================================
GridBootRom	PROC
		cli
		mov	ax, 2000h	; use segment 2000h as ROM subsystem scratch space
		mov	ds, ax
		mov	ax, 0E401h	; initialise ROM subsystem
		int	15h
		jnc	.romInitOk

.romInitDone	jmp	.leaveFunction

.romInitOk	cmp	al, 0		; ROM images stil to process?
		jz	.romInitDone

		mov_	cl, al		; CX is our loop counter
		xor_	ch, ch
.checkRomImage	; Get info for current ROM image (ES=ROM image base, DH=ROM system type)
		push	cx
		mov	ax, 0E402h	; get ROM image info
		mov_	dl, cl
		dec	dl		; convert loop index into ROM image ordinal
		int	15h
		pop	cx
		jc	.romInitDone	; bail our on error

		; Validate ROM image is bootable
		push	cx
		cmp	dh, 1		; skip ROM if it's not MS-DOS format
		jnz	.romCheckDone
		mov	ax, es
		add	ax, 1FF0h	; repoint ES to ROM image header
		mov	es, ax
		mov	ax, 0E403h	; enable ROM image
		int	15h
		mov	di, 0
		cmp	[es:di], 0BB66h,DATA=WORD	; check for ROM header signature
		jnz	.romCheckDone
		cmp	[es:di+3], 80h,DATA=BYTE	; bootable flag set?
		jnz	.romCheckDone

		; ROM image passed checks, copy the entire 128K ROM mapping section
		; into 8000:0000 - 9000:FFFF then unmap the ROM image.
		; ??? This is where the ROMs are mapped anyway -- is it just reads that
		; are mapped to the ROM sockets so we can shadow-copy them into RAM?
		push	ds
		push	es
		push	di
		mov	ax, es
		sub	ax, 1FF0h	; point DS to ROM mapping base (source)
		mov	ds, ax
		mov	ax, 8000h	; point ES to high RAM (under ROM mapping?)
		mov	es, ax
		mov	si, 0
		mov	di, 0
		mov	cx, 8000h
		rep movsw		; copy first 64K
		mov	ax, ds
		add	ax, 1000h
		mov	ds, ax
		mov	ax, es
		add	ax, 1000h
		mov	es, ax
		mov	si, 0
		mov	di, 0
		mov	cx, 8000h
		rep movsw		; copy second 64K
		pop	di
		pop	es
		pop	ds

		; Setup a far return to the boot sector code on the stack,
		; them retf to hand control to the ROM's code (implies the
		; code must be position-independent?)
		mov	ax, [es:di+4]	; load boot sector offset
		cmp	ax, 8000h	; ignore values that would wrap past 1MB
		jnb	.jmpBootsector
		add	ax, 8000h
.jmpBootsector	push	ax		; push boot sector segment
		mov	ax, 0
		push	ax		; push boot sector offset
		mov	ax, 0E404h	; unmap ROM image
		int	15h
		mov	dl, 'r'		; set drive index for boot sector code
		sti
		retf

.romCheckDone	; advance to next ROM image
		pop	cx
		loop	.nextRomImage
		push	cx
		pop	cx
.leaveFunction	sti
		retn
.nextRomImage	jmp	.checkRomImage
		ENDPROC	GridBootRom

; =====================================================================
; IsExtFdIndex
; On entry:
;   AL == drive index
; On return:
;   ZF set if drive index represents the external floppy drive
; =====================================================================
IsExtFdIndex	PROC
		push	ax
		push	dx
		mov_	ah, al
		mov	dx, PORT_PRINTER_STATUSC
		in	al, dx
		and	al, PRINTER_STATC_EFLOPPY | PRINTER_STATC_TAPE
		cmp	al, PRINTER_STATC_EFLOPPY	; Ext floppy present?
		jnz	.leaveFunction

		mov	dx, PORT_PRINTER_STATUSB
		in	al, dx
		and	al, PRINTER_STATB_BORA		; 0=A, 1=B
		mov_	dl, al				; Ext drive index in DL
		jz	.knowExtIndex
		mov	dx, PORT_ROM_SUBSYSTEM1
		in	al, dx
		and	al, GRID_BKPL_MASK
		jz	.knowExtIndex		; no internal drives
		cmp	al, GRID_BKPL_20
		mov	dl, 2
		jz	.knowExtIndex		; Backplane 2 has two floppies
		dec	dl
		cmp	al, GRID_BKPL_40	; One floppy for backplanes 1, 3, 9
		jb	.knowExtIndex
		cmp	al, GRID_BKPL_90
		jz	.knowExtIndex
		dec	dl			; No floppies for backplanes 4-8

.knowExtIndex	cmp_	ah, dl
.leaveFunction	pop	dx
		pop	ax
		retn
		ENDPROC	IsExtFdIndex

; =====================================================================
; Grid15Rom
; Handler for int15/E4, subfunctions 00h-1Fh
; =====================================================================
GridRomHandlers	d w	GridRom00
		d w	GridRom01
		d w	GridRom02
		d w	GridRom03
		d w	GridRom04
Grid15Rom	PROC
		cmp	al, Grid15Rom - GridRomHandlers
		jb	.rangeOk
		mov	ah, GRID_INT15_ERROR
		jmp	.leaveFunction

.rangeOk	push	di
		push	si
		mov	di, ax
		and	di, 0FFh
		shl	di, 1
		call	[cs:di+GridRomHandlers]
		pop	si
		pop	di
.leaveFunction	retn
		ENDPROC	Grid15Rom

; =====================================================================
; GridRom00 [TechRef 3-17]
; Get ROM subsystem information.
; On return:
;   AL == max number of ROM images supported?
;   BX == minimum size of data segment required for further int15/E40?
;         calls (in paragraphs).
;   CL == number of hardware ROM sockets?
;
; BUG: [TechRef 3-17] documents AL=hardware rom sockets, BX=data
;      segment size, but that doesn't match the implementation.
; =====================================================================
GridRom00	PROC
		mov	bx, APPROM_MAX_ROM_IMAGES
		mov	al, SIZE#APPROM_DSEG_ENTRY
		mul	bl		; calculate total space needed for ROM image tracking
		add	ax, SIZE#APPROM_DSEG_HEADER	; plus header

		add	ax, 0Fh		; round up to next whole paragraph
		shr	ax, 1
		shr	ax, 1
		shr	ax, 1
		shr	ax, 1

		xchg	ax, bx
		mov	cl, APPROM_MAX_SOCKETS
		retn
		ENDPROC	GridRom00

; =====================================================================
; GridRom01 [TechRef 3-17]
; Initialise ROM subsystem.
; Each physical ROM slot is tested for application ROMs.  When two or
; more ROM ICs make up one logical ROM image, the driver enters this
; data in its ROM table.  The function then returns the number of
; logical ROM images.
;
; On entry:
;   DS == data segment provided by calling application.  Must be at
;         at least as large as the value returned by int15/E400.
;
; On return:
;   AL == number of logical ROM images
; =====================================================================
GridRom01	PROC
XXX_7FA		equ	7FAh
XXX_7F8		equ	7F8h
XXX_7F9		equ	7F9h
XXX_BD0		equ	0BD0h
XXX_BD1		equ	0BD1h
		; Save all registers we'll use
		push	es
		push	bp
		push	dx
		push	cx
		push	bx

		; Save ROM subsystem status
		mov	dx, PORT_ROM_SUBSYSTEM
		in	al, dx
		push	ax

		; Map application ROMs over video address space
		mov	al, 1
		out	dx, al

		; Reset ROM mapping registers???
		mov	al, 0
		mov	dx, XXX_7FA
		out	dx, al
		mov	al, 0
		mov	dx, XXX_7F8
		out	dx, al
		mov	dx, XXX_7F9
		out	dx, al

		; Clear AppROM data segment
		mov	cx, APPROM_MAX_ROM_IMAGES
		mov	al, SIZE#APPROM_DSEG_ENTRY
		mul	cl
		add	ax, SIZE#APPROM_DSEG_HEADER
		mov	cx, ax
		push	ds
		pop	es
		xor_	ax, ax
		mov	di, ax
		cld
		rep stosb

		; Initialize AppROM data segment
		mov	[APPROM_DSEG_HEADER.signature], APPROM_DSEG_SIGNATURE
		mov	[APPROM_DSEG_HEADER.curRomIndex], 0FFh

		; Setup for ROM scan
		; ES -> ROM header (assuming it is mapped correctly)
		; BX == index of current ROM image
		; SI -> ROM image slot in data segment
		mov	ax, APPROM_MAPPING_END - SIZE#APPROM_HEADER
		mov	es, ax
		xor_	bx, bx
		mov	si, SIZE#APPROM_DSEG_HEADER

.checkRomLoop	; Set ROM size registers??? and check whether ROM signature
		; is present where we expect it to be.
		mov	ax, bx
		mov	dx, XXX_BD0
		out	dx, al
		mov	dx, XXX_BD1
		out	dx, al
		cmp	[es:APPROM_HEADER.romHereFlag], APPROM_ROMHEREFLAG
		jz	.romPresent
		jmp	.moveToNextRom

.romPresent	; ??? Exclude some ROM images from the scan based on undocumented word?
		mov	ax, [es:APPROM_HEADER.unknownWord]
		cmp	ax, 0
		jnz	.L1
		mov	ax, 80h		; default if not set in header
.L1		mov	cl, 7
		shr	ax, cl		; get hi-bit into lowest bit
		mov	cx, ax
		mov	ax, bx
		and	ax, 3		; clamp loop counter to lo bits
		cmp	ax, cx		; ??? weird comparison
		jb	.checkRomSize
		jmp	.moveToNextRom	; skip ROM image if test met

.checkRomSize	; Mark ROM image present in data segment
		mov	al, 0FFh
		mov	[si+APPROM_DSEG_ENTRY.romPresentFlag], al

		; Calculate correct port to set to map ROM.
		; Images > 4 are on the second ROM socket???
		mov	cx, bx
		shr	cx, 1
		shr	cx, 1
		add	cx, XXX_7F8

		; Check ROM size and jump to appropriate mapping code
		cmp	[es:APPROM_HEADER.numRomsInPkg], 1
		ja	.romLess128k	; multi-ROM packages can only be in < 128K ROMs
		cmp	[es:romSize], 128
		jb	.romLess128k	; 128K is the largest ROM supported
		jmp	.rom128k

.romLess128k	mov	dx, cx
		mov	al, 0
		out	dx, al		; set ROM mapping

		mov	dx, XXX_7FA
		mov	al, 1
		out	dx, al		; ??? 7FA
		jmp	.set7FA

.rom128k	mov	dx, cx
		mov	al, 1
		out	dx, al		; set ROM mapping (different from above???)

		mov	dx, XXX_7FA
		mov	al, 0
		out	dx, al		; ??? 7FA (different from above)

		; Compare lower 64K of mapped ROM with upper 64K to see if the
		; header area is identical -- likely indicates a 64K ROM that
		; is mis-mapped (maybe because 128K ROM pinouts weren't
		; standardized when BIOS code was written?)
		mov	dx, cx
		push	ds
		push	si
		mov	ax, APPROM_MAPPING_END - 64K - SIZE#APPROM_HEADER
		mov	dx, ax
		xor_	di, di
		mov	si, di
		mov	cx, SIZE#APPROM_HEADER / 2
		cls
		repe cmpsw
		pop	si
		pop	ds
		jnz	.low128Ok
		mov	al, 0		; ROM mirrored in low-64K, pinout wrong so unmap hi-address bit???
		out	dx, al
		jmp	.set7FA

.low128Ok	; ROM header not mirrored, check old header still present
		cmp	[es:APPROM_HEADER.romHereFlag], APPROM_ROMHEREFLAG
		jz	.set7FA
		mov	al, 0		; ROM header not there, mark ROM image not valid
		mov	[si+APPROM_DSEG_ENTRY.romPresentFlag], al
		jmp	.moveToNextRom

.set7FA		cmp	[es:APPROM_HEADER.numRomsInPkg], 1
		jbe	.singleRomPkg
		mov	[si+APPROM_DSEG_ENTRY.unknownFlag80h], 80h
		mov	ax, bx
		ENDPROC	GridRom01

; =====================================================================
; GridConfig4B [UNDOC]
; Undocumented BIOS function that checks for the presence of a secret
; password reset dongle attached to the parallel port.  If present, the
; CMOS flag enabling the boot password is cleared.
;
; On return:
;   AH == 0
;   CL == 3 if CMOS power loss or hardware failure
;      == 2 if password reset dongle not present
;      == 1 if password successfully disabled
; =====================================================================
GridConfig4B	PROC
		push	ax

		; Check RTC good flag set and power fail flag not set
		mov	al, CMOS_STATUS_D
		call	ReadCmos
		test	al, 80h
		jz	.prereqFailed
		mov	al, CMOS_STATUS_DIAG
		call	ReadCmos
		test	al, 80h
		jz	.cmosOk

.prereqFailed	pop	ax
		mov	ah, 0
		mov	cl, GRID_INT15_CMOS_BAD
		retn

.cmosOk		; Attempt to clear the password flag
		call	PwBackdoor1
		jc	.prereqFailed	; Dead code??? PwBackdoor1 always sets CF
		pop	ax
		mov	al, CMOS_GRIDFLAGS
		call	ReadCmos
		mov	cl, GRID_INT15_PW_CLEARED
		test	al, GF_PASSWORD	; Password clear succeeded?
		mov	ah, 0
		jz	.pwClearFailed
		retn

.pwClearFailed	inc	cl		; inc to GRID_INT15_NO_DONGLE
		retn
		ENDPROC	GridConfig4B

ENDPROGRAM	INT15_GRID
