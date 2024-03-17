
INT15_GRID	PROGRAM	OutFile=build/int15_grid.obj

		include	"macros.inc"
		include	"segments/bda.inc"
		include	"cmos.inc"
		include	"grid.inc"
		include	"keyboard.inc"
		include	"parallel.inc"
		include	"video.inc"

		EXTERN	CmosRead, CmosWrite
		EXTERN	GridCpuFast, GridCpuSlow
		EXTERN	VidInit_External, VidInit_Internal
		EXTERN	VidLoadColMapExpRegs
		EXTERN	MachineId, GridModelNumber

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
		mov	bp, sp

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
		mov	bp, sp
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
		cmp	al, ah		; exceeded max valid?
		jb	.callHandler
		pop	bx		; cleanup stack from Int15_Grid
		mov	ah, GRID_INT15_ERROR
		retn

.callHandler	xor_	ah, ah		; convert handler index to lookup table offset:
		shl	ax, 1		; mul by 2
		add	bx, ax		; add lookup table base
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
		mov	ah, .handlersEnd - .handlers
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
		mov	dl, al
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
		mov	dl, VidColorMap	; return current color map index
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
		call	CmosWrite
		xchg	al, ah
		pushf
		push	cx
		xor_	cx, cx
		call	KbWaitIde_Cli
		jnz	.leaveFunction2
		mov	al, KBC_CMD_BKLITE
		out	PORT_KBC_CMD, al
		xor_	cx, cx
		KbWaitIde_Cli
		jnz	.leaveFunction2
		mov	al, ah
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
		mov	al, dl
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
		mov	dl, al
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
		mov	ah, .handlersEnd - .handlers
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
.handlersEnd	d w	GridConfig4B
		ENDPROC	Grid15Config

; =====================================================================
; GridConfig40 [TechRef 3-22]
; Get EMS RAM size
; =====================================================================
GridConfig40	PROC
		pushf
		mov	al, CMOS_GRIDFLAGS
		call	CmosRead
		test	al, GF_EMS	; configured for EMS?
		mov	cx, 0		; if not, report no EMS RAM
		jz	.leaveFunction

.readExpMemSize	mov	al, CMOS_EXPMEM_LOBYTE
		call	CmosRead
		mov_	cl, al
		mov	al, CMOS_EXPMEM_HIBYTE
		call	CmosRead
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
		call	CmosRead
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
		mov_	ah, al
		and	ah, GF_UNKNOWN	; preserve unknown flag
		mov_	al, dl
		and	al, GF_EMS	; set new extra RAM type
		or_	al, ah		; merge in other flags
		mov	ah, CMOS_GRIDFLAGS
		call	CmosWrite
		xor_	ax, ax
		popf
		retn

.enquireRamType	mov	al, CMOS_GRIDFLAGS
		call	CmosRead
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
		xor_	al, 1		; invert to match documented return
		pop	dx
		mov_	dl, al
		xor_	ax, ax
		retn
		ENDPROC	GridConfig46

; =====================================================================
; GridConfig47 [UNDOC]
; Undocumented BIOS function that selects which of the two application
; ROM sockets will be mapped into memory when PORT_APPROM_ENABLE is
; set to one.
; On entry:
;   DL == ROM socket (0/1)
; =====================================================================
GridConfig47	PROC
		push	dx
		mov_	al, dl
		and	al, 1		; 1500 series only has two ROM sockets
		mov	dx, PORT_APPROM_SELECT
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
		mov	di, GridModelNumber
		mov	ch, [es:di]

		pop	di
		pop	es
		xor_	ax, ax
		retn
		ENDPROC	GridConfig48

ENDPROGRAM	INT15_GRID
