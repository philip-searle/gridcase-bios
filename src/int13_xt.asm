
INT13_XT	PROGRAM	OutFile=build/int13_xt.obj

		include	"macros.inc"
		include	"segments/bda.inc"
		include	"segments/ivt.inc"
		include	"dma.inc"
		include	"grid.inc"
		include	"hdc_xt.inc"
		include	"int13.inc"

		EXTERN	HdAtInit
		EXTERN	HdXtOpComplete

		PUBLIC	HdInit

TODO		%macro
$		equ	%1 - 0C822h
%:		db	0
		%endmacro
HdXtResetHdc	TODO	0CAD3h
HdXtReset	TODO	0CAA7h
HdXtLastStatus	TODO	0CA5Fh
HdXtRead	TODO	0CB4Bh
HdXtWrite	TODO	0CBCAh
HdXtVerify	TODO	0CBE1h
HdXtFormat	TODO	0CBDCh
HdXtFormatTk	TODO	0CBD4h
HdXtDrvParams	TODO	0CA6Ah
HdXtInitParams	TODO	0CADDh
HdXtReadScBuf	TODO	0CB41h
HdXtWriteScBuf	TODO	0CBC0h
HdXtCheckRdy	TODO	0CBE9h
HdXtDiagRam	TODO	0CBF6h
HdXtDiagDrive	TODO	0CBF1h
HdXtFds		TODO	0CF4Ch
HdXtBdaToDs	TODO	0CEB3h
HdXtDriveIndex	TODO	0CF68h
$		equ	0

; ===========================================================================
; HdInit
; Detects and initializes the hard disk controller (XT or AT-compatible).
; ===========================================================================
HdInit		PROC
		mov	dx, PORT_ROM_SUBSYSTEM1
		in	al, dx
		and	al, GRID_SLED_MASK
		cmp	al, GRID_SLED_30
		jz	HdXtInit
		cmp	al, GRID_SLED_40
		jz	HdXtInit
		cmp	al, GRID_SLED_80
		jz	HdXtInit
		jmp	HdAtInit
		ENDPROC	HdInit

; ===========================================================================
; HdXtInit
; Initializes the XT-compatible hard disk controller.
; ===========================================================================
HdXtInit	PROC
		mov	dx, PORT_HD_XT_STATUS
		in	al, dx
		and	al, HD_XT_STATUS_TXIDLE
		cmp	al, 0			; HDC ready to talk?
		jz	.hdcReady

		; Reset HDC if it's not ready to talk
		out	dx, al
		mov	cx, HD_XT_RESET_TIMEOUT
.waitHdcReset	loop	.waitHdcReset

.hdcReady	; Install XT hard drive BIOS into IVT
		cli				; Don't allow interrupts while IVT is modified
		mov	si, HdXtInt13
		mov	bx, IvtInt13
		xor_	ax, ax
		mov	es, ax			; ES -> IVT

		mov	cx, [es:bx]		; CX:DX -> old int13 handler
		mov	dx, [es:bx+2]

		mov	[es:bx], si		; Replace int13 handler
		mov	[es:bx+2], cs

		mov	bx, IvtInt40		; Copy old int13 handler to int40
		mov	[es:bx], cx
		mov	[es:bx+2], dx

		mov	bx, IvtInt76		; Install operation complete handler
		mov	si, HdXtOpComplete
		mov	[es:bx], si
		mov	[es:bx+2], cs

		sti				; IVT modification complete

		; Init hard drives now -- we only support one
		push	es
		mov	ax, BDA_SEGMENT
		mov	es, ax
		inc	[es:HdCount]
		pop	es
		call	HdXtInitDrive0

		retn
		ENDPROC	HdXtInit

; ===========================================================================
; HdXtInitDrive0
; Initializes the hard disk controller for the first hard drive.
; ===========================================================================
HdXtInitDrive0	PROC
		; Disable interrupts while we install the drive's parameter table
		cli
		call	HdXtFds
		mov_	si, bx
		mov	bx, IvtHd0Parms
		xor_	ax, ax
		mov	es, ax			; ES -> IVT
		mov	[es:bx], si
		mov	ax, cs
		mov	[es:bx+2], ax
		sti

		; Init BDA fields
		call	HdXtBdaToDs
		xor_	al, al
		mov	[HdLastOpStatus], al
		mov	[HdXtRegPacket+HD_XT_REG_PACK.driveHead], al

		; Verify sectors present and valid
		call	HdXtDriveIndex
		xor_	dh, dh			; head=0
		mov_	dl, al
		mov	ah, INT13CMD_VERIFY
		mov	al, 1			; sector count=1
		mov	cx, 1			; track=0, sector=1
		int	13h

		; Reset hard disk controller
		push	dx
		call	HdXtResetHdc
		pop	dx

		; Test for drive ready
		mov	ah, INT13CMD_TESTREADY
		int	13h

		; Run controller RAM diagnostics
		mov	ah, INT13CMD_RAMDIAG
		int	13h
		jb	.failure

		; Run controller internal diagnostics
		mov	ah, INT13CMD_CONTROLLERDIAG
		int	13h
		jb	.failure

		; Controller check out OK, wait for drive to be ready
		xor_	ax, ax
		mov	[TimerTicks], ax
.waitDriveReady	mov	ax, [TimerTicks]
		cmp	ax, HD_XT_DIAG_TIMEOUT
		jnb	.failure
		mov	ah, INT13CMD_TESTREADY
		int	13h
		jc	.waitDriveReady

		; Drive is ready, initialize base tables and recalibrate
		mov	ah, INT13CMD_INITPAIR
		int	13h
		mov	ah, INT13CMD_RECALIBRATE
		int	13h
		jnb	.initDone

.failure	; Something went wrong, notify the user
		; TODO: why do it one char at a time?
		xor_	si, si
		mov	bx, kHdDiagFailure
		mov	cx, SIZE#kHdDiagFailure
		FillerNop			; ???
.printError	mov	al, [cs:bx+si]
		mov	ah, 0Eh			; Write character in AL to screen
		int	10h
		inc	si
		loop	.printError
		stc

.initDone	; Give the drive a short delay to settle down then
		; we're ready to disable DMA/interrupts and start
		; servicing int13 calls.
		xor_	ax, ax
		mov	[TimerTicks], ax
.1		mov	ax, [TimerTicks]
		cmp	ax, HD_XT_INIT_TIMEOUT
		jb	.1
		mov	[HdLastOpStatus], 0
		call	HdXtNoDmaInt
		retn
		ENDPROC	HdXtInitDrive0

; ===========================================================================
; HdXtNoDmaInt
; Disables DMA and interrupt requests from the hard disk controller.
; ===========================================================================
HdXtNoDmaInt	PROC
		mov	dx, PORT_HD_XT_DMAINT
		xor_	al, al
		out	dx, al			; disable interrupt and DMA
		mov	al, 7			; mask DMA channel 3
		out	PORT_DMA1_MASK, al;
		retn
		ENDPROC	HdXtNoDmaInt

; Message used when XT hard disk controller init fails
kHdDiagFailure	db	'Hard Disk Diagnostic Failure', 0Dh, 0Ah

; ===========================================================================
; HdXtControlByte
; Returns the control byte for the current disk.
; ===========================================================================
HdXtControlByte	PROC
		push	es
		push	bx
		call	HdXtFds
		mov	al, [es:bx+FIXED_DISK_PARAMS_XT.controlByte],DISP=WORD
		pop	bx
		pop	es
		retn
		ENDPROC	HdXtControlByte

; ===========================================================================
; HdXtInt13
; Interrupt handler for int 13h requests.
; ===========================================================================
HdXtInt13	PROC
		; Save all  registers and setup the int13 stack frame
		sti
		push	bp
		push	ax
		push	ds
		push	di
		push	si
		push	es
		push	dx
		push	cx
		push	bx
		push	ax
		mov_	bp, sp		; bp -> Int13HdXtStack
		call	HdXtBdaToDs	; ds -> BDA

		; Dispatch to floppy disk BIOS if necessary
		cmp	dl, 80h
		jnb	.isHardDrive

		; Request is for a floppy drive
		push	ax
		call	HdXtDriveIndex	; ??? why do we need to do this?
		and	al, 7Fh		; ???
		jmp	.forwardInt13
		FillerNop

.isHardDrive	; Hard drive 87h gets remapped to the single supported drive number
		; ??? why is this
		cmp	dl, 87h
		jnz	.checkReset
		push	ax
		call	HdXtDriveIndex
		mov_	dl, al		; remap 87h -> 80h
		pop	ax

.checkReset	; AH == 0 (reset disk system) must be forwarded to the floppy
		; drive BIOS unconditionaly (AH == 0Dh resets HD BIOS only)
		and	dl, 7Fh		; convert drive number to floppy range
		and_	ah, ah		; is it INT13CMD_RESET?
		jnz	.notCmdReset
		push	ax
		int	40h		; Forward to relocated floppy int13 handler
		pop	ax
		cmp	dl, [HdCount]	; request for a valid hard drive?
		jnb	.requestDone

.notCmdReset	; Check whether the request is for a valid hard drive,
		; and reject it if not.
		cmp	dl, [HdCount]
		jb	.validHdIndex
		mov	[bp+Int13HdXtStack.ah], INT13STAT_BADCMD
		jmp	.requestDone

.validHdIndex	; The request is for a valid hard drive, but if it's
		; not for the one drive we support then it must be for
		; a second hard disk BIOS.  Forward the request to that
		; BIOS using the second int13 cascade interrupt.
		push	ax
		call	HdXtDriveIndex
		and	al, 7Fh		; convert drive number to index
		cmp_	al, dl		; request is for our drive?
		jz	.dispatchCmd
		; IBM BIOS always handles READPARAMS in HD BIOS, not floppy BIOS,
		; so we do the same ???
		cmp	ah, INT13CMD_READPARAMS
		jnz	.forwardInt13
		; Requests for hard drive > 0 must be forwarded, we only support one drive
		cmp	al, 0
		jz	.dispatchCmd

.forwardInt13	; Requests for drive 0 must be forwarded to the relocated floppy
		; int 13 handler.  Requests for any other drive must be forwarded
		; to int42h on the assumption that there's a second hard drive
		; controller installed which has been relocated there.
		cmp	al, 0
		pop	ax		; clear stack for forwarding interrupt
		pop	ax
		pop	bx
		pop	cx
		pop	dx
		pop	es
		pop	si
		pop	di
		pop	ds
		pop	ax
		pop	bp
		jz	.forwardFloppy
		int	42h		; forward to int13h second cascade interrupt
		retf	2

.forwardFloppy	int	40h		; forward to int13h relocated floppy handler
		retf	2

.dispatchCmd	; Dispatch to the appropriate command handler, then disable DMA and interrupts
		pop	ax
		call	HdXtDispatch
		call	HdXtNoDmaInt

.requestDone	; Update last operation status in BDA and setup flags for return
		pop	ax
		mov	[HdLastOpStatus], ah
		or_	ah, ah
		clc
		jz	.leaveFunction
		cmc

.leaveFunction	pop	bx
		pop	cx
		pop	dx
		pop	es
		pop	si
		pop	di
		pop	ds
		pop	bp
		pop	bp
		retf	2
		ENDPROC	HdXtInt13

; ===========================================================================
; HdXtHandlers
; List of handlers for int13h commands, each one followed by the command byte
; to be sent to the disk controller when handling it.
; ===========================================================================
JumpTableEntry	%macro	HandlerOffset, CommandByte
		dw	%HandlerOffset
		db	%CommandByte
		%endmacro
HdXtHandlerSize	equ	3
HdXtHandlers	JumpTableEntry	HdXtReset,	HD_XT_CMD_INIT
		JumpTableEntry	HdXtLastStatus,	0
		JumpTableEntry	HdXtRead,	HD_XT_CMD_READ_DATA
		JumpTableEntry	HdXtWrite,	HD_XT_CMD_WRITE_DATA
		JumpTableEntry	HdXtVerify,	HD_XT_CMD_VERIFY
		JumpTableEntry	HdXtFormat,	HD_XT_CMD_FORMAT_TRACK
		JumpTableEntry	HdXtFormat,	HD_XT_CMD_FORMAT_BTRACK
		JumpTableEntry	HdXtFormatTk,	HD_XT_CMD_FORMAT_DRIVE
		JumpTableEntry	HdXtDrvParams,	0
		JumpTableEntry	HdXtInitParams,	HD_XT_CMD_INIT
		JumpTableEntry	HdXtRead,	HD_XT_CMD_READ_LONG
		JumpTableEntry	HdXtWrite,	HD_XT_CMD_WRITE_LONG
		JumpTableEntry	HdXtVerify,	HD_XT_CMD_SEEK
		JumpTableEntry	HdXtReset,	HD_XT_CMD_INIT
		JumpTableEntry	HdXtReadScBuf,	HD_XT_CMD_READ_SEC_BUF
		JumpTableEntry	HdXtWriteScBuf,	HD_XT_CMD_WRITE_SEC_BUF
		JumpTableEntry	HdXtCheckRdy,	HD_XT_CMD_TEST_READY
		JumpTableEntry	HdXtVerify,	HD_XT_CMD_RECALIBRATE
		JumpTableEntry	HdXtDiagRam,	HD_XT_CMD_DIAG_RAM
		JumpTableEntry	HdXtDiagDrive,	HD_XT_CMD_DIAG_DRIVE
		JumpTableEntry	HdXtDiagRam,	HD_XT_CMD_DIAG_HDC
HdXtHandlersEnd

; ===========================================================================
; HdXtDispatch
; Builds the HdXtRegPacket contents then sends control to the appropriate
; HdXtHandlers entry based on AL.
; ===========================================================================
HdXtDispatch	PROC
		; Construct HdXtRegPacket for command handler to use later.
		; Mostly a one-to-one copy of the int13 input registers.
		push	bx
		mov	bx, HdXtRegPacket
		mov	[bx+HD_XT_REG_PACK.command], 0
		mov	[bx+HD_XT_REG_PACK.driveHead], dh
		dec	cl
		mov	[bx+HD_XT_REG_PACK.chSector], cl
		mov	[bx+HD_XT_REG_PACK.cylinderLo], ch
		mov	al, [bp+Int13HdXtStack.ax]
		mov	[bx+HD_XT_REG_PACK.blockCount], al
		call	HdXtControlByte
		mov	[bx+HD_XT_REG_PACK.controlByte], al
		pop	bx

		; Range-check subfunction in AL
		mov	al, [bp+Int13HdXtStack.ah]
		mov	[bp+Int13HdXtStack.ah], INT13STAT_OK
		cmp	al, (HdXtHandlersEnd - HdXtHandlers) / HdXtHandlerSize
		ENDPROC	HdXtDispatch

ENDPROGRAM	INT13_XT
