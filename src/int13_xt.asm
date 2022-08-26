
INT13_XT	PROGRAM	OutFile=build/int13_xt.obj

		include	"macros.inc"
		include	"segments/bda.inc"
		include	"segments/ivt.inc"
		include	"dma.inc"
		include	"grid.inc"
		include	"hdc_xt.inc"
		include	"int13.inc"
		include	"pic.inc"

		EXTERN	HdAtInit

		PUBLIC	HdInit, HdXtEarlyReset

; ===========================================================================
; HdInit
; Detects and initializes the hard disk controller (XT or AT-compatible).
; ===========================================================================
HdInit		PROC
		mov	dx, PORT_ROM_SUBSYSTEM1
		in	al, dx
		and	al, GRID_BKPL_MASK
		cmp	al, GRID_BKPL_30
		jz	HdXtInit
		cmp	al, GRID_BKPL_40
		jz	HdXtInit
		cmp	al, GRID_BKPL_80
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
		mov	cx, HD_XT_RESET_TIMEOUT2
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
		JumpTableEntry	HdXtDoCmd,	HD_XT_CMD_DIAG_RAM
		JumpTableEntry	HdXtDiagDrive,	HD_XT_CMD_DIAG_DRIVE
		JumpTableEntry	HdXtDoCmd,	HD_XT_CMD_DIAG_HDC
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
		jb	.inRange
		mov	[bp+Int13HdXtStack.ah], INT13STAT_BADCMD
		retn

.inRange	; Dispatch via jump table
		cbw
		mov_	si, ax			; multiply by HdXtHandlerSize
		add_	si, ax
		add_	si, ax
		mov	al, [si+cs:HdXtHandlers+2]	; load command byte
		mov	[HdXtRegPacket.command], al
		jmp	[si+cs:HdXtHandlers]
		ENDPROC	HdXtDispatch

; ===========================================================================
; HdXtLastStatus [TechRef 9-35]
; Returns status of last operation performed, then clears it.
; BUG: last operation status is returned to int13 caller in AL, not AH as
;       most BIOSes do (see RBIL).
; ===========================================================================
HdXtLastStatus	PROC
		mov	al, [HdLastOpStatus]
		mov	[bp+Int13HdXtStack.al], al
		xor_	al, al			; INT13STAT_OK
		jmp	HdXtRetStatus
		ENDPROC	HdXtLastStatus

; ===========================================================================
; HdXtDrvParams [TechRef 9-37]
; ===========================================================================
HdXtDrvParams	PROC
		call	HdXtFds

		mov	ax, [es:bx+FIXED_DISK_PARAMS_XT.cylinders],DISP=WORD
		xchg	ah, al		; convert to little-endian format
		dec	ax		; reserve two cylinders for destructive
		dec	ax		; read/write test routines
		mov_	ch, al		; CH == low 8 bits of cylinder count

		and	ah, 3		; high 2 bits of cylinder count will end up in CL
		mov	al, [es:bx+FIXED_DISK_PARAMS_XT.reserved],DISP=WORD
		shl	al, 1		; combine max sector number with cylinder hi-bits
		shl	al, 1
		shr	ax, 1
		shr	ax, 1
		mov_	cl, al

		mov	dh, [es:bx+FIXED_DISK_PARAMS_XT.heads],DISP=WORD
		dec	dh		; adjust for zero-based head index

		mov	dl, [HdCount]

		; Return values in saved registers
		xor_	ax, ax
		mov	bx, [bp+Int13HdXtStack.bx]
		mov	[bp+Int13HdXtStack.ax], ax
		mov	[bp+Int13HdXtStack.bx], bx
		mov	[bp+Int13HdXtStack.cx], cx
		mov	[bp+Int13HdXtStack.dx], dx
		retn
		ENDPROC	HdXtDrvParams

; ===========================================================================
; HdXtReset [TechRef 9-35], [Techref 9-38], [Techref 9-39]
; ===========================================================================
HdXtReset	PROC
VerifySector0	%macro
		; Verify the first sector is valid by calling the int13
		; interrupt handler directly (to bypass anyone who has
		; hooked our IVT entry).)
		mov	ax, (INT13CMD_VERIFY << 8) | 01h
		mov	cx, 1		; verify one sector on cylinder 0
		mov	dl, [bp+Int13HdXtStack.dx]
		xor_	dh, dh		; head 0
		pushf
		push	cs
		call	HdXtInt13
		%endmacro

		VerifySector0
		call	HdXtResetHdc
		VerifySector0
		mov	[HdXtRegPacket.command], HD_XT_CMD_INIT
		call	HdXtInitParams
		retn
		%dropmacro VerifySector0
		ENDPROC	HdXtReset

; ===========================================================================
; HdXtResetHdc
; Pokes the hard drive controller to reset it.
; ===========================================================================
HdXtResetHdc	PROC
		mov	dx, PORT_HD_XT_RESET
		out	dx, al			; any write resets HDC
		mov	cx, HD_XT_RESET_TIMEOUT
		loop	$
		retn
		ENDPROC	HdXtResetHdc

; ===========================================================================
; HdXtInitParams [TechRef 9-37] [TechRef 9-40]
; ===========================================================================
HdXtInitParams	PROC
		call	HdXtSelectDisk
		jc	.leaveFailure
		Delay	3

		call	HdXtWriteCmd		; init the HDC from AL
		jc	.leaveFailure
		call	HdXtWaitReady
		jc	.leaveFailure

		; Tell the HDC what the attached drive looks like.
		; The first part of the FIXED_DISK_PARAMS_XT struc matches
		; the 8-byte parameter block expected by the controller.
		push	es
		push	bx
		call	HdXtFds
		mov	cx, 8
		mov	dx, PORT_HD_XT_DATA
.writeParams	call	HdXtWaitReady
		jnc	.writeParam		; timed out?
		pop	bx
		pop	es
		jmp	.leaveFailure
		FillerNop
.writeParam	mov	al, [es:bx]		; load byte from FDS
		out	dx, al			; write to HDC
		inc	bx
		loop	.writeParams
		pop	bx
		pop	es

		; Wait for the HDC to complete the init and give us a status response
		mov	dx, PORT_HD_XT_STATUS
		xor_	cx, cx			; maximum wait
.waitStatus	in	al, dx
		and	al, HD_XT_STATUS_RXIDLE
		cmp	al, HD_XT_STATUS_RXIDLE
		jz	.statusReady
		loop	.waitStatus

.leaveFailure	call	HdXtResetHdc
		mov	[bp+Int13HdXtStack.ah], INT13STAT_BADPARAMTBL
		retn

.statusReady	; Read command completion status and make sure it's good
		mov	dx, PORT_HD_XT_STATUS
		in	al, dx
		call	HdXtWaitReady		; wait for controller to ack status read
		jc	.leaveFailure
		mov	dx, PORT_HD_XT_DATA
		in	al, dx
		and	al, HD_XT_CC_ERROR
		jnz	.leaveFailure
		xor_	al, al			; INT13STAT_OK
		jmp	HdXtRetStatus

		; ??? dead code that falls through to HdXtRetStatus
		mov	al, INT13STAT_RESETFAIL
		ENDPROC	HdXtInitParams

; ===========================================================================
; HdXtRetStatus
; Stores the INT13STAT_* value contained in AL to the AH register saved in
; the Int13HdXtStack instance.
; ===========================================================================
HdXtRetStatus	PROC
		mov	[bp+Int13HdXtStack.ah], al
		retn
		ENDPROC	HdXtRetStatus

; ===========================================================================
; HdXtReadScBuf [TechRef 9-45]
; Reads the sector buffer from the HDC to ES:BX.
; ===========================================================================
HdXtReadScBuf	PROC
		mov	[HdXtRegPacket.blockCount], 1
		; Disable retries on ECC errors since we want the
		; actual data read, not a corrected version of it
		; BUG??? this is immediately overwritten in HdXtRead
		or	[HdXtRegPacket.controlByte], HD_XT_CB_NOECCRETRY
		; fall-through to HdXtRead
		ENDPROC	HdXtReadScBuf

; ===========================================================================
; HdXtRead [TechRef 9-35]
; ===========================================================================
HdXtRead	PROC
		; DMA mode: single mode, increment addresses, no auto-init,
		;           write to memory, channel 3
		mov	al, 47h
		mov	[HdXtRegPacket.controlByte], HD_XT_CB_STEP_2
		; fall-through to HdXtDoDma
		ENDPROC	HdXtRead

; ===========================================================================
; HdXtDoDma
; Performs a DMA transfer to/from the HDC.
; On entry:
;   AL == DMA mode to use (for PORT_DMA1_MODE)
;   BP -> Int13HdXtStack instance
; ===========================================================================
HdXtDoDma	PROC
		; Setup for DMA transfer
		mov	es, [bp+Int13HdXtStack.es]
		mov	bx, [bp+Int13HdXtStack.bx]
		call	HdXtSetupDma
		jc	.dmaSetupFailed

		; Enable DMA and interrupts on the HDC
		mov	dx, PORT_HD_XT_DMAINT
		mov	al, HD_XT_DMAINT_DRQ3 | HD_XT_DMAINT_IRQ14
		out	dx, al
		Delay	3

		; Select the correct drive
		call	HdXtSelectDisk
		jc	.leaveRecover
		Delay	3

		; Write the command to the HDC
		call	HdXtWriteCmd
		jc	.leaveRecover

		; 'No retry on ECC error' is a one-shot flag, so reset it for future requests
		; Also clear the completion flag, the interrupt will set it later
		and	[HdXtRegPacket.controlByte], ~HD_XT_CB_NOECCRETRY
		mov	[HdTaskComplete], 0

		; Unmask DMA channel 3
		mov	al, 3
		out	PORT_DMA1_MASK, al

		; Unmask HDC interrupt line
		in	al, PORT_PIC2_MASK
		and	al, ~IRQ_HDC
		Delay	3
		out	PORT_PIC2_MASK, al
		Delay	3
		in	al, PORT_PIC1_MASK
		and	al, ~IRQ_CASCADE
		Delay	3
		out	PORT_PIC1_MASK, al

		; Wait for DMA to complete and return result
		call	HdXtWaitDma
		jc	.leaveRecover
		call	HdXtXlatStatus
		retn

; ---------------------------------------------------------------------------
.leaveRecover	; Shared function tail that reintializes the drive then
		; returns with the carry flag set
		mov	ah, [bp+Int13HdXtStack.ah]
		push	ax
		call	HdXtInitParams
		pop	ax
		mov	[bp+Int13HdXtStack.ah], ah
		stc
		retn

.dmaSetupFailed	mov	al, INT13STAT_DMABOUNDARY
		jmp	HdXtRetStatus
		ENDPROC	HdXtDoDma

; ===========================================================================
; HdXtWriteScBuf [TechRef 9-45]
; Writes the sector buffer directly.
; ===========================================================================
HdXtWriteScBuf	PROC
		mov	[HdXtRegPacket.blockCount], 1
		; Disable retries on ECC errors since we want the
		; actual data written, not a corrected version of it
		; BUG??? this is immediately overwritten in HdXtWrite
		or	[HdXtRegPacket.controlByte], HD_XT_CB_NOECCRETRY
		; fall-through to HdXtWrite
		ENDPROC	HdXtWriteScBuf

; ===========================================================================
; HdXtWrite [TechRef 9-36] [TechRef 9-44]
; ===========================================================================
HdXtWrite	PROC
		mov	[HdXtRegPacket.controlByte], HD_XT_CB_STEP_2
		; DMA mode: single mode, increment addresses, no auto-init,
		;           read from memory, channel 3
		mov	al, 04Bh
		jmp	HdXtDoDma
		ENDPROC	HdXtWrite

; ===========================================================================
; HdXtFormatTk [TechRef 9-44]
; Int13 service not fully documented in TechRef.  As HdXtFormat, but formats
; all tracks.
; ===========================================================================
HdXtFormatTk	PROC
		; Clear cylinder-hi bits
		; BUG??? should this be ANDing with 0C00h instead so it
		;        clears all cylinder bits and therefore formats the
		;        entire drive?
		and	[HdXtRegPacket.chSector], 0C0h
		call	HdXtResetHdc
		; fall-through to HdXtFormat
		ENDPROC	HdXtFormatTk

; ===========================================================================
; HdXtFormat [TechRef 9-36] [TechRef 9-44]
; Formats a single track, optionally setting bad sector flags depending on
; the HDC command byte.
; ===========================================================================
HdXtFormat	PROC
		mov	[HdXtRegPacket.controlByte], HD_XT_CB_NORETRY | HD_XT_CB_NOECCRETRY | HD_XT_CB_STEP_2
		; fall-through to HdXtVerify
		; BUG??? should this be jumping instead?  HdXtVerify
		;        immediately overwrites the controlByte...
		ENDPROC	HdXtFormat

; ===========================================================================
; HdXtVerify [TechRef 9-36]
; ===========================================================================
HdXtVerify	PROC
		mov	[HdXtRegPacket.controlByte], HD_XT_CB_NORETRY | HD_XT_CB_NOECCRETRY | HD_XT_CB_STEP_2
		jmp	HdXtDoCmd
		FillerNop
		ENDPROC	HdXtVerify

; ===========================================================================
; HdXtCheckRdy [TechRef 9-38]
; ===========================================================================
HdXtCheckRdy	PROC
		mov	[HdXtRegPacket.controlByte], HD_XT_CB_NORETRY | HD_XT_CB_NOECCRETRY
		jmp	HdXtDoCmd
		FillerNop
		ENDPROC	HdXtCheckRdy

; ===========================================================================
; HdXtDiagDrive
; Not documented in TechRef.  Runs drive diagnostics.
; ===========================================================================
HdXtDiagDrive	PROC
		mov	[HdXtRegPacket.controlByte], HD_XT_CB_NORETRY | HD_XT_CB_NOECCRETRY
		; fall-through to HdXtDoCmd
		ENDPROC	HdXtDiagDrive

; ===========================================================================
; HdXtDoCmd
; Runs the command from HdXtRegPacket, waits for the HDC to send an
; interrupt, then returns the appropriate response in the current
; Int13HdXtStack.
; ===========================================================================
HdXtDoCmd	PROC
		call	HdXtSelectDisk
		jc	.leaveFailure

		; Enable HDC interrupt
		mov	al, HD_XT_DMAINT_IRQ14
		mov	dx, PORT_HD_XT_DMAINT
		out	dx, al
		Delay	3

		; Write command to HDC, setup BDA for interrupt flag, unmask
		; HDC IRQ, and reset the one-shot 'no retry on ECC error' flag
		call	HdXtWriteCmd
		jc	.leaveFailure
		mov	[HdTaskComplete], 0
		and	[HdXtRegPacket.controlByte], ~HD_XT_CB_NOECCRETRY

		in	al, PORT_PIC2_MASK
		and	al, ~IRQ_HDC
		Delay	3
		out	PORT_PIC2_MASK, al
		in	al, PORT_PIC1_MASK
		and	al, ~IRQ_CASCADE
		Delay	3
		out	PORT_PIC1_MASK, al

		call	HdXtWaitDma
		jc	.leaveRecover
		call	HdXtXlatStatus
		retn

.leaveRecover	jmp	HdXtDoDma.leaveRecover

.leaveFailure	; Some commands have specific error codes, others
		; end up in the generic .leaveRecover path
		cmp	[HdXtRegPacket.command], HD_XT_CMD_DIAG_RAM
		mov	al, INT13STAT_BADSECTOR
		jz	.retStatusInAh

		cmp	[HdXtRegPacket.command], HD_XT_CMD_DIAG_HDC
		mov	al, INT13STAT_BADCONTROLLER
		jz	.retStatusInAh

		cmp	[HdXtRegPacket.command], HD_XT_CMD_TEST_READY
		mov	al, INT13STAT_UNKNOWN
		jz	.retStatusInAh

		cmp	[HdXtRegPacket.command], HD_XT_CMD_RECALIBRATE
		mov	al, INT13STAT_BAD_CALIBRATE
		jnz	.leaveRecover

.retStatusInAh	jmp	HdXtRetStatus
		ENDPROC	HdXtDoCmd

; ===========================================================================
; HdXtWriteCmd
; Writes the current command to the HDC followed by it's parameters.
; On failure, sets CF and stores the appropriate int13 status code in the
; current HdXtInt13Stack.ah instance.
; ===========================================================================
HdXtWriteCmd	PROC
		mov	si, HdXtRegPacket.command
		call	HdXtWriteCmd6
		jnc	.leaveFunction
		mov	[bp+Int13HdXtStack.ah], INT13STAT_TIMEOUT
.leaveFunction	retn
		ENDPROC	HdXtWriteCmd

; ===========================================================================
; HdXtWriteCmd6
; Writes six bytes from [SI] to the HDC data port, waiting for the controller
; to be ready before each byte.  On failure, sets CF.
; ===========================================================================
HdXtWriteCmd6	PROC
		call	HdXtWaitReady
		jc	.leaveFailure
		mov	dx, PORT_HD_XT_DATA
		mov	cx, SIZE#HD_XT_REG_PACK

.writeByte	call	HdXtWaitReady
		jc	.leaveFailure
		mov	al, [si]
		out	dx, al
		inc	si
		loop	.writeByte
		clc
		retn

.leaveFailure	stc
		retn
		ENDPROC	HdXtWriteCmd6

; ===========================================================================
; HdXtSetupDma
; Sets up a DMA transfer to/from ES:BX for Int13HdXtStack.al sectors.
; Size of sector data transferred depends on the hard drive type and whether
; a long transfer (with ECC bytes) has been requested.
; On entry:
;  AL == DMA mode register to use for transfer (must use channel 3)
; ===========================================================================
HdXtSetupDma	PROC
		push	cx		; we need CX for rotates/multiplies
		cli			; prevent interrupts while we tweak DMA ports

		; Set DMA mode for transfer
		out	PORT_DMA1_CLEAR_FF, al
		Delay	3
		out	PORT_DMA1_MODE, al

		; Convert segment:offset in ES:BX to a 20-bit linear address in CH:AX
		mov	cl, 4
		mov	ax, es
		rol	ax, cl		; extract high four bits for DMA page register
		mov_	ch, al
		and	al, 0F0h	; mask and add offset to segment
		add_	ax, bx
		jnc	.1		; addition generated carry?
		inc	ch		; increment upper nibble if so
.1		push	ax		; save lower 16-bits of address for .leaveFunction

		; Set address in DMA base and page registers
		out	PORT_DMA_CHAN3_CUR, al
		Delay	3
		mov_	al, ah
		out	PORT_DMA_CHAN3_CUR, al
		mov_	al, ch
		Delay	3
		and	al, 0Fh
		out	PORT_DMA_PAGE3, al

		; Figure out the correct sector size for the drive type and HDC command,
		; then multiply by the sector count to get total DMA transfer size
		push	cx
		push	dx
		mov	cx, HD_XT_SCBUF_LEN_ECC		; assume ECC bytes
		mov	dx, PORT_HD_XT_CONFIG
		in	al, dx
		cmp	al, HD_XT_DRIVE_TYPE_FF		; extra ECC bytes for this drive?
		jz	.knowDriveType
		mov	cx, HD_XT_SCBUF_LEN_ESDIECC
.knowDriveType	cmp	[HdXtRegPacket.command], HD_XT_CMD_READ_LONG
		jz	.knowCommand
		cmp	[HdXtRegPacket.command], HD_XT_CMD_WRITE_LONG
		jz	.knowCommand
		mov	cx, HD_XT_SCBUF_LEN_DEFAULT	; no ECC bytes for normal commands
.knowCommand	xor_	ah, ah
		mov	al, [bp+Int13HdXtStack.al]
		mul	cx				; multiply sector buffer size by count
		pop	dx
		pop	cx

		; DMA terminal count is one less than bytes to be transferred
		dec	ax
		push	ax				; save count for .leaveFunction
		out	PORT_DMA_CHAN3_WC, al		; write low byte of count
		mov_	al, ah
		Delay	3
		out	PORT_DMA_CHAN3_WC, al		; write high byte of count
		Delay	3

		; Enable interrupts and set flags to indicate whether
		; the DMA transfer would cross a 64KB boundary
		sti
		pop	cx				; pop transfer length, minus one
		pop	ax				; pop normalised base address
		add_	ax, cx				; set CF if base+length overflows into the 17th bit
		pop	cx
		retn
		ENDPROC	HdXtSetupDma

; ===========================================================================
; HdXtXlatStatus
; Examines the value of Int13HdXtStack.ah and the HDC status registers, then
; returns the appropriate INT13STAT_* value in AL.
; ===========================================================================
HdXtXlatStatus	PROC
		; If caller already set a status then preserve it
		mov	al, [bp+Int13HdXtStack.ah]
		or_	al, al
		jnz	.examineHdc
		retn

.examineHdc	mov	dx, PORT_HD_XT_STATUS
.waitHdcIdle	; Wait for the HDC to not be busy.
		; ??? is this correct?  why not just check HD_XT_STATUS_BUSY?
		in	al, dx
		and	al, ~HD_XT_STATUS_RESERVED
		cmp	al, 0
		jnz	.waitHdcIdle

		; Request detailed status from HDC, resetting
		; it if there are any problems [TechRef 9-43]
		call	HdXtSelectDisk
		jc	.resetHdc
		mov	[HdXtRegPacket.command], HD_XT_CMD_STATUS
		call	HdXtWriteCmd
		jc	.resetHdc

		; Read four bytes of error status, reusing HdXtRegPacket to
		; store it temporarily
		call	HdXtWaitReady
		jc	.resetHdc
		mov	dx, PORT_HD_XT_DATA
		mov	cx, 4
		xor_	si, si
.readStatusByte	in	al, dx
		mov	[HdXtRegPacket+si], al
		inc	si
		loop	.readStatusByte
		; Read command completion byte
		call	HdXtWaitReady
		jc	.resetHdc
		mov	dx, PORT_HD_XT_DATA
		in	al, dx
		and	al, HD_XT_CC_ERROR
		jz	.gotHdcStatus

.resetHdc	; Something's gone wrong reading detailed status from the HDC.
		; Just reset the controller and hope for the best.
		call	HdXtResetHdc
		call	HdXtInitParams
		mov	[bp+Int13HdXtStack.ah], INT13STAT_SENSEFAILED
		stc
		retn

.gotHdcStatus	; Examine the detailed error status stored in HdXtRegPacket [TechRef 9-43]
		; and convert it to an INT13STAT_* value.
		; Bits 0-5 of the first byte contain the error code.
		mov	al, [HdXtRegPacket.command]
		mov_	bl, al
		and	al, 0Fh		; use AL to index the error code sub-tables
		and	bl, 30h		; use upper bits to select error handler code block
		xor_	bh, bh
		mov	cl, 3
		shr	bx, cl
		jmp	[cs:bx+.errorHandlers]

.upperNibble0	; Error codes 00-07 seem to be for things that don't fit the other three categories.
		; Simple translation to an INT13STAT_* value is all that's needed.
		cmp	al, .errors0xEnd - .errors0x
		FillerNop
		FillerNop
		jnb	.undefinedError
		mov	bx, .errors0x
		xlat	[cs:bx]
.storeError	mov	[bp+Int13HdXtStack.ah], al
		retn

.upperNibble1	; Error codes 10-1B are for data read errors.
		; Codes >= 08 are ECC errors, so for those we check the last
		; uncorrected ECC length and use it to decide the 'real' error code.
		cmp	al, .errors1xEnd - .errors1x
		FillerNop
		FillerNop
		jnb	.undefinedError
		mov	bx, .errors1x
		mov_	ah, al
		xlat	[cs:bx]
		mov	[bp+Int13HdXtStack.ah], al	; store initial error status
		test	ah, 8				; ECC error code?
		jz	.error1xDone			; If not, all done

		; Request last ECC error length.  If that command reports an
		; error then we assume the HDC has failed.
		call	HdXtSelectDisk
		jc	.error1xReset
		mov	[HdXtRegPacket.command], HD_XT_CMD_LAST_ECC_LEN
		call	HdXtWriteCmd
		jc	.error1xReset
		call	HdXtWaitReady
		jc	.error1xReset
		mov	dx, PORT_HD_XT_DATA	; read and discard ECC length
		in	al, dx
		call	HdXtWaitReady
		jc	.error1xReset
		mov	dx, PORT_HD_XT_DATA	; read command completion byte
		in	al, dx
		and	al, HD_XT_CC_ERROR	; command failed?
		jz	.error1xDone		; assume controller failure if so
		mov	[bp+Int13HdXtStack.ah], INT13STAT_BADCONTROLLER

.error1xDone	retn
.error1xReset	call	HdXtResetHdc
		call	HdXtInitParams
		stc
		retn
.undefinedError	mov	[bp+Int13HdXtStack.ah], INT13STAT_UNDEFINED
		stc
		retn

.upperNibble2	; Error code 20-21 are range-check errors.
		; Simple translation to INT13STAT_* codes is enough.
		cmp	al, .errors2xEnd - .errors2x
		FillerNop
		FillerNop
		jnb	.undefinedError
		mov	bx, .errors2x
		xlat	[cs:bx]
		jmp	.storeError

.upperNibble3	; Codes 30-32 are internal HDC errors.
		; Simple translation to INT13STAT_* codes is enough.
		cmp	al, .errors3xEnd - .errors3x
		FillerNop
		FillerNop
		jnb	.undefinedError
		mov	bx, .errors3x
		xlat	[cs:bx]
		jmp	.storeError

		; Handlers for the different classes of HDC error codes
.errorHandlers	d w	.upperNibble0
		d w	.upperNibble1
		d w	.upperNibble2
		d w	.upperNibble3

		; Translation of HDC error codes [TechRef 9-46] to INT13STAT_* statuses
		; Codes 00-07 don't seem quite right...
.errors0x	d b	INT13STAT_OK		; 00 No error
		d b	INT13STAT_BADSEEK	; 01 Omitted from TechRef (IBM BIOS: no index signal)
		d b	INT13STAT_BADCONTROLLER	; 02 Omitted from TechRef (IBM BIOS: no seek complete signal)
		d b	INT13STAT_TIMEOUT	; 03 Write fault (BUG: should this be INT13STAT_WRITEFAULT?)
		d b	INT13STAT_TIMEOUT	; 04 Drive not ready (BUG: should this be INT13STAT_NOTREADY?)
		d b	INT13STAT_BADCONTROLLER	; 05 Omitted from TechRef (IBM BIOS: not used)
		d b	INT13STAT_BADCONTROLLER	; 06 Track 0 not found
		d b	INT13STAT_BADCONTROLLER	; 07 Omitted from TechRef (IBM BIOS: not used)
.errors0xEnd
.errors1x	d b	INT13STAT_BADECC	; 10 Omitted from TechRef (IBM BIOS: ID read ECC error)
		d b	INT13STAT_BADECC	; 11 Uncorrectable data error
		d b	INT13STAT_BADADDRMARK	; 12 Data address mark not found
		d b	INT13STAT_BADADDRMARK	; 13 Omitted from TechRef (IBM BIOS: not used)
		d b	INT13STAT_NOSECTOR	; 14 Omitted from TechRef (IBM BIOS: sector not found)
		d b	INT13STAT_BADSEEK	; 15 Seek error
		d b	INT13STAT_BADADDRMARK	; 16 Omitted from TechRef (IBM BIOS: not used)
		d b	INT13STAT_BADCONTROLLER	; 17 Omitted from TechRef (IBM BIOS: not used)
		d b	INT13STAT_DATACORRECTED	; 18 Correctable data error
		d b	INT13STAT_NOTUSED	; 19 Track flagged as bad
		d b	21h			; 1A Omitted from TechRef
		d b	INT13STAT_BADCONTROLLER	; 1B Omitted from TechRef
.errors1xEnd
.errors2x	d b	INT13STAT_BADCMD	; 20 Invalid command
		d b	INT13STAT_BADADDRMARK	; 21 Illegal sector address
.errors2xEnd
.errors3x	d b	INT13STAT_BADCONTROLLER	; 30 Sector buffer error
		d b	INT13STAT_BADCONTROLLER	; 31 Controller ROM checksum error
		d b	INT13STAT_BADECC	; 32 ECC generator error
.errors3xEnd
		ENDPROC	HdXtXlatStatus

; ===========================================================================
; HdXtWaitReady
; Waits for the drive controller to be ready to perform a byte transfer (in
; any direction).
; ===========================================================================
HdXtWaitReady	PROC
		push	dx
		push	cx

		mov	cx, HD_XT_READY_TIMEOUT
.waitHdcReady2	push	cx
		xor_	cx, cx
		mov	dx, PORT_HD_XT_STATUS
.waitHdcReady	in	al, dx
		test	al, HD_XT_STATUS_READY
		jnz	.leaveSuccess
		loop	.waitHdcReady
		pop	cx
		loop	.waitHdcReady2

		; Timed out, report error
		mov	[bp+Int13HdXtStack.ah], INT13STAT_TIMEOUT
		pop	cx
		pop	dx
		stc
		retn

.leaveSuccess	pop	cx
		pop	cx
		pop	dx
		clc
		retn
		ENDPROC	HdXtWaitReady

; ===========================================================================
; HdXtWaitDma
; Waits for the hard drive controller to complete the current DMA transfer.
; ===========================================================================
HdXtWaitDma	PROC
		; Need interrupts enabled so HDC can signal task completion
		sti

		; Determine correct timeout based on command in progress
		; ??? looks like 0FFxxh or 000xxh, the timeout value from
		;     the FIXED_DISK_PARAMS_XT structure doesn't seem to
		;     have much influence on the total timeout...
		push	es
		push	bx
		call	HdXtFds
		mov	cl, [es:bx+FIXED_DISK_PARAMS_XT.checkTimeout],DISP=WORD
		mov	ch, 0FFh
		cmp	[HdXtRegPacket.command], HD_XT_CMD_DIAG_DRIVE
		jz	.haveTimeout
		mov	cl, [es:bx+FIXED_DISK_PARAMS_XT.formatTimeout],DISP=WORD
		cmp	[HdXtRegPacket.command], HD_XT_CMD_FORMAT_DRIVE
		jz	.haveTimeout
		mov	cl, [es:bx+FIXED_DISK_PARAMS_XT.stdTimeout],DISP=WORD
		xor_	ch, ch
.haveTimeout	mov_	bx, cx
		pop	es		; preserve BX value, it is the timeout
		pop	es

.waitDma2	xor_	cx, cx
.waitDma	cmp	[HdTaskComplete], 0FFh
		jz	.dmaComplete
		loop	.waitDma
		dec	bx
		jnz	.waitDma2

.leaveRecover	; Disable interrupts, acknowledge any IRQs, mask off the
		; HDC DMA channel, then reset the hard disk controller
		cli
		mov	al, NONSPECIFIC_EOI
		out	PORT_PIC2_CTRL, al
		Delay	3
		out	PORT_PIC1_CTRL, al
		Delay	3
		mov	al, 7		; mask DMA channel 3
		out	PORT_DMA1_MASK, al
		sti			; done tweaking the PIC
		call	HdXtResetHdc
		mov	[bp+Int13HdXtStack.ah], INT13STAT_TIMEOUT
		stc
		retn

.dmaComplete	; Disable HDC interrupts and DMA, then wait for the HDC
		; to become idle again.
		mov	dx, PORT_HD_XT_DMAINT
		xor_	al, al
		out	dx, al
		call	HdXtWaitReady
		jc	.leaveRecover

		; Read command completion byte and translate to either
		; INT13STAT_OK or INT13STAT_BADADDRMARK (??? is this correct)
		mov	dx, PORT_HD_XT_DATA
		in	al, dx
		and	al, HD_XT_CC_ERROR
		mov	[bp+Int13HdXtStack.ah], al
		retn
		ENDPROC	HdXtWaitDma

; ===========================================================================
; HdXtOpComplete
; Int76 handler that is called when the HDC signals an interrupt.
; Takes care of setting the HdTaskComplete flag.
; ===========================================================================
HdXtOpComplete	PROC
		push	ds
		push	ax

		mov	ax, BDA_SEGMENT
		mov	ds, ax
		mov	[HdTaskComplete], 0FFh

		mov	al, NONSPECIFIC_EOI
		out	PORT_PIC2_CTRL, al
		Delay	3
		out	PORT_PIC1_CTRL, al
		Delay	3
		mov	al, 7		; mask DMA channel 3
		out	PORT_DMA1_MASK, al

		pop	ax
		pop	ds
		iret
		ENDPROC	HdXtOpComplete

; ===========================================================================
; HdXtBdaToDs
; Places the BDA segment base into DS.
; ===========================================================================
HdXtBdaToDs	PROC
		push	ax
		mov	ax, BDA_SEGMENT
		mov	ds, ax
		pop	ax
		retn
		ENDPROC	HdXtBdaToDs

; ===========================================================================
; HdXtEarlyReset
; Called early in the boot process to initialize any XT-compatible hard drive
; that's installed in the computer.
; ===========================================================================
HdXtEarlyReset	PROC
		push	cx
		push	dx
		push	si
		push	ax
		push	ds

		; Only drive backplanes > 20h and < 50h can have
		; XT-compatible hard drives attached.
		mov	dx, PORT_ROM_SUBSYSTEM1
		in	al, dx
		and	al, GRID_BKPL_MASK
		cmp	al, GRID_BKPL_20
		jbe	.leaveFunction
		cmp	al, GRID_BKPL_50
		jnb	.leaveFunction

		; Give the controller a few chances to reset itself
		mov	cx, 4
.testHdcIdle	mov	dx, PORT_HD_XT_STATUS
		in	al, dx
		and	al, HD_XT_STATUS_TXIDLE
		cmp	al, 0
		jz	.hdcIdle
		out	dx, al		; write to PORT_HD_XT_RESET
		push	cx
		mov	cx, HD_XT_RESET_TIMEOUT
.waitHdcReset	loop	.waitHdcReset	; wait a bit for reset to take effect
		pop	cx
		loop	.testHdcIdle

.hdcIdle	; Select the drive and ask it to check whether it's ready
		call	HdXtSelectDisk
		mov	dx, PORT_HD_XT_DMAINT
		dec	al		; dec from 00h to FFh
		out	dx, al		; enable DMA and interrupts
		push	cs
		pop	ds
		mov	si, .kTestReadyCmd
		call	HdXtWriteCmd6

.leaveFunction	pop	ds
		pop	ax
		pop	si
		pop	dx
		pop	cx
		retn

		; all-zeroes command packet is HD_XT_CMD_TEST_READY
.kTestReadyCmd	ds	HD_XT_REG_PACK
		ENDPROC	HdXtEarlyReset

; ===========================================================================
; HdXtSelectDisk
; Selects the disk and waits for it to be ready for XXX
; ===========================================================================
HdXtSelectDisk	PROC
		push	cx
		push	ax

		; Wait for the HDC to be ready and idle, resetting it once
		; and trying again before declaring failure
		mov	ah, 0		; AH tracks whether we have reset the HDC yet
.waitHdcReady	mov	cx, 4000
.testHdcReady	mov	dx, PORT_HD_XT_STATUS
		in	al, dx
		; BUG??? This should be:
		;        and al, ~HD_XT_STATUS_RESERVED
		;        because reserved bits aren't documented to be one
		cmp	al, HD_XT_STATUS_RESERVED
		jz	.hdcReady
		loop	.testHdcReady
		cmp	ah, 0		; have we reset the HDC already?
		jnz	.hdcResetFailed	; give up if so
		mov	al, 1		; ??? reset doesn't care what AL is...
		out	dx, al		; reset HDC
		mov	ah, 1
		mov	cx, 0
.waitReset	Delay	3
		loop	.waitReset
		jmp	.waitHdcReady

.hdcResetFailed	jmp	.leaveFailure
		FillerNop

.hdcReady	; Select disk drive and wait for it to become selected
		mov	cx, 4000
		mov	dx, PORT_HD_XT_SELECT
		mov	al, 1
		out	dx, al		; select disk
		dec	dx		; dec to PORT_HD_XT_STATUS
.waitSelected	in	al, dx
		and	al, ~HD_XT_STATUS_RESERVED
		cmp	al, HD_XT_STATUS_BUSY | HD_XT_STATUS_CONTROL | HD_XT_STATUS_READY
		jz	.leaveSuccess
		loop	.waitSelected

.leaveFailure	pop	ax
		pop	cx
		mov	[bp+Int13HdXtStack.ah], INT13STAT_TIMEOUT
		stc
		retn

.leaveSuccess	pop	ax
		pop	cx
		clc
		retn
		ENDPROC	HdXtSelectDisk

; ===========================================================================
; HdXtFds
; Examines the sense switches returned by PORT_HD_XT_CONFIG then loads a
; pointer to the appropriate FIXED_DISK_PARAMS_XT structure in ES:BX.
; ===========================================================================
HdXtFds		PROC
		push	dx
		push	ax

		mov	dx, PORT_HD_XT_CONFIG
		in	al, dx
		mov	bx, FDS_XT_DEFAULT
		cmp	al, HD_XT_DRIVE_TYPE_F6
		jz	.leaveFunction
		mov	bx, FDS_XT_F6
		cmp	al, HD_XT_DRIVE_TYPE_FE
		jnz	.leaveFunction
		mov	bx, FDS_XT_FE

.leaveFunction	push	cs
		pop	es

		pop	ax
		pop	dx
		retn
		ENDPROC	HdXtFds

; ===========================================================================
; HdXtDriveIndex
; Returns the BIOS drive index for the current request in AL.
; Since this BIOS only supports on hard drive, this will always be 80h.
; ===========================================================================
HdXtDriveIndex	PROC
		mov	al, 80h
		retn
		ENDPROC	HdXtDriveIndex

; ===========================================================================
; FIXED_DISK_PARAMS_XT instances for the three supported XT-compatible hard
; drives.  From [TechRef 9-33] [TechRef 9-40]:
;
; GRiD				Supported	Sectors
; Option	HDC Model	Disk Drive	/Track	Heads	Cylinders
;    320 (MFM)	JVC 5523-1	10MB JVC	    17	    2	      612
;    321 (RLL)	JVC 5523-2	20MB JVC	    34	    2	      615
;    321 (RLL)	JVC 5523-3	20MB ALPS	    34	    2	      615
;
; Note that this doesn't match the table embedded in the BIOS here -- there's
; an additional 5MB drive supported as well.
; ALso note that cylinder numbers are reduced by two when reported via
; int13/08h because they are reserved for drive diagnostics.
; ===========================================================================
;				        rwc   wp     ctrl  |---timeout---|
;				cyl hd  cyl  cyl ecc byte  std   fmt   chk spt
FDS_XT_F6	FDS_XT_INSTANCE	612, 2, 306,   0, 11, 05h, 30h, 0FFh, 0FFh, 17	; 5MB
FDS_XT_FE	FDS_XT_INSTANCE	612, 4, 612,   0, 11, 05h, 30h, 0FFh, 0FFh, 17	; 20MB
FDS_XT_DEFAULT	FDS_XT_INSTANCE	612, 2, 612, 356, 11, 05h, 30h, 0FFh, 0FFh, 34	; 10MB

ENDPROGRAM	INT13_XT
