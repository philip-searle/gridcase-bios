
FD_CONTROLLER	PROGRAM	OutFile=build/fd_controller.obj

		include	"macros.inc"
		include	"segments/bda.inc"
		include	"segments/ivt.inc"
		include	"cmos.inc"
		include	"dma.inc"
		include	"fdc.inc"
		include	"int13.inc"
		include	"isr.inc"
		include	"pic.inc"

		EXTERN	FdDoSeek, FdCalibrate
		EXTERN	FdGetMediaState
		EXTERN	FdExternalType
		EXTERN	LoadBdaToDs
		EXTERN	ReadCmos
		EXTERN	TenthMilliDelay

		PUBLIC	FdCheckTk0
		PUBLIC	FdConvResult
		PUBLIC	FdDiskChange
		PUBLIC	FdGetDriveType
		PUBLIC	FdGetDriveType2
		PUBLIC	FdHeadDelay
		PUBLIC	FdLoadDpt
		PUBLIC	FdReadResult
		PUBLIC	FdResetSelect
		PUBLIC	FdSenseInt
		PUBLIC	FdSetupDma
		PUBLIC	FdUpdateMotor
		PUBLIC	FdWaitInt
		PUBLIC	FdWriteByte
		PUBLIC	FdWriteByte2
		PUBLIC	FdWriteDptBytes
		PUBLIC	FdWriteDptBytes2
		PUBLIC	FdWriteHdUs
		PUBLIC	IntE_Actual

; ---------------------------------------------------------------------
; Issues a "Sense Interrupt Status" command to the FDC and returns with
; AH containing FDC status register ST0.
FdSenseInt	PROC
		mov	al, FDC_CMD_SIS
		call	FdWriteByte

		; BUG: This code attempts to reuse the retn statement in
		;      FdReadResult, but at some point that function was
		;      altered to preserve the ST0 value in the BDA by
		;      pushing/popping it around the call to FdReadResult.
		;      This function wasn't updated to match so if the
		;      above call to FdWriteByte fails we'll end up with
		;      an unbalanced stack when we pop a value this function
		;      never pushed.  Bad Things can be expected after that.
		jc	FdReadResultAh.leaveFunction

		; Read two byte result from SIS command
		mov	cx, 2
		; fall-through int FdReadResultAh
		ENDPROC	FdSenseInt

; ---------------------------------------------------------------------
; Reads CX result bytes from the FDC data port into thew FdScratch
; result buffer.  Also returns the first byte of the result in AH.
FdReadResultAh	PROC
		mov	si, FdScratch.fdcSt0
		push	[si]		; attempt to save prev ST0?
		call	FdReadResult
		jc	.leaveFunction
		mov	ax, [si]	; load ST0
		xchg	ah, al		; swap result bytes
.leaveFunction	pop	[si]		; attempt to restore prev ST0?
		retn
		ENDPROC	FdReadResultAh

; ---------------------------------------------------------------------
; Sets ZF according to track zero flag in FDC
FdCheckTk0	PROC
		push	ax
		mov	al, FDC_CMD_SDS	; "sense drive status"
		call	FdWriteByte
		jc	.leaveFunction
		call	FdWriteHdUs	; cmd byte 2: head/unit

		mov	cx, 1		; read ST3 result
		call	FdReadResultAh
		test	ah, FDC_ST3_TK0	; set flags for return
.leaveFunction	pop	ax
		retn
		ENDPROC	FdCheckTk0

; ---------------------------------------------------------------------
; Reads a standard 7-byte response from the FDC and examines ST3 to
; decide whether an error has occurred.  Sets FdScratch.returnAh
; accordingly.
FdConvResult	PROC
		call	FdReadResult7
		jc	.leaveFunction

		; Check ST0 interrupt code for obvious problems
		and	ah, FDC_ST0_INTERRUPT_CODE
		jz	.leaveFunction		; normal termination
		cmp	ah, FDC_IC_INCOMPLETE
		jnz	.unknownError

		; Check ST1 for more detailed error flags
		mov	ah, [FdScratch.fdcSt1]
		and	ah, ~FDC_ST1_UNUSED	; mask unused bits
		mov	bx, 8			; check all 8 bits
.checkErrors	dec	bx
		shr	ah, 1			; shift next bit into CF
		jc	.translateError		; bit set? use it.
		jnz	.checkErrors		; else check next bit

.unknownError	mov	al, INT13STAT_BADCONTROLLER

.leaveFailure	or	[FdScratch.returnAh], al
		stc

.leaveFunction	retn

.errorCodes	; Mapping of bits in ST1 to int13 error codes.
		; Due to the way the bits are checked above, the table
		; is indexed 'backwards', e.g. bit 0 is at table index 7.
		db	INT13STAT_NOSECTOR	; ST1 end of cylinder
		db	INT13STAT_BADCONTROLLER	; ST1 unused
		db	INT13STAT_BADECC	; ST1 data error
		db	INT13STAT_DMAOVERRUN	; ST1 overrun
		db	INT13STAT_BADCONTROLLER	; ST1 unused
		db	INT13STAT_NOSECTOR	; ST1 no data
		db	INT13STAT_WRPROT	; ST1 not writable
		db	INT13STAT_BADADDRMARK	; ST1 missing address mark

.translateError	mov	al, [cs:bx+.errorCodes]
		jmp	.leaveFailure
		ENDPROC	FdConvResult

; ---------------------------------------------------------------------
; Checks and resets the FDC disk change line, setting
; FdScratch.returnAh and CF appropriately.
FdDiskChange	PROC
		mov	dx, PORT_FDC_DIGIN
		in	al, dx
		or_	al, al			; evaluate flags
		jns	.leaveFunction		; no disk change?

		; Disk change line set, seek one track and then back
		; to track 0 to force the FDC to refresh its idea of
		; what media is present.
		mov	bl, [bp+IsrStackAx.dl]
		xor_	bh, bh

		; Short delay before seeking (why?)
		mov	ch, 9
		loop	$

		mov	ch, 1			; seek to track 1
		call	FdDoSeek

		; Another short delay (why?)
		push	cx
		mov	cx, 0
		loop	$
		pop	cx

		; Recalibrate back to track 0 and check that the disk
		; change line has cleared itself - report error if not
		call	FdCalibrate
		mov	dx, PORT_FDC_DIGIN
		in	al, dx
		or_	al, al			; evaluate flags
		mov	al, FD_CL_CHANGED
		jns	.L1
		mov	al, FD_CL_ERROR
		stc
.L1		mov	[FdScratch.returnAh], al

.leaveFunction	retn
		ENDPROC	FdDiskChange

; ---------------------------------------------------------------------
; Delays the appropriate length of time according to the head settle
; and head load values for the current disk drive type.
FdHeadDelay	PROC
		call	FdLoadDpt
		or_	dx, dx		; ???
		jz	.hdSettleDone

		; If we need head settling time,
		mov	al, [es:si+DISKETTE_PARMS.headSettleTime]
		or_	al, al
		jnz	.calcHeadSettle

		; No head settle time in DPB?  Use a default instead.
		mov	al, 0Fh

		; If we have a 360K drive we need a slightly longer
		; default head settle time.
		mov	bl, [bp+IsrStackAx.dl]
		xor_	bh, bh
		push	ax
		call	FdGetMediaState
		mov_	cl, al
		pop	ax
		and	cl, FDMS_LAST_ACCESS
		jz	.longerSettle
		cmp	cl, FDMS_LA_K360M_360D
		jnz	.calcHeadSettle
.longerSettle	mov	al, 14h

.calcHeadSettle	; convert head settle time to usable value and delay
		mov	ah, 10
		mul	ah
		mov_	cx, ax
.headSettleLoop	call	TenthMilliDelay
		loop	.headSettleLoop

.hdSettleDone	; OS Hook - Device busy, diskette motor start
		mov	ax, 90FDh
		int	15h
		jc	.leaveFunction		; OS hook can veto delay

		mov	al, [es:si+DISKETTE_PARMS.motorStartTime]
		mov	ah, [bp+IsrStackAx.ah]	; load int13 function code?
		shl	ah, 1			; ???
		jnc	.leaveFunction		; ??? no int13 functions > 80h?

		; The entire rest of this function seems to be useless!
		; It always gets skipped by the test above since no int13
		; function codes exist with their top bit set.
		test	ah, 2
		jz	.L1
		cmp	al, 8			; clamp minimum 8
		jnb	.L2
		mov	al, 8
.L1		cmp	al, 5			; clamp minimum 5
		jnb	.L2
		mov	al, 5

.L2		; Multiply delay into useable value
		push	ax
		mov	ah, 125
		mul	ah
		mov	dx, 1000
		mul	dx
		mov_	cx, dx
		mov_	dx, ax
		mov	ah, 86h			; System wait
		int	15h
		pop	ax
		jnc	.leaveFunction		; int15 hook can veto wait

.wait1		mov	cx, 1250
.wait2		call	TenthMilliDelay
		loop	.wait2
		dec	al
		jnz	.wait1

.leaveFunction	and	[bp+IsrStackAx.ah], 7Fh
		retn
		ENDPROC	FdHeadDelay

; ---------------------------------------------------------------------
; As FdGetDriveType, but returns with CF set if CMOS config is known to
; be invalid.
FdGetDriveType2	PROC
		mov	al, CMOS_STATUS_DIAG | NMI_ENABLE
		call	ReadCmos
		sti
		cmp	al, 0C0h		; check pwr fail/bad checksum
		cmc
		jc	FdGetDriveType.leaveFunction
		; fall-through to FdGetDriveType
		ENDPROC	FdGetDriveType2

; ---------------------------------------------------------------------
; Returns CMOS drive type ID in AL for the current drive.
FdGetDriveType	PROC
		mov	al, CMOS_FD_TYPE | NMI_ENABLE
		call	ReadCmos
		sti
		cmp	[bp+IsrStackAx.dl], 2
		jnz	.notFd2
		call	FdExternalType
		jmp	.driveTypeInAl
		FillerNop

.notFd2		cmp	[bp+IsrStackAx.dl], 0
		jnz	.driveTypeInAl
		; Move FD1 drive type to lower nibble
		push	cx
		mov	cl, 4
		ror	al, cl
		pop	cx

.driveTypeInAl	mov_	ah, al
		and	al, 0Fh

.leaveFunction	retn
		ENDPROC	FdGetDriveType

; ---------------------------------------------------------------------
; Resets the FDC and sets up drive motor and select bits to match the
; contents of the BDA.  Reloads head load/settle times if it detects
; abnormal command completion.  Returns with CF set on error.
FdResetSelect	PROC
		; Don't let the FDC interrupt us while we're resetting it
		cli

		; Mark all drives not calibrated
		and	[FdCalibStatus], 70h

		; Get drive motor enable bits into the correct format
		; for the FDC digital output register
		mov	al, [FdMotorStatus]
		mov_	ah, al
		mov	cl, 4
		shl	al, cl

		; Setup digital output register lower half:
		; enable DMA/interrupts, place FDC into reset, select drive 2
		or	al, FDC_DIGOUT_DMAINT | 2

		; Adjust drive select bits based on enabled motor line
		test	ah, 40h		; FD2 selected?
		jnz	.buildRegValue
		dec	ax
		test	ah, 20h		; FD1 selected?
		jnz	.buildRegValue
		dec	ax

.buildRegValue	mov	dx, PORT_FDC_DIGOUT
		out	dx, al

		; Delay a bit before taking the FDC out of reset
		mov	cx, 4
		loop	$
		or	al, FDC_DIGOUT_RESET
		out	dx, al

		; Safe to allow FDC interrupts now, we expect one after reset
		sti
		call	FdWaitInt
		call	FdSenseInt
		jc	.leaveFunction

		; Check for abnormal termination (expected after reset?)
		and	ah, FDC_ST0_INTERRUPT_CODE
		xor	ah, FDC_ST0_INTERRUPT_CODE
		test	ah, FDC_ST0_INTERRUPT_CODE
		jz	.abnormal

		or	[FdScratch.returnAh], INT13STAT_BADCONTROLLER
		jmp	.leaveFunction

.abnormal	; Re-specify load/unload times
		mov	al, FDC_CMD_SPECIFY
		call	FdWriteByte
		jc	.leaveFunction
		xor_	bx, bx
		call	FdWriteDptBytes2

.leaveFunction	retn
		ENDPROC	FdResetSelect

; ---------------------------------------------------------------------
; Updates motor status bits in the BDA based on current drive and motor
; timeout, then syncs the motor state with the FDC.
FdUpdateMotor	PROC
		; Don't let us get interrupted while poking the BDA
		cli

		; Convert drive index to motor status bit mask in DH
		mov	cl, [bp+IsrStackAx.dl]
		mov	dh, 1
		shl	dh, cl

		; Motor should be on? If it isn't, use max motor timeout
		mov	ch, [FdMotorStatus]
		mov	al, 0FFh
		test_	ch, dh
		jz	.L1

		; Add drive motor timeout to current count
		call	FdLoadDpt
		mov	ah, [es:si+DISKETTE_PARMS.motorWaitTime]
		mov	al, FD_MIN_MOTOR_TIMEOUT
		cmp	[FdMotorTimeout], ah
		jbe	.L2
		sub_	al, ah
		add	al, [FdMotorTimeout]

.L1		or	[bp+IsrStackAx.ah], INT13STAT_TIMEOUT

.L2		; Sync motor with FDC and updte status byte
		; TOOD: lots of bit-twiddling here, comment it all...
		mov	[FdMotorTimeout], al
		and	ch, 80h
		or_	ch, dh
		mov_	al, ch
		mov	cl, 4
		shl	al, cl
		shl	dh, cl
		or_	ch, dh
		or	al, 0Ch
		or	al, [bp+IsrStackAx.dl]
		mov	dx, PORT_FDC_DIGOUT
		out	dx, al
		mov	[FdMotorStatus], ch

		; All done, safe for interrupts now
		sti
		retn
		ENDPROC	FdUpdateMotor

; ---------------------------------------------------------------------
; Loads diskette parameter table address to ES:SI from the IVT @ Int 1Eh
FdLoadDpt	PROC
		xor_	si, si
		mov	es, si
		les	si, [es:IvtDisketteTable]
		retn
		ENDPROC	FdLoadDpt

; ---------------------------------------------------------------------
; Configures the DMA controller to perform a read/write of one sector
; given the current registers in the IsrStackAx instance.
FdSetupDma	PROC
		; Setup the correct DMA mode for te given int13/AX
		; function.  Note that the 8237 uses read/write in the
		; opposite sense to the CPU: reads are from memory ->
		; I/O device, and writes are I/O device -> memory!
		mov	ah, [bp+IsrStackAx.ah]

		; DMA mode: single transfer, increment, read, channel 2
		shr	ah, 1
		mov	al, 4Ah
		jb	.gotDmaMode
		; DMA mode: single transfer, increment, write, channel 2
		shr	ah, 1
		mov	al, 46h
		jb	.gotDmaMode
		; DMA mode: single transfer, increment, verify, channel 2
		mov	al, 42h

.gotDmaMode	mov	dx, PORT_DMA1_MODE
		out	dx, al

		; Lots of bit twiddling to get the transfer address
		; and count into the right format for DMA registers
		; TODO: comment this...
		mov	dx, [bp+IsrStackAx.es]
		call	FdLoadDpt
		mov	cl, 4
		rol	dx, cl
		mov_	ch, dl
		and	ch, 0Fh
		and	dl, 0F0h
		add	dx, [bp+IsrStackAx.bx]
		adc	ch, 0
		mov	al, [bp+IsrStackAx.al]
		xor_	ah, ah
		mov	cl, [es:si+DISKETTE_PARMS.sectorSize]
		shl	ax, cl
		cmp	ax, 512
		ja	.leaveFailure
		mov	cl, 7
		shl	ax, cl
		dec	ax
		mov_	bx, dx
		add_	dx, ax
		jc	.leaveFailure

		; Write to DMA controller with interrupts disabled
		cli

		; Ensure flip-flop is clear (no partial writes)
		mov	dx, PORT_DMA1_CLEAR_FF
		out	dx, al

		; Write word count
		mov	dx, PORT_DMA_CHAN2_WC
		Delay	2
		out	dx, al
		xchg	al, ah
		Delay	2
		out	dx, al

		; Write base address
		mov	dx, PORT_DMA_CHAN2_BASE
		xchg	ax, bx
		Delay	2
		out	dx, al
		xchg	al, ah
		Delay	2
		out	dx, al

		; Enable interrupts, we're done with the 16-bit registers
		sti

		; Setup page register and unmask the DMA channel
		xchg	al, ch
		mov	dx, PORT_DMA_PAGE2
		out	dx, al
		mov	dx, PORT_DMA1_MASK
		mov	al, 2			; clear mask, ch 2
		Delay	1
		out	dx, al

		; Return success
		clc

.leaveFunction	retn

.leaveFailure	mov	[FdScratch.returnAh], INT13STAT_DMABOUNDARY
		stc
		jmp	.leaveFunction
		ENDPROC	FdSetupDma

; ---------------------------------------------------------------------
; Blocks until the floppy disk controller is ready to read/write from
; its data port.
; On success:
;   CF clear
;   AL == FDC status register
;   DX == 3F4h (PORT_FDC_STATUS)
; On failure:
;   CF set, 80h placed in FdScratch.returnAh
FdWaitReady	PROC
		mov	dx, PORT_FDC_STATUS
		push	cx
		xor_	cx, cx
.waitFdcReady	in	al, dx
		test	al, FDC_STATUS_REQUEST
		loope	.waitFdcReady
		jnz	.leaveFunction

		; Timeout waiting for FDC to be ready
		or	[FdScratch.returnAh], INT13STAT_TIMEOUT
		stc

.leaveFunction	pop	cx
		retn
		ENDPROC	FdWaitReady

; ---------------------------------------------------------------------
; Builds the second byte of an FDC command by combining the drive index
; and head number from the current int13 call, then writes it to the
; FDC.  Command byte format is:
;   X X X X X HD US1 US0
FdWriteHdUs	PROC
		mov	al, [bp+IsrStackAx.dh]
		shl	al, 1
		shl	al, 1
		or	al, [bp+IsrStackAx.dl]
		clc
		; fall-through to FdWriteByte2
		ENDPROC	FdWriteHdUs

; ---------------------------------------------------------------------
; As FdWriteByte, but first performs a check of the carry flag and
; returns immediately if it is set.  Useful when writing bytes to the
; FDC in a loop as the result of the previous write is checked
; automatically.
FdWriteByte2	PROC
		jc	FdWriteByte.leaveFunction
		; fall-through to FdWriteByte
		ENDPROC	FdWriteByte2

; ---------------------------------------------------------------------
; Waits for the FDC to be ready to accept data, then writes the byte in
; AL to it's data port.  Sets CF on failure.
FdWriteByte	PROC
		mov_	ah, al
		call	FdWaitReady
		jc	.leaveFunction

		; Expect FDC to be waiting for data
		test	al, FDC_STATUS_DIRECTION
		jnz	.failure

		; Increment to the data port and write
		inc	dx
		mov_	al, ah
		out	dx, al
		retn

.failure	; Drain data port and report error
		call	FdReadResult7
		or	[FdScratch.returnAh], INT13STAT_BADCONTROLLER
		stc

.leaveFunction	retn
		ENDPROC	FdWriteByte

; ---------------------------------------------------------------------
; Writes two consecutive bytes to the FDC from the current diskette
; parameter table.  Sets CF on failure.
; On entry:
;   BX == index into diskette parameter table
FdWriteDptBytes2	PROC
		mov	cx, 2
		; fall-through to FdWriteDptBytes
		ENDPROC	FdWriteDptBytes2

; ---------------------------------------------------------------------
; Writes CX bytes from the current diskette parameter table at index BX
; to the floppy disk controller.  Sets CF on failure.
FdWriteDptBytes	PROC
		call	FdLoadDpt
.writeByte	mov	al, [es:si+bx]
		call	FdWriteByte2
		jc	.leaveFunction
		inc	bx
		loop	.writeByte
		clc

.leaveFunction	retn
		ENDPROC	FdWriteDptBytes

; ---------------------------------------------------------------------
; As FdReadResult, but always reads seven bytes.  This is enough to
; read the standard 7-byte command result (ST0/1/2, C/H/R/N).
FdReadResult7	PROC
		mov	cx, 7
		; fall-through to FdReadResult
		ENDPROC	FdReadResult7

; ---------------------------------------------------------------------
; Reads bytes from the floppy disk controller's data port, storing them
; from FdScratch.fdcSt0
; On entry:
;   CX == number of bytes to read
; On return:
;   CF set on error
;   AH == first byte read
FdReadResult	PROC
		mov	bx, FdScratch.fdcSt0
.readByte	call	FdWaitReady
		jc	.leaveFunction

		; Verify FDC isn't busy or waiting on the CPU
		and	al, FDC_STATUS_DIRECTION | FDC_STATUS_BUSY
		cmp	al, FDC_STATUS_DIRECTION | FDC_STATUS_BUSY
		jnz	.failure

		; Advance to data port and read the byte
		inc	dx
		in	al, dx
		mov	[bx], al
		inc	bx

		; Brief delay so FDC can keep up?
		push	cx
		mov	cx, 11
		loop	$
		pop	cx

		; Read more bytes
		loop	.readByte
		clc
		jmp	.leaveFunction

		; ??? dead code - previous jmp used to be conditional?
		or	[FdScratch.returnAh], INT13STAT_BADCONTROLLER

.failure	stc

.leaveFunction	mov	ah, [FdScratch.fdcSt0]
		retn
		ENDPROC	FdReadResult

; ---------------------------------------------------------------------
; Hardware interrupt handler - floppy disk controller
; Acknowledges the interrupt and sets a flag in the BDA so int13 code
; knows the FDC requires attention.
IntE_Actual	PROC
		push	ax
		push	dx
		push	ds

		; Set flag for int13 handlers
		call	LoadBdaToDs
		or	[FdCalibStatus], FDCAL_INT

		; ack the interrupt
		mov	al, NONSPECIFIC_EOI
		mov	dx, PORT_PIC1_CTRL
		out	dx, al

		; notify OS hook and return
		pop	ds
		pop	dx
		mov	ax, 9101h	; OS Hook - interrupt complete
		int	15h
		pop	ax
		iret
		ENDPROC	IntE_Actual

; ---------------------------------------------------------------------
; Waits for the IntE handler to signal that it has been called.
; Sets CF on timeout.
FdWaitInt	PROC
		; OS hook - diskette busy
		mov	ax, 9001h
		int	15h
		jc	.timeout		; OS hook can veto spinloop

		; Spin waiting for interrupt flag to change
		mov	cx, 0Fh
.wait1		push	cx
		mov	cx, 0
.wait2	test	[FdCalibStatus], FDCAL_INT
		loope	.wait2
		pop	cx
		loope	.wait1
		jnz	.success

.timeout	call	FdResetSelect
		mov	[FdScratch.returnAh], INT13STAT_TIMEOUT
		stc
		retn

.success	and	[FdCalibStatus], ~FDCAL_INT
		retn
		ENDPROC	FdWaitInt

ENDPROGRAM	FD_CONTROLLER
