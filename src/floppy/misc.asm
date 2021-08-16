
; ---------------------------------------------------------------------
; Busy loop that delays for approximately one tenth of a millisecond.
TenthMilliDelay	PROC
		; Clock count for each instruction works out to total
		; execution time of ((DelayFactor*10)-15) clocks.
		; A near call to this method takes 7 clocks, plus 8
		; for a loop opcode to repeat it.  That cancels out our
		; adjustment of -4 to DelayFactor making this method's
		; execution time depend solely on the value chosen
		; for delay factor.
		push	cx			; 3 clocks
		mov	cl, [cs:DelayFactor]	; 5 clocks
		sub	cl, 4			; 3 clocks
		mov	ch, 0			; 2 clocks
.loop		clc				; 2 clocks
		loop	.loop			; 8 clocks normally, 4 on final
		pop	cx			; 5 clocks
		retn				; 11 clocks
		ENDPROC	TenthMilliDelay

; ---------------------------------------------------------------------
; Determines drive info byte from drive/media characteristics.
; TODO: comment all this...
FdExtractDriveInfo	PROC
		mov	al, [bp+IsrStackAx.dl]
		call	IsExtFdIndex
		stc
		jz	.L1

		; Reset data transfer rate
		mov	dx, PORT_FDC_DIGIN
		mov	[FdConfig], 0
		mov	al, 0
		out	dx, al

		; Try seekking past track 40
		call	FdUpdateMotor
		mov	ch, 50
		call	FdSeek

.L1		mov	ax, 0		; try to find track 0
		jc	.mediaKnown
		mov	ch, 10		; retry 10 times
		jmp	.seekTrack0

.checkTrack0	push	cx
		call	FdCheckTk0
		pop	cx
		jc	.mediaKnown
		jnz	.foundTrack0
		dec	ch		; count down retries

.seekTrack0	push	cx
		call	FdSeek
		pop	cx
		jb	.mediaKnown
		or_	ch, ch		; out of retries?
		jnz	.checkTrack0

		; Out of retries - did we find track 0?
		push	cx
		call	FdCheckTk0
		pop	cx
		jc	.mediaKnown
		mov	ax, 107h	; ???
		jnz	.mediaKnown

.foundTrack0	; Reach track zero with less than ten steps from track
		; 50, so assume it's a 40-track disk.
		mov	ax, 93h

.mediaKnown	; Update media state and merge in drive info
		mov	bl, [bp+IsrStackAx.dl]
		mov	bh, 0
		call	FdSetMediaState
		pushf
		call	FdGetDriveInfo
		or_	al, ah
		call	FdSetDriveInfo
		popf
		mov	[FdConfig], 0C0h	; ???
		retn
		ENDPROC	FdExtractDriveInfo

; ---------------------------------------------------------------------
; Resets the floppy disk controller and checks that the drive types
; stored in the CMOS RAM make sense when compared to the hardware.
; Any problems are reported on screen.
FdCheckConfigValid	PROC
		; Since we're called during POST we aren't actually in
		; an int13 interrupt call.  We need to reserve some stack
		; space for a simulated IsrStackAx structure so we can
		; call floppy disk int13 code later.
		sub	sp, 10h
		mov_	bp, sp

		call	LoadBdaToDs
		mov	[FdConfig], 1
		mov	[FdDriveInfo], 66h
		mov	[Fd2DriveInfo], 6

		; Turn on (unmask) floppy disk DMA channel
		mov	dx, PORT_PIC1_MASK
		in	al, dx
		and	al, 0BEh
		Delay	2
		out	dx, al

		; Reset the floppy disk controller
		; BUG: code uses the AT-compatible hard disk controller
		;      port instead of the floppy disk one?!
		mov	dx, PORT_HDC_DIGOUT
		mov	al, HDC_DIGOUT_SRST
		out	dx, al

		; Give the FDC enough time to reset properly
		mov	cx, 400
		loop	$

		; Take it out of reset
		xor_	al, al
		out	dx, al

		; Reset the BIOS floppy subsystem
		xor_	ah, ah
		xor_	dl, dl
		int	13h
		jnc	.resetOk
		Inline	WriteString,'Diskette subsystem reset failed',0Dh,0Ah,0
		call	SetSoftResetFlag
		jmp	.checkCmos

		; Setup IsrStackAx and extract drive info for each
		; drive in turn.
.resetOk	mov	[bp+IsrStackAx.dx], 0
		call	FdExtractDriveInfo
		mov	[bp+IsrStackAx.dl], 2
		call	FdExtractDriveInfo
		mov	[bp+IsrStackAx.dl], 1
		call	FdExtractDriveInfo
		jnb	.checkCmos
		; Odd call to drive type here, it all ends up at the same place
		call	FdGetDriveType2
		jc	.checkCmos
		jz	.checkCmos

.checkCmos	; Ensure drive motors stop on next tick and tear down
		; IsrStackAx structure as we don't need it anymore
		mov	[FdMotorTimeout], 1
		add	sp, 10h

		; Check CMOS checksum is OK before proceeding with
		; drive type checks.  If it is bad other code in POST
		; will report that, we don't need to.
		mov	al, CMOS_STATUS_DIAG | NMI_DISABLE
		call	ReadCmos
		mov_	bh, al
		test	bh, 70h		; checksum OK?
		jz	.checksumOk
		jmp	.leaveFunction
		nop			; assembler-inserted nop

.checksumOk	; Extract the CMOS floppy disk types into AL (FD0) and
		; CL (FD1).  External drive is dynamic and not stored
		; in CMOS so we don't need to check it.
		mov	al, CMOS_FD_TYPE | NMI_DISABLE
		call	ReadCmos
		mov_	cl, al
		shr	al, 1
		shr	al, 1
		shr	al, 1
		shr	al, 1

		; Check FD0 and FD1 media types in turn, counting
		; number of valid drives in BL.
		xor_	bl, bl		; drive count starts at zero
		mov	si, Fd0MediaState
		call	FdMediaTypeOk
		jc	.badDriveConfig

		mov_	al, cl
		inc	si
		inc	bl
		call	FdMediaTypeOk
		jc	.badDriveConfig

		; If both drives check out, we're done
		jmp	.leaveFunction

.badDriveConfig	push	bx
		or	bh, 20h		; mark CMOS config as bad
		mov_	al, bh
		mov	ah, CMOS_STATUS_DIAG | NMI_DISABLE
		call	WriteCmos
		Inline	WriteString,'Invalid configuration information; code ',0
		pop	bx
		push	ax
		mov_	al, bl
		xor_	ah, ah
		call	WriteCharHex2
		Inline	WriteString,0Dh,0Ah,0
		pop	ax
		call	SetSoftResetFlag

.leaveFunction	retn
		ENDPROC	FdCheckConfigValid

; ---------------------------------------------------------------------
; Checks whether the CMOS floppy disk drive type in AL conflicts with
; the floppy media byte stored at [SI].  Sets CF on failure.
FdMediaTypeOk	PROC
		and	al, 0Fh		; mask to drive type nibble
		cmp	[si], 0,DATA=BYTE
		jnz	.mediaStateSet

		; Media byte not set, expect drive not present
		cmp	al, 0
		jz	.configOk
		jmp	.configMismatch
		nop			; assembler-inserted nop

.mediaStateSet	; Extra checks for 360KB media
		cmp	[si], 93h,DATA=BYTE
		jnz	.not360K
		cmp	al, 1
		jnz	.configMismatch
		jmp	.configOk
		nop			; assembler-inserted nop

.not360K	cmp	al, 2
		jb	.configMismatch

.configOk	clc
		jmp	.leaveFunction
		nop			; assembler-inserted nop

.configMismatch	stc

.leaveFunction	retn
		ENDPROC	FdMediaTypeOk

