
; Constants for drive count and 0-based indexes
MAX_FLOPPY_DRIVES	equ	3
MAX_FLOPPY_INDEX	equ	(MAX_FLOPPY_DRIVES - 1)

; ---------------------------------------------------------------------
; Int13Fd_Actual
; Provides int13 floppy disk services [TechRef 9-4].
; ---------------------------------------------------------------------
Int13Fd_Actual	PROC
		call	MakeIsrStack

		; Mysterious check for int13, subfunction 0D4h.
		; No idea what software calls this and I can't find anywhere
		; that documents what this subfunction might do.
		; In this version of the BIOS it hardlocks the machine.
		cmp	ah, 0D4h
		jnz	.notFunctionD4
		Inline	WriteString, 0Dh, 0Ah, 0Ah, \
			'A newer version of this software is required on this machine.', \
			07h, 00h
		jmp	$

.notFunctionD4	; Dispatch via function table.
		; Add AX to the stack image created by MakeIsrStack earlier.
		push	ax
		mov_	bp, sp
		mov	di, (.handlersLast - .handlers)
		call	FuncToOffset
		jnb	.validFunction
		; Strange default for invalid function codes...
		jmp	FdChangeLine

.validFunction	; Compare the drive index to the maximum valid floppy drive
		; so that handlers which care about the drive index don't
		; need to make their own comparison and can just use the flags
		cmp	[bp+IsrStackAx.dx], MAX_FLOPPY_DRIVES,DATA=BYTE
		cmc

		jmp	[cs:.handlers+di]

.handlers	dw	FdReset
		dw	FdStatus
		dw	FdPerformOp	; Read sectors
		dw	FdPerformOp	; Write sectors
		dw	FdPerformOp	; Verify sectors
		dw	FdFormat
		dw	FdUnsupported	; AX = 06h/07h only supported on XT
		dw	FdUnsupported
		dw	FdDiskParms
		dw	FdUnsupported	; AX = 09h to 14h not supported by
		dw	FdUnsupported	;      floppy disk subsystem.  Some are
		dw	FdUnsupported	;      supported by the AT-compatible
		dw	FdUnsupported	;      hard disk subsystem.
		dw	FdUnsupported
		dw	FdUnsupported
		dw	FdUnsupported
		dw	FdUnsupported
		dw	FdUnsupported
		dw	FdUnsupported
		dw	FdUnsupported
		dw	FdUnsupported
		dw	FdDasdType
		dw	FdChangeLine
		dw	FdSetDasdType
.handlersLast	dw	FdSetMediaType
		ENDPROC	Int13Fd_Actual

