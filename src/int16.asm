
; ===========================================================================
; Int16_Actual [TechRef 8-5]
; Provides BIOS keyboard services.
; ===========================================================================
Int16_Actual	PROC
		; Re-enable interrupts (safe because the keyboard can't cause
		; another interrupt fast enough to overflow our stack).
		sti

		; Save registers used by all Int16 handlers, range check
		; the AH function argument, and dispatch via jump table.
		push	di
		push	ds
		mov	di, .handlersLast - .handlers
		call	FuncToOffset
		jb	KbUnsupported
		jmp	[cs:di+.handlers]

.handlers	dw	KbReadChar
		dw	KbCheckChar
		dw	KbShiftStatus
		dw	KbSetTypematic
		dw	KbUnsupported
		dw	KbPlaceChar
		dw	KbUnsupported
		dw	KbUnsupported
		dw	KbUnsupported
		dw	KbUnsupported
		dw	KbUnsupported
		dw	KbUnsupported
		dw	KbUnsupported
		dw	KbUnsupported
		dw	KbUnsupported
		dw	KbUnsupported
		dw	KbReadExt
		dw	KbCheckCharExt
.handlersLast	dw	KbShiftStatusEx
		ENDPROC	Int16_Actual

; ===========================================================================
; KbShiftExt [TechRef 8-7]
; Reads extended shift status for enhanced keyboard.
; ===========================================================================
KbShiftStatusEx	PROC
		mov	al, [KbShiftFlags2]
		mov	ah, [KbStatusFlags3]

		; Build int16/AH=12 AH return value
		and	ah, KBSTAT3_RALT | KBSTAT3_RCTRL
		test	al, KBSHIFT2_SYSREQ
		jz	.L1
		or_	ah, 80h		; mark sysreq key pressed
.L1		; Mask off shift states that don't have indicator LEDs
		and	al, ~(KBSHIFT2_SYSREQ | KBSHIFT2_PAUSE | KBSHIFT2_INSERT)
		or_	ah, al

		; Fall-through into regular shift status function
		ENDPROC	KbShiftStatusEx

; ===========================================================================
; KbShiftStatus [TechRef 8-6]
; Reads shift status for all keyboard types.
; ===========================================================================
KbShiftStatus	PROC
		mov	al, [KbShiftFlags1]
		; Fall-through into shared function tail
		ENDPROC	KbShiftStatus

; ===========================================================================
; KbStatusRet
; Shared function tail for Int16 status handlers
; ===========================================================================
KbStatusRet	PROC
		pop	ds
		pop	di
		iret
		ENDPROC	KbStatusRet

; ===========================================================================
; KbUnsupported
; Stub for int16 functions we do not support.
; ===========================================================================
KbUnsupported	PROC
		; Can't find any docs on what unsupported int16 functions
		; return.  AT BIOS doesn't do anything special for invalid
		; functions but AH does end up modified by a dec/jz chain.
		; I guess making sure AX is changed is enough for compat...
		dec	ax
		jmp	KbStatusRet
		ENDPROC	KbUnsupported

; ===========================================================================
; KbReadChar [TechRef 8-6]
; Reads next character from the keyboard.
; ===========================================================================
KbReadChar	PROC
		call	KbSyncLedState

		; Disable interrupts while we check the keyboard buffer
		cli
		mov	di, [KbNextChar]
		cmp	di, [KbLastChar]	; key available?
		jnz	.keyAvailable

		; If not key available, wait for one to appear
		mov	ax, 9002h
		int	15h			; OS hook - device busy, keyboard
.waitKeyAvail	cli				; disable interrupts while we check the keyboard buffer
		mov	di, [KbNextChar]
		cmp	di, [KbLastChar]	; key available?
		jnz	.keyAvailable
		sti				; give KBC a chance to trigger interrupt
		jmp	.waitKeyAvail

.keyAvailable	; Load BIOS scan code and ASCII value into AH:AL
		mov	ax, [di]
		call	KbAdvanceBuf
		mov	[KbNextChar], di
		sti				; done modifying keyboard buffer

		; Mysterious numpad translation?
		cmp	ah, SC2_PREFIX_EXT	; [TechRef 8-37] claims E0h-prefixed scan codes aren't supported?
		jnz	.notExtCode
		call	KbTranslateExt
		jmp	.gotKey
		nop				; assembler-inserted nop

.notExtCode	; Mysterious extra translation...
		; Maybe something in the int9 handler produces these values...
		cmp	ah, 85h
		jnb	.waitKeyAvail
		cmp	al, 0E0h
		jnz	.L1
		cmp	ah, 0
		jz	.L1
		mov	al, 0
.L1		cmp	al, 0F0h
		jnz	.gotKey
		cmp	ah, 0
		jnz	.waitKeyAvail

.gotKey		; Got a scan code and ASCII value, sync LEDs and undo int16 stack frame
		call	KbSyncLedState
		pop	ds
		pop	di
		iret
		ENDPROC	KbReadChar

; ===========================================================================
; KbReadExt [TechRef 8-7]
; Reads key from enhanced keyboard.
; ===========================================================================
KbReadExt	PROC
		call	KbSyncLedState

		; Disable interrupts while we check the keyboard buffer
		cli
		mov	di, [KbNextChar]
		cmp	di, [KbLastChar]	; key available?
		jnz	.keyAvailable

		; If not key available, wait for one to appear
		mov	ax, 9002h
		int	15h			; OS hook - device busy, keyboard
.waitKeyAvail	cli				; disable interrupts while we check the keyboard buffer
		mov	di, [KbNextChar]
		cmp	di, [KbLastChar]	; key available?
		jnz	.keyAvailable
		sti				; give KBC a chance to trigger interrupt
		jmp	.waitKeyAvail

.keyAvailable	; Load BIOS scan code and ASCII value into AH:AL
		mov	ax, [di]
		call	KbAdvanceBuf
		mov	[KbNextChar], di
		sti				; done modifying keyboard buffer

		; Less mysterious translation that the non-ext read method
		cmp	al, 0F0h
		jnz	.gotKey
		cmp	ah, 0
		jz	.gotKey
		mov	al, 0

.gotKey		; Got a scan code and ASCII value, sync LEDs and undo int16 stack frame
		call	KbSyncLedState
		pop	ds
		pop	di
		iret
		ENDPROC	KbReadExt

; ===========================================================================
; KbCheckChar [TechRef 8-6]
; Nonblocking form of KbReadChar.
; ===========================================================================
KbCheckChar	PROC
		call	KbSyncLedState

		; Disable interrupts while we check the keyboard buffer
		cli
		mov	di, [KbNextChar]
.checkKeyAvail	cmp	di, [KbLastChar]	; key available?
		jz	.leaveFunction

		; Load BIOS scan code and ASCII value into AH:AL
		mov	ax, [di]
		pushf

		; Mysterious numpad translation?
		cmp	ah, SC2_PREFIX_EXT	; [TechRef 8-37] claims E0h-prefixed scan codes aren't supported?
		jnz	.notExtCode
		call	KbTranslateExt
		jmp	.gotKey
		nop				; assembler-inserted nop

.notExtCode	; Mysterious extra translation...
		; Maybe something in the int9 handler produces these values...
		cmp	ah, 85h
		jnb	.gotExtKey
		cmp	al, 0E0h
		jnz	.L1
		cmp	ah, 0
		jz	.L1
		mov	al, 0
.L1		cmp	al, 0F0h
		jnz	.gotKey
		cmp	ah, 0
		jnz	.gotExtKey

.gotKey 	popf

.leaveFunction	; Shared function tail with KbCheckCharExt
		sti				; done modifying keyboard buffer
		pop	ds
		pop	di
		retf	2

.gotExtKey	; Skip extended keyboard scancodes
		popf
		call	KbAdvanceBuf
		mov	[KbNextChar], di
		jmp	.checkKeyAvail
		ENDPROC	KbCheckChar

; ===========================================================================
; KbCheckCharExt [TechRef 8-7]
; Nonblocking form of KbCheckChar.
; ===========================================================================
KbCheckCharExt	PROC
		call	KbSyncLedState

		; Disable interrupts while we check the keyboard buffer
		cli
		mov	di, [KbNextChar]
		cmp	di, [KbLastChar]	; key available?
		jz	KbCheckChar.leaveFunction

		; Load BIOS scan code and ASCII value into AH:AL
		mov	ax, [di]

		; Mysterious scancode translation
		cmp	al, 0F0h
		jnz	.gotKey
		cmp	ah, 0
		jz	.gotKey
		mov	al, 0

.gotKey		jmp	KbCheckChar.leaveFunction
		ENDPROC	KbCheckCharExt

; ===========================================================================
; KbPlaceChar [TechRef 8-7]
; Places an entry in the keyboard buffer as if recieved via Int9.
; ===========================================================================
KbPlaceChar	PROC
		; Disable interrupts while we modify the keyboard buffer
		cli

		call	KbExtendBuf
		jz	.kbBufFull
		mov	di, [KbLastChar]
		mov	[di], cx
		call	KbAdvanceBuf
		mov	[KbLastChar], di
		mov	al, 0			; success
		jmp	.leaveFunction
		nop				; assembler-inserted nop

.kbBufFull	mov	al, 1			; failure

.leaveFunction	sti
		pop	ds
		pop	di
		iret
		ENDPROC	KbPlaceChar

; ===========================================================================
; KbSetTypematic
; Set keyboard repeat rate and delay.
; Not documented in TechRef?
; ===========================================================================
KbSetTypematic	PROC
		cmp	al, 5			; only subfunction 5 supported
		jnz	.leaveFunction

		; Range-check arguments (why test instead of cmp?)
		test	bl, 0E0h		; repeat rate must be <= 1Fh
		jnz	.leaveFunction
		test	bh, 0FCh		; delay value must be <= 3
		jnz	.leaveFunction

		; Combine arguments into KBC command byte
		ror	bh, 1
		ror	bh, 1
		ror	bh, 1
		or_	bh, bl

		call	KbDisable
		mov	ah, KB_CMD_SET_TYPEMATIC
		call	KbWriteDataByte
		jb	.kbEnable
		mov_	ah, bh
		call	KbWriteDataByte

.kbEnable	call	KbEnable

.leaveFunction	pop	ds
		pop	di
		iret
		ENDPROC	KbSetTypematic

; ===========================================================================
; KbTranslateExt
; Mysterious translation for extended keyboard scancodes.
; TODO: figure out what this is for.
; ===========================================================================
KbTranslateExt	PROC
		cmp	ax, 0E02Fh
		jnz	.L1
		mov	ax, 352Fh

.L1		cmp	ax, 0E00Dh
		jnz	.L2
		mov	ax, 1C0Dh

.L2		cmp	ax, 0E00Ah
		jnz	.L3
		mov	ax, 1C0Ah

.L3		retn
		ENDPROC	KbTranslateExt

