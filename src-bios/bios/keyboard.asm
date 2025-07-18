
KEYBOARD	PROGRAM	OutFile=keyboard.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"segments/bda.inc"
		include	"keyboard.inc"
		include	"pic.inc"

		PUBLIC	KbAckInt
		PUBLIC	KbAdvanceBuf
		PUBLIC	KbDisable
		PUBLIC	KbEnable
		PUBLIC	KbExtendBuf
		PUBLIC	KbSyncLedState
		PUBLIC	KbWaitIbe
		PUBLIC	KbWaitIbe_Cli
		PUBLIC	KbWriteDataByte
		PUBLIC	KbXlatScancode

; ---------------------------------------------------------------------
; KbAckInt
; Disables interrupts, issues a non-specific EOI to PIC1, then enables
; the keyboard.  Returns with interrupts disabled.
; ---------------------------------------------------------------------
KbAckInt	PROC
		cli
		push	ax
		mov	al, NONSPECIFIC_EOI
		out	PORT_PIC1_CTRL, al
		call	KbEnable
		pop	ax
		retn
		ENDPROC	KbAckInt

; ---------------------------------------------------------------------
; KbSyncLedState
; Updates the keyboard LED state to match the current keyboard shift
; states, setting the KBSTAT4_LED_UPDATE flag while the update is in
; progress.
; ---------------------------------------------------------------------
KbSyncLedState	PROC
		push	ax
		push	cx

		; Prevent interrupts while we check whether the LED
		; state needs updating.
		cli
		mov	al, [KbStatusFlags4]
		or	[KbStatusFlags4], KBSTAT4_LED_UPDATE
		test	al, KBSTAT4_LED_UPDATE	; recursive call?
		jnz	.leaveFunction		; skip if so

		mov	ah, [KbShiftFlags1]
		mov	cl, 4			; move shift states
		shr	ah, cl			; to lowest bits
		and	ax, 0707h		; isolate shift/led bits
		cmp_	ah, al			; LEDs match shift state?
		clc				; assume they do...
		jz	.ledStateSynced		; leave if so

		; Re-enable interrupts, disable the keyboard, and
		; send the new LED state to the keyboard.
		sti
		call	KbDisable
		xchg	ah, al			; save shift flags in al
		mov	ah, KB_CMD_INDICATORS
		call	KbWriteDataByte
		jnb	.setLedState

		; If an error occurred, just reset to defaults
		mov	ah, KB_CMD_DEFAULTS
		call	KbWriteDataByte
		xchg	ah, al
		jmp	.enableKb

.setLedState	; Restore shift flags and use them for the LED state
		xchg	ah, al
		call	KbWriteDataByte

.enableKb	call	KbEnable
		stc				; set return value

.ledStateSynced	; Update stored LED state with interrupts disabled
		pushf
		cli
		and	[KbStatusFlags4], ~(KBSTAT4_LED_SCRLOCK | KBSTAT4_LED_NUMLOCK | KBSTAT4_LED_CAPLOCK | KBSTAT4_LED_UPDATE)
		or	[KbStatusFlags4], ah
		popf

.leaveFunction	pop	cx
		pop	ax
		retn
		ENDPROC	KbSyncLedState

; ---------------------------------------------------------------------
; KbXlatScancode
; Looks up the supplied scan code in a translation table.
;
; On entry:
;   AL == scan code
;   DI -> scan code translation table
;
; On return:
;   AL == translated scan code
;   ZF set if no valid translation
; ---------------------------------------------------------------------
KbXlatScancode	PROC
		dec	al		; convert 1-based scan code to index
		cmp	al, SC2_F12	; range check max valid index
		jnb	.leaveFunction

		xchg	bx, di
		xlat	[cs:bx]		; translate scan code
		xchg	bx, di
		cmp	al, 0FFh	; set flags for return

.leaveFunction	retn
		ENDPROC	KbXlatScancode

; ---------------------------------------------------------------------
; KbExtendBuf
; Returns KbLastChar advanced by one keyboard buffer entry in DI,
; wrapped to the start of the keyboard buffer if necessary.  ZF set if
; wrapping occurs.
; ---------------------------------------------------------------------
KbExtendBuf	PROC
		mov	di, [KbLastChar]
		; Fallthrough to KbExtendBuf
		ENDPROC	KbExtendBuf

; ---------------------------------------------------------------------
; Returns DI advanced by one keyboard buffer entry, wrapped to the
; start of the keyboard buffer if necessary.  ZF set if wrapping occurs.
; ---------------------------------------------------------------------
KbAdvanceBuf	PROC
		inc	di
		inc	di
		cmp	di, [KbBufEnd]
		jb	.L1
		mov	di, [KbBufStart]	; wrap to start
.L1		cmp	di, [KbNextChar]	; set flags for return
		retn
		ENDPROC	KbAdvanceBuf

; ---------------------------------------------------------------------
; KbWaitIbe_Cli
; As KbWaitIbe, but disables interrupts.
; ---------------------------------------------------------------------
KbWaitIbe_Cli	PROC
		cli
		; Fallthrough into KbWaitIbe
		ENDPROC	KbWaitIbe_Cli

; ---------------------------------------------------------------------
; KbWaitIbe
; Waits for keyboard controller input buffer to be empty (i.e. keyboard
; ready to accept data).  Loops up to CX times.  Sets ZF on success.
; ---------------------------------------------------------------------
KbWaitIbe	PROC
		push	ax
		push	cx

.checkIbe	in	al, PORT_KBC_STATUS
		test	al, KBC_STATUS_IBF
		loopne	.checkIbe

		pop	cx
		pop	ax
		retn
		ENDPROC	KbWaitIbe

; ---------------------------------------------------------------------
; KbWriteDataByte
; Writes the byte in AH to the keyboard controller's data port,
; effectively sending the byte to the keyboard for processing.
; Sets CF on failure.
; ---------------------------------------------------------------------
KbWriteDataByte	PROC
		push	bx
		mov	bx, 3			; attempt three retries
		push	ax
		push	cx

		; Clear ack/retry bits
		and	[KbStatusFlags4], ~(KBSTAT4_ACK | KBSTAT4_RESEND)

.retry		xor_	cx, cx
		call	KbWaitIbe		; wait for keyboard to be ready

		mov_	al, ah
		out	PORT_KBC_DATA, al	; write data

		mov	cx, KBC_DBYTE_TIMEOUT
.waitResponse	test	[KbStatusFlags4], KBSTAT4_ACK | KBSTAT4_RESEND
		loope	.waitResponse

		jz	.noResponse		; timeout expired?
		test	[KbStatusFlags4], KBSTAT4_ACK
		jnz	.leaveFunction		; ack response

		; Not timed out, not ack.  Must be a resend response.
		; Reset flags and try again.
		and	[KbStatusFlags4], ~(KBSTAT4_ACK | KBSTAT4_RESEND)
.noResponse	dec	bx
		jnz	.retry
		stc

.leaveFunction	pop	cx
		pop	ax
		pop	bx
		retn
		ENDPROC	KbWriteDataByte

; ---------------------------------------------------------------------
; KbEnable
; Commands the keyboard controller to recommence key scanning.
; ---------------------------------------------------------------------
KbEnable	PROC
		push	ax
		push	cx

		xor_	cx, cx			; maximum wait for IBE
		call	KbWaitIbe

		mov	al, KBC_CMD_ENABLE
		out	PORT_KBC_CMD, al

		pop	cx
		pop	ax
		retn
		ENDPROC	KbEnable

; ---------------------------------------------------------------------
; KbDisable
; Instructs the keyboard controller to cease scanning the keys.
; ---------------------------------------------------------------------
KbDisable	PROC
		xor_	cx, cx			; maximum wait for IBE
		call	KbWaitIbe
		jnz	.leaveFunction
		call	KbWaitIbe_Cli
		jnz	.leaveFunction

		mov	al, KBC_CMD_DISABLE
		out	PORT_KBC_CMD, al
		sti
		call	KbWaitIbe

.leaveFunction	retn
		ENDPROC	KbDisable

ENDPROGRAM	KEYBOARD
