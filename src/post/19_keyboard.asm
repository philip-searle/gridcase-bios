
POST19_Keyboard	PROC
		; Enter via fall-through from previous POST code

; ---------------------------------------------------------------------------
; Initialize and test keyboard interface

		; No point reporting keyboard errors if the user
		; can't see them, so skip if no display
		test	[InterruptFlag], 20h
		jnz	.l1
		jmp	.pastKeybInit

; ---------------------------------------------------------------------------
.l1		; Throughout this proc:
		;  - SI -> ASCIIZ string describing component being tested
		;          (or most recently failed component).
		;  - DI -> code to jump to after all tests are complete.

		mov	di, .kbIfTest1
		call	KbWaitReady
		mov	si, .kPartKbc
		jnz	.pastClockTest		; no point testing anything if controller isn't ready

; ---------------------------------------------------------------------------
		; Check clock line is in expected state
		mov	al, KBC_CMD_RD_TST
		out	PORT_KBC_CMD, al
		mov	dx, KBC_SHORT_TIMEOUT
		call	KbWaitResponse
		jz	.reportFailure
		in	al, PORT_KBC_DATA
		test	al, 1			; Keyboard clock line high?
		mov	si, .kPartClockLine
.pastClockTest	jnz	.reportFailure

; ---------------------------------------------------------------------------
		; Enable keyboard
		call	KbWaitReady
		mov	si, .kPartKbc
		jnz	.reportFailure
		mov	al, KBC_CMD_ENABLE
		out	PORT_KBC_CMD, al
		mov	dx, KBC_SHORT_TIMEOUT
		call	KbWaitResponse

; ---------------------------------------------------------------------------
		; Reset keyboard (not controller!) and wait for it to respond with
		; Basic Assurance Test (BAT) success.  Repeat reset if keyboard
		; responds with 'resend'.  Allow for one 'acknowledge' response,
		; as long as it is immediately followed by a BAT success.
		mov	bl, 0			; BL stores 'acknowledge' response (if any)
		mov	dx, KB_RESET_TIMEOUT
.resetKeyboard	call	KbWaitReady
		mov	si, .kPartKbc
		jnz	.reportFailure
		mov	al, KB_CMD_RESET
		out	PORT_KBC_DATA, al
		mov	si, .kPartKeyboard

.waitKbReset	call	KbWaitResponse
		mov	ch, KB_RESET_REPLY_DELAY
.waitKbReply	Delay	2
		loop	.waitKbReply
		jz	.kbNoResponse
		test	al, KBC_STATUS_TxTO
		in	al, PORT_KBC_DATA
		jz	.kbCmdSent

		; timeout? might have got an OK response, retry reset if not
		cmp	al, KB_REPLY_BAT_OK
		jz	.kbResetOk
		jmp	.retryKbReset

		; no timeout? handle various possible responses
.kbCmdSent	cmp	al, KB_REPLY_RESEND
		jnb	.resetKeyboard
		cmp	al, KB_REPLY_BAT_OK
		jz	.kbResetOk
		cmp	bl, KB_REPLY_ACK	; already had an ack? fail if so
		jz	.reportFailure
		mov_	bl, al			; store ack for next loop

.retryKbReset	jmp	.waitKbReset

; ---------------------------------------------------------------------------
; Handle reporting keyboard failures of various types
.kbNoResponse	cmp	bl, 0			; have we seen an 'acknowledge' response from the keyboard?
		jnz	.reportFailure		; if so, just report failure
		mov	di, .kbIfTest2		; if no, ???

.reportFailure	Inline	ConString,'Keyboard ',0
		call	ConString		; write failed component (in SI)
		Inline	ConString,' failure',0Dh,0Ah,0
		jmp	di			; ???

; ---------------------------------------------------------------------------
; String constants interpolated into error message above
.kPartKbc	db	'controller'		; reuses nul from next constant
.kPartKeyboard	db	0			; empty string, results in "keyboard failure" message
.kPartClockLine	db	'clock line',0
.kPartDataLine	db	'data line',0
.kPartStuckKey	db	'stuck key',0

; ---------------------------------------------------------------------------
; Keyboard has been reset, now check for stuck keys
.kbResetOk	mov	si, .kPartStuckKey
		mov	dx, KBC_STUCK_KEY_TIMEOUT
		mov	ah, 4			; read four scancodes before declaring stuck key
.checkStuckKey	call	KbWaitResponse
		jz	.noStuckKeys
		in	al, PORT_KBC_DATA	; read and discard scancode
		dec	ah
		jnz	.checkStuckKey
.l2		jmp	.reportFailure

; ---------------------------------------------------------------------------
; After reporting failure, this section attempts to get more details about
; the exact error.
.kbIfTest1	call	SetCriticalErr
.kbIfTest2	mov	di, .kbIfTested		; just continue after reporting these errors
		mov	si, .kPartKbc
		call	KbWaitReady
		jnz	.l2			; timeout? blame KBC
		mov	al, KBC_CMD_IFTEST
		out	PORT_KBC_CMD, al	; ask KBC to check keyboard interface
		mov	dx, KBC_IFTEST_TIMEOUT
		call	KbWaitResponse
		jz	.l2			; KBC didn't complete interface test?  declare it bad
		in	al, PORT_KBC_DATA	; read interface test result
		cmp	al, 0
		jz	.noStuckKeys		; no error detected? carry on then...
		mov	si, .kPartClockLine
		cmp	al, 3
		jb	.l2			; errors 1/2 == clock line stuck lo/hi
		mov	si, .kPartDataLine
		jmp	.l2			; errors 3/4 == data line stuck lo/hi

; ---------------------------------------------------------------------------
; We end up here after running the KB interface test above.
.kbIfTested	call	SetCriticalErr
		; fall through into .noStuckKeys outcome

; ---------------------------------------------------------------------------
; Finalize keyboard init
.noStuckKeys	mov	al, KBC_CMD_WRITE
		out	PORT_KBC_CMD,al		; write to command byte
		call	KbWaitReady
		mov	al, 45h			; KBC: enable IRQ1 interrupt, set SYS flag (warm boot)
						;      enable keyboard, convert set 2 scancodes to set 1
		out	PORT_KBC_DATA, al

.pastKeybInit	; Exit via fall-through to next POST procedure
		ENDPROC POST19_Keyboard

