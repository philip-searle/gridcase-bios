
POST12_InitKbc	PROC
		; Enter via fall-through from previous POST code

; ---------------------------------------------------------------------------
; Initialize keyboard controller
		mov	al, CHECKPOINT_KEYBOARD
		out	PORT_DIAGNOSTICS, al

		mov	al, 0F0h		; why this value?
		mov	[InterruptFlag], al

		; Reset keyboard controller
		in	al, PORT_KBC_DATA	; clear data ready flag
		call	KbWaitReady
		mov	al, KBC_CMD_RESET
		out	PORT_KBC_CMD, al	; reset keyboard controller

		; Wait for keyboard controller to response
		call	KbWaitReady
		in	al, PORT_KBC_DATA	; clear data ready flag
		mov	dx, KBC_RESET_TIMEOUT	; long delay while KBC resets itself
		call	KbWaitResponse
		jnz	.kbcReset

.kbcInitFail	cli				; prevent FatalBeeps being interrupted
		mov	al, BEEP_KBC
		jmp	FatalBeeps

		; Verify OK response from KBC
.kbcReset	in	al, PORT_KBC_DATA	; read KBC reset response
		cmp	al, KBC_RESET_OK	; keyboard controller reports success?
		jnz	.kbcInitFail		; report error if not

		; Setup KBC mode
		mov	al, KBC_CMD_WRITE	; next write to data port is command
		out	PORT_KBC_CMD, al
		call	KbWaitReady
		jnz	.kbcInitFail
		mov	al, 5Ch			; KBC command:
						;   convert set 2 scancodes to set 1 (PC compatibility mode)
						;   disable keyboard
						;   override inhibit keyswitch
						;   system flag (mark as warm boot)
						;   disable IRQ1 interrupt on KBC IBF
		out	PORT_KBC_DATA, al

		; Read KBC data port and store in BDA?
		call	KbWaitReady
		mov	al, KBC_CMD_READ	; read input port
		out	PORT_KBC_CMD, al
		mov	dx, KBC_SHORT_TIMEOUT	; wait for command to be processed
		call	KbWaitResponse
		jz	.kbcInitFail
		in	al, PORT_KBC_DATA
		and	al, 0F0h		; isolate upper nibble
		mov	[InterruptFlag], al

		; Exit via fall-through to next POST procedure
		ENDPROC POST12_InitKbc

