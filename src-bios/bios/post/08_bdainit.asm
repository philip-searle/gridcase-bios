
POST08_BdaInit	PROC
		; Enter via fall-through from previous POST code

; ---------------------------------------------------------------------------
; Initialize BIOS data area
; Setup keyboard buffer, detect parallel and serial ports.
.memTestFin	cmp	bx, SOFT_RESET_FLAG
		jnz	.l1
		mov	[SoftResetFlag], bx	; reset soft reset flag

		; Initialize keyboard ring buffer
.l1		mov	ax, KbBuffer
		mov	[KbNextChar], ax
		mov	[KbLastChar], ax
		mov	[KbBufStart], ax
		add	ax, KB_BUFFER_LENGTH
		mov	[KbBufEnd], ax

		mov	[KbClickVol], DEFAULT_KBCLICK_VOL

		; Initialize comms port timeouts, two at a time
		mov	ax, (DEFAULT_PARALLEL_TIMEOUT << 8) | DEFAULT_PARALLEL_TIMEOUT
		mov	[ParPort1Timeout], ax
		mov	[ParPort3Timeout], ax
		mov	ax, (DEFAULT_SERIAL_TIMEOUT << 8) | DEFAULT_SERIAL_TIMEOUT
		mov	[SerPort1Timeout], ax
		mov	[SerPort3Timeout], ax

		; Detect serial and parallel ports and record the base I/O
		; ports of any that are found in the BIOS data area.
		mov	si, Ser1BasePort
		mov	dx, PORT_SER1_LCR
		call	SerDetectPort
		mov	dx, PORT_SER2_LCR
		call	SerDetectPort
		%IF	BIOS_VERSION > 19880912
			; 1988 BIOS only supports two serial ports
			mov	dx, PORT_SER3_LCR
			call	SerDetectPort
			mov	dx, PORT_SER4_LCR
			call	SerDetectPort
		%ENDIF

		mov	bx, Par1BasePort
		mov	dx, PORT_PAR_CANDIDATE1
		call	ParDetectPort
		mov	dx, PORT_PAR_CANDIDATE2
		call	ParDetectPort
		mov	dx, PORT_PAR_CANDIDATE3
		call	ParDetectPort

		; Exit via fall-through to next POST procedure
		ENDPROC POST08_BdaInit
