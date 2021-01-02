
POST08_InitBda	PROC
		; Enter via fall-through from previous POST code

; ---------------------------------------------------------------------------
; Initialize BIOS data area
; Setup keyboard buffer, detect parallel and serial ports.
.memTestFin	cmp	bx, SOFT_RESET_FLAG
		jnz	.l14
		mov	[SoftResetFlag], bx	; reset soft reset flag?x

		; Initialize keyboard ring buffer
.l14		mov	ax, kbBuffer
		mov	[kbNextChar], ax
		mov	[kbLastChar], ax
		mov	[kbBufStart], ax
		add	ax, KB_BUFFER_LENGTH
		mov	[kbBufEnd], ax

		mov	[errorCodes], 100h	; what does this value mean?

		; Initialize comms port timeouts, two at a time
		mov	ax, (DEFAULT_PARALLEL_TIMEOUT << 8) | DEFAULT_PARALLEL_TIMEOUT
		mov	[parPort1Timeout], ax
		mov	[parPort3Timeout], ax
		mov	ax, (DEFAULT_SERIAL_TIMEOUT << 8) | DEFAULT_SERIAL_TIMEOUT
		mov	[serPort1Timeout], ax
		mov	[serPort3Timeout], ax

		; Detect serial and parallel ports and record the base I/O
		; ports of any that are found in the BIOS data area.
		mov	si, ser1BasePort
		mov	dx, PORT_SER1_LCR
		call	SerDetectPort
		mov	dx, PORT_SER2_LCR
		call	SerDetectPort
		mov	dx, PORT_SER3_LCR
		call	SerDetectPort
		mov	dx, PORT_SER4_LCR
		call	SerDetectPort

		mov	bx, par1BasePort
		mov	dx, PORT_PAR_CANDIDATE1
		call	ParDetectPort
		mov	dx, PORT_PAR_CANDIDATE2
		call	ParDetectPort
		mov	dx, PORT_PAR_CANDIDATE3
		call	ParDetectPort

		; Exit via fall-through to next POST procedure
		ENDPROC POST08_InitBda

