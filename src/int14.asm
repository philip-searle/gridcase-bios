
INT14		PROGRAM	OutFile=build/int14.obj

		include	"macros.inc"
		include	"segments/bda.inc"

		EXTERN	MakeIsrStack, UnmakeIsrStack
		EXTERN	FuncToOffset
		EXTERN	BaudRateInit

		PUBLIC	Int14_Actual
		PUBLIC	SerDetectPort

MAX_SERIAL_PORTS	equ	4

; ===========================================================================
; Int14_Actual [TechRef 12-2]
; Provides int14 serial communications services.
;
; On entry:
; 	AH == function number
; 	AL == function data
; 	DX == port number (zero based)
; ===========================================================================
Int14_Actual	PROC
		; Allow us to be interrupted, we're not doing anything too
		; critical here (not strictly necessary since MakeIsrStack
		; does this as well).
		sti
		call	MakeIsrStack

		; Range check arguments
		cmp	dx, MAX_SERIAL_PORTS
		jnb	SerStatus.sharedLeave

		mov	di, .handlersLast - .handlers
		call	FuncToOffset
		jb	SerSend.leaveSuccess	; BUG? Unsupported function should
						; set CF, not clear the upper bit of AH?

		; Load base port address and timeout from BDA
		mov_	bx, dx
		mov	cl, [bx+SerPort1Timeout]
		xor_	ch, ch
		shl	cx, 1
		shl	cx, 1
		shl	bx, 1
		mov	dx, [bx+Ser1BasePort]
		or_	dx, dx		; port present?
		jz	SerStatus.sharedLeave

		; BX is our scratch register, clear it up front
		xor_	bx, bx

		; Increment port to Modem Control register
		add	dx, 4

		; Dispatch via jump table
		jmp	[cs:di+.handlers],DATA=WORD

.handlers	dw	SerInit
		dw	SerSend
		dw	SerReceive
.handlersLast	dw	SerStatus
		ENDPROC	Int14_Actual

; ===========================================================================
; SerInit [12-3]
; Initialize a serial I/O port.
;
; On entry:
; 	AL == initialization parameters
; 	DX == serial port Modem Control register
; 	Stack layout from MakeIsrStack
; ===========================================================================
SerInit		PROC
		mov_	ah, al
		dec	dx		; change port to Line Control register

		; Set baud rate
		mov_	bl, ah
		and	bl, 0E0h	; isolate baud rate bits
		mov	cl, 4
		shr	bl, cl		; convert to offset into baud rate table
		mov	bx, [cs:bx+BaudRateInit]
		call	SerSetDivisor

		; Set parity/stop/data bit settings
		mov_	al, ah
		and	al, 01Fh	; isolate parity/stop/data bits
					; clear DLAB bit set by SerSetDivisor
		out	dx, al
		inc	dx		; return port to Modem Control register
		Delay	2

		; Disable all serial interrupt sources.
		; [TechRef 12-5] lists the I/O registers in an inconsistent
		; order (DLAE column does not alternate 0/1).  A careless
		; reading might lead one to think this code is dealing with
		; the Baud Divisor Latch MDB register, which makes no sense
		; at all.  It actually deals with the Interrupt Enable register.
		sub	dx, 3		; change port to Interrupt Enable register
		xor_	al, al
		out	dx, al
		add	dx, 3		; return port to Modem Control register
		Delay	2

		; fall-through to SerStatus
		ENDPROC	SerInit

; ===========================================================================
; SerStatus [TechRef 12-3]
; Returns contents of Line Status register.
;
; On entry:
; 	DX == serial port Modem Control register
; 	Stack layout from MakeIsrStack
;
; On return:
; 	AH = line status
; 	AL = modem status
; ===========================================================================
SerStatus	PROC
		inc	dx		; change port to Line Status register
		in	al, dx
		xchg	ah, al		; place line status in AX

		inc	dx		; change port to Modem Status register
		Delay	2

.sharedLeave2	; Shared function tail for Int14 code that needs to
		; read the port in DX before returning
		in	al, dx		; place modem status in AL

.sharedLeave	; Shared function tail for Int14 code
		jmp	UnmakeIsrStack
		ENDPROC	SerStatus

; ===========================================================================
; SerSend [TechRef 12-3]
; On entry:
; 	AL == character to send
; 	DX == serial port Modem Control register
; 	Stack layout from MakeIsrStack
; ===========================================================================
SerSend		PROC
		mov_	ah, al
		mov	al, 3		; set DTR and RTS
		out	dx, al

		inc	dx		; change port to Modem Status register
		inc	dx
.waitDsrCts	in	al, dx		; read modem status
		and	al, 30h		; isolate DDSR and DCTS bits
		cmp	al, 30h		; receiver ready for us?
		jnz	.noDsrCts

		; Receiver is ready for us to send
		dec	dx		; change port to Line Status register
.waitXmitEmpty2	xor_	bx, bx
.waitXmitEmpty	in	al, dx		; read line status
		test	al, 20h		; isolate xmit register empty bit
		jz	.xmitRegFull
		sub	dx, 5		; change port to Transmit Holding register
		xchg	al, ah
		out	dx, al		; transmit the character

.leaveSuccess	; Shared function tail for Int14 code
		and	ah, 7Fh		; clear error bit
		jmp	SerStatus.sharedLeave

.xmitRegFull	dec	bx		; decrement timeout count
		jnz	.waitXmitEmpty
		loop	.waitXmitEmpty2
		jmp	.xmitRecvErr

.noDsrCts	dec	bx		; decrement timeout count
		jnz	.waitDsrCts
		loop	.waitDsrCts
		dec	dx		; change register to Line Status register
					; fallthrough

.xmitRecvErr	; Shared function tail for Int14 xmit/recv error
		in	al, dx		; read line status
		xchg	ah, al
		and	ah, 7Fh
		or	ah, 80h		; set error bit
		jmp	SerStatus.sharedLeave
		ENDPROC	SerSend

; ===========================================================================
; SerReceive [TechRef 12-3]
; On entry:
; 	DX == serial port Model Control register
; 	Stack layout from MakeIsrStack
; On return:
; 	AL == character read
; ===========================================================================
SerReceive	PROC
		mov	al, 1		; Set DTR, clear RTS
		out	dx, al

		inc	dx		; change port to Modem Status register
		inc	dx
.waitDsr	in	al, dx		; read modem status
		test	al, 20h		; test DSR bit
		jnz	.haveDsr
		dec	bx		; decrement timeout count
		jnz	.waitDsr
		loop	.waitDsr

		; Failed to see DSR set in time
		dec	dx		; change port to Line Status register
		jmp	.recvErr

.haveDsr	dec	dx		; change port to Line Status register
.waitData2	xor_	bx, bx
.waitData	in	al, dx		; read line status
		test	al, 1		; byte received?
		jz	.noDataYet

		; Byte received
		mov_	ah, al		; line status to AH
		and	ah, 7Fh		; clear error bit
		and	ah, 9Eh		; mark data available
		sub	dx, 5		; change port to Reciever Buffer register
		jmp	SerStatus.sharedLeave2

.noDataYet	dec	bx		; decrement timeout count
		jnz	.waitData
		loop	.waitData2

.recvErr	xor_	ah, ah
		jmp	SerSend.xmitRecvErr
		ENDPROC	SerReceive

; ===========================================================================
; SerSetDivisor
;
; On entry:
; 	DX == serial port Line Control register
; 	BX == baud divisor latch register value
;
; On return:
; 	DX == unchanged
; 	DI == baud divisor latch LSB register port number
; 	Serial port DLAB bit set so Baud Divisor Latch registers are visible
; ===========================================================================
SerSetDivisor	PROC
		mov	al, 80h		; set line control register:
					;   5 bit word length
					;   1 stop bit
					;   parity disabled
					;   break control disabled
					;   divisor latch access enabled
		out	dx, al

		mov_	di, dx		; save original port number
		dec	dx		; change port to Baud Divisor MSB register
		dec	dx
		mov_	al, bh
		Delay	2
		out	dx, al		; write MSB
		dec	dx		; change port to Baud Divisor LSB register
		mov_	al, bl
		Delay	2
		out	dx, al		; write LSB
		xchg	di, dx		; restore DX
		retn
		ENDPROC	SerSetDivisor

; ===========================================================================
; SerDetectPort
; Detects whether a serial port exists with the specified Line Control
; register port number.  If it does, then its IO port base address is
; written to the address in SI and the number of serial ports in the BDA is
; incremented.  The serial port will need resetting after this method is
; called as the detection process changes the baud rate divisor.
;
; On entry:
; 	DX == serial port Line Control register
; 	SI -> serial port IO base address word
;
; On return (if serial port detected):
; 	SI advanced by two
; 	Count of serial ports in BDA equipment word incremented
; ===========================================================================
SerDetectPort	PROC
		mov	bx, 1		; set maximum baud divisor
		call	SerSetDivisor	; also sets DI to baud divisor register number

		xor_	al, al		; set potential serial port to 'safe' settings
		out	dx, al		; line control register:
					;   5/1/N, break control and interrupt disabled
					;   baud divisor latch registers hidden
					; DI now set to xmit holding register number
		Delay	2

		xchg	dx, di
		out	dx, al		; clear xmit holding register

		xchg	dx, di
		mov	bl, 0AAh	; set test pattern to baud divisor registers
		call	SerSetDivisor
		xchg	di, dx
		in	al, dx		; read back baud divisor LDB
		cmp_	al, bl		; does it match?
		jnz	.done

		; If register can be set, assume port exists and increment serial port count
		mov	[si], dx	; store IO port base
		add	[EquipmentWord+1], 2,DATA=BYTE
		inc	si		; advance pointer
		inc	si

.done		retn
		ENDPROC	SerDetectPort

ENDPROGRAM	INT14
