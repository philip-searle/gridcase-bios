
; PC/AT BIOS technically supported four parallel ports, but the GRiD
; 1520 only implements I/O ports for the first one.  Expansion hardware
; could, in theory, decode the additional I/O port addresses although
; I'm not aware of any that ever did.
MAX_PRINTER_PORTS	equ	4

; ---------------------------------------------------------------------
; Int17_Actual [TechRef 11-2]
; (Very) basic parallel printer interface.
; ---------------------------------------------------------------------
Int17_Actual	PROC
		; BIOS parallel printer routines don't use the parallel
		; port interrupt, so turn interrupts back on to keep
		; the system responsive
		sti
		call	MakeIsrStack

		cmp	dx, MAX_PRINTER_PORTS
		jnb	.leaveFunction

		mov	di, .handlersLast - .handlers
		call	FuncToOffset
		jc	.leaveFunction

		; Load timeout and base I/O port for requested printer
		; into CL and DX.  The remainder of this function will
		; use inc/dec to switch between the three I/O ports
		; rather than reload the base port each time.
		mov_	bx, dx
		mov	cl, [bx+ParPort1Timeout]
		shl	bx, 1
		mov	dx, [bx+Par1BasePort]

		; Does the parallel port even exist?
		or_	dx, dx
		jz	.leaveFunction

		inc	dx		; increment to status port
		xor_	ch, ch		; CH used for timeout flag

		; Dispatch via jump table
		jmp	[cs:di+.handlers]

.handlers	dw	.printChar
		dw	.initPrinter
.handlersLast	dw	.printerStatus

; ---------------------------------------------------------------------
; Sends a single character to the printer
.printChar	mov_	ah, al		; save character in AH for later

		; Check whether the printer is ready to accept data
		in	al, dx		; read printer status
		in	al, dx		; ??? why read it twice
		; Isolate and invert ~BUSY bit
		and	al, PRINTER_STATB_BUSY
		xor	al, PRINTER_STATB_BUSY
		jz	.notBusy

		; Notify any OS that the printer is busy
		push	ax
		mov	ax, 90FEh	; OS hook - device busy, printer
		int	15h
		pop	ax

.waitNotBusy	; Poll the printer until it's not busy
		xor_	bx, bx
.waitNotBusy2	in	al, dx		; read printer status
		in	al, dx		; ??? why read it twice
		test	al, PRINTER_STATB_BUSY
		jnz	.notBusy
		dec	bx
		jnz	.waitNotBusy2
		loop	.waitNotBusy

		; Polling loop finished but printer still busy.
		; Flag timeout for later use.
		inc	ch
		jmp	.writeDone

.notBusy	dec	dx		; decrement to data port
		mov_	al, ah		; restore byte to print
		out	dx, al		; send data to printer
		inc	dx		; increment to printer control port
		inc	dx
		; Pulse printer strobe pin
		mov	al, PRINTER_CTRL_SLCTIN | PRINTER_CTRL_INIT | PRINTER_CTRL_STROBE
		Delay	2
		out	dx, al
		Delay	2
		xor	al, PRINTER_CTRL_STROBE
		out	dx, al
		dec	dx		; decrement to status port

.writeDone	mov_	al, ah		; preserve AL for int return

; ---------------------------------------------------------------------
; Read status from printer and return in AL
.printerStatus	xchg	ax, bx

.buildStatus	; Read printer status, mask off non-standard bits, and
		; invert any negative-logic bits, and return in AL.
		in	al, dx		; read printer status
		in	al, dx		; ??? why read it twice
		and	al, ~PRINTER_STATB_GRIDBITS
		xor	al, PRINTER_STATB_ACK | PRINTER_STATB_ERROR
		mov_	bh, al
		or_	bh, ch		; merge in timeout flag
		xchg	ax, bx


; ---------------------------------------------------------------------
; Shared function tail
.leaveFunction	jmp	UnmakeIsrStack

; ---------------------------------------------------------------------
; Tell the printer to reset itself
.initPrinter	mov_	bl, al		; preserve AL for later
		inc	dx		; increment to control port
		mov	al, PRINTER_CTRL_SLCTIN
		out	dx, al		; assert init line, deselect printer
		; Give the printer plenty of time to reset
		mov	cx, 1029h
		loop	$
		; Deassert init line, select printer
		xor	al, PRINTER_CTRL_INIT
		out	dx, al
		dec	dx		; decrement to status port
		jmp	.buildStatus
		ENDPROC	Int17_Actual

