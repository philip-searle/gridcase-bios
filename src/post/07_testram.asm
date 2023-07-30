
POST07_TestRam	PROC
		; Enter via fall-through from previous POST code

; ---------------------------------------------------------------------------
; Verify RAM refresh is working
		mov	al, CHECKPOINT_RAM_REFRESH
		out	PORT_DIAGNOSTICS, al

		mov	dx, PORT_KBC_PORTB
		mov	cx, 1000h		; Max loop iterations; RAM refresh toggle should
						; change state twice long before this many loops
		mov	bl, 3			; predecrement, so CX=3 for two loops

		in	al, dx			; read KBC port B
		and	al, 10h			; isolate RAM refresh toggle
.testRamRefresh	dec	bl
		jz	.ramRefreshOk		; detected three toggles? all is well then

		mov_	ah, al			; store last read of refresh toggle
.pollRamRefresh	in	al, dx			; read KBC port B
		and	al, 10h			; isolate RAM refresh toggle
		cmp_	ah, al			; changed from previous read?
		jnz	.testRamRefresh		; go through the test again if so
		loop	.pollRamRefresh

		; If we make it through 1000h polls without the RAM refresh toggle changing state
		; twice then it's probably broken - notify the user
		mov	al, BEEP_RAM_REFRESH
		jmp	FatalBeeps

; ---------------------------------------------------------------------------
; Verify RAM can be read/written
; TODO: figure out how this works in more detail...
.ramRefreshOk	mov	al, CHECKPOINT_RAM
		out	PORT_DIAGNOSTICS, al

		; Toggle parity check enables
		mov	al, 0Ch			; KBC port B: disable IO and RAM parity checks
		out	PORT_KBC_PORTB, al
		mov	al, 0			; KBC port B: enable IO and RAM parity checks
		out	PORT_KBC_PORTB, al

		; Load soft reset flag.  If we encounter a parity error while
		; doing this then assume the flag was zero (the parity error
		; will be caught by the following tests, so no need to report
		; it right now).
		mov	ds, [cs:kBdaSegment]
		mov	bx, [SoftResetFlag]
		in	al, PORT_KBC_PORTB
		test	al, 80h			; RAM parity error occurred?
		jz	.l1			; continue if not
		xor_	bx, bx			; if so, don't count it as a soft reset
		mov	al, 0Ch			; toggle IO and RAM parity checks
		out	PORT_KBC_PORTB, al
		mov	al, 0
		out	PORT_KBC_PORTB, al

.l1		mov	ax, cs			; load a fake return stack to handle
		mov	ss, ax			; the checking the first 64KB of RAM
		mov	sp, .returnStack1
		jmpn	DetectMemController

.loc_F8387	jnb	.memc1M			; TODO: where does this come from?
.memc256K	mov	dx, PORT_PAR_PORTC_R
		mov	al, 1
		out	dx, al			; TODO: parallel port C isn't writable though?

.memc1M		mov	ax, BDA_SEGMENT		; Set DS to BDA
		mov	ds, ax
		mov	ax, 0			; Set ES to first 64K
		mov	es, ax
		cmp	bx, SOFT_RESET_FLAG	; is it a soft reset?
		jz	.memTestClear		; skip memory test if so

		mov	ax, cs			; load a fake return stack to handle
		mov	ss, ax			; (TODO: what?)
		mov	sp, .returnStack2
		mov	bp, 8000h		; check 64KB at a time
		jmp	TestMemData

.loc_F83AC	jnb	.l5
		mov_	bx, ax
		mov	al, BEEP_RAM_OE
		cmp	dx, kOddEvenLogic
		jz	.memTestBeep
		mov	al, BEEP_RAM_ADDRESS_LINE
		cmp	dx, kAddressLine
		jz	.memTestBeep

.memTestBeepDet	xor_	bx, cx
		mov	cx, 10h
		mov	ax, 0FH
.l3		rol	bx, 1
		jnb	.l4
		inc	ah
		add_	al, cl
.l4		loop	.l3
		dec	ah
		jz	.memTestBeep
		mov	al, BEEP_RAM_MULTIPLE
.memTestBeep	jmp	FatalBeeps

.l5		jmp	TestMemLoAddr

.returnStack2	dw	.loc_F83AC
		dw	.loc_F83E4
.returnStack1	dw	.loc_F8387

.loc_F83E4	mov_	bx, ax
		jb	.memTestBeepDet

.memTestClear	xor_	ax, ax		; clear first 64K of RAM
		mov	es, ax
		xor_	di, di
		mov	cx, 8000h
		rep stosw

		mov	ax, 0
		mov	ss, ax
		mov	sp, 8000h
		mov	ds, [cs:kBdaSegment]

		mov	al, CHECKPOINT_RAM_PARITY
		out	PORT_DIAGNOSTICS, al

		call	CheckParityErr
		test	al, 80h			; RAM parity error occurred?
		jz	.memTestFin		; continue if not
		mov	al, BEEP_RAM_PARITY	; report error if so
		jmp	FatalBeeps

.memTestFin	; Exit via fall-through to next POST procedure
		ENDPROC POST07_TestRam

