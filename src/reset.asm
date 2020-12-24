
; Number of times to toggle the speaker for a fatal beep
FATAL_BEEP_LENGTH	equ	300

; ===========================================================================
; FatalBeeps
;
; Produces three groups of beeps using the value in AL, outputs AL to the
; diagnostic port, and then halts the CPU and does not return.
; Can be called before the programmable timer is initialized so we have to
; bit-bang the speaker by hand.
;
; On entry:
;   AL == Beep code: 8-bit value "xxaabbcc"
;         xx = Don't care
;         aa = Count of beeps minus one, first group
;         bb = Count of beeps minus one, second group
;         cc = Count of beeps minus one, second group
; ===========================================================================
FatalBeeps	PROC
		out	PORT_DIAGNOSTICS, al
		jmp	.l1
		nop				; Mystery nop to jump over?
.l1		mov	bh, [cs:BeepFactor]	; cs override because ds may not be valid
		mov	dh, al,CODE=LONG	; move beep code to dh; al will be used for port writes
		mov	al, 0Ch			; setup KBC port B write: both parity checks disabled
		rol	dh, 2			; pre-rotate beep code for main loop
		mov	bl, 3			; three groups of beeps

; ---------------------------------------------------------------------------
.beepGroup	rol	dh, 2			; rotate next beep group to lower bits of dh
		mov	dl, dh,CODE=LONG	; copy beep group and isolate lower bits
		and	dl, 3
		inc	dl			; beep codes are stored excess-one so adjust for true value

; ---------------------------------------------------------------------------
.beep		mov	si, FATAL_BEEP_LENGTH
.l2		xor	al, 2			; toggle speaker
		out	PORT_KBC_PORTB, al

		mov	cl, bh,CODE=LONG	; loop 4xbeep factor between toggles
		mov	ch, 0
		add	cx, cx,CODE=LONG
		add	cx, cx,CODE=LONG
		loop	$			; 4 clock loop

		dec	si
		jnz	.l2

; ---------------------------------------------------------------------------
		mov	ch, bh,CODE=LONG	; delay 256xbeep factor between beeps
.beepGap	Delay	1
		loop	.beepGap

		dec	dl			; beep completed
		jnz	.beep			; loop if more in this group

; ---------------------------------------------------------------------------
		mov	ch, bh,CODE=LONG			; delay 1024xbeep factor between beep groups
.beepGroupGap	Delay	4
		loop	.beepGroupGap

		dec	bl			; beep group completed
		jnz	.beepGroup		; loop if not last group

		hlt				; halt computer
		ENDPROC	FatalBeeps

; ===========================================================================
; Reset handler
; Called immediately after startup to verify basic hardware functionality.
; Also handles CPU resets to get out of protected modes.
; Can be jumped to at any time (in real mode) to perform a system reset.
; ===========================================================================
Reset_Actual	PROC
		; Make sure we won't be interruted
		cli				; ensure interrupts are disabled (may not be the case for a warm boot)
		mov	al, NMI_DISABLE		; ensure NMIs are disabled
		out	PORT_CMOS_ADDRESS, al
		cld				; ensure direction flag is in known state

		; Extract shutdown reason (if any) from CMOS
		mov	ah, 0			; Assume no shutdown reason (cold boot)
		in	al, PORT_KBC_STATUS
		test	al, KBC_STATUS_SYS	; cold boot?
		jz	.l1			; if so, skip shutdown reason extraction
		mov	al, CMOS_SHUTDOWN_REASON | NMI_DISABLE
		out	PORT_CMOS_ADDRESS, al
		Delay	2
		in	al, PORT_CMOS_DATA	; read shutdown reason form CMOS
		mov	ah, al,CODE=LONG	; store it for later comparisons
.l1		Delay	2

		; Reset CMOS shutdown reason now we've got it
		mov	al, CMOS_SHUTDOWN_REASON | NMI_DISABLE
		out	PORT_CMOS_ADDRESS, al
		Delay	2
		mov	al, 0
		out	PORT_CMOS_DATA, al	; reset shutdown reason
		; no Delay needed: port not accessed immediately

		; Was the shutdown intended to get out of protected mode?
		; If so, skip the full POST and get back to running user code as fast as possible.
		; This test relies on all protected mode shutdown reasons being contiguous.
		cmp	ah, SD_BLOCK_MOVE
		ja	.l2
		cmp	ah, SD_PM_TEST7_PASS
		jnb	.afterPicInit
.l2
; ---------------------------------------------------------------------------
; Initialize NPU/FPU
		out	PORT_287RESET, al	; ensure 287 is in real mode

; ---------------------------------------------------------------------------
; Initialize PIC2
		mov	dx, PORT_PIC2_CTRL
		mov	al, 15h			; PIC2 ICW1: ICW4 needed, cascade mode, 80x86 mode,
						;            edge triggered mode, ICW1 being issued
		out	dx, al
		inc	dx			; Advance port to PORT_PIC2_MASK ready for ICW2-4
		mov	al, 70h			; PIC2 ICW2: IVT offset
		Delay	2
		out	dx, al
		mov	al, 2			; PIC2 ICW3: slave controller attached to interrupt pin 2
		Delay	2
		out	dx, al
		mov	al, 1			; PIC2 ICW4: non-buffered mode, no auto-eoi, 80x86 mode
		Delay	2
		out	dx, al
		mov	al, 0FFh		; PIC2 OCW1: disable all interrupt lines
		Delay	2
		out	dx, al
		; No delay needed; port not accessed immediately

; ---------------------------------------------------------------------------
; Initialize PIC1
		mov	dx, PORT_PIC1_CTRL
		mov	al, 15h			; PIC1 ICW1: ICW4 needed, cascade mode, 80x86 mode,
						;            edge triggered mode, ICW1 being issued
		out	dx, al
		inc	dx			; advance port to PORT_PIC2_MASK ready for ICW2-4
		mov	al, 8			; PIC1 ICW2: IVT offset
		Delay	2
		out	dx, al
		mov	al, 4			; PIC1 ICW3: slave controller attached to interrupt pin 2
		Delay	2
		out	dx, al
		mov	al, 1			; PIC1 ICW4: nonbuffered mode, no auto-eoi, 80x86 mode
		Delay	2
		out	dx, al
		mov	al, 0FBh		; PIC1 OCW1: enable IRQ video interrupt
		Delay	2
		out	dx, al

; ---------------------------------------------------------------------------
; Setup stack and data segment
.afterPicInit	mov	bx, STACK_SEGMENT
		mov	ss, bx			; interrupts inhibited until next instruction completes
		mov	sp, STACK_TOP
		mov	ds, [cs:kBdaSegment]

; ---------------------------------------------------------------------------
; Jump to appropriate handler for shutdown reason
		sub	ah, SD_BOOTLOADER_REQ	; adjust for jump table
		jb	SDH_POST
		cmp	ah, SD_JMP_WITHOUT_INT - SD_BOOTLOADER_REQ
		ja	SDH_POST
		mov	bl, ah,CODE=LONG
		shl	bx, 1
		jmp	[cs:.sdHandlers+bx]

.sdHandlers	dw	SDH_00
		dw	SDH_01
		dw	SDH_02
		dw	SDH_03
		dw	SDH_04
		dw	SDH_05
		dw	SDH_02

		ENDPROC	Reset_Actual

; ===========================================================================
; Shutdown handler for SD_JMP_WITH_INT
; ===========================================================================
SDH_01		PROC
		call	WaitKbEmpty
		mov	al, 20h			; Issue non-specific EOI to both PICs
		out	PORT_PIC2_CTRL, al
		out	PORT_PIC1_CTRL, al
		; FALLTHROUGH into sdh02
		ENDPROC	SDH_01

; ===========================================================================
; Shutdown handler for SD_PM_TEST7_PASS
; Also fallthrough from previous handler
; ===========================================================================
SDH_02		PROC
		jmpf	[AdapterRomOffset]
		ENDPROC	SDH_02

; ===========================================================================
; Shutdown handler for SD_BLOCK_MOVE
; ===========================================================================
SDH_05		PROC
		mov	ss, [AdapterRomSegment]	; restore temporary stack
		mov	sp, [AdapterRomOffset]
		call	DisableA20		; no A20 needed now we're back in real mode
		mov	al, NMI_ENABLE		; NMI can be turned on since we're running user code again
		out	PORT_CMOS_ADDRESS, al

		pop	ds			; restore registers from temp stack
		pop	es
		popa

		xchg	al, ah
		in	al, PORT_DIAGNOSTICS	; fetch return value from scratch port
		xchg	al, ah			; move it into AH
		iret				; return from int15, user code will never know
						; the contortions we went through to move their data
		ENDPROC	SDH_05

; ===========================================================================
; Shutdown handler for non-recoverable shutdowns
; i.e. actual POST rather than a reboot to get out of protected mode.
; ===========================================================================
SDH_POST	PROC
		; What are all these mystery ports doing?
		mov	dx, PORT_UNKNOWN_426
		mov	al, 1			; set port 426 to 1
		out	dx, al
		mov	bl, 5			; delay for 5x64k nop loops
		xor	cx, cx,CODE=LONG
.l1		nop
		loop	.l1
		dec	bl
		jnz	.l1
		mov	dx, PORT_UNKNOWN_426	; unnecessary reload of dx?
		mov	al, 0			; set port 426 to 0
		out	dx, al
		mov	dx, PORT_UNKNOWN_FFF
		mov	al, 1			; set port FFF to 1
		out	dx, al
		mov	dx, PORT_UNKNOWN_6F8
		mov	al, 0			; set port 6F8 to 0
		out	dx, al
		mov	dx, PORT_UNKNOWN_7FD
		mov	al, 0			; set port 7FD to 0
		out	dx, al

; ---------------------------------------------------------------------------
; Check for undocumented shutdown reason 0FCh -- maybe BIOS password?
		cmp	ah, SD_UNKNOWN_FC
		jz	.l2
		mov	[ds:SoftResetFlag], 0	; mark it as not a soft reset?

; ---------------------------------------------------------------------------
; Disable NMI checks before we start testing things
.l2		mov	al, 0Ch
		out	PORT_KBC_PORTB, al

; ---------------------------------------------------------------------------
; CPU register test
; Seems to be left over from the original Phoenix BIOS, since we've already
; used a bunch of registers checking for protected mode recovery.  I suppose
; it might catch a CPU that's bad in one or two bits we've not touched yet...
		mov	al, CHECKPOINT_CPU_REGS
		out	PORT_DIAGNOSTICS, al

		mov	ax, 55AAh		; load alternating bit pattern to test registers
.testRegisters	mov	ss, ax			; cycle the test pattern through all registers
		mov	si, ss
		mov	bx, si,CODE=LONG
		mov	ds, bx
		mov	di, ds
		mov	cx, di,CODE=LONG
		mov	es, cx
		mov	bp, es
		mov	dx, bp,CODE=LONG
		mov	sp, dx,CODE=LONG
		cmp	ax, sp,CODE=LONG	; did the test pattern survive all register transfers?
		jz	.registersOk		; if so, continue with POST
		hlt				; if not, we can't continue.
						; since interrupts are disabled the system is soft-locked
						; and the user will see an apparently 'dead' computer

.registersOk	not	ax			; invert the test pattern
		cmp	ax, 55AAh		; do we have the original value (i.e. inverted twice)?
		jnz	.testRegisters		; if not, redo the test to check registers can hold inverted bits

; ---------------------------------------------------------------------------
; Disable video output on both MDA and CGA adaptors
		mov	al, 11h			; MDA: high resolution, disable display
						; CGA: 80x25 text mode, video disabled
		mov	dx, PORT_MDA_MODE_CTRL	; disable display until 6845 is initialized; the
						; IBM 5151 monitor can be damaged by out of range HSYNC input
						; (I know the GRiD 1520 doesn't have an MDA-compatible output,
						; but this is probably from the generic Phoenix BIOS)
		out	dx, al

		; Reuse the port number in DX and the value in AL to disable the CGA
		; adapter (upper byte of CGA port number is the same as the MDA port)
		mov	dl, PORT_CGA_MODE_CTRL & 0FFh
		out	dx, al

		; Delay loop (why is this needed? where did the loop count come from?)
		; Note that CL still has AAh in from the CPU test, so this actually
		; loops 51,370 times instead of 51,200.
		mov	ch, 200
		loop	$

; ---------------------------------------------------------------------------
; CMOS register read/write test
; Rotate a single bit through the shutdown reason register to test CMOS RAM.
		mov	al, CHECKPOINT_CMOS_RW
		out	PORT_DIAGNOSTICS, al

		mov	ah, 1			; set a single bit in the test pattern

		; Write test pattern to CMOS
.testCmosRam	mov	al, CMOS_SHUTDOWN_REASON | NMI_DISABLE
		out	PORT_CMOS_ADDRESS, al
		Delay	2
		mov	al, ah,CODE=LONG
		out	PORT_CMOS_DATA, al
		Delay	2

		; Read back test pattern from CMOS
		mov	al, CMOS_SHUTDOWN_REASON | NMI_DISABLE
		out	PORT_CMOS_ADDRESS, al
		Delay	2
		in	al, PORT_CMOS_DATA

		cmp	al, ah,CODE=LONG	; does it match?
		jz	.l3			; keep going if so
		mov	al, BEEP_CMOS_FAIL	; if not, probable stuck bits in CMOS RAM
		jmp	FatalBeeps

.l3		rol	ah, 1			; shift bit and try again
		jnb	.testCmosRam		; (until the bit falls out into the carry)

; ---------------------------------------------------------------------------
; Checksum BIOS ROM content
		mov	al, CHECKPOINT_BIOS_CHECKSUM
		out	PORT_DIAGNOSTICS, al

		; ChecksumRom routine expects a near call, so simulate one
		; with a hardcoded return stack in ROM and a JMP (SS:SP is
		; set but we haven't yet run the RAM test or started the DMA
		; controller refreshing DRAM yet, so we can't use a real stack).
		mov	cx, BIOS_START_PARA	; CX is checksum start address
		mov	ax, cs
		mov	ss, ax
		mov	sp, .returnStack1
		jmp	ChecksumRom

.returnStack1	dw	.checksumDone

.checksumDone	jz	.checksumOk		; is our EPROM OK?
		mov	al, BEEP_BIOS_CHECKSUM
		jmp	FatalBeeps

; ---------------------------------------------------------------------------
; Initialize programmable interval timer
.checksumOk	mov	al, CHECKPOINT_TIMER
		out	PORT_DIAGNOSTICS, al

		; Start RAM refreshing from 0 when timer starts
		mov	al, 0
		out	PORT_DMA_RAM_REFRESH_ADDRESS, al

		; Initialize channel 1
		mov	dx, PORT_PIT_MODE
		mov	al, 54h			; PIT mode: counter 1 select, lobyte only access mode,
						;           mode 2 select (rate generator), 16bit counter
		out	dx, al

		mov	ss, [cs:kBdaSegment]	; why reset SS here? SP is still set from the ROM checksum earlier...
		pusha				; why pusha here?

		dec	dx			; adjust port downwards to PORT_PIT_COUNTER1
		dec	dx
		mov	al, 12h			; PIT channel 1: divisor = 12h (18)
		out	dx, al

		; Initialize channel 0
		inc	dx			; adjust port back to 43h (PORT_PIT_MODE)
		inc	dx
		mov	al, 36h			; PIT mode: counter 0 select, lobyte/hibyte access mode,
						;           mode 3 (square wave generator), 16bit counter
		out	dx, al

		dec	dx			; adjust port downwards to PORT_PIT_COUNTER0
		dec	dx
		dec	dx
		mov	al, 0			; maximum divisor for counter 0: (1)0000h == 18.2Hz clock tick
		out	dx, al			; write lobyte of divisor

		xchg	bh, bl			; possible bug? meant to exchange AL,AH instead but still works because AX is 0?
		xor	cx, cx,CODE=LONG	; setup maximum loop count for timer test below
		Delay	1			; only one jmp needed because of xchg/xor above?
		out	dx, al

		; Test channel 0 is counting before we use it for RAM refresh.
		; No RAM refresh setup yet so interrupts can't be used. To test the timer, we
		; repeatedly latch it's value, using it to set bits in BH and clear bits in
		; BL. If we loop more than FFFFh times without encountering each bit at least
		; once then we assume the timer has failed.
		; Note that BX still contains AA55h from the CPU register test above, so the
		; AND/OR on high and low bytes still tests each bit.
.timerTest	mov	dx, PORT_PIT_MODE
		cmp	bx, 0FF00h		; reached target value?
		jz	.timerTestOk		; continue if so

		; latch counter 0 and read it
		mov	al, 0			; PIT mode: counter 0 select, latch count value command
		out	dx, al
		dec	dx			; adjust port downwards to PORT_PIT_COUNTER0
		dec	dx
		dec	dx
		Delay	1			; prev 3 'dec' is clocks equivalent to one 'jmp short' so we only need one delay here
		in	al, dx			; read timer counter 0 latched value (lobyte)
		or	bh, al,CODE=LONG	; set bits 
		and	bl, al,CODE=LONG	; clear bit
		in	al, dx			; read timer counter 0 latched value (hibyte)
		or	bh, al,CODE=LONG	; set bits
		and	bl, al,CODE=LONG	; clear bits

		loop	.timerTest		; keep looping until target bit pattern reached
		mov	al, BEEP_TIMER_FAIL	; if loop exits here we call it a failure
		jmp	FatalBeeps

.timerTestOk
		; Initialize channel 2
		mov	al, 0B6h		; PIT mode: counter 2 select, lobyte/hibyte access,
						;           mode 3 select (square wave)
		out	dx, al

		dec	dx			; adjust port downwards to PORT_PIT_CHANNEL2
		mov	al, 33h			; divisor lobyte
		Delay	2
		out	dx, al
		mov	al, 5h			; divisor hibyte
		Delay	2
		out	dx, al			; counter 2 divisor == 533h

; ---------------------------------------------------------------------------
; Initialize DMA controllers and test channel 0 registers
		mov	al, CHECKPOINT_DMA
		out	PORT_DIAGNOSTICS, al

		; Any value written to a master clear port resets the controller
		out	PORT_DMA1_MASTER_CLEAR, al
		out	PORT_DMA2_MASTER_CLEAR, al

		; Setup DMA controllers: memory-to-memory transfer disabled,
		; channel 0 address hold disabled, controller enabled, normal
		; timing, fixed priority, late write selection, DREQ sense
		; active high, DACK sense active low
		mov	al, 0
		out	PORT_DMA1_COMMAND, al
		out	PORT_DMA2_COMMAND, al

		; Test that DMA channel 0 can write/read 0 and 1 bits in all
		; locations for current and base address registers.
		mov	dx, PORT_DMA_CHAN0_CUR
		mov	bx, 8000h		; test pattern: 1 bit set
		mov	cx, 40h			; 40h iterations: test bits into 2x16bit registers
						; (DMA channel 0 current address+base) normal and inverted

.dmaZeroTest	test	cl, 1
		jz	.l4			; odd iterations test the base address
		inc	dx			; so increment port to PORT_DMA_CHAN0_BASE

.l4		mov	al, bh,CODE=LONG
		out	dx, al			; output test pattern for register upper byte
		xchg	ax, bx			; xchg to get test pattern lower byte into AL
		Delay	2
		out	dx, al			; output test pattern for register lower byte
		xchg	ax, bx			; xchg to get test pattern back into BX for next loop
		Delay	2

		in	al, dx			; read back register value hibyte
		xchg	ah, al			; shift into AH
		Delay	2
		in	al, dx			; read back register value lobyte
		cmp	bx, ax,CODE=LONG	; value read back matches test pattern?
		jnz	.dmaZeroFail		; if not, report failure

		test	cl, 1			; odd iteration (DMA base register)?
		jz	.l5
		dec	dx			; reset port for next iteration after testing base register
		sar	bx, 1			; shift test pattern over by 1 bit

.l5		cmp	cl, 21h			; are we halfway through the test cycle?
		jnz	.l6
		mov	bh, 7Fh			; if so, switch to inverted test pattern

.l6		loop	.dmaZeroTest		; loop until all bits tested
		jmp	.dmaZeroOk		; then continue successfully

.dmaZeroFail	mov	al, BEEP_DMA_FAIL
		jmp	FatalBeeps

; ---------------------------------------------------------------------------
; Set DMA channel modes
.dmaZeroOk	mov	dx, PORT_DMA1_MODE

.setDmaModes	mov	al, 40h			; DMA mode: channel 0, verify mode, single transfer mode
		mov	cx, 4			; process all four channels on the DMA controller

.setDmaMode	out	dx, al			; set mode for one channel
		inc	al			; advance mode to next channel
		loop	.setDmaMode

		; Advance port to update DMA2 controller modes
		add	dl, PORT_DMA2_MODE - PORT_DMA1_MODE
		jnb	.setDmaModes		; go again for second controller

		; Setup cascade from second DMA controller
		mov	al, 0C0h		; DMA2 mode: channel 4 to cascade mode
		out	PORT_DMA2_MODE, al
		mov	al, 0			; DMA2: enable channel 4
		out	PORT_DMA2_MASK, al

; ---------------------------------------------------------------------------
; Set DMA channel address registers
		mov	al, CHECKPOINT_DMA_PR
		out	PORT_DIAGNOSTICS, al

		jmp	.l7			; jump over data array
.dmaPagePorts	db	PORT_DMA_PAGE0
		db	PORT_DMA_PAGE1
		db	PORT_DMA_PAGE2
		db	PORT_DMA_PAGE3
		; 	PORT_DMA_PAGE4 not set because it's used for the cascade feature
		db	PORT_DMA_PAGE5
		db	PORT_DMA_PAGE6
		db	PORT_DMA_PAGE7

		; Set all page registers, then verify they read back OK
.l7		mov	dx, cs
		mov	ds, dx
		mov	bl, 0FFh		; set address registers to all-ones
.pagePortsTest	mov	di, .setPageReg

.pagePortsLoop	mov	si, .dmaPagePorts
		mov	cx, 7			; seven page registers to set
		mov	dh, 0			; clear upper half of port number
.nextPageReg	lodsb				; load page register port
		mov	dl, al,CODE=LONG	; merge into DX
		jmp	di			; jump to test handler

.setPageReg	mov	al, bl,CODE=LONG
		out	dx, al			; set DMA page register
		jmp	.l8

.checkPageReg	in	al, dx			; read DMA page register
		cmp	al, bl,CODE=LONG			; matches what we wrote?
		jz	.l8			; continue if so
		mov	al, BEEP_DMA_PR_FAIL	; report error if not
		jmp	FatalBeeps

.l8		loop	.nextPageReg		; loop over all page registers

		cmp	di, .checkPageReg	; are we on the verification pass?
		jz	.dmaPageCheckOk		; continue with POST if so
		mov	di, .checkPageReg	; rerun loop with verification instead of setting if not
		jmp	.pagePortsLoop

.dmaPageCheckOk	xor	bl, 0FFh		; invert test pattern
		jz	.pagePortsTest		; rerun tests with all-zero pattern before continuing

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

		mov	ah, al,CODE=LONG	; store last read of refresh toggle
.pollRamRefresh	in	al, dx			; read KBC port B
		and	al, 10h			; isolate RAM refresh toggle
		cmp	ah, al,CODE=LONG	; changed from previous read?
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
		jz	.l9			; continue if not
		xor	bx, bx,CODE=LONG	; if so, don't count it as a soft reset
		mov	al, 0Ch			; toggle IO and RAM parity checks
		out	PORT_KBC_PORTB, al
		mov	al, 0
		out	PORT_KBC_PORTB, al

.l9		mov	ax, cs			; load a fake return stack to handle
		mov	ss, ax			; the checking the first 64KB of RAM
		mov	sp, .returnStack2
		jmpn	MirrorRam512

.loc_F8387	jnb	.l10			; TODO: where does this come from?
		mov	dx, PORT_PAR_PORTC_R
		mov	al, 1
		out	dx, al			; TODO: parallel port C isn't writable though?

.l10		mov	ax, BDA_SEGMENT		; Set DS to BDA
		mov	ds, ax
		mov	ax, 0			; Set ES to first 64K
		mov	es, ax
		cmp	bx, SOFT_RESET_FLAG	; is it a soft reset?
		jz	.memTestClear		; skip memory test if so

		mov	ax, cs			; load a fake return stack to handle
		mov	ss, ax			; (TODO: what?)
		mov	sp, .returnStack3
		mov	bp, 8000h		; TODO: is this a length?
		jmp	.loc_F9369

.loc_F83AC	jnb	.l13
		mov	bx, ax,CODE=LONG
		mov	al, BEEP_RAM_OE
		cmp	dx, .aOddEvenLogic
		jz	.memTestBeep
		mov	al, BEEP_RAM_ADDRESS_LINE
		cmp	dx, .aAddressLine
		jz	.memTestBeep

.memTestBeepDet	xor	bx, cx,CODE=LONG
		mov	cx, 10h
		mov	ax, 0FH
.l11		rol	bx, 1
		jnb	.l12
		inc	ah
		add	al, cl,CODE=LONG
.l12		loop	.l11
		dec	ah
		jz	.memTestBeep
		mov	al, BEEP_RAM_MULTIPLE
.memTestBeep	jmp	FatalBeeps

.l13		jmp	.loc_F93C6

.returnStack3	dw	.loc_F83AC
		dw	.loc_F83E4
.returnStack2	dw	.loc_F8387

.loc_F83E4	mov	bx, ax,CODE=LONG
		jb	.memTestBeepDet

.memTestClear	xor	ax, ax,CODE=LONG	; clear first 64K of RAM
		mov	es, ax
		xor	di, di,CODE=LONG
		mov	cx, 8000h
		rep stosw

		mov	ax, 0
		mov	ss, ax
		mov	sp, 8000h
		mov	ds, [cs:kBdaSegment]

		mov	al, CHECKPOINT_RAM_PARITY
		out	PORT_DIAGNOSTICS, al

		call	ToggleNmiChecks
		test	al, 80h			; RAM parity error occurred?
		jz	.memTestFin		; continue if not
		mov	al, BEEP_RAM_PARITY	; report error if so
		jmp	FatalBeeps

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

; ---------------------------------------------------------------------------
; Test DMA controllers
; Assumes that all channel registers for a controller are at regular offsets
; from each other.  Does not test the first channel because it is either used
; for RAM refresh (DMA1) or the cascade feature (DMA2).

		; Test DMA1 registers
		mov	al, CHECKPOINT_DMA1
		out	PORT_DIAGNOSTICS, al
		mov	dx, PORT_DMA_CHAN1_BASE
		mov	cx, 6			; 2 registers x 3 channels
		mov	si, PORT_DMA_CHAN2_BASE - PORT_DMA_CHAN1_WC
		call	TestDmaRegisters
		jnb	.testDma1Ok
		mov	al, BEEP_DMA1_REG
		jmp	FatalBeeps

		; Test DMA2 registers
.testDma1Ok	mov	al, CHECKPOINT_DMA2
		out	PORT_DIAGNOSTICS, al
		mov	dx, PORT_DMA_CHAN5_BASE
		mov	cx, 6			; 2 registers x 3 channels
		mov	si, PORT_DMA_CHAN6_BASE - PORT_DMA_CHAN5_WC
		call	TestDmaRegisters
		jnb	.testDma2Ok
		mov	al, BEEP_DMA2_REG
		jmp	FatalBeeps

; ---------------------------------------------------------------------------
; Test PIC mask registers

		; Test PIC1 mask register
.testDma2Ok	mov	al, CHECKPOINT_PIC1
		out	PORT_DIAGNOSTICS, al
		mov	dx, PORT_PIC1_MASK
		call	TestPicMaskReg
		jz	.testPic1Ok
		mov	al, BEEP_PIC1_REG
		jmp	FatalBeeps

.testPic1Ok	mov	al, CHECKPOINT_PIC2
		out	PORT_DIAGNOSTICS, al
		mov	dx, PORT_PIC2_MASK
		call	TestPicMaskReg
		jz	.testPic2Ok
		mov	al, BEEP_PIC2_REG
		jmp	FatalBeeps

; ---------------------------------------------------------------------------
; Initialize the interrupt vector table and enable interrupts
.testPic2Ok	mov	al, CHECKPOINT_IVT
		out	PORT_DIAGNOSTICS, al

		mov	di, 0			; point ES:DI at IVT start
		mov	es, di
		mov	bx, cs			; BX is BIOS code segment
		mov	ds, bx
		mov	dx, DummyIsr
		push	dx			; why save DX here?

		; Set int 00h - 1Eh to DummyIsr
		mov	cx, 78h
.clearIvt	mov	ax, dx,CODE=LONG
		stosw				; store ISR offset
		mov	ax, bx,CODE=LONG
		stosw				; store ISR segment
		loop	.clearIvt

		; Set int 60h - 67h to 0000:0000
		mov	ax, cx,CODE=LONG	; CX was 0 from loop above
		mov	di, 180h		; start filling from int 30
		mov	cx, 0Eh			; fill 7x2 words
		rep stosw

		; Set int 08h - int 1Eh
		mov	si, InitialIvt
		mov	di, 20h
		mov	cx, 18h
		pop	dx			; why pop an unmodified value?
.loadIvt1	lodsw				; load ISR offset
		cmp	ax, dx,CODE=LONG	; ISR offset is DummyIsr?
		jnz	.loadIvtEntry1
		add	di, 4			; skip if so
		jmp	.loadIvtLoop1
.loadIvtEntry1	stosw				; store ISR offset
		mov	ax, cs
		stosw				; store ISR segment
.loadIvtLoop1	loop	.loadIvt1

		; Set int 1Fh to 0000:0000 (InitialIvt sets the offset to 0
		; but the segment was set to CS, so we must correct it)
		mov	[es:7Eh], 0,DATA=WORD

		; Set int 70h to 78h
		; DI is still pointing to InitialIvt table from previous loop
		mov	di, 1C0h		; start filling in from int 70h
		mov	cx, 8
.loadIvt2	lodsw				; load ISR offset
		cmp	ax, dx,CODE=LONG	; ISR offset is DummyIsr?
		jnz	.loadIvtEntry2
		add	di, 4			; skip if so
		jmp	.loadIvtLoop2
.loadIvtEntry2	stosw				; store ISR offset
		mov	ax, cs
		stosw				; store ISR segment
.loadIvtLoop2	loop	.loadIvt2

		; Set a few other ICT entries manually
		; [Compat] is this required because the InitalIvt table
		;          must keep the same items in the same order?
		mov	[es:08h], IntNmi_Compat,DATA=WORD
		mov	[es:0Ah], cs
		mov	[es:14h], PrntScrn_Compat,DATA=WORD
		mov	[es:16h], cs

		; Enable interrupts
		mov	al, 0FAh		; PIC1: set interrupt mask:
						;   IRQ0 - timer (disabled)
						;   IRQ1 - keyboard
						;   IRQ2 - cascade (disabled)
						;   IRQ3 - serial port, COM2
						;   IRQ4 - modem, COM1
						;   IRQ5 - reserved
						;   IRQ6 - floppy drive
						;   IRQ7 - parallel port
		out	PORT_PIC1_MASK, al
		sti

		ENDPROC	SDH_POST


