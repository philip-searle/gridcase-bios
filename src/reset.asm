
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
		call	KbWaitEmpty
		mov	al, NONSPECIFIC_EOI	; Issue non-specific EOI to both PICs
		out	PORT_PIC2_CTRL, al	; to clear timer interrupt
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
		mov	dx, PORT_ROM_SUBSYSTEM1
		mov	al, 0			; ROM subsystem == 0?
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
; Because the POST code is really quite long it's split up into separate
; procedures.  This allows us to reuse local labels between sections of the
; POST code that are not related, as well as keeping the individual 'chunks'
; of the POST process in more manageable blocks.
;
; Each included file is expected to define a single PROC that will be entered
; by 'falling through' from the previous file.  It is expected to continue to
; the next part of the POST code in the same way: by 'falling off' the end of
; the PROC.
		include	"src/post_01_cpu.asm"
		include "src/post_02_vidoff.asm"
		include "src/post_03_cmosreg.asm"
		include "src/post_04_bioscsum.asm"
		include "src/post_05_initpit.asm"
		include "src/post_06_initdma.asm"
		include "src/post_07_testram.asm"
		include "src/post_08_initbda.asm"
		include "src/post_09_dmareg.asm"
		include "src/post_10_picreg.asm"
		include "src/post_11_ivt.asm"
		include "src/post_12_kbc.asm"
		include "src/post_13_cmosdiag.asm"
		include "src/post_14_vidinit.asm"
		include "src/post_15_hdroms.asm"
		include "src/post_16_vidinit.asm"

		; Enough hardware is initialized that we can safely ID ourselves
		call	WriteBiosBanner

		include "src/post_17_protmode.asm"
		include "src/post_18_timer2.asm"
		include "src/post_19_keyboard.asm"

		; Comprehensive memory test only on cold boots
		mov	ax, [SoftResetFlag]
		and	ax, 0FFFEh
		cmp	ax, SOFT_RESET_FLAG
		jz	.warmBoot
		call	HdcTestDriveReady
		mov	al, CHECKPOINT_RAM_ALL
		out	PORT_DIAGNOSTICS, al
		call	TestAllMem
.warmBoot	call	IdeAutodetect_70	; ???
		call	KbWaitEmpty		; consume any keypresses that skipped the memtest

; ---------------------------------------------------------------------------
; We're ready for the timer and keyboard to be processed normally via interrupts
		mov	al, 0F8h		; unmask IRQ0/1/2 (timer, keyboard, cascade)
		out	PORT_PIC1_MASK, al
		sti

		; 0A0h == 'reading 2-byte KB ID in progress', 'force numlock on'
		mov	[KbStatusFlags3], 0A0h
		call	KbWaitReady
		jnz	.l3			; don't request KB ID if KBC isn't ready in time
		mov	al, KB_CMD_READ_KB_ID
		out	PORT_KBC_DATA, al

; ---------------------------------------------------------------------------
; POST tests that require keyboard and timer interrupts can now be run
.l3
		include	"src/post_20_rtc.asm"

; ---------------------------------------------------------------------------
; If we don't have video at this point (and no earlier failures were fatal)
; then go into a reset loop (TODO: is this right?)
		mov	ds, [cs:kBdaSegment]
		test	[InterruptFlag], 20h
		jnz	.haveVideo
		jmp	Reset_Actual

.haveVideo
		include	"src/post_21_validconfig.asm"

		ENDPROC	SDH_POST

