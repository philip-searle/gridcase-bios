
RESET		PROGRAM	OutFile=reset.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"segments/bda.inc"
		include	"segments/ivt.inc"
		include	"bios-version.inc"
		include	"cmos.inc"
		include	"descriptors.inc"
		include	"diagnostics.inc"
		include	"dma.inc"
		include	"grid.inc"
		include	"keyboard.inc"
		include	"npu.inc"
		include	"parallel.inc"
		include	"pic.inc"
		include	"pit.inc"
		include	"serial.inc"
		include	"vga.inc"
		include	"video.inc"

		EXTERN	Beep, BeepFactor
		EXTERN	kIvtSegment, kBdaSegment, InitialIvt
		EXTERN	kAddressLine, kOddEvenLogic, kRomBadChecksum
		EXTERN	UnexpectedInt, IntNmi_Compat, PrntScrn_Compat, SoftwareIret, Int8_Compat
		EXTERN	VidInit, VidInitBacklite
		EXTERN	VidTestMem, TestAllMem, TestMemData, TestMemLoAddr
		EXTERN	DetectMemC, DetectMemSize, MemSetXmsEms
		EXTERN	ChecksumBios, ChecksumRom, InitOptionRoms, CheckParityErr
		EXTERN	SerDetectPort, ParDetectPort
		EXTERN	TestDmaRegs, TestPicMaskReg
		EXTERN	KbWaitReady, KbWaitResponse, KbWaitEmpty
		EXTERN	ReadCmos, WriteCmos
		EXTERN	SetCriticalErr, InitTimerTicks
		EXTERN	GridAutodetect, FdCheckConfigValid, HdDetect70, HdXtEarlyReset, HdInit
		EXTERN	A20Disable, PmClearTraces
		%IF	BIOS_VERSION > 19880912
			; 1988 BIOS doesn't support password feature
			EXTERN	PwEnabled, PwStartInput, PwPrompt, PwProcessInput, PwEndInput
			EXTERN	PwCompareStored, PwBackdoor2, PwIncorrect, PwClearBuffer
		%ENDIF
		%IF	BIOS_VERSION = 19891025
			EXTERN	VgaIdentify, VgaGetFlags, VgaUnknown2, VgaUnknown3
		%ENDIF
		EXTERN	ConString, ConString_Inline, ConCharHex2, ConCharHex4
		EXTERN	ConBadCsumMsg, ConBiosBanner
		EXTERN	PromptF1Cont
		EXTERN	SDH_03, SDH_04

		PUBLIC	FatalBeeps
		PUBLIC	Reset_Actual

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
;         cc = Count of beeps minus one, third group
; ===========================================================================
FatalBeeps	PROC
		out	PORT_DIAGNOSTICS, al
		jmp	.l1
		FillerNop
.l1		mov	bh, [cs:BeepFactor]	; cs override because ds may not be valid
		mov_	dh, al			; move beep code to dh; al will be used for port writes
		mov	al, 0Ch			; setup KBC port B write: both parity checks disabled
		rol	dh, 2			; pre-rotate beep code for main loop
		mov	bl, 3			; three groups of beeps

; ---------------------------------------------------------------------------
.beepGroup	rol	dh, 2			; rotate next beep group to lower bits of dh
		mov_	dl, dh			; copy beep group and isolate lower bits
		and	dl, 3
		inc	dl			; beep codes are stored excess-one so adjust for true value

; ---------------------------------------------------------------------------
.beep		mov	si, FATAL_BEEP_LENGTH
.l2		xor	al, 2			; toggle speaker
		out	PORT_KBC_PORTB, al

		mov_	cl, bh			; loop 4xbeep factor between toggles
		mov	ch, 0
		add_	cx, cx
		add_	cx, cx
		loop	$			; 4 clock loop

		dec	si
		jnz	.l2

; ---------------------------------------------------------------------------
		mov_	ch, bh			; delay 256xbeep factor between beeps
.beepGap	Delay	1
		loop	.beepGap

		dec	dl			; beep completed
		jnz	.beep			; loop if more in this group

; ---------------------------------------------------------------------------
		mov_	ch, bh			; delay 1024xbeep factor between beep groups
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
		in	al, PORT_CMOS_DATA	; read shutdown reason from CMOS
		mov_	ah, al			; store it for later comparisons
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
		cmp	ah, SD_UNKNOWN_9
		ja	.l2
		cmp	ah, SD_UNKNOWN_6
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
		mov	al, ~IRQ_CASCADE	; PIC1 OCW1: enable cascade from PIC2
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
		mov_	bl, ah
		shl	bx, 1
		jmp	[cs:.sdHandlers+bx]

.sdHandlers	dw	SDH_00	; SD_BOOTLOADER_REQ
		dw	SDH_01	; SD_JMP_WITH_INT
		dw	SDH_02	; SD_UNKNOWN_6
		dw	SDH_03	; SD_PM_BLOCK_MOVE
		dw	SDH_04	; SD_PM_TEST1
		dw	SDH_05	; SD_UNKNOWN_9
		dw	SDH_02	; SD_JMP_WITHOUT_INT

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
		call	A20Disable		; no A20 needed now we're back in real mode
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
		%IF	BIOS_VERSION > 19880912
			mov	bl, 5			; delay for 5x64k nop loops
			xor_	cx, cx
.l1			nop
			loop	.l1
			dec	bl
			jnz	.l1
			mov	dx, PORT_UNKNOWN_426	; unnecessary reload of dx?
			mov	al, 0			; set port 426 to 0
			out	dx, al
		%ENDIF
		mov	dx, PORT_XMS_ENABLE	; POST code requires easy access
		mov	al, 1			; to hi-RAM so map it as XMS
		out	dx, al			; instead of EMS

		; Default to outputting video to the external D-sub connector.
		; Also make sure the application ROMs aren't mapped over the
		; video RAM (might still be enabled from pre-reboot).
		mov	dx, PORT_ROM_SUBSYSTEM1
		mov	al, 0
		out	dx, al
		mov	dx, PORT_VID_EXTERNAL
		mov	al, VID_EXTERNAL_ENABLE
		out	dx, al

; ---------------------------------------------------------------------------
; ??? Check for undocumented shutdown reason 0FCh -- maybe BIOS password?
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
		include	"post/00_vga.asm"
		include	"post/01_cpu.asm"
		include "post/02_vidoff.asm"
		include "post/03_cmosreg.asm"
		include "post/04_bioscsum.asm"
		include "post/05_pitinit.asm"
		include "post/06_dmainit.asm"
		include "post/07_testram.asm"
		include "post/08_bdainit.asm"
		include "post/09_dmareg.asm"
		include "post/10_picreg.asm"
		include "post/11_ivt.asm"
		include "post/12_kbc.asm"
		include "post/13_cmosdiag.asm"
		include "post/14_vidinit.asm"
		include "post/15_hdroms.asm"
		include "post/16_vidinit.asm"

		; Enough hardware is initialized that we can safely ID ourselves
		call	ConBiosBanner

		include "post/17_protmode.asm"
		include "post/18_timer2.asm"
		include "post/19_keyboard.asm"

		; Comprehensive memory test only on cold boots
		mov	ax, [SoftResetFlag]
		and	ax, ~CRITICAL_ERR_FLAG
		cmp	ax, SOFT_RESET_FLAG
		jz	.warmBoot
		call	HdXtEarlyReset
		mov	al, CHECKPOINT_RAM_ALL
		out	PORT_DIAGNOSTICS, al
		call	TestAllMem
.warmBoot	call	HdDetect70	; ???
		call	KbWaitEmpty	; consume any keypresses that skipped the memtest

; ---------------------------------------------------------------------------
; We're ready for the timer and keyboard to be processed normally via interrupts
		mov	al, ~(IRQ_TIMER | IRQ_KEYBOARD | IRQ_CASCADE)
		out	PORT_PIC1_MASK, al
		sti

		; 0A0h == 'reading 2-byte KB ID in progress', 'force numlock on'
		mov	[KbStatusFlags3], KBSTAT3_ID_1 | KBSTAT3_FORCE_NUMLOCK
		call	KbWaitReady
		jnz	.l3			; don't request KB ID if KBC isn't ready in time
		mov	al, KB_CMD_READ_KB_ID
		out	PORT_KBC_DATA, al

; ---------------------------------------------------------------------------
; POST tests that require keyboard and timer interrupts can now be run
.l3
		include	"post/20_rtc.asm"

; ---------------------------------------------------------------------------
; If we don't have video at this point (and no earlier failures were fatal)
; then go into a reset loop (TODO: is this right?)
		mov	ds, [cs:kBdaSegment]
		test	[InterruptFlag], 20h
		jnz	.haveVideo
		jmp	Reset_Actual

; ---------------------------------------------------------------------------
; Check CMOS config matches detected hardware
.haveVideo
		include	"post/21_validconfig.asm"

; ---------------------------------------------------------------------------
; Reconfigure memory controller for EMS if required
		call	MemSetXmsEms

; ---------------------------------------------------------------------------
; Update equipment byte based on detected floppy drives
		call	FdCheckConfigValid
		cmp	[Fd1MediaState], 0
		jz	.updatedFdCount
		or	[EquipmentWord], 40h,DATA=BYTE	; why not add?
		cmp	[Fd2MediaState], 00h,DATA=BYTE
		jz	.updatedFdCount
		add	[EquipmentWord], 40h,DATA=BYTE

; ---------------------------------------------------------------------------
; Get hard drive controller initialized and hooked into the IVT
.updatedFdCount	call	HdInit

; ---------------------------------------------------------------------------
; Detect and init option ROMs outside the the hard disk controller range
		mov	si, OPT_ROM_SEG_LO
		mov	cx, OPT_ROM_SEG_HI
		mov	dl, 0			; DL used to indicate no video available yet
		call	InitOptionRoms

; ---------------------------------------------------------------------------
; Init Int1A time-of-day counter if the RTC is running
		mov	ds, [cs:kBdaSegment]
		mov	al, CMOS_STATUS_DIAG | NMI_DISABLE
		call	ReadCmos
		test	al, 80h			; power good?
		jnz	.pastTodInit		; skip time-of-day init if not
		call	InitTimerTicks
		jb	.pastTodInit
		Inline	ConString,'Time-of-day not set - please set current time',0Dh,0Ah,0
		call	SetCriticalErr

.pastTodInit	sti				; re-enable interrupts after ReadCmos


; ---------------------------------------------------------------------------
; Alert user if keyboard is locked (GRiD 1500 series do not support the
; keyboard lock feature, so this should never trigger).
		; unset kb ID state bits and numlock forced on bit
		and	[KbStatusFlags3], ~(KBSTAT3_ID_1 | KBSTAT3_ID_2 | KBSTAT3_FORCE_NUMLOCK)
		in	al, PORT_KBC_STATUS
		test	al, KBC_STATUS_INH	; Keyboard inhibited?
		jnz	.pastKbLock		; if so, skip warning
		Inline	ConString,'Keyboard is locked - please unlock',0Dh,0Ah,0
		call	SetCriticalErr

; ---------------------------------------------------------------------------
; If POST checks resulted in a critically important message, prompt the user
; to press F1 to acknowledge it.
.pastKbLock	test	[SoftResetFlag], CRITICAL_ERR_FLAG,DATA=BYTE	; lowest bit set (important message)?
		jz	.pastF1Prompt
		call	PromptF1Cont		; make sure user has read POST messages
		jmp	.pastF1Prompt		; ??? useless jmp

; ---------------------------------------------------------------------------
; Clear the soft reset flag.  Code past this point cannot change behaviour
; based on whether the reset was considered to be keyboard-intiated or not.
.pastF1Prompt	mov	bx, 0
		mov	[SoftResetFlag], bx	; POST is over, no more soft resetting

; ---------------------------------------------------------------------------
; Clear the area used by protected-mode descriptor tables, IVT, etc.
		call	PmClearTraces

; ---------------------------------------------------------------------------
; Reset the stack and initialize the NPU (if present)
		mov	ax, STACK_SEGMENT
		mov	ss, ax
		mov	sp, STACK_TOP2
		mov	ax, 0
		mov	es, ax
		cli
		in	al, PORT_PIC2_MASK	; save PIC2 mask in AL
		fninit				; init NPU
		push	ax			; reserve a word on the stack
						; we don't care about the exact value
						; because we overwrite it immediately

		; Issue FNSTCW, wait a bit, and see if it wrote the expected value.
		; We can't use the no-wait version because if an NPU is not
		; present then we will lockup waiting for it to respond.
		mov_	bp, sp			; point BP to word we just reserved
		fnstcw	[bp+0]			; store NPU control word
		Delay	4
		and	[bp+0], 01F3Fh,DATA=WORD	; mask off reserved bits to 0
		cmp	[bp+0], 0033Fh,DATA=WORD	; equal to default NPU control word
							; as loaded by FNINIT?
		jnz	.afterNpuInit			; if not, assume no NPU

		; Store NPU status word
		fnstsw	[bp+0]
		Delay	4
		; Mask off condition codes - they are undefined after FINIT.
		; The rest of the status word should be equal to zero after FINIT.
		; BUG: mask should be 0B8BFH, the value actually used excludes
		;      bit 13 (highest bit of the top-of-stack pointer field)
		;      and includes bit 6 ('stack fault' on 80387 but 'reserved'
		;      on the 80287.
		;      In practice, bit 6 is likely always zero on an 80287.
		;      Bit 7 won't be checked, but it should be zero anyway.
		and	[bp+0], 098FFh,DATA=WORD
		cmp	[bp+0], 0,DATA=WORD	; equal to default NPU status word
						; as loaded by FINIT?
		jnz	.afterNpuInit		; if not, assume no NPU

		; If we make it here, the NPU appears to be present and functioning
		; as expected.  Mark it as available for user code.
		or	[EquipmentWord], 2,DATA=BYTE	; set 'NPU present' bit
		and	al, ~IRQ_NPU			; unmask coprocessor interrupt

.afterNpuInit	; Unreserve word from stack. Note that this doesn't pop to
		; the same register we pushed from because we need to preserve
		; the PIC2 mask value in AL.
		pop	bx
		; Unmask IRQ 9 so it can be redirected to the IRQ 2 hander in
		; software.  Not really anything to do with the NPU, but this is
		; our last opportunity to do it before the system expansion ROM
		; is checked (it might use it?)
		and	al, ~IRQ_9
		out	PORT_PIC2_MASK, al
		sti

; ---------------------------------------------------------------------------
; Locate and init system expansion ROM.
; Not very well documented, but the IBM AT BIOS supports a 64KB ROM mapped at
; E000:0000 that is initialized after all others, right before the Int19 IPL.
; It must have the AA55h signature and the checksum must cover the entire
; 64KB range.  Purpose unclear; possibly for BIOS expansion in later models.
		mov	si, SYS_ROM_SEG
		mov	ds, si
		cmp	[0], 0AA55h,DATA=WORD	; check for option ROM signature
		jnz	.afterSysRom
		xor_	cx, cx			; sys rom always 64KB in size
		call	ChecksumRom
		jnz	.sysRomBad
		mov	es, [cs:kBdaSegment]
		mov	[es:AdapterRomOffset], 3
		mov	[es:AdapterRomSegment], ds
		mov	al, 0 | NMI_ENABLE	; disable NMI during sys opt ROM init
		out	PORT_CMOS_ADDRESS, al
		callf	[es:AdapterRomOffset]
		jmp	.afterSysRom

.sysRomBad	call	ConCharHex4
		Inline	ConString,'0h Optional ',0
		mov	si, kRomBadChecksum
		call	ConBadCsumMsg

.afterSysRom	mov	ds, [cs:kBdaSegment]	; reset DS -> BDA after sys opt ROM init

; ---------------------------------------------------------------------------
; Reset parity checks (ROM search may have read from open bus)
		mov	al, 0			; KBC port B: speaker off, RAM
						; and I/O parity checking on,
						; keyboard enabled
		out	PORT_KBC_PORTB, al
		mov	al, 0 | NMI_ENABLE
		out	PORT_CMOS_ADDRESS, al

		%IF	BIOS_VERSION = 19880912
			; 1988 BIOS does a bunch of extra port writes here
			; What are these port writes doing?
			mov	dx, PORT_UNKNOWN_426
			mov	al, 0
			out	dx, al
			mov	dx, PORT_424
			mov	al, 1
			out	dx, al
		%ENDIF
		; Fall-through into SDH_00
		ENDPROC	SDH_POST

; ===========================================================================
; Shutdown handler called to request bootloader
; Also fallthrough from SDH_POST for cold boot.
; ===========================================================================
SDH_00		PROC
		%IF	BIOS_VERSION > 19880912
			; GRiD extensions to password-lock BIOS bootloader
			; only supported after 1988 BIOS
			call	PwEnabled
			jnb	.ipl
			call	PwStartInput
			call	PwPrompt
			call	PwProcessInput
			call	PwEndInput
			call	PwCompareStored
			jnb	.passwordOk
			call	PwBackdoor2
			jnb	.passwordOk

			; Password didn't match, notify user and restart
			call	PwIncorrect
			mov	ds, [cs:kBdaSegment]
			mov	[SoftResetFlag], SOFT_RESET_FLAG
			jmp	Reset_Actual

.passwordOk		call	PwClearBuffer
.ipl			; End of password checks, resume IPL
		%ENDIF

		int	19h			; load system from disk
		jmp	SDH_00			; retry if bootloader failed

		ENDPROC	SDH_00

ENDPROGRAM	RESET
