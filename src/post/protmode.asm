
POST_PROTMODE	PROGRAM	OutFile=build/post/protmode.obj

		include	"macros.inc"
		include	"segments/bda.inc"
		include	"cmos.inc"
		include	"descriptors.inc"
		include	"diagnostics.inc"
		include	"keyboard.inc"

		EXTERN	A20Disable, A20Enable
		EXTERN	ReadCmos, WriteCmos
		EXTERN	FatalBeeps
		EXTERN	WriteString_Inline, WriteString, WriteCrLf
		EXTERN	WriteChar, WriteCharHex2, WriteCharHex4
		EXTERN	kBdaSegment
		EXTERN	SetSoftResetFlag
		EXTERN	ResetNmiChecks, ResetCpu

		PUBLIC	kAddressLine
		PUBLIC	kOddEvenLogic
		PUBLIC	DetectMemSize
		PUBLIC	PmClearTraces
		PUBLIC	TestAllMem
		PUBLIC	TestMemData
		PUBLIC	TestMemLoAddr
		PUBLIC	SDH_04

; =====================================================================
; Procedures in this file are called to enter, leave, and run tests
; within the 80286's protected mode.
; =====================================================================

; Machine Status Word to load when enabling protected mode
kProtModeMsw	dw	1

; ---------------------------------------------------------------------
; Location of system data structures during protected mode operations
; Code assumes that these are contiguous

; Start of system data structures
kPmSysData	equ	0D0A0h

; Interrupt descriptor table
kPmIdt		equ	kPmSysData
kPmIdtLen	equ	800h

; Global descriptor table
kPmGdt		equ	kPmIdt+kPmIdtLen
kPmGdtLen	equ	88h

;kPmSysDataLen	equ	kPmGdt+kPmGdtLen

; =====================================================================
; Protected mode GDT template.
; Entries are calculated to be a valid GDT with self-referencing GDT
; descriptor when copied to real-mode address 0000:kPmGdt.
; =====================================================================
kProtModeGdt	DESC_TABLE
		; Null descriptor
dtiNull		DESC_SYSTEM	0000000h, 00000h, Type=0, Present=0
		; GDT
dtiGdt		DESC_DATA	kPmGdt, 00088h
		; IDT
dtiIdt		DESC_DATA	kPmIdt, 00800h
		; Bios Data Area
dtiBda		DESC_DATA	0000400h, 00300h	; TODO: constants
		; MDA refresh buffer
dtiMda		DESC_DATA	00B0000h, 01000h	; TODO: constants
		; CGA refresh buffer
dtiCga		DESC_DATA	00B8000h, 04000h	; TODO: ... really for all of these ...
		; VGA refresh buffer
dtiVga		DESC_DATA	00A0000h, 0FFFFh
		; HDC option ROM space?
dtiHdcRom	DESC_DATA	00C0000h, 0FFFFh
		; BIOS code
dtiBiosCode	DESC_CODE	00F0000h, 0FFFFh
		; Used by PmDetectMem, base filled in at runtime
dtiDetectMem	DESC_DATA	0000000h, 0FFFFh
		; unused
dtiUnused2	DESC_DATA	0000000h, 0FFFFh
		; Stack, base filled in at runtime
dtiStack	DESC_DATA	0000000h, 0FFFFh
		; unused
dtiUnused3	DESC_DATA	0000000h, 0FFFFh
		; TSS
dtiTss		DESC_SYSTEM	000C000h, 00800h, Type=DT_SYS_TSS
		; ???
dtiUnused4	DESC_DATA	000D910h, 00800h
		; LDT
dtiLdt		DESC_SYSTEM	000D000h, 00088h, Type=DT_SYS_LDT, Dpl=3
		; ???
dtiUnused5	DESC_DATA	000D920h, 00088h

; =====================================================================
; PmIret
; Dummy interrupt handler for protected mode.
; Output a distinct code to the diagnostic port as it is not expected
; to be called in normal use.
; =====================================================================
PmIret		PROC
		push	ax
		mov	al, CHECKPOINT_PM_IRET
		out	PORT_DIAGNOSTICS, al
		pop	ax
		iret
		ENDPROC	PmIret

; =====================================================================
; PmClearTraces
; Clears memory that was modified during protected-mode POST operations
; back to zero.  Call from real-mode since it wipes out the GDT and
; related structures,
; =====================================================================
PmClearTraces	PROC
		mov	di, kPmIdt
		mov	cx, 444h
		mov	ax, 0
		mov	es, ax
		rep	stosw
		retn
		ENDPROC	PmClearTraces

; =====================================================================
; PmEnter
; Attempts to enter protected mode.
;
; On success, returns in protected mode with:
; 	GDT setup with contents of kProtModeGdt
; 	IDT setup with entries pointed to PmIret
; 	CS and SS set to appropriate selectors from GDT
; 	All other registers preserved
;
; On failure, stays in real mode with:
; 	error message printed with with beep code
; 	CF set
; =====================================================================
PmEnter		PROC
		mov	al, 0		; clear diagnostic port for mode switch
		out	PORT_DIAGNOSTICS, al

		; Need to enable A20 line to get access to all address
		; space before we can attempt setting up for mode switch
		call	A20Enable
		jz	.a20Enabled
		call	A20Disable
		push	ds
		mov	ds, [cs:kBdaSegment]
		test	[InterruptFlag], 20h
		jnz	.l1
		mov	al, BEEP_A20_FAIL
		jmp	FatalBeeps
.l1		pop	ds
		Inline	WriteString,'Gate A20 failure',0Dh,0Ah,0
		call	SetSoftResetFlag
		stc
		retn

; ---------------------------------------------------------------------
; We're committed to setting up a protected-mode environment now

.a20Enabled	; Setup a protected-mode IDT (8 bytes per entry)
		pusha
		mov	ax, 0
		mov	es, ax
		mov	di, kPmIdt
		mov	cx, kPmIdtLen/8

.setupIdtEntry	mov	ax, PmIret
		stosw			; store handler offset
		mov	ax, OFFSET#dtiBiosCode
		stosw			; store code segment descriptor
		mov	ax, 8700h
		stosw			; store flags: present, DPL=0, type=trap gate
		mov	ax, es
		stosw			; store 0 for reserved word
		loop	.setupIdtEntry

		; Setup protected-mode GDT
		push	cs
		pop	ds
		mov	si, kProtModeGdt
		mov	di, kPmGdt
		mov	cx, kPmGdtLen/2
		rep	movsw

		; Convert real-mode SS into linear address and place it
		; in the stack descriptor's base field
		mov	ax, ss
		shl	ax, 4
		; [ReadMe] explicit segment override
		SEGES:mov	[kPmGdt+dtiStack+DESCRIPTOR.BaseLo], ax
		mov	ax, ss
		shr	ah, 4
		mov	[es:kPmGdt+dtiStack+DESCRIPTOR.BaseHi], ah

		; Disable NMIs while we switch modes
		mov	al, NMI_DISABLE
		out	PORT_CMOS_ADDRESS, al

; ---------------------------------------------------------------------
; Perform the mode switch

		; Load GDT and IDT descriptor base/limit then
		; flip the PM flag and farjmp to clear prefetch queue
		lgdt	[es:kPmGdt+dtiGdt]
		lidt	[es:kPmGdt+dtiIdt]
		lmsw	[kProtModeMsw]
		PmJmp	dtiBiosCode, .clearPrefetch

; ---------------------------------------------------------------------
; We are now in protected mode

.clearPrefetch	; Reload SS from the GDT so we can use our stack again
		mov	ax, dtiStack
		mov	ss, ax

		; Restore all registers and declare success
		popa
		clc
		retn
		ENDPROC	PmEnter

; =====================================================================
; PmLeaveTest1
; Transitions from protected mode back to real mode.  Since the 80286
; was not designed to do this, the keyboard controller is employed to
; pulse the CPUs reset input.  Early BIOS POST code notices this and
; returns control to our calling code.  Slow but effective.
; =====================================================================
PmLeaveTest1	PROC
		pusha			; save all registers

		; Convert stack descriptor back to segment
		mov	si, dtiGdt
		mov	ds, si
		mov	si, ss
		call	PmDescToSegment

		; Store stack location, to be restored on reboot
		mov	si, dtiBda
		mov	ds, si
		mov	[AdapterRomSegment], ax
		mov	[AdapterRomOffset], sp

		; Set CMOS state so early POST knows to restore
		; state to SDH_04 after reset
		mov	al, SD_PM_TEST1
		mov	ah, CMOS_SHUTDOWN_REASON | NMI_DISABLE
		call	WriteCmos

		; Jump into the void
		jmp	ResetCpu
		ENDPROC	PmLeaveTest1

; =====================================================================
; SDH_04
; Shutdown handler for SD_PM_TEST1.
; Called in real mode after reset.
; =====================================================================
SDH_04		PROC
		; Restore saved stack pointer and enable interrupts
		mov	ds, [cs:kBdaSegment]
		mov	sp, [AdapterRomOffset]
		mov	ss, [AdapterRomSegment]
		sti

		; Restore real mode memory map
		call	A20Disable
		sti

		; Check most recent diagnostic code and report error if appropriate
		in	al, PORT_DIAGNOSTICS
		cmp	al, CHECKPOINT_PM_IRET
		jnz	.leaveProc

		test	[InterruptFlag], 20h
		jnz	.l1
		mov	al, BEEP_PM_IRET
		jmp	FatalBeeps
.l1		Inline	WriteString,'Unexpected interrupt in protected mode',0Dh,0Ah,0
		call	SetSoftResetFlag

.leaveProc	popa
		retn
		ENDPROC	SDH_04

; =====================================================================
; PmDescToSegment
; Loads the base from a descriptor table entry and converts it to a
; real-mode segment base.
;
; On entry:
;   DS -> descriptor table base
;   SI == table index to load
;
; On return:
;   AX == real-mode segment base
; =====================================================================
PmDescToSegment	PROC
		mov	ax, [si+DESCRIPTOR.BaseLo]
		mov	bl, [si+DESCRIPTOR.BaseHi]
		shr	ax, 4
		shl	bl, 4
		or_	ah, bl
		retn
		ENDPROC	PmDescToSegment

; =====================================================================
; DetectMemSize
; Checks base and expansion memory to determine amount fitted, updating
; CMOS values to match.  Call from real mode; this proc enters/leaves
; protected mode multiple times internally.
; =====================================================================
DetectMemSize	PROC
		push	ds
		call	PmEnter		; we need to be in protected mode
		jb	.leaveProc	; keep CF set to indicate failure

		; Detect base memory fromm 64KB to 640KB
		mov	ax, dtiGdt	; PmDetectMemory require DS -> GDT
		mov	ds, ax
		mov	bl, 64 >> SEG_SIZE_SHIFT
		mov	bh, 640 >> SEG_SIZE_SHIFT
		call	PmDetectMem

		; Convert count of segments to count of KB
		mov	al, SEG_SIZE_KB
		mul	bl
		mov	bx, dtiBda
		mov	es, bx
		; [ReadMe] explicit segment override
		SEGES:mov	[MemorySizeKb], ax

		; Set "top 128K base memory installed" flag
		cmp	ax, 512
		jbe	.checkExtMem
		mov	al, CMOS_INFORMATION | NMI_DISABLE
		call	ReadCmos
		or	al, CMOS_INFO_128K
		mov	ah, CMOS_INFORMATION | NMI_DISABLE
		call	WriteCmos

.checkExtMem	; Detect extended memory from 1MB  to 8MB-128KB ???
		mov	bl, 1024 >> SEG_SIZE_SHIFT
		mov	bh, ((16*1024) - 128) >> SEG_SIZE_SHIFT
		call	PmDetectMem

		; Subtract max base memory size and convert count
		; of segments to count of KB, then update CMOS
		sub	bl, 1024 >> SEG_SIZE_SHIFT
		mov	bh, 0
		shl	bx, SEG_SIZE_SHIFT

		mov	ah, CMOS_EXPMEM2_LOBYTE | NMI_DISABLE
		mov_	al, bl
		call	WriteCmos
		mov	ah, CMOS_EXPMEM_LOBYTE | NMI_DISABLE
		call	WriteCmos

		mov	ah, CMOS_EXPMEM2_HIBYTE | NMI_DISABLE
		mov_	al, bh
		call	WriteCmos
		mov	ah, CMOS_EXPMEM_HIBYTE | NMI_DISABLE
		call	WriteCmos

		; Leave protected mode
		call	PmLeaveTest1
		call	ResetNmiChecks

.leaveProc	pop	ds
		retn
		ENDPROC	DetectMemSize

; =====================================================================
; PmDetectMem
; Detects writable memory by altering GDT descriptor dtiDetectMem and
; reading/writing tests pattern 55AAh to the start of each 64KB segment
; in turn.
;
; On entry:
;   BL == index of 64KB segment to start at (e.g. 1 == 64KB)
;   BH == index of 64KB segment to end at (e.g. 0Ah == 640KB)
;   DS -> descriptor for GDT
;
; On return:
;   BL == index of highest 64KB segment to pass tests
;   ZF set on success, clear on failure
; =====================================================================
PmDetectMem	PROC
		mov	di, 0
		mov	[dtiDetectMem+DESCRIPTOR.BaseLo], di

.check64K	; Update the descriptor base and reload it
		; to force the descriptor cache to be updated
		mov	[dtiDetectMem+DESCRIPTOR.BaseHi], bl
		mov	ax, dtiDetectMem
		mov	es, ax

		; Write test pattern and check it is retained
		mov	[es:di],55AAh, DATA=WORD
		Delay	2		; delay to allow bus capacitance to decay
		cmp	[es:di], 55AAh, DATA=WORD
		jnz	.leaveProc

		; Memory present, clear it all to zero
		mov_	ax, di
		mov	cx, 8000h	; clear 64K by words
		rep	stosw
		inc	bl		; advance segment
		cmp_	bl, bh		; reached last one?
		jnz	.check64K

.leaveProc	retn
		ENDPROC	PmDetectMem

kDecreasingMem	db	' Decreasing available memory',0Dh,,0Ah,0

; =====================================================================
; TestAllMem
; Cycles through all memory (base and extended/expanded), testing each
; segment's address and data lines.  Reports progress to the screen
; as it goes.  ESC terminates memory tests.
; On failure, decreased detected memory size to known good size.
; Call from real-mode (switches to/from protected-mode internally).
; =====================================================================
TestAllMem	PROC
		pusha

		; Get cursor position/blink state
		mov	ah, 3
		mov	bh, 0
		int	10h

		; Make cursor invisible
		push	cx		; preserve original cursor blink state
		mov	ah, 1
		or	ch, 20h		; make invisible
		int	10h

		; Store initial cursor position in DI
		mov	ah, 3
		mov	bh, 0
		int	10h
		mov_	di, dx

		; Display initial message.  We know the first 64K is good
		; because the POST calls FatalBeep if it isn't.
		Inline	WriteString,'064K Base Memory, ',0

		; Display placeholder for memory past 640K
		mov	ah, 3
		mov	bh, 0
		int	10h		; save cursor location again?

		; Display placeholder message for EMS/XMS memory test
		; and enable NMI checks for the memory test.
		push	ax
		mov	al, CMOS_GRIDFLAGS | NMI_ENABLE
		pushf
		call	ReadCmos
		popf
		test	al, GF_EMS
		pop	ax
		jz	.memExtended
		Inline	WriteString,'00000K EMS Memory',0Dh,0Ah,0
		jmp	.startMemTest
		FillerNop
.memExtended	Inline	WriteString,'00000K Extended',0Dh,0Ah,0

.startMemTest	mov	si, 1		; start memtest offset at 1
					; high bit is data/address test flag

.testPhase	; BH:BL == count of KB
		; Equivalent to count of 256KB plus remainder.
		; Shift by 2 to convert to count of 64KB plus remainder.
		mov	bx, [MemorySizeKb]
		shl	bx, 2
		mov	bl, 1		; start mem test at 64KB

		; Check all segments
		xchg	dx, di
		call	TestMemSegments
		xchg	dx, di
		jnb	.lowMemChecked

		cmp_	bl, bh		; reached end?
		jz	.reachedEnd

		; Memory fault found, report it and limit mme size to checked amount
		test	[InterruptFlag], 20h
		jnz	.l2
		mov	al, BEEP_RAM_ALL
		jmp	FatalBeeps
.l2		push	si
		mov	si, kDecreasingMem
		call	WriteString
		pop	si
		mov	bh, 0		; reset max segment to end memtest early
		shl	bx, SEG_SIZE_SHIFT
		mov	[MemorySizeKb], bx

.lowMemChecked	; Don't bother checking extended/expanded memory if we
		; dont't have video output ???
		test	[InterruptFlag], 20h
		jz	.nextTestPass

		; Read extended/expanded memory size
		mov	al, CMOS_EXPMEM2_LOBYTE | NMI_DISABLE
		call	ReadCmos
		mov_	bl, al
		mov	al, CMOS_EXPMEM2_HIBYTE | NMI_DISABLE
		call	ReadCmos
		sti

		; Check extended/expanded memory
		mov_	bh, al
		shl	bx, 2		; convert count of KB in BX to count of 64KB in BH
		mov	bl, 10h		; start mem test at 1MB
		add_	bh, bl
		and	si, 8000h	; reset memtest offset
		call	TestMemSegments
		jnb	.nextTestPass	; continue if all segments OK

		; Check for user terminating memory test
		cmp_	bl, bh		; reached the end but not OK?
.reachedEnd	jz	.userTerminated	; must be user keypress then

		; If we reach here the memory is bad and we know where.
		; Report error and reduce memory size accordingly.
		push	si
		mov	si, kDecreasingMem
		call	WriteString
		pop	si
		sub	bl, 10h		; account for 1MB start segment

		mov	bh, 0
		shl	bx, SEG_SIZE_SHIFT
		mov_	al, bl
		mov	ah, CMOS_EXPMEM2_LOBYTE | NMI_DISABLE
		call	WriteCmos
		mov	ah, CMOS_EXPMEM_LOBYTE | NMI_DISABLE
		call	WriteCmos
		mov_	al, bh
		mov	ah, CMOS_EXPMEM2_HIBYTE | NMI_DISABLE
		call	WriteCmos
		mov	ah, CMOS_EXPMEM_HIBYTE | NMI_DISABLE
		call	WriteCmos
		sti

.nextTestPass	and	si, 8000h	; reset memtest offset
		add	si, 8001h	; and advance test phase flag
		jb	.testsDone
		jmp	.testPhase	; go for second pass if not done

.testsDone	mov	ah, 1
		pop	cx		; restore cursor state
		int	10h
		popa
		retn

.userTerminated	mov	ah, 3
		mov	bh, 0
		int	10h		; read cursor position
		push	dx		; save it

		mov_	dx, di		; overwrite memtest progress with cancelled message
		mov	ah, 2
		mov	bh, 0
		int	10h
		Inline	WriteString,'Memory tests terminated by keystroke  ',0

		pop	dx		; restore cursor position
		mov	ah, 2
		mov	bh, 0
		int	10h
		jmp	.testsDone
		ENDPROC	TestAllMem

; =====================================================================
; TestMemSegments
; Checks a range of memory segments testing address and data lines.
; Call from real-mode (switches to/from protected-mode internally).
;
; On entry:
;   BL == lowest 64K segment to check
;   BH == highest 64K segment to check
;   DX == cursor position from int10h/03
;   SI == count of 64KB segments checked so far
;         bit 15 ignored (used as a flag by called subproc)
;
; On return:
;   BL, SI incremented by successfully checked segment count
;   CF clear on success
;   CF set on failure
;      BH==BL if user terminated memory test with ESC
;      BH!=BL is memory error detected, BL is failing segment
; =====================================================================
TestMemSegments	PROC
		push	di
		push	ds
		push	es
		cld

		; Check whether we've reached the end of the segment
		; range and dispatch accordingly
.nextSegment	cmp_	bl, bh
		jb	.checkSegment
		jmp	.leaveProc
		FillerNop

.checkSegment	; Enter protected-mode, update our reserved GDT entry
		; to point to the segment we want to check, and then
		; defer to TestMem to perform the actual memtest.
		call	PmEnter
		jb	.leaveFailure
		mov	ax, dtiGdt
		mov	ds, ax
		mov	[dtiDetectMem+DESCRIPTOR.BaseHi], bl
		mov	ax, dtiDetectMem
		mov	es, ax		; reload modified selector
		mov	bp, 8000h	; ??? 8000h words == 64KB
		push	dx
		call	TestMem		; DX==failing address after this
		pushf
		call	PmLeaveTest1
		popf
		sti
		jnb	.memOk

		; Memory failure detected if we reach here
		call	ResetNmiChecks
		call	ReportMemError
		jmp	.leaveFailure2

.memOk		; Memory was OK, did we detect any parity errors?
		call	ResetNmiChecks
		jz	.updateProgress	; continue if we didn't

		; Parity error detected if we reach here
		mov	dx, kParity
		call	ReportMemError
		jmp	.leaveFailure2

.updateProgress	; No errors detected if we make it here, so update
		; the screen and check for user pressing ESC
		pop	dx
		push	bx		; preserve BX
		mov	es, dx		; preserve DX in ES

		mov	ah, 3
		mov	bh, 0
		int	10h		; read cursor state
		push	dx		; preserve cursor state

		mov	dx, es
		mov	ah, 2
		mov	bh, 0		; move cursor back to where it was
		int	10h		; when we entered the proc

		inc	si		; increment visible counter
		mov_	ax, si
		shl	ax, SEG_SIZE_SHIFT
		call	WriteMemSize

		pop	dx		; restore cursor state
		mov	ah, 2
		mov	bh, 0
		int	10h		; move cursor back after updating screen

		mov	dx, es
		pop	bx
		in	al, PORT_KBC_STATUS
		test	al, KBC_STATUS_OBF	; keyboard has data for us?
		jz	.advanceSegment
		in	al, PORT_KBC_DATA	; read it if so
		cmp	al, SC2_ESC		; key was ESC?
		jnz	.advanceSegment		; continue if not

		; User pressed ESC if we reached here
		mov_	bl, bh		; move to the end of the segments immediately
		jmp	.leaveFailure

.advanceSegment	inc	bl		; move to next segment
		jmp	.nextSegment

.leaveFailure2	pop	dx
.leaveFailure	stc
.leaveProc	pop	es
		pop	ds
		pop	di
		retn
		ENDPROC	TestMemSegments

; =====================================================================
; TestMem
; Tests a segment of memory can be successfully written and read.
;
; On entry:
;   SI == bit 15 1 to test high address line
;         bit 15 0 to test regular address line
;   BP == length of segment to test
;   ES -> segment to be tested
;   SP -> different segment
;
; On return:
;   DX -> string describing last check made
;   CF set on failure
; =====================================================================
TestMem		PROC
		or_	si, si		; dispatch tail call to appropriate address handler
		jns	TestMemHiAddr
		jmp	TestMemLoAddr
		FillerNop
		ENDPROC	TestMem

; =====================================================================
; TestMemHiAddr
; Checks that the upper 8 bits of the address bus are working by
; checking that a value pushed to the stack and the inverted value
; moved to the same offset in the segment being checked read back
; differently.  If the upper address bits are stuck then the readback
; via the ES segment would actually load data from the SP segment.
;
; On entry/return:
;   See TestMem header comment
; =====================================================================
TestMemHiAddr	PROC
		mov	dx, kHiAddressLine
		push	ax		; store AX on stack
		not	ax
		mov_	di, sp		; store inverted AX via ES to same offset
		mov	[es:di], ax
		pop	cx		; attempt to poo AX into CX
					; this may read the inverted value just moved via ES
					; if the upper address bits are broken
		not	ax
		cmp_	ax, cx		; value written to different segment?
		jz	TestMemData	; yes, tail call to next test
		jmp	TestMemFail	; no, report failure
		FillerNop
		ENDPROC	TestMemHiAddr

; =====================================================================
; TestMemData
; Tests data lines and read/write to all words in segment, as well as
; the odd/even byte swap logic.
;
; On entry/return:
;   See TestMem header comment
; =====================================================================
TestMemData	PROC
		; Test data written reads back OK, one bit at a time
		mov	dx, kDataLine
		mov	di, 0		; test starts at beginning of segment
		mov	ax, 1		; test low bit first

.testDataLine	mov	[es:di], ax	; store test value
		mov	cx, [es:di]	; read it back
		cmp_	cx, ax		; did it match?
		jnz	TestMemFail
		shl	ax, 1		; shift test bit left
		jnb	.testDataLine	; continue until all bits tested

		; Test every word in the segment writes/reads OK
		mov	dx, kWriteRead
		mov_	al, bl
		mov_	ah, al
		xor	ax, 0EFh

.testWriteRead	; Fill all words in segment with test value
		mov	di, 0
		mov_	cx, bp
		rep	stosw

		; Check all words in segment read back OK
		mov	di, 0
		mov_	cx, bp
		repe	scasw
		jnz	TestMemFailCx

		; Run write/read test twice?  If not, swap AH/AL and go again
		xchg	ah, al
		cmp_	ah, bl
		jnz	.testWriteRead

		; Test first 10 words in segment read correctly when
		; accessed at odd addresses (even/odd byte swap logic).
		; Expected value (AX) produced by xchg at end of previous test.
		mov	dx, kOddEvenLogic
		mov	di, 1
		mov	cl, 0Ah
		repe	scasw
		jnz	TestMemFailCx

		; Clear all words in segment to zero (xor AX to clear
		; bits known to be set from previous successful tests)
		xor	ax, 0FF10h
		mov	di, 0
		mov_	cx, bp
		rep	stosw

		; All done!
		retn
		ENDPROC	TestMemData

; =====================================================================
; TestMemFailAddr
; Shared function tail for memory test failure of the address lines
; On return:
;   DX -> string describing failing component
;   Falls through to TestMemFailCx.
; =====================================================================
TestMemFailAddr	PROC
		mov	dx, kAddressLine
		; fall-through to following proc
		ENDPROC	TestMemFailAddr

; =====================================================================
; TestMemFailCx
; Shared function tail for memory test failure where DI contains the
; offset of the failing word within the segment (plus two).
; On return:
;   DI == offset of failing word in segment under test
;   CX == value read from failing word
;   Falls through to TestMemFail
; =====================================================================
TestMemFailCx	PROC
		sub	di, 2	; adjust for postincrement after test failure was detected
		mov	cx, [es:di]
		; fall-through to following proc
		ENDPROC	TestMemFailCx

; =====================================================================
; TestMemFail
; Shared function tail for memory test failure where AX contains the
; expected value.
; On return:
;   ES:0 == expected word
;   CF set
; =====================================================================
TestMemFail	PROC
		; [ReadMe] explicit segment override
		SEGES:mov	[0], ax
		stc
		retn
		ENDPROC	TestMemFail

; =====================================================================
; TestMemLoAddr
; Clears the specified segment to zero, checking addresses at powers of
; two can be written and read back successfully.
; On entry:
;   See TestMem header comment
; =====================================================================
TestMemLoAddr	PROC
		mov	dx, kWriteRead

		; Pick a 16-bit test value with at least one bit set and make
		; sure the segment to be tested doesn't contain it (we expect it
		; to be cleared to zero so this is a sanity check against RAM
		; being bad before we start checking).
		mov_	al, bl			; use segment index as test value
		mov_	ah, bl
		not	ax			; invert it to make sure we've got at least one set bit
						; (AX == FFFF -> 0000 would mean we're checking ROM, which won't happen)
		mov	di, 0			; check from start of segment
		mov_	cx, bp
		repe	scasw			; check all words
		jnz	TestMemFailCx		; if found, assume we've got bad RAM

		; Write the test value complement to addresses in the segment
		; that are even powers of two.  If we see these complemented values
		; anywhere else within the segment then we know we're not writing
		; (or reading) data properly.
		not	ax
		mov	di, 2
.writeSentinals	mov	[es:di], ax
		shl	di, 1
		jb	.wroteSentinals		; done if we've passed 64KB?
		cmp_	di, bp			; passed end of area under test?
		jbe	.writeSentinals		; if not, keep going

		; Scan through the segment until we find a value that is not
		; the initial test value.  We should only find them at addresses
		; where we wrote the sentinal values (addresses that are powers
		; of two).  A mismatch at any other location indicates bad RAM.
.wroteSentinals	not	ax			; restore original test value
		mov	dx, 2			; DX holds the next expected mismatch address
		mov	di, 0			; start scanning at start of segment
		mov_	cx, bp
.scanSentinals	repe	scasw			; find next mismatch
		jz	.clearSentinals		; reached end of are under test?
		add	dx, 2
		cmp_	di, dx			; mismatch at expected address?
		jnz	TestMemFailAddr		; if not, report bad RAM
		sub	dx, 2
		shl	dx, 1			; advance expected mismatch address
		jmp	.scanSentinals		; keep going

		; Once testing is complete, clear the segment back to zero
.clearSentinals	mov	di, 0
		mov_	ax, di
		mov_	cx, bp
		rep	stosw
		retn
		ENDPROC	TestMemLoAddr

; =====================================================================
; Constants for memory test errors.
; Ordering is important here: constants before kAddressLine will have
; the failing segment reported, while kAddressLine and later constants
; will have the offset and expected/actual values read/written output.
; =====================================================================
kParity		db	'parity',0
kDataLine	db	'data line',0
kOddEvenLogic	db	'odd/even logic',0
kDwordLogic	db	'double word logic',0
kHiAddressLine	db	'high '			; combines with next constant
kAddressLine	db	'address line',0
kWriteRead	db	'write/read',0

		Unused	kDwordLogic		; ???

; =====================================================================
; ReportMemError
; Writes an error message to the screen reporting a memory failure.
; On entry:
;   DX -> string describing memory subsystem where error occurred
;   BL == index of segment with error
; On entry, if DX >= kAddressLine:
;   DI == offset of error within segment
;   CX == actual value to be read/written
;   AX == expected value
; =====================================================================
ReportMemError	PROC
		push	si
		Inline	WriteString,'Memory ',0
		mov_	si, dx
		call	WriteString
		Inline	WriteString,' failure at ',0
		cmp	dx, kAddressLine
		jb	.segmentOnly

		; Report failing address and expected/actual values
		push	ax
		mov_	al, bl
		call	WriteCharHex2		; output segment
		mov_	ax, di
		call	WriteCharHex4		; output offset
		Inline	WriteString,', read ',0
		mov_	ax, cx
		call	WriteCharHex4		; output actual value
		Inline	WriteString,' expecting ',0
		pop	ax
		call	WriteCharHex4		; output expected value
		call	WriteCrLf
		pop	si
		retn				; early return

		; Report failing segment only
.segmentOnly	mov_	al, bl
		call	WriteCharHex2		; output segment start
		Inline	WriteString,'0000-',0
		mov_	al, bl
		call	WriteCharHex2		; output segment end
		Inline	WriteString,'FFFF',0Dh,0Ah,0
		pop	si
		retn
		ENDPROC	ReportMemError

; =====================================================================
; WriteMemSize
; Outputs a decimal value to the screen.
; On entry:
;   AX == value to output
; =====================================================================
WriteMemSize	PROC
		push	ax
		push	dx
		push	si
		mov	si, .divisorsEnd - .divisors
		cmp	bl, 10h			; do we need all 5 digits?
		jnb	.printDigit
		sub	si, 4			; decrease to 3 digits if not

.printDigit	xor_	dx, dx
		; ??? operand is offset backwards by 2, error by original programmer?
		div	[cs:.divisors-2+si]	; extract digit
		add	al, '0'
		call	WriteChar
		mov_	ax, dx			; retrieve remainder
		dec	si
		dec	si
		jnz	.printDigit

		pop	si
		pop	dx
		pop	ax
		retn

.divisors	dw	1
		dw	10
		dw	100
		dw	1000
		dw	10000
.divisorsEnd	ENDPROC	WriteMemSize

ENDPROGRAM	POST_PROTMODE
