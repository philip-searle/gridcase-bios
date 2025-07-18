
INT15		PROGRAM	OutFile=int15.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"segments/bda.inc"
		include	"cmos.inc"
		include	"diagnostics.inc"
		include	"game.inc"
		include	"int15.inc"
		include	"keyboard.inc"
		include	"pic.inc"
		include	"pit.inc"

		EXTERN	kBdaSegment, LoadBdaToDs, FuncToOffset
		EXTERN	DelayFactor, kConfigTable
		EXTERN	ReadCmos, WriteCmos
		EXTERN	Int15_Grid
		EXTERN	A20Enable, A20Disable
		EXTERN	CheckParityErr, EnableNmi
		EXTERN	ResetCpu
		EXTERN	GridCpuFast, GridCpuSlow
		EXTERN	GridToggleExt
		EXTERN	VidIncColMap, VidDecColMap

		PUBLIC	Int15_Actual
		PUBLIC	SDH_03

; =====================================================================
; Int15_Actual
; AT-compatible services and GRiD extended services.
; =====================================================================
Int15_Actual	PROC
		cmp	ah, INT15_GRID_SUBSYSTEM
		jnz	.notGridReq
		jmp	Int15_Grid

; ---------------------------------------------------------------------
; Default handler for int15/4f keyboard intercept is how GRiD implements
; the CPU speed and video colour map adjustment features
; [TechRef 8-6, 8-10, 8-13]
.notGridReq	cmp	ah, INT15_KB_INTERCEPT
		jnz	.notKbIntercept
		push	ds
		push	ax
		mov	ax, BDA_SEGMENT
		mov	ds, ax
		; Ctrl+Alt held down?
		mov	al, [KbShiftFlags1]
		and	al, KBSHIFT1_RSHIFT | KBSHIFT1_LSHIFT | KBSHIFT1_CTRL | KBSHIFT1_ALT
		cmp	al, KBSHIFT1_CTRL | KBSHIFT1_ALT
		pop	ax
		pop	ds
		jnz	.notKbIntercept

		; Ctrl+Alt+Tab toggles external video output
		cmp	al, SC2_TAB
		jnz	.notTab
		call	GridToggleExt
		jmp	.keyIntercepted
		FillerNop

		; Ctrl+Alt+PgUp sets CPU speed to fast
.notTab		cmp	al, SC2_PGUP
		jnz	.notPgUp
		call	GridCpuFast
		jmp	.keyIntercepted
		FillerNop

		; Ctrl+Alt+PgDown sets CPU speed to slow
.notPgUp	cmp	al, SC2_PGDN
		jnz	.notPgDn
		call	GridCpuSlow
		jmp	.keyIntercepted
		FillerNop

		; Ctrl+Alt+End moves forwards through the colour maps
.notPgDn	cmp	al, SC2_END
		jnz	.notEnd
		call	VidIncColMap
		jmp	.keyIntercepted
		FillerNop

		; Ctrl+Alt+Home moves backwards through the colour maps
.notEnd		cmp	al, SC2_HOME
		jnz	.notKbIntercept
		call	VidDecColMap
		; fall-through to .keyIntercepted

.keyIntercepted	clc			; let caller know we handled the key
		retf	2		; drop flags from stack

; ---------------------------------------------------------------------
; Handle a couple of functions specially before we dispatch to the real
; handler code with a jump table.
.notKbIntercept	cmp	ah, INT15_DEVICE_POST
		jz	.1
		; Enable interrupts so the device gets a chance to do stuff???
		sti

.1		cmp	ah, INT15_GET_CONFIG_TABLE
		jnz	.2
		; function code 0C0h is too large for jump table
		jmp	SysConfigTable

.2		sub	ah, 80h
		jb	.unsupported
		push	di
		mov	di, .lastHandler - .handlers
		push	ds
		call	FuncToOffset
		pop	ds
		jb	.unsupported2
		jmp	[cs:.handlers+di]

		; Jump table for int15 functions 80h-91h
		; [TechRef 3-7]
.handlers	dw	SysNoop		; 80h Device open
		dw	SysNoop		; 81h Device close
		dw	SysNoop		; 82h Program termination
		dw	SysSetTimer	; 83h Event wait
		dw	SysJoystick	; 84h Joystick support
		dw	SysNoop		; 85h Emulate SysReq key
		dw	SysWait		; 86h Wait
		dw	SysBlockMove	; 87h Block move
		dw	SysXmsSize	; 88h Determine extended memory size
		dw	SysProtMode	; 89h Processor to virtual mode
		dw	.unsupported2	; 8Ah Undefined
		dw	.unsupported2	; 8Bh Undefined
		dw	.unsupported2	; 8Ch Undefined
		dw	.unsupported2	; 8Dh Undefined
		dw	.unsupported2	; 8Eh Undefined
		dw	.unsupported2	; 8Fh Undefined
		dw	SysNoop		; 90h Device busy loop
.lastHandler	dw	SysIntDone	; 91h Set interrupt complete flag

.unsupported2	pop	di
.unsupported	mov	ah, INT15_RET_UNSUPPORTED
		stc
		retf	2
		ENDPROC	Int15_Actual

; =====================================================================
; SysBlockMove [TechRef 3-9]
; Enters protected mode to copy data to/from extended memory on behalf
; of the caller.  Expects to be called with DI on the stack.
; The return value of this proc is stored in PORT_DIAGNOSTICS so it can
; be carried across the CPU reset which transitions us back to real
; mode.  SDH_03 retrieves it and converts that to the format expected
; by the INT15_BLOCK_MOVE caller.
; =====================================================================
SysBlockMove	PROC
		pop	di		; restore saved register

; ---------------------------------------------------------------------
; Try to get access to extended memory.
		call	A20Enable
		jz	.a20Enabled
		call	A20Disable
		mov	ah, INT15_BM_A20FAILURE
		xchg	ah, al
		out	PORT_DIAGNOSTICS, al
		xchg	ah, al
		stc
		retf	2

; ---------------------------------------------------------------------
; Modify the GDT the caller passed in by filling in the code, stack,
; and GDT data selectors base values and flags.  The limits can all be
; set to the maximum of 64KB.
.a20Enabled	push	ax
		push	ds
		call	LoadBdaToDs	; we use the BDA in this section

		; assume success, we can change the return value later if needed
		xor_	al, al
		out	PORT_DIAGNOSTICS, al

		; populate code descriptor
		push	cx
		push	di
		push	bp
		mov_	di, si
		add	di, BlockMoveGdt.biosCode
		call	MakeGdtEntryCs

		; populate stack selector
		mov	ax, ss
		mov	ch, 93h		; data segment, present, dpl=0, writeable, accessed
		mov_	di, si
		add	di, BlockMoveGdt.stack
		call	MakeGdtEntry0

		; populate GDT data descriptor
		mov_	bp, si
		mov	ax, es
		mov_	di, si
		add	di, BlockMoveGdt.gdt
		mov	ch, [es:di+DESCRIPTOR.Flags]
		call	MakeGdtEntry

		pop	bp
		pop	di
		pop	cx

; ---------------------------------------------------------------------
; Set the shutdown reason so when we reset the CPU we end up running
; the block move cleanup code.  [TechRef 3-11] refers to restoring real
; mode "when Shutdown (09) is executed" even though our block move
; shutdown code is 7 (interestingly the IBM BIOS used code 9).
		mov	al, SD_PM_BLOCK_MOVE
		mov	ah, CMOS_SHUTDOWN_REASON | NMI_DISABLE
		call	WriteCmos

; ---------------------------------------------------------------------
; Save registers (DS is already pushed) and enter protected mode
		pusha
		push	es
		mov	[AdapterRomSegment], ss
		mov	[AdapterRomOffset], sp
		lgdt	[es:si+BlockMoveGdt.gdt]
		lidt	[es:BlockMoveLidt]
		lmsw	[cs:kProtModeMsw]
		PmJmp	BlockMoveGdt.biosCode, .enterPm
.enterPm	mov	ax, BlockMoveGdt.stack
		mov	ss, ax

; ---------------------------------------------------------------------
; Setup registers and copy the data
		mov	ax, BlockMoveGdt.sourceData
		mov	ds, ax
		mov	ax, BlockMoveGdt.targetData
		mov	es, ax
		xor_	si, si
		xor_	di, di
		rep movsw

; ---------------------------------------------------------------------
; Check for any parity errors and set return value if so
		in	al, PORT_KBC_PORTB
		test	al, 0C0h
		jz	.resetCpu
		mov	al, INT15_BM_PARITY_ERROR
		out	PORT_DIAGNOSTICS, al
		sub	si, 2			; backup to the last word written
		mov	ax, [si]		; read it...
		mov	[si], ax		; ...write it...
		call	CheckParityErr		; ...check parity ok

; ---------------------------------------------------------------------
; Return to real mode by resetting the CPU (it's the only way on an 80286)
.resetCpu	jmp	ResetCpu
		ENDPROC	SysBlockMove

; =====================================================================
; SDH_03
; Shutdown handler for SD_PM_BLOCK_MOVE.  Extracts the return value
; left in the diagnostic port and returns to the code that originally
; called INT15_BLOCK_MOVE.
; =====================================================================
SDH_03		PROC
		; Get us back on our original stack and restore saved registers
		mov	ds, [cs:kBdaSegment]
		mov	ss, [AdapterRomSegment]
		mov	sp, [AdapterRomOffset]
		pop	es
		popa
		pop	ds

		; Restore real-mode A20 state and enable interrupts
		call	EnableNmi
		call	A20Disable
		sti

		; Check for A20Disable failure
		in	al, PORT_DIAGNOSTICS
		jz	.retValueSet		; A20Disable call failed?
		or_	al, al			; and we don't have a failure code already?
		jnz	.retValueSet
		mov	al, INT15_BM_A20FAILURE	; set new failure code if so
		out	PORT_DIAGNOSTICS, al

.retValueSet	; Extract return value from diagnostic port and return it
		pop	ax
		xchg	al, ah
		in	al, PORT_DIAGNOSTICS
		xchg	al, ah
		cmp	[cs:kProtModeMsw+1], ah	; set flags from return value
		retf	2
		ENDPROC	SDH_03

; =====================================================================

; GDT selector used for user code after switching to protected mode
kPmSwitchCs	dw	PmSwitchGdt.cs

; Machine status word used to switch to protected mode
kProtModeMsw	dw	1

; =====================================================================
; SysProtMode [TechRef 3-11]
; Switches to protected mode and returns to the caller.
; =====================================================================
SysProtMode	PROC
		pop	di		; restore saved register

; ---------------------------------------------------------------------
; Try to get access to extended memory.
		call	A20Enable
		jz	.a20Enabled
		call	A20Disable
		mov	ah, INT15_SWITCH_PM_FAILURE
		stc
		retf	2

; ---------------------------------------------------------------------
; Convert the segment:offset return address on the stack into a
; selector:offset we can return to in protected mode
.a20Enabled	pop	ax
		add	sp, 4
		push	[cs:kPmSwitchCs]
		push	ax

; ---------------------------------------------------------------------
; Disable interrupts, poke our code selector into the caller's GDT, and
; update the PIC IVT offsets.  Then switch to protected mode.
		cli
		lgdt	[es:si+PmSwitchGdt.gdt]
		lidt	[es:si+PmSwitchGdt.idt]

		push	cx
		push	di
		push	bp
		mov_	di, si
		add	di, PmSwitchGdt.biosCode
		call	MakeGdtEntryCs
		pop	bp
		pop	di
		pop	cx

		xchg	bh, bl
		mov	dx, PORT_PIC2_CTRL
		mov	ah, 2
		call	SysRelocatePic
		mov	al, 0FFh
		out	dx, al

		xchg	bh, bl
		mov	dx, PORT_PIC1_CTRL
		mov	ah, 4
		call	SysRelocatePic
		mov	al, 0FBh
		out	dx, al

		lmsw	[cs:kProtModeMsw]
		PmJmp	PmSwitchGdt.biosCode, .enterPm
.enterPm	mov	ax, PmSwitchGdt.ds
		mov	ds, ax
		mov	ax, PmSwitchGdt.es
		mov	es, ax
		mov	ax, PmSwitchGdt.ss
		mov	ss, ax
		clc
		retf
		ENDPROC	SysProtMode

; =====================================================================
; SysXmsSize [TechRef 3-11]
; =====================================================================
SysXmsSize	PROC
		pop	di		; restore saved register
		mov	al, CMOS_EXPMEM2_HIBYTE | NMI_ENABLE
		call	ReadCmos
		mov_	ah, al
		mov	al, CMOS_EXPMEM2_LOBYTE | NMI_ENABLE
		call	ReadCmos
		sti
		clc
		retf	2
		ENDPROC	SysXmsSize

; =====================================================================
; SysNoop
; Does nothing, successfully
; =====================================================================
SysNoop		PROC
		pop	di		; restore saved register
		xor_	ah, ah
		retf	2
		ENDPROC	SysNoop

; =====================================================================
; SysIntDone
; As SysNoop, but returns with an iret to preserve the flags
; =====================================================================
SysIntDone	PROC
		pop	di		; restore saved register
		xor_	ah, ah
		iret
		ENDPROC	SysIntDone

; =====================================================================
; SysSetTimer [TechRef 3-7]
; =====================================================================
SysSetTimer	PROC
		pop	di		; restore saved register
		push	ds
		call	LoadBdaToDs
		cli			; prevent interrupts while we update BDA

		; Don't let two timers be set at the same time
		test	[TimerFlags], TIMER_SET
		stc
		jnz	.leaveFunction

		; Store timer details in BDA
		mov	[TimerFlags], TIMER_SET
		mov	[TimerValue], dx
		mov	[TimerValue+2], cx
		mov	[TimerNotifyPtrOffset], bx
		mov	[TimerNotifyPtrSegment], es
		sti

		; Enable RTC periodic interrupt
		mov	al, CMOS_STATUS_B | NMI_ENABLE
		call	ReadCmos
		and	al, 7Fh		; ensure normal clock updates
		or	al, 40h		; enable periodic interrupt
		mov	ah, CMOS_STATUS_B | NMI_ENABLE
		call	WriteCmos

		; Configure PIC to allow RTC interrupt through
		in	al, PORT_PIC2_MASK
		and	al, ~IRQ_RTC		; unmask RTC (IRQ8/int70)
		out	PORT_PIC2_MASK, al

.leaveFunction	sti
		mov	ah, 0		; return success
		pop	ds
		retf	2
		ENDPROC	SysSetTimer

; =====================================================================
; SysWait [TechRef 3-8]
; =====================================================================
SysWait		PROC
		pop	di		; restore saved register
		push	ds
		call	LoadBdaToDs
		cli			; prevent interrupts while we update BDA

		; Don't let two timers be set at the same time
		test	[TimerFlags], TIMER_SET
		stc
		jnz	.leaveFunction

		; Store timer details in BDA
		mov	[TimerFlags], TIMER_SET
		mov	[TimerValue], dx
		mov	[TimerValue+2], cx
		mov	[TimerNotifyPtrOffset], TimerFlags
		mov	[TimerNotifyPtrSegment], ds

		; Enable RTC periodic interrupt
		mov	al, CMOS_STATUS_B | NMI_ENABLE
		call	ReadCmos
		and	al, 7Fh		; ensure normal clock updates
		or	al, 40h		; enable periodic interrupt
		mov	ah, CMOS_STATUS_B | NMI_ENABLE
		call	WriteCmos

		; Configure PIC to allow RTC interrupt through
		in	al, PORT_PIC2_MASK
		and	al, ~IRQ_RTC		; unmask RTC (IRQ8/int70)
		out	PORT_PIC2_MASK, al
		sti

		; Spinloop until timer expires
		mov	ax, 620			; ???
		div	[cs:DelayFactor], DATA=BYTE
		mov	ah, 0			; ignore remainder
		push	cx
		push	dx
.spinloop	Delay	2
		nop
		Delay	2
		sub_	dx, ax			; decrement time remaining
		sbb	cx, 0
		jb	.waitComplete		; count of microseconds complete?
		test	[TimerFlags], TIMER_EXPIRED
		jz	.spinloop

.waitComplete	clc				; return success
		pop	dx
		pop	cx
		mov	[TimerFlags], 0
.leaveFunction	sti
		mov	ah, 0
		pop	ds
		retf	2
		ENDPROC	SysWait

; =====================================================================
; SysJoystick
; [TechRef 3-7] documents this as "joystick support - not used" even
; though it's fully implemented.  The GRiDCase 1500 series don't have
; built-in game ports -- are they also missing hardware that causes
; this code to fail?  TODO: check this.
; =====================================================================
SysJoystick	PROC
		cmp	dl, 1			; subfunction 1 reads joystick positions
		mov	dx, PORT_GAME
		jz	.readPositions
		pop	di
		jb	.readSwitches		; subfunction 0 reads switches

		; no other subfunctions supported
		mov	ah, INT15_RET_UNSUPPORTED
		stc
		retf	2

; ---------------------------------------------------------------------
; Reading switches is a simple port read.
.readSwitches	in	al, dx
		and	ax, GAME_BUTTON_MASK
		retf	2

; ---------------------------------------------------------------------
; Reading positions is more complicated: we must activate the built-in
; one-shot timer by writing to the game port and then spinloop until we
; detect the joystick axis bit changing from 0 to 1.
.readPositions	mov	ah, 1			; start by reading joystick A, X axis
		mov	cx, 4			; read 2 joysticks, 2 axes each

.readValue	push	cx
		mov	cx, GAME_READ_TIMEOUT
		cli				; don't allow interrupts to throw us off
		call	SysReadCount0		; store initial counter valur in DI
		mov_	di, bx

		out	dx, al			; dummy write to trigger game port one-shot
		mov	bx, 0

.waitOneShot	in	al, dx			; read joystick status
		test_	ah, al			; one-shot expired?
		loopne	.waitOneShot
		jnz	.resetGamePort
		call	SysReadCount0		; read new counter and subtract from the
		sti				; initial value, then convert to axis position
		neg	bx
		add_	bx, di
		mov	cl, 4
		shr	bx, cl
		and	bh, 1

.resetGamePort	sti
		mov	cx, GAME_READ_TIMEOUT
.waitPortReset	in	al, dx			; wait for all four one-shot timers to expire
		test	al, GAME_POSITION_MASK
		loopne	.waitPortReset

		pop	cx
		shl	ah, 1			; advance to next value
		push	bx			; store axis value on stack
		loop	.readValue

		; pop axis values off the stack into return registers
		pop	dx
		pop	cx
		pop	bx
		pop	ax
		pop	di
		retf	2
		ENDPROC	SysJoystick

; =====================================================================
; SysConfigTable [TechRef 3-8]
; =====================================================================
SysConfigTable	PROC
		push	cs
		pop	es
		mov	bx, kConfigTable
		mov	ah, 0
		clc
		retf	2
		ENDPROC	SysConfigTable

; =====================================================================
; BlockMoveFail
; Handler for any interrupts that occur while performing a block move
; operation in protected mode.  Sets an error code and immediately
; resets the CPU to return to real mode.
; =====================================================================
BlockMoveFail	PROC
		push	ax
		mov	al, INT15_BM_CPU_EXCEPTION
		out	PORT_DIAGNOSTICS, al
		pop	ax
		jmp	ResetCpu
		ENDPROC	BlockMoveFail

; =====================================================================
; Protected-mode interrupt descriptor table and LIDT load data used
; when performing a block move operation.
BlockMoveLidt	dw	(BlockMoveIdt.l - BlockMoveIdt)
		dw	BlockMoveIdt
		db	0Fh
		db	93h
		dw	0

BlockMoveIdt	; give name to start of IDT
i		%FOR	1..32
		IDT_ENTRY Offset=BlockMoveFail, Selector=BlockMoveGdt.biosCode
		%ENDFOR	i
BlockMoveIdt.l	; give name to end of IDT

; =====================================================================
; MakeGdtEntryCs
; As MakeGdtEntry, but always builds a code segment based at CS:0.
; =====================================================================
MakeGdtEntryCs	PROC
		mov	ch, 9Bh		; code segment, present, dpl=0, accessed
		mov	ax, cs
		; fall-through to MakeGdtEntry0
		ENDPROC	MakeGdtEntryCs

; =====================================================================
; MakeGdtEntry0
; As MakeGdtEntry, but always builds a segment based at AX:0.
; =====================================================================
MakeGdtEntry0	PROC
		mov	bp, 0
		; fall-through to MakeGdtEntry
		ENDPROC	MakeGdtEntry0

; =====================================================================
; MakeGdtEntry
; Updates a GDT descriptor with a new base and access rights.
; The descriptor limit will be reset to 0FFFFh.
; On entry:
;   ES:DI -> descriptor to update
;   AX:BP == new descriptor base
;   CH    == new access rights
; =====================================================================
MakeGdtEntry	PROC
		push	ax
		push	cx
		mov	cl, 4
		rol	ax, cl
		mov_	ch, al
		and	ch, 0Fh
		and	al, 0F0h
		add_	ax, bp
		adc	ch, 0
		mov	[es:di+DESCRIPTOR.Limit], 0FFFFh
		mov	[es:di+DESCRIPTOR.BaseLo], ax
		mov	[es:di+DESCRIPTOR.BaseHi], ch
		pop	cx
		mov	[es:di+DESCRIPTOR.Flags], ch
		mov	[es:di+DESCRIPTOR.Reserved], 0
		pop	ax
		retn
		ENDPROC	MakeGdtEntry

; =====================================================================
; SysRelocatePic
; Adjusts the specified Programmable Interrupt Controller's IVT offset.
; On entry:
;   DX == PIC control port to adjust
;   BH == PIC ICW2 byte (IVT offset)
;   AH == PIC ICW3 byte
; =====================================================================
SysRelocatePic	PROC
		mov	al, 11h		; ICW1: reinit PIC
		out	dx, al
		inc	dx		; advance to PIC mask register
		mov_	al, bh
		Delay	2
		out	dx, al		; ICW2: set IVT offset
		mov_	al, ah
		Delay	2
		out	dx, al		; ICW3: connect master/slave PICs
		mov	al, 1
		Delay	2
		out	dx, al		; ICW4
		retn
		ENDPROC	SysRelocatePic

; =====================================================================
; SysReadCount0
; Reads PIT channel 0 and returns the current value in BX.
; =====================================================================
SysReadCount0	PROC
		push	dx
		mov	dx, PORT_PIT_MODE
		mov	al, 0		; select counter 0, latch current value
		out	dx, al
		dec	dx		; decrement to PORT_PIT_COUNTER0
		dec	dx
		dec	dx
		Delay	1
		in	al, dx
		mov_	bl, al
		Delay	2
		in	al, dx
		mov_	bh, al
		pop	dx
		retn
		ENDPROC	SysReadCount0

ENDPROGRAM	INT15
