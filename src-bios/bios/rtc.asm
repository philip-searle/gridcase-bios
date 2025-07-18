
RTC		PROGRAM	OutFile=rtc.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"segments/bda.inc"
		include	"cmos.inc"
		include	"fdc.inc"
		include	"pic.inc"

		EXTERN	LoadBdaToDs, FuncToOffset
		EXTERN	ReadCmos, WriteCmos

		PUBLIC	Int8_Actual, Int1A_Actual, Int70

; =====================================================================
; Int1A_Actual [TechRef 6-8]
; =====================================================================
Int1A_Actual	PROC
		sti
		push	ds
		call	LoadBdaToDs

		; Dispatch to appropriate subfunction handler
		sub	ah, 1
		jb	RtcReadTod
		push	di
		mov	di, .lastHandler - .handlers
		call	FuncToOffset
		jb	.leaveFailure
		jmp	[cs:di+.handlers]

.handlers	dw	RtcSetTod
		dw	RtcReadTime
		dw	RtcSetTime
		dw	RtcReadDate
		dw	RtcSetDate
		dw	RtcSetAlarm
.lastHandler	dw	RtcResetAlarm

; ---------------------------------------------------------------------
; Shared function tail for int1A handlers.
; Writes the value in AL to CMOS register AH, then performs an iret
; leaving 0 in AH, interrupts enabled, and DI and DS popped from the
; stack.
.writeAndLeave	call	WriteCmos
		; fall-through to .leaveSuccess

; ---------------------------------------------------------------------
; Shared function tail for int1A handlers.
; Performs an iret after setting AH to 0, enabling interrupts, and
; popping DI and DS from the stack.
.leaveSuccess	mov	ah, 0
		sti
		pop	di
		pop	ds
		iret

; ---------------------------------------------------------------------
; Shared function tail for int1A handlers.
; Enables interrupts, pops DI and DS from the stack, then returns using
; a retf to preserve the flags.
.leaveFailure	sti
		pop	di
		pop	ds
		retf	2
		ENDPROC	Int1A_Actual

; =====================================================================
; RtcSetTod [TechRef 6-8]
; Sets the time-of-day clock, also resetting the overflow count.
; =====================================================================
RtcSetTod	PROC
		mov	[TimerTicks], dx
		mov	[TimerTicks+2], cx
		xor_	ah, ah
		mov	[TimerOverflow], ah
		pop	di
.leaveFunction	; function tail shared with RtcReadTod
		sti
		pop	ds
		iret
		ENDPROC	RtcSetTod

; =====================================================================
; RtcReadTod [TechRef 6-8]
; Returns timer tick count in CX:DX and overflow flag in AH.
; Resets the overflow flag after reading it.
; =====================================================================
RtcReadTod	PROC
		mov	dx, [TimerTicks]
		mov	cx, [TimerTicks+2]
		xor_	ax, ax
		xchg	al, [TimerOverflow]
		jmp	RtcSetTod.leaveFunction
		ENDPROC	RtcReadTod

; =====================================================================
; Int8_Actual [TechRef 6-10]
; Interrupt handler for PIT time-of-day interrupt.
; Maintains the time-of-day tick count and overflow flag in the BDA, as
; well as turning off the floppy drive motors when their timer expires.
; =====================================================================
Int8_Actual	PROC
		push	ds
		push	ax
		push	dx
		call	LoadBdaToDs

; ---------------------------------------------------------------------
; Increment the time-of-day counter, handling rollover between low and
; high components, as well as rollover at midnight.
		mov	ax, [TimerTicks]
		mov	dx, [TimerTicks+2]
		inc	ax
		jnz	.1
		inc	dx		; rolled over low component
.1		cmp	dx, TIMER_TICKS_PER_DAY >> 16
		ja	.wrapMidnight
		jb	.storeNewTod
		cmp	ax, TIMER_TICKS_PER_DAY & 0FFFFh
		jb	.storeNewTod
.wrapMidnight	; increment the overflow instead of setting it in case user
		; code doesn't check the TOD for more than 24 hours (i.e.
		; treat it as a count of overflows instead of a flag).
		inc	[TimerOverflow]
		xor_	ax, ax
		xor_	dx, dx

.storeNewTod	mov	[TimerTicks], ax
		mov	[TimerTicks+2], dx

; ---------------------------------------------------------------------
; Decrement the floppy motor timeout and turn off the motors if it has
; reached zero.
		dec	[FdMotorTimeout]
		jnz	.fdMotorDone
		mov	dx, PORT_FDC_DIGOUT
		mov	al, FDC_DIGOUT_DMAINT | FDC_DIGOUT_RESET
		out	dx, al
		and	[FdMotorStatus], 0F0h

; ---------------------------------------------------------------------
; Notify the user interrupt then let the PIC know we're done
.fdMotorDone	int	1Ch
		mov	al, NONSPECIFIC_EOI
		out	PORT_PIC1_CTRL, al
		pop	dx
		pop	ax
		pop	ds
		iret
		ENDPROC	Int8_Actual

; =====================================================================
; RtcSetTime [TechRef 6-9]
; =====================================================================
RtcSetTime	PROC
		call	RtcStopClock
		mov	ah, CMOS_RTC_SECONDS | NMI_ENABLE
		mov_	al, dh
		call	WriteCmos
		mov	ah, CMOS_RTC_MINUTES | NMI_ENABLE
		mov_	al, cl
		call	WriteCmos
		mov	ah, CMOS_RTC_HOURS | NMI_ENABLE
		mov_	al, ch
		call	WriteCmos

		; Re-enable clock updates
		mov	al, CMOS_STATUS_B | NMI_ENABLE
		mov_	ah, al
		call	ReadCmos
		and	al, 7Eh
		push	dx
		and	dl, 1		; merge daylight-savings bit
		or_	al, dl
		pop	dx
		jmp	Int1A_Actual.writeAndLeave
		ENDPROC	RtcSetTime

; =====================================================================
; RtcReadTime [TechRef 6-9]
; =====================================================================
RtcReadTime	PROC
		call	RtcWaitUpdate
		jc	RtcReadDate.leaveFailure
		mov	al, CMOS_RTC_SECONDS | NMI_ENABLE
		call	ReadCmos
		mov_	dh, al
		mov	al, CMOS_RTC_MINUTES | NMI_ENABLE
		call	ReadCmos
		mov_	cl, al
		mov	al, CMOS_STATUS_B | NMI_ENABLE
		call	ReadCmos
		mov_	dl, al
		and	dl, 1		; isolate daylight-savings bit
		mov	al, CMOS_RTC_HOURS | NMI_ENABLE
		jmp	RtcReadDate.readAndLeave

		and	al, 3		; ??? dead code
		ENDPROC	RtcReadTime

; =====================================================================
; RtcReadDate [TechRef 6-9]
; =====================================================================
RtcReadDate	PROC
		call	RtcWaitUpdate
		jc	.leaveFailure
		mov	al, CMOS_RTC_DAY_OF_MONTH | NMI_ENABLE
		call	ReadCmos
		mov_	dl, al
		mov	al, CMOS_RTC_MONTH | NMI_ENABLE
		call	ReadCmos
		mov_	dh, al
		mov	al, CMOS_RTC_YEAR | NMI_ENABLE
		call	ReadCmos
		mov_	cl, al
		mov	al, CMOS_CENTURY | NMI_ENABLE
.readAndLeave	; function tail shared with RtcReadTime
		call	ReadCmos
		mov_	ch, al
		jmp	Int1A_Actual.leaveSuccess

.leaveFailure	; function tail shared with RtcReadDate
		jmp	Int1A_Actual.leaveFailure
		ENDPROC	RtcReadDate

; =====================================================================
; RtcSetDate [TechRef 6-9]
; =====================================================================
RtcSetDate	PROC
		call	RtcStopClock
		mov	ah, CMOS_RTC_DAY_OF_MONTH | NMI_ENABLE
		mov_	al, dl
		call	WriteCmos
		mov	ah, CMOS_RTC_MONTH | NMI_ENABLE
		mov_	al, dh
		call	WriteCmos
		mov	ah, CMOS_RTC_YEAR | NMI_ENABLE
		mov_	al, cl
		call	WriteCmos
		mov	ah, CMOS_CENTURY | NMI_ENABLE
		mov_	al, ch
		call	WriteCmos
		; re-enable clock updates
		mov	al, CMOS_STATUS_B | NMI_ENABLE
		mov_	ah, al
		call	ReadCmos
		and	al, 7Fh
		jmp	Int1A_Actual.writeAndLeave
		ENDPROC	RtcSetDate

; =====================================================================
; RtcSetAlarm [Techref 6-9]
; =====================================================================
RtcSetAlarm	PROC
		mov	al, CMOS_STATUS_B | NMI_ENABLE
		call	ReadCmos
		test	al, 20h		; alarm already enabled?
		jnz	.leaveFailure

		call	RtcStopClock
		mov	ah, CMOS_ALARM_SECONDS | NMI_ENABLE
		mov_	al, dh
		call	WriteCmos
		mov	ah, CMOS_ALARM_MINUTES | NMI_ENABLE
		mov_	al, cl
		call	WriteCmos
		mov	ah, CMOS_ALARM_HOURS | NMI_ENABLE
		mov_	al, ch
		call	WriteCmos

		; Re-enable clock updates and turn on the alarm interrupt
		mov	al, CMOS_STATUS_B | NMI_ENABLE
		mov_	ah, al
		call	ReadCmos
		and	al, 7Fh		; enable clock updates
		or	al, 20h		; enable alarm interrupt
		call	WriteCmos
		in	al, PORT_PIC2_MASK
		and	al, ~IRQ_RTC	; unmask rtc interrupt
		Delay	2
		out	PORT_PIC2_MASK, al
		jmp	Int1A_Actual.leaveSuccess

.leaveFailure	xor_	ax, ax
		stc
		jmp	Int1A_Actual.leaveFailure
		ENDPROC	RtcSetAlarm

; =====================================================================
; RtcResetAlarm [TechRef 6-9]
; =====================================================================
RtcResetAlarm	PROC
		mov	al, CMOS_STATUS_B | NMI_ENABLE
		mov_	ah, al
		call	ReadCmos
		and	al, 57h		; disable alarm interrupt (and square wave freq???)
		jmp	Int1A_Actual.writeAndLeave
		ENDPROC	RtcResetAlarm

; =====================================================================
; Int70 [TechRef 6-10]
; Handles periodic and alarm interrupts.
; =====================================================================
Int70		PROC
		push	ax
		push	dx
		push	di
		push	ds

		; Check whether timer is enabled
		mov	al, CMOS_STATUS_B | NMI_ENABLE
		call	ReadCmos
		mov_	ah, al
		mov	al, CMOS_STATUS_C | NMI_ENABLE
		call	ReadCmos
		and_	al, ah		; isolate interrupt bits
		test	al, 40h		; periodic interrupt fired?
		jz	.timerHandled

		; Decrement timer and see if it's expired
		call	LoadBdaToDs
		sub	[TimerValue], 3D0h, DATA=WORD	; ???
		sbb	[TimerValue+2], 0, DATA=WORD
		jnb	.timerHandled

		; Notify user that timer has expired and reset it
		mov	[TimerFlags], 0
		lds	di, [TimerNotifyPtrOffset]
		or	[di], TIMER_EXPIRED, DATA=BYTE
		mov_	di, ax
		mov_	al, ah
		and	al, 3Fh		; disable periodic interrupt
		mov	ah, CMOS_STATUS_B | NMI_ENABLE
		call	WriteCmos
		mov_	ax, di

.timerHandled	; Check whether alarm is enabled
		test	al, 20h
		jz	.alarmHandled
		int	4Ah		; notify user alarm has expired

.alarmHandled	; acknowledge interrupt
		mov	al, NONSPECIFIC_EOI
		out	PORT_PIC2_CTRL, al
		out	PORT_PIC1_CTRL, al
		pop	ds
		pop	di
		pop	dx
		pop	ax
		iret
		ENDPROC	Int70

; =====================================================================
; RtcStopClock
; Resets RTC to default timing and disables clock updates.
; Returns CMOS RAM valid flag in AL bit 7.
; =====================================================================
RtcStopClock	PROC
		call	RtcWaitUpdate
		mov	ah, CMOS_STATUS_A | NMI_ENABLE
		mov	al, 26h			; set default timings
		call	WriteCmos
		mov	ah, CMOS_STATUS_B | NMI_ENABLE
		mov	al, 82h			; disable clock updates
		call	WriteCmos
		mov	al, CMOS_STATUS_C | NMI_ENABLE
		call	ReadCmos		; read to clar interrupt flags
		mov	al, CMOS_STATUS_D | NMI_ENABLE
		jmp	ReadCmos		; read to get (and clear) CMOS valid bit
		ENDPROC	RtcStopClock

; =====================================================================
; RtcWaitUpdate
; Waits for the RTC to not be in the middle of an update cycle (i.e.
; for it to be safe to perform a clock update).
; Returns AX unmodified, CF clear, interrupts disabled on success.
; Return AX=0, CF set, interrupts enabled on failure.
; =====================================================================
RtcWaitUpdate	PROC
		push	cx
		push	ax
		xor_	cx, cx		; wait max loops
.checkRtc	sti			; don't disable interrupts while waiting
		mov	al, CMOS_STATUS_A | NMI_ENABLE
		call	ReadCmos	; interrupts disabled by this call
		test	al, 80h		; update in progress?
		loopne	.checkRtc
		pop	ax
		jz	.leaveFunction
		xor_	ax, ax
		sti
		stc
.leaveFunction	pop	cx
		retn
		ENDPROC	RtcWaitUpdate

ENDPROGRAM	RTC
