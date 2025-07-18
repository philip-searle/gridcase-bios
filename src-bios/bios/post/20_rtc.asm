
; ---------------------------------------------------------------------------
; Beep to alert the user POST is nearly complete and then test that the real
; time clock is updating.
POST20_RTC	PROC
		; Enter via fall-through from previous POST code

; ---------------------------------------------------------------------------
; Skip RTC update test if display not available?
		test	[InterruptFlag], 20h
		jz	.skipRtcTest

		call	Beep

		mov	al, CMOS_STATUS_DIAG | NMI_DISABLE
		call	ReadCmos
		test	al, 80h		; RTC power off?
		jnz	.rtcTested	; Don't bother testing whether it's ticking if so
					; (power fail would have already been reported)

		mov_	bh, al		; save diagnostic byte

		mov	cx, 910		; ??? why 910 loops?
		mov	al, CHECKPOINT_RTC
		out	PORT_DIAGNOSTICS, al

; ---------------------------------------------------------------------------
		; Loop until we see the RTC update-in-progress flag set
.testUipSet	mov	al, CMOS_STATUS_A | NMI_DISABLE
		call	ReadCmos
		test	al, 80h		; RTC update in progress?
		jnz	.rtcUipSet
		dec	dl		; DL not initialized in this proc, possible bug?
					; Results in first loop being repeated average 80h times
					; Subsequent loops repeat FFh times.
		jnz	.testUipSet
		loop	.testUipSet

; ---------------------------------------------------------------------------
		; Reaching here means the update-in-progress flag didn't get
		; set within a reasonable amount of time, so report an error.
.rtcNotOk	test	[InterruptFlag], 20h
		jnz	.l1		; will always be taken, flag is checked at proc start
		mov	al, BEEP_RTC
		jmp	FatalBeeps
.l1		Inline	ConString,'Time-of-day clock stopped - please set current time',0Dh,0Ah,0
		call	SetCriticalErr
		mov	ah, CMOS_STATUS_DIAG | NMI_DISABLE
		mov_	al, bh		; restore original diagnostic byte
		call	WriteCmos	; (why? it's not been modified)

; ---------------------------------------------------------------------------
		; Bit of jumping around, not sure why
.skipRtcTest	jmp	.rtcTested

; ---------------------------------------------------------------------------
.rtcUipSet	; RTC update-in-progress flag was seen to be set.  Now let's see
		; whether it toggles back to unset.
		mov	cx, 1610	; ??? why 1610 loops?
.testUipUnset	mov	al, CMOS_STATUS_A | NMI_DISABLE
		call	ReadCmos
		sti
		test	al, 80h		; RTC update in progress?
		loopnz	.testUipUnset	; keep checking if so
		jnz	.rtcNotOk

.rtcTested	; Exit via fall-through to next POST procedure
		ENDPROC POST20_RTC

