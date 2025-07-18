
POST13_CmosDiag	PROC

; ---------------------------------------------------------------------------
; Update CMOS diagnostic byte
		mov	al, CHECKPOINT_CMOS_CSUM
		out	PORT_DIAGNOSTICS, al

		jmp	.checkCmosCsum
.decAndReadCmos	dec	bl			; decrement CMOS register number
		mov_	al, bl
		jmp	ReadCmos		; tail call

		; Autodetect hardware changes where possible and then
		; check the power fail bit in CMOS
.checkCmosCsum	call	GridAutodetect
		mov	al, CMOS_STATUS_D | NMI_DISABLE
		call	ReadCmos
		and	al, 80h			; ZF set == RTC has lost power
		pushf				; save result
		mov	al, CMOS_STATUS_DIAG | NMI_DISABLE
		call	ReadCmos
		and	al, 90h			; Preserve power bad and mem size bad flags
		popf				; restore prev result
		jnz	.rtcPwrChecked
		or	al, 80h			; set power bad flag if power previously lost

		; Some diagnostic bits can be cleared on soft reset
.rtcPwrChecked	sti
		mov	bx, [SoftResetFlag]
		and	bl, ~CRITICAL_ERR_FLAG	; mask off unrelated flag
		cmp	bx, SOFT_RESET_FLAG
		jz	.rtcNotSoft		; soft reset?
		and	al, 0EFh		; clear mem size OK flag on soft reset (we autodetected the new memsize)

		; Read stored CMOS checksum
.rtcNotSoft	mov_	bh, al			; store diagnostic bte for later
		mov	bl, CMOS_EXPMEM2_LOBYTE | NMI_DISABLE
		call	.decAndReadCmos
		mov_	cl, al
		call	.decAndReadCmos
		mov_	ch, al

		; Calculate new checksum and compare to stored one
		mov	ah, 0			; CMOS checksum is calulated one byte at a time
.calcCmosCsum	call	.decAndReadCmos
		sub_	cx, ax
		cmp	bl, CMOS_FD_TYPE | NMI_DISABLE
		jnz	.calcCmosCsum
		jcxz	.rtcCsumDone		; CMOS checksum matched?
		or	bh, 40h			; record if it didn't

.rtcCsumDone	mov_	al, bh			; store new diagnostic byte
		mov	ah, CMOS_STATUS_DIAG | NMI_DISABLE
		call	WriteCmos
		sti

		; Exit via fall-through to next POST procedure
		ENDPROC POST13_CmosDiag

