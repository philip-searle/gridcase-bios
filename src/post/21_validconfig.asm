
; ---------------------------------------------------------------------------
; Check various CMOS configuration settings are as expected and warn the user
; if they are not.
POST21_ConfigOk	PROC
		; Enter via fall-through from previous POST code

		mov	al, CMOS_STATUS_DIAG | NMI_DISABLE
		call	ReadCmos
		mov_	bh, al
					; BH contains diagnostic bits and will
					; be updated as this proc proceeded
		mov	cl, 2		; CL contains the ID of the most recent
					; check performed by this proc

; ---------------------------------------------------------------------------
; Alert if RTC power or CMOS checksum are bad.
		test	bh, 80h		; is RTC power on?
		jnz	.invalidConfig

		inc	cl
		test	bh, 40h		; is CMOS checksum good?
		jnz	.invalidConfig

; ---------------------------------------------------------------------------
; Alert if detected base or expansion memory sizes differ from CMOS config.
		or	bh, 10h		; assume memory size is not good

		inc	cl
		mov	al, CMOS_BASEMEM_LOBYTE | NMI_DISABLE
		call	ReadCmos
		cmp	al, [MemorySizeKb]
		jnz	.memSizeInvalid

		inc	cl
		mov	al, CMOS_BASEMEM_HIBYTE | NMI_DISABLE
		call	ReadCmos
		cmp	al, [MemorySizeKb+1]
		jnz	.memSizeInvalid

		inc	cl
		mov	al, CMOS_EXPMEM_LOBYTE | NMI_DISABLE
		call	ReadCmos
		mov_	bl, al
		mov	al, CMOS_EXPMEM2_LOBYTE | NMI_DISABLE
		call	ReadCmos
		cmp_	bl, al
		jnz	.memSizeInvalid

		inc	cl
		mov	al, CMOS_EXPMEM_HIBYTE | NMI_DISABLE
		call	ReadCmos
		mov_	bl, al
		mov	al, CMOS_EXPMEM2_HIBYTE | NMI_DISABLE
		call	ReadCmos
		cmp_	bl, al
		jnz	.memSizeInvalid

		inc	cl
		and	bh, 0EFh	; mark memory size as good
		test	bh, 20h		; power on config good (for things tested earlier in POST)?
		jz	.configOk

; ---------------------------------------------------------------------------
; Handle outcome of config checks
.memSizeInvalid	or	bh, 20h		; mark memory size as not good (again)

.invalidConfig	; Rewrite CMOS diagnostic byte with updated value and report error
		mov_	al, bh
		mov	ah, CMOS_STATUS_DIAG | NMI_DISABLE
		call	WriteCmos
		sti

		Inline	ConString,'Invalid configuration information; code ',0
		mov_	al, cl
		xor_	ah, ah
		call	ConCharHex2
		Inline	ConString,0Dh,0Ah,0
		call	SetCriticalErr
		; fall-through into .configOk path

.configOk	sti			; make sure all paths that get here re-enable interrupts

		; Exit via fall-through to next POST procedure
		ENDPROC POST21_ConfigOk

