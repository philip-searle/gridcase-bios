
POST04_BiosCsum	PROC
		; Enter via fall-through from previous POST code

; ---------------------------------------------------------------------------
; Checksum BIOS ROM content
		mov	al, CHECKPOINT_BIOS_CSUM
		out	PORT_DIAGNOSTICS, al

		; ChecksumRom routine expects a near call, so simulate one
		; with a hardcoded return stack in ROM and a JMP (SS:SP is
		; set but we haven't yet run the RAM test or started the DMA
		; controller refreshing DRAM yet, so we can't use a real stack).
		mov	cx, BIOS_START_PARA	; CX is checksum start address
		mov	ax, cs
		mov	ss, ax
		mov	sp, .returnStack
		jmp	ChecksumRom

.returnStack	dw	.checksumDone

.checksumDone	jz	.checksumOk		; is our EPROM OK?
		mov	al, BEEP_BIOS_CHECKSUM
		jmp	FatalBeeps

.checksumOk	; Exit via fall-through to next POST procedure
		ENDPROC POST04_BiosCsum

