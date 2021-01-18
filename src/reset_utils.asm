
; Prefix for ROM checksum failure message
kRomBadChecksum	db	'ROM bad checksum = ',0

kOptionRomSig	equ	0AA55h

; =====================================================================
; InitOptionRoms
; Searches from segment SI:0 to CX:0 on 2KB boundaries for the option
; ROM signature (AA55h).  For each option ROM found, if its checksum is
; is valid then its entrypoint is called.
;
; On entry:
; 	SI -> start segment for search
; 	CX -> end segment for search (exclusive)
;	DL == 0 if video adapter has been initialized
; 	   == 1 if no video output is available
; =====================================================================
InitOptionRoms	PROC
		xor	bp, bp,CODE=LONG

.findRom	mov	ds, si		; adjust DS to next candidate segment
		push	cx
		push	ds
		mov	si, 80h		; SI = ROM header increment (2K)
		cmp	[0], 0AA55h,DATA=WORD	; option ROM header found?
		jnz	.advanceSearch	; if not, try again

		; Load the ROM size byte (count of 512 byte pages).
		; As it will be in the upper half of CX, with the lower
		; half cleared that is equivalent to being divided by two
		; (now a count of 256 byte pages).
		mov	ch, [2]
		xor	cl, cl,CODE=LONG
		mov	si, cx,CODE=LONG
		shr	si, 3		; divide by 8 (equivalent to divide
					; by 16, making a segment address)
		push	si		; save segment address of next potential ROM
		shl	cx, 1		; multiply by two to get back original byte count
					; 
		call	ChecksumOptRom
		jnz	.badChecksum

		; Option ROM checksum/size correct, call entrypoint
		mov	es, [cs:kBdaSegment]
		mov	[es:AdapterRomOffset], 3	; TODO: struc this
		mov	[es:AdapterRomSegment], ds
		or	dl, 3		; ???
		in	al, PORT_PIC1_MASK
		push	ax		; save PIC1 mask
		push	dx		; save flag
		callf	[es:AdapterRomOffset]
		; restore state after option ROM call
		pop	dx
		pop	ax
		out	PORT_PIC1_MASK, al
		jmp	.afterRomFound

.badChecksum	push	ax
		mov	ax, ds
		call	WriteCharHex4
		Inline	WriteString,'0h Optional ',0
		pop	ax
		mov	si, kRomBadChecksum
		call	WriteBadCsumMsg

.afterRomFound	pop	si		; pop current ROM candiate location

.advanceSearch	pop	cx		; pop search end segment
		add	si, cx		; advance to next potential ROM location
		pop	cx		; pop original end segment
		cmp	si, cx		; reached the end yet?
		jb	.findRom	; continue if not

		; Completed ROM search, print a CRLF if we found any ROMs
		; just in case they printed anything without a newline.
		; If any option ROM entrypoint set BP to non-zero, then
		; set the soft reset flag on the ROMs behalf.
		test	dl, 2		; found a ROM?
		jb	.leaveProc
		call	WriteCrLf
		or	bp, bp,CODE=LONG
		jz	.leaveProc
		call	SetSoftResetFlag

.leaveProc	ENDPROC	InitOptionRoms

