
POST_RESET_UTIL	PROGRAM	OutFile=reset_utils.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"segments/bda.inc"
		include	"pic.inc"

		EXTERN	kBdaSegment
		EXTERN	ChecksumRom
		EXTERN	ConCharHex4, ConCrLf, ConString_Inline
		EXTERN	ConBadCsumMsg
		EXTERN	SetCriticalErr

		PUBLIC	kRomBadChecksum
		PUBLIC	InitOptionRoms
		PUBLIC	TestDmaRegs
		PUBLIC	TestPicMaskReg

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
		xor_	bp, bp

.findRom	mov	ds, si		; adjust DS to next candidate segment
		push	cx
		push	ds
		mov	si, 80h		; SI = ROM header increment (2K)
		cmp	[0], kOptionRomSig,DATA=WORD	; option ROM header found?
		jnz	.advanceSearch	; if not, try again

		; Load the ROM size byte (count of 512 byte pages).
		; As it will be in the upper half of CX, with the lower
		; half cleared that is equivalent to being divided by two
		; (now a count of 256 byte pages).
		mov	ch, [2]
		xor_	cl, cl
		mov_	si, cx
		shr	si, 3		; divide by 8 (equivalent to divide
					; by 16, making a segment address)
		push	si		; save segment address of next potential ROM
		shl	cx, 1		; multiply by two to get back original byte count
					;
		call	ChecksumRom
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
		call	ConCharHex4
		Inline	ConString,'0h Optional ',0
		pop	ax
		mov	si, kRomBadChecksum
		call	ConBadCsumMsg

.afterRomFound	pop	si		; pop current ROM candidate location

.advanceSearch	pop	cx		; pop search end segment
		add_	si, cx		; advance to next potential ROM location
		pop	cx		; pop original end segment
		cmp_	si, cx		; reached the end yet?
		jb	.findRom	; continue if not

		; Completed ROM search, print a CRLF if we found any ROMs
		; just in case they printed anything without a newline.
		; If any option ROM entrypoint set BP to non-zero, then
		; require the user to acknowledge whatever message the ROM displayed.
		test	dl, 2		; found a ROM?
		jz	.leaveProc
		call	ConCrLf
		or_	bp, bp
		jz	.leaveProc
		call	SetCriticalErr

.leaveProc	retn
		ENDPROC	InitOptionRoms

; =====================================================================
; TestPicMaskReg
; Tests that all bits in a PIC mask register can be written and read
; with both zero and one.
;
; On entry:
; 	DX == PIC mask register port number
;
; On return:
; 	ZF set on success
; =====================================================================
TestPicMaskReg	PROC
		mov	cx, 10h		; 8 registers, each tested twice
		mov	bh, 7Fh		; initial test pattern, write zero

.testRegister	mov_	al, bh		; write PIC mask register
		out	dx, al
		Delay	2

		in	al, dx		; read back value writtem
		cmp_	bh, al		; same?
		jnz	.leaveProc	; if not, return with ZF clear

		sar	bh, 1		; shift test pattern over one bit
		cmp	cl, 9		; half-way through testing?
		jnz	.l1		; invert the test pattern if so
		mov	bh, 80h
.l1		loop	.testRegister

		; Successful test if we make it here
		xor_	al, al

.leaveProc	retn
		ENDPROC	TestPicMaskReg

; =====================================================================
; TestDmaRegs
; Tests that all bits in a DMA controller's address/length registers
; can be written and read with both zero and one.
;
; On entry:
; 	DX == DMA controller register port number
; 	CX == number of registers to test
; 	SI == increment between adjacent registers
;
; On return:
; 	CF set on failure
; =====================================================================
TestDmaRegs	PROC
		mov	bx, 8000h	; test pattern: one bit set
		push	cx		; preserve register count
		mov	cx, 32		; load count of bits per register

.testBit	mov_	al, bh		; write register hibyte
		out	dx, al
		Delay	2
		xchg	ax, bx		; write register lobyte
		out	dx, al
		Delay	2
		xchg	ax, bx

		; Read back register value and compare
		in	al, dx		; read hibyte
		Delay	2
		xchg	ah, al
		in	al, dx		; read lobyte
		cmp_	bx, ax
		jnz	.failure

		sar	bx, 1		; shift test pattern over one bit
		cmp	cl, 17		; halfway through this register?
		jnz	.l1		; invert test pattern if so
		mov	bh, 7Fh		; BL is already FFh from shifting
.l1		loop	.testBit

		; Successful test of register if we make it here.
		add_	dx, si		; advance to next register
		pop	cx		; restore register count
		loop	TestDmaRegs	; go again if we need to
		retn

.failure	pop	cx
		stc
		retn
		ENDPROC	TestDmaRegs

ENDPROGRAM	POST_RESET_UTIL
