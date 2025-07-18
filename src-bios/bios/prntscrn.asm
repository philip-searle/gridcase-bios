
PRNTSCRN	PROGRAM	OutFile=prntscrn.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"segments/bda.inc"
		include	"video.inc"

		EXTERN	LoadBdaToDs

		PUBLIC	PrntScrn_Actual

; ---------------------------------------------------------------------
; PrntScrn_Actual
; Handler for the "Print Screen" key.
; Supports text mode displays only.
; ---------------------------------------------------------------------
PrntScrn_Actual	PROC
		push	ax
		push	ds
		call	LoadBdaToDs

		; Mark print screen operation as started.  Don't allow
		; a second one to be started if one is already in progress.
		mov	al, PRNTSCRN_INPROGRESS
		xchg	al, [PrtScrStatus]
		dec	al
		jz	.leaveFunction

		; Print screen takes some time, so to keep the time-of-day
		; counter keeps working we must run with interrupts enabled
		sti
		push	bx
		push	cx
		push	dx

		xor_	dx, dx		; LPT1
		mov	ah, 2		; Get printer status
		int	17h
		xor	ah, 80h		; invert "not busy" bit
		test	ah, 0A0h	; test "busy" and "acknowledge" bits
		jz	.printerReady
		mov	bl, PRNTSCRN_ERROR
		jmp	.leaveFunction2
		FillerNop

.printerReady	mov	ah, 0Fh		; Get current video mode
		int	10h
		mov_	bl, ah

		mov	ah, 3		; Read cursor position
		int	10h
		push	dx		; save cursor position

		mov	cl, 25		; calculate screen rows
		cmp	[VidActiveMode], VID_MODE_EXT_48
		jnz	.printScreen
		mov	cl, 50

		; Main print loop.  Registers used are:
		;   AL == current character being printed
		;   DH == current row being printed
		;   DL == current column being printed
		;   CL == terminal count for rows
		;   BL == terminal count for columns
		; Current row/character rely on 0FFh wrapping back to 00h
		; and incrementing the upper byte to handle end-of-row.
.printScreen	mov	dh, 0FFh
		jmp	.nextLine

.readNextChar	mov	ah, 2		; Set cursor location to DH=row, DL=col
		int	10h

		mov	ah, 8		; Read character at cursor location
		int	10h

		; Check for CR, LF, VT, and FF characters
		; Handle them specially.
		sub	al, 10		; shift character so LF=0
		jz	.replaceWithSp	; LF gets replaced with space
		dec	ax		; shift character to VT=0
		jz	.printChar	; VT gets printed as-is?
		cmp	al, 2		; FF and CR get replaced with space
		ja	.printChar

.replaceWithSp	mov	al, 21
.printChar	add	al, 11
		jz	.replaceWithSp	; NUL gets replaced with space
		call	PrintChar
		jnz	.printingDone
		inc	dx		; advance column
		cmp_	dl, bl		; reached last column?
		jb	.readNextChar

.nextLine	call	PrintCrLf
		jnz	.printingDone	; end if reached last line
		mov	dl, 0FFh	; reset column count
		inc	dx		; increment row, zero column
		cmp_	dh, cl		; reached last row?
		jb	.readNextChar

		xor_	bl, bl		; prepare new status byte
.printingDone	pop	dx		; restore cursor position
		mov	ah, 2
		int	10h

.leaveFunction2	mov	[PrtScrStatus], bl
		pop	dx
		pop	cx
		pop	bx

.leaveFunction	pop	ds
		pop	ax
		iret
		ENDPROC	PrntScrn_Actual

; ---------------------------------------------------------------------
; PrintCrLf
; Output CR/LF pair to LPT1.
; ---------------------------------------------------------------------
PrintCrLf	PROC
		mov	al, 0Dh		; print carriage-return
		call	PrintChar
		jnz	PrintChar.leaveFunction
		mov	al, 0Ah		; print line-feed
		; fall-through into PrintChar
		ENDPROC	PrintCrLf

; ---------------------------------------------------------------------
; PrintChar
; Outputs a character to LPT1.
; On entry:
;   AL == character to print
; On success:
;   ZF set
;   BL unmodified
; On failure:
;   ZF clear
;   BL == 0FFh
; ---------------------------------------------------------------------
PrintChar	PROC
		push	dx
		xor_	ah, ah
		xor_	dx, dx
		int	17h		; output character in AL to printer DX
		pop	dx
		and	ah, 9		; mask off I/O error and timeout bits
		jz	.leaveFunction
		mov	bl, 0FFh	; return error code
.leaveFunction	retn
		ENDPROC	PrintChar

ENDPROGRAM	PRNTSCRN
