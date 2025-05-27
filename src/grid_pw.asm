
GRID_PW		PROGRAM	OutFile=build/grid_pw.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"segments/bda.inc"
		include	"cmos.inc"
		include	"grid.inc"

		EXTERN	ReadCmos, WriteCmos
		EXTERN	ConString_Inline, ConChar

		PUBLIC	GridConfig4B
		PUBLIC	PwEnabled, PwStartInput, PwPrompt
		PUBLIC	PwProcessInput, PwEndInput, PwCompareStored
		PUBLIC	PwBackdoor2, PwIncorrect, PwClearBuffer

PW_LENGTH	equ	8
PwCryptKey	db	5Bh,44h,5Eh,68h,7Dh,5Fh,7Ch,4Dh

; Key for PwBackdoor2 given the above PwCryptKey:
;
; Input  | Position in password
; Char   | 0      1      2      3      4      5      6      7
; -------+--------------------------------------------------------
; 0(30h) | k(6Bh) t(74h) n(6Eh) X(58h) M(4Dh) O(6Fh) L(4Ch) }(7Dh)
; 1(31h) |
; 2(32h) |
; 3(33h) |
; 4(34h) |
; 5(35h) |
; 6(36h) |
; 7(37h) |
; 8(38h) |
; 9(39h) |


; =====================================================================
; PwBackdoor1
; Backdoor to bypass the BIOS boot password.  Not publicly documented
; by GRiD.
;
; Places a series of 32 values on the parallel port's data pins but
; does not trigger the strobe line.  A dongle attached to the port is
; expected to set the status lines so the appropriate response value
; can be read from the printer status port.  There is a seven-cycle
; delay before the response is read.  If all 32 challenge/response
; pairs succeed, then the 'password set' flag in CMOS memory is
; reset.
;
; On return:
;   CF set if password was not reset.
;   CF clear is password was reset.
;
; BUG: CF is always set, even if backdoor was successful.
; BUG: successful backdoor also sets EMS config flag in CMOS.
;
; Challenges: 08 10 18 20 28 30 38 40 48 50 58 60 68 70 78 80 88 90 98 A0 A8 B0 B8 C0 C8 D0 D8 E0 E8 F0 F8 00
; Responses:  88 90 98 A0 A8 B0 78 80 88 90 98 A0 A8 B0 B8 C0 C8 D0 D8 E0 E8 F0 F8 40 48 50 58 60 68 70 78 80
; (response is:
;    challenge xor 40h for challenges > 38h and <= B8h
;    challenge xor 80h for other challenges
; )
; =====================================================================
PwBackdoor1	PROC
		push	ax
		push	cx
		push	dx
		push	ds
		mov	ax, BDA_SEGMENT
		mov	ds, ax

		; Can't use bypass dongle if there aren't any parallel ports
		test	[EquipmentWord], 0C000h,DATA=WORD
		jz	.noBackdoor

		mov	dx, [Par1BasePort]
		mov	cx, 20h
		mov	al, 0
.issueChallenge	add	al, 8		; move to next value
		out	dx, al		; place on parallel port
		mov_	ah, al		; save challenge in AH
		Delay	1		; give dongle time to respond
		inc	dx		; advance to status port
		in	al, dx		; read status lines
		dec	dx		; return to data port
		and	al, 0F8h	; mask off reserved bits

		; Calculate expected response
		cmp	ah, 38h
		jbe	.keepRange	; clamp min response
		cmp	ah, 0B8h
		ja	.keepRange	; clamp max response

		xor	al, 40h		; make challenge non-monotonic
		jmp	.checkResponse

.keepRange	xor	al, 80h
.checkResponse	cmp_	ah, al		; got expected response?
		mov_	al, ah		; expected response becomes seed for next challenge
		loope	.issueChallenge
		jnz	.noBackdoor

		; All expected responses were matched so reset the password flag.
		; BUG: also resets EMS status
		mov	al, GF_EMS
		mov	ah, CMOS_GRIDFLAGS
		call	WriteCmos
		clc
		; BUG: missing retn means the caller always sees a failure

.noBackdoor	stc
		pop	ds
		pop	dx
		pop	cx
		pop	ax
		retn
		ENDPROC	PwBackdoor1

; =====================================================================
; GridConfig4B [UNDOC]
; Undocumented BIOS function that checks for the presence of a secret
; password reset dongle attached to the parallel port.  If present, the
; CMOS flag enabling the boot password is cleared.
;
; On return:
;   AH == 0
;   CL == 3 if CMOS power loss or hardware failure
;      == 2 if password reset dongle not present
;      == 1 if password successfully disabled
; =====================================================================
GridConfig4B	PROC
		push	ax

		; Check RTC good flag set and power fail flag not set
		mov	al, CMOS_STATUS_D
		call	ReadCmos
		test	al, 80h
		jz	.prereqFailed
		mov	al, CMOS_STATUS_DIAG
		call	ReadCmos
		test	al, 80h
		jz	.cmosOk

.prereqFailed	pop	ax
		mov	ah, 0
		mov	cl, GRID_INT15_CMOS_BAD
		retn

.cmosOk		; Attempt to clear the password flag
		call	PwBackdoor1
		jnb	.prereqFailed	; Dead code??? PwBackdoor1 always sets CF
		pop	ax
		mov	al, CMOS_GRIDFLAGS
		call	ReadCmos
		mov	cl, GRID_INT15_PW_CLEARED
		test	al, GF_PASSWORD	; Password clear succeeded?
		mov	ah, 0
		jz	.pwClearFailed
		retn

.pwClearFailed	inc	cl		; inc to GRID_INT15_NO_DONGLE
		retn
		ENDPROC	GridConfig4B

; =====================================================================
; PwEnabled
; Checks whether the BIOS boot password is enabled.
; Also calls PwBackdoor1 to allow the user to disable the password if
; they have the appropriate dongle attached.
;
; On return:
;   CF clear if password disabled (or PwBackdoor1 reset it)
;   CF set if password enabled.
; =====================================================================
PwEnabled	PROC
		push	ax

		mov	al, CMOS_STATUS_D
		call	ReadCmos
		test	al, 80h		; CMOS contents valid?
		jz	.noPassword	; If not, password can't be enabled

		mov	al, CMOS_STATUS_DIAG
		call	ReadCmos
		test	al, 80h		; CMOS lost power?
		jz	.maybePassword	; If not, password flag is valid

.noPassword	pop	ax
		retn

.maybePassword	call	PwBackdoor1	; XXX: naughty!
		jnb	.noPassword
		mov	al, CMOS_GRIDFLAGS
		call	ReadCmos
		shr	al, 1		; Shift GF_PASSWORD flag into CF
		shr	al, 1
		pop	ax
		retn
		ENDPROC	PwEnabled

; =====================================================================
; PwBcdToChars
; Helper method that converts BCD value in AL to two ASCII characters
; and stores them at DS:DI, advancing DI appropriately.
;
; On entry:
;   AL == BCD value to convert
;   DI -> location to store ASCII digits
; =====================================================================
PwBcdToChars	PROC
		Unused	PwBcdToChars
		push	cx
		mov_	ah, al		; save AL

		mov	cl, 4
		shr	al, cl		; get first BCD value in place
		add	al, '0'		; convert to ASCII
		stosb			; write to memory

		mov_	al, ah		; restore AL
		and	al, 0Fh		; mask to lower BCD value
		add	al, '0'		; convert to ASCII
		stosb			; write to memory

		pop	cx
		retn
		ENDPROC	PwBcdToChars

; =====================================================================
; PwUpcase
; Helper method that converts the eight characters at DS:SI to
; uppercase, in-place.  Only supports ASCII characters a-z -> A-Z.
; All other values are unchanged.
;
; On entry:
;   SI -> eight characters
;
; On return:
;   SI -> eight characters, uppercased
;   All other registers preserved
; =====================================================================
PwUpcase	PROC
		Unused	PwUpcase
		push	ax
		push	cx
		push	si

		mov	cx, 8
.upcaseChar	lodsb			; get character
		cmp	al, 'a'
		jb	.nextChar
		cmp	al, 'z'
		ja	.nextChar
		and	al, ~20h	; Clear ASCII lowercase bit
		mov	[si-1], al	; Replace in-place
.nextChar	loop	.upcaseChar

		pop	si
		pop	cx
		pop	ax
		retn
		ENDPROC	PwUpcase

; [Compat] Assumes these three BDA variables are arrange consecutively
BDA_SAVE_SIZE	equ	(SIZE#KbNextChar + SIZE#KbLastChar + SIZE#KbBuffer)

PWDA		STRUC
.UserPw		d	PW_LENGTH * b	; User-entered plaintext password
.UserCryptPw	d	PW_LENGTH * b	; Encrypted version of prior
.StoredCryptPw	d	PW_LENGTH * b	; Encrypted password to compare against
		d	8 * b
.BdaSaveState	d	BDA_SAVE_SIZE * b
ENDSTRUC	PWDA

; =====================================================================
; PwEndInput
; Call when ending BIOS password input.  Restores the original BDA's
; keyboard buffer state.
; =====================================================================
PwEndInput	PROC
		Unused	PwEndInput
		push	ax
		push	si
		push	di
		push	ds
		push	es

		; BUG??? Are we only called with interrupts disabled?
		;        Is this always safe?
		mov	ax, BDA_SEGMENT
		mov	es, ax
		mov	di, KbNextChar
		mov	ax, PWDA_SEGMENT
		mov	ds, ax
		mov	si, PWDA.BdaSaveState
		mov	cx, SIZE#PWDA.BdaSaveState / 2
		rep movsw

		pop	es
		pop	ds
		pop	di
		pop	si
		pop	ax
		retn
		ENDPROC	PwEndInput

; =====================================================================
; PwStartInput
; Call when starting BIOS password input.  Copies the keyboard buffer
; state from the BDA to a save area.
; =====================================================================
PwStartInput	PROC
		Unused PwStartInput
		push	ax
		push	si
		push	di
		push	ds
		push	es

		mov	ax, BDA_SEGMENT
		mov	ds, ax
		mov	si, KbNextChar
		mov	ax, PWDA_SEGMENT
		mov	es, ax
		mov	di, PWDA.BdaSaveState
		mov	cx, SIZE#PWDA.BdaSaveState / 2
		cli
		rep movsw			; Copy BDA keyboard buffer
		mov	[KbNextChar], 1Eh	; Reset it to empty
		mov	[KbLastChar], 1Eh	; ??? why 1Eh and not zero?
		sti

		pop	es
		pop	ds
		pop	di
		pop	si
		pop	ax
		retn
		ENDPROC	PwStartInput

; =====================================================================
; PwCrypt
; Encodes/decodes a password using XOR against a fixed key.
;
; On entry:
;   ES:DI -> 8-byte buffer to be encrypted/decrypted
;   DS:SI -> 8-byte buffer to contain the result
;
; On return:
;   SI, DI destroyed
; =====================================================================
PwCrypt		PROC
		push	bx
		push	cx

		mov	bx, PwCryptKey
		mov	cx, PW_LENGTH
.cryptByte	mov	al, [cs:bx]		; load key byte
		mov	ah, [es:di]		; load source byte
		xor_	al, ah			; crypt
		mov	[si], al		; store to output buffer
		inc	di			; advance
		inc	si
		inc	bx
		loop	.cryptByte

		pop	cx
		pop	bx
		retn
		ENDPROC	PwCrypt

; =====================================================================
; PwCompareStored
; Compares the password stored in CMOS_GRID_PASSWORD to the value
; entered by the user in PWDA.UserPw.
;
; On return:
;   CF set if passwords do not match.
;   CF clear if passwords match.
; =====================================================================
PwCompareStored	PROC
		push	ax
		push	cx
		push	dx
		push	si
		push	di
		push	ds
		push	es
		cld

		; Encrypt the password entered by the user
		mov	cx, PWDA_SEGMENT
		mov	es, cx
		xor_	di, di
		mov	ds, cx
		mov	si, PWDA.UserCryptPw
		call	PwCrypt

		; Copy stored encrypted password out of CMOS
		mov	di, PWDA.StoredCryptPw
		mov	ax, es
		mov	ds, ax
		mov	al, CMOS_GRID_PASSWORD
		mov_	ah, al
		mov	cx, PW_LENGTH
.readCmos	call	ReadCmos
		stosb
		inc	ah
		mov_	al, ah
		loop	.readCmos

		mov	di, PWDA.StoredCryptPw
		mov	si, PWDA.UserCryptPw
		; Fallthrough to PwCompareBuffer
		ENDPROC	PwCompareStored

; =====================================================================
; PwCompareBuffer
; Shared function tail for PwCompareStored and PwBackdoor2.
; Calls PwCompare then pops ES, DS, DI, SI, DX, CX, and AX.
; =====================================================================
PwCompareBuffer	PROC
		call	PwCompare
		pop	es
		pop	ds
		pop	di
		pop	si
		pop	dx
		pop	cx
		pop	ax
		retn
		ENDPROC	PwCompareBuffer

; =====================================================================
; PwClearBuffer
; Zeroes out the buffers that may have held passwords (plaintext or
; encrypted).
; BUG: intended to clear the first four 8-byte buffers in the PWDA
; segment (32 bytes), but due to a type it actually clears the first
; 32h (50 decimal) bytes.  This wipes out the saved BDA keyboard buffer
; we intended to restore after the password routines return.
; =====================================================================
PwClearBuffer	PROC
		push	ax
		push	cx
		push	di
		push	es

		mov	ax, PWDA_SEGMENT
		mov	es, ax
		xor_	ax, ax
		mov_	di, ax
		mov	cx, 32h		; BUG: should be 32 decimal
		rep stosb

		pop	es
		pop	di
		pop	cx
		pop	ax
		retn
		ENDPROC	PwClearBuffer

; =====================================================================
; PwBackdoor2
; Undocumented bypass for the BIOS boot password.
; =====================================================================
PwBackdoor2	PROC
		push	ax
		push	cx
		push	dx
		push	si
		push	di
		push	ds
		push	es

		mov	ah, 4		; read date from RTC
		int	1Ah		; DL/DH/CL/CH = d/m/y/c

		mov	ax, PWDA_SEGMENT
		mov	es, ax
		mov	ds, ax

		mov	di, PWDA.StoredCryptPw
		mov_	al, cl
		call	PwBcdToChars	; convert year
		mov_	al, dh
		call	PwBcdToChars	; convert month
		mov_	al, dl
		call	PwBcdToChars	; convert day
		mov	al, 0
		call	PwBcdToChars	; pad password with zeroes

		mov	si, PWDA.UserCryptPw
		push	si
		mov	di, PWDA.StoredCryptPw
		call	PwCrypt
		pop	si

		xor_	di, di
		call	PwUpcase
		jmp	PwCompareBuffer
		ENDPROC	PwBackdoor2

; =====================================================================
; PwGetLanguage
; Returns the language to be used for the BIOS password prompt.
; On return:
;   AL == language code (see GF_PW_LANG_* constants)
; =====================================================================
PwGetLanguage	PROC
		mov	al, CMOS_GRIDFLAGS
		call	ReadCmos
		and	al, GF_PW_LANG_MASK
		shr	al, 1
		shr	al, 1
		retn
		ENDPROC	PwGetLanguage

; =====================================================================
; PwIncorrect
; Displays "password incorrect" message in the appropriate language,
; then waits for the user to strike a key.
; =====================================================================
PwIncorrect	PROC
		mov	ah, 02h		; set cursor position
		mov	dx, 0C10h	; to R12C16
		mov	bh, 0
		int	10h

		call	PwGetLanguage
		cmp	al, GF_PW_LANG_FR
		jz	.langFr
		cmp	al, GF_PW_LANG_DE
		jz	.langDe

.langEn		Inline	ConString,'Incorrect password; press any key',0
		jmp	.waitChar
.langDe		Inline	ConString,'Ung',81h,'ltiges Passwort, beliebige Test dr',81h,'cken',0
		jmp	.waitChar
.langFr		Inline	ConString,'Mot de passe incorrect; appuyez sur une touche',0

.waitChar	mov	ah, 0		; read char with wait
		int	16h
		retn
		ENDPROC	PwIncorrect

; =====================================================================
; PwProcessInput
; =====================================================================
PwProcessInput	PROC
		push	es
		mov	ax, PWDA_SEGMENT
		mov	es, ax

		xor_	di, di
		cld
		xor_	di, di		; ??? duplicate xor

		; Clear user password input
		mov	al, 0
		mov	cx, PW_LENGTH
		rep stosb
		xor_	di, di

.getChar	mov	ah, 0		; read char with wait
		int	16h
		cmp	al, 0Dh
		jz	.gotEnter
		cmp	al, 8
		jnz	.gotOtherChar

		; got backspace
		cmp	di, 0		; already at start of input?
		jz	.getChar	; ignore if so

		; Update screen
		mov	bx, 07h
		mov	ah, 0Eh		; write char and advance cursor
		int	10h		; AL == BS from above, moves cursor back
		mov	al, ' '
		int	10h		; overwrite with space
		mov	al, 08h
		int	10h		; move cursor back again

		; Remove char from input buffer
		dec	di
		mov	al, 0
		mov	[es:di], al
		jmp	.getChar

.gotOtherChar	cmp	di, PW_LENGTH	; input buffer full?
		jnb	.getChar	; ignore if so
		cmp	al, 0		; 'special' key (e.g. F1)?
		jz	.getChar	; ignore if so

		; Uppercase ASCII characters
		cmp	al, 'a'
		jl	.upcased
		cmp	al, 'z'
		ja	.upcased
		and	al, ~('a' ^ 'A')

.upcased	stosb			; store in input buffer

		mov	bx, 07h		; update screen
		mov	al, '*'
		mov	ah, 0Eh		; write char and advance cursor
		int	10h
		jmp	.getChar

.gotEnter	mov	bx, 07h		; update screen
		mov	al, 0Dh
		mov	ah, 0Eh
		int	10h		; write CR
		mov	al, 0Dh		; ??? write another CR
		int	10h
		mov	al, 0Ah		; write LF
		int	10h

		pop	es
		retn
		ENDPROC	PwProcessInput

; =====================================================================
; PwCompare
;
; On entry:
;   DS:SI -> buffer 1
;   ES:DI -> buffer 2
;
; On return:
;   SI,DI destroyed
;   CF clear if buffers match
;   CF set if buffers do not match
; =====================================================================
PwCompare	PROC
		mov	cx, PW_LENGTH
		repe cmpsb
		jz	.matched
		stc
		jmp	.leaveFunction
.matched	clc
.leaveFunction	retn
		ENDPROC	PwCompare

; =====================================================================
; WriteBcdByte
; Outputs the value packed BCD value in AX to the screen as two
; hexadecimal digits.
;
; On entry:
;   AX == packed BCD value to print
;   BL <= 1 suppress leading zero
;       > 1 print leading zero
; =====================================================================
WriteBcdByte	PROC
		push	ax
		push	bx
		push	cx
		push	dx

		mov	dl, 2		; two digits to output
		push	ax
		mov	cl, 4
		shr	al, cl		; shift hi-digit down
		cmp	al, 0
		jnz	.printDigit
		cmp	bl, 1
		jbe	.nextDigit

.printDigit	push	bx
		mov	ah, 0Eh		; write char and advance cursor
		add	al, '0'		; convert to ASCII
		mov	bx, 7
		int	10h
		pop	bx

.nextDigit	dec	dl		; decrement digit count
		jz	.leaveFunction
		pop	ax		; restore original value
		and	al, 0Fh		; mask off lo-digit
		jmp	.printDigit

.leaveFunction	pop	dx
		pop	cx
		pop	bx
		pop	ax
		retn
		ENDPROC	WriteBcdByte

; =====================================================================
; PwPrompt
; Prompts the user to enter the BIOS boot password.
; Also displays the current date so the user can calculate today's
; backdoor password.
; =====================================================================
PwPrompt	PROC
		mov	ah, 2		; set cursor position
		mov	dx, 0910h	; to R9C16
		mov	bh, 0
		int	10h

		call	PwGetLanguage
		push	ax		; save language index
		cmp	al, GF_PW_LANG_FR
		jz	.dateFr
		cmp	al, GF_PW_LANG_DE
		jz	.dateDe

.dateEn		Inline	ConString,'Today',27h,'s date: ',0
		jmp	.printDate
.dateDe		Inline	ConString,'Heutiges Datum: ',0
		jmp	.printDate
.dateFr		Inline	ConString,'Date d',27h,'aujourd',27h,'hui: ',0

.printDate	mov	ah, 4		; read date from RTC
		int	1Ah		; DL/DH/CL/CH = d/m/y/c
		push	cx
		push	dx

		; print current date in d/m/y format
		mov	bl, 1		; suppress leading zero
		mov_	al, dh		; print month
		call	WriteBcdByte
		mov	al, '/'
		call	ConChar
		inc	bl		; ??? unnecessary inc
		pop	ax
		call	WriteBcdByte	; print day
		mov	al, '/'
		call	ConChar
		pop	ax
		call	WriteBcdByte	; print year

		; print password prompt
		mov	ah, 2		; set cursor position
		mov	dx, 0A10h	; to R10C16
		mov	bh, 0
		int	10h
		pop	ax		; restore language index
		cmp	al, GF_PW_LANG_FR
		jz	.promptFr
		cmp	al, GF_PW_LANG_DE
		jz	.promptDe

.promptEn	Inline	ConString,'Enter password, then press RETURN: ',0
		jmp	.leaveFunction
.promptDe	Inline	ConString,'Bitte Passwort eingeben, mit RETURN best',84h,'tigen: ',0
		jmp	.leaveFunction
.promptFr	Inline,	ConString,'Entrez mot de passe, et appuyez sur RETURN: ',0

.leaveFunction	retn
		ENDPROC	PwPrompt

ENDPROGRAM	GRID_PW
