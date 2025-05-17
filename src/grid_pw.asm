
GRID_PW		PROGRAM	OutFile=build/grid_pw.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"segments/bda.inc"
		include	"cmos.inc"
		include	"grid.inc"

		EXTERN	ReadCmos, WriteCmos

		PUBLIC	GridConfig4B

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

ENDPROGRAM	GRID_PW
