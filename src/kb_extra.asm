
KB_EXTRA	PROGRAM	OutFile=build/kb_extra.obj

		include	"macros.inc"
		include	"segments/bda.inc"
		include	"keyboard.inc"

		EXTERN	KbAckInt

		PUBLIC	KbKeyClick, KbCtrlAltCheck

; ===========================================================================
; KbUnknown
; ??? Dead code
; ===========================================================================
KbUnknown	PROC
		Unused	KbUnknown
		mov	[KbLastChar], si
		sti
		call	KbKeyClick
		jmp	KbAckInt
		ENDPROC	KbUnknown

; ===========================================================================
; KbKeyClick
; Toggles the speaker to emulate a keyboard click.
; ===========================================================================
KbKeyClick	PROC
		push	cx
		xor_	cx, cx
		or	cl, [KbClickVol]	; load key click volume
		jz	.leaveFunction		; no click wanted?

		in	al, PORT_KBC_PORTB
		push	ax
		and	al, 0FCh		; turn off speaker
		mov	cl, [KbClickVol]
		xor	al, 2			; use timer 2 to control speaker freq
		cli				; no interrupts while we bit-bang the speaker
		out	PORT_KBC_PORTB, al	; turn on speaker
.1		loop	.1
		xor	al, 2			; turn off speaker
		out	PORT_KBC_PORTB, al
		sti				; interrupts safe again
		pop	ax			; restore original speaker state
		out	PORT_KBC_PORTB, al
.leaveFunction	pop	cx
		retn
		ENDPROC	KbKeyClick

; ===========================================================================
; KbCtrlAltCheck
; Handles special key combinations Ctrl+Alt+Minus and Ctrl+Alt+Plus to adjust
; the keyboard click volume.  Intended to be a drop-in replacement for a
;   cmp ah, SC2_KP_DEL
; instruction in the keyboard handler (Phoenix BIOS patch?)
; On return:
;   ZF set if key combination was Ctrl+Alt+Del
; ===========================================================================
KbCtrlAltCheck	PROC
		push	ax
		mov	al, [KbClickVol]
		cmp	ah, SC2_KP_MINUS
		jz	.quieter
		cmp	ah, SC2_KP_PLUS
		jnz	.leaveFunction
		shl	al, 1			; make 2x louder
		jnz	.clampUpper
		mov	al, 1			; inc from silent
.clampUpper	jns	.saveClickVol
.quieter	shr	al, 1			; make 1/2 quieter
.saveClickVol	mov	[KbClickVol], al
		call	KbKeyClick

.leaveFunction	pop	ax
		cmp	ah, SC2_KP_DEL		; check for ctrl+alt+del
		retn
		ENDPROC	KbCtrlAltCheck

ENDPROGRAM	KB_EXTRA
