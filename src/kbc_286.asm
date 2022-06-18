
KBC_286		PROGRAM	OutFile=build/kbc_286.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"keyboard.inc"

		EXTERN	KbWaitIbe, KbWaitIbe_Cli

		PUBLIC	A20Disable
		PUBLIC	A20Enable
		PUBLIC	ResetCpu

; ---------------------------------------------------------------------
; A20Enable
; Uses the keyboard controller to enable the A20 gate, granting access
; to all physical memory.
; ---------------------------------------------------------------------
A20Enable	PROC
		push	ax

		; KBC output port value [TechRef 8-27]
		; External keyboard enable, IRQ1 enable, A20 enable
		mov	ah, 0DFh

		jmp	A20SetState
		ENDPROC	A20Enable

; ---------------------------------------------------------------------
; A20Disable
; Uses the keyboard controller to disable the A20 gate, restricting
; the CPU to just the first megabyte of memory.
; ---------------------------------------------------------------------
A20Disable	PROC
		push	ax

		; KBC output port value [TechRef 8-27]
		; External keyboard enable, IRQ1 enable A20 disable
		mov	ah, 0DDh

		; Fallthrough to A20SetState
		ENDPROC	A20Disable

; ---------------------------------------------------------------------
; A20SetState
; Writes the value in AH to the keyboard controller's output port.
; Intended to be used for enabling/disabling the A20 gate but can
; actually modify any bit in the output port.  Not intended to be
; called directly -- push AX, then jump to the function start.
; ---------------------------------------------------------------------
A20SetState	PROC
		push	cx

		; Wait for KBC to be ready to accept commands then
		; make sure interrupts are disabled
		mov	cx, KBC_WRITE_TIMEOUT
		call	KbWaitIbe
		jnz	.leaveFunction
		call	KbWaitIbe_Cli
		jnz	.leaveFunction

		; Write new value to KBC output port
		mov	al, KBC_CMD_WRITEP2
		out	PORT_KBC_CMD, al
		call	KbWaitIbe
		jnz	.leaveFunction
		mov_	al, ah
		out	PORT_KBC_DATA, al
		call	KbWaitIbe

.leaveFunction	pop	cx
		pop	ax
		retn
		ENDPROC	A20SetState

; ---------------------------------------------------------------------
; ResetCpu
; Pulses bit 0 of the keyboard controller's output port P2.  This
; causes the 80286 CPU's reset line to be asserted.
; ---------------------------------------------------------------------
ResetCpu	PROC
		mov	cx, KBC_WRITE_TIMEOUT
		call	KbWaitIbe_Cli
		mov	al, KBC_CMD_PULSEP2
		out	PORT_KBC_CMD, al

.hlt		; The keyboard controller is not very fast so the 286 will
		; continue executing code while the KBC processed the reset
		; request.  Halt with interrupts disabled while this happens.
		hlt
		jmp	.hlt
		ENDPROC	ResetCpu

ENDPROGRAM	KBC_286
