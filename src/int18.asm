
INT18		PROGRAM	OutFile=build/int18.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"keyboard.inc"

		EXTERN	Beep, BeepFactor
		EXTERN	WaitKey
		EXTERN	WriteString_Inline

		PUBLIC	Int18
		PUBLIC	PromptF1Cont

; ===========================================================================
; Int18
; Called after bootloader failure.
; IBM PCs use this to enter ROM-BASIC, but the GRiD computers (and almost all
; IBM-compatible PC clones) do not have this available.  So we just report
; an error and wait for the user to acknowledge it.
; ===========================================================================
Int18		PROC
		Inline	WriteString,0Dh,0Ah,' strike F1 to retry boot',0Dh,0Ah,0
		call	BeepBeepWaitF1
		iret
		ENDPROC	Int18

; ===========================================================================
; PromptF1Cont
; Prompts the user to press the F1 key with a double beep.
; ===========================================================================
PromptF1Cont	PROC
		Inline	WriteString,' Strike the F1 key to continue',0Dh,0Ah,0
		; Fallthough into BeepBeepWaitF1
		ENDPROC	PromptF1Cont

; ===========================================================================
; BeepBeepWaitF1
; Sounds the speaker twice and then waits for the user to press the F1 key.
; ===========================================================================
BeepBeepWaitF1	PROC
		call	Beep

		; short delay between the two beeps
		mov	ch, [cs:BeepFactor]
		shr	ch, 1
.beepDelay	ror	al, 1Fh		; delay by 36 clocks
		loop	.beepDelay

		call	Beep

.waitF1		call	WaitKey
		cmp	ah, SC2_F1
		jnz	.waitF1
		retn
		ENDPROC	BeepBeepWaitF1

ENDPROGRAM	INT18
