
INT71		PROGRAM	OutFile=build/int71.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"pic.inc"

		PUBLIC	Int71

; ---------------------------------------------------------------------
; Handler for IRQ9 [TechRef 3-29]
; Redirects to interrupt 0Ah for backward compatibility with the
; original PC IRQ assigments.
; ---------------------------------------------------------------------
Int71		PROC
		push	ax
		mov	al, NONSPECIFIC_EOI
		out	PORT_PIC2_CTRL, al
		pop	ax
		int	0Ah
		iret
		ENDPROC	Int71

ENDPROGRAM	INT71
