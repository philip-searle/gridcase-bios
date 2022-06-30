
INT75		PROGRAM	OutFile=build/int75.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"npu.inc"
		include	"pic.inc"

		PUBLIC	Int75

; ---------------------------------------------------------------------
; Interrupt handler for co-processor error state [TechRef 2-5]
; When the coprocessor detects an error condition it generates an error
; signal.  Support circuitry converts this to an int75h with the
; coprocessor BUSY flag set.  This procedure takes care of converting
; this 80287-specific error state into a call to the int2h NMI handler,
; as the original PC BIOS would have had to deal with.
; ---------------------------------------------------------------------
Int75		PROC
		push	ax
		mov	al, 0
		out	PORT_287CLRBUSY, al	; clear 287 BUSY flag
		mov	al, NONSPECIFIC_EOI
		out	PORT_PIC2_CTRL, al	; clear interrupt
		out	PORT_PIC1_CTRL, al	; clear cascaded interrupt
		pop	ax
		int	2			; redirect to NMI handler
		iret
		ENDPROC	Int75

ENDPROGRAM	INT75
