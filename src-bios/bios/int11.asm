
INT11		PROGRAM	OutFile=int11.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"segments/bda.inc"

		EXTERN	LoadBdaToDs

		PUBLIC	Int11_Actual

; ---------------------------------------------------------------------
; Int11_Actual [TechRef 3-6]
; ---------------------------------------------------------------------
Int11_Actual	PROC
		push	ds
		call	LoadBdaToDs
		mov	ax, [EquipmentWord]
		pop	ds
		iret
		ENDPROC	Int11_Actual

ENDPROGRAM	INT11
