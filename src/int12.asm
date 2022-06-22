
INT12		PROGRAM	OutFile=build/int12.obj

		include	"macros.inc"
		include	"segments/bda.inc"

		EXTERN	LoadBdaToDs

		PUBLIC	Int12_Actual

; ---------------------------------------------------------------------
; Int12_Actual [TechRef 3-7]
; ---------------------------------------------------------------------
Int12_Actual	PROC
		push	ds
		call	LoadBdaToDs
		mov	ax, [MemorySizeKb]
		pop	ds
		iret
		ENDPROC	Int12_Actual

ENDPROGRAM	INT12
