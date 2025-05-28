
SYSIDENT	PROGRAM	OutFile=build/sysident.obj

		include	"macros.inc"
		include	"segments.inc"

		PUBLIC	GridSysId

; ---------------------------------------------------------------------------
; System identification section at F000:DFD0
; ---------------------------------------------------------------------------

		; EPROM part numbers, arranged so each split ROM has a unique number.
		; 1520 BIOS EPROM part numbers differ in the final digit.
		; Note that the arrow is offset by one byte, so even/odd ROMs have it in different columns.
		; [Public] 0FDFDh:000Ch == 2D2Dh ('--')
		; [TechRef] 3-26 "System Identification"
		db	'330000663365--0000 <<-- ppaarrtt  nnuummbbeerr'

		; [Public] 0F000h:0DFFEh == 34h
		; [TechRef] 3-26 "System Identification"
GridSysId	dw	0034h

ENDPROGRAM	SYSIDENT
