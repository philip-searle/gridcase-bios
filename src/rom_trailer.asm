
ROM_TRAILER	PROGRAM	OutFile=build/rom_trailer.obj

		EXTERN	Reset_Compat

		include	"macros.inc"
		include	"segments.inc"

; ---------------------------------------------------------------------------
; Power-On Reset Vector at F000:FFF0
; ---------------------------------------------------------------------------
		;FillRom	0FFF0h, 0FFh

		; [Public] CPU reset vector is top of address space minus 16 bytes
		; AT BIOS calls this P_O_R (POWER ON RESET)
		; Must be a far jump to set CS to the correct value.
Reset		CompatAddress 0FFF0h
		jmpf	Reset_Compat

		; [Public] IBM BIOS stores ROM date at F000:FFF5 (AT BIOS calls this 'RELEASE MARKER')
ReleaseMarker	CompatAddress 0FFF5h
		db	'03/11/89'

		db	0FFh		; Unused byte

		; [Public] IBM BIOS stores machine identification byte at F000:FFFE
MachineId	CompatAddress 0FFFEh
		db	0FCh		; Model FC == IBM AT (6 MHz)
		db	00		; Submodel 00 == no specific submodel

ENDPROGRAM	ROM_TRAILER
