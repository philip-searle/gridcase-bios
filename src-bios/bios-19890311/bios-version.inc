
; Constant for conditional assembly
BIOS_VERSION	equ	19890311

; BIOS identification for ROM trailer
%BIOS_DATE	%SET	'03/11/89'

; Startup banner
; Not sure why it's both nul-padded to 40 bytes and also nul-terminated
BIOS_BANNER	%macro
		db	'80286 ROM BIOS Version 3.06    3/11/89',0,0,0
		%endmacro

; Even/odd ROM checksum bytes
BIOS_CHECKSUM_E	equ	0E0h
BIOS_CHECKSUM_O	equ	024h
