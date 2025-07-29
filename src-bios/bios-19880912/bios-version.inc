
; Constant for conditional assembly
BIOS_VERSION	equ	19880912

; BIOS identification for ROM trailer
%BIOS_DATE	%SET	'09/12/88'

; Startup banner
; 1988 BIOS doesn't include GRiD modification date like later ones do
BIOS_BANNER	%macro
		db	'80286 ROM BIOS Version 3.06',0
		%endmacro

; Even/odd ROM checksum bytes
BIOS_CHECKSUM_E	equ	02Dh
BIOS_CHECKSUM_O	equ	0E8h
