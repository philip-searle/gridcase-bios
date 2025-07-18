
%IF TYPE#SEGMENTS_DEFINED = '?'
%ERROR Must include segments.inc first
%ENDIF

; Interrupt vector table layout

[IVT]

IvtInt00		equ	00h * 4
IvtInt02		equ	02h * 4
IvtInt05		equ	05h * 4
IvtInt08		equ	08h * 4
IvtInt10		equ	10h * 4
IvtInt13		equ	13h * 4
IvtInt14		equ	14h * 4
IvtVidParms		equ	1Dh * 4	; Int1D is a far pointer, not a real ISR
IvtDisketteTable	equ	1Eh * 4	; Int1E is a far pointer, not a real ISR
IvtVidGrapFont		equ	1Fh * 4
IvtInt40		equ	40h * 4
IvtHd0Parms		equ	41h * 4	; Int41 is a far pointer, not a real ISR
IvtHd1Parms		equ	46h * 4	; Int46 is a far pointer, not a real ISR
IvtInt60		equ	60h * 4
IvtInt67		equ	67h * 4
IvtInt70		equ	70h * 4
IvtInt76		equ	76h * 4

[CODE]
