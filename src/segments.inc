
; BIOS code (F000:8000 - F000:FFFF)
BIOS_SEGMENT	equ	0F000h
BIOS_START	equ	08000h

BIOS_START_PARA	equ	(BIOS_SEGMENT + BIOS_START >> 4)

; BIOS stack (0000:0300 - 0000:0380)
STACK_SEGMENT	equ	0030h
STACK_TOP	equ	0080h

; BIOS data area (0000:0400 - 0000:04FF)
BDA_SEGMENT	equ	0040h

[BDA]
$		equ	0067h
AdapterRomOffset	d w
AdapterRomSegment	d w

$		equ	0072h
SoftResetFlag		d w

