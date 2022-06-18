
; Predefine all virtual segments (that shouldn't be emitted into the
; final BIOS image), then switch back to the CODE segment so data and
; instructions will be emitted into it.

; BIOS code (F000:8000 - F000:FFFF)
[CODE]	SEGMENT WIDTH=16, ALIGN=1, CLASS=CODE, PURPOSE=DATA|CODE, COMBINE=PUBLIC

BIOS_SEGMENT	equ	0F000h
BIOS_START	equ	08000h
BIOS_START_PARA	equ	(BIOS_SEGMENT + BIOS_START >> 4)

; BIOS stack (0030:0000 - 0030:0080)
[STACK]	SEGMENT WIDTH=16, ALIGN=16, CLASS=STACK, PURPOSE=STACK

STACK_SEGMENT	equ	0030h
STACK_TOP	equ	0080h
STACK_TOP2	equ	0100h

; BIOS data area (0040:0000 - 0040:00FF)
[BDA]	SEGMENT WIDTH=16, ALIGN=16, CLASS=VIRT, PURPOSE=DATA, COMBINE=COMMON

BDA_SEGMENT	equ	0040h

; Interrupt vector table (0000:0000 - 0000:0400)
[IVT]	SEGMENT WIDTH=16, ALIGN=16, CLASS=VIRT, PURPOSE=DATA, COMBINE=COMMON

IVT_SEGMENT	equ	0000h

; Upper and lower limits for hard disk option ROM search
HD_ROM_SEG_LO	equ	0C000h
HD_ROM_SEG_HI	equ	0C800h

; Upper and lower limits for regular option ROM search
OPT_ROM_SEG_LO	equ	0C800h
OPT_ROM_SEG_HI	equ	0E000h

; Location for system option ROM search
SYS_ROM_SEG	equ	0E000h

;  Default to CODE segment
[CODE]
