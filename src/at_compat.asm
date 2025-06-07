
AT_COMPAT	PROGRAM	OutFile=build/at_compat.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"int13.inc"
		include	"pic.inc"

		EXTERN	Reset_Actual
		EXTERN	IntNmi_Actual, HdAtInt13, Int19_Actual
		EXTERN	Int14_Actual, Int16_Actual, Int9_Actual, Int13Fd_Actual
		EXTERN	IntE_Actual, Int17_Actual, Int12_Actual, Int11_Actual
		EXTERN	Int15_Actual, Int1A_Actual, Int8_Actual
		EXTERN	UnexpectedInt, Int18, Int70, Int71, Int75, Int10_Actual
		EXTERN	PrntScrn_Actual

		PUBLIC	Reset_Compat, IntNmi_Compat, Int13Hd_Compat
		PUBLIC	Int8_Compat, DisketteParams, FixedDiskParams
		PUBLIC	BaudRateInit, InitialIvt, PrntScrn_Compat, SoftwareIret
		PUBLIC	DelayFactor, BeepFactor
		PUBLIC	VidRegenLengths, VidColumns, VidModeSets
		PUBLIC	GraphicsChars
		PUBLIC	Copyr_Phoenix, Copyr_Phoenix2
		PUBLIC	kConfigTable

		PUBLIC_COMPAT	Copyr_Compat
		PUBLIC_COMPAT	Int9_Compat, IntE_Compat, Int10_Compat
		PUBLIC_COMPAT	Int11_Compat, Int12_Compat, Int13Fd_Compat
		PUBLIC_COMPAT	Int15_Compat, Int14_Compat, Int16_Compat
		PUBLIC_COMPAT	Int17_Compat, Int19_Compat, Int1A_Compat
		PUBLIC_COMPAT	VidModeTable

%XxxBase	%seta 0E000h

; ===========================================================================
; IBM AT compatibility section
;
; The IBM XT BIOS was located at F000:E000 and many programs hardcoded the
; addresses of various entrypoints and data structures (despite the published
; BIOS listing warning against this in favour of the documented interrupt
; handlers).  To prevent these programs from breaking when the AT BIOS was
; released, all BIOS code was located below this zone with 'stub' entrypoints
; to redirect any direct callers to the real code elsewhere.
;
; Since large portions of this area ended up consisting entirely of unused
; bytes (0FFh) GRiD reused some of the spare space for GRiD-specific code
; such as the BIOS password feature.
; ===========================================================================

; [Compat] The IBM AT BIOS had a 22-character part number and IBM copyright
;          notice at 0F00:E000.  Some programs expect to see the letters 'IBM'
;          at address 0F00:E00E but the rest of the string can be blanked out.
;          The purpose of the 08ah and 0c3h are currently unknown (the AT BIOS
;          had spaces in those locations).  Maybe for programs that checksummed
;          the copyright message?
Copyr_Compat	d	13 * byte 0FFh
		db	08Ah, 'IBM', 0C3h
		d	4 * byte 0FFh

; Phoenix seem to have rounded their copyright notice up to the next paragraph
		FillRom	0E020h, 0FFh
Copyr_Phoenix	db	'Copyright (c) 1985,1986 Phoenix Technologies Ltd',0

; [Compat] Some programs hardcoded the destination of the power-on reset
;          vector (F000:E05B) so the AT BIOS keeps it as a compatibility thunk
		FillRom	0E05Bh, 0FFh
Reset_Compat	jmpn	Reset_Actual

; Second part of Phoenix copyright notice is after the Reset_Compat jump.
; Maybe because this lines it up nicely with the first part in a hexdump?
Copyr_Phoenix2	db	0Dh,0Ah,'All Rights Reserved',0Dh,0Ah,0Ah,0

; ---------------------------------------------------------------------------

; [Compat] Non-maskable interrupt entrypoint must be the same as the XT BIOS.
		FillRom	0E2C3h,00h
IntNmi_Compat	jmpn	IntNmi_Actual

; ---------------------------------------------------------------------------

; [Compat] Int13 fixed disk entrypoint must be the same as the XT BIOS.
		FillRom 0E3FEh,0FFh
Int13Hd_Compat	jmpn	HdAtInt13

; ---------------------------------------------------------------------------

; IBM AT BIOS stored the fixed disk parameter table somewhere after the NMI
; entrypoint.  The GRiD BIOS doesn't have it in the exact same address, but
; maybe it's important to keep it in the same segment?
;                                 cyl  hd     wpc ctl  land  spt
FixedDiskParams	FDS_AT_INSTANCE	0132h,  4,    80h, 0h, 131h, 11h	; Type 01 10MB
		FDS_AT_INSTANCE	0267h,  4,   12Ch, 0h, 267h, 11h	; Type 02 20MB
		FDS_AT_INSTANCE	0267h,  6,   12Ch, 0h, 267h, 11h	; Type 03 31MB
		FDS_AT_INSTANCE	03ACh,  8,   200h, 0h, 3ACh, 11h	; Type 04 62MB
		FDS_AT_INSTANCE	03ACh,  6,   200h, 0h, 3ACh, 11h	; Type 05 47MB
		FDS_AT_INSTANCE	0267h,  4, 0FFFFh, 0h, 267h, 11h	; Type 06 20MB
		FDS_AT_INSTANCE	01CEh,  8,   100h, 0h, 1FFh, 11h	; Type 07 31MB
		FDS_AT_INSTANCE	02DDh,  5, 0FFFFh, 0h, 2DDh, 11h	; Type 08 30MB
		FDS_AT_INSTANCE	0384h, 15, 0FFFFh, 8h, 385h, 11h	; Type 09 112MB
		FDS_AT_INSTANCE	0334h,  3, 0FFFFh, 0h, 334h, 11h	; Type 10 20MB
		FDS_AT_INSTANCE	0357h,  5, 0FFFFh, 0h, 357h, 11h	; Type 11 35MB
		FDS_AT_INSTANCE	0357h,  7, 0FFFFh, 0h, 357h, 11h	; Type 12 50MB
		FDS_AT_INSTANCE	0132h,  8,    80h, 0h, 13Fh, 11h	; Type 13 20MB
		FDS_AT_INSTANCE	02DDh,  7, 0FFFFh, 0h, 2DDh, 11h	; Type 14 43MB
		FDS_AT_INSTANCE	0000h,  0,     0h, 0h,   0h,  0h	; Type 15 RESERVED
		FDS_AT_INSTANCE	0264h,  4,     0h, 0h, 297h, 11h	; Type 16 20MB GRiD supported [TechRef 6-18]
		FDS_AT_INSTANCE	03D1h,  5,   12Ch, 0h, 3D1h, 11h	; Type 17 41MB GRiD supported [TechRef 6-18]
		FDS_AT_INSTANCE	03D1h,  7, 0FFFFh, 0h, 3D1h, 11h	; Type 18 57MB GRiD supported [TechRef 6-18]
		FDS_AT_INSTANCE	0400h,  7,   200h, 0h, 3FFh, 11h	; Type 19 60MB GRiD supported [TechRef 6-18]
		FDS_AT_INSTANCE	02DDh,  5,   12Ch, 0h, 2DCh, 11h	; Type 20 30MB GRiD supported [TechRef 6-18]
		FDS_AT_INSTANCE	02DDh,  7,   12Ch, 0h, 2DCh, 11h	; Type 21 43MB GRiD supported [TechRef 6-18]
		FDS_AT_INSTANCE	02DDh,  5,   12Ch, 0h, 2DDh, 11h	; Type 22 30MB GRiD supported [TechRef 6-18]
		FDS_AT_INSTANCE	0132h,  4,     0h, 0h, 150h, 11h	; Type 23 10MB GRiD supported [TechRef 6-18]
		FDS_AT_INSTANCE	0000h,  0,     0h, 0h,   0h,  0h	; Type 24 UNUSED
		FDS_AT_INSTANCE	0267h,  4,     0h, 0h, 267h, 11h	; Type 25 20MB
		FDS_AT_INSTANCE	0400h,  4, 0FFFFh, 0h, 3FFh, 11h	; Type 26 34MB
		FDS_AT_INSTANCE	0400h,  5, 0FFFFh, 0h, 3FFh, 11h	; Type 27 43MB
		FDS_AT_INSTANCE	0400h,  8, 0FFFFh, 0h, 3FFh, 11h	; Type 28 68MB
		FDS_AT_INSTANCE	0200h,  8,   100h, 0h, 200h, 11h	; Type 29 34MB
		FDS_AT_INSTANCE	0267h,  2,   267h, 0h, 267h, 11h	; Type 30 10MB
		FDS_AT_INSTANCE	0000h,  0,     0h, 0h,   0h,  0h	; Type 31 UNUSED
		FDS_AT_INSTANCE	0000h,  0,     0h, 0h,   0h,  0h	; Type 32 UNUSED
		FDS_AT_INSTANCE	0000h,  0,     0h, 0h,   0h,  0h	; Type 33 UNUSED
		FDS_AT_INSTANCE	0000h,  0,     0h, 0h,   0h,  0h	; Type 34 UNUSED
		FDS_AT_INSTANCE	0400h,  9,   400h, 8h, 400h, 11h	; Type 35 77MB
		FDS_AT_INSTANCE	0400h,  5,   200h, 0h, 400h, 11h	; Type 36 43MB
		FDS_AT_INSTANCE	033Eh, 10, 0FFFFh, 8h, 33Eh, 11h	; Type 37 69MB
		FDS_AT_INSTANCE	0337h, 10,   100h, 8h, 338h, 11h	; Type 38 68MB
		FDS_AT_INSTANCE	0267h,  4,    80h, 0h, 298h, 11h	; Type 39 20MB
		FDS_AT_INSTANCE	0267h,  8,    80h, 0h, 298h, 11h	; Type 40 41MB
		FDS_AT_INSTANCE	0395h, 15, 0FFFFh, 8h, 396h, 11h	; Type 41 114MB
		FDS_AT_INSTANCE	03FFh, 15, 0FFFFh, 8h, 400h, 11h	; Type 42 127MB
		FDS_AT_INSTANCE	0337h, 10,   200h, 8h, 337h, 11h	; Type 43 102MB
		FDS_AT_INSTANCE	0334h,  6, 0FFFFh, 0h, 334h, 11h	; Type 44 41MB
		FDS_AT_INSTANCE	0400h,  8, 0FFFFh, 0h, 400h, 11h	; Type 45 68MB
		FDS_AT_INSTANCE	039Dh,  9, 0FFFFh, 8h, 39Dh, 11h	; Type 46 69MB
		FDS_AT_INSTANCE	02BBh,  7,   100h, 0h, 2BCh, 11h	; Type 47 41MB

; ---------------------------------------------------------------------------

; [Compat] Int19 vector must remain the same as in XT BIOS, some programs
;          will jump here to reload the operating system from disk.
		FillRom	0E6F2h, 0FFh
Int19_Compat	jmpn	Int19_Actual

; GRiD BIOS stores int15/C0h config table in slack space after the int19 thunk
kConfigTable	dw	8	; number of bytes following
		db	0FCh	; model
		db	1	; submodel
		db	0	; BIOS revision
		db	70H	; feature byte 1:
				;   2nd interrupt controller (8259) installed
				;   Real-Time Clock installed
				;   INT 15/AH=4Fh called upon INT 09h
		db	0	; feature byte 2
		db	0	; feature byte 3
		db	0	; feature byte 4
		db	8	; feature byte 5
		db	0	; Phoenix BIOS extra byte
		db	3	; Phoenix BIOS major version
		db	6	; Phoenix BIOS minor version
		db	'PTL'	; Phoenix BIOS signature
		db	0, 0	; Phoenix BIOS extra bytes

; ---------------------------------------------------------------------------

; [Compat] Baud rate init table must be in the same place as the XT BIOS
		FillRom	0E729h, 0FFh
BaudRateInit	dw	1047	; 100 Baud
		dw	768	; 150
		dw	384	; 300
		dw	192	; 600
		dw	96	; 1200
		dw	48	; 2400
		dw	24	; 4800
		dw	12	; 9600

; ---------------------------------------------------------------------------

; [Compat] Int14 (serial I/O) entrypoint must be the same place as the XT BIOS
Int14_Compat	jmpn	Int14_Actual

; ---------------------------------------------------------------------------

; [Compat] Int16 (keyboard) entrypoint must be the same place as the XT BIOS
		FillRom	0E82Eh, 0FFh
Int16_Compat	jmpn	Int16_Actual

; ---------------------------------------------------------------------------

; [Compat] Int9 (keyboard input) entrypoint must be same as XT BIOS
		FillRom	0E987h, 0FFh
Int9_Compat	jmpn	Int9_Actual

; ---------------------------------------------------------------------------

; [Compat] Int13 floppy disk entrypoint must be the same as the XT BIOS
		FillRom	0EC59h,0FFh
Int13Fd_Compat	jmpn	Int13Fd_Actual

; ---------------------------------------------------------------------------

; [Compat] IntE (floppy interrupt) must be same location as the XT BIOS
		FillRom	0EF57h, 0FFh
IntE_Compat	jmpn	IntE_Actual

; ---------------------------------------------------------------------------

; [Compat] Default diskette parameter table expected at XT BIOS location
		FillRom	0EFC7h, 0FFh
DisketteParams	db	0DFh	; 1st specify byte
		db	2	; 2nd specify byte
		db	25h	; Motor wait time
		db	2	; 512 bytes/sector
		db	12h	; Last sector on track
		db	1Bh	; Gap length
		db	0FFh	; DTL(?)
		db	54h	; Gap length for format
		db	0F6h	; Fill byte for format
		db	0Fh	; Head settle time (milliseconds)
		db	8	; Motor start time (1/8 seconds)

; ---------------------------------------------------------------------------

; [Compat] Int17 (printer services) must be same location as the XT BIOS
Int17_Compat	jmpn	Int17_Actual

; ---------------------------------------------------------------------------

; [Compat] Int10 (video services) must be same location as the XT BIOS
		FillRom	0F065h, 0FFh
Int10_Compat	jmpn	Int10_Actual

; ---------------------------------------------------------------------------

; [Compat] Video mode tables should be the same as the XT BIOS
		FillRom	0F0A4h, 0FFh
VidModeTable	db	38h, 28h, 2Dh, 0Ah, 1Fh, 6, 19h		; CGA 40x25
		db	1Ch, 2, 7, 6, 7
		db	0, 0, 0, 0
		db	71h, 50h, 5Ah, 0Ah, 1Fh, 6, 19h		; CGA 80x25
		db	1Ch, 2, 7, 6, 7
		db	0, 0, 0, 0
		db	38h, 28h, 2Dh, 0Ah, 7Fh, 6, 64h		; CGA graphics
		db	70h, 2, 1, 6, 7
		db	0, 0, 0, 0
		db	61h, 50h, 52h, 0Fh, 19h, 6, 19h		; MDA 80x25
		db	19h, 2, 0Dh, 0Bh, 0Ch
		db	0, 0, 0, 0

VidRegenLengths	dw	2048,4096,16384,16384

VidColumns	dw	2828h,5050h,2828h,5050h			; Not sure about these...

VidModeSets	db	2Ch,28h,2Dh,29h,2Ah,2Eh,1Eh,29h

; GRiD password code is placed after the video constants.
; We'll use a second code segment to hold the EOI functions
; and let the linker worry about putting it all in the correct place.
[CODE2]	SEGMENT WIDTH=16, ALIGN=1, CLASS=CODE, PURPOSE=DATA|CODE, COMBINE=PUBLIC

; ---------------------------------------------------------------------------
; EoiPic1
; Interrupt handler.  Sends a non-specific end-of-interrupt to PIC1.
EoiPic1		PROC
		push	ax
		mov	al, NONSPECIFIC_EOI
		out	PORT_PIC1_CTRL, al
		pop	ax
		iret
		ENDPROC	EoiPic1

; ---------------------------------------------------------------------------
; EoiPic1and2
; Interrupt handler.  Sends a non-specific end-of-interrupt to both PICs.
EoiPic1and2	PROC
		push	ax
		mov	al, NONSPECIFIC_EOI
		out	PORT_PIC2_CTRL, al
		out	PORT_PIC1_CTRL, al
		pop	ax
		iret
		ENDPROC	EoiPic1and2

; GRiD IDE Identify, memory controller, and HD spindown code is placed after the
; EOI function.  We'll use a third code segment to hold the remainder of the
; at_compat code and let the linker worry about putting it all in the correct place.
[CODE3]	SEGMENT WIDTH=16, ALIGN=1, CLASS=CODE, PURPOSE=DATA|CODE, COMBINE=PUBLIC

%XxxBase	%seta 0F841h

; ---------------------------------------------------------------------------
; [Compat] Int12 (memory size) must be at same location as the XT BIOS
		;FillRom	0F841h, 0FFh
Int12_Compat	jmpn	Int12_Actual

; ---------------------------------------------------------------------------
; [Compat] Int11 (equipment check) must be at same location as the XT BIOS
		FillRom	0F84Dh, 0FFh
Int11_Compat	jmpn	Int11_Actual

; ---------------------------------------------------------------------------
; [Compat] Int15 (AT extended services) must be at the same location as the
;          IBM XT and AT BIOSes.  In the XT, this was for cassette services
;          and the AT repurposed it for PC/AT-specific BIOS calls.  GRiD
;          also extended it with their own APIs.
		FillRom	0F859h, 0FFh
Int15_Compat	jmpn	Int15_Actual

; ---------------------------------------------------------------------------
; [Compat] CGA graphics character set must be in same location as the XT BIOS
		FillRom	0FA6Eh, 0FFh
GraphicsChars:
INCLUDE		"graphics_charset.asm"

; ---------------------------------------------------------------------------
; [Compat] Int1A (timer) must be in the same location as the XT BIOS
Int1A_Compat	jmpn	Int1A_Actual

; ---------------------------------------------------------------------------
; [Compat] Int8 (timer interrupt) must be in the same location as the XT BIOS
		FillRom	0FEA5h, 0FFh
Int8_Compat	jmpn	Int8_Actual

; ---------------------------------------------------------------------------
; [Compat] Initial IVT contents must be in the same location as the XT BIOS
;          because some programs will try to restore interrupts by copying
;          out of the BIOS, ignoring that other programs may have hooked the
;          interrupt in the meantime.
		FillRom	0FEF3h, 0FFh
InitialIvt:	; PIC1 hardware interrupts
		dw	Int8_Compat	; Int8: timer
		dw	Int9_Compat	; Int9: keyboard
		dw	EoiPic1		; IntA: cascade input
		dw	UnexpectedInt	; IntB: unused
		dw	UnexpectedInt	; IntC: unused
		dw	UnexpectedInt	; IntD: unused
		dw	IntE_Compat	; IntE: diskette
		dw	EoiPic1		; IntF: unused?
		; Software interrupts
		dw	Int10_Compat	; Int10: video
		dw	Int11_Compat	; Int11: equipment
		dw	Int12_Compat	; Int12: memory size
		dw	Int13Fd_Compat	; Int13: disks
		dw	Int14_Compat	; Int14: serial
		dw	Int15_Compat	; Int15: AT/GRiD services
		dw	Int16_Compat	; Int16: keyboard
		dw	Int17_Compat	; Int17: printer
		dw	Int18		; Int18: BASIC
		dw	Int19_Compat	; Int19: bootloader
		dw	Int1A_Compat	; Int1A: RTC
		dw	SoftwareIret	; Int1B: keyboard break
		dw	SoftwareIret	; Int1C: timer break
		dw	VidModeTable	; Video parameters
		dw	DisketteParams	; Disk parameters
		dw	0		; Pointer to video extensions
		; PIC2 hardware interrupts
		dw	Int70		; Int70: RTC
		dw	Int71		; Int71: redirect to IntA
		dw	UnexpectedInt	; Int72: unused
		dw	UnexpectedInt	; Int73: unused
		dw	UnexpectedInt	; Int74: unused
		dw	Int75		; Int75: NPU/FPU
		dw	UnexpectedInt	; Int76: unused
		dw	EoiPic1and2	; Int77: ???

; ---------------------------------------------------------------------------
; [Compat] Empty software interrupt routine may be reused by applications
;          so it can't move from where the XT BIOS placed it
		FillRom	0FF53h, 0FFh
SoftwareIret	iret

; ---------------------------------------------------------------------------
; [Compat] Print screen handler is called by applications; don't move it
PrntScrn_Compat	jmpn	PrntScrn_Actual


; ---------------------------------------------------------------------------
		FillRom	0FFD2h, 0FFh
DelayFactor	db	64h	; For short timing loops, CPU-dependent
BeepFactor	db	3Ch	; Timing for speaker beep pitch

; GRiD BIOS has additional bytes following the timing constants.
; Their purpose is currently unknown.
		db	15h, 15h, 31h, 31h, 3, 3, 6, 6

; ---------------------------------------------------------------------------

; GRiD BIOS date (and maybe checksums?).  Split so even/odd ROMs each get a copy.
		db	'0011//1199//8877'
		db	0,0
		db	0E0h,24h

ENDPROGRAM	AT_COMPAT
