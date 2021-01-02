
; BIOS code (F000:8000 - F000:FFFF)
BIOS_SEGMENT	equ	0F000h
BIOS_START	equ	08000h

BIOS_START_PARA	equ	(BIOS_SEGMENT + BIOS_START >> 4)

; BIOS stack (0000:0300 - 0000:0380)
STACK_SEGMENT	equ	0030h
STACK_TOP	equ	0080h

; BIOS data area (0000:0400 - 0000:04FF)
BDA_SEGMENT	equ	0040h

; Upper and lower limits for hard disk option ROM search
HD_ROM_SEG_LO	equ	0C000h
HD_ROM_SEG_HI	equ	0C800h

[BDA]

; Base I/O port for each detected serial port
; POST code assumes these are stored contiguously
ser1BasePort		d w
ser2BasePort		d w
ser3BasePort		d w
ser4BasePort		d w

; Base I/O port for each detected parallel port
; POST code assumes these are stored contiguously
par1BasePort		d w
par2BasePort		d w
par3BasePort		d w

; Extended BIOS data area segment address
ebdaSegment		d w

equipmentWord		d w

interruptFlag		d b

memorySizeKb		d w

errorCodes		d w

KB_BUFFER_LENGTH	equ	20h
kbShiftFlags1		d b
kbShiftFlags2		d b
altNumpad		d b
kbNextChar		d w
kbLastChar		d w
kbBuffer		d KB_BUFFER_LENGTH * b

$		equ	0067h
AdapterRomOffset	d w
AdapterRomSegment	d w

$		equ	0072h
SoftResetFlag		d w
SOFT_RESET_FLAG	equ	1234h	; Set in SoftResetFlag to mark software-initiated reset via JMP to F000:FFF0

$		equ	78h
DEFAULT_PARALLEL_TIMEOUT	equ	14h
parPort1Timeout		d b
parPort2Timeout		d b
parPort3Timeout		d b

$		equ	7CH
DEFAULT_SERIAL_TIMEOUT		equ	01h
serPort1Timeout		d b
serPort2Timeout		d b
serPort3Timeout		d b
serPort4Timeout		d b

kbBufStart		d w
kbBufEnd		d w

