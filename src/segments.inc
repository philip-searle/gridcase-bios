
; BIOS code (F000:8000 - F000:FFFF)
BIOS_SEGMENT	equ	0F000h
BIOS_START	equ	08000h

BIOS_START_PARA	equ	(BIOS_SEGMENT + BIOS_START >> 4)

; BIOS stack (0000:0300 - 0000:0380)
STACK_SEGMENT	equ	0030h
STACK_TOP	equ	0080h

; BIOS data area (0000:0400 - 0000:04FF)
BDA_SEGMENT	equ	0040h

; Interrupt vector table (0000:0000 - 0000:0400)
IVT_SEGMENT	equ	0000h

; Upper and lower limits for hard disk option ROM search
HD_ROM_SEG_LO	equ	0C000h
HD_ROM_SEG_HI	equ	0C800h

[IVT]

IvtInt00	equ	00h * 4
IvtInt02	equ	02h * 4
IvtInt05	equ	05h * 4
IvtInt08	equ	08h * 4
IvtInt14	equ	14h * 4
IvtInt1F	equ	1Fh * 4
IvtInt60	equ	60h * 4
IvtInt67	equ	67h * 4
IvtInt70	equ	70h * 4

[BDA]

; Base I/O port for each detected serial port
; POST code assumes these are stored contiguously
Ser1BasePort		d w
Ser2BasePort		d w
Ser3BasePort		d w
Ser4BasePort		d w

; Base I/O port for each detected parallel port
; POST code assumes these are stored contiguously
Par1BasePort		d w
Par2BasePort		d w
Par3BasePort		d w

; Extended BIOS data area segment address
EbdaSegment		d w

EquipmentWord		d w

; TODO: document what uses this
InterruptFlag		d b

MemorySizeKb		d w

; TODO: document what this is
ErrorCodes		d w

KB_BUFFER_LENGTH	equ	20h
KbShiftFlags1		d b
KbShiftFlags2		d b
AltNumpad		d b
KbNextChar		d w
KbLastChar		d w
KbBuffer		d KB_BUFFER_LENGTH * b

; Various places document this as 'adapter ROM offset/segment' with no
; further details.  It's actually just a scratch area for storing a far
; code pointer temporarily.
$		equ	0067h
AdapterRomOffset	d w
AdapterRomSegment	d w

$		equ	0072h
SoftResetFlag		d w
SOFT_RESET_FLAG	equ	1234h	; Set in SoftResetFlag to mark software-initiated reset via JMP to F000:FFF0

$		equ	78h
DEFAULT_PARALLEL_TIMEOUT	equ	14h
ParPort1Timeout		d b
ParPort2Timeout		d b
ParPort3Timeout		d b

$		equ	7CH
DEFAULT_SERIAL_TIMEOUT		equ	01h
SerPort1Timeout		d b
SerPort2Timeout		d b
SerPort3Timeout		d b
SerPort4Timeout		d b

KbBufStart		d w
KbBufEnd		d w
