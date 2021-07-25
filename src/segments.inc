
; BIOS code (F000:8000 - F000:FFFF)
BIOS_SEGMENT	equ	0F000h
BIOS_START	equ	08000h

BIOS_START_PARA	equ	(BIOS_SEGMENT + BIOS_START >> 4)

; BIOS stack (0000:0300 - 0000:0380)
STACK_SEGMENT	equ	0030h
STACK_TOP	equ	0080h
STACK_TOP2	equ	0100h

; BIOS data area (0000:0400 - 0000:04FF)
BDA_SEGMENT	equ	0040h

; Interrupt vector table (0000:0000 - 0000:0400)
IVT_SEGMENT	equ	0000h

; Upper and lower limits for hard disk option ROM search
HD_ROM_SEG_LO	equ	0C000h
HD_ROM_SEG_HI	equ	0C800h

; Upper and lower limits for regular option ROM search
OPT_ROM_SEG_LO	equ	0C800h
OPT_ROM_SEG_HI	equ	0E000h

; Location system option ROM search
SYS_ROM_SEG	equ	0E000h

[IVT]

IvtInt00		equ	00h * 4
IvtInt02		equ	02h * 4
IvtInt05		equ	05h * 4
IvtInt08		equ	08h * 4
IvtInt13		equ	13h * 4
IvtInt14		equ	14h * 4
IvtDisketteTable	equ	1Eh * 4	; Int1E is a far pointer, not a real ISR
IvtInt1F		equ	1Fh * 4
IvtInt40		equ	40h * 4
IvtHd0Parms		equ	41h * 4	; Int41 is a far pointer, not a real ISR
IvtHd1Parms		equ	46h * 4	; Int46 is a far pointer, not a real ISR
IvtInt60		equ	60h * 4
IvtInt67		equ	67h * 4
IvtInt70		equ	70h * 4
IvtInt76		equ	76h * 4

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
KBSHIFT1_RSHIFT		equ	01h	; Right shift key down
KBSHIFT1_LSHIFT		equ	02h	; Left shift key down
KBSHIFT1_CTRL		equ	04h	; Control key down
KBSHIFT1_ALT		equ	08h	; Alt key down
KBSHIFT1_SCRLOCK	equ	10h	; Scroll lock on
KBSHIFT1_NUMLOCK	equ	20h	; Num lock on
KBSHIFT1_CAPLOCK	equ	40h	; Caps lock on
KBSHIFT1_INSERT		equ	80h	; Insert on

KbShiftFlags2		d b
KBSHIFT2_RALT		equ	01h	; Right alt key down
KBSHIFT2_LALT		equ	02h	; Left alt key down
KBSHIFT2_SYSREQ		equ	04h	; SysReq key down
KBSHIFT2_PAUSE		equ	08h	; Pause key activated
KBSHIFT2_SCRLOCK	equ	10h	; ScrollLock key down
KBSHIFT2_NUMLOCK	equ	20h	; NumLock key down
KBSHIFT2_CAPLOCK	equ	40h	; CapsLock key down
KBSHIFT2_INSERT		equ	80h	; Insert key down

AltNumpad		d b
KbNextChar		d w
KbLastChar		d w
KbBuffer		d KB_BUFFER_LENGTH * b

$		equ	41h
HdRegPacket		ds	HDC_REG_PACKET

$		equ	49h
VidActiveMode		d b

$		equ	63h
; I/O port address of the MDA/CGA index register
VidBasePort		d w

; Copy of MDA/CGA 6845 mode control register
VidModeCtrl		d b

; Various places document this as 'adapter ROM offset/segment' with no
; further details.  It's actually just a scratch area for storing a far
; code pointer temporarily.
$		equ	0067h
AdapterRomOffset	d w
AdapterRomSegment	d w

$		equ	006Ch
TimerTicks		d d

$		equ	0071h
; Non-zero if ctrl+break entered at keyboard
CtrlBrkFlag		d b

SoftResetFlag		d w
SOFT_RESET_FLAG	equ	1234h	; Set in SoftResetFlag to mark software-initiated reset via JMP to F000:FFF0

CompatAddress	74h
HdLastOpStatus		d b
HdCount			d b
HdControl		d b
HdIoOffset		d b

CompatAddress	78h
DEFAULT_PARALLEL_TIMEOUT	equ	14h
ParPort1Timeout		d b
ParPort2Timeout		d b
ParPort3Timeout		d b

; [TechRef 9-9] documents BDA offset 7Bh as the external floppy drive state
;               machine byte and notes that "The IBM AT uses absolute address
;               40:7Bh to store the timeout value for a third line printer
;               port (LPT3).  The LPT3 port is not used by GriDCase 1500
;               Series computers."
;               The IBM AT actually uses BDA offset 7Bh for LPT4.  After
;               substituting the correct LPT number, this note is accurate.
ParPort4Timeout		d b
Fd2MediaState	equ	ParPort4Timeout

CompatAddress	7Ch
DEFAULT_SERIAL_TIMEOUT		equ	01h
SerPort1Timeout		d b
SerPort2Timeout		d b
SerPort3Timeout		d b
SerPort4Timeout		d b

CompatAddress	80h
KbBufStart		d w
KbBufEnd		d w

$		equ	8Ch
HdStatus		d b
HdError			d b
HdTaskComplete		d b

$		equ	90h
Fd0MediaState		d b
Fd1MediaState		d b
; Fd2MediaState stored elsewhere (external drive)

$		equ	96h
KbStatusFlags3		d b
KBSTAT3_LASTE1		equ	01h	; Last scancode was E1h
KBSTAT3_LASTE0		equ	02h	; Last scancode was E0h
KBSTAT3_RCTRL		equ	04h	; Right control key active
KBSTAT3_RALT		equ	08h	; Right alt key active
KBSTAT3_ENHKB		equ	10h	; 101/102 key keyboard present
KBSTAT3_FORCE_NUMLOCK	equ	20h	; Forced numlock on
KBSTAT3_ID_2		equ	40h	; Reading two byte KB ID, read first byte
KBSTAT3_ID_1		equ	80h	; Reading two byte KB ID, read no bytes yet

KbStatusFlags4		d b
KBSTAT4_LED_SCRLOCK	equ	01h	; Scroll lock LED is lit
KBSTAT4_LED_NUMLOCK	equ	02h	; Num lock LED is lit
KBSTAT4_LED_CAPLOCK	equ	04h	; Caps lock LED is lit
KBSTAT4_RESERVED	equ	08h	; Reserved
KBSTAT4_ACK		equ	10h	; Ack received from KB
KBSTAT4_RESEND		equ	20h	; Resend requested by KB
KBSTAT4_LED_UPDATE	equ	40h	; LED update in progress
KBSTAT4_TX_ERR		equ	80h	; Keyboard transmit error

