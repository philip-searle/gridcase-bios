
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

CompatAddress		3Eh
; Floppy drive calibration status, i.e. whether the disk motor is up
; to speed.  Our BIOS also uses the uppermost bit to communicate between
; floppy disk int13 handlers and the intE hardware interrupt, although
; this doesn't appear to be standardized between BIOS vendors.
FdCalibStatus		d b
FDCAL_INT	equ	80h		; 1 = waiting for intE to occur
FDCAL_RESERVED	equ	78h
FDCAL_FD2	equ	04h		; 1 = drive 1 calibrated
FDCAL_FD1	equ	02h		; 1 = drive 1 calibrated
FDCAL_FD0	equ	01h		; 1 = drive 0 calibrated

CompatAddress		3Fh
; Which floppy drive motors the BIOS thinks should be enabled
FdMotorStatus		d b
FDMOTOR_UNUSED	equ	0C8h
FDMOTOR_DRIVE	equ	30h
FDMOTOR_MOTOR2	equ	04h
FDMOTOR_MOTOR1	equ	02h
FDMOTOR_MOTOR0	equ	01h

CompatAddress		40h
FdMotorTimeout		d b

$		equ	41h
; int13 hard disk and floppy disk handlers do not execute simultaneously
; so they share temporary storage space in the BDA
HdRegPacket		ds	HDC_REG_PACKET
$		equ	HdRegPacket
FdScratch		ds	FDC_SCRATCH

$		equ	49h
VidActiveMode		d b
VidTextColumns		d w
VidPageSize		d w
VidPageOffset		d w
VidCursorPos		d 8 * w
VidCursorShape		d w
VidActivePage		d b

$		equ	63h
; I/O port address of the MDA/CGA index register
VidBasePort		d w

; Copy of MDA/CGA 6845 mode control register
VidModeCtrl		d b

VidColorPalette		d b

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

$		equ	8Bh
FdConfig		d b	; Bitfield, see FDCONF_* constants

FDCONF_DATA_RATE	equ	0C0h	; last data rate sent to contoller
					;   mask for FDCONF_DTR_* values
FDCONF_STEP_RATE	equ	030h	; last step rate sent to controller
					;   mask for FDCONF_SR_* values
FDCONF_OP_DTR		equ	00Ch	; data rate, set at start of operation
FDCONF_UNUSED		equ	003h

$		equ	8Ch
HdStatus		d b
HdError			d b
HdTaskComplete		d b

$		equ	8Fh
FdDriveInfo		d b	; Bitfield, see FDDI_* constants (FD1/0 in upper/lower nibble)
Fd0MediaState		d b	; Bitfield, see FDMS_* constants
Fd1MediaState		d b
; Fd2MediaState stored elsewhere (external drive)

FDDI_UNUSED		equ	08h
FDDI_DETERMINED		equ	04h	; Drive type determined?
FDDI_MULTIRATE		equ	02h	; Drive is multi-rate?
FDDI_CHANGELINE		equ	01h	; Drive has change line detection?

FDMS_DATA_TRANSFER_RATE	equ	0C0h	; mask for FDMS_DTR_* values
FDMS_DOUBLE_STEP	equ	20h	; double stepping required?
FDMS_KNOWN_MEDIA	equ	10h	; media in drive known?
FDMS_UNUSED		equ	08h
FDMS_LAST_ACCESS	equ	07h	; mask for FDMS_LA_* values

FDMS_DTR_500K		equ	00h	; 500Kbit/sec data transfer rate
FDMS_DTR_300K		equ	40h	; 300kbit/sec
FDMS_DTR_250K		equ	80h	; 250Kbit/sec
FDMS_DTR_1M		equ	0C0h	; 1Mbit/sec

FDMS_LA_T360M_360D	equ	00h	; trying 360K media in 360K drive
FDMS_LA_T360M_12D	equ	01h	; trying 360K media in 1.2M drive
FDMS_LA_T12M_12D	equ	02h	; trying 1.2M media in 1.2M drive
FDMS_LA_K360M_360D	equ	03h	; known 360K media in 360K drive
FDMS_LA_K360M_12D	equ	04h	; known 360K media in 1.2M drive
FDMS_LA_K12M_12D	equ	05h	; known 1.2M media in 1.2M drive
FDMS_LA_UNUSED		equ	06h
FDMS_LA_35		equ	07h	; 720K media in 720K drive or
					; 1.44M media in 1.44M drive

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

$		equ	0A6h
Fd2DriveInfo		d b		; as FdDriveInfo, but for the external drive

