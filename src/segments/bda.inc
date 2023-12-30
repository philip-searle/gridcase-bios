
	include1	"segments.inc"

; BIOS Data Area layout and constants

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

; Par4BasePort in XT BIOS.
; Extended BIOS data area segment address in post-AT machines.
; 1520 BIOS uses it to store external floppy drive state.
Fd2Cylinder		d b
Fd2OpStartState		d b

EquipmentWord		d w

; TODO: document what uses this
InterruptFlag		d b

MemorySizeKb		d w

; The speaker in the 1500-series computer doesn't have an adjustable volume,
; so we emulate it by using KbClickVol to control the duration of the beeps
; used by the key click feature.  Note that although it's a word value,
; initialized to 100h the code that implements the key click only uses the
; first byte and clamps it's value to the 0-127 range.
; [Compat] The IBM BIOS refers to these two bytes as MFG_ERR_FLAG and
; documents them as "SCRATCHPAD FOR MANUFACTURING ERROR CODES".  The Compaq
; 286 BIOS has a keyclick feature that matches our usage here.
KbClickVol		d w
DEFAULT_KBCLICK_VOL	equ	100h

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

;XXX: CompatAddress		3Eh
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

;XXX: CompatAddress		3Fh
; Which floppy drive motors the BIOS thinks should be enabled
FdMotorStatus		d b
FDMOTOR_UNUSED	equ	0C8h
FDMOTOR_DRIVE	equ	30h
FDMOTOR_MOTOR2	equ	04h
FDMOTOR_MOTOR1	equ	02h
FDMOTOR_MOTOR0	equ	01h

;XXX: CompatAddress		40h
FdMotorTimeout		d b

; A 'packet' of values to send to the XT-compatible disk controller's
; registers.  The IBM Fixed Disk Drive Adapter BIOS refers to this as
; the Device Control Block (DCB).  [TechRef 9-43] refers to it as a
; 6-byte command block.  Field layouts given below are taken from the
; IBM Fixed Disk Drive Adapter manual, so may not quite match what
; the JVC disk controller calls them.
HD_XT_REG_PACK	struc
.command	db	0	; Command (determines interpretation of subsequent bytes)
.driveHead	db	0	; 00dhhhhh (d=drive, h=head)
.chSector	db	0	; ccssssss (c=cylinder high bits, s=sector)
.cylinderLo	db	0	; Cylinder low bits
.blockCount	db	0	; Block count or interleave factor
.controlByte	db	0	; ra000sss (r=retry, a=retry on ECC, s=step option)
		endstruc

; A 'packet' of values to send to the AT-compatible disk controller's
; registers.  Used by HdAtBuildRegPack/HdAtSendRegPack.
HD_AT_REG_PACK	struc
		db	0	; dummy byte because we share storage
				; with an FdScratch instance
.writePrecomp	db	0
.sectorCount	db	0
.sectorNumber	db	0
.cylinderLo	db	0
.cylinderHi	db	0
.driveHead	db	0
.command	db	0
		endstruc

; Scratch space used by floppy disk int13 handlers to pass return values
; back to the main int13 handler and store command results read from the
; floppy disk controller.  Overlaid onto the same storage space as
; HD_AT_REG_PACK since the floppy and hard disk routines will not be
; accessed simultaneously.
FDC_SCRATCH	struc
.returnAh	db	0	; return value to be placed in AH

.fdcSt0		db	0	; fields returned from a standard 7-byte
.fdcSt1		db	0	; FDC command result
.fdcSt2		db	0
.fdcCylinder	db	0
.fdcHead	db	0
.fdcSector	db	0
.fdcNumBytes	db	0
		endstruc

$		equ	41h
; int13 hard disk and floppy disk handlers do not execute simultaneously
; so they share temporary storage space in the BDA
; in the same way, AT and XT hard disk controllers aren't installed at
; the same time so they use the same storage space
Int13Scratch		d 8 * b
$		equ	Int13Scratch
			db	0	; dummy byte because we share storage
					; with an FdScratch instance
HdXtRegPacket		ds	HD_XT_REG_PACK
$		equ	Int13Scratch
HdAtRegPacket		ds	HD_AT_REG_PACK
$		equ	Int13Scratch
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

LastInterrupt		d b

$		equ	006Ch
TimerTicks		d d
TimerOverflow		d b
; Equivalent to (24 * 60 * 60 * (1193180.0 / 65536.0))
; See [TechRef 6-8] for where this ratio is defined
TIMER_TICKS_PER_DAY	equ	1_573_040

; Non-zero if ctrl+break entered at keyboard
CtrlBrkFlag		d b

SoftResetFlag		d w
SOFT_RESET_FLAG		equ	1234h	; Set in SoftResetFlag to mark software-initiated reset via JMP to F000:FFF0
CRITICAL_ERR_FLAG	equ	0001h	; Set in SoftResetFlag to indicate user must acknowledge error during POST

;XXX: CompatAddress	74h
HdLastOpStatus		d b
HdCount			d b
HdControl		d b
HdIoOffset		d b

;XXX: CompatAddress	78h
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

;XXX: CompatAddress	7Ch
DEFAULT_SERIAL_TIMEOUT		equ	01h
SerPort1Timeout		d b
SerPort2Timeout		d b
SerPort3Timeout		d b
SerPort4Timeout		d b

;XXX: CompatAddress	80h
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
Fd0OpStartState		d b
Fd1OpStartState		d b
Fd0Cylinder		d b
Fd1Cylinder		d b
; Fd2Cylinder stored elsewhere (external drive)

FDDI_UNUSED		equ	08h
FDDI_DETERMINED		equ	04h	; Drive type determined?
FDDI_MULTIRATE		equ	02h	; Drive is multi-rate?
FDDI_CHANGELINE		equ	01h	; Drive has change line detection?

; Several places test the 'drive type determined' and 'multi-rate' bits
; in the FdDriveInfo byte to determine whether they can safely operate
; on the current drive/media combination.  This constant keeps us from
; having to repeat the very long expression.
; TODO: figure out why we're testing the multi-rate bits?
FDDI_DRIVES_OK		equ	((FDDI_DETERMINED | FDDI_MULTIRATE) << 4) | FDDI_DETERMINED | FDDI_MULTIRATE

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

TimerNotifyPtrOffset	d w
TimerNotifyPtrSegment	d w
TimerValue		d d
TimerFlags		d b
TIMER_SET		equ	01h	; Timer value set
TIMER_EXPIRED		equ	80h	; Timer value expired
TIMER_RESERVED		equ	7Eh

$		equ	0A6h
Fd2DriveInfo		d b		; as FdDriveInfo, but for the external drive

; 0040:0100 is equivalent to 0050:0000
; Technically outside the BDA, but segment 50 seems to have always been
; intended as an extended BDA/BASIC/DOS data segment.
$		equ	100h
PrtScrStatus		d b
PRNTSCRN_OK		equ	0
PRNTSCRN_INPROGRESS	equ	1
PRNTSCRN_ERROR		equ	0FFh

[CODE]
