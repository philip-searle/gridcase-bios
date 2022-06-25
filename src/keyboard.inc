; ---------------------------------------------------------------------------
; Keyboard controller
PORT_KBC_DATA	equ	60h
PORT_KBC_PORTB	equ	61h	; port B
PORT_KBC_STATUS	equ	64h	; (R-) status port
PORT_KBC_CMD	equ	64h	; (-W) command port

; Bitmasks for PORT_KBC_STATUS reads
KBC_STATUS_PERR	equ	80h	; keyboard data parity error
KBC_STATUS_RxTO	equ	40h	; recieve timeout
KBC_STATUS_TxTO	equ	20h	; transmit timeout
KBC_STATUS_INH	equ	10h	; keyboard comms inhibit
KBC_STATUS_A2	equ	08h	; A2 address line for last port write
KBC_STATUS_SYS	equ	04h	; Last reset type: 0=cold boot, 1=warm boot
KBC_STATUS_IBF	equ	02h	; input buffer full
KBC_STATUS_OBF	equ	01h	; output buffer full

; Commands for PORT_KBC_CMD [TechRef 8-16]
KBC_CMD_RESET	equ	0AAh	; reset and self-test
KBC_CMD_WRITE	equ	060h	; next write to data port is a command
KBC_CMD_READ	equ	0C0h	; read KBC input port and place on KBC_PORT_DATA
KBC_CMD_WRITEP2	equ	0D1h	; write KBC output port with next byte
KBC_CMD_RD_TST	equ	0E0h	; read test inputs (T0, T1)
KBC_CMD_DISABLE	equ	0ADh	; disable keyboard
KBC_CMD_ENABLE	equ	0AEh	; enable keyboard
KBC_CMD_IFTEST	equ	0ABh	; test keyboard interface
KBC_CMD_PULSEP2	equ	0FEh	; pulse KBC output port P2, bit 0, resetting the CPU

; Misc KBC constants
; Why some many different timeouts?
KBC_RESET_TIMEOUT	equ	1870h	; 18h x 70h loops waiting for reset response
KBC_SHORT_TIMEOUT	equ	0179h	; 01h x 79h loops waiting for write command response
KBC_STUCK_KEY_TIMEOUT	equ	02B7h	; 02h x B7h loops waiting for stuck key response
KBC_IFTEST_TIMEOUT	equ	02E7h	; 02h x E7h loops waiting for interface test to complete
KBC_DBYTE_TIMEOUT	equ	164Eh	; 5710 loops waiting for data byte write
KBC_WRITE_TIMEOUT	equ	2C9Ch	; 11420 loops waiting for output port write
KBC_RESET_OK		equ	55h	; success response from KBC self test

; ---------------------------------------------------------------------------
; Keyboard (not controller) commands and responses [TechRef 8-27 to 8-35]
KB_CMD_RESET		equ	0FFh	; reset keyboard and run basic assurance test
KB_CMD_READ_KB_ID	equ	0F2h	; read 2-byte keyboard ID
KB_CMD_SET_TYPEMATIC	equ	0F3h	; set typematic rate and delay
KB_CMD_DEFAULTS		equ	0F4h	; reset to defaults
KB_CMD_INDICATORS	equ	0EDh	; set/reset status indicators

KB_REPLY_BAT_OK		equ	0AAh	; reset complete and Basic Assessment Tests OK
KB_REPLY_RESEND		equ	0FEh	; host should resend last command
KB_REPLY_ACK		equ	0FAh	; 'acknowledge' ???
KB_REPLY_OVERRUN	equ	0FFh	; keyboard buffer capacity exceeded

KB_RESET_TIMEOUT	equ	3908h
KB_RESET_REPLY_DELAY	equ	0C8h	; this x 100h x 2 io_delay = delay between
					; keyboard reset and any expected response

; ---------------------------------------------------------------------------
; Keyboard scan code set 2
; [TechRef] refers to this as 'system scancodes', as opposed to 'keyboard
; scancodes' which are scan code set 1.
; Scan code set 3 are basically useless due to lack of software support and
; later hardware misimplementing it.  The 1520 BIOS has no support for it.
; Entries marked 'synthetic' are never returned by the hardware but are used
; by the Int9 handler to return extended keys combined with shift/ctrl/alt to
; the application layer (why?)
SC2_ESC			equ	01h
SC2_TAB			equ	0Fh
SC2_TAB_CTRL		equ	94h	; special code for ctrl+tab?
SC2_ENTER		equ	1Ch
SC2_LCTRL		equ	1Dh
SC2_LSHIFT		equ	2Ah
SC2_RSHIFT		equ	36h
SC2_ALT			equ	38h
SC2_CAPLOCK		equ	3Ah
SC2_F1			equ	3Bh
SC2_NUMLOCK		equ	45h
SC2_SCRLOCK		equ	46h
SC2_HOME		equ	47h
SC2_PGUP		equ	49h
SC2_END			equ	4Fh
SC2_PGDN		equ	51h
SC2_INSERT		equ	52h
SC2_SYSREQ		equ	54h
SC2_F11			equ	57h
SC2_F12			equ	58h

SC2_EXT_SOLIDUS		equ	35h	; numpad '/'
SC2_EXT_SOLIDUS_ALT	equ	0A4h	; numpad '/' with ALT (synthetic)
SC2_EXT_SOLIDUS_CTRL	equ	95h	; numpad '/' with CTRL (synthetic)

SC2_EXT_KP_STAR		equ	37h	; numpad '*'

SC2_KP_7		equ	47h
SC2_KP_DEL		equ	53h

SC2_EXT_SYSRQ		equ	72h	; ctrl+prtscr

SC2_EXT_ENTER_ALT	equ	0A6h	; numpad Enter with ALT (synthetic)

SC2_BREAK		equ	80h	; OR with make code to get break code

SC2_PREFIX_EXT		equ	0E0h	; prefix for extended (AT) scan code
SC2_PREFIX_NOBREAK	equ	0E1h	; prefix for scan code with no break code expected

