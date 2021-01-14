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

; Commands for PORT_KBC_CMD
KBC_CMD_RESET	equ	0AAh	; reset and self-test
KBC_CMD_WRITE	equ	060h	; next write to data port is a command
KBC_CMD_READ	equ	0C0h	; read KBC input port and place on KBC_PORT_DATA
KBC_CMD_RD_TST	equ	0E0h	; read test inputs (T0, T1)
KBC_CMD_ENABLE	equ	0AEh	; enable keyboard
KBC_CMD_IFTEST	equ	0ABh	; test keyboard interface

; Misc KBC constants
; Why some many different timeouts?
KBC_RESET_TIMEOUT	equ	1870h	; 18h x 70h loops waiting for reset response
KBC_SHORT_TIMEOUT	equ	0179h	; 01h x 79h loops waiting for write command response
KBC_STUCK_KEY_TIMEOUT	equ	02B7h	; 02h x B7h loops waiting for stuck key response
KBC_IFTEST_TIMEOUT	equ	02E7h	; 02h x E7h loops waiting for interface test to complete
KBC_RESET_OK		equ	55h	; success response from KBC self test

; ---------------------------------------------------------------------------
; Keyboard (not controller) commands and responses
KB_CMD_RESET		equ	0FFh	; reset keyboard and run basic assurance test
KB_CMD_READ_KB_ID	equ	0F2h	; read 2-byte keyboard ID

KB_REPLY_BAT_OK		equ	0AAh	; reset complete and BAT tests OK
KB_REPLY_RESEND		equ	0FEh	; host should resend last command
KB_REPLY_ACK		equ	0FAh	; 'acknowledge' ???

KB_RESET_TIMEOUT	equ	3908h
KB_RESET_REPLY_DELAY	equ	0C8h	; this x 100h x 2 io_delay = delay between
					; keyboard reset and any expected response

