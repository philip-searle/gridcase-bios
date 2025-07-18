
; =====================================================================
; I/O ports used by the floppy disk controller
; =====================================================================

; [TechRef 9-16] (-W) Digital Output Register
; An 8-bit write-only register controlling drive, motor, and special
; feature selection.
PORT_FDC_DIGOUT		equ	3F2h
FDC_DIGOUT_RESERVED	equ	80h
FDC_DIGOUT_MOTOR	equ	70h	; Motor enable bits
					;   000 = No motors enabled
					;   001 = Drive A: motor on
					;   010 = Drive B: motor on
					;   100 = Drive E: motor on
					;   Other combinations documented
					;   as 'invalid'
FDC_DIGOUT_DMAINT	equ	08h	; 1 = Enable IRQ6, DMA channel 2
FDC_DIGOUT_RESET	equ	04h	; 0 = Reset FDC
FDC_DIGOUT_DRIVE	equ	03h	; Drive enable bits
					;   00 = Drive A: enabled
					;   01 = Drive B: enabled
					;   10 = Drive E: enabled
					;   11 = Not applicable

; [TechRef 9-17] (R-) Main Status Register
; An 8-bit read-only register that can be accessed at any time.
PORT_FDC_STATUS		equ	3F4h
FDC_STATUS_REQUEST	equ	80h	; Request for master
					;   0 = No request active
					;   1 = Data register is ready to
					;       send/receive data.
FDC_STATUS_DIRECTION	equ	40h	; Data transfer direction
					;   0 = CPU -> FDC
					;   1 = FDC -> CPU
FDC_STATUS_DMA		equ	20h	; DMA/non-DMA mode
					;   0 = DMA mode
					;   1 = non-DMA mode
FDC_STATUS_BUSY		equ	10h	; When set, FDC is reading/writing
FDC_STATUS_RESERVED	equ	0Ch
FDC_STATUS_SEEK2	equ	02h	; Drive B: or E: in seek mode
FDC_STATUS_SEEK		equ	01h	; Drive A: in seek mode

; [TechRef 9-17] (RW) Data Register
; An 8-bit read/write buffer.
PORT_FDC_DATA		equ	3F5h

; [TechRef 9-18] (R-) Digital Input Register
; An 8-bit read-only register, shared with the hard disk controller.
PORT_FDC_DIGIN		equ	3F7h
FDC_DIGIN_CHANGELINE	equ	80h	; Disk change detection
					;   0 = change line inactive
					;   1 = change line active (only
					;       checked for non-360KB media)
PORT_DIGIN_RESERVED	equ	7Fh	; Reserved for hard disk controller

; [TechRef 9-18] (-W) Disk Control Register
; An 8-bit write-only register used to select media density and transfer rate.
PORT_FDC_DISK_CONTROL	equ	3F7h
FDC_DISK_RESERVED	equ	0FCh
FDC_DISK_MEDIA		equ	03h	; Media density and transfer rate
					;   00 = Normal density, 500kb/sec
					;   01 = Not supported
					;   10 = High density, 250kb/sec
					;   11 = Invalid

; =====================================================================
; Command bytes that can be send to the floppy disk controller.
; [TechRef 9-20]
; Constants commented with '*' can have extra bits set in the upper
; nibble to indicate things such as MT/MF/SK (see TechRef table 9-5).
; =====================================================================

FDC_CMD_READ		equ	06h	; * Read data
FDC_CMD_READ_DEL	equ	0Ch	; * Read deleted data
FDC_CMD_WRITE		equ	05h	; * Write data
FDC_CMD_WRITE_DEL	equ	09h	;   Write deleted data
FDC_CMD_READ_ID		equ	0Ah	; * Read first valid sector ID after
					;   index hole from cylinder
FDC_CMD_READ_TRACK	equ	02h	; * Read contents of cylinder from
					;   index hole to end of track
FDC_CMD_SCAN_EQ		equ	11h	;   Compare data
FDC_CMD_SCAN_LTEQ	equ	19h	;   Compare data
FDC_CMD_SCAN_GTEQ	equ	10h	;   Compare data
FDC_CMD_FORMAT		equ	0Dh	; * Format an entire cylinder
FDC_CMD_RECALIBRATE	equ	07h	; * Retract head to track zero
FDC_CMD_SIS		equ	08h	; * Determine cause of interrupt
FDC_CMD_SPECIFY		equ	03h	;   Set head load time and drive
					;   stepping rate
FDC_CMD_SDS		equ	04h	; * Read drive status from ST3
FDC_CMD_SEEK		equ	0Fh	; * Move head to new cylinder

; Additional bits for commands above marked with '*'
FDC_CMD_MT		equ	80h	; Multi-track command
FDC_CMD_MF		equ	40h	; MFM mode, not FM
FDC_CMD_SK		equ	20h	; Skip deleted data address marks

; =====================================================================
; Bit masks for the FDC status registers ST0-ST4
; =====================================================================
FDC_ST0_INTERRUPT_CODE	equ	0C0h	; Reason for most recent interrupt
					; 0 0 = normal termination of cmd
					; 0 1 = abrupt termination,
					;       incomplete execution
					; 1 0 = invalid commad,
					;       execution was not begun
					; 1 1 = abnormal termination due to
					;       drive READY signal changing
FDC_ST0_SEEK_END	equ	20h	; 0 = seek in progress,
					; 1 = seek complete
FDC_ST0_EQUIP_CHECK	equ	10h	; 0 = normal operation
					; 1 = fault from drive or no track 0
					;     after 77 steps during recalibrate
FDC_ST0_NOT_READY	equ	08h	; 0 = drive ready to accept data
					; 1 = read/write when drive not ready
FDC_ST0_HEAD		equ	04h	; 0 = head 0 selected at interrupt
					; 1 = head 1 selected at interrupt
FDC_ST0_UNIT_SELECT	equ	03h	; mask for index of unit selected
					; at interrupt time (0-3)

; Constants for the possible FDC_ST0_INTERRUPT_CODE bits
FDC_IC_NORMAL		equ	00h
FDC_IC_INCOMPLETE	equ	40h
FDC_IC_INVALID		equ	80h
FDC_IC_ABNORMAL		equ	0C0h

; ST1 has bits for error conditions
FDC_ST1_END_OF_CYLINDER	equ	80h
FDC_ST1_UNUSED		equ	48h
FDC_ST1_OVERRUN		equ	20h
FDC_ST1_NO_DATA		equ	04h
FDC_ST1_NOT_WRITABLE	equ	02h
FDC_ST1_MISSING_ADDR	equ	01h

; ST3 bits mirror the drive control lines
FDC_ST3_FAULT		equ	80h
FDC_ST3_WRITE_PROTECT	equ	40h
FDC_ST3_READY		equ	20h
FDC_ST3_TK0		equ	10h
FDC_ST3_TWO_SIDE	equ	08h
FDC_ST3_HEAD		equ	04h
FDC_ST3_US1		equ	02h
FDC_ST3_US0		equ	01h

; Minimum motor timeout following a disk operation
FD_MIN_MOTOR_TIMEOUT	equ	0EDh
