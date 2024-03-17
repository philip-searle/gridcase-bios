
; ---------------------------------------------------------------------------
; GRiD-specific I/O ports and definitions
; ---------------------------------------------------------------------------

; ---------------------------------------------------------------------------
; CPU speed selection [TechRef 2-15]
PORT_CPU_SPEED		equ	416h
CPU_SPEED_HIGH		equ	000h
CPU_SPEED_LOW		equ	001h
CPU_SPEED_RESERVED	equ	0FEh

; ---------------------------------------------------------------------------
; ROM subsystem select/status
; Mentioned in [TechRef 3-30] with the note "Write subsystem select, read
; subsystem status" but unfortunately not elaborated on.  Possibly this is
; just one port incompletely decoded.
PORT_ROM_SUBSYSTEM1	equ	6F8h
PORT_ROM_SUBSYSTEM2	equ	6F9h
PORT_ROM_SUBSYSTEM3	equ	6FAh
PORT_ROM_SUBSYSTEM4	equ	6FBh
PORT_ROM_SUBSYSTEM5	equ	6FCh
PORT_ROM_SUBSYSTEM6	equ	6FDh
PORT_ROM_SUBSYSTEM7	equ	6FEh
PORT_ROM_SUBSYSTEM8	equ	6FFh

; Upper half of PORT_ROM_SUBSYSTEM1 contains type ID for the internal
; floppy/hard disk backplane.  This is used to determine the number of
; floppy drives (when combined with the external floppy detect bits in
; the parallel port status registers), as well as the presence of
; internal hard disk drives and the method to use to detect its size.
; Note that if a backplane supporting internal floppy drives is present
; then it is assumed that all of them are populated 3.5" 1.44MB models.
GRID_BKPL_MASK	equ	0F0h
				;	Floppy0	Floppy1	HardDisk
GRID_BKPL_NONE	equ	00h	; 0  =	None	None	None
GRID_BKPL_10	equ	10h	; 1  =	1.44MB	None	None
GRID_BKPL_20	equ	20h	; 2  =	1.44MB	1.44MB	None
GRID_BKPL_30	equ	30h	; 3  =	1.44MB	None	XT-compatible (subtypes 1/2/3)
GRID_BKPL_40	equ	40h	; 4  =	None	None	??-compatible (subtype 4)
GRID_BKPL_50	equ	50h	; 5  =	None	None	AT-compatible (subtype 4)
GRID_BKPL_60	equ	60h	; 6  =	None	None	None - not used?
GRID_BKPL_70	equ	70h	; 7  =	None	None	AT-compatible (subtypes 5/6/7/9 identified by model number)
GRID_BKPL_80	equ	80h	; 8  =	None	None	XT-compatible (unknown)
GRID_BKPL_90	equ	90h	; 9  =	1.44MB	None	AT-compatible (subtypes 5/6/7/8 identified by model number)
; Backplanes IDs A-F not used?

; ---------------------------------------------------------------------------
; GRiD hard disk autodetect values
; 				  Iface	BIOS		SpinDown
; 				   Type	Type	 Size	Support		Notes
GRID_HD_3	equ	3	;    XT
GRID_HD_4	equ	4	;    AT   17	 40MB	No
GRID_HD_5	equ	5	;    AT    2	 20MB	Yes
GRID_HD_6	equ	6	;    AT   17	 40MB	Yes
GRID_HD_7	equ	7	;    AT  224	100MB	No		*1
GRID_HD_9	equ	9	;    AT   17	 40MB	Yes

; *1 Drive may support power commands but [TechRef 3-25] claims it is
;    not supported in the BIOS because the disk's mass would consume
;    too much power on spinup to be worthwhile spinning down.

; ---------------------------------------------------------------------------
; GRiD int15 extensions
GRID_INT15_ERROR	equ	86h	; Error return in AX for unsupported functions
GRID_INT15_OUTOFRANGE	equ	02h	; Error return in AX for input param out of range
GRID_INT15_NOTAPEDRIVE	equ	02h	; Return in AX for int 15/E444h when no tape drive detected

; ---------------------------------------------------------------------------
; (-W) Application ROM enable [TechRef 2-13]
; Controls whether address space A0000h-BF000h is mapped to display RAM or to
; the ROMs mounted in the user-accessible sockets above the keyboard.
; 0 == map display RAM
; 1 == map application ROMs (mapped at top of address space)
PORT_APPROM_ENABLE	equ	405h

; ---------------------------------------------------------------------------
; (-W) Application ROM select [TechRef 2-13]
; Controls which ROM socket is mapped into application ROM space.
; PORT_APPROM_ENABLE must be set to 1 for the ROM contents to be visible.
PORT_APPROM_SELECT	equ	440h

; ---------------------------------------------------------------------------
; Mystery ports!
; No idea what these do yet.
PORT_UNKNOWN_426	equ	0426h	; set to 1 then 0 in POST
PORT_UNKNOWN_FFF	equ	0FFFh	; set to 1 in POST
