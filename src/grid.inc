
; ---------------------------------------------------------------------------
; GRiD-specific I/O ports and definitions
; ---------------------------------------------------------------------------

; ---------------------------------------------------------------------------
; ROM subsystem select/status
; Mentioned in [TechRef 3-30] with the note "Write subsystem select, read
; subsystem status" but unfortunately not elaborated on.  Possibly this is
; just one port incompletely decoded.
PORT_ROM_SUBSYSTEM1	EQU	6F8h
PORT_ROM_SUBSYSTEM2	EQU	6F9h
PORT_ROM_SUBSYSTEM3	EQU	6FAh
PORT_ROM_SUBSYSTEM4	EQU	6FBh
PORT_ROM_SUBSYSTEM5	EQU	6FCh
PORT_ROM_SUBSYSTEM6	EQU	6FDh
PORT_ROM_SUBSYSTEM7	EQU	6FEh
PORT_ROM_SUBSYSTEM8	EQU	6FFh

; ---------------------------------------------------------------------------
; Mystery ports!
; No idea what these do yet.
PORT_UNKNOWN_426	equ	0426h	; set to 1 then 0 in POST
PORT_UNKNOWN_FFF	equ	0FFFh	; set to 1 in POST
PORT_UNKNOWN_7FD	equ	07FDh	; set to 0 in POST

