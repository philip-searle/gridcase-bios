
CMOS		PROGRAM	OutFile=build/cmos.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"cmos.inc"

		PUBLIC	ReadCmos, WriteCmos

; ---------------------------------------------------------------------
; ReadCmos
; On entry:
;   AL == CMOS register number to read
; On return:
;   AL == CMOS register contents
;   Interrupts disabled
ReadCmos	PROC
		push	dx
		mov	dx, PORT_CMOS_ADDRESS
		call	CmosHook
		cli
		out	dx, al
		inc	dx		; advance to CMOS data port
		Delay	2
		in	al, dx
		pop	dx
		retn
		ENDPROC	ReadCmos

; ---------------------------------------------------------------------
; WriteCmos
; On entry:
;   AH == CMOS register number to write
;   AL == data to write
; On return:
;   Interrupts disabled
WriteCmos	PROC
		push	dx
		mov	dx, PORT_CMOS_ADDRESS
		xchg	ah, al
		call	CmosHook
		cli
		out	dx, al
		xchg	ah, al
		inc	dx		; advance to CMOS data port
		Delay	2
		out	dx, al
		pop	dx
		retn
		ENDPROC	WriteCmos

; ---------------------------------------------------------------------
; Called before setting the CMOS address register.
; Possibly a hook for remapping registers?
CmosHook	PROC
		retn
		ENDPROC	CmosHook

ENDPROGRAM	CMOS
