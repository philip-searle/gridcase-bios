
GRID_MISC	PROGRAM	OutFile=grid_misc.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"hdc_at.inc"
		include	"parallel.inc"

		PUBLIC	DetectMemC, HdWaitSpinUp

; =====================================================================
; DetectMemC
; Detects the type of memory controller installed in the system.
; Sets ports FFD and FFE to zero, then checks whther address 8000:0000
; is writable and does not alias to 0000:0000.
;
; On return:
;   CF clear if 1MB memory controller
;   CF set if 256KB memory controller
;   Ports FFD and FFE set to zero
; =====================================================================
DetectMemC	PROC
		push	ax
		push	dx
		push	di
		push	ds
		push	es

		mov	dx, PORT_PAR_PORTB_W
		xor_	al, al
		out	dx, al		; set port FFD to zero
		Delay	2
		inc	dx		; advance to port FFE
		out	dx, al		; set port FFE to zero

		xor_	ax, ax
		mov	ds, ax		; DS addresses 0K-64K
		mov	ax, 8000h
		mov	es, ax		; ES addresses 512K-576K

		xor_	di, di			; check word at offset 0
		push	[di],DATA=W		; save original value from 0000:0000
		mov	[di], 0,DATA=W		; clear all bits
		push	[es:di],DATA=W		; save original value from 8000:0000
		mov	[es:di], 55AAh,DATA=W	; write alternating bits at 8000:0000
		nop				; wait a bit...
		nop

		cmp	[es:di], 55AAh,DATA=W	; check test pattern is still there
		jnz	.not1M			; maybe no RAM fitted there, can't be 1MB controller

		cmp	[di], 55AAh,DATA=W	; did it also write to 0000:0000?
		jz	.not1M			; if so, 512K aliases to 0K, assume it's a 256K controller

		; Write to 512K doesn't alias to 0K, assume it's a 1MB controller
		clc
		jmp	.leaveFunction

.not1M		stc

.leaveFunction	pop	[es:di],DATA=W	; restore original values
		pop	[di],DATA=W

		pop	es
		pop	ds
		pop	di
		pop	dx
		pop	ax
		retn
		ENDPROC	DetectMemC

; =====================================================================
; HdWaitSpinUp
; Wait for HDC to report drive ready, then delay a fair bit longer.
; Is this to ensure the drive motor isn't pulling inrush current while
; we're turning on the display?  (Plasma/EL may pull too much current
; if activated at the same time).
; =====================================================================
HdWaitSpinUp	PROC
		; wait for drive to be ready (??? no timeout!)
		mov	dx, PORT_HD_AT_STATUS
.waitDriveReady	Delay	3
		in	al, dx		; read drive status
		test	al, HD_AT_STATUS_DRDY
		jz	.waitDriveReady

		; 143FF36h (21233462) clock loop
		; @ 12MHz that == ~1.7 seconds
		mov	bx, 18
.outerLoop	; 1179637 clocks
		xor_	cx, cx		; 2 clocks (FFFFh iterations)
.innerLoop	; 18 clocks (14 on final iteration)
		nop			; 3 clocks
		or	ax, [si]	; 7 clocks
		loop	.innerLoop	; 8 clocks (4 when not taken)
		dec	bx		; 2 clocks
		jnz	.outerLoop	; 7 clocks (3 when not taken)
		retn
		ENDPROC	HdWaitSpinUp

; =====================================================================
; GRiD ROM copyright notice
		d b	'Copyright (C) 1987-88, GRiD Systems Corporation'

ENDPROGRAM	GRID_MISC
