
GRID_MISC	PROGRAM	OutFile=build/grid_misc.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"grid.inc"
		include	"hdc_at.inc"
		include	"hdc_xt.inc"
		include	"parallel.inc"

		EXTERN	HdAtIdentify

		PUBLIC	DriveIdentify, DetectMemC, HdWaitSpinUp

; Supported model numbers for IDE IDENTIFY response
IdCp3022	db	'CP3022'
IdCp3024	db	'CP3024'
IdCp3044	db	'CP3044'
IdCp3042	db	'CP3042'
IdCp344		db	'CP344'
IdCp3104	db	'CP3104'

TestDriveType	%macro	GridDriveType, XtConfigReg
		mov	dx, %GridDriveType
		cmp	al, %XtConfigReg
		jz	.driveTypeKnown
		%endmacro

TestDriveModel	%macro	ModelNumber
		mov	si, %ModelNumber
		mov	cx, SIZE#%ModelNumber
		repe cmpsb
		jz	.detectAtDone
		%endmacro

; =====================================================================
; DriveIdentify
; Tries to identify XT ('incompatible') and AT ('compatible') hard
; drives by checking drive backplane ID and IDE IDENTIFY values.
;
; On return:
;   AX == drive type (GRID_HD_* constant)
; =====================================================================
DriveIdentify	PROC
		; Ready drive backplane type and distinguish
		push	dx
		mov	dx, PORT_ROM_SUBSYSTEM1
		in	al, dx
		and	al, GRID_BKPL_MASK

		cmp	al, GRID_BKPL_30
		jnz	.notXtDrive

		; Type 30 backplane: XT-compatible drive.
		; Must interrogate drive controller configuration register.
		mov	dx, PORT_HD_XT_CONFIG
		in	al, dx
		TestDriveType	GRID_HD_3, HD_XT_DRIVE_TYPE_FE
		TestDriveType	GRID_HD_2, HD_XT_DRIVE_TYPE_F6
		TestDriveType	GRID_HD_1, HD_XT_DRIVE_TYPE_FF

		; Unknown XT-compatible drive
		mov	dx, GRID_HD_UNKNOWN
		jmp	.driveTypeKnown

.notXtDrive	; AT-compatible drive: check backplane type to
		; determine if it's a known model.
		TestDriveType	GRID_HD_4, GRID_BKPL_50

		; Not a fixed AT-compatible type, is a backplane
		; that supports drives with the IDENTIFY command?
		cmp	al, GRID_BKPL_90
		jz	.autoDetectAt
		cmp	al, GRID_BKPL_70
		jz	.autoDetectAt

		; Unknown AT-compatible drive
		mov	dx, GRID_HD_UNKNOWN
		; Fallthrough to .driveTypeKnown

.driveTypeKnown	mov_	ax, dx
		pop	dx
		retn

.autoDetectAt	; Use the IDE IDENTIFY DRIVE command to get the drive
		; model number and match it to a list of known drives.
		push	bx
		push	cx
		push	ds
		push	si
		push	es
		push	di

		mov	dx, GRID_HD_UNKNOWN
		call	HdAtIdentify
		jb	.detectAtDone

		; Skip the first part of the drive's model number
		; (everything before the first '-').  This is very
		; specific to the supported Conner drive models.
		; BUG: we check up to 40 bytes but HdAtIdentify only
		; copies out 34???
		mov	cx, SIZE#IdentifyDrive.modelNumber
.skipMdlPrefix	cmp	[es:di], '-',DATA=BYTE
		jz	.foundHyphen
		inc	di
		loop	.skipMdlPrefix
		jmp	.detectAtDone	; no hyphen?  give up if so
.foundHyphen	inc	di

		; Skip any spaces after the hyphen
		; ??? which drive models require this?
.skipSpaces	cmp	[es:di], ' ',DATA=BYTE
		jnz	.skippedSpaces
		inc	di
		loop	.skipSpaces
		jmp	.detectAtDone	; all spaces?  give up if so

.skippedSpaces	; Compare the model number substring we're pointing to
		; to with our list of supported drive models.
		mov_	bx, di		; BX holds our starting point

		mov	dx, GRID_HD_5
		mov	ax, cs
		mov	ds, ax		; setup for cmpsb against ROM
		TestDriveModel	IdCp3022

		mov	dx, GRID_HD_5
		mov_	di, bx
		TestDriveModel	IdCp3024

		mov_	di, bx
		mov	dx, GRID_HD_6
		TestDriveModel	IdCp344

		mov	dx, GRID_HD_9
		mov_	di, bx
		TestDriveModel	IdCp3044

		mov_	di, bx
		TestDriveModel	IdCp3042

		mov_	di, bx
		mov	dx, GRID_HD_7
		TestDriveModel	IdCp3104

		; No drive model match
		mov	dx, GRID_HD_UNKNOWN
		; fallthrough to .detectAtDone

.detectAtDone	; Drive type is in DX by now
		pop	di
		pop	es
		pop	si
		pop	ds
		pop	cx
		pop	bx
		jmp	.driveTypeKnown
		ENDPROC	DriveIdentify

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
