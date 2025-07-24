
GRID_DRIVEID	PROGRAM	OutFile=grid_driveid.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"bios-version.inc"
		include	"grid.inc"
		include	"hdc_at.inc"
		include	"hdc_xt.inc"

		EXTERN	HdAtIdentify

		PUBLIC	DriveIdentify

; Supported model numbers for IDE IDENTIFY response
IdCp3022	db	'CP3022'
		%IF	BIOS_VERSION > 19880912
IdCp3024	db	'CP3024'
IdCp3044	db	'CP3044'
IdCp3042	db	'CP3042'
		%ENDIF
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

		%IF	BIOS_VERSION > 19880912
			; 1988 BIOS does not support CP3024 drive model
			mov	dx, GRID_HD_5
			mov_	di, bx
			TestDriveModel	IdCp3024
		%ENDIF

		mov_	di, bx
		mov	dx, GRID_HD_6
		TestDriveModel	IdCp344

		%IF	BIOS_VERSION > 19880912
			; 1988 BIOS does not support CP3044/CP3042 drive models
			mov	dx, GRID_HD_9
			mov_	di, bx
			TestDriveModel	IdCp3044

			mov_	di, bx
			TestDriveModel	IdCp3042
		%ENDIF

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

ENDPROGRAM	GRID_DRIVEID
