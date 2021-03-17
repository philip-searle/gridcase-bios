
; ===========================================================================
; Int19_Actual
; Bootloader, called after POST (and in theory by any application software
; that wants to reload the operating system, although under DOS deficiancies
; in many TSRs and system software mean that it is not usable once DOS has
; loaded -- see "DOS Internals" by Geoff Chappell for an in-depth discussion
; on this problem).
; ===========================================================================
Int19_Actual	PROC
.BOOT_DRIVE_UNKNOWN	equ	0FFh
.BOOT_DRIVE_HD1		equ	080h

.BOOT_KEY_NONE		equ	0FFh
.BOOT_KEY_FLOPPY	equ	'f'
.BOOT_KEY_HARDDISK	equ	'h'
.BOOT_KEY_EXTRA_FLOPPY	equ	'e'

; ---------------------------------------------------------------------------
; Prepare for disk operations: setup diskette parameter table and reset disks
; ---------------------------------------------------------------------------
		; Need interrupts enabled for disk I/O to work
		sti

		; Int13 requires valid diskette parameter table
		xor_	ax, ax
		mov	ds, ax
		mov	[IvtDisketteTable], DisketteParams,DATA=WORD
		mov	[IvtDisketteTable+2], cs

		; Reset disk system
		xor_	ah, ah
		mov_	dl, ah
		int	13h

		; Setup registers for bootloader loop:
		; DL is used for the boot drive ID
		mov	ds, [cs:kBdaSegment]
		mov	dl, .BOOT_DRIVE_UNKNOWN

; ---------------------------------------------------------------------------
; Main bootloader loop
; ---------------------------------------------------------------------------
.bootloaderLoop	; Auto-detect boot floppy disk drive ID and allow the user to
		; press a key to override the boot order
		call	GridBootFdIds
		call	GridWaitKey

		; If key pressed then determine which drive to boot from.
		jnb	.keyPressed
		; If drive ID known then autoboot has already run and we
		; know which drive to boot from so do not rerun autoboot.
		cmp	dl, .BOOT_DRIVE_UNKNOWN
		jnz	.manualBoot
		; Otherwise reset any keypress and attempt boot drive autodetect
		mov	al, .BOOT_KEY_NONE
		jmp	.autoBoot

; ---------------------------------------------------------------------------
; User pressed a key: determine which one and try appropriate boot method.
; Set DL to selected drive ID and SI to point to descriptive string.
; ---------------------------------------------------------------------------
.keyPressed	; Check for hard disk boot request
		mov	dl, .BOOT_DRIVE_HD1
		mov	si, Int19_kHard
		; Lower-case any key pressed by the user
		cmp	al, 'A'
		jb	.lowerCased
		cmp	al, 'Z'
		ja	.lowerCased
		or	al, 20h
.lowerCased	cmp	al, .BOOT_KEY_HARDDISK
		jz	.manualBoot

		; Check for floppy disk boot request
		mov	si, Int19_kFloppy
		mov_	dl, bh
		cmp	al, .BOOT_KEY_FLOPPY
		jz	.manualBoot

		; Check for external floppy boot request
		mov	si, Int19_kExtraFloppy
		mov_	dl, bl
		cmp	al, .BOOT_KEY_EXTRA_FLOPPY
		jz	.manualBoot

		; No valid key pressed, try all drives
		jmp	.autoBoot

; ---------------------------------------------------------------------------
; Attempt boot from selected drive
; ---------------------------------------------------------------------------
.manualBoot	push	dx
		push	si
		call	LoadBootSector
		; If we returned from LoadBootSector then the boot failed
		pop	si
		push	si
		call	WriteString
		mov	si, Int19_kDiskBootError
		call	WriteString
		pop	si
		pop	dx

.restartLoop	; Loop restart jmp reused as near jmp thunk from code below
		jmps	.bootloaderLoop

; ---------------------------------------------------------------------------
; Attempt to boot from all drives in sequence to determine first one present
; ---------------------------------------------------------------------------
.autoBoot	; Attempt boot from GRiD ROM subsystem
		push	bx
		call	GridBootRom
		pop	bx
		mov	ah, 1
		int	16h		; Check keypress present
		jnz	.restartLoop

		; Attempt boot from first hard drive
		mov	dl, .BOOT_DRIVE_HD1
		call	LoadBootSector
		mov	ah, 1
		int	16h		; Check keypress present
		jnz	.restartLoop

		; Attempt boot from internal floppy
		mov_	dl, bh
		call	LoadBootSector
		mov	ah, 1
		int	16h		; Check keypress present
		jnz	.restartLoop

		; Attempt boot from external floppy
		push	cx
		mov	cx, 12000	; Short delay (why?)
.extFloppyDelay	call	ShortDelay
		loop	.extFloppyDelay
		pop	cx
		mov_	dl, bl
		call	LoadBootSector
		mov	ah, 1
		int	16h		; Check keypress present
		jnz	.restartLoop

		; All boot methods failed, report error
		mov	si, Int19_kDiskBootError
		call	WriteString

		; Delay before retrying bootloader loop
		push	cx
		mov	cx, 12000
.allFailedDelay	call	ShortDelay
		loop	.allFailedDelay
		pop	cx
		jmp	.autoBoot

		ENDPROC	Int19_Actual

; ===========================================================================
; LoadBootSector
; Attempts to load the boot sector from a disk drive to 0000:7C00.
; If successful, jumps to the loaded code and does not return.
; 
; On entry:
;   DL == drive ID
; 
; On return:
;   Load failed or sector was not considered to be executable.
; ===========================================================================
LoadBootSector	PROC
.LOAD_RETRIES	equ	5

; Attempt to load the boot sector, retrying multiple times.  Need to reset
; the disk system between each try to maximise changes of success.
		mov	cx, 5
.loadSector	push	bx
		push	cx
		sti
		xor_	ah, ah
		int	13h		; reset disk system

		les	bx, [cs:kBootSegOffset]
		xor_	dh, dh		; head 0
		mov	cx, 0001h	; track 0, sector 1
		mov	ax, 0201h	; read 1 sector
		int	13h		; read to es:bx
		pop	cx
		pop	bx
		jb	.sectorLoadFail

		test	dl, 80h		; Bit 80h set indicates HD1
		jnz	.hardDrive

; Ensure floppy disk first sector is executable.
; First word must be >= xx06h and the next eight words must contain at least
; one word with a different value to the first.
; 
; This accounts for all DOS boot sectors, which start with a short JMP:
;  * DOS 1.0 did not have a BIOS Parameter Block but jumps over the first
;    31h bytes of the boot sector (EB 1F).
;  * DOS 2+ which jump over the BPB (EB 14+ depending on DOS version).
; All of these boot sectors have constant data in the following eight words
; which differs from the first two bytes.
; 
; Non-executable boot sectors will likely be encountered on unformatted media
; which will have sectors initialised to a constant value: not necessarily
; zero (values such as 4E are common fill bytes) but the check for the next
; eight words catches them.
; 
; Note that we do NOT check for the last two bytes being the 55AAh marker
; as many sources say we should -- it is not present in DOS 1.0 boot sectors.
		SEGES mov	ax, [7C00h]
		cmp	al, 6
		jb	.leaveFunction
		push	cx
		push	di
		mov	di, 7C02h
		mov	cx, 8
		repe	scasw
		pop	di
		pop	cx
		jz	.leaveFunction
		jmp	.jmpBootSector
		nop			; assembler-inserted nop

; Ensure hard disk first sector is bootable.  Since hard disk boot sectors
; were never standardised in the same way that floppy disk boot sectors were,
; we perform minimal validation: just a check for the trailing AA55h marker.
.hardDrive	SEGES cmp	[7DFEh], 0AA55h,DATA=WORD
		jnz	.leaveFunction

; Prepare the environment for the boot sector and jump to it.
.jmpBootSector	push	ax
		mov	ax, KbBuffer	; clear BDA keyboard buffer
		mov	[KbNextChar], ax
		mov	[KbLastChar], ax
		mov	[KbBufStart], ax
		add	ax, KB_BUFFER_LENGTH
		mov	[KbBufEnd], ax
		pop	ax

		add	sp, 6		; known stack depth?

		push	es
		pop	ds		; boot sector expects valid DS

		; Jump to boot sector.  We do not return from this.
		jmpf	[cs:kBootSegOffset]

; Check whether failure was a bad command (fatal) and retry if not.
.sectorLoadFail	test	ah, 1
		jnz	.leaveFunction
		loop	.loadSector

.leaveFunction	retn
		ENDPROC	LoadBootSector

; ===========================================================================
; Int19 error message constants
; ===========================================================================
Int19_kHard		db	'Hard',0
Int19_kFloppy		db	'Floppy',0
Int19_kExtraFloppy	db	'Extra Floppy',0
Int19_kDiskBootError	db	' Disk Boot Error',0Dh,0Ah,0

; ===========================================================================
; GridWaitKey
; Waits for a short delay.  If keyboard buffer contains any keypresses on
; entry or keys are pressed during the delay, then the most recent keypress
; is returned.
; 
; On return:
;   AX = last key code pressed
;   CF set if no keys pressed
; ===========================================================================
GridWaitKey	PROC
		mov	cx, 5000h	; TODO: how long delay is this?
.delayLoop	push	cx
		mov	ah, 1
		int	16h		; check keyboard
		pop	cx
		jnz	.drainKbBuf	; drain if keypress present
		loop	.delayLoop

		stc			; CF set if no keys pressed
		retn

.drainKbBuf	xor_	ah, ah
		int	16h		; read keyboard
		push	ax		; store scancode
		mov	ah, 1
		int	16h		; check keyboard
		pop	ax		; restore scancode
		jnz	.drainKbBuf
		retn
		ENDPROC	GridWaitKey

; ===========================================================================
; GridBootFdIds
; Detects appropriate floppy drive IDs for booting from (00 to 02).
; 
; On return:
;   BH = primary floppy boot drive ID
;   BL = extra floppy boot drive ID
; ===========================================================================
GridBootFdIds	PROC
.ExternalDriveMask	equ	PRINTER_STATC_EFLOPPY | PRINTER_STATC_TAPE

; Default to booting from drives 00/01
		push	dx
		mov	bx, 0001h

; Check whether either an external floppy/tape are present.  If so, then
; use the default boot order.
		mov	dx, PORT_PRINTER_STATUSC
		in	al, dx		; get printer+grid floppy status
		and	al, .ExternalDriveMask
		cmp	al, .ExternalDriveMask
		jnz	.leaveFunction

; Fetch drive sled type from ROM subsystem
		mov	dx, PORT_ROM_SUBSYSTEM1
		in	al, dx
		and	al, GRID_SLED_MASK

; Drive sleds with zero/one floppy drive use the default boot order
		cmp	al, GRID_SLED_NONE
		jz	.leaveFunction
		cmp	al, GRID_SLED_40
		jz	.leaveFunction
		cmp	al, GRID_SLED_50
		jz	.leaveFunction
		cmp	al, GRID_SLED_70
		jz	.leaveFunction
		cmp	al, GRID_SLED_80
		jz	.leaveFunction
; Drive sleds with two floppy drives need the external drive's ID adjusting upwards
		cmp	al, GRID_SLED_20
		jnz	.checkExtSwap
		mov	bx, 0002h
; If external drive is set to be A: then swap the floppy/extra floppy boot IDs
.checkExtSwap	mov	dx, PORT_PRINTER_STATUSB
		in	al, dx		; get printer+grid floppy status
		test	al, PRINTER_STATB_BORA
		jnz	.leaveFunction
		mov	bx, 0100h

.leaveFunction	pop	dx
		retn
		ENDPROC	GridBootFdIds

