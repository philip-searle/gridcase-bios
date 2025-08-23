; Included at two different locations,
; depending on BIOS version.

; =====================================================================
; GridBootRom
; Checks all available application ROMs and returns if none of them
; are bootable.  Loads ROM boot sector and jumps to it otherwise.
; =====================================================================
GridBootRom	PROC
		cli
		mov	ax, 2000h	; use segment 2000h as ROM subsystem scratch space
		mov	ds, ax
		mov	ax, 0E401h	; initialise ROM subsystem
		int	15h
		jnc	.romInitOk

.romInitDone	jmp	.leaveFunction

.romInitOk	cmp	al, 0		; ROM images stil to process?
		jz	.romInitDone

		mov_	cl, al		; CX is our loop counter
		xor_	ch, ch
.checkRomImage	; Get info for current ROM image (ES=ROM image base, DH=ROM system type)
		push	cx
		mov	ax, 0E402h	; get ROM image info
		mov_	dl, cl
		dec	dl		; convert loop index into ROM image ordinal
		int	15h
		pop	cx
		jc	.romInitDone	; bail our on error

		; Validate ROM image is bootable
		push	cx
		cmp	dh, 1		; skip ROM if it's not MS-DOS format
		jnz	.romCheckDone
		mov	ax, es
		add	ax, 1FF0h	; repoint ES to ROM image header
		mov	es, ax
		mov	ax, 0E403h	; enable ROM image
		int	15h
		mov	di, 0
		cmp	[es:di], 0BB66h,DATA=WORD	; check for ROM header signature
		jnz	.romCheckDone
		cmp	[es:di+3], 80h,DATA=BYTE	; bootable flag set?
		jnz	.romCheckDone

		; ROM image passed checks, copy the entire 128K ROM mapping section
		; into 8000:0000 - 9000:FFFF then unmap the ROM image.
		; ??? This is where the ROMs are mapped anyway -- is it just reads that
		; are mapped to the ROM sockets so we can shadow-copy them into RAM?
		push	ds
		push	es
		push	di
		mov	ax, es
		sub	ax, 1FF0h	; point DS to ROM mapping base (source)
		mov	ds, ax
		mov	ax, 8000h	; point ES to high RAM (under ROM mapping?)
		mov	es, ax
		mov	si, 0
		mov	di, 0
		mov	cx, 8000h
		rep movsw		; copy first 64K
		mov	ax, ds
		add	ax, 1000h
		mov	ds, ax
		mov	ax, es
		add	ax, 1000h
		mov	es, ax
		mov	si, 0
		mov	di, 0
		mov	cx, 8000h
		rep movsw		; copy second 64K
		pop	di
		pop	es
		pop	ds

		; Setup a far return to the boot sector code on the stack,
		; them retf to hand control to the ROM's code (implies the
		; code must be position-independent?)
		mov	ax, [es:di+4]	; load boot sector offset
		cmp	ax, 8000h	; ignore values that would wrap past 1MB
		jnb	.jmpBootsector
		add	ax, 8000h
.jmpBootsector	push	ax		; push boot sector segment
		mov	ax, 0
		push	ax		; push boot sector offset
		mov	ax, 0E404h	; unmap ROM image
		int	15h
		mov	dl, 'r'		; set drive index for boot sector code
		sti
		retf

.romCheckDone	; advance to next ROM image
		pop	cx
		loop	.nextRomImage
		push	cx
		pop	cx
.leaveFunction	sti
		retn
.nextRomImage	jmp	.checkRomImage
		ENDPROC	GridBootRom
