; Included at two different locations,
; depending on BIOS version.

; =====================================================================
; IsExtFdIndex
; On entry:
;   AL == drive index
; On return:
;   ZF set if drive index represents the external floppy drive
; =====================================================================
IsExtFdIndex	PROC
		push	ax
		push	dx
		mov_	ah, al
		mov	dx, PORT_PRINTER_STATUSC
		in	al, dx
		and	al, PRINTER_STATC_EFLOPPY | PRINTER_STATC_TAPE
		cmp	al, PRINTER_STATC_EFLOPPY	; Ext floppy present?
		jnz	.leaveFunction

		mov	dx, PORT_PRINTER_STATUSB
		in	al, dx
		and	al, PRINTER_STATB_BORA		; 0=A, 1=B
		mov_	dl, al				; Ext drive index in DL
		jz	.knowExtIndex
		mov	dx, PORT_ROM_SUBSYSTEM1
		in	al, dx
		and	al, GRID_BKPL_MASK
		jz	.knowExtIndex		; no internal drives
		cmp	al, GRID_BKPL_20
		mov	dl, 2
		jz	.knowExtIndex		; Backplane 2 has two floppies
		dec	dl
		cmp	al, GRID_BKPL_40	; One floppy for backplanes 1, 3, 9
		jb	.knowExtIndex
		cmp	al, GRID_BKPL_90
		jz	.knowExtIndex
		dec	dl			; No floppies for backplanes 4-8

.knowExtIndex	cmp_	ah, dl
.leaveFunction	pop	dx
		pop	ax
		retn
		ENDPROC	IsExtFdIndex
