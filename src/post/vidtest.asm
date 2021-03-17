
; =====================================================================
; Procedures in this file are called during POST to test video adapter
; functionality.  They cannot use any timers, IVT entries, or keyboard
; resources as those system functions have not yet been initialized.
; The Bios Data Area is available for use and memory NMI handler is
; configured.
; =====================================================================

; =====================================================================
; VidTestBeep
; Produces 4-1 beep pattern before returning.
; =====================================================================
VidTestBeep	PROC
		mov	cx, 4
.beep1		call	Beep
		loop	.beep1
		loop	$		; rest between beep groups
		call	Beep
		mov	ch, 0FFh	; slightly shorter rest?
		loop	$
		retn
		ENDPROC	VidTestBeep

; =====================================================================
; VidTestRefresh
; Checks that the configured video adapter is toggling the refresh in
; progress bit in its status register.  Calls VidTestBeep on failure.
;
; On return:
; 	CF set on failure
; =====================================================================
VidTestRefresh	PROC
		mov	dx, [VidBasePort]
		add	dx, VID_STATUS_PORT_OFFSET

		; Read status port and isolate refresh bit
		in	al, dx
		and	al, 1
		mov_	ah, al		; store for comparison

		xor_	cx, cx		; loop as long as possible
.waitRefresh	in	al, dx
		and	al, 1
		xor_	al, ah		; compare to initial read
		loope	.waitRefresh
		jz	VidTestFail	; tail call on failure

		retn			; return on success
		ENDPROC	VidTestRefresh

; =====================================================================
; VidTestFail
; Shared function tail for failure cases
; =====================================================================
VidTestFail	PROC
		call	VidTestBeep
		stc
		; fall-through to VidTestDone to return
		ENDPROC	VidTestFail

; =====================================================================
; VidTestDone
; Shared function tail for leaving video test procs.
; Also fallthrough after handling failure case.
; =====================================================================
VidTestDone	PROC
		retn
		ENDPROC	VidTestDone

; =====================================================================
; VidTestMem
; TODO: document this
; =====================================================================
VidTestMem	PROC
; ---------------------------------------------------------------------
; Test display controller memory
		mov	al, CHECKPOINT_VIDMEM
		out	PORT_DIAGNOSTICS, al

		; Setup assuming an MDA adapter:
		;   DX == mode control port
		;   ES == refresh buffer segment
		;   BP == refresh buffer length
		mov	dx, PORT_MDA_MODE_CTRL
		mov	bp, MDA_REGEN_SEG
		mov	es, bp
		mov	bp, MDA_REGEN_LEN

		mov	al, [EquipmentWord]
		and	al, 30h		; isolate display type bits
		cmp	al, 30h		; monochrome 80x25?
		jz	.adapterKnown
		; If not MDA, adjust setup
		mov	dl, PORT_CGA_MODE_CTRL & 0FFh
		mov	bp, CGA_REGEN_SEG
		mov	es, bp
		mov	bp, CGA_REGEN_LEN

.adapterKnown	mov	al, 1		; MDA/CGA mode control: disable video
		out	dx, al
		call	TestMemData
		jb	VidTestFail
		call	TestMemLoAddr
		jb	VidTestFail
		call	ResetNmiChecks
		jnz	VidTestFail

; ---------------------------------------------------------------------
; Set a valid video mode and check refresh signal is toggling correctly
		mov	al, CHECKPOINT_VIDEO_INIT
		out	PORT_DIAGNOSTICS, al

		; Call int10/00 to get the regen buffer cleared.
		; Use shift/mask/sbb to convert stored equipment word
		; bits into a mode value:
		;   00:EGA/VGA -> 2 (80x25)    01:CGA40 -> 1 (40x25)
		;   10:CGA80   -> 3 (80x25)    11:MDA   -> 2 (80x25)
		; TODO: 2 is not an MDA-supported mode, is it ignored?
		mov	al, [EquipmentWord]
		and	ax, 30h		; isolate display type bits
		shr	al, 5		; move into bit0/carry
		sbb	al, 0FEh	; convert to video mode
		int	10h		; set video mode

		mov	al, CHECKPOINT_VIDREFRESH
		out	PORT_DIAGNOSTICS, al
		call	VidTestRefresh
		jnb	.refreshOk
		; If refresh not OK, then VidTestRefresh has already
		; called VidTestBeep and set the carry flag, so just
		; return without any further action.
		jmp	VidTestDone

.refreshOk	; Output checkpoint code showing successful test of
		; default video mode:
		;   30 -> CHECKPOINT_VIDOK_EGA
		;   31 -> CHECKPOINT_VIDOK_CGA40
		;   32 -> CHECKPOINT_VIDOK_CGA80
		;   33 -> CHECKPOINT_VIDOK_MDA
		mov	al, [EquipmentWord]
		shr	al, 4		; pull default video mode bits to bottom
		and	al, 3		; clear other bits
		or	al, 30h		; convert to checkpoint code
		out	PORT_DIAGNOSTICS, al
		retn
		ENDPROC	VidTestMem

