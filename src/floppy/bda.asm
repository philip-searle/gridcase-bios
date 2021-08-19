
; =====================================================================
; Functions that access and update the floppy disk variables in the
; BIOS Data Area.  Almost all these functions expect BP to point to an
; initialised IsrStackAx instance and operate implicitly on the drive
; index contained in [bp+IsrStackAx.dl].
; =====================================================================

; ---------------------------------------------------------------------
; Returns the drive info nibble in the low half of AL.
FdGetDriveInfo	PROC
		mov	al, [FdDriveInfo]
		cmp	[bp+IsrStackAx.dl], 2
		jnz	.notFd2

		; Thrird floppy drive info isn't stored alongside the
		; standard two in the BDA
		mov	al, [Fd2DriveInfo]
		jmp	.gotDriveInfo
		FillerNop

.notFd2		; Move FD1 drive info nibble down if needed
		cmp	[bp+IsrStackAx.dl], 0
		jz	.gotDriveInfo
		mov	cl, 4
		shr	al, cl

.gotDriveInfo	; Mask off unused bits
		and	al, 0Fh
		retn
		ENDPROC	FdGetDriveInfo

; ---------------------------------------------------------------------
; Stores the drive info nibble in the low half of AL into the BDA for
; the drive index stored in BL.
FdSetDriveInfo	PROC
		mov	cl, 0F0h	; mask for later and-or

		; Third floppy drive info is stored as-is elsewhere
		cmp	bl, 2
		jnz	.notFd2
		mov	[Fd2DriveInfo], al
		retn

.notFd2		or_	bl, bl		; FD0?
		jz	.mergeDriveInfo

		; FD1, shift to correct nibble and adjust mask
		mov	cl, 4
		shl	al, cl
		mov	cl, 0Fh

.mergeDriveInfo	and	[FdDriveInfo], cl	; clear existing bits
		or	[FdDriveInfo], al	; set new bits
		retn
		ENDPROC	FdSetDriveInfo

; ---------------------------------------------------------------------
; Attempts to select the next possible media type for the current drive.
; Returns with CF set if the media type changes.
FdTryNextMediaType	PROC
		; Get drive type, if it's 3.5" low-density we don't need
		; to do anything as it only supports one media type
		mov	bl, [bp+IsrStackAx.dl]
		xor_	bh, bh
		call	FdGetDriveType
		cmp	al, FD_3_720K
		jz	.leaveFunction

		; If media type has already been determined we don't
		; need to do anything
		test	[FdDriveInfo], FDDI_DRIVES_OK
		jz	.leaveFunction

		; If disk not changed, nothing to do
		mov	dx, PORT_FDC_DIGIN
		in	al, dx
		or_	al, al		; evaluate flags
		jns	.leaveFunction	; check change line bit

		; Advance media type and reset 'known media' flag
		call	FdGetMediaState
		and	al, ~FDMS_KNOWN_MEDIA
		inc	al		; advance last access
		test	al, 4		; reached the 'known' range?
		jz	.L1
		sub	al, 3		; reset back to 'tried' range
.L1		dec	al		; reduce back down

		; Notify hardware about new media type
		call	FdSetMediaState
		call	FdResetSelect
		call	FdDiskChange
		stc

.leaveFunction	retn
		ENDPROC	FdTryNextMediaType

; ---------------------------------------------------------------------
; ??? helper for FdPerformOp
; Something to do with detecting the media type in the drive?
sub_FACA1	PROC
		call	FdGetMediaState2
		mov_	cl, ah
		and	ah, 0F0h
		test	ah, FDMS_KNOWN_MEDIA
		jz	.unknownMedia
		retn

.unknownMedia	; Error encountered already?
		mov	al, [FdScratch.returnAh]
		or_	al, al
		jnz	.errorAlready

		mov	cl, 87h		; ???
		mov	ah, 80h
		push	ax
		mov_	al, cl
		call	FdSetOpStartState
		pop	ax
		jmp	.setMedia

.errorAlready	; Certain error codes can be caused by using the wrong
		; media type, so detect them and retry media detection
		and	al, 1Fh
		cmp	al, INT13STAT_BADADDRMARK
		jz	.chooseMedia
		cmp	al, INT13STAT_NOSECTOR
		jz	.chooseMedia
		cmp	al, INT13STAT_BADECC
		jz	.chooseMedia

.leaveFailure	mov	al, 0
		push	cx
		mov_	cx, ax
		call	FdGetOpStartState
		xchg	al, cl
		call	FdSetOpStartState
		xchg	al, cl
		pop	cx
		call	FdSetMediaState
		stc
		retn

.chooseMedia	; Choose a new media type based on the last one used:
		; 00h -> 80h -> 60h -> 00h
		or_	ah, ah		; evaluate flags
		mov	ah, FDMS_DTR_250K
		jz	.chosenMedia
		mov	ah, FDMS_DTR_300K | FDMS_DOUBLE_STEP
		js	.chosenMedia
		mov	ah, FDMS_DTR_500K

.chosenMedia	; Compare the new media type in AH to the operational
		; starting state to see whether we've tried all media
		; types and returned to our starting state
		call	FdGetOpStartState
		xor_	al, ah
		and	al, FDMS_DATA_TRANSFER_RATE
		jnz	.setMedia

		; If we've tried all media types then return an error.
		; Comparison against BH is an odd way to test for zero...
		cmp	[FdScratch.returnAh], bh
		jnz	.haveErrorCode
		mov	[FdScratch.returnAh], INT13STAT_BADADDRMARK
.haveErrorCode	jmp	.leaveFailure

.setMedia	; TODO: what is all this?
		call	FdSetMediaState2
		call	FdGetDriveInfo
		mov_	cl, al
		and	al, FDDI_DETERMINED | FDDI_MULTIRATE
		test	ah, FDMS_DATA_TRANSFER_RATE
		jns	.L6
		cmp	al, 2
		jz	.chooseMedia
		jmp	.L8
.L6		jz	.L7
		cmp	al, 4
		jz	.chooseMedia
.L7		cmp	al, 0
		jz	.chooseMedia
.L8		and	cl, 1
		test	ah, 20h
		jz	.L9
		dec	cl
.L9		cmp	[bp+IsrStackAx.ch], 40
		adc	cl, 0FFh
		jns	.leaveSuccess
		jmp	.chooseMedia

.leaveSuccess	and	[FdScratch.returnAh], 0
		call	FdSetMediaState2
		retn
		ENDPROC	sub_FACA1

; ---------------------------------------------------------------------
; Synchronizes the data transfer rate, head load time, and head settle
; time from the BDA to the FDC.
FdSyncDtr	PROC
		; Have we already synced it?  Nothing to do if so.
		call	FdGetMediaState
		and	al, FDMS_DATA_TRANSFER_RATE
		cmp	[FdConfig], al
		jz	.dtrSynced

		mov	[FdConfig], al		; mark as synced
		rol	al, 1			; move data rate to low bits
		rol	al, 1
		mov	dx, PORT_FDC_DISK_CONTROL
		cmp	al, 0			; 500kb/sec?
		jz	.writeDtr

		; 
		push	ax
		push	dx
		call	FdGetDriveType
		cmp	al, FD_5_1M		; 1.2MB 5.25" drive?
		pop	dx
		pop	ax
		jnz	.writeDtr		; if so, use 500kb/sec
		xor	al, 3			; density for it

.writeDtr	out	dx, al

.dtrSynced	; Send specify command to FDC to set step rate (SRT)
		; and head unload time (HUT)
		push	ax
		push	dx
		mov_	ah, al
		push	ax
		mov	al, FDC_CMD_SPECIFY
		call	FdWriteByte
		call	FdGetDriveType
		cmp	al, FD_3_1M		; Bodge for 3.5" 1.44MB drive
		pop	ax
		mov	al, 0DFh		; standard step rate
		jnz	.writeStrHut
		test	ah, 82h			; ???
		jz	.writeStrHut
		mov	al, 0EFh		; slightly longer step rate

.writeStrHut	call	FdWriteByte
		mov	al, 2			; head load time
		call	FdWriteByte
		pop	dx
		pop	ax
		clc
		retn
		ENDPROC	FdSyncDtr

; ---------------------------------------------------------------------
; Updates drive info in BDA from drive media state byte in AH.
FdUpdateDrvInfo	PROC
		mov	bl, [bp+IsrStackAx.dl]
		mov	bh, 0

		; Media already known?  Nothing to do if so.
		push	ax
		call	FdGetMediaState
		test	al, FDMS_KNOWN_MEDIA
		pop	ax
		jnz	.leaveFunction

		; Encountered timeout?  Also nothing to do.
		cmp	bh, [FdScratch.returnAh]
		jb	.leaveFunction

		; 500kb/sec data transfer rate has nothing for us to do.
		; Otherwise, adjust the multi-rate bit and drive type
		; determined bits based on chosen DTR.
		call	FdGetMediaState2
		push	ax
		call	FdResetMediaState
		pop	ax
		call	FdGetDriveInfo
		and	ah, FDMS_DATA_TRANSFER_RATE
		jz	.leaveFunction
		jns	.L1
		and	al, 5
		jmp	.L2
.L1		and	al, 3
.L2		push	ax
		call	FdSetDriveInfo
		pop	ax

.leaveFunction	retn
		ENDPROC	FdUpdateDrvInfo

; ---------------------------------------------------------------------
; Sets the media state and operational starting state for the specified
; floppy disk drive.
; On entry:
;   BL == drive index
;   BH == new operational starting state
;   AH == top three bits of new media state?
FdResetMediaState	PROC
		; Set appropriate 'last access' bits based on data rate/double stepping flags
		add	ah, FDMS_KNOWN_MEDIA | FDMS_LA_K360M_360D
		cmp	ah, FDMS_KNOWN_MEDIA | FDMS_DTR_250K | FDMS_LA_K360M_360D
		jz	.mediaKnown
		inc	ah
		cmp	ah, FDMS_KNOWN_MEDIA | FDMS_DTR_300K | FDMS_DOUBLE_STEP | FDMS_LA_K360M_12D
		jz	.mediaKnown
		inc	ah
		cmp	ah, FDMS_KNOWN_MEDIA | FDMS_DTR_500K | FDMS_LA_K12M_12D
		jz	.mediaKnown
		or	ah, FDMS_LA_35

.mediaKnown	; Set media state and merge in extra flag if needed
		call	FdSetMediaState2
		call	FdGetDriveType2
		jc	.mediaStateSet
		cmp	al, FD_5_1M
		jbe	.mediaStateSet
		push	ax
		call	FdGetMediaState
		or	al, FDMS_LA_35
		call	FdSetMediaState
		pop	ax

.mediaStateSet	push	ax
		mov_	al, bh
		call	FdSetOpStartState
		pop	ax
		retn
		ENDPROC	FdResetMediaState

