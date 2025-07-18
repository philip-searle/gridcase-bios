
FD_STD_DPBS	PROGRAM	OutFile=fd_std_dpbs.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"segments/bda.inc"
		include	"int13.inc"

		PUBLIC	Dpb525_360K
		PUBLIC	Dpb525_12M
		PUBLIC	Dpb525_12M_Lo
		PUBLIC	Dpb35_720K
		PUBLIC	Dpb35_14M
		PUBLIC	Dpb35_14M_Lo

; ---------------------------------------------------------------------
; Standard diskette parameter blocks plus track count and media bytes.
; Actually instances of DPB_MEDIA struc, but EuroASM doesn't seem to
; support nested DS pseudo-instructions so I've had to 'unwrap' them.
; ---------------------------------------------------------------------

; 5.25" 40-track drive (360K)
Dpb525_360K	ds	DISKETTE_PARMS, \
				.specify1=0DFh, .specify2=2, \
				.motorWaitTime=25h, \
				.sectorSize=2, .lastSectorId=9, \
				.gapLength=2Ah, .dtl=0FFh, \
				.formatGapLen=50h, .formatFillByte=0F6h, \
				.headSettleTime=0Fh, .motorStartTime=8
		db	39	; valid tracks 0-39
		db	FDMS_DTR_250K

; 5.25" 80-track drive (1.2M) with 40-track media (360K)
; FDMS_DOUBLE_STEP flag added to media byte in code
Dpb525_12M_Lo	ds	DISKETTE_PARMS, \
				.specify1=0DFh, .specify2=2, \
				.motorWaitTime=25h, \
				.sectorSize=2, .lastSectorId=9, \
				.gapLength=2Ah, .dtl=0FFh, \
				.formatGapLen=50h, .formatFillByte=0F6h, \
				.headSettleTime=0Fh, .motorStartTime=8
		db	39	; valid tracks 0-39
		db	FDMS_DTR_300K

; 5.25" 80-track drive (1.2M)
Dpb525_12M	ds	DISKETTE_PARMS, \
				.specify1=0DFh, .specify2=2, \
				.motorWaitTime=25h, \
				.sectorSize=2, .lastSectorId=15, \
				.gapLength=1Bh, .dtl=0FFh, \
				.formatGapLen=54h, .formatFillByte=0F6h, \
				.headSettleTime=0Fh, .motorStartTime=8
		db	79	; valid tracks 0-79
		db	FDMS_DTR_500K

; 3.5" drive, low density (720K)
Dpb35_720K	ds	DISKETTE_PARMS, \
				.specify1=0DFh, .specify2=2, \
				.motorWaitTime=25h, \
				.sectorSize=2, .lastSectorId=9, \
				.gapLength=2Ah, .dtl=0FFh, \
				.formatGapLen=50h, .formatFillByte=0F6h, \
				.headSettleTime=0Fh, .motorStartTime=8
		db	79	; valid tracks 0-79
		db	FDMS_DTR_250K

; 3.5" drive, high density (1.44M) with low density media (720K)
Dpb35_14M_Lo	ds	DISKETTE_PARMS, \
				.specify1=0EFh, .specify2=2, \
				.motorWaitTime=25h, \
				.sectorSize=2, .lastSectorId=9, \
				.gapLength=2Ah, .dtl=0FFh, \
				.formatGapLen=50h, .formatFillByte=0F6h, \
				.headSettleTime=0Fh, .motorStartTime=8
		db	79	; valid tracks 0-79
		db	FDMS_DTR_250K

; 3.5" drive, high density (1.44M)
Dpb35_14M	ds	DISKETTE_PARMS, \
				.specify1=0DFh, .specify2=2, \
				.motorWaitTime=25h, \
				.sectorSize=2, .lastSectorId=18, \
				.gapLength=1Bh, .dtl=0FFh, \
				.formatGapLen=6Ch, .formatFillByte=0F6h, \
				.headSettleTime=0Fh, .motorStartTime=8
		db	79	; valid tracks 0-79
		db	FDMS_DTR_500K

ENDPROGRAM	FD_STD_DPBS
