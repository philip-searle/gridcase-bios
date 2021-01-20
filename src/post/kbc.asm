
; =====================================================================
; Procedures in this file are called during POST while testing the
; keyboard controller.  They cannot use any timers, or IVT entries as
; those system functions have not yet been initialized.
; =====================================================================

; Number of loops to make before deciding KBC has no more data
KBWAIT_DELAY	equ	5000

; =====================================================================
; KbWaitEmpty
; Reads and discard data from the keyboard controller until no more
; data is returned.
; =====================================================================
KbWaitEmpty2	PROC	; Internal entrypoint for KbWaitEmpty
		in	al, PORT_KBC_DATA	; read and discard byte
		; fall-through to KbWaitEmpty
		ENDPROC	KbWaitEmpty2

KbWaitEmpty	PROC
		mov	cx, KBWAIT_DELAY
.checkKbc	in	al, PORT_KBC_STATUS	; check KBC status
		test	al, KBC_STATUS_OBF	; data waiting?
		loope	.checkKbc		; keep checking if not
		jnz	KbWaitEmpty2		; read data and go again if necessary
		retn
		ENDPROC	KbWaitEmpty

; =====================================================================
; KbWaitResponse
; Waits for the keyboard controller to have data for reading.
; 
; On entry:
;   DL == timeout (will test KBC DL*256 times waiting for data)
;   DH == repeat count for timeout
; 
; On return:
;   AL == KBC status register
;   ZF set if no response from KBC within timeout
; =====================================================================
KbWaitResponse	PROC
		mov	ch, dl,CODE=LONG; the first pass through the timeout
					; loop may be more than DL*256 as the
					; lower half of CX is not explicitly set
					; not really a problem...

.checkKbc	in	al, PORT_KBC_STATUS	; check KBC status
		test	al, KBC_STATUS_OBF	; data waiting?
		loope	.checkKbc		; keep checking if not
		jnz	.leaveProc		; return if so
		dec	dh
		jnz	.checkKbc		; repeat timeout loop

.leaveProc	retn
		ENDPROC	KbWaitResponse

; =====================================================================
; KbWaitReady
; Waits for the keyboard controller to be ready to accept commands.
; Returns with ZF set on success.
; =====================================================================
KbWaitReady	PROC
		mov	cx, 0		; maximum loops waiting for KBC
.checkKbc	Delay	1
		in	al, PORT_KBC_STATUS	; check KBC status
		test	al, KBC_STATUS_IBF	; ready for input?
		loopne	.checkKbc		; keep checking if not
		retn
		ENDPROC	KbWaitReady

