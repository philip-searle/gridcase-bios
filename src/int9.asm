
; Useful reference for int9 handler is PC Tech Journal, Vol 5 No 7.
; https://archive.org/details/PC_Tech_Journal_vol05_n07/page/n149/mode/2up

; Scancodes that require special processing by the int9 handler.
; Mostly these are shift keys and keys that are new for the 101 key keyboard.
Int9ScanCodes	db	SC2_LCTRL
		db	SC2_LSHIFT
		db	SC2_RSHIFT
		db	SC2_ALT
		db	SC2_SYSREQ
		db	SC2_CAPLOCK
		db	SC2_NUMLOCK
		db	SC2_SCRLOCK
		db	SC2_BREAK | SC2_LCTRL
		db	SC2_BREAK | SC2_LSHIFT
		db	SC2_BREAK | SC2_RSHIFT
		db	SC2_BREAK | SC2_ALT
		db	SC2_BREAK | SC2_SYSREQ
		db	SC2_BREAK | SC2_CAPLOCK
		db	SC2_BREAK | SC2_NUMLOCK
		db	SC2_BREAK | SC2_SCRLOCK
		db	SC2_INSERT
		db	SC2_BREAK | SC2_INSERT
		db	KB_REPLY_OVERRUN
		db	KB_REPLY_RESEND
		db	KB_REPLY_ACK
		db	SC2_PREFIX_EXT
		db	SC2_PREFIX_NOBREAK
		db	0ABh			; second byte of EnhKB ID reply (follows ack)
		db	041h			; third byte of EnhKB ID reply (translated)
		db	085h			; third byte of EnhKB ID reply (untranslated)
		db	037h			; PrtScr/* key
		db	0FFh			; end of list sentinal value

; The first set of entries in Int9ScanCodes modify the shift state bytes
; in the BDA.  This table stores the mask used by the Int9 handler for those
; scan codes.  Mask can be for any shift state byte; the code knows which one.
Int9ShiftFlags	db	KBSHIFT1_CTRL
		db	KBSHIFT1_LSHIFT
		db	KBSHIFT1_RSHIFT
		db	KBSHIFT1_ALT
		db	KBSHIFT2_SYSREQ
		db	KBSHIFT2_CAPLOCK
		db	KBSHIFT2_NUMLOCK
		db	KBSHIFT2_SCRLOCK
		db	~ KBSHIFT1_CTRL
		db	~ KBSHIFT1_LSHIFT
		db	~ KBSHIFT1_RSHIFT
		db	~ KBSHIFT1_ALT
		db	~ KBSHIFT2_SYSREQ
		db	~ KBSHIFT2_CAPLOCK
		db	~ KBSHIFT2_NUMLOCK
		db	~ KBSHIFT2_SCRLOCK

; Int9 handler offsets for each entry in Int9ScanCodes
Int9Handlers	dw	Int9_Actual.makeLCtrl
		dw	Int9_Actual.makeShift
		dw	Int9_Actual.makeShift
		dw	Int9_Actual.makeAlt
		dw	Int9_Actual.makeSysReq
		dw	Int9_Actual.makeCapLk
		dw	Int9_Actual.makeNumLk
		dw	Int9_Actual.makeScrLk
		dw	Int9_Actual.breakLCtrl
		dw	Int9_Actual.breakShift
		dw	Int9_Actual.breakShift
		dw	Int9_Actual.breakAlt
		dw	Int9_Actual.breakSysReq
		dw	Int9_Actual.breakLk
		dw	Int9_Actual.breakLk
		dw	Int9_Actual.breakLk
		dw	Int9_Actual.makeIns
		dw	Int9_Actual.breakIns
		dw	Int9_Actual.kbOverrun
		dw	Int9_Actual.kbResend
		dw	Int9_Actual.kbAck
		dw	Int9_Actual.handleE0
		dw	Int9_Actual.handleE1
		dw	Int9_Actual.handleIdByte2
		dw	Int9_Actual.handleIdByte3
		dw	Int9_Actual.handleIdByte3
		dw	Int9_Actual.handlePrtScr
		dw	Int9_Actual.regularKeyCode

; ---------------------------------------------------------------------
; Int9_Actual
; Interrupt handler for keyboard input.
; This is a maze of special cases, backward compatibility, and the need
; to support both the GRiD 84-key internal keyboard and an external
; keyboard (101 or 84 key).
; ---------------------------------------------------------------------
Int9_Actual	PROC
		; Enable interrupts immediately so we don't lose any other hardware interrupts
		sti
		cld

		; Save registers
		push	bp
		push	ax
		push	bx
		push	cx
		push	dx
		push	si
		push	di
		push	ds
		push	es

		; Setup access to BDA, prevent user pressing any keys,
		; and read the current keypress
		call	LoadBdaToDs
		call	KbDisable
		in	al, PORT_KBC_DATA

		; OS Hook - keyboard interrupt
		mov	ah, 4Fh
		stc
		int	15h
		jb	.osHookPassed	; OS hook suppressed scan code?
		jmp	.leaveFunction	; skip all handling if so

; ---------------------------------------------------------------------
; Setup registers for the rest of the int9 handler
; AH == scan code
; BH == KbShiftFlags2
; BL == KbShiftFlags1
; CH == KbStatusFlags3
; CL == Shift flag adjustment from Int9ShiftFlags
.osHookPassed
		mov	bx, [KbShiftFlags1]	; load KbShiftFlags1+2
		mov_	ah, al			; scancode -> ah

		; Scan through Int9ScanCodes to find a matching scan code.
		; When done, es:di will point one past the matching scan code
		; or one past the end of the table.
		; In any case, we decrement the index by one to get the
		; Int9ShiftFlags and handler index.  This means that the
		; last entry in the table is our fallback entry used for
		; scan codes that don't need special handling.
		push	es
		push	cs
		pop	es
		mov	cx, Int9ShiftFlags - Int9ScanCodes
		mov	di, Int9ScanCodes
		repne	scasb
		pop	es
		sub	di, Int9ScanCodes + 1	; convert address to table index
						; +1 is to account for scasb
						; incrementing di for all comparisons,
						; including successful ones

		mov	cl, [cs:di+Int9ShiftFlags]
		mov	ch, [KbStatusFlags3]

		; Dispatch to handler via jump table
		shl	di, 1
		jmp	[cs:di+Int9Handlers]

; ---------------------------------------------------------------------
; Handle insert in combination with ctrl or al specially for the GRiDCase.
; If we detect either of those, divert to GRiD-specific key tests.
.makeIns	test	bl, KBSHIFT1_CTRL | KBSHIFT1_ALT
		jz	.notCtrlAltIns


.regularKeyCode	or_	al, al
		jns	.makeCode		; hi-bit clear for make code,
		jmp	.clearE0E1		; break codes don't need special handling

; ---------------------------------------------------------------------
; Pause key has special behaviour if it's being held donw
.makeCode	test	bh, KBSHIFT2_PAUSE	; pause key pressed?
		jz	.notPause
		; Pause key auto-unpresses if another key is pressed while
		; it is being held down
		and	bh, ~KBSHIFT2_PAUSE
		jmp	.clearE0E1

; ---------------------------------------------------------------------
; Holding the alt key and pressing digits on the numeric keypad allows
; the user to build up an ASCII character code which will be inserted
; into the input buffer on completion.  Pressing DEL while alt is held
; down will cancel any entered digits.
.notKeypad_jmp	jmp	.notKeypad
.notPause	; Check scancode against numeric keypad:
		; 7 8 9
		; 4 5 6
		; 1 2 3
		; 0 DEL
		; All these keys have consecutive scancodes and when
		; numlock is on they do not have the 36h prefix.
		cmp	al, SC2_KP_7
		jb	.notKeypad_jmp
		cmp	al, SC2_KP_DEL
		ja	.notKeypad_jmp

.notCtrlAltIns	; Check for ctrl+alt being held down and perform any
		; processing for BIOS key combinations.
		test	bl, KBSHIFT1_ALT
		jz	.altNotDown
		test	bl, KBSHIFT1_CTRL
		jz	.notBiosChord
		call	CheckBiosChords		; both keys pressed, check third one
		jnz	.notBiosChord		; zero flag set indicates reboot wanted
		mov	[SoftResetFlag], SOFT_RESET_FLAG
		jmpf	0f000h:Reset_Compat

; ---------------------------------------------------------------------
; Extended key codes introduced with the PC/AT keyboard have their own
; special prefix (for grey keys in the numeric keypad cluster).
.notBiosChord	test	ch, KBSTAT3_LASTE0	; extended key from AT kb layout?
		jz	.lastNotE0
		mov	di, KbXlatExtended
		sub	al, SC2_SCRLOCK		; adjust for xlat table base
		call	KbXlatScancode
		jz	.xlatFail
		mov_	ah, al			; got extended key code
		xor_	al, al			; no ASCII representation
		jmp	.processKeyCode

; ---------------------------------------------------------------------
; Alt key held down.
.lastNotE0	cmp	ah, SC2_KP_DEL		; alt+del clears ASCII code buffer
		jz	.keypadClear
		mov	di, KbXlatAlt
		call	KbXlatScancode
		; Translated key codes less than 10 are not real, they
		; represent the alt+numpad ASCII code buffer value
		cmp	al, 10
		jb	.gotKeypadDigit
		jmp	.processKeyCode

; ---------------------------------------------------------------------
; Shift ASCII digits up and add in new digit.
.gotKeypadDigit	mov_	di, ax			; save ax
		mov	al, 10
		mul	[AltNumpad]		; shift digits up
		add_	ax, di			; add in new digit
		jmp	.keypadUpdated

; ---------------------------------------------------------------------
.keypadClear	xor_	al, al			; cancel keypad ASCII code
.keypadUpdated	mov	[AltNumpad], al		; store ASCII code from keypad
.xlatFail	jmp	.keyProcessed

; ---------------------------------------------------------------------
; Alt key not down so check for ctrl key
.altNotDown	test	bl, KBSHIFT1_CTRL
		jz	.ctrlNotDown
		mov	di, KbXlatCtrl
		call	KbXlatScancode
		jnb	.xlatFail
		jmp	.processExtKey

; ---------------------------------------------------------------------
; Ctrl not down so check shift keys
.ctrlNotDown	test	ch, KBSTAT3_LASTE0	; extended keys don't care
		jnz	.noModifiers		; about shift key state

		mov	di, KbXlatShift		; assume shift keys down
		test	bl, KBSHIFT1_NUMLOCK
		jnz	.numLockOn
		test	bl, KBSHIFT1_RSHIFT | KBSHIFT1_LSHIFT
		jnz	.xlatScancode
		jmp	.noModifiers
		nop				; useless nop

.numLockOn	; If numlock is on, invert the shift key test
		test	bl, KBSHIFT1_RSHIFT | KBSHIFT1_LSHIFT
		jz	.xlatScancode

.noModifiers	mov	di, KbXlat

		; Some special handling to maintain the shift state of
		; the insert key (did anyone ever use this flag?)
		cmp	al, SC2_INSERT
		jnz	.xlatScancode
		test	bh, KBSHIFT2_INSERT
		jnz	.keypadClear
		xor	bl, KBSHIFT1_INSERT
		or	bh, KBSHIFT2_INSERT

.xlatScancode	call	KbXlatScancode
		jnb	.xlatFail
		jmp	.capsLockAdjust

; ---------------------------------------------------------------------
; Extended keys (the grey ones) on the AT keyboard have special code to
; generate the appropriate scancodes depending on the currently pressed
; modifier keys.
.notKeypad	mov_	al, ah
		mov	[AltNumpad], 0
		test	ch, KBSTAT3_LASTE0	; extended key?
		jz	.notExtKey

		cmp	al, SC2_EXT_SOLIDUS	; is it '/'?
		jnz	.notExtSolidus
		mov	ah, SC2_EXT_SOLIDUS_ALT
		test	bl, KBSHIFT1_ALT
		jnz	.gotExtKeyCode
		mov	ah, SC2_EXT_SOLIDUS_CTRL
		test	bl, KBSHIFT1_CTRL
		jnz	.gotExtKeyCode
		mov	ax, (SC2_PREFIX_EXT << 8) | '/'
		jmp	.processKeyCode

.extAltEnter	mov	ah, SC2_EXT_ENTER_ALT

.gotExtKeyCode	xor_	al, al			; extended keys have no ASCII representation
		jmp	.processKeyCode
		db	90h			; assembler-inserted nop

.notExtSolidus	cmp	al, SC2_ENTER		; ALT+KbEnter can also be
		jnz	.notExtKey		; input using the normal
		test	bl, KBSHIFT1_ALT	; Enter key
		jnz	.extAltEnter

; ---------------------------------------------------------------------
; If we reach here we've determined that it's not a key on the extended
; keys added by the PC/AT.  Pass it through scancode->ASCII conversion.
.notExtKey	mov	di, KbXlatAlt
		test	bl, KBSHIFT1_ALT
		jnz	.xlatExtCode
		mov	di, KbXlatCtrl
		test	bl, KBSHIFT1_CTRL
		jnz	.xlatExtCode
		mov	di, KbXlatShift
		test	bl, KBSHIFT1_RSHIFT | KBSHIFT1_LSHIFT
		jnz	.xlatExtCode
		mov	di, KbXlat

.xlatExtCode	call	KbXlatScancode
		jnb	.keyProcessed		; no valid xlat?  ignore key.

; ---------------------------------------------------------------------
; Perform caps-lock shifting if necessary.  Certain extended keys do
; not need shifting applied to them, so check for those first.
		; Ctrl+Print Screen?
		cmp	ax, (SC2_EXT_KP_STAR << 8) | SC2_EXT_SYSRQ
		jz	.processExtKey

		; Ctrl+Tab
		cmp	ax, (SC2_TAB << 8) | SC2_TAB_CTRL
		jz	.processExtKey

		; F11+F12 are extended keys (introduced on AT keyboard)
		cmp	ah, SC2_F11
		jnb	.processExtKey

		; All scancodes between numlock and F11 are extended keys
		; that return an E0 prefix
		cmp	ah, SC2_NUMLOCK
		jnb	.processE0Prefix

		; Functions keys are also extended keys
		cmp	ah, SC2_F1
		jnb	.processExtKey

		; Not sure what this set of tests is for?
		test	bl, KBSHIFT1_ALT
		jz	.capsLockAdjust
		push	ax
		add	al, ' '
		pop	ax
		js	.processExtKey

.capsLockAdjust	test	bl, KBSHIFT1_CAPLOCK
		jz	.capsLockDone
		cmp	al, 'A'
		jb	.notCapsRange
		cmp	al, 'Z'
		ja	.notCapsRange
		add	al, 20h		; to lowercase
		jmp	.capsLockDone

.notCapsRange	cmp	al, 'a'
		jb	.capsLockDone
		cmp	al, 'z'
		ja	.capsLockDone
		sub	al, 20h		; to uppercase
.capsLockDone	jmp	.processE0Prefix

; ---------------------------------------------------------------------
; When we reach here we have an extended key scancode in AL which must
; be transferred to AH.  If the previous scancode received was the E0
; extended key prefix then also transfer that into AL.
.processExtKey	mov_	ah, al
		xor_	al, al
.processE0Prefix
		test	ch, KBSTAT3_LASTE0
		jz	.processKeyCode
		or_	al, al		; have ASCII code?
		jnz	.haveAsciiCode
		mov	al, SC2_PREFIX_EXT
		jmp	.processKeyCode
.haveAsciiCode	mov	ah, SC2_PREFIX_EXT

; ---------------------------------------------------------------------
; At this point we have a scancode and ASCII code in AX.  Now we must
; insert it into the keyboard buffer.
.processKeyCode	cli
		call	KbExtendBuf
		jz	.kbOverrun
		mov	di, [KbLastChar]
		mov	[di], ax		; insert into buffer
		call	KbAdvanceBuf
		mov	[KbLastChar], di
		call	KbKeyClick		; audible feedback

		; Inform any OS hook the keyboard interrupt has been processed
		mov	ax, 9102h		; OS hook - keyboard
		int	15h

; ---------------------------------------------------------------------
; Key has either been processed or is a keypress we should ignore (e.g.
; invalid combination of extended key and modifiers).  Perform any
; post-processing needed to keep keyboard state in sync.
.keyProcessed	; Start by checking for pause key
		test	bh, KBSHIFT2_PAUSE
		jz	.clearE0E1
		test	[KbShiftFlags2], KBSHIFT2_PAUSE
		jnz	.clearE0E1
		mov	[KbShiftFlags1], bx
		call	KbAckInt
.waitPauseOff	test	[KbShiftFlags2], KBSHIFT2_PAUSE
		cli
		jz	.leaveFunction
		sti				; give user a chance
		jmp	.waitPauseOff		; to unpause

.clearE0E1	; Reset prefix flags now we've processed any
		; following scancodes
		and	ch, ~(KBSTAT3_LASTE0 | KBSTAT3_LASTE1)

.storeBdaFlags	; Store back the in-progress shift and status flags
		mov	[KbStatusFlags3], ch
		mov	[KbShiftFlags1], bx	; store both bytes

; ---------------------------------------------------------------------
; Unwind the stack frame, sync the keyboard LEDs with the updated
; BDA state, and reset the PIC so further keypresses can be received.
.leaveFunction	pop	es
		pop	ax			; pushed from ds
		pop	di
		pop	si
		pop	dx
		pop	cx
		pop	bx

		push	ax
		mov	al, NONSPECIFIC_EOI
		out	PORT_PIC1_CTRL, al	; ack the interrupt

		call	KbSyncLedState
		jb	.leaveFunction2
		call	KbEnable

.leaveFunction2	pop	ds			; restore ds from ax
		pop	ax
		pop	bp
		iret				; really leave

; ---------------------------------------------------------------------
; Handle keyboard buffer overrun by beeping
.kbOverrun	sti
		call	Beep
		jmp	.keyProcessed

; ---------------------------------------------------------------------
; Handle resend request from keyboard by recording it, not by doing
; anything to fix it.
.kbResend	or	[KbStatusFlags4], KBSTAT4_RESEND
		jmp	.leaveFunction

; ---------------------------------------------------------------------
; Handle acknowledge response from keyboard by recording it
.kbAck		or	[KbStatusFlags4], KBSTAT4_ACK
		jmp	.leaveFunction

; ---------------------------------------------------------------------
; Handle sysrq key being pressed by recording it and letting any OS
; hook intercept it.
.makeSysReq	test_	bh, cl			; sysrq already down?
		jnz	.sysrqDown		; skip OS hook if so
		mov	ax, 8500h		; OS hook - system request
		int	15h
.sysrqDown	or_	bh, cl			; mark sysrq down
		jmp	.clearE0E1

; ---------------------------------------------------------------------
; Handle sysrq key being released by recording it and letting any OS
; hook intercept it.
.breakSysReq	and_	bh, cl			; mark sysrq up
		mov	ax, 8501h		; OS hook - system request down
		int	15h
		jmp	.clearE0E1

; ---------------------------------------------------------------------
; Intermediate jmp so we can use short displacements below
.thunkHandleFF	jmp	.regularKeyCode

; ---------------------------------------------------------------------
; PrtScr/* key is complicated and needs different handling depending on
; which type of keyboard is attached.
.handlePrtScr	test	ch, KBSTAT3_ENHKB	; 101 key keyboard?
		jnz	.enhPrtScr

		; Non-enhanced keyboard needs shift but not ctrl
		; or alt held to access the PrtScr key
		test	bl, KBSHIFT1_RSHIFT | KBSHIFT1_LSHIFT
		jz	.thunkHandleFF
		test	bl, KBSHIFT1_CTRL | KBSHIFT1_ALT
		jnz	.thunkHandleFF
		jmp	.printScreen

.enhPrtScr	; Enhanced keyboard has a dedicated PrtScr/SysRq key
		; so try to distinguish them
		test	bl, KBSHIFT1_ALT	; alt+prtscr not valid
		jnz	.thunkHandleFF
		test	ch, KBSTAT3_LASTE0	; no E0 prefix means another key
		jz	.noE0PrtScr
		test	bl, KBSHIFT1_CTRL	; ctrl+prtscr is sysrq
		jz	.printScreen
		mov	ax, (SC2_EXT_SYSRQ << 8) + 00h
		jmp	.processKeyCode

.noE0PrtScr	; Special handling for prtscr without a leading E0 prefix?
		mov	ax, 9600h		; ???
		test	bl, KBSHIFT1_CTRL
		jnz	.notStar		; ???
		mov	ax, (SC2_EXT_KP_STAR << 8) + '*'
.notStar	jmp	.processKeyCode

.printScreen	and	[KbStatusFlags3], ~(KBSTAT3_LASTE0 | KBSTAT3_LASTE1)
		call	KbExtendBuf
		jz	.prtScrDone
		call	KbAckInt
		push	bp
		int	5			; Print screen
		pop	bp
.prtScrDone	jmp	.leaveFunction

; ---------------------------------------------------------------------
; Handle second byte of enhanced keyboard ID reply.  Must have received
; the initial ack byte but not yet the second for it to be valid.
.handleIdByte2	test	ch, KBSTAT3_ID_1
		jz	.thunkHandleFF
		test	ch, KBSTAT3_ID_2
		jnz	.thunkHandleFF
		or	ch, KBSTAT3_ID_2	; mark as received
		jmp	.clearE0E1

; ---------------------------------------------------------------------
; Handle third byte of enhanced keyboard ID reply.  If both previous
; bytes successfully received then record that we have an enhanced
; keyboard and set the numlock state appropriately.
.handleIdByte3	test	ch, KBSTAT3_ID_1
		jz	.thunkHandleFF
		test	ch, KBSTAT3_ID_2
		jz	.thunkHandleFF

		or	ch, KBSTAT3_ENHKB	; enhanced keyboard present
		test	ch, KBSTAT3_FORCE_NUMLOCK
		jz	.noForceNumLock
		or	bl, KBSHIFT1_NUMLOCK
.noForceNumLock	jmp	.clearE0E1

; ---------------------------------------------------------------------
; Handle an E0 prefix by recording that we have seen it,
.handleE0	or	ch, KBSTAT3_LASTE0
		jmp	.storeBdaFlags

; ---------------------------------------------------------------------
; Handle an E1 prefix by recording that we have seen it.
.handleE1	or	ch, KBSTAT3_LASTE1
		jmp	.storeBdaFlags

; ---------------------------------------------------------------------
; Handle alt or ctrl being pressed.  Don't persist shift state changes
; for keys without a break code.
.makeAlt	test	bl, KBSHIFT1_ALT
		jnz	.makeLCtrl		; pressing a second alt key doesn't
						; clear the ALT+numpad buffer
		mov	[AltNumpad], 0

.makeLCtrl	test	ch, KBSTAT3_LASTE1	; no break code expected?
		jnz	.skipShiftAdj
		call	KbShiftAdjSet
		and	ch, ~KBSTAT3_LASTE0

.skipShiftAdj	jmp	.storeBdaFlags

; ---------------------------------------------------------------------
; Handle shift being pressed by storing the new shift state.
.makeShift	test	ch, KBSTAT3_LASTE0
		jnz	.ignoreModifier
		or_	bl, cl

.ignoreModifier	jmp	.clearE0E1

; ---------------------------------------------------------------------
; Handle alt being released by clearing the shift state and applying
; the alt+numpad code that has been built up (if any).
.breakAlt	call	KbShiftAdjClear
		xor_	ax, ax
		or	al, [AltNumpad]
		jz	.ignoreModifier
		jmp	.processKeyCode

; ---------------------------------------------------------------------
; Handle ctrl being released by clearing the shift state.
.breakLCtrl	test	ch, KBSTAT3_LASTE1
		jnz	.skipShiftAdj
		call	KbShiftAdjClear
		and	ch, ~KBSTAT3_LASTE0
		jmp	.storeBdaFlags

; ---------------------------------------------------------------------
; Handle shift being released by clearing the shift state.
.breakShift	test	ch, KBSTAT3_LASTE0
		jnz	.ignoreModifier
		and_	bl, cl
		jmp	.clearE0E1

; ---------------------------------------------------------------------
; Handle caps lock being pressed: ignore if ctrl is down.
.makeCapLk	; Fallthrough into shared code

; ---------------------------------------------------------------------
; Generic code to handle caps+num lock being pressed
.toggleLk	test_	bh, cl			; key already down?
		jnz	.ignoreLk
		test	bl, KBSHIFT1_CTRL	; ctrl pressed?
		jnz	.ignoreLk		; ignore lock if so
		or_	bh, cl			; mark key as down
		xor_	bl, cl			; toggle shift state

.ignoreLk	jmp	.clearE0E1

; ---------------------------------------------------------------------
; Generic code to handle caps+num lock being released
.breakLk	and_	bh, cl			; mark key as up
		jmp	.clearE0E1

; ---------------------------------------------------------------------
; Handle num lock being pressed: 84 key keyboard has pause on
; ctrl+numlock and the 101 version does not.  Forward to generic code
; if we detect a valid numlock press.
.makeNumLk	test	ch, KBSTAT3_LASTE1	; no break code?
		jnz	.makePause		; must be pause key

		test	ch, KBSTAT3_ENHKB
		jz	.noEnhNumLk

		; Ctrl+numlock does nothing on enhanced keyboard
		test	bl, KBSHIFT1_CTRL
		jz	.lkTestCtrl
		jmp	.clearE0E1

.noEnhNumLk	; Ctrl+numlock on 84-key is pause key
		test	bl, KBSHIFT1_CTRL

; Shared code for testing ctrl key for caps/num/scr lock
.lkTestCtrl	jz	.toggleLk
		test	ch, KBSTAT3_ENHKB
		jnz	.toggleLk
		; Fallthrough into pause key handler

; ---------------------------------------------------------------------
; Handle pause key being pressed by setting the shift state and if in
; video mode 2 (text mode) reset the CGA/MDA mode control register to
; ensure that the display is turned on so the user can see any output.
; There is no break code for the pause key -- the user has to press any
; other key to break out of pause mode.
.makePause	test	bh, KBSHIFT2_PAUSE
		jz	.doMakePause		; don't pause twice
		jmp	.clearE0E1

.doMakePause	or	bh, KBSHIFT2_PAUSE	; enable shift state

		; Ensure display is turned on if in text mode
		mov	al, [VidActiveMode]
		and	al, 0FEh
		cmp	al, 2			; in text mode?
		jnz	.makePauseDone		; skip if not
		push	dx
		mov	dx, [VidBasePort]
		add	dx, VID_MODE_PORT_OFFSET
		mov	al, [VidModeCtrl]
		out	dx, al
		pop	dx

.makePauseDone	jmp	.keyProcessed

; ---------------------------------------------------------------------
; Handle scroll lock being pressed with special handling for ctrl key
; being pressed (triggers ctrl+break handler).
.makeScrLk	test	ch, KBSTAT3_LASTE0
		jnz	.pauseBrk
		test	ch, KBSTAT3_ENHKB
		jz	.noEnhScrollLk

		; 101-key keyboard clears pause if ctrl is held down,
		; is scroll lock otherwise
		test	bl, KBSHIFT1_CTRL
		jz	.lkTestCtrl
		jmp	.scrLkClrPause
		nop				; assembler-inserted nop

.noEnhScrollLk	; 84-key keyboard clears pause if ctrl held down and in
		; pause shift state, otherwise  is scroll lock.
		test	bl, KBSHIFT1_CTRL
		jz	.lkTestCtrl
		test	bh, KBSHIFT2_PAUSE
		jz	.pauseBrk

.scrLkClrPause	and	bh, ~KBSHIFT2_PAUSE
		jmp	.clearE0E1

.pauseBrk	test	bl, KBSHIFT1_ALT
		jz	.doCtrlBrk
		test	ch, KBSTAT3_ENHKB
		jz	.ignoreScrLk
		test	bl, KBSHIFT1_CTRL
		jz	.makePause
.ignoreScrLk	jmp	.clearE0E1

; ---------------------------------------------------------------------
; Forward ctrl+break keypress to interrupt handler.  Clear pause shift
; state if we're in it.
.doCtrlBrk	test	bh, KBSHIFT2_PAUSE
		jnz	.scrLkClrPause

		; Clear keyboard buffer
		mov	ax, [KbBufStart]
		mov	[KbNextChar], ax
		mov	[KbLastChar], ax

		; Set ctrl+break flag and make interrupt call
		mov	[CtrlBrkFlag], 80h
		pushf
		cli
		push	cs
		mov	ax, 0A54Eh		; ???
		push	ax
		call	MakeIsrStack
		int	1Bh			; Ctrl+break handler
		jmp	UnmakeIsrStack
		xor_	ax, ax
		jmp	.processKeyCode

; ---------------------------------------------------------------------
; Handle insert key being released by clearing the key state.
.breakIns	and	bh, ~KBSHIFT2_INSERT
		jmp	.clearE0E1

		ENDPROC	Int9_Actual

