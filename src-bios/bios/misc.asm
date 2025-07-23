
MISC		PROGRAM	OutFile=misc.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"segments/bda.inc"
		include	"bios-version.inc"
		include	"cmos.inc"
		include	"diagnostics.inc"
		include	"isr.inc"
		include	"keyboard.inc"

		EXTERN	BeepFactor
		EXTERN	Copyr_Phoenix, Copyr_Phoenix2
		EXTERN	ConString, ConCrLf, ConChar
		EXTERN	Reset_Compat

		PUBLIC	Beep
		PUBLIC	FuncToOffset, LoadBdaToDs
		PUBLIC	ChecksumBios, ChecksumRom
		PUBLIC	CheckParityErr, EnableNmi, IntNmi_Actual
		PUBLIC	SetCriticalErr
		PUBLIC	ParDetectPort
		PUBLIC	MakeIsrStack, UnmakeIsrStack, UnmakeIsrStack2
		PUBLIC	kBdaSegment, kBootSegOffset, kIvtSegment, kCrLf
		PUBLIC	ConBiosBanner, ConBadCsumMsg
		PUBLIC	ConString2, ConString_Inline
		PUBLIC	ConCharHex4, ConCharHex2
		PUBLIC	WaitKey

BEEP_LENGTH	equ	450

; ===========================================================================
; Beep
; Sounds a short beep from the system speaker.
; Expects the keyboard controller and PIT to be configured, so this function
; can't be used during POST -- use FatalBeeps for that.
; ===========================================================================
Beep		PROC
		push	ax
		push	bx
		push	cx
		in	al, PORT_KBC_PORTB
		push	ax		; save original PORTB value
		mov	bx, BEEP_LENGTH
		and	al, 0FCh	; enable speaker
.toggleSpeaker	xor	al, 2		; toggle speaker data bit
		out	PORT_KBC_PORTB, al
		mov	cl, [cs:BeepFactor]
		mov	ch, 0
		add_	cx, cx		; multiply by four
		add_	cx, cx
.1		loop	.1
		dec	bx
		jnz	.toggleSpeaker

		pop	ax		; restore original speaker state
		out	PORT_KBC_PORTB, al

		pop	cx
		pop	bx
		pop	ax
		retn
		ENDPROC	Beep

; ===========================================================================
; ResetNmi
; Resets the parity error detected bits and enables parity error NMIs.
; ??? dead code
; ===========================================================================
ResetNmi	PROC
		Unused	ResetNmi
		call	CheckParityErr
		; fall-through to EnableNmi
		ENDPROC	ResetNmi

; ===========================================================================
; EnableNmi
; Enables non-maskable interrupt input to the CPU.
; ===========================================================================
EnableNmi	PROC
		mov	al, CMOS_RTC_SECONDS | NMI_ENABLE
		jmp	SetNmi
		ENDPROC	EnableNmi

; ===========================================================================
; DisableNmi
; Disables non-maskable interrupt input to the CPU.
; ===========================================================================
DisableNmi	PROC
		mov	al, CMOS_RTC_SECONDS | NMI_DISABLE
		; fall-through to SetNmi
		ENDPROC	DisableNmi

; ===========================================================================
; SetNmi
; ===========================================================================
SetNmi		PROC
		out	PORT_CMOS_ADDRESS, al
		retn
		ENDPROC	SetNmi

; ===========================================================================
; ChecksumBios
; Checksums 8000h bytes from CX:0h, returning an 8-bit
; checksum in AL.
; ===========================================================================
ChecksumBios	PROC
		mov	ds, cx
		mov	ch, 80h
		; fall-through to ChecksumRom
		ENDPROC	ChecksumBios

; ===========================================================================
; ChecksumRom
; Checksums CX bytes of ROM content from DS:0, returning an 8-bit
; checksum in AL.
; ===========================================================================
ChecksumRom	PROC
		xor_	al, al
		xor_	si, si
.1		add	al, [si]
		inc	si
		loop	.1
		or_	al, al
		retn
		ENDPROC	ChecksumRom

; ===========================================================================
; MakeIsrStack
; Manipulates the stack so the, on return, all registers except SP are
; unaltered but the stack now contains copies of:
; ES, DS, BP, DI, SI, DX, CX, BX.
; (see IsrStack struct for offsets relative to SP)
; Also re-enables interrupts.
; ===========================================================================
MakeIsrStack	PROC
		sti
		; Push all registers to make the expected IsrStack stack frame
		; (almost: IP from the near call that got us here will end up
		; where BX should be, and vice-versa; we will adjust this later)
		push	cx
		push	dx
		push	si
		push	di
		push	bp
		push	ds
		push	es
		mov_	bp, sp			; save stack frame base in BP
		push	[bp+IsrStack.bx]	; move return address to bottom of stack
		mov	[bp+IsrStack.bx], bx	; move BX into its proepr place
		mov	bp, [bp+IsrStack.bp]	; restore original value of BP
		retn				; pop return address off stack
		ENDPROC	MakeIsrStack

; ===========================================================================
; MemClear
; Clears memory from BP:0 to SI:0 in 1KB blocks.
; ??? dead code
; ===========================================================================
MemClear	PROC
		Unused	MemClear
		push	cx
		push	di
		push	es
		push	bp

		xor_	ax, ax
.1		mov	es, bp
		xor_	di, di
		mov	cx, 1024 / 2
		rep stosw
		add	bp, 1024 >> 4
		cmp_	bp, si
		jb	.1

		pop	bp
		pop	es
		pop	di
		pop	cx
		retn
		ENDPROC	MemClear

; ===========================================================================
; FuncToOffset
; Range checks a BIOS function number and converts it to a jump table offset.
; Also loads DS with kBdaSegemnt.
; On entry:
;   AH == BIOS function number
;   DI == maximum valid jump table offset
; On return:
;   CF set if function number too large
;   DI == jump table offset for function
; ===========================================================================
FuncToOffset	PROC
		push	ax
		mov_	al, ah
		xor_	ah, ah
		shl	ax, 1
		cmp_	di, ax
		jb	.1
		mov_	di, ax
.1		pop	ax
		; fall-through into LoadBdaToDs
		ENDPROC	FuncToOffset

; ===========================================================================
; LoadBdaToDs
; Sets DS to point to the BIOS Data Area.
; ===========================================================================
LoadBdaToDs	PROC
		mov	ds, [cs:kBdaSegment],DATA=WORD
		retn
		ENDPROC	LoadBdaToDs

; ===========================================================================
; ConBiosBanner
; Writes data to the screen identifying the BIOS version and copyright.
; ===========================================================================
ConBiosBanner	PROC
		Inline	ConString,0Dh,0
		; Just display the copyright name, skip the dates
		; Actually just display the short name?  Weird.
		mov	si, Copyr_Phoenix+24
		mov	cx, 8
		call	ConString_Len
		mov	si, kBiosBanner
		call	ConString
		call	ConCrLf
		mov	si, Copyr_Phoenix
		call	ConString
		mov	si, Copyr_Phoenix2
		jmp	ConString
		ENDPROC	ConBiosBanner

kBiosBanner	BIOS_BANNER

; ===========================================================================
; ConBadCsumMsg
; Writes the message pointed to by CS:SI followed by the byte in AL as
; hexadecimal digits, followed by a newline.
; Also sets the soft reset flag.
; ===========================================================================
ConBadCsumMsg	PROC
		call	ConString
		call	ConCharHex2
		call	SetCriticalErr
		mov	si, kHexCrLf
		jmp	ConString
		ENDPROC	ConBadCsumMsg

; ===========================================================================
; ConString_Inline
; Writes a string from the address immediately following the near call that
; entered this procedure.  Plays with the stack so that on returning,
; execution continues at the byte immediately following the string's
; nul-terminator.
; ===========================================================================
ConString_Inline	PROC
		push	bp
		mov_	bp, sp
		push	si		; save SI, things are about to get weird
		mov	si, [bp+2]	; load return address into SI (we expect it to
					; point to a string constant instead of code)
		call	ConString
		inc	si		; advance past nul-terminator
		mov	[bp+2], si	; update return address to point past string
		pop	si		; restore registers, back to normal
		pop	bp
		retn
		ENDPROC	ConString_Inline

; ===========================================================================
; ConCharHex4
; Writes the word in AX to the screen as four upper-case hex digits.
; ===========================================================================
ConCharHex4	PROC
		xchg	ah, al		; move upper byte to lower
		call	ConCharHex2	; write upper byte
		xchg	ah, al		; restore lower byte
		; fall-through to ConCharHex2
		ENDPROC	ConCharHex4

; ===========================================================================
; ConCharHex2
; Writes the byte in AL to the screen as two upper-case hex digits.
; ===========================================================================
ConCharHex2	PROC
		push	ax
		push	cx
		mov	cl, 4
		shr	al, cl		; move upper nibble to lower
		pop	cx
		call	ConCharHex1	; write upper nibble
		pop	ax		; restore lower nibble
		; fall-through to ConCharHex1
		ENDPROC	ConCharHex2

; ===========================================================================
; ConCharHex1
; Writes the lower nibble of AL to the screen as an upper-case hex digit.
; ===========================================================================
ConCharHex1	PROC
		push	ax
		and	al, 0Fh		; isolate lower nibble
		add	al, 90h		; convert 00-0F -> 90-9F
		daa			; convert 90-99/9A-9F -> 90-99/00-05 with carry set appropriately
		adc	al, 40h		; convert 90-99/00-05 -> D0-D9/41-46
		daa			; convert D0-D9/41-46 -> 30-39/41-46 (ASCII 0-9/A-F)
		call	ConChar
		pop	ax
		retn
		ENDPROC	ConCharHex1

; ===========================================================================
; PromptKey
; Displays the string located at CS:SI, waits for a key to be pressed, then
; returns it in AL (upper-cased if in a-z range).
; Original scancode returned in AL.
; ===========================================================================
PromptKey	PROC
		call	ConString
		; fall-through to WaitKey
		ENDPROC	PromptKey

; ===========================================================================
; WaitKey
; Waits for a key to be pressed, then returns it in AL (upper-cased if in a-z
; range).  Original scancode returned in AL.
; ===========================================================================
WaitKey		PROC
		xor_	ah, ah
		int	16h		; read key
		and	al, 7Fh		; mask to ASCII range
		cmp	al, 'a'
		jb	.leaveFunction
		cmp	al, 'z'
		ja	.leaveFunction
		sub	al, 'a' - 'A'	; convert to uppercase
.leaveFunction	retn
		ENDPROC	WaitKey

; ===========================================================================
; IntNmi_Actual
; Handles non-maskable interrupt by prompting the user to continue, reboot,
; or disable NMI checks.
; ===========================================================================
IntNmi_Actual	PROC
		push	bp
		mov_	bp, sp		; setup stack frame to access return address
		push	ax
		push	si

		; Prevent recursive calls
		call	DisableNmi

		; Increment count of NMIs
		in	al, PORT_DIAGNOSTICS
		inc	al
		out	PORT_DIAGNOSTICS, al

		; Did a parity error cause us to be called?
		in	al, PORT_KBC_PORTB
		push	ax
		or	al, 00Ch	; set 'disable RAM and I/O parity check' bits
		out	PORT_KBC_PORTB, al
		test	al, 0C0h	; test 'RAM nad I/O parity error occurred' bits
		pop	ax
		jz	.leaveFunction	; not a parity error? ignore it

		; Determine parity error source and prompt the user for what to do
		push	ax
		mov	si, kParity1Err
		js	.foundErrorSrc
		mov	si, kParity2Err
.foundErrorSrc	call	ConString
		Inline	ConString,' interrupt at ',0
		mov	ax, [bp+4]	; load return address segment
		call	ConCharHex4
		mov	al, ':'
		call	ConChar
		mov	ax, [bp+2]	; load return address offset
		call	ConCharHex4
		Inline	ConString,'.',0Dh,0Ah,' Type (S)hut off NMI,',0
		mov	si, kRebootPrompt2
		call	PromptKey
		call	ConCrLf

		; User wants to restart?
		cmp	al, 'R'
		jnz	.noRestart
		jmp	Reset_Compat
.noRestart	; User wants to disable parity check NMIs?
		cmp	al, 'S'
		pop	ax
		jnz	.noStopNmi
		or	al, 0Ch		; set 'disable memory and I/O parity check' bits

.noStopNmi	test	al, 40h		; disable I/O parity checks if one occurred
		jz	.leaveFunction	; even if the user didn't ask us to???
		or	al, 8

.leaveFunction	; Update NMI parity check disable bits
		out	PORT_KBC_PORTB, al
		call	EnableNmi
		pop	si
		pop	ax
		pop	bp
		iret
		ENDPROC	IntNmi_Actual

kParity2Err	db	0Dh,0Ah,'I/O card parity',0
kParity1Err	db	0Dh,0Ah,'Memory parity',0

; ===========================================================================
; ConString2
; Writes the string at CS:SI to the screen, destroying CX and SI.
; ===========================================================================
ConString2	PROC
		or	cx, 0FFFFh	; don't limit string length
		; fall-through to ConString_Len
		ENDPROC	ConString2

; ===========================================================================
; ConString_Len
; Writes the string at CS:SI to the screen, up to a maximum of CX characters.
; Destroys SI.
; ===========================================================================
ConString_Len	PROC
		push	ax
.writeChar	mov	al, [cs:si]
		or_	al, al
		jz	.leaveFunction
		call	ConChar
		inc	si
		loop	.writeChar
.leaveFunction	pop	ax
		retn
		ENDPROC	ConString_Len

; ===========================================================================
; CheckParityErr
; Checks for parity error, then resets the parity error bits by toggling the
; parity check enable bits.
; On return:
;   AX == bit 7 set if memory parity error already occurred
;         bit 6 set if I/O parity error already occurred
; ===========================================================================
CheckParityErr	PROC
		push	ax
		push	dx
		mov	dx, PORT_KBC_PORTB
		in	al, dx
		xor	al, 0Ch		; toggle disable bits once
		Delay	1
		out	dx, al
		xor	al, 0Ch		; toggle disable bits twice
		Delay	1
		out	dx, al
		test	al, 0C0h	; test parity occurred bits
		pop	dx
		pop	ax
		retn
		ENDPROC	CheckParityErr

; ===========================================================================
; ParDetectPort
; Detects the presence of a writable parallel port.
; On entry:
;   DX == base parallel port number
;   BX == next storage location for found ports
; ===========================================================================
ParDetectPort	PROC
		mov	al, 0Bh
		out	dx, al		; write test pattern to data port
		inc	dx		; advance port number to control port
		inc	dx
		inc	ax		; printer control port: initialize and select printer
		out	dx, al
		dec	dx		; decrement port number back to data port
		dec	dx
		in	al, dx		; read back value
		cmp	al, 0Bh		; matches what we wrote?
		jnz	.leaveFunction
		mov	[bx], dx	; record that port exists
		inc	bx
		inc	bx		; advance to next available port array location
		add	[EquipmentWord+1], 40h,DATA=BYTE	; increment port count
.leaveFunction	retn
		ENDPROC	ParDetectPort

; ===========================================================================

; BIOS Data Area segment, referenced as a word and combines with the zero
; byte from the following constant to form 0040h.
; An unnecessarily complicated way to save one byte...
kBdaSegment	db	BDA_SEGMENT & 0FFh
		%IF	(BOOT_OFFSET & 0FFh) <> (BDA_SEGMENT & 0FF00h)
		%ERROR	"Constant merging hack won't work with current BDA_SEGMENT and BOOT_OFFSET values"
		%ENDIF

; Offset to load boot segment to (combines with previous constant)
kBootSegOffset	dw	BOOT_OFFSET

; Constant for interrupt vector table segment
kIvtSegment	dw	0

; ===========================================================================
; UnmakeIsrStack2
; As UnamkeIsrStack, but pops AX first.
; Enter with a jmp, not call.
; ===========================================================================
UnmakeIsrStack2	PROC
		pop	ax
		; fall-through to UnmakeIsrStack
		ENDPROC	UnmakeIsrStack2

; ===========================================================================
; UnmakeIsrStack
; Cleans up the stack created by MakeIsrStack and performs an iret.
; Enter with a jmp, not a call.
; ===========================================================================
UnmakeIsrStack	PROC
		pop	es
		pop	ds
		pop	bp
		pop	di
		pop	si
		pop	dx
		pop	cx
		pop	bx
		iret
		ENDPROC	UnmakeIsrStack

; ===========================================================================

; String constants for console output
kHexCrLf	db	'h'
kCrLf		db	0Dh,0Ah,0

; ===========================================================================
; SetCriticalErr
; Sets bit 0 of the SoftResetFlag to indicate that a critical error occurred
; during the startup process and that the user should be forced to acknowledge
; it before the boot process completes.  Also (ab)used to require ; the user
; to deactivate the keyboard lock during POST.
; ===========================================================================
SetCriticalErr	PROC
		push	ds
		call	LoadBdaToDs
		or	[SoftResetFlag], CRITICAL_ERR_FLAG,DATA=BYTE
		pop	ds
		retn
		ENDPROC	SetCriticalErr

; For some reason the parity error prompt text is split from the NMI handler
; by the kb_extra code.  Rather than have an entirely separate object file for
; it, we'll just stick it in a new segment and let the linker handle placing
; it in the correct place.
[CODE2]	SEGMENT WIDTH=16, ALIGN=1, CLASS=CODE, PURPOSE=DATA|CODE, COMBINE=PUBLIC
kRebootPrompt1	db	'.  Type'
kRebootPrompt2	db	' (R)eboot, other keys to continue', 0
		Unused	kRebootPrompt1

ENDPROGRAM	MISC
