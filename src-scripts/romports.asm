ROMPORTS	PROGRAM	OutFile=build/romports.exe, FORMAT=MZ, ENTRY=Init

[CODE]		SEGMENT	purpose=CODE,width=16,align=BYTE,combine=PUBLIC,class=""
[DATA]		SEGMENT	purpose=DATA,width=16,align=BYTE,combine=PUBLIC,class=""
[BSS]		SEGMENT	purpose=BSS,width=16,align=BYTE,combine=PUBLIC,class=""
		EUROASM	CPU=286, PRIV=ENABLED, AUTOSEGMENT=ON


; Macro to suppress unused symbol warning for fall-through
Fallthrough	%macro
		EUROASM	PUSH, NOWARN=2101
Fallthrough_%.:	equ	%1
		EUROASM	POP
		%endmacro

Ports		d w 06F8h, 0000h
		d w 07FAh, 0000h
		d w 07F8h, 0000h
		d w 07F9h, 0000h
		d w 0BD0h, 0000h
		d w 0BD1h, 0000h
PortCount	equ	($ - Ports) / 4
		; DumpSeg+DumpOffset must be immediately after Ports
		; because we abuse PortCount+1 to display them without
		; them being adjustable by the input routines.
		; Order is also important because we load it with LDS.
DumpOffset	d w 00000h
DumpSeg		d w 0A000h

CopyRom		d (16 * 16) * b

%if OFFSET#Ports <> 0
%error "mov si,Ports" -> "xor si,si" depends on Ports being at offset 0
%endif

Init		PROC
		; Init
		push	PARA#[DATA]
		pop	ds
		push	ds
		pop	es

MainLoop	; Play with ports, grab ROM content
		mov	cx, PortCount
		mov	si, Ports
PlayPorts	lodsw
		mov	dx, ax
		lodsw
		out	dx, al
		loop	PlayPorts

		push	ds
		lds	si, [DumpOffset]
		mov	di, CopyRom
		mov	cx, 16 * 16
GrabRomContent	lodsb
		stosb
		loop	GrabRomContent
		pop	ds

		mov	cx, PortCount
		mov	si, Ports
ResetPorts	lodsw
		mov	dx, ax
		xor	ax, ax
		out	dx, al
		lodsw
		loop	ResetPorts

		mov	ax, 0003h	; Mode 3: 80x25 color text
		int	10h

		; Display registers
		mov	si, Ports
		mov	cx, PortCount+1	; HACK: pretend there's an extra port
DumpPorts	call	ConHexReg
		loop	DumpPorts

		; Display dump
		mov	cx, 16
		mov	si, CopyRom
DumpRom		push	cx

		mov	cx, 8
DumpRow		lodsw
		call	ConHex4
		loop	DumpRow

		pop	cx
		call	ConCrlf
		loop	DumpRom

		; Process input
; =========================================================== 338
;		%comment
		xor	ah, ah
		int	16h
		xor	ah, ah

		cmp	al, '['
		jne	.1
		sub	[DumpOffset], 16*16*16
		jmp	.inputDone

.1		cmp	al, ']'
		jne	.2
		add	[DumpOffset], 16*16*16
		jmp	.inputDone

.2		cmp	al, '/'
		jne	.3
		xor	[DumpSeg], (0A000h ^ 0B000h),DATA=WORD
		jmp	.inputDone

.3		cmp	al, '1'
		jb	.inputDone
		sub	al, '1'
		cmp	al, PortCount
		jge	.inputDone
		shl	al, 2
		add	ax, Ports + 2
		mov	bx, ax
		xor	[bx], 1,DATA=BYTE
		;jmp	.inputDone
;		%endcomment
; ===========================================================


.inputDone	jmp	MainLoop
		ENDPROC	Init

; =========================================================== 338
;%comment
ConHexReg	PROC
		call	ConHex4_BE
		call	ConHex4_BE
		Fallthrough ConCrlf
		ENDPROC	ConHexReg

ConCrlf		PROC
		mov	ax, 0E0Dh
		int	10h
		mov	al, 0Ah
		int	10h
		ret
		ENDPROC	ConCrlf

ConHex4_BE	lodsw
		xchg	ah, al
ConHex4		PROC
		call	ConHex2
		xchg	ah, al
		call	ConHex2
		Fallthrough ConHexSpace
		ENDPROC	ConHex4

ConHexSpace	PROC
		mov	al, ' '
		Fallthrough ConChar
		ENDPROC	ConHexSpace

ConChar		PROC
		push	bx
		xor	bx, bx
		mov	ah, 0Eh
		int	10h
		pop	bx
		ret
		ENDPROC	ConChar

ConHex2		PROC
		push	ax
		shr	al, 4
		call	ConHex1
		pop	ax
		Fallthrough ConHex1
		ENDPROC	ConHex2

ConHex1		PROC
		push	ax
		and	al, 0Fh
		add	al, 90h
		daa
		adc	al, 40h
		daa
		call	ConChar
		pop	ax
		ret
		ENDPROC	ConHex1
;%endcomment
; ===========================================================
; ===========================================================

;%Display segments
		ENDPROGRAM	ROMPORTS
