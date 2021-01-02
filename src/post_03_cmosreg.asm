
POST03_CmosReg	PROC
		; Enter via fall-through from previous POST code

; ---------------------------------------------------------------------------
; CMOS register read/write test
; Rotate a single bit through the shutdown reason register to test CMOS RAM.
		mov	al, CHECKPOINT_CMOS_RW
		out	PORT_DIAGNOSTICS, al

		mov	ah, 1			; set a single bit in the test pattern

		; Write test pattern to CMOS
.testCmosRam	mov	al, CMOS_SHUTDOWN_REASON | NMI_DISABLE
		out	PORT_CMOS_ADDRESS, al
		Delay	2
		mov	al, ah,CODE=LONG
		out	PORT_CMOS_DATA, al
		Delay	2

		; Read back test pattern from CMOS
		mov	al, CMOS_SHUTDOWN_REASON | NMI_DISABLE
		out	PORT_CMOS_ADDRESS, al
		Delay	2
		in	al, PORT_CMOS_DATA

		cmp	al, ah,CODE=LONG	; does it match?
		jz	.l3			; keep going if so
		mov	al, BEEP_CMOS_FAIL	; if not, probable stuck bits in CMOS RAM
		jmp	FatalBeeps

.l3		rol	ah, 1			; shift bit and try again
		jnb	.testCmosRam		; (until the bit falls out into the carry)

		; Exit via fall-through to next POST procedure
		ENDPROC POST03_CmosReg

