
; ===========================================================================
; Stack layout built by MakeIsrStack
IsrStack	struc
.es		dw	0
.ds		dw	0
.bp		dw	0
.di		dw	0
.si		dw	0
.dx		dw	0
.cx		dw	0
.bx		dw	0
.returnOffset	dw	0
.returnSegment	dw	0
.returnFlags	dw	0

; Add aliases for byte registers
$		equ	.bx
.bl		db	0
.bh		db	0
		endstruc

; Stack layout used by floppy disk int13, which adds AX to the saved registers
IsrStackAx	struc
.ax		dw	0
.es		dw	0
.ds		dw	0
.bp		dw	0
.di		dw	0
.si		dw	0
.dx		dw	0
.cx		dw	0
.bx		dw	0
.returnOffset	dw	0
.returnSegment	dw	0
.returnFlags	dw	0

; Add aliases for byte registers
$		equ	.ax
.al		db	0
.ah		db	0
$		equ	.dx
.dl		db	0
.dh		db	0
$		equ	.cx
.cl		db	0
.ch		db	0
		endstruc

