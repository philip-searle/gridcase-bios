GRID_CPU	PROGRAM	OutFile=grid_cpu.obj

		include	"macros.inc"
		include	"segments.inc"
		include	"grid.inc"

		PUBLIC	GridCpuFast, GridCpuSlow

; ===========================================================================
; GridCpuFast
; Changes the 80286 CPU clock to run at the normal speed of 10MHz
; ===========================================================================
GridCpuFast	PROC
		push	ax
		mov	al, 0
		jmp	GridCpuSpeed
		FillerNop
		ENDPROC	GridCpuFast

; ===========================================================================
; GridCpuSlow
; Changes the 80286 CPU clock to run at the slower speed of 5MHz
; ===========================================================================
GridCpuSlow	PROC
		push	ax
		mov	al, 1
		; fall-through to GridCpuSpeed
		ENDPROC	GridCpuSlow

; ===========================================================================
; GridCpuSpeed
; Sets the CPU to either 10MHz or 5MHz depending on whether AL is 0 or 1.
; On entry, expects AX to have been pushed on the stack.
; ===========================================================================
GridCpuSpeed	PROC
		push	dx
		mov	dx, PORT_CPU_SPEED
		out	dx, al
		pop	dx
		pop	ax
		retn
		ENDPROC	GridCpuSpeed

ENDPROGRAM	GRID_CPU
