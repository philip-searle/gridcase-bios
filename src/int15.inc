
	include1	"descriptors.inc"

; Int 15h AH function numbers [TechRef 3-7, 8-6]
INT15_KB_INTERCEPT	equ	04Fh
INT15_BLOCK_MOVE	equ	087h
INT15_SWITCH_PM		equ	089h
INT15_DEVICE_POST	equ	091h
INT15_GET_CONFIG_TABLE	equ	0C0h
INT15_GRID_SUBSYSTEM	equ	0E4h

; Return value for unsupported function numbers
INT15_RET_UNSUPPORTED	equ	086h

; Return values for INT15_BLOCK_MOVE [TechRef 3-9]
INT15_BM_SUCCESS	equ	0
INT15_BM_PARITY_ERROR	equ	1
INT15_BM_CPU_EXCEPTION	equ	2
INT15_BM_A20FAILURE	equ	3

; Return values for INT15_SWITCH_PM [TechRef 3-11]
INT15_SWITCH_PM_SUCCESS	equ	0
INT15_SWITCH_PM_FAILURE	equ	0FFh

; Global descriptor table layout for INT15_BLOCK_MOVE [TechRef 3-10]
BlockMoveGdt		struc
.dummy			ds	DESCRIPTOR
.gdt			ds	DESCRIPTOR
.sourceData		ds	DESCRIPTOR
.targetData		ds	DESCRIPTOR
.biosCode		ds	DESCRIPTOR
.stack			ds	DESCRIPTOR
			endstruc

; Global descriptor table layut for INT15_SWITCH_PM [TechRef 3-13]
PmSwitchGdt		struc
.dummy			ds	DESCRIPTOR
.gdt			ds	DESCRIPTOR
.idt			ds	DESCRIPTOR
.ds			ds	DESCRIPTOR
.es			ds	DESCRIPTOR
.ss			ds	DESCRIPTOR
.cs			ds	DESCRIPTOR
.biosCode		ds	DESCRIPTOR
			endstruc
