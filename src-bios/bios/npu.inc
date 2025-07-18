
; ---------------------------------------------------------------------------
; 80287 FPU/NPU control ports
; Ports 0E8h-0EFh and 0F8h-0FFh are reserved for 286/287 private communication.
; See [TechRef 2-5] and [FE3001A manual], section 2.9 "Coprocessor Interface"
; for details of the co-processor I/O port interface.
PORT_287CLRBUSY	equ	0F0h	; write all-bits zero to clear 80287 BUSY flag
PORT_287RESET	equ	0F1h	; write all-bits zero to reset 80287
