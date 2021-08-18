; ===========================================================================
; 
; GRiD 1520 BIOS
; 
; Main file; all code/data is ultimately included from here.
; 
; Comments marked with [TechRef] refer to the GRiDCase 1500 Series Hardware
; Technical Reference.
; 
; Comments marked with [Public] identify locations/APIs mentioned in publicly
; available documentation.
; 
; Comments marked with [Compat] identify places where data/code layout must
; remain fixed for compatibility with software that relies on undocumented or
; semi-documented functionality.
; 
; Comments marked with [ReadMe] refer to the file README.txt in this project.
; ===========================================================================

		include	"src/macros.inc"
		include	"src/pic.inc"
		include	"src/pit.inc"
		include	"src/keyboard.inc"
		include	"src/cmos.inc"
		include	"src/diagnostics.inc"
		include	"src/dma.inc"
		include	"src/npu.inc"
		include	"src/parallel.inc"
		include	"src/serial.inc"
		include	"src/video.inc"
		include	"src/hdc_at.inc"
		include	"src/fdc.inc"
		include	"src/grid.inc"
		include "src/isr.inc"
		include "src/int13.inc"
		include	"src/segments.inc"
		include	"src/descriptors.inc"
		include	"src/todo.asm"

; ---------------------------------------------------------------------------
; 32K BIOS starts at F000:8000
; [TechRef] 3-1 "ROM-BIOS AND I/O REGISTERS"
; It's easier to generate a 64KB ROM with 32KB empty at the start, and then
; reduce it to 32KB later than it is to work out how an offset origin works.
; ---------------------------------------------------------------------------
$		equ	BIOS_START

		; Copyright notice at start, doubled so both the even and odd ROMs contain a complete copy
		db	'CCooppyyrriigghhtt  11998855,,11998866  PPhhooeenniixx  TTeecchhnnoollooggiieess  LLttdd..'

		include	"src/reset.asm"
		include	"src/reset_utils.asm"
		include	"src/post/vidtest.asm"
		include	"src/post/kbc.asm"
		include	"src/post/timer.asm"
		include	"src/post/protmode.asm"
		include	"src/int19.asm"
		include	"src/int18.asm"
		include	"src/int13_at.asm"
		include	"src/int13_grid.asm"
		include	"src/int14.asm"
		include	"src/int16.asm"
		include	"src/int9.asm"
		include	"src/kbxlat.asm"
		include	"src/keyboard.asm"
		include	"src/kbc_286.asm"
		include	"src/int13_fd.asm"
		include	"src/floppy/int13_handlers.asm"
		include	"src/floppy/std_dpbs.asm"
		include	"src/floppy/bda.asm"
		include	"src/floppy/commands.asm"
		include	"src/floppy/controller.asm"
		include	"src/floppy/misc.asm"
		include	"src/int17.asm"
		include	"src/int10.asm"

; ---------------------------------------------------------------------------
; System identification section at F000:DFD0
; ---------------------------------------------------------------------------
		FillRom	0DFD0h, 0FFh

		; EPROM part numbers, arranged so each split ROM has a unique number.
		; 1520 BIOS EPROM part numbers differ in the final digit.
		; Note that the arrow is offset by one byte, so even/odd ROMs have it in different columns.
		; [Public] 0FDFDh:000Ch == 2D2Dh
		; [TechRef] 3-26 "System Identification"
		db	'330000663365--0000 <<-- ppaarrtt  nnuummbbeerr'

		; [Public] 0F000h:0DFFEh == 34h
		; [TechRef] 3-26 "System Identification"
GridSysId	CompatAddress 0DFFEh
		dw	0034h

; ---------------------------------------------------------------------------
; XT compatibility section at F000:E000
; AT BIOS listing refers to this as ORGS.ASM
; ---------------------------------------------------------------------------
		FillRom	0E000h, 0FFh
		include	"src/at_compat.asm"

; ---------------------------------------------------------------------------
; Power-On Reset Vector at F000:FFF0
; ---------------------------------------------------------------------------
		FillRom	0FFF0h, 0FFh

		; [Public] CPU reset vector is top of address space minus 16 bytes
		; AT BIOS calls this P_O_R (POWER ON RESET)
Reset		CompatAddress 0FFF0h
		jmp	(BIOS_SEGMENT):Reset_Compat

		; [Public] IBM BIOS stores ROM date at F000:FFF5 (AT BIOS calls this 'RELEASE MARKER')
ReleaseMarker	CompatAddress 0FFF5h
		db	'03/11/89'

		db	0FFh		; Unused byte

		; [Public] IBM BIOS stores machine identification byte at F000:FFFE
MachineId	CompatAddress 0FFFEh
		db	0FCh		; Model FC == IBM AT (6 MHz)
		db	00		; Submodel 00 == no specific submodel

; ===========================================================================
; END OF PROGRAM
; ===========================================================================

