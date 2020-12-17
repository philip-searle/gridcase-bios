
; ===========================================================================
;
; Macro definitions used throughout the BIOS code.
;
; ===========================================================================

[map symbols]

BIOS_START      equ     08000h

; FillRom address, fill
; 
; Advances the current location to the specified address, filling any space
; with the byte provided.
%macro FillRom 2.nolist
		times   %1-($-$$) db %2
%endmacro

; CompatAddress address
;
; Raises an error if the current assembly location is not equal to the address
; provided.  Meant to prevent items that have fixed addresses due to external
; compatibility constraints from shifting around by accident.
%macro CompatAddress 1.nolist
		%assign ACTUAL_ADDRESS ($-$$)
		%assign COMPAT_ADDRESS %1
		%if ACTUAL_ADDRESS != COMPAT_ADDRESS
		%error Compatibility constraint failed: %00 must be at COMPAT_ADDRESS but was at ACTUAL_ADDRESS
		%endif
%endmacro

