
; ===========================================================================
;
; Macro definitions used throughout the BIOS code.
;
; ===========================================================================

EUROASM FPU=ON, LISTINCLUDE=ON, LISTVAR=ON, LISTMACRO=OFF, LISTREPEAT=OFF, AUTOALIGN=OFF

; FillRom address, fill
; 
; Advances the current location to the specified address, filling any space
; with the byte provided.
FillRom		%macro
		d	(%1 - offset#$) * byte %2
		%endmacro

; CompatAddress address
;
; Raises an error if the current assembly location is not equal to the address
; provided.  Meant to prevent items that have fixed addresses due to external
; compatibility constraints from shifting around by accident.
CompatAddress	%macro
%here		%seta offset#$
		%if %1 != %here
		%error Compatibility constraint failed: %00 must be at %1 but was at %here
		%endif
		%endmacro

; Delay count
;
; Inserts short jumps to delay execution by count*7 clocks.
Delay		%macro
i		%for	1..%1
		jmps	$+2
		%endfor
%endmacro

; Underscore-suffixed opcodes are actually macros that specify an alternate
; instruction coding.  Used to match the encoding used by GRiD's original
; assembler, which preferred the numerically higher of ambiguous encodings.
add_		%macro
		add	%*,CODE=LONG
		%endmacro
and_		%macro
		and	%*,CODE=LONG
		%endmacro
cmp_		%macro
		cmp	%*,CODE=LONG
		%endmacro
mov_		%macro
		mov	%*,CODE=LONG
		%endmacro
or_		%macro
		or	%*,CODE=LONG
		%endmacro
sub_		%macro
		sub	%*,CODE=LONG
		%endmacro
test_		%macro
		test	%*,CODE=LONG
		%endmacro
xor_		%macro
		xor	%*,CODE=LONG
		%endmacro

; Inline WriteString,'StringConstant',0
; 
; Encodes a call to WriteString_Inline with subroutine parameters encoded
; as literals immediately following the call instruction.  Used for one-off
; string literals that don't need individual names.
Inline		%macro
		%if "%1" == "WriteString"
		call	WriteString_Inline
i		%for 2..%#
		db	%2
		%shift
		%endfor
		%else
		%error "Unknown Inline: %1"
		%endif
		%endmacro

