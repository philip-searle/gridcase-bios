
; ===========================================================================
;
; Struct and macro definitions used for protected mode descriptors
;
; ===========================================================================

; Shift count to convert address to count of 64KB segments
; e.g. 640KB == 10 * 64KB == (640 >> SEG_SIZE_SHIFT) * SEG_SIZE_KB
SEG_SIZE_SHIFT	equ	6
SEG_SIZE_KB	equ	64

; STRUC describing a generic descriptor table entry
DESCRIPTOR      STRUC
.Limit          d w
.BaseLo         d w
.BaseHi         d b
.Flags          d b
.Reserved       d w
ENDSTRUC        DESCRIPTOR

; ---------------------------------------------------------------------
; Macro to introduce descriptor table constants.
; Required if you want to use the 'Name' parameter to the DESC_* macros
; as it sets up the table base address.
DESC_TABLE      %MACRO
                %DESC_TABLE_BASE        %SETA   offset#$
%ENDMACRO       DESC_TABLE

; ---------------------------------------------------------------------
; Macro that defines a descriptor table entry constant.
; When used, the macro invocation's label will be defined to be the
; descriptor's offset from the start of the descriptor table.
DESC_GENERIC    %MACRO  Base, Limit, Flags
                %:      equ     OFFSET#($ - %DESC_TABLE_BASE)
                ds      DESCRIPTOR, \
                        .Limit=%Limit, \
                        .BaseLo=(%Base & 0FFFFh), \
                        .BaseHi=((%Base & 0FF0000h) >> 16), \
                        .Flags=%Flags, \
                        .Reserved=0
%ENDMACRO       DESC_GENERIC

; Type values for system descriptors
DT_SYS_INVALID  equ     0
DT_SYS_TSS      equ     1
DT_SYS_LDT      equ     2
DT_SYS_TSS_BUSY equ     3
; 4-7 = control descriptors
; 8-F = reserved by Intel

; ---------------------------------------------------------------------
; Macro that defines a system descriptor.
; 'Type' should be one of the DT_SYS_* constants.
DESC_SYSTEM     %MACRO  Base, Limit, Type, Present=1, Dpl=0
%:              DESC_GENERIC    %Base, %Limit, \
                        (%Present<<7) | (%Dpl<<5) | 00h | %Type
%ENDMACRO       DESC_SYSTEM

; ---------------------------------------------------------------------
; Macro that defines a code segment descriptor.
DESC_CODE       %MACRO  Base, Limit, Present=1, Dpl=0, Conforming=0, Readable=1, Accessed=1
%:              DESC_GENERIC    %Base, %Limit, \
                        (%Present<<7) | (%Dpl<<5) | 18h | (%Conforming<<2) | (%Readable<<1) | (%Accessed<<0)
%ENDMACRO       DESC_CODE

; ---------------------------------------------------------------------
; Macro that defiens a data segment descriptor.
DESC_DATA       %Macro  Base, Limit, Present=1, Dpl=0, ExpandDown=0, Writable=1, Accessed=1
%:              DESC_GENERIC    %Base, %Limit, \
                        (%Present<<7) | (%Dpl<<5) | 10h | (%ExpandDown<<2) | (%Writable<<1) | (%Accessed<<0)
%ENDMACRO       DESC_DATA

