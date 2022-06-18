GRiDCase 1520 BIOS Disassembly
==============================

This is a work-in-progress project to create a documented assembly listing of
the GRiDCase 1520 BIOS that can be assembled into a byte-identical image of the
original ROMs.

Current progress is ~30% complete.

Once completed, I hope to go back to the two earlier BIOS images that have been
dumped and figure out what the differences are.

This project is using EuroAssembler (<https://euroassembler.eu/eadoc/>),
MakePP (<http://makepp.sourceforge.net/>), and Perl 5.10.

References
==========

Some code comments are prefixed with a tag in square brackets.  These are
denote important compatibility constraints or restrictions on behaviour
imposed on the BIOS.  The possible tags are:

 * [TechRef]
   Refers to the GRiDCase 1500 Series Hardware Technical Reference.

 * [Public]
   Identifies locations/APIs mentioned in publicly available documentation,
   e.g. Ralph Brown's Interrupt List or IBM Technical Reference.

 * [Compat]
   Identifies places where data/code layout must remain fixed for compatibility
   with software that relies on undocumented or semi-documented functionality.

 * [ReadMe]
   Refers to the file README.txt in this project.

Why EuroAssembler?
==================

CODE=LONG instruction modifier
------------------------------

The x86 instruction set includes several different ways of encoding certain
instructions, usually those that encode a reg+reg/mem operand with a direction
flag.  For example:

	xchg	cx, dx

can be encoded as either 87CA or as 87D1.  Most assemblers arbitrarily choose
one variant (usually the numerically lowest as a side effect of how they are
coded).

While I don't know which assembler was used to create the GRiDCase 1520
BIOS, it seems to have consistently picked the numerically higher variant for
instructions that reference two register operands.

Unfortunately very few assemblers document how they assemble instructions with
multiple encodings, and those that do invariably choose the opposite way to
that needed to reproduce the GRiD BIOS.  EuroAssembler allows the programmer
to choose which encoding they want using the CODE modifier.  For this reason,
this project is written for EuroAssembler.  The syntax and behaviour of the
CODE instruction is documented at:

	<https://euroassembler.eu/eadoc/index.htm#CODEeq>

Because these instruction suffixes are quite lengthy and often spill over
into the comments column I have added some macros to make them less visually
intrusive (see src/macros.inc).  These are named after the instruction they
replace, suffixed with an underscore.  For example:

	xchg	cx, dx,CODE=LONG	; Verbose but obvious
	xchg_	cx, dx			; Less obvious but more readable

Explicit segment override
-------------------------

There is one problem I have found with EuroAssembler: it sometimes requires
segment overrides to be given as explicit prefixes instead of recognizing them
in the operand expression.

EuroASM allows you to include a segment override as part of a memory variable
"register expression" only.  It will not recognise one as part of a memory
variable "address expression".  That is, it will silently ignore a segment
override in an operand field unless the operand includes a register.  Some
places in the BIOS require a memory expression using a numeric expression
only -- for these few places EuroASM requires you to provide an instruction
prefix using the SEGCS/SEGSS/SEGDS/SEGES mnemonics.  E.g.

	mov	ax, [es:di]		; OK, assembles with prefix since a register is referenced
	mov	ax, [es:di+1234h]	; OK, still has register even with offset
	mov	ax, [es:1234h]		; Bad: no register referenced, segment override ignored
SEGES:	mov	ax, [1234h]		; Workaround: explicit segment override prefix

This is not explicitly called out in the documentation but can be inferrred
from these three sections:

 * Prefix field: <https://euroassembler.eu/eadoc/index.htm#StmentPrefix>
 * Register expressions: <https://euroassembler.eu/eadoc/index.htm#RegisterExpressions>
 * Address expressions: <https://euroassembler.eu/eadoc/index.htm#AddressExpressions>

