GRiDCase 1520 BIOS Disassembly
==============================

This is a work-in-progress project to create a documented assembly listing of
the GRiDCase 1520 BIOS that can be assembled into a byte-identical image of the
original ROMs.

As of commit 6275509ec2ae56762f64cc711beffe5b96b138b5 (2025-06-01) it builds a
bit-perfect image of the 1989-03-11 BIOS ROM.

As of commit 3fee0c9f57cc2062df9b08df6bdaba677943b058 (2025-07-29) it also
builds a bit-perfect image of the 1988-09-12 BIOS ROM.

Once documentation of this image is complete, I hope to go back to the two
earlier BIOS images that have been dumped and figure out what the differences
are.

This project is using EuroAssembler (<https://euroassembler.eu/eadoc/>),
MakePP (<http://makepp.sourceforge.net/>), and Perl 5.

Refer to BUILDING.txt for how to build the BIOS image.

References
==========

Some code comments are prefixed with a tag in square brackets.  These denote
important compatibility constraints or restrictions on behaviour imposed on the
BIOS, as well as references to GRiD documentation.  The possible tags are:

 * [TechRef]
   Refers to "GRiDCase 1500 Series Hardware Technical Reference Manual", October 1988.
   GRiD order number 1500-50.

 * [GridEprom]
   Refers to "Programming EPROMs for GRiDCase and Compass", October 1986.
   GRiD order number 031031-44.

 * [FE3001A manual]
   Refers to "FE3001A AT Clock Generation and Cycle Control Device".
   Western Digital advance information, 10/24/90.

 * [Public]
   Identifies locations/APIs mentioned in publicly available documentation,
   e.g. Ralph Brown's Interrupt List or IBM Technical Reference.

 * [Compat]
   Identifies places where data/code layout must remain fixed for compatibility
   with software that relies on undocumented or semi-documented functionality.

 * [UNDOC]
   Marks functionality or behaviour that is not documented in public documentation
   but may be used by non-BIOS code.

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

I don't know which assembler GRiD used to create the 1520 BIOS, but it seems to
have consistently picked the numerically higher variant for instructions that
reference two register operands.

Unfortunately very few assemblers document how they assemble instructions with
multiple encodings, and those that do invariably choose the opposite way to that
needed to reproduce the GRiD BIOS.  EuroAssembler allows the programmer to
choose which encoding they want on a per-instruction basis using the CODE
modifier.  For this reason, this project is written for EuroAssembler.  The
syntax and behaviour of the CODE instruction is documented at:

	<https://euroassembler.eu/eadoc/index.htm#CODEeq>

Because these instruction suffixes are quite lengthy and often spill over into
the comments column I have added some macros to make them less visually
intrusive (see src/macros.inc).  These are named after the instruction they
replace, suffixed with an underscore.  For example:

	xchg	cx, dx,CODE=LONG	; Verbose but obvious
	xchg_	cx, dx			; Less obvious but more readable

Explicit segment override
-------------------------

There is one problem I have found with EuroAssembler: it sometimes requires
segment overrides to be given as explicit prefixes instead of recognizing them
in the operand expression.

When using a segment override on a memory variable "address expression" only
on the target of a mov instruction, it will not recognise it.  That is, it will
silently ignore a segment override in a mov target operand field unless the
operand includes a register.  Some places in the BIOS require this -- for these
few places EuroASM requires you to provide an instruction prefix using the
SEGCS/SEGSS/SEGDS/SEGES mnemonics.  For example:

	mov	[es:di], ax		; OK, assembles with prefix since a register is referenced
	mov	[es:di+1234h], ax	; OK, still has register even with offset
	mov	[es:1234h], ax		; Bad: no register referenced, segment override ignored
SEGES:	mov	[1234h], ax		; Workaround: explicit segment override prefix

This used to also affect source operands as well and was fixed in euroasm.20240831.
