
Explicit segment override
=========================

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

