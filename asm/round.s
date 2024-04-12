.include "macros.inc"
.section .text
.set noreorder

.set abi_arg0, $f12

glabel roundf
round.w.s $f0, abi_arg0
jr $ra
 mfc1 $v0, $f0
