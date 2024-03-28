.include "macros.inc"

.section .data
glabel __n64Assert_Condition
.skip 4
glabel __n64Assert_Filename
.skip 4
glabel __n64Assert_LineNum
.skip 4
glabel __n64Assert_Message
.skip 4

.section .text

glabel __n64Assert
sw $a0, __n64Assert_Condition
sw $a1, __n64Assert_Filename
sw $a2, __n64Assert_LineNum
sw $a3, __n64Assert_Message
syscall
nop

