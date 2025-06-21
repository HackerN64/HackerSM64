.include "macros.inc"
#include "src/debugger/assert.h"

.section .data
glabel __n64Assert_Filename
.skip 4
glabel __n64Assert_LineNum
.skip 4
glabel __n64Assert_Condition
.skip 4
glabel __n64Assert_MessageBuf
.skip ASSERT_MESGBUF_SIZE

.section .text

glabel __n64Assert
sw $a0, __n64Assert_Filename
sw $a1, __n64Assert_LineNum
sw $a2, __n64Assert_Condition
syscall
nop

