// Detect if the emux emulator extension is supported
// https://hackmd.io/@rasky/r1k7na6Jn

.include "macros.inc"
.section .text, "ax"
.set noreorder

glabel emux_detect
or $v0, $0, $0
jr $ra
tne $v0, $v0, 0x0
