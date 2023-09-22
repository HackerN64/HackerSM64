.include "macros.inc"
.section .text, "ax"
.set noreorder
.set noat

glabel fcr_get_rounding_mode
cfc1 $v0, $31
jr $ra
andi $v0, 0x3

glabel fcr_set_rounding_mode
cfc1 $t0, $31
xor $at, $t0, $a0
andi $at, $at, 0x3
xor $at, $at, $t0
jr $ra
ctc1 $at, $31

glabel fcr_get_enabled_exceptions
cfc1 $v0, $31
srl $v0, $v0, 7
jr $ra
andi $v0, $v0, 0x1f

glabel fcr_set_enabled_exceptions
cfc1 $t0, $31
sll $a0, $a0, 7
xor $at, $t0, $a0
andi $at, $at, 0xf80
xor $at, $at, $t0
jr $ra
ctc1 $at, $31

glabel fcr_enable_exceptions
cfc1 $at, $31
andi $a0, $a0, 0x1f
sll $a0, $a0, 7
or $at, $at, $a0
jr $ra
ctc1 $at, $31

glabel fcr_disable_exceptions
cfc1 $at, $31
andi $a0, $a0, 0x1f
sll $a0, $a0, 7
or $at, $at, $a0
subu $at, $at, $a0
jr $ra
ctc1 $at, $31

glabel fcr_get_cause
cfc1 $v0, $31
srl $v0, $v0, 12
jr $ra
andi $v0, $v0, 0x3f

glabel fcr_get_flags
cfc1 $v0, $31
srl $v0, $v0, 2
jr $ra
andi $v0, $v0, 0x1f

glabel fcr_clear_flags
cfc1 $t0, $31
xor $at, $t0, $0
andi $at, $at, 0x7c
xor $at, $at, $t0
jr $ra
ctc1 $at, $31
