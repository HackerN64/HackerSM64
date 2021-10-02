# assembler directives
.set gp=64

.include "macros.inc"


.section .text, "ax"

.balign 32
glabel mtxf_to_mtx_asm
    lwc1 $f6, gWorldScale
    li.s $f8, 1.0
    li $v0, 1
    li.s $f4, 65536.0
    # flags will never update in this loop, so do the compare only once
    # if this ever breaks contact someone2639, and then maybe MIPS Technologies, Inc.
    c.eq.s $f6, $f8
1:
    lwc1 $f0, ($a1)
    lwc1 $f2, 0x04($a1)
    bc1f 3f
    nop
2:
    andi $t0, $v0, (1 << 1)
    mul.s $f0, $f4
    trunc.w.s $f0, $f0
    mfc1 $t3, $f0
    addiu $a1, 8
    sra $t4, $t3, 16
    sh $t4, 0x00($a0)
    sh $t3, 0x20($a0)

    addiu $v0, 2
    bnez $t0, storezero

    mul.s $f2, $f4
    trunc.w.s $f2, $f2
    mfc1 $t3, $f2
    sra $t4, $t3, 16
    sh $t4, 0x02($a0)
    sh $t3, 0x22($a0)
loopend:
.set noreorder
    bnel $v0, 0x11, 1b
    addiu $a0, 4
.set reorder

    li $t1, 1
    sh $t1, 0x02($a0)
    jr $ra
storezero:
    sh $zero, 0x02($a0)
    sh $zero, 0x22($a0)
    j loopend
3:
    div.s $f0, $f6
    div.s $f2, $f6
    b 2b

