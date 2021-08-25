# assembler directives
.set noat      # allow manual use of $at
.set noreorder # don't insert nops after branches
.set gp=64

.include "macros.inc"


.section .text, "ax"

# This file is handwritten.

glabel decompress
    lw    $a3, 8($a0)
    lw    $t9, 0xc($a0)
    lw    $t8, 4($a0)
    add   $a3, $a3, $a0
    add   $t9, $t9, $a0
    move  $a2, $zero
    addi  $a0, $a0, 0x10
    add   $t8, $t8, $a1
.L802772C0:
    bnel  $a2, $zero, .L802772D8
     slt   $t1, $t0, $zero
    lw    $t0, ($a0)
    li    $a2, 32
    addi  $a0, $a0, 4
    slt   $t1, $t0, $zero
.L802772D8:
    beql  $t1, $zero, .L802772F8
     lhu   $t2, ($a3)
    lb    $t2, ($t9)
    addi  $t9, $t9, 1
    addi  $a1, $a1, 1
    b     .L80277324
     sb    $t2, -1($a1)
    lhu   $t2, ($a3)
.L802772F8:
    addi  $a3, $a3, 2
    srl   $t3, $t2, 0xc
    andi  $t2, $t2, 0xfff
    sub   $t1, $a1, $t2
    addi  $t3, $t3, 3
.L8027730C:
    lb    $t2, -1($t1)
    addi  $t3, $t3, -1
    addi  $t1, $t1, 1
    addi  $a1, $a1, 1
    bnez  $t3, .L8027730C
     sb    $t2, -1($a1)
.L80277324:
    sll   $t0, $t0, 1
    bne   $a1, $t8, .L802772C0
     addi  $a2, $a2, -1
    jr    $ra
     nop