// assembler directives
.set noat      // allow manual use of $at
.set noreorder // don't insert nops after branches
.set gp=64

#include "macros.inc"


.section .text, "ax"

glabel setgp
    lui $gp, %hi(_gp)
    jal $ra
     addiu $gp, %lo(_gp)
