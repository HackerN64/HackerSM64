// assembler directives
.set noat      // allow manual use of $at
.set noreorder // don't insert nops after branches
.set gp=64

#include "macros.inc"


.section .text, "ax"

glabel entry_point

entry_point:
    lui   $t0, %hi(_mainSegmentBssStart)
    lui   $t1, %hi(_mainSegmentBssSize)
    addiu $t0, %lo(_mainSegmentBssStart)
    addiu $t1, %lo(_mainSegmentBssSize)
.clear_bytes:
    addi  $t1, $t1, -8 # Subtract 8 bytes from the amount remaining
    sw    $zero, ($t0)  # Clear 4 bytes
    sw    $zero, 4($t0) # Clear the next 4 bytes
    bnez  $t1, .clear_bytes # Continue clearing until clear_bytes is 0
     addi  $t0, $t0, 8 # Increment the address of bytes to clear
    lui   $t2, %hi(main_func) # Get the high half of the init function address
    lui   $sp, %hi(gIdleThreadStack) # Set the high half of the stack pointer to that of the idle thread stack
    addiu $t2, %lo(main_func) # Get the low half of the init function address
    jr    $t2 # Jump to the init function
     addiu $sp, %lo(gIdleThreadStack) # Set the low half of the stack pointer to that of the idle thread stack
