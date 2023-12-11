// Used as part of emulator detection. The dynarec core of PJ64 4.0 only updates
// the COUNT register in between recompiled functions, so this will return 0 on
// PJ64 4.0 dynarec, but will instead return the count factor on other versions.

.include "macros.inc"
.section .text, "ax"
.set noreorder

.balign 32
glabel pj64_get_count_factor_asm
mfc0 $t0, $9 // $9 is the COUNT register
mfc0 $t1, $9 
jr $ra
subu $v0, $t1, $t0
