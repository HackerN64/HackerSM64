#pragma once
#include <PR/os_cont.h>

/* Demo Macros */

.macro stick x, y
    .byte \x, \y
.endm

.macro end_demo
    .half 0
    .byte 0, 0
    .half 0
    .half 0
.endm

.macro for holdcount_frames
    .half \holdcount_frames
.endm

/* purely for legibility */
#define frames

.macro press buttonMask
    .half \buttonMask
    .half 0
.endm

/* Pretty names to save string buffer space in RAM */
#define A A_BUTTON
#define B B_BUTTON
#define Z Z_TRIG
#define Start START_BUTTON
#define L L_TRIG
#define R R_TRIG

#define C_Up U_CBUTTONS
#define C_Down D_CBUTTONS
#define C_Left L_CBUTTONS
#define C_Right R_CBUTTONS

/* Macro for no button */
#define _ 0
