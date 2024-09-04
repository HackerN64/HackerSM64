#pragma once

/* Demo Macros */

.macro start_demo name
    .section .data
.endm

.macro end_demo name
    .byte 0, 0, 0, 0
.endm

.macro for holdcount_frames
    .byte \holdcount_frames
.endm

/* purely for legibility */
#define frames

.macro stick x, y
    .byte \x, \y
.endm

.macro press b
    .byte \b
.endm

/* Button macros are shifted weirdly */
#define A A_BUTTON >> 8
#define B B_BUTTON >> 8
#define Z Z_TRIG >> 8
#define Start START_BUTTON >> 8

/* Might as well set pretty names for C buttons too */
#define C_Up U_CBUTTONS
#define C_Down D_CBUTTONS
#define C_Left L_CBUTTONS
#define C_Right R_CBUTTONS

/* Macro for no button */
#define _ 0
