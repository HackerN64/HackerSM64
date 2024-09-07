#pragma once
#include <PR/os_cont.h>

/* Demo Macros */

.macro set_rng seed
    .byte 0, 0
    .half \seed
.endm

.macro yaw y
    .half \y
.endm

.macro end_demo
    .word 0, 0, 0
.endm

.macro for holdcount_frames
    .byte \holdcount_frames
.endm

/* purely for legibility */
#define frames

.macro mag m
    .float \m
.endm

.macro stick x, y
    .byte \x, \y
.endm

.macro press b
    .half \b
.endm

/* Button macros are shifted weirdly */
#define A A_BUTTON
#define B B_BUTTON
#define Z Z_TRIG
#define Start START_BUTTON

/* Might as well set pretty names for C buttons too */
#define C_Up U_CBUTTONS
#define C_Down D_CBUTTONS
#define C_Left L_CBUTTONS
#define C_Right R_CBUTTONS

/* Macro for no button */
#define _ 0
