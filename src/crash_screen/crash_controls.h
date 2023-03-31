#pragma once

#include <ultra64.h>

#include "types.h"
#include "crash_screen.h"

typedef union {
    struct PACKED {
        u8 up    : 1;
        u8 down  : 1;
        u8 left  : 1;
        u8 right : 1;
        u8 : 4;
    } pressed;
    struct PACKED {
        u8 : 4;
        u8 up    : 1;
        u8 down  : 1;
        u8 left  : 1;
        u8 right : 1;
    } held;
    u8 raw;
} CrashScreenDirections;

enum ControlTypes {
    CONT_DESC_LIST_END = -1,
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_SCROLL_LIST,
    CONT_DESC_CURSOR,
    CONT_DESC_CURSOR_VERTICAL,
    CONT_DESC_CURSOR_HORIZONTAL,
    CONT_DESC_JUMP_TO_ADDRESS,
    CONT_DESC_TOGGLE_ASCII,
    CONT_DESC_TOGGLE_FUNCTIONS,
    CONT_DESC_TOGGLE_UNKNOWNS,
    NUM_CONT_DESC,
};

struct ControlType {
    /*0x00*/ const char *control;
    /*0x04*/ const char *description;
}; /*0x08*/

extern CrashScreenDirections gCrashScreenDirectionFlags;

extern const struct ControlType gCrashControlsDescriptions[];

extern const enum ControlTypes defaultPageControls[];

void crash_screen_input_default(void);
void update_crash_screen_input(void);
void draw_controls_box(void);
