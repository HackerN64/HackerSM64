#pragma once

#include <ultra64.h>

#include "types.h"
// #include "crash_main.h"


typedef union {
    struct PACKED {
        u8 up    : 1;
        u8 down  : 1;
        u8 left  : 1;
        u8 right : 1;
        u8       : 4;
    } pressed;
    struct PACKED {
        u8       : 4;
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
#ifdef INCLUDE_DEBUG_MAP
    CONT_DESC_TOGGLE_FUNCTIONS,
#endif
    CONT_DESC_CYCLE_FLOATS_MODE,
    CONT_DESC_CHANGE_SETTING,
    CONT_DESC_RESET_SETTING,
    NUM_CONT_DESC,
};

struct ControlType {
    /*0x00*/ const char* control;
    /*0x04*/ const char* description;
}; /*0x08*/


extern _Bool gCSSwitchedPage;
extern _Bool gCSDrawControls;
extern CrashScreenDirections gCSDirectionFlags;
extern struct CSController* const gCSCompositeController;
extern const struct ControlType gCSControlDescriptions[];
extern const enum ControlTypes defaultContList[];


void crash_screen_set_page(enum CrashScreenPages page);
u32 clamp_view_to_selection(u32 scrollIndex, u32 selectIndex, const u32 numRows, const u32 step);
void crash_screen_update_input(void);
void draw_controls_box(void);
