#pragma once

#include <ultra64.h>

#include "types.h"

#include "game/input.h"

#include "crash_screen/cs_pages.h"
#include "crash_screen/cs_settings.h"


// Input string defines:
#define STR_A       "A"
#define STR_B       "B"
#define STR_Z       "Z"
#define STR_START   "START"
#define STR_UP      "UP"
#define STR_DOWN    "DOWN"
#define STR_LEFT    "LEFT"
#define STR_RIGHT   "RIGHT"
#define STR_L       "L"
#define STR_R       "R"


typedef enum CSSettingsGroup_controls {
    CS_OPT_HEADER_CONTROLS,
    CS_OPT_CONTROLS_CURSOR_WAIT_FRAMES,
    CS_OPT_CONTROLS_ANALOG_DEADZONE,
    CS_OPT_END_CONTROLS,
} CSSettingsGroup_controls;


typedef struct CSController {
    /*0x00*/ s16 rawStickX;
    /*0x02*/ s16 rawStickY;
    /*0x04*/ u16 buttonDown;
    /*0x06*/ u16 buttonPressed;
    /*0x08*/ u16 buttonReleased;
} CSController; /*0x0A*/


typedef union CrashScreenDirections {
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

typedef enum PACKED ControlTypes {
    CONT_DESC_LIST_END = -1,
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_PAGE_SELECT,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_HIDE_CRASH_SCREEN,
#ifdef UNF
    CONT_DESC_OS_PRINT,
#endif // UNF
    CONT_DESC_SCROLL_LIST,
    CONT_DESC_CURSOR,
    CONT_DESC_CURSOR_VERTICAL,
    CONT_DESC_CURSOR_HORIZONTAL,
    CONT_DESC_JUMP_TO_ADDRESS,
    CONT_DESC_SET_THREAD,
    CONT_DESC_TOGGLE_ASCII,
#ifdef INCLUDE_DEBUG_MAP
    CONT_DESC_TOGGLE_FUNCTIONS,
#endif // INCLUDE_DEBUG_MAP
    CONT_DESC_CYCLE_FLOATS_MODE,
    CONT_DESC_CHANGE_SETTING,
    CONT_DESC_RESET_SETTING,
    NUM_CONT_DESC,
} ControlTypes;

typedef struct ControlType {
    /*0x00*/ const char* control;
    /*0x04*/ const char* description;
} ControlType; /*0x08*/


extern struct CSSetting cs_settings_group_controls[];
extern const enum ControlTypes cs_cont_list_default[];

extern CrashScreenDirections gCSDirectionFlags;
extern CSController* const gCSCompositeController;
extern const ControlType gCSControlDescriptions[];

extern struct CSPopup gCSPopup_pages;
extern struct CSPopup gCSPopup_controls;


u32 cs_clamp_view_to_selection(u32 scrollIndex, u32 selectIndex, const u32 numRows, const u32 step);
_Bool can_switch_page(void);
#ifdef UNF
void cs_os_print_page(CSPage* page);
#else // !UNF
#define cs_os_print_page(page)
#endif // !UNF
void cs_update_input(void);
