#include <ultra64.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen.h"
#include "game/game_input.h"
#include "crash_controls.h"
#include "pages/stack_trace.h"


_Bool gCSSwitchedPage = FALSE;
_Bool gCSDrawControls = FALSE;

CrashScreenDirections gCSDirectionFlags;

static OSTime sCSInputTimeY = 0;
static OSTime sCSInputTimeX = 0;


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

const struct ControlType gCSControlDescriptions[] = {
    [CONT_DESC_SWITCH_PAGE      ] = { .control = STR_L"/"STR_R,                             .description = "switch page"                          },
    [CONT_DESC_SHOW_CONTROLS    ] = { .control = STR_START,                                 .description = "show/hide page controls"              },
    [CONT_DESC_CYCLE_DRAW       ] = { .control = STR_Z,                                     .description = "cycle drawing overlay and background" },
    [CONT_DESC_SCROLL_LIST      ] = { .control = STR_UP"/"STR_DOWN,                         .description = "scroll list"                          },
    [CONT_DESC_CURSOR           ] = { .control = STR_UP"/"STR_DOWN"/"STR_LEFT"/"STR_RIGHT,  .description = "move cursor"                          },
    [CONT_DESC_CURSOR_VERTICAL  ] = { .control = STR_UP"/"STR_DOWN,                         .description = "move cursor"                          },
    [CONT_DESC_CURSOR_HORIZONTAL] = { .control = STR_LEFT"/"STR_RIGHT,                      .description = "move cursor"                          },
    [CONT_DESC_JUMP_TO_ADDRESS  ] = { .control = STR_A,                                     .description = "jump to specific address"             },
    [CONT_DESC_TOGGLE_ASCII     ] = { .control = STR_B,                                     .description = "toggle bytes as hex or ascii"         },
    [CONT_DESC_TOGGLE_UNKNOWNS  ] = { .control = STR_A,                                     .description = "toggle unknowns in list"              },
    [CONT_DESC_TOGGLE_FUNCTIONS ] = { .control = STR_B,                                     .description = "toggle function names"                },
};


void update_crash_screen_direction_input(void) {
    OSTime currTime = osGetTime();

    gCSDirectionFlags.pressed.up    = FALSE;
    gCSDirectionFlags.pressed.down  = FALSE;
    gCSDirectionFlags.pressed.left  = FALSE;
    gCSDirectionFlags.pressed.right = FALSE;

    s16 rawStickX  = gPlayer1Controller->rawStickX;
    s16 rawStickY  = gPlayer1Controller->rawStickY;
    u16 buttonDown = gPlayer1Controller->buttonDown;

    _Bool up    = ((buttonDown & (U_CBUTTONS | U_JPAD)) || (rawStickY >  60));
    _Bool down  = ((buttonDown & (D_CBUTTONS | D_JPAD)) || (rawStickY < -60));
    _Bool left  = ((buttonDown & (L_CBUTTONS | L_JPAD)) || (rawStickX < -60));
    _Bool right = ((buttonDown & (R_CBUTTONS | R_JPAD)) || (rawStickX >  60));

    if (up ^ down) {
        if (
            !(
                gCSDirectionFlags.held.up ||
                gCSDirectionFlags.held.down
            )
        ) { // prev Y
            // On press
            sCSInputTimeY = currTime;
            gCSDirectionFlags.pressed.up   = up;
            gCSDirectionFlags.pressed.down = down;
        } else {
            // held
            OSTime diff = (currTime - sCSInputTimeY);
            if (diff > FRAMES_TO_CYCLES(10)) {
                gCSDirectionFlags.pressed.up   = up;
                gCSDirectionFlags.pressed.down = down;
            }
        }
    }

    if (left ^ right) {
        if (
            !(
                gCSDirectionFlags.held.left ||
                gCSDirectionFlags.held.right
            )
        ) { // prev X
            // On press
            sCSInputTimeX = currTime;
            gCSDirectionFlags.pressed.left  = left;
            gCSDirectionFlags.pressed.right = right;
        } else {
            // held
            OSTime diff = (currTime - sCSInputTimeX);
            if (diff > FRAMES_TO_CYCLES(10)) {
                gCSDirectionFlags.pressed.left  = left;
                gCSDirectionFlags.pressed.right = right;
            }
        }
    }

    gCSDirectionFlags.held.up    = up;
    gCSDirectionFlags.held.down  = down;
    gCSDirectionFlags.held.left  = left;
    gCSDirectionFlags.held.right = right;
}

void clamp_view_to_selection(const u32 numRows, const u32 step) {
    const size_t size = (numRows * step);

    gScrollAddress = CLAMP(gScrollAddress, (gSelectedAddress - (size - 1)), (gSelectedAddress - (step - 1)));
    gScrollAddress = CLAMP(gScrollAddress, VALID_RAM_START, (VALID_RAM_END - size));
    gScrollAddress = ALIGN(gScrollAddress, step);
}

const enum ControlTypes defaultPageControls[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_LIST_END,
};

_Bool update_crash_screen_page(void) {
    enum CrashScreenPages prevPage = gCSPageID;

    gCSSwitchedPage = FALSE;

    u16 buttonPressed = gPlayer1Controller->buttonPressed;

    if (buttonPressed & L_TRIG) {
        gCSPageID--; // Previous Page.
    }
    if (buttonPressed & R_TRIG) {
        gCSPageID++; // Next page.
    }

    if (gCSPageID == prevPage) {
        return FALSE;
    }

    // Wrap pages.
    if (gCSPageID > MAX_PAGES) {
        gCSPageID = (NUM_PAGES - 1);
    }
    if (gCSPageID >= NUM_PAGES) {
        gCSPageID = FIRST_PAGE;
    }

    // Reset certain values when the page is changed.
    gStackTraceIndex = 0;
    gCSDrawControls = FALSE;
    gCSSwitchedPage = TRUE;

    return TRUE;
}

void crash_screen_update_input(void) {
    if (gControllerBits) {
#ifdef ENABLE_RUMBLE
        block_until_rumble_pak_free();
#endif
        osContStartReadDataEx(&gSIEventMesgQueue);
    }
    extern void read_controller_inputs(s32 threadID);
    read_controller_inputs(gActiveCSThreadInfo->thread.id);

    u16 buttonPressed = gPlayer1Controller->buttonPressed;

    // Global controls.
    if (buttonPressed & Z_TRIG) {
        gCSDrawCrashScreen ^= TRUE;
        if (gCSDrawCrashScreen) {
            gCSDrawSavedScreenshot ^= TRUE;
        } else if (!gCSDrawSavedScreenshot) {
            gCSDrawCrashScreen = TRUE;
            gCSDrawSavedScreenshot = TRUE;
            gCSDrawControls = FALSE;
        }
    }

    if (gCSDrawCrashScreen && (buttonPressed & START_BUTTON)) {
        gCSDrawControls ^= TRUE;
    }

    if (!gCSDrawCrashScreen) {
        return;
    }

    update_crash_screen_direction_input();

    if (gCSDrawControls) {
        return;
    }

    if (gAddressSelectMenuOpen) {
        crash_screen_select_address();
        return;
    }

    struct CSPage* page = &gCSPages[gCSPageID];
    
    if (update_crash_screen_page()) {
        page = &gCSPages[gCSPageID];
        if (page->initFunc != NULL && !page->flags.initialized) {
            page->initFunc();
            page->flags.initialized = TRUE;
        }
        return;
    }

    // Run the page-specific input function.
    if (page->inputFunc != NULL && !page->flags.crashed) {
        page->inputFunc();
    }
}

void draw_controls_box(void) {
    crash_screen_draw_dark_rect(
        (CRASH_SCREEN_X1 + (TEXT_WIDTH(1) / 2)), (CRASH_SCREEN_Y1 + (TEXT_HEIGHT(1) / 2)),
        (CRASH_SCREEN_W  -  TEXT_WIDTH(1)     ), (CRASH_SCREEN_H  -  TEXT_HEIGHT(1)     ),
        CS_DARKEN_SEVEN_EIGHTHS
    );
    struct CSPage* page = &gCSPages[gCSPageID];

    // "[page name] PAGE CONTROLS"
    crash_screen_print(TEXT_X(1), TEXT_Y(1), STR_COLOR_PREFIX"%s PAGE CONTROLS", COLOR_RGBA32_CRASH_PAGE_NAME, page->name);

    const enum ControlTypes* list = page->contList;
    const struct ControlType* desc = NULL;

    u32 line = 3;

    while (*list != CONT_DESC_LIST_END) {
        desc = &gCSControlDescriptions[*list++];
        // [control]
        // [description]
        crash_screen_print(TEXT_X(2), TEXT_Y(line), "%s:\n "STR_COLOR_PREFIX"%s", desc->control, COLOR_RGBA32_CRASH_CONTROLS, desc->description);
        line += 2;
    }

    osWritebackDCacheAll();
}