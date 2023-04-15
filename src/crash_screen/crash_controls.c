#include <ultra64.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen.h"
#include "game/game_init.h"
#include "crash_controls.h"
#include "pages/stack_trace.h"


_Bool gDrawControls = FALSE;

CrashScreenDirections gCrashScreenDirectionFlags;

static OSTime sCrashScreenInputTimeY = 0;
static OSTime sCrashScreenInputTimeX = 0;


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

const struct ControlType gCrashControlsDescriptions[] = {
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

    gCrashScreenDirectionFlags.pressed.up    = FALSE;
    gCrashScreenDirectionFlags.pressed.down  = FALSE;
    gCrashScreenDirectionFlags.pressed.left  = FALSE;
    gCrashScreenDirectionFlags.pressed.right = FALSE;

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
                gCrashScreenDirectionFlags.held.up ||
                gCrashScreenDirectionFlags.held.down
            )
        ) { // prev Y
            // On press
            sCrashScreenInputTimeY = currTime;
            gCrashScreenDirectionFlags.pressed.up   = up;
            gCrashScreenDirectionFlags.pressed.down = down;
        } else {
            // held
            OSTime diff = (currTime - sCrashScreenInputTimeY);
            if (diff > FRAMES_TO_CYCLES(10)) {
                gCrashScreenDirectionFlags.pressed.up   = up;
                gCrashScreenDirectionFlags.pressed.down = down;
            }
        }
    }

    if (left ^ right) {
        if (
            !(
                gCrashScreenDirectionFlags.held.left ||
                gCrashScreenDirectionFlags.held.right
            )
        ) { // prev X
            // On press
            sCrashScreenInputTimeX = currTime;
            gCrashScreenDirectionFlags.pressed.left  = left;
            gCrashScreenDirectionFlags.pressed.right = right;
        } else {
            // held
            OSTime diff = (currTime - sCrashScreenInputTimeX);
            if (diff > FRAMES_TO_CYCLES(10)) {
                gCrashScreenDirectionFlags.pressed.left  = left;
                gCrashScreenDirectionFlags.pressed.right = right;
            }
        }
    }

    gCrashScreenDirectionFlags.held.up    = up;
    gCrashScreenDirectionFlags.held.down  = down;
    gCrashScreenDirectionFlags.held.left  = left;
    gCrashScreenDirectionFlags.held.right = right;
}

const enum ControlTypes defaultPageControls[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_LIST_END,
};

_Bool update_crash_screen_page(void) {
    enum CrashScreenPages prevPage = gCrashPage;

    if (gPlayer1Controller->buttonPressed & L_TRIG) {
        gCrashPage--; // Previous Page.
        gCrashScreenUpdateFramebuffer = TRUE;
    }
    if (gPlayer1Controller->buttonPressed & R_TRIG) {
        gCrashPage++; // Next page.
        gCrashScreenUpdateFramebuffer = TRUE;
    }

    if (gCrashPage == prevPage) {
        return FALSE;
    }

    // Wrap pages.
    if (gCrashPage > MAX_PAGES) {
        gCrashPage = (NUM_PAGES - 1);
    }
    if (gCrashPage >= NUM_PAGES) {
        gCrashPage = FIRST_PAGE;
    }
    gCrashScreenUpdateFramebuffer = TRUE;

    // Reset certain values when the page is changed.
    gStackTraceIndex = 0;
    gDrawControls = FALSE;
    gCrashScreenSwitchedPage = TRUE;

    return TRUE;
}

void crash_screen_update_input(void) {
    // Global controls.
    if (gPlayer1Controller->buttonPressed & Z_TRIG) {
        gDrawCrashScreen ^= TRUE;
        if (gDrawCrashScreen) {
            gDrawBackground ^= TRUE;
        } else if (!gDrawBackground) {
            gDrawCrashScreen = TRUE;
            gDrawBackground = TRUE;
            gDrawControls = FALSE;
        }
        gCrashScreenUpdateFramebuffer = TRUE;
    }

    if (gDrawCrashScreen && (gPlayer1Controller->buttonPressed & START_BUTTON)) {
        gDrawControls ^= TRUE;
        gCrashScreenUpdateFramebuffer = TRUE;
    }

    if (!gDrawCrashScreen) {
        return;
    }

    update_crash_screen_direction_input();

    if (gDrawControls) {
        return;
    }

    if (gAddressSelectMenuOpen) {
        crash_screen_select_address();
        return;
    }
    
    if (update_crash_screen_page()) {
        if (gCrashScreenPages[gCrashPage].initFunc != NULL && !gCrashScreenPages[gCrashPage].flags.initialized) {
            gCrashScreenPages[gCrashPage].initFunc();
            gCrashScreenPages[gCrashPage].flags.initialized = TRUE;
        }
        return;
    }

    // Run the page-specific input function.
    if (gCrashScreenPages[gCrashPage].inputFunc != NULL && !gCrashScreenPages[gCrashPage].flags.skip) {
        gCrashScreenPages[gCrashPage].inputFunc();
    }
}

void draw_controls_box(void) {
    crash_screen_draw_dark_rect(
        (CRASH_SCREEN_X1 + (TEXT_WIDTH(1) / 2)), (CRASH_SCREEN_Y1 + (TEXT_HEIGHT(1) / 2)),
        (CRASH_SCREEN_W  -  TEXT_WIDTH(1)     ), (CRASH_SCREEN_H  -  TEXT_HEIGHT(1)     ),
        CS_DARKEN_SEVEN_EIGHTHS
    );
    // "[page name] PAGE CONTROLS"
    crash_screen_print(TEXT_X(1), TEXT_Y(1), STR_COLOR_PREFIX"%s %s", COLOR_RGBA32_CRASH_PAGE_NAME, gCrashScreenPages[gCrashPage].name, "PAGE CONTROLS");

    const enum ControlTypes* list = gCrashScreenPages[gCrashPage].pageControlsList;
    const struct ControlType* desc = NULL;

    u32 line = 3;

    while (*list != CONT_DESC_LIST_END) {
        desc = &gCrashControlsDescriptions[*list++];
        // [control]
        // [description]
        line += crash_screen_print(TEXT_X(2), TEXT_Y(line), "%s:\n "STR_COLOR_PREFIX"%s", desc->control, COLOR_RGBA32_CRASH_CONTROLS, desc->description);
    }

    osWritebackDCacheAll();
}
