#include <string.h>
#include "types.h"
#include "segments.h"

#include "config/config_debug.h"
#include "engine/math_util.h"
#include "demo_system.h"
#include "game_init.h"
#include "level_update.h"
#include "memory.h"
#include "save_file.h"

#ifdef ENABLE_DEMO_SYSTEM
void *demoInputsMalloc = NULL;
u32 gCurrentDemoSize = 0;
u32 gCurrentDemoIdx = 0;
struct DemoFile gDemos[LEVEL_COUNT] ALIGNED8;
static u16 sDemoCountdown = 0;
u16 gDemoLevel = 0;
u16 gFinalDemoLevel = 0;
u8 gDemoActive = FALSE;

void dma_new_demo_data() {
    void *demoBank = get_segment_base_addr(SEGMENT_DEMO_INPUTS);

    u8 *romStart = gDemos[gDemoLevel].romStart + (sizeof(struct DemoInput) * gCurrentDemoIdx);
    u8 *romEnd;
    if (gCurrentDemoIdx + DEMO_BANK_INPUT_CAPACITY > gCurrentDemoSize) {
        romEnd = gDemos[gDemoLevel].romEnd;
    }
    else {
        romEnd = romStart + DEMO_INPUTS_POOL_SIZE;
    }

    dma_read(demoBank, romStart, romEnd);
}

/**
 * If a demo sequence exists, this will run the demo input list until it is complete.
 */
void run_demo_inputs(void) {
    if (gDemoActive == FALSE) return;
    // Eliminate the unused bits.
    gPlayer1Controller->controllerData->button &= VALID_BUTTONS;

    // Check if a demo inputs list exists and if so,
    // run the active demo input list.
    if (gCurrDemoInput != NULL) {
        // The timer variable being 0 at the current input means the demo is over.
        // Set the button to the END_DEMO mask to end the demo.
        if (gCurrDemoInput->timer == 0) {
            gPlayer1Controller->controllerData->stick_x = 0;
            gPlayer1Controller->controllerData->stick_y = 0;
            gPlayer1Controller->controllerData->button = END_DEMO;
            gDemoActive = FALSE;
        } else {
            // Backup the start button if it is pressed, since we don't want the
            // demo input to override the mask where start may have been pressed.
            u16 startPushed = (gPlayer1Controller->controllerData->button & START_BUTTON);

            // Perform the demo inputs by assigning the current button mask and the stick inputs.
            gPlayer1Controller->controllerData->stick_x = gCurrDemoInput->stickX;
            gPlayer1Controller->controllerData->stick_y = gCurrDemoInput->stickY;

            // FEATURE
            // In Vanilla SM64, only a limited amount of buttons are recorded,
            //  so that it fits in 8 bytes instead of 16. This saves ROM space.
            // We do not care about this, since we are not constrained by ROM size
            //  Additionally, demos are simply not included in the final build if not used.
            gPlayer1Controller->controllerData->button = gCurrDemoInput->buttonMask;

            // If start was pushed, put it into the demo sequence being input to end the demo.
            gPlayer1Controller->controllerData->button |= startPushed;

            // Run the current demo input's timer down. if it hits 0, advance the demo input list.
            if (--gCurrDemoInput->timer == 0) {
                struct DemoInput *demoBank = get_segment_base_addr(SEGMENT_DEMO_INPUTS);
                u8 needs_dma = (gCurrDemoInput == &demoBank[DEMO_BANK_INPUT_CAPACITY - 1]);
                gCurrDemoInput++;
                gCurrentDemoIdx++;
                if (needs_dma) {
                    dma_new_demo_data();
                    gCurrDemoInput = demoBank;
                }
            }
        }
    }
}
#endif // ENABLE_DEMO_SYSTEM

/**
 * If level is a valid value, tell the level script up the chain to jump there.
 * If level is LEVEL_NONE, do nothing unless a demo is ready to play.
 */
s32 run_level_id_or_demo(s32 level) {
    gCurrDemoInput = NULL;

    if (level == LEVEL_NONE) {
#ifdef ENABLE_DEMO_SYSTEM
        if (!gPlayer1Controller->buttonDown && !gPlayer1Controller->stickMag) {
            if ((++sDemoCountdown) >= PRESS_START_DEMO_TIMER) {
                u32 demoCount = 0;

                // DMA in the Level Demo List
                // Should always DMA in (LEVEL_COUNT * 8) bytes
                dma_read((u8 *) &gDemos, demoFile, demoFileEnd);

                // Find a non-null demo in the list
                // (If a demo played already, increment first before checking)
                do {
                    if (gDemoLevel >= LEVEL_MAX) {
                        gDemoLevel = 0;
                    }
                    gDemoLevel++;
                    demoCount++;
                     // No demos installed in assets/demos/; continue playing the title screen
                    if (demoCount > (LEVEL_MAX * 2)) {
                        sDemoCountdown = 0;
                        return level;
                    }
                } while (gDemos[gDemoLevel].romStart == NULL);

                // After gFinalDemoLevel's demo is done playing, the intro splash should play
                // The vanilla functionality is to always assume PSS is the final demo.
                for (int i = 0; i < LEVEL_COUNT; i++) {
                    if (gDemos[i].romStart != NULL) {
                        gFinalDemoLevel = i + 1;
                    }
                }

                gCurrentDemoSize = (u32) gDemos[gDemoLevel].romEnd - (u32) gDemos[gDemoLevel].romStart;
                gCurrentDemoIdx = 0;
                dma_new_demo_data();
                struct DemoInput *demoBank = get_segment_base_addr(SEGMENT_DEMO_INPUTS);

                // Point the current input to the demo segment
                gCurrDemoInput = demoBank;
                level = gDemoLevel + 1;
                gCurrSaveFileNum = 1;
                gCurrActNum = 1;
                sDemoCountdown = 0;
            }
        } else { // activity was detected, so reset the demo countdown.
            sDemoCountdown = 0;
        }
#endif // ENABLE_DEMO_SYSTEM
    }
    return level;
}

#ifdef DEMO_RECORDING_MODE

static u32 sDemoInputCount = 0;
static u8 sDemoRecordingActive = FALSE;

void print_demo_input(struct DemoInput *d) {
    char buttonStr[100];
    char *buttonPtr = buttonStr;
    u16 button = d->buttonMask & ~(START_BUTTON);

    if (button == 0) {
        sprintf(buttonStr, "_");
    } else {

        if (button & A_BUTTON) {
            buttonPtr += sprintf(buttonPtr, "A | ");
        }
        if (button & B_BUTTON) {
            buttonPtr += sprintf(buttonPtr, "B | ");
        }
        if (button & L_TRIG) {
            buttonPtr += sprintf(buttonPtr, "L | ");
        }
        if (button & R_TRIG) {
            buttonPtr += sprintf(buttonPtr, "R | ");
        }
        if (button & Z_TRIG) {
            buttonPtr += sprintf(buttonPtr, "Z | ");
        }

        if (button & U_CBUTTONS) {
            buttonPtr += sprintf(buttonPtr, "C_Up | ");
        }
        if (button & D_CBUTTONS) {
            buttonPtr += sprintf(buttonPtr, "C_Down | ");
        }
        if (button & L_CBUTTONS) {
            buttonPtr += sprintf(buttonPtr, "C_Left | ");
        }
        if (button & R_CBUTTONS) {
            buttonPtr += sprintf(buttonPtr, "C_Right | ");
        }

        if (button & U_JPAD) {
            buttonPtr += sprintf(buttonPtr, "U_JPAD | ");
        }
        if (button & D_JPAD) {
            buttonPtr += sprintf(buttonPtr, "D_JPAD | ");
        }
        if (button & L_JPAD) {
            buttonPtr += sprintf(buttonPtr, "L_JPAD | ");
        }
        if (button & R_JPAD) {
            buttonPtr += sprintf(buttonPtr, "R_JPAD | ");
        }

        u32 len = strlen(buttonStr);
        buttonStr[len - 3] = 0; // Remove the trailing ' | '
    }

    char text[100];

    sprintf(text, "for %3d frames; stick %4d, %4d;  press %s\n",
        d->timer,
        d->stickX,
        d->stickY,
        buttonStr
    );
    osSyncPrintf(text);
}

// TODO: When libcart is merged, replace all these print functions
//       with file i/o that automatically saves the file to the SD Card.
void print_demo_header() {
    char header[500];
    sprintf(header, "#include \"demo_macros.inc\"\n \n");
    osSyncPrintf(header);
    sDemoRecordingActive = TRUE;
}

s32 print_demo_footer(UNUSED s32 arg) {
    char footer[300];

#define STUB_LEVEL(_0, _1, _2, _3, _4, _5, _6, _7, _8) "stub_level",
#define DEFINE_LEVEL(_0, _1, _2, filename, _4, _5, _6, _7, _8, _9, _10) #filename,
    // Level to String conversion for telling the player where to save the file
    static char sLevel2Str[LEVEL_COUNT][20] = {
        #include "levels/level_defines.h"
    };
#undef STUB_LEVEL
#undef DEFINE_LEVEL

    sprintf(footer, R"(
end_demo
/* Copy the above output to 'assets/demos/%s.s' */
)", sLevel2Str[TEST_LEVEL - 1]);
    osSyncPrintf(footer);
    sDemoRecordingActive = FALSE;
    return 0;
}

void record_demo() {
    // record the player's button mask and current rawStickX and rawStickY.
    u16 buttonMask = gPlayer1Controller->buttonDown;
    s8 stickX = gPlayer1Controller->rawStickX;
    s8 stickY = gPlayer1Controller->rawStickY;

    // Rrecord the distinct input and timer so long as they are unique.
    // If the timer hits 0xFFFF, reset the timer for the next demo input.
    if (gRecordedDemoInput.timer == 0xFFFF || buttonMask != gRecordedDemoInput.buttonMask
        || stickX != gRecordedDemoInput.stickX || stickY != gRecordedDemoInput.stickY) {
        if (sDemoInputCount == 0) {
            // Wait 4 frames in the demo so that the RNG lines up while recording and during playback.
            gRecordedDemoInput.timer += 4;
        }
        if (sDemoRecordingActive) {
            print_demo_input(&gRecordedDemoInput);
        }
        gRecordedDemoInput.timer = 0;
        gRecordedDemoInput.buttonMask = buttonMask;
        gRecordedDemoInput.stickX = stickX;
        gRecordedDemoInput.stickY = stickY;
        sDemoInputCount++;
    }
    gRecordedDemoInput.timer++;
}

#endif // DEMO_RECORDING_MODE

