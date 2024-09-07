#include <string.h>
#include "types.h"
#include "segments.h"

#include "demo_system.h"
#include "game_init.h"
#include "level_update.h"
#include "memory.h"
#include "save_file.h"

void *demoInputsMalloc = NULL;
u32 gCurrentDemoSize = 0;
u32 gCurrentDemoIdx = 0;
struct DemoFile gDemos[LEVEL_COUNT] ALIGNED8;
static u16 sDemoCountdown = 0;
u16 gDemoLevel = 0;

u8 player_action_reads_stick(struct MarioState *m) {
    if (m->action & (ACT_FLAG_SWIMMING | ACT_FLAG_ON_POLE)) {
        return TRUE;
    }
    return FALSE;
}

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

void print_demo_input(struct DemoInput *d) {
    char buttonStr[20];
    char *buttonPtr = buttonStr;

    if (d->timer == 0) {
        osSyncPrintf("end_demo\n");
        return;
    }

    if (d->buttonMask == 0) {
        sprintf(buttonStr, "_");
    } else {
        u16 button = d->buttonMask;

        if (button & A_BUTTON) {
            buttonPtr += sprintf(buttonPtr, "A | ");
        }
        if (button & B_BUTTON) {
            buttonPtr += sprintf(buttonPtr, "B | ");
        }
        if (button & Z_TRIG) {
            buttonPtr += sprintf(buttonPtr, "Z | ");
        }
        if (button & START_BUTTON) {
            buttonPtr += sprintf(buttonPtr, "Start | ");
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

        u32 len = strlen(buttonStr);
        buttonStr[len - 3] = 0; // Remove the trailing ' | '
    }

    char text[100];

    if (player_action_reads_stick(gMarioState)) {
        sprintf(text, "for %3d frames;  mag %2f;  stick %3d, %3d;  press %s\n",
            d->timer,
            d->stickMag,
            gPlayer1Controller->rawStickX,
            gPlayer1Controller->rawStickY,
            buttonStr
        );
    } else {
        sprintf(text, "for %3d frames;  mag %2f;  yaw %6d;  press %s\n",
            d->timer,
            d->stickMag,
            d->stickYaw,
            buttonStr
        );
    }
    osSyncPrintf(text);
}
// this function records distinct inputs over a 255-frame interval to RAM locations and was likely
// used to record the demo sequences seen in the final game. This function is unused.
void record_demo(void) {
    if (gMarioState == NULL) return;
    // record the player's button mask and current rawStickX and rawStickY.
    u16 buttonMask = gPlayer1Controller->buttonDown;
    s16 intendedYaw = gMarioState->intendedYaw;
    f32 stickMag = gMarioState->intendedMag;

    // Rrecord the distinct input and timer so long as they are unique.
    // If the timer hits 0xFF, reset the timer for the next demo input.
    if (gRecordedDemoInput.timer == 0xFF || buttonMask != gRecordedDemoInput.buttonMask
        || intendedYaw != gRecordedDemoInput.stickYaw || stickMag != gRecordedDemoInput.stickMag) {
        print_demo_input(&gRecordedDemoInput);
        gRecordedDemoInput.timer = 0;
        gRecordedDemoInput.buttonMask = buttonMask;
        gRecordedDemoInput.stickYaw = intendedYaw;
        gRecordedDemoInput.stickMag = stickMag;
    }
    gRecordedDemoInput.timer++;
}

/**
 * If a demo sequence exists, this will run the demo input list until it is complete.
 */
void run_demo_inputs(void) {
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
        } else {
            // Backup the start button if it is pressed, since we don't want the
            // demo input to override the mask where start may have been pressed.
            u16 startPushed = (gPlayer1Controller->controllerData->button & START_BUTTON);

            // Perform the demo inputs by assigning the current button mask and the stick inputs.
            if (player_action_reads_stick(gMarioState)) {
                gPlayer1Controller->controllerData->stick_x = gCurrDemoInput->stickPos[0];
                gPlayer1Controller->controllerData->stick_y = gCurrDemoInput->stickPos[1];
            }

            // To assign the demo input, the button information is stored in
            // an 8-bit mask rather than a 16-bit mask. this is because only
            // A, B, Z, Start, and the C-Buttons are used in a demo, as bits
            // in that order. In order to assign the mask, we need to take the
            // upper 4 bits (A, B, Z, and Start) and shift then left by 8 to
            // match the correct input mask. We then add this to the masked
            // lower 4 bits to get the correct button mask.
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
                // print_demo_input(gCurrDemoInput);
            }
        }
    }
}

/**
 * Run the demo timer on the PRESS START screen after a number of frames.
 * This function returns the level ID from the first byte of a demo file.
 * It also returns the level ID from intro_regular (file select or level select menu)
 */
s32 run_level_id_or_demo(s32 level) {
    gCurrDemoInput = NULL;

    if (level == LEVEL_NONE) {
        if (!gPlayer1Controller->buttonDown && !gPlayer1Controller->stickMag) {
            // start the demo. 800 frames has passed while
            // player is idle on PRESS START screen.
            if ((++sDemoCountdown) == PRESS_START_DEMO_TIMER) {
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

                gCurrentDemoSize = (u32) gDemos[gDemoLevel].romEnd - (u32) gDemos[gDemoLevel].romStart;
                gCurrentDemoIdx = 0;
                dma_new_demo_data();

                struct DemoInput *demoBank = get_segment_base_addr(SEGMENT_DEMO_INPUTS);

                // Point the current input to the demo segment
                gCurrDemoInput = demoBank;
                level = gDemoLevel + 1;
                gCurrSaveFileNum = 1;
                gCurrActNum = 1;
            }
        } else { // activity was detected, so reset the demo countdown.
            sDemoCountdown = 0;
        }
    }
    return level;
}
