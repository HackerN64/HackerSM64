#include <PR/ultratypes.h>

#include "audio/external.h"
#include "engine/math_util.h"
#include "game/area.h"
#include "game/game_init.h"
#include "game/level_update.h"
#include "game/main.h"
#include "game/memory.h"
#include "game/print.h"
#include "game/save_file.h"
#include "game/sound_init.h"
#include "game/rumble_init.h"
#include "level_table.h"
#include "seq_ids.h"
#include "sm64.h"
#include "title_screen.h"

/**
 * @file title_screen.c
 * This file implements how title screen functions.
 * That includes playing demo sequences, introduction screens
 * and a level select used for testing purposes.
 */

#define STUB_LEVEL(textname, _1, _2, _3, _4, _5, _6, _7, _8) textname,
#define DEFINE_LEVEL(textname, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10) textname,

static char sLevelSelectStageNames[64][16] = {
    #include "levels/level_defines.h"
};
#undef STUB_LEVEL
#undef DEFINE_LEVEL

#ifdef KEEP_MARIO_HEAD
#ifndef DISABLE_DEMO
static u16 sDemoCountdown = 0;
struct DemoFile gDemos[LEVEL_COUNT] ALIGNED8;
#endif // DISABLE_DEMO
static s16 sPlayMarioGreeting = TRUE;
static s16 sPlayMarioGameOver = TRUE;


#ifndef DISABLE_DEMO

#define PRESS_START_DEMO_TIMER 800

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
                     // No demos installed in assets/demos/; continue playing the mario head
                    if (demoCount > (LEVEL_MAX * 2)) {
                        sDemoCountdown = 0;
                        return level;
                    }
                } while (gDemos[gDemoLevel].romStart == NULL);

                struct DemoInput *demoBank = (struct DemoInput *) get_segment_base_addr(SEGMENT_DEMO_INPUTS);
                dma_read((u8 *) demoBank, gDemos[gDemoLevel].romStart, gDemos[gDemoLevel].romEnd);

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
#endif // DISABLE_DEMO
#endif // KEEP_MARIO_HEAD


u8 gLevelSelectHoldKeyIndex = 0;
u8 gLevelSelectHoldKeyTimer = 0;

/**
 * Level select intro function, updates the selected stage
 * count if an input was received. signals the stage to be started
 * or the level select to be exited if start or the quit combo is pressed.
 */
s32 intro_level_select(void) {
    u32 index = 0;
    if (gPlayer1Controller->rawStickY < -60
        || gPlayer1Controller->rawStickX < -60
        || gPlayer1Controller->buttonDown & (D_CBUTTONS | D_JPAD | L_CBUTTONS | L_JPAD)
    ) {
            index++;
    }

    if (gPlayer1Controller->rawStickY > 60
        || gPlayer1Controller->rawStickX > 60
        || gPlayer1Controller->buttonDown & (U_CBUTTONS | U_JPAD | R_CBUTTONS | R_JPAD)
    ) {
            index += 2;
    }

    if (((index ^ gLevelSelectHoldKeyIndex) & index) == 2) {
        if (gCurrLevelNum > LEVEL_MAX) {
            gCurrLevelNum = LEVEL_MIN;
        } else if (gPlayer1Controller->buttonDown & B_BUTTON) {
            play_sound(SOUND_GENERAL_LEVEL_SELECT_CHANGE, gGlobalSoundSource);
            gCurrLevelNum += 10;
        } else {
            play_sound(SOUND_GENERAL_LEVEL_SELECT_CHANGE, gGlobalSoundSource);
            gCurrLevelNum++;
        }
    }

    if (((index ^ gLevelSelectHoldKeyIndex) & index) == 1) {
        if (gCurrLevelNum < LEVEL_MIN) {
            // Same applies to here as above
            gCurrLevelNum = LEVEL_MAX;
        } else if (gPlayer1Controller->buttonDown & B_BUTTON) {
            play_sound(SOUND_GENERAL_LEVEL_SELECT_CHANGE, gGlobalSoundSource);
            gCurrLevelNum -= 10;
        } else {
            play_sound(SOUND_GENERAL_LEVEL_SELECT_CHANGE, gGlobalSoundSource);
            gCurrLevelNum--;
        }
    }

    if (gLevelSelectHoldKeyTimer == 10) {
        gLevelSelectHoldKeyTimer = 8;
        gLevelSelectHoldKeyIndex = 0;
    } else {
        gLevelSelectHoldKeyTimer++;
        gLevelSelectHoldKeyIndex = index;
    }

    if ((index & 0x3) == 0) gLevelSelectHoldKeyTimer = 0;
    if (gCurrLevelNum > LEVEL_MAX) gCurrLevelNum = LEVEL_MIN; // exceeded max. set to min.
    if (gCurrLevelNum < LEVEL_MIN) gCurrLevelNum = LEVEL_MAX; // exceeded min. set to max.
    // Use file 4 and last act as a test
    gCurrSaveFileNum = 4;
    gCurrActNum = 6;

    print_text_aligned(160, 80, "SELECT STAGE", TEXT_ALIGN_CENTER);
    print_text_aligned(160, 30, "PRESS START BUTTON", TEXT_ALIGN_CENTER);
    print_text_fmt_int(40, 60, "%2d", gCurrLevelNum);
    print_text(80, 60, sLevelSelectStageNames[gCurrLevelNum - 1]); // print stage name

    // start being pressed signals the stage to be started. that is, unless...
    if (gPlayer1Controller->buttonPressed & (START_BUTTON | A_BUTTON)) {
        // ... the level select quit combo is being pressed, which uses START. If this
        // is the case, quit the menu instead.
        if (gPlayer1Controller->buttonDown == (Z_TRIG | START_BUTTON | L_CBUTTONS)) { // quit level select
            gDebugLevelSelect = FALSE;
            return LEVEL_RESTART_GAME;
        }
        play_sound(SOUND_MENU_STAR_SOUND, gGlobalSoundSource);
        return gCurrLevelNum;
    }
    return LEVEL_NONE;
}

#ifdef KEEP_MARIO_HEAD
/**
 * Regular intro function that handles Mario's greeting voice and game start.
 */
s32 intro_regular(void) {
    s32 level = LEVEL_NONE;

    // When the game stars, gGlobalTimer is less than 129 frames,
    // so Mario greets the player. After that, he will always say
    // "press start to play" when it goes back to the title screen
    // (using SAVE AND QUIT)
    if (sPlayMarioGreeting) {
        if (gGlobalTimer < 129) {
            play_sound(SOUND_MARIO_HELLO, gGlobalSoundSource);
        } else {
            play_sound(SOUND_MARIO_PRESS_START_TO_PLAY, gGlobalSoundSource);
        }
        sPlayMarioGreeting = FALSE;
    }
    print_intro_text();
#ifdef DEBUG_LEVEL_SELECT
    if (gPlayer1Controller->buttonDown & L_TRIG) {
        gDebugLevelSelect = TRUE;
    }
#endif // DEBUG_LEVEL_SELECT
    if (gPlayer1Controller->buttonPressed & START_BUTTON) {
        play_sound(SOUND_MENU_STAR_SOUND, gGlobalSoundSource);
#if ENABLE_RUMBLE
        queue_rumble_data(60, 70);
        queue_rumble_decay(1);
#endif // ENABLE_RUMBLE
        // calls level ID 100 (or 101 adding level select bool value)
        // defined in level_intro_mario_head_regular JUMP_IF commands
        // 100 is File Select - 101 is Level Select
        level = (LEVEL_FILE_SELECT + gDebugLevelSelect);
        sPlayMarioGreeting = TRUE;
    }
#if !defined(DISABLE_DEMO) && defined(KEEP_MARIO_HEAD)
    return run_level_id_or_demo(level);
#else
    return level;
#endif // DISABLE_DEMO && KEEP_MARIO_HEAD
}

/**
 * Game over intro function that handles Mario's game over voice and game start.
 */
s32 intro_game_over(void) {
    s32 level = LEVEL_NONE;

    if (sPlayMarioGameOver == TRUE) {
        play_sound(SOUND_MARIO_GAME_OVER, gGlobalSoundSource);
        sPlayMarioGameOver = FALSE;
    }

    print_intro_text();

    if (gPlayer1Controller->buttonPressed & START_BUTTON) {
        play_sound(SOUND_MENU_STAR_SOUND, gGlobalSoundSource);
#if ENABLE_RUMBLE
        queue_rumble_data(60, 70);
        queue_rumble_decay(1);
#endif // ENABLE_RUMBLE
        // same criteria as intro_regular
        level = LEVEL_FILE_SELECT + gDebugLevelSelect;
        sPlayMarioGameOver = TRUE;
    }
#if !defined(DISABLE_DEMO) && defined(KEEP_MARIO_HEAD)
    return run_level_id_or_demo(level);
#else
    return level;
#endif // DISABLE_DEMO && KEEP_MARIO_HEAD
}

#endif // KEEP_MARIO_HEAD

/**
 * Plays the casual "It's a me mario" when the game stars.
 */
s32 intro_play_its_a_me_mario(void) {
    play_sound(SOUND_MENU_COIN_ITS_A_ME_MARIO, gGlobalSoundSource);
    return LEVEL_NONE + 1;
}

/**
 * Update intro functions to handle title screen actions.
 * Returns a level ID after their criteria is met.
 */
s32 lvl_intro_update(s16 arg, UNUSED s32 unusedArg) {
    switch (arg) {
        case LVL_INTRO_PLAY_ITS_A_ME_MARIO: return intro_play_its_a_me_mario();
#ifdef KEEP_MARIO_HEAD
        case LVL_INTRO_REGULAR:             return intro_regular();
        case LVL_INTRO_GAME_OVER:           return intro_game_over();
#else // KEEP_MARIO_HEAD
        case LVL_INTRO_REGULAR:
#ifdef DEBUG_LEVEL_SELECT
            if (gPlayer1Controller->buttonDown & L_TRIG) {
                gDebugLevelSelect = TRUE;
            }
#endif // DEBUG_LEVEL_SELECT
            // fallthrough
        case LVL_INTRO_GAME_OVER:           return (LEVEL_FILE_SELECT + gDebugLevelSelect);
#endif // KEEP_MARIO_HEAD
        case LVL_INTRO_LEVEL_SELECT:        return intro_level_select();
        default: return LEVEL_NONE;
    }
}
