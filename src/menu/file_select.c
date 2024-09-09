#include <PR/ultratypes.h>
#include <PR/gbi.h>

#include "audio/external.h"
#include "behavior_data.h"
#include "dialog_ids.h"
#include "engine/behavior_script.h"
#include "engine/graph_node.h"
#include "engine/math_util.h"
#include "file_select.h"
#include "game/area.h"
#include "game/game_init.h"
#include "game/ingame_menu.h"
#include "game/object_helpers.h"
#include "game/object_list_processor.h"
#include "game/print.h"
#include "game/save_file.h"
#include "game/segment2.h"
#include "game/segment7.h"
#include "game/spawn_object.h"
#include "game/rumble_init.h"
#include "sm64.h"

/**
 * @file file_select.c
 * This file implements how the file select and it's menus render and function.
 * That includes button IDs rendered as object models, strings, hand cursor,
 * special menu messages and phases, button states and button clicked checks.
 */

// Amount of main menu buttons defined in the code called by spawn_object_rel_with_rot.
// See file_select.h for the names in MenuButtonTypes.
static struct Object *sMainMenuButtons[MENU_BUTTON_OPTION_MAX];

// Used to defined yes/no fade colors after a file is selected in the erase menu.
// sYesNoColor[0]: YES | sYesNoColor[1]: NO
static u8 sYesNoColor[2];

// The button that is selected when it is clicked.
static s8 sSelectedButtonID = MENU_BUTTON_NONE;

// Whether we are on the main menu or one of the submenus.
static s8 sCurrentMenuLevel = MENU_LAYER_MAIN;

// 2D position of the cursor on the screen.
// sCursorPos[0]: X | sCursorPos[1]: Y
static f32 sCursorPos[] = {0, 0};

// Determines which graphic to use for the cursor.
static s16 sCursorClickingTimer = 0;

// Equal to sCursorPos if the cursor gets clicked, {-10000, -10000} otherwise.
static s16 sClickPos[] = {-10000, -10000};

// Used for determining which file has been selected during copying and erasing.
static s8 sSelectedFileIndex = -1;

// Whether to fade out text or not.
static s8 sFadeOutText = FALSE;

// The message currently being displayed at the top of a menu.
static s8 sStatusMessageID = 0;

// Used for text fading. The alpha value of text is calculated as
// gDialogTextAlpha - sTextFadeAlpha.
static u8 sTextFadeAlpha = 0;

// File select timer that keeps counting until it reaches 1000.
// Used to prevent buttons from being clickable as soon as a menu loads.
// Gets reset when you click an empty save, existing saves in copy and erase menus
// and when you click yes/no in the erase confirmation prompt.
static s16 sMainMenuTimer = 0;

// Sound mode menu buttonID, has different values compared to gSoundMode in audio.
// 0: gSoundMode = 0 (Stereo) | 1: gSoundMode = 3 (Mono) | 2: gSoundMode = 1 (Headset)
s8 sSoundMode = 0;

// Active language for EU arrays
// 0: English | 1: French | 2: German

// Tracks which button will be pressed in the erase confirmation prompt (yes/no).
static s8 sEraseYesNoHoverState = MENU_ERASE_HOVER_NONE;

// Used for the copy menu, defines if the game as all 4 save slots with data.
// if TRUE, it doesn't allow copying more files.
static s8 sAllFilesExist = FALSE;

// Defines the value of the save slot selected in the menu.
// Mario A: 1 | Mario B: 2 | Mario C: 3 | Mario D: 4
static s8 sSelectedFileNum = 0;

// Which coin score mode to use when scoring files. 0 for local
// coin high score, 1 for high score across all files.
static s8 sScoreFileCoinScoreMode = 0;

#ifdef MULTILANG
// Index of the selected language in the above array.
static s8 sSelectedLanguageIndex = LANGUAGE_ENGLISH;

// Whether to open the language menu when the game is booted.
static s8 sOpenLangSettings = FALSE;
#endif

/**
 * Yellow Background Menu Initial Action
 * Rotates the background at 180 grades and it's scale.
 * Although the scale is properly applied in the loop function.
 */
void beh_yellow_background_menu_init(void) {
    gCurrentObject->oFaceAngleYaw = 0x8000;
    gCurrentObject->oMenuButtonScale = 9.0f;
}

/**
 * Yellow Background Menu Loop Action
 * Properly scales the background in the main menu.
 */
void beh_yellow_background_menu_loop(void) {
    cur_obj_scale(9.0f);
}

/**
 * Check if a button was clicked.
 * depth = 200.0 for main menu, 22.0 for submenus.
 */
s32 check_clicked_button(s16 x, s16 y, f32 depth) {
    f32 a = 52.4213f;
    f32 newX = ((f32) x * 160.0f) / (a * depth);
    f32 newY = ((f32) y * 120.0f) / (a * 3 / 4 * depth);
    s16 maxX = newX + 25.0f;
    s16 minX = newX - 25.0f;
    s16 maxY = newY + 21.0f;
    s16 minY = newY - 21.0f;

    if (sClickPos[0] < maxX && minX < sClickPos[0] && sClickPos[1] < maxY && minY < sClickPos[1]) {
        return TRUE;
    }
    return FALSE;
}

/**
 * Grow from main menu, used by selecting files and menus.
 */
void bhv_menu_button_growing_from_main_menu(struct Object *button) {
    if (button->oMenuButtonTimer < 16) {
        button->oFaceAngleYaw += 0x800;
    }
    if (button->oMenuButtonTimer < 8) {
        button->oFaceAnglePitch += 0x800;
    }
    if (button->oMenuButtonTimer >= 8 && button->oMenuButtonTimer < 16) {
        button->oFaceAnglePitch -= 0x800;
    }
    button->oParentRelativePosX -= button->oMenuButtonOrigPosX / 16.0f;
    button->oParentRelativePosY -= button->oMenuButtonOrigPosY / 16.0f;
    if (button->oPosZ < button->oMenuButtonOrigPosZ + 17800.0f) {
        button->oParentRelativePosZ += 1112.5f;
    }
    button->oMenuButtonTimer++;
    if (button->oMenuButtonTimer == 16) {
        button->oParentRelativePosX = 0.0f;
        button->oParentRelativePosY = 0.0f;
        button->oMenuButtonState = MENU_BUTTON_STATE_FULLSCREEN;
        button->oMenuButtonTimer = 0;
    }
}

/**
 * Shrink back to main menu, used to return back while inside menus.
 */
void bhv_menu_button_shrinking_to_main_menu(struct Object *button) {
    if (button->oMenuButtonTimer < 16) {
        button->oFaceAngleYaw -= 0x800;
    }
    if (button->oMenuButtonTimer < 8) {
        button->oFaceAnglePitch -= 0x800;
    }
    if (button->oMenuButtonTimer >= 8 && button->oMenuButtonTimer < 16) {
        button->oFaceAnglePitch += 0x800;
    }
    button->oParentRelativePosX += button->oMenuButtonOrigPosX / 16.0f;
    button->oParentRelativePosY += button->oMenuButtonOrigPosY / 16.0f;
    if (button->oPosZ > button->oMenuButtonOrigPosZ) {
        button->oParentRelativePosZ -= 1112.5f;
    }
    button->oMenuButtonTimer++;
    if (button->oMenuButtonTimer == 16) {
        button->oParentRelativePosX = button->oMenuButtonOrigPosX;
        button->oParentRelativePosY = button->oMenuButtonOrigPosY;
        button->oMenuButtonState = MENU_BUTTON_STATE_DEFAULT;
        button->oMenuButtonTimer = 0;
    }
}

/**
 * Grow from submenu, used by selecting a file in the score menu.
 */
void bhv_menu_button_growing_from_submenu(struct Object *button) {
    if (button->oMenuButtonTimer < 16) {
        button->oFaceAngleYaw += 0x800;
    }
    if (button->oMenuButtonTimer < 8) {
        button->oFaceAnglePitch += 0x800;
    }
    if (button->oMenuButtonTimer >= 8 && button->oMenuButtonTimer < 16) {
        button->oFaceAnglePitch -= 0x800;
    }
    button->oParentRelativePosX -= button->oMenuButtonOrigPosX / 16.0f;
    button->oParentRelativePosY -= button->oMenuButtonOrigPosY / 16.0f;
    button->oParentRelativePosZ -= 116.25f;
    button->oMenuButtonTimer++;
    if (button->oMenuButtonTimer == 16) {
        button->oParentRelativePosX = 0.0f;
        button->oParentRelativePosY = 0.0f;
        button->oMenuButtonState = MENU_BUTTON_STATE_FULLSCREEN;
        button->oMenuButtonTimer = 0;
    }
}

/**
 * Shrink back to submenu, used to return back while inside a score save menu.
 */
void bhv_menu_button_shrinking_to_submenu(struct Object *button) {
    if (button->oMenuButtonTimer < 16) {
        button->oFaceAngleYaw -= 0x800;
    }
    if (button->oMenuButtonTimer < 8) {
        button->oFaceAnglePitch -= 0x800;
    }
    if (button->oMenuButtonTimer >= 8 && button->oMenuButtonTimer < 16) {
        button->oFaceAnglePitch += 0x800;
    }
    button->oParentRelativePosX += button->oMenuButtonOrigPosX / 16.0f;
    button->oParentRelativePosY += button->oMenuButtonOrigPosY / 16.0f;
    if (button->oPosZ > button->oMenuButtonOrigPosZ) {
        button->oParentRelativePosZ += 116.25f;
    }
    button->oMenuButtonTimer++;
    if (button->oMenuButtonTimer == 16) {
        button->oParentRelativePosX = button->oMenuButtonOrigPosX;
        button->oParentRelativePosY = button->oMenuButtonOrigPosY;
        button->oMenuButtonState = MENU_BUTTON_STATE_DEFAULT;
        button->oMenuButtonTimer = 0;
    }
}

/**
 * A small increase and decrease in size.
 * Used by failed copy/erase/score operations and sound mode select.
 */
void bhv_menu_button_zoom_in_out(struct Object *button) {
    if (sCurrentMenuLevel == MENU_LAYER_MAIN) {
        if (button->oMenuButtonTimer < 4) {
            button->oParentRelativePosZ -= 20.0f;
        }
        if (button->oMenuButtonTimer >= 4) {
            button->oParentRelativePosZ += 20.0f;
        }
    } else {
        if (button->oMenuButtonTimer < 4) {
            button->oParentRelativePosZ += 20.0f;
        }
        if (button->oMenuButtonTimer >= 4) {
            button->oParentRelativePosZ -= 20.0f;
        }
    }
    button->oMenuButtonTimer++;
    if (button->oMenuButtonTimer == 8) {
        button->oMenuButtonState = MENU_BUTTON_STATE_DEFAULT;
        button->oMenuButtonTimer = 0;
    }
}

/**
 * A small temporary increase in size.
 * Used while selecting a target copy/erase file or yes/no erase confirmation prompt.
 */
void bhv_menu_button_zoom_in(struct Object *button) {
    button->oMenuButtonScale += 0.0022f;
    button->oMenuButtonTimer++;
    if (button->oMenuButtonTimer == 10) {
        button->oMenuButtonState = MENU_BUTTON_STATE_DEFAULT;
        button->oMenuButtonTimer = 0;
    }
}

/**
 * A small temporary decrease in size.
 * Used after selecting a target copy/erase file or
 * yes/no erase confirmation prompt to undo the zoom in.
 */
void bhv_menu_button_zoom_out(struct Object *button) {
    button->oMenuButtonScale -= 0.0022f;
    button->oMenuButtonTimer++;
    if (button->oMenuButtonTimer == 10) {
        button->oMenuButtonState = MENU_BUTTON_STATE_DEFAULT;
        button->oMenuButtonTimer = 0;
    }
}

/**
 * Menu Buttons Menu Initial Action
 * Aligns menu buttons so they can stay in their original
 * positions when you choose a button.
 */
void bhv_menu_button_init(void) {
    gCurrentObject->oMenuButtonOrigPosX = gCurrentObject->oParentRelativePosX;
    gCurrentObject->oMenuButtonOrigPosY = gCurrentObject->oParentRelativePosY;
}

/**
 * Menu Buttons Menu Loop Action
 * Handles the functions of the button states and
 * object scale for each button.
 */
void bhv_menu_button_loop(void) {
    switch (gCurrentObject->oMenuButtonState) {
        case MENU_BUTTON_STATE_DEFAULT: // Button state
            gCurrentObject->oMenuButtonOrigPosZ = gCurrentObject->oPosZ;
            break;
        case MENU_BUTTON_STATE_GROWING: // Switching from button to menu state
            if (sCurrentMenuLevel == MENU_LAYER_MAIN) {
                bhv_menu_button_growing_from_main_menu(gCurrentObject);
            }
            if (sCurrentMenuLevel == MENU_LAYER_SUBMENU) {
                bhv_menu_button_growing_from_submenu(gCurrentObject); // Only used for score files
            }
            gDialogTextAlpha = 0;
            sCursorClickingTimer = 4;
            break;
        case MENU_BUTTON_STATE_FULLSCREEN: // Menu state
            break;
        case MENU_BUTTON_STATE_SHRINKING: // Switching from menu to button state
            if (sCurrentMenuLevel == MENU_LAYER_MAIN) {
                bhv_menu_button_shrinking_to_main_menu(gCurrentObject);
            }
            if (sCurrentMenuLevel == MENU_LAYER_SUBMENU) {
                bhv_menu_button_shrinking_to_submenu(gCurrentObject); // Only used for score files
            }
            gDialogTextAlpha = 0;
            sCursorClickingTimer = 4;
            break;
        case MENU_BUTTON_STATE_ZOOM_IN_OUT:
            bhv_menu_button_zoom_in_out(gCurrentObject);
            sCursorClickingTimer = 4;
            break;
        case MENU_BUTTON_STATE_ZOOM_IN:
            bhv_menu_button_zoom_in(gCurrentObject);
            sCursorClickingTimer = 4;
            break;
        case MENU_BUTTON_STATE_ZOOM_OUT:
            bhv_menu_button_zoom_out(gCurrentObject);
            sCursorClickingTimer = 4;
            break;
    }
    cur_obj_scale(gCurrentObject->oMenuButtonScale);
}

/**
 * Handles how to exit the score file menu using button states.
 */
void exit_score_file_to_score_menu(struct Object *scoreFileButton, s8 scoreButtonID) {
    // Begin exit
    if (scoreFileButton->oMenuButtonState == MENU_BUTTON_STATE_FULLSCREEN
        && sCursorClickingTimer == 2) {
        play_sound(SOUND_MENU_CAMERA_ZOOM_OUT, gGlobalSoundSource);
#if ENABLE_RUMBLE
        queue_rumble_data(5, 80);
#endif
        scoreFileButton->oMenuButtonState = MENU_BUTTON_STATE_SHRINKING;
    }
    // End exit
    if (scoreFileButton->oMenuButtonState == MENU_BUTTON_STATE_DEFAULT) {
        sSelectedButtonID = scoreButtonID;
        if (sCurrentMenuLevel == MENU_LAYER_SUBMENU) {
            sCurrentMenuLevel = MENU_LAYER_MAIN;
        }
    }
}

static const Vec3s sSaveFileButtonPositions[] = {
    {  711, 311, -100 }, // SAVE_FILE_A
    { -166, 311, -100 }, // SAVE_FILE_B
    {  711,   0, -100 }, // SAVE_FILE_C
    { -166,   0, -100 }, // SAVE_FILE_D
};

#define SPAWN_FILE_SELECT_FILE_BUTTON(parent, saveFile)                                                 \
    spawn_object_rel_with_rot((parent),                                                                 \
    (save_file_exists(saveFile) ? MODEL_MAIN_MENU_MARIO_SAVE_BUTTON : MODEL_MAIN_MENU_MARIO_NEW_BUTTON),\
    bhvMenuButton,                                                                                      \
    sSaveFileButtonPositions[saveFile][0],                                                              \
    sSaveFileButtonPositions[saveFile][1],                                                              \
    sSaveFileButtonPositions[saveFile][2],                                                              \
    0x0, -0x8000, 0x0)

#define MENU_BUTTON_SCALE 0.11111111f

/**
 * Render buttons for the menu.
 * Also check if the save file exists to render a different Mario button.
 */
void render_menu_buttons(s32 selectedButtonID) {
    struct Object *button = sMainMenuButtons[selectedButtonID];
    // MENU_BUTTON_SCORE ->  7
    // MENU_BUTTON_COPY  -> 14
    // MENU_BUTTON_ERASE -> 21
    s32 idx = (selectedButtonID - 3) * 7;

    // File A
    sMainMenuButtons[idx + 0] = SPAWN_FILE_SELECT_FILE_BUTTON(button, SAVE_FILE_A);
    sMainMenuButtons[idx + 0]->oMenuButtonScale = MENU_BUTTON_SCALE;
    // File B
    sMainMenuButtons[idx + 1] = SPAWN_FILE_SELECT_FILE_BUTTON(button, SAVE_FILE_B);
    sMainMenuButtons[idx + 1]->oMenuButtonScale = MENU_BUTTON_SCALE;
    // File C
    sMainMenuButtons[idx + 2] = SPAWN_FILE_SELECT_FILE_BUTTON(button, SAVE_FILE_C);
    sMainMenuButtons[idx + 2]->oMenuButtonScale = MENU_BUTTON_SCALE;
    // File D
    sMainMenuButtons[idx + 3] = SPAWN_FILE_SELECT_FILE_BUTTON(button, SAVE_FILE_D);
    sMainMenuButtons[idx + 3]->oMenuButtonScale = MENU_BUTTON_SCALE;

    // Return to main menu button
    sMainMenuButtons[idx + 4] =
        spawn_object_rel_with_rot(button, MODEL_MAIN_MENU_YELLOW_FILE_BUTTON,
                                  bhvMenuButton,  711, -388, -100, 0x0, -0x8000, 0x0);
    sMainMenuButtons[idx + 4]->oMenuButtonScale = MENU_BUTTON_SCALE;
    // Switch to copy menu button
    sMainMenuButtons[idx + 5] =
        spawn_object_rel_with_rot(button, selectedButtonID == MENU_BUTTON_SCORE ? MODEL_MAIN_MENU_BLUE_COPY_BUTTON : MODEL_MAIN_MENU_GREEN_SCORE_BUTTON,
                                  bhvMenuButton,    0, -388, -100, 0x0, -0x8000, 0x0);
    sMainMenuButtons[idx + 5]->oMenuButtonScale = MENU_BUTTON_SCALE;
    // Switch to erase menu button
    sMainMenuButtons[idx + 6] =
        spawn_object_rel_with_rot(button, selectedButtonID == MENU_BUTTON_ERASE ? MODEL_MAIN_MENU_BLUE_COPY_BUTTON : MODEL_MAIN_MENU_RED_ERASE_BUTTON,
                                  bhvMenuButton, -711, -388, -100, 0x0, -0x8000, 0x0);
    sMainMenuButtons[idx + 6]->oMenuButtonScale = MENU_BUTTON_SCALE;
}

#define SCORE_TIMER 31
/**
 * In the score menu, checks if a button was clicked to play a sound, button state and other functions.
 */
void check_score_menu_clicked_buttons(struct Object *scoreButton) {
    if (scoreButton->oMenuButtonState == MENU_BUTTON_STATE_FULLSCREEN) {
        s32 buttonID;
        // Configure score menu button group
        for (buttonID = MENU_BUTTON_SCORE_MIN; buttonID < MENU_BUTTON_SCORE_MAX; buttonID++) {
            s16 buttonX = sMainMenuButtons[buttonID]->oPosX;
            s16 buttonY = sMainMenuButtons[buttonID]->oPosY;

            if (check_clicked_button(buttonX, buttonY, 22.0f) == TRUE && sMainMenuTimer >= SCORE_TIMER) {
                // If menu button clicked, select it
                if (buttonID == MENU_BUTTON_SCORE_RETURN || buttonID == MENU_BUTTON_SCORE_COPY_FILE
                    || buttonID == MENU_BUTTON_SCORE_ERASE_FILE) {
                    play_sound(SOUND_MENU_CLICK_FILE_SELECT, gGlobalSoundSource);
#if ENABLE_RUMBLE
                    queue_rumble_data(5, 80);
#endif
                    sMainMenuButtons[buttonID]->oMenuButtonState = MENU_BUTTON_STATE_ZOOM_IN_OUT;
                    sSelectedButtonID = buttonID;
                }
                else { // Check if a save file is clicked
                    if (sMainMenuTimer >= SCORE_TIMER) {
                        // If clicked in a existing save file, select it too see it's score
                        if (save_file_exists(buttonID - MENU_BUTTON_SCORE_MIN) == TRUE) {
                            play_sound(SOUND_MENU_CAMERA_ZOOM_IN, gGlobalSoundSource);
#if ENABLE_RUMBLE
                            queue_rumble_data(5, 80);
#endif
                            sMainMenuButtons[buttonID]->oMenuButtonState = MENU_BUTTON_STATE_GROWING;
                            sSelectedButtonID = buttonID;
                        }
                        else {
                            // If clicked in a non-existing save file, play buzz sound
                            play_sound(SOUND_MENU_CAMERA_BUZZ, gGlobalSoundSource);
#if ENABLE_RUMBLE
                            queue_rumble_data(5, 80);
#endif
                            sMainMenuButtons[buttonID]->oMenuButtonState =
                                MENU_BUTTON_STATE_ZOOM_IN_OUT;
                            if (sMainMenuTimer >= SCORE_TIMER) {
                                sFadeOutText = TRUE;
                                sMainMenuTimer = 0;
                            }
                        }
                    }
                }
                sCurrentMenuLevel = MENU_LAYER_SUBMENU;
                break;
            }
        }
    }
}

#undef SCORE_TIMER

#define BUZZ_TIMER 21

/**
 * Copy Menu phase actions that handles what to do when a file button is clicked.
 */
void copy_action_file_button(struct Object *copyButton, s32 copyFileButtonID) {
    switch (copyButton->oMenuButtonActionPhase) {
        case COPY_PHASE_MAIN: // Copy Menu Main Phase
            if (sAllFilesExist == TRUE) { // Don't enable copy if all save files exists
                return;
            }
            if (save_file_exists(copyFileButtonID - MENU_BUTTON_COPY_MIN) == TRUE) {
                // If clicked in a existing save file, ask where it wants to copy
                play_sound(SOUND_MENU_CLICK_FILE_SELECT, gGlobalSoundSource);
#if ENABLE_RUMBLE
                queue_rumble_data(5, 80);
#endif
                sMainMenuButtons[copyFileButtonID]->oMenuButtonState = MENU_BUTTON_STATE_ZOOM_IN;
                sSelectedFileIndex = copyFileButtonID - MENU_BUTTON_COPY_MIN;
                copyButton->oMenuButtonActionPhase = COPY_PHASE_COPY_WHERE;
                sFadeOutText = TRUE;
                sMainMenuTimer = 0;
            } else {
                // If clicked in a non-existing save file, play buzz sound
                play_sound(SOUND_MENU_CAMERA_BUZZ, gGlobalSoundSource);
#if ENABLE_RUMBLE
                queue_rumble_data(5, 80);
#endif
                sMainMenuButtons[copyFileButtonID]->oMenuButtonState = MENU_BUTTON_STATE_ZOOM_IN_OUT;
                if (sMainMenuTimer >= BUZZ_TIMER) {
                    sFadeOutText = TRUE;
                    sMainMenuTimer = 0;
                }
            }
            break;
        case COPY_PHASE_COPY_WHERE: // Copy Menu "COPY IT TO WHERE?" Phase (after a file is selected)
            sMainMenuButtons[copyFileButtonID]->oMenuButtonState = MENU_BUTTON_STATE_ZOOM_IN_OUT;
            if (save_file_exists(copyFileButtonID - MENU_BUTTON_COPY_MIN) == FALSE) {
                // If clicked in a non-existing save file, copy the file
                play_sound(SOUND_MENU_STAR_SOUND, gGlobalSoundSource);
#if ENABLE_RUMBLE
                queue_rumble_data(5, 80);
#endif
                copyButton->oMenuButtonActionPhase = COPY_PHASE_COPY_COMPLETE;
                sFadeOutText = TRUE;
                sMainMenuTimer = 0;
                save_file_copy(sSelectedFileIndex, copyFileButtonID - MENU_BUTTON_COPY_MIN);
                sMainMenuButtons[copyFileButtonID]->header.gfx.sharedChild =
                    gLoadedGraphNodes[MODEL_MAIN_MENU_MARIO_SAVE_BUTTON_FADE];
                sMainMenuButtons[copyFileButtonID - MENU_BUTTON_COPY_MIN]->header.gfx.sharedChild =
                    gLoadedGraphNodes[MODEL_MAIN_MENU_MARIO_SAVE_BUTTON_FADE];
            } else {
                // If clicked in a existing save file, play buzz sound
                if (MENU_BUTTON_COPY_FILE_A + sSelectedFileIndex == copyFileButtonID) {
                    play_sound(SOUND_MENU_CAMERA_BUZZ, gGlobalSoundSource);
#if ENABLE_RUMBLE
                    queue_rumble_data(5, 80);
#endif
                    sMainMenuButtons[MENU_BUTTON_COPY_FILE_A + sSelectedFileIndex]->oMenuButtonState = MENU_BUTTON_STATE_ZOOM_OUT;
                    copyButton->oMenuButtonActionPhase = COPY_PHASE_MAIN;
                    sFadeOutText = TRUE;
                    return;
                }
                if (sMainMenuTimer >= BUZZ_TIMER) {
                    sFadeOutText = TRUE;
                    sMainMenuTimer = 0;
                }
            }
            break;
    }
}

#define ACTION_TIMER 30

/**
 * In the copy menu, checks if a button was clicked to play a sound, button state and other functions.
 */
void check_copy_menu_clicked_buttons(struct Object *copyButton) {
    if (copyButton->oMenuButtonState == MENU_BUTTON_STATE_FULLSCREEN) {
        s32 buttonID;
        // Configure copy menu button group
        for (buttonID = MENU_BUTTON_COPY_MIN; buttonID < MENU_BUTTON_COPY_MAX; buttonID++) {
            s16 buttonX = sMainMenuButtons[buttonID]->oPosX;
            s16 buttonY = sMainMenuButtons[buttonID]->oPosY;

            if (check_clicked_button(buttonX, buttonY, 22.0f) == TRUE) {
                // If menu button clicked, select it
                if (buttonID == MENU_BUTTON_COPY_RETURN || buttonID == MENU_BUTTON_COPY_CHECK_SCORE
                    || buttonID == MENU_BUTTON_COPY_ERASE_FILE) {
                    if (copyButton->oMenuButtonActionPhase == COPY_PHASE_MAIN) {
                        play_sound(SOUND_MENU_CLICK_FILE_SELECT, gGlobalSoundSource);
#if ENABLE_RUMBLE
                        queue_rumble_data(5, 80);
#endif
                        sMainMenuButtons[buttonID]->oMenuButtonState = MENU_BUTTON_STATE_ZOOM_IN_OUT;
                        sSelectedButtonID = buttonID;
                    }
                }
                else {
                    // Check if a file button is clicked to play a copy action
                    if (sMainMenuButtons[buttonID]->oMenuButtonState == MENU_BUTTON_STATE_DEFAULT
                        && sMainMenuTimer >= ACTION_TIMER) {
                        copy_action_file_button(copyButton, buttonID);
                    }
                }
                sCurrentMenuLevel = MENU_LAYER_SUBMENU;
                break;
            }
        }

        // After copy is complete, return to main copy phase
        if (copyButton->oMenuButtonActionPhase == COPY_PHASE_COPY_COMPLETE
            && sMainMenuTimer > ACTION_TIMER) {
            copyButton->oMenuButtonActionPhase = COPY_PHASE_MAIN;
            sMainMenuButtons[MENU_BUTTON_COPY_MIN + sSelectedFileIndex]->oMenuButtonState =
                MENU_BUTTON_STATE_ZOOM_OUT;
        }
    }
}

/**
 * Erase Menu phase actions that handles what to do when a file button is clicked.
 */
void erase_action_file_button(struct Object *eraseButton, s32 eraseFileButtonID) {
    switch (eraseButton->oMenuButtonActionPhase) {
        case ERASE_PHASE_MAIN: // Erase Menu Main Phase
            if (save_file_exists(eraseFileButtonID - MENU_BUTTON_ERASE_MIN) == TRUE) {
                // If clicked in a existing save file, ask if it wants to delete it
                play_sound(SOUND_MENU_CLICK_FILE_SELECT, gGlobalSoundSource);
#if ENABLE_RUMBLE
                queue_rumble_data(5, 80);
#endif
                sMainMenuButtons[eraseFileButtonID]->oMenuButtonState = MENU_BUTTON_STATE_ZOOM_IN;
                sSelectedFileIndex = eraseFileButtonID - MENU_BUTTON_ERASE_MIN;
                eraseButton->oMenuButtonActionPhase = ERASE_PHASE_PROMPT;
                sFadeOutText = TRUE;
                sMainMenuTimer = 0;
            } else {
                // If clicked in a non-existing save file, play buzz sound
                play_sound(SOUND_MENU_CAMERA_BUZZ, gGlobalSoundSource);
#if ENABLE_RUMBLE
                queue_rumble_data(5, 80);
#endif
                sMainMenuButtons[eraseFileButtonID]->oMenuButtonState = MENU_BUTTON_STATE_ZOOM_IN_OUT;

                if (sMainMenuTimer >= BUZZ_TIMER) {
                    sFadeOutText = TRUE;
                    sMainMenuTimer = 0;
                }
            }
            break;
        case ERASE_PHASE_PROMPT: // Erase Menu "SURE? YES NO" Phase (after a file is selected)
            if (MENU_BUTTON_ERASE_MIN + sSelectedFileIndex == eraseFileButtonID) {
                // If clicked in a existing save file, play click sound and zoom out button
                // Note: The prompt functions are actually called when the ERASE_MSG_PROMPT
                // message is displayed with print_erase_menu_prompt
                play_sound(SOUND_MENU_CLICK_FILE_SELECT, gGlobalSoundSource);
#if ENABLE_RUMBLE
                queue_rumble_data(5, 80);
#endif
                sMainMenuButtons[MENU_BUTTON_ERASE_MIN + sSelectedFileIndex]->oMenuButtonState =
                    MENU_BUTTON_STATE_ZOOM_OUT;
                eraseButton->oMenuButtonActionPhase = ERASE_PHASE_MAIN;
                sFadeOutText = TRUE;
            }
            break;
    }
}

#undef BUZZ_TIMER

/**
 * In the erase menu, checks if a button was clicked to play a sound, button state and other functions.
 */
void check_erase_menu_clicked_buttons(struct Object *eraseButton) {
    if (eraseButton->oMenuButtonState == MENU_BUTTON_STATE_FULLSCREEN) {
        s32 buttonID;
        // Configure erase menu button group
        for (buttonID = MENU_BUTTON_ERASE_MIN; buttonID < MENU_BUTTON_ERASE_MAX; buttonID++) {
            s16 buttonX = sMainMenuButtons[buttonID]->oPosX;
            s16 buttonY = sMainMenuButtons[buttonID]->oPosY;

            if (check_clicked_button(buttonX, buttonY, 22.0f) == TRUE) {
                // If menu button clicked, select it
                if (buttonID == MENU_BUTTON_ERASE_RETURN || buttonID == MENU_BUTTON_ERASE_CHECK_SCORE
                    || buttonID == MENU_BUTTON_ERASE_COPY_FILE) {
                    if (eraseButton->oMenuButtonActionPhase == ERASE_PHASE_MAIN) {
                        play_sound(SOUND_MENU_CLICK_FILE_SELECT, gGlobalSoundSource);
#if ENABLE_RUMBLE
                        queue_rumble_data(5, 80);
#endif
                        sMainMenuButtons[buttonID]->oMenuButtonState = MENU_BUTTON_STATE_ZOOM_IN_OUT;
                        sSelectedButtonID = buttonID;
                    }
                }
                else {
                    // Check if a file button is clicked to play an erase action
                    if (sMainMenuTimer >= ACTION_TIMER) {
                        erase_action_file_button(eraseButton, buttonID);
                    }
                }
                sCurrentMenuLevel = MENU_LAYER_SUBMENU;
                break;
            }
        }
        // After erase is complete, return to main erase phase
        if (eraseButton->oMenuButtonActionPhase == ERASE_PHASE_MARIO_ERASED
            && sMainMenuTimer > ACTION_TIMER) {
            eraseButton->oMenuButtonActionPhase = ERASE_PHASE_MAIN;
            sMainMenuButtons[MENU_BUTTON_ERASE_MIN + sSelectedFileIndex]->oMenuButtonState =
                MENU_BUTTON_STATE_ZOOM_OUT;
        }
    }
}

#undef ACTION_TIMER

#ifdef MULTILANG
    #define SOUND_BUTTON_Y 388
#else
    #define SOUND_BUTTON_Y 0
#endif

/**
 * Render buttons for the sound mode menu.
 */
void render_sound_mode_menu_buttons(struct Object *soundModeButton) {
#ifdef ENABLE_STEREO_HEADSET_EFFECTS
    // Stereo option button
    sMainMenuButtons[MENU_BUTTON_STEREO] = spawn_object_rel_with_rot(
        soundModeButton, MODEL_MAIN_MENU_GENERIC_BUTTON, bhvMenuButton,  533, SOUND_BUTTON_Y, -100, 0x0, -0x8000, 0x0);
    sMainMenuButtons[MENU_BUTTON_STEREO]->oMenuButtonScale = MENU_BUTTON_SCALE;
    // Mono option button
    sMainMenuButtons[MENU_BUTTON_MONO] = spawn_object_rel_with_rot(
        soundModeButton, MODEL_MAIN_MENU_GENERIC_BUTTON, bhvMenuButton,    0, SOUND_BUTTON_Y, -100, 0x0, -0x8000, 0x0);
    sMainMenuButtons[MENU_BUTTON_MONO]->oMenuButtonScale = MENU_BUTTON_SCALE;
    // Headset option button
    sMainMenuButtons[MENU_BUTTON_HEADSET] = spawn_object_rel_with_rot(
        soundModeButton, MODEL_MAIN_MENU_GENERIC_BUTTON, bhvMenuButton, -533, SOUND_BUTTON_Y, -100, 0x0, -0x8000, 0x0);
    sMainMenuButtons[MENU_BUTTON_HEADSET]->oMenuButtonScale = MENU_BUTTON_SCALE;
#else
    // Stereo option button
    sMainMenuButtons[MENU_BUTTON_STEREO] = spawn_object_rel_with_rot(
        soundModeButton, MODEL_MAIN_MENU_GENERIC_BUTTON, bhvMenuButton,  355, SOUND_BUTTON_Y, -100, 0x0, -0x8000, 0x0);
    sMainMenuButtons[MENU_BUTTON_STEREO]->oMenuButtonScale = MENU_BUTTON_SCALE;
    // Mono option button
    sMainMenuButtons[MENU_BUTTON_MONO] = spawn_object_rel_with_rot(
        soundModeButton, MODEL_MAIN_MENU_GENERIC_BUTTON, bhvMenuButton, -355, SOUND_BUTTON_Y, -100, 0x0, -0x8000, 0x0);
    sMainMenuButtons[MENU_BUTTON_MONO]->oMenuButtonScale = MENU_BUTTON_SCALE;
#endif

#ifdef MULTILANG
    // Return button
    sMainMenuButtons[MENU_BUTTON_OPTION_RETURN] = spawn_object_rel_with_rot(
        soundModeButton, MODEL_MAIN_MENU_YELLOW_FILE_BUTTON, bhvMenuButton, 0, -533, -100, 0x0, -0x8000, 0x0);
    sMainMenuButtons[MENU_BUTTON_OPTION_RETURN]->oMenuButtonScale = MENU_BUTTON_SCALE;
#else
    // Zoom in current selection
    sMainMenuButtons[MENU_BUTTON_SOUND_OPTION_MIN + sSoundMode]->oMenuButtonState = MENU_BUTTON_STATE_ZOOM_IN;
#endif
}

/**
 * In the sound mode menu, checks if a button was clicked to change sound mode & button state.
 */
void check_sound_mode_menu_clicked_buttons(struct Object *soundModeButton) {
    if (soundModeButton->oMenuButtonState == MENU_BUTTON_STATE_FULLSCREEN) {
        s32 buttonID;
        // Configure sound mode menu button group
        for (buttonID = MENU_BUTTON_OPTION_MIN; buttonID < MENU_BUTTON_OPTION_MAX; buttonID++) {
            s16 buttonX = sMainMenuButtons[buttonID]->oPosX;
            s16 buttonY = sMainMenuButtons[buttonID]->oPosY;

            if (check_clicked_button(buttonX, buttonY, 22.0f) == TRUE) {
                // If sound mode button clicked, select it and define sound mode
                // The check will always be true because of the group configured above (In JP & US)
                if (buttonID >= MENU_BUTTON_SOUND_OPTION_MIN && buttonID < MENU_BUTTON_SOUND_OPTION_MAX) {
                    if (soundModeButton->oMenuButtonActionPhase == SOUND_MODE_PHASE_MAIN) {
                        play_sound(SOUND_MENU_CLICK_FILE_SELECT, gGlobalSoundSource);
#if ENABLE_RUMBLE
                        queue_rumble_data(5, 80);
#endif
                        sMainMenuButtons[buttonID]->oMenuButtonState = MENU_BUTTON_STATE_ZOOM_IN_OUT;
#ifndef MULTILANG
                        // Sound menu buttons don't return to Main Menu with multilang enabled
                        sSelectedButtonID = buttonID;
#endif
                        sSoundMode = buttonID - MENU_BUTTON_SOUND_OPTION_MIN;
                        save_file_set_sound_mode(sSoundMode);
                    }
                }
#ifdef MULTILANG
                // If neither of the buttons above are pressed, return to main menu
                if (buttonID == MENU_BUTTON_OPTION_RETURN) {
                    play_sound(SOUND_MENU_CLICK_FILE_SELECT, gGlobalSoundSource);
                    sMainMenuButtons[buttonID]->oMenuButtonState = MENU_BUTTON_STATE_ZOOM_IN_OUT;
                    sSelectedButtonID = buttonID;
                }
#endif
                sCurrentMenuLevel = MENU_LAYER_SUBMENU;

                break;
            }
        }
    }
}

/**
 * Loads a save file selected after it goes into a full screen state
 * retuning sSelectedFileNum to a save value defined in fileNum.
 */
void load_main_menu_save_file(struct Object *fileButton, s32 fileNum) {
    if (fileButton->oMenuButtonState == MENU_BUTTON_STATE_FULLSCREEN) {
        sSelectedFileNum = fileNum;
    }
}

/**
 * Clears a section of sMainMenuButtons.
 */
void delete_menu_button_objects(s16 minID, s16 maxID) {
    for (s16 buttonID = minID; buttonID < maxID; buttonID++) {
        obj_mark_for_deletion(sMainMenuButtons[buttonID]);
    }
}

/**
 * Hides buttons of corresponding button menu groups.
 */
void hide_submenu_buttons(s16 prevMenuButtonID) {
    switch (prevMenuButtonID) {
        case MENU_BUTTON_SCORE:      delete_menu_button_objects(MENU_BUTTON_SCORE_MIN,  MENU_BUTTON_SCORE_MAX ); break;
        case MENU_BUTTON_COPY:       delete_menu_button_objects(MENU_BUTTON_COPY_MIN,   MENU_BUTTON_COPY_MAX  ); break;
        case MENU_BUTTON_ERASE:      delete_menu_button_objects(MENU_BUTTON_ERASE_MIN,  MENU_BUTTON_ERASE_MAX ); break;
        case MENU_BUTTON_SOUND_MODE: delete_menu_button_objects(MENU_BUTTON_OPTION_MIN, MENU_BUTTON_OPTION_MAX); break;
    }
}

/**
 * Returns from the previous menu back to the main menu using
 * the return button (or sound mode) as source button.
 */
void return_to_main_menu(s16 prevMenuButtonID, struct Object *sourceButton) {
    // If the source button is in default state and the previous menu in full screen,
    // play zoom out sound and shrink previous menu
    if (sourceButton->oMenuButtonState == MENU_BUTTON_STATE_DEFAULT
        && sMainMenuButtons[prevMenuButtonID]->oMenuButtonState == MENU_BUTTON_STATE_FULLSCREEN) {
        play_sound(SOUND_MENU_CAMERA_ZOOM_OUT, gGlobalSoundSource);
        sMainMenuButtons[prevMenuButtonID]->oMenuButtonState = MENU_BUTTON_STATE_SHRINKING;
        sCurrentMenuLevel = MENU_LAYER_MAIN;
    }
    // If the previous button is in default state, return back to the main menu
    if (sMainMenuButtons[prevMenuButtonID]->oMenuButtonState == MENU_BUTTON_STATE_DEFAULT) {
        sSelectedButtonID = MENU_BUTTON_NONE;
        hide_submenu_buttons(prevMenuButtonID);
    }
}

void load_menu_from_submenu(s16 prevMenuButtonID, s16 selectedButtonID, struct Object *sourceButton) {
    // If the source button is in default state and the previous menu in full screen,
    // play zoom out sound and shrink previous menu
    if ((sourceButton->oMenuButtonState == MENU_BUTTON_STATE_DEFAULT)
     && (sMainMenuButtons[prevMenuButtonID]->oMenuButtonState == MENU_BUTTON_STATE_FULLSCREEN)) {
        play_sound(SOUND_MENU_CAMERA_ZOOM_OUT, gGlobalSoundSource);
        sMainMenuButtons[prevMenuButtonID]->oMenuButtonState = MENU_BUTTON_STATE_SHRINKING;
        sCurrentMenuLevel = MENU_LAYER_MAIN;
    }
    // If the previous button is in default state
    if (sMainMenuButtons[prevMenuButtonID]->oMenuButtonState == MENU_BUTTON_STATE_DEFAULT) {
        if (selectedButtonID != prevMenuButtonID) {
            hide_submenu_buttons(prevMenuButtonID);
        }
        // Play zoom in sound, select score menu and render it's buttons
        sSelectedButtonID = selectedButtonID;
        play_sound(SOUND_MENU_CAMERA_ZOOM_IN, gGlobalSoundSource);
        sMainMenuButtons[selectedButtonID]->oMenuButtonState = MENU_BUTTON_STATE_GROWING;
        render_menu_buttons(selectedButtonID);
    }
}

// Loads score menu from the previous menu using "CHECK SCORE" as source button.
void load_score_menu_from_submenu(s16 prevMenuButtonID, struct Object *sourceButton) {
    load_menu_from_submenu(prevMenuButtonID, MENU_BUTTON_SCORE, sourceButton);
}

// Loads copy menu from the previous menu using "COPY FILE" as source button.
void load_copy_menu_from_submenu(s16 prevMenuButtonID, struct Object *sourceButton) {
    load_menu_from_submenu(prevMenuButtonID, MENU_BUTTON_COPY, sourceButton);
}

// Loads erase menu from the previous menu using "ERASE FILE" as source button.
void load_erase_menu_from_submenu(s16 prevMenuButtonID, struct Object *sourceButton) {
    load_menu_from_submenu(prevMenuButtonID, MENU_BUTTON_ERASE, sourceButton);
}


static const Vec3s sSaveFileButtonInitPositions[] = {
    { -6400, 2800, 0 }, // SAVE_FILE_A
    {  1500, 2800, 0 }, // SAVE_FILE_B
    { -6400,    0, 0 }, // SAVE_FILE_C
    {  1500,    0, 0 }, // SAVE_FILE_D
};

#define SPAWN_FILE_SELECT_FILE_BUTTON_INIT(saveFile)                                                                                            \
    spawn_object_rel_with_rot(o, (save_file_exists(saveFile) ? MODEL_MAIN_MENU_MARIO_SAVE_BUTTON_FADE : MODEL_MAIN_MENU_MARIO_NEW_BUTTON_FADE), \
                              bhvMenuButton,                                                                                                    \
                              sSaveFileButtonInitPositions[saveFile][0],                                                                        \
                              sSaveFileButtonInitPositions[saveFile][1],                                                                        \
                              sSaveFileButtonInitPositions[saveFile][2],                                                                        \
                              0x0, 0x0, 0x0)

/**
 * Menu Buttons Menu Manager Initial Action
 * Creates models of the buttons in the menu. For the Mario buttons it
 * checks if a save file exists to render an specific button model for it.
 * Unlike buttons on submenus, these are never hidden or recreated.
 */
void bhv_menu_button_manager_init(void) {
    // File A
    sMainMenuButtons[MENU_BUTTON_PLAY_FILE_A] = SPAWN_FILE_SELECT_FILE_BUTTON_INIT(SAVE_FILE_A);
    sMainMenuButtons[MENU_BUTTON_PLAY_FILE_A]->oMenuButtonScale = 1.0f;
    // File B
    sMainMenuButtons[MENU_BUTTON_PLAY_FILE_B] = SPAWN_FILE_SELECT_FILE_BUTTON_INIT(SAVE_FILE_B);
    sMainMenuButtons[MENU_BUTTON_PLAY_FILE_B]->oMenuButtonScale = 1.0f;
    // File C
    sMainMenuButtons[MENU_BUTTON_PLAY_FILE_C] = SPAWN_FILE_SELECT_FILE_BUTTON_INIT(SAVE_FILE_C);
    sMainMenuButtons[MENU_BUTTON_PLAY_FILE_C]->oMenuButtonScale = 1.0f;
    // File D
    sMainMenuButtons[MENU_BUTTON_PLAY_FILE_D] = SPAWN_FILE_SELECT_FILE_BUTTON_INIT(SAVE_FILE_D);
    sMainMenuButtons[MENU_BUTTON_PLAY_FILE_D]->oMenuButtonScale = 1.0f;
    // Score menu button
    sMainMenuButtons[MENU_BUTTON_SCORE] =
        spawn_object_rel_with_rot(o, MODEL_MAIN_MENU_GREEN_SCORE_BUTTON,
                                  bhvMenuButton, -6400, -3500, 0, 0x0, 0x0, 0x0);
    sMainMenuButtons[MENU_BUTTON_SCORE]->oMenuButtonScale = 1.0f;
    // Copy menu button
    sMainMenuButtons[MENU_BUTTON_COPY] =
        spawn_object_rel_with_rot(o, MODEL_MAIN_MENU_BLUE_COPY_BUTTON,
                                  bhvMenuButton, -2134, -3500, 0, 0x0, 0x0, 0x0);
    sMainMenuButtons[MENU_BUTTON_COPY]->oMenuButtonScale = 1.0f;
    // Erase menu button
    sMainMenuButtons[MENU_BUTTON_ERASE] =
        spawn_object_rel_with_rot(o, MODEL_MAIN_MENU_RED_ERASE_BUTTON,
                                  bhvMenuButton,  2134, -3500, 0, 0x0, 0x0, 0x0);
    sMainMenuButtons[MENU_BUTTON_ERASE]->oMenuButtonScale = 1.0f;
    // Sound mode menu button (Option Mode in EU)
    sMainMenuButtons[MENU_BUTTON_SOUND_MODE] =
        spawn_object_rel_with_rot(o, MODEL_MAIN_MENU_PURPLE_SOUND_BUTTON,
                                  bhvMenuButton,  6400, -3500, 0, 0x0, 0x0, 0x0);
    sMainMenuButtons[MENU_BUTTON_SOUND_MODE]->oMenuButtonScale = 1.0f;

    gDialogTextAlpha = 0;
}

/**
 * In the main menu, check if a button was clicked to play it's button growing state.
 * Also play a sound and/or render buttons depending of the button ID selected.
 */
void check_main_menu_clicked_buttons(void) {
    // Sound mode menu is handled separately because the button ID for it
    // is not grouped with the IDs of the other submenus.
    if (check_clicked_button(sMainMenuButtons[MENU_BUTTON_SOUND_MODE]->oPosX,
                                sMainMenuButtons[MENU_BUTTON_SOUND_MODE]->oPosY, 200.0f)) {
        sMainMenuButtons[MENU_BUTTON_SOUND_MODE]->oMenuButtonState = MENU_BUTTON_STATE_GROWING;
        sSelectedButtonID = MENU_BUTTON_SOUND_MODE;
    } else {
        // Main Menu buttons
        s8 buttonID;
        // Configure Main Menu button group
        for (buttonID = MENU_BUTTON_MAIN_MIN; buttonID < MENU_BUTTON_MAIN_MAX; buttonID++) {
            s16 buttonX = sMainMenuButtons[buttonID]->oPosX;
            s16 buttonY = sMainMenuButtons[buttonID]->oPosY;

            if (check_clicked_button(buttonX, buttonY, 200.0f)) {
                // If menu button clicked, select it
                sMainMenuButtons[buttonID]->oMenuButtonState = MENU_BUTTON_STATE_GROWING;
                sSelectedButtonID = buttonID;
                break;
            }
        }
    }

#ifdef MULTILANG
    // Open Options Menu if sOpenLangSettings is TRUE (It's TRUE when there's no saves)
    if (sOpenLangSettings && (sMainMenuTimer >= 5)) {
        sMainMenuButtons[MENU_BUTTON_SOUND_MODE]->oMenuButtonState = MENU_BUTTON_STATE_GROWING;
        sSelectedButtonID = MENU_BUTTON_SOUND_MODE;
        sOpenLangSettings = FALSE;
    }
#endif

    // Play sound of the save file clicked
    switch (sSelectedButtonID) {
        case MENU_BUTTON_PLAY_FILE_A:
        case MENU_BUTTON_PLAY_FILE_B:
        case MENU_BUTTON_PLAY_FILE_C:
        case MENU_BUTTON_PLAY_FILE_D:
            play_sound(SOUND_MENU_STAR_SOUND_OKEY_DOKEY, gGlobalSoundSource);
#if ENABLE_RUMBLE
            queue_rumble_data(60, 70);
            queue_rumble_decay(1);
#endif
            break;
        // Play sound of the button clicked and render buttons of that menu.
        case MENU_BUTTON_SCORE:
        case MENU_BUTTON_COPY:
        case MENU_BUTTON_ERASE:
            play_sound(SOUND_MENU_CAMERA_ZOOM_IN, gGlobalSoundSource);
#if ENABLE_RUMBLE
            queue_rumble_data(5, 80);
#endif
            render_menu_buttons(sSelectedButtonID);
            break;
        case MENU_BUTTON_SOUND_MODE:
            play_sound(SOUND_MENU_CAMERA_ZOOM_IN, gGlobalSoundSource);
#if ENABLE_RUMBLE
            queue_rumble_data(5, 80);
#endif
            render_sound_mode_menu_buttons(sMainMenuButtons[MENU_BUTTON_SOUND_MODE]);
            break;
    }
}

/**
 * Menu Buttons Menu Manager Loop Action
 * Calls a menu function depending of the button chosen.
 * sSelectedButtonID is MENU_BUTTON_NONE when the file select
 * is loaded, and that checks what buttonID is clicked in the main menu.
 */
void bhv_menu_button_manager_loop(void) {
    switch (sSelectedButtonID) {
        case MENU_BUTTON_NONE: check_main_menu_clicked_buttons(); break;

        case MENU_BUTTON_PLAY_FILE_A: load_main_menu_save_file(sMainMenuButtons[MENU_BUTTON_PLAY_FILE_A], 1); break;
        case MENU_BUTTON_PLAY_FILE_B: load_main_menu_save_file(sMainMenuButtons[MENU_BUTTON_PLAY_FILE_B], 2); break;
        case MENU_BUTTON_PLAY_FILE_C: load_main_menu_save_file(sMainMenuButtons[MENU_BUTTON_PLAY_FILE_C], 3); break;
        case MENU_BUTTON_PLAY_FILE_D: load_main_menu_save_file(sMainMenuButtons[MENU_BUTTON_PLAY_FILE_D], 4); break;

        case MENU_BUTTON_SCORE: check_score_menu_clicked_buttons(sMainMenuButtons[MENU_BUTTON_SCORE]); break;
        case MENU_BUTTON_COPY:  check_copy_menu_clicked_buttons (sMainMenuButtons[MENU_BUTTON_COPY ]); break;
        case MENU_BUTTON_ERASE: check_erase_menu_clicked_buttons(sMainMenuButtons[MENU_BUTTON_ERASE]); break;

        case MENU_BUTTON_SCORE_FILE_A: exit_score_file_to_score_menu(sMainMenuButtons[MENU_BUTTON_SCORE_FILE_A], MENU_BUTTON_SCORE); break;
        case MENU_BUTTON_SCORE_FILE_B: exit_score_file_to_score_menu(sMainMenuButtons[MENU_BUTTON_SCORE_FILE_B], MENU_BUTTON_SCORE); break;
        case MENU_BUTTON_SCORE_FILE_C: exit_score_file_to_score_menu(sMainMenuButtons[MENU_BUTTON_SCORE_FILE_C], MENU_BUTTON_SCORE); break;
        case MENU_BUTTON_SCORE_FILE_D: exit_score_file_to_score_menu(sMainMenuButtons[MENU_BUTTON_SCORE_FILE_D], MENU_BUTTON_SCORE); break;

        case MENU_BUTTON_SCORE_RETURN:     return_to_main_menu         (MENU_BUTTON_SCORE, sMainMenuButtons[MENU_BUTTON_SCORE_RETURN    ]); break;
        case MENU_BUTTON_SCORE_COPY_FILE:  load_copy_menu_from_submenu (MENU_BUTTON_SCORE, sMainMenuButtons[MENU_BUTTON_SCORE_COPY_FILE ]); break;
        case MENU_BUTTON_SCORE_ERASE_FILE: load_erase_menu_from_submenu(MENU_BUTTON_SCORE, sMainMenuButtons[MENU_BUTTON_SCORE_ERASE_FILE]); break;

        case MENU_BUTTON_COPY_FILE_A: break;
        case MENU_BUTTON_COPY_FILE_B: break;
        case MENU_BUTTON_COPY_FILE_C: break;
        case MENU_BUTTON_COPY_FILE_D: break;

        case MENU_BUTTON_COPY_RETURN:      return_to_main_menu         (MENU_BUTTON_COPY, sMainMenuButtons[MENU_BUTTON_COPY_RETURN     ]); break;
        case MENU_BUTTON_COPY_CHECK_SCORE: load_score_menu_from_submenu(MENU_BUTTON_COPY, sMainMenuButtons[MENU_BUTTON_COPY_CHECK_SCORE]); break;
        case MENU_BUTTON_COPY_ERASE_FILE:  load_erase_menu_from_submenu(MENU_BUTTON_COPY, sMainMenuButtons[MENU_BUTTON_COPY_ERASE_FILE ]); break;

        case MENU_BUTTON_ERASE_FILE_A: break;
        case MENU_BUTTON_ERASE_FILE_B: break;
        case MENU_BUTTON_ERASE_FILE_C: break;
        case MENU_BUTTON_ERASE_FILE_D: break;

        case MENU_BUTTON_ERASE_RETURN:      return_to_main_menu         (MENU_BUTTON_ERASE, sMainMenuButtons[MENU_BUTTON_ERASE_RETURN     ]); break;
        case MENU_BUTTON_ERASE_CHECK_SCORE: load_score_menu_from_submenu(MENU_BUTTON_ERASE, sMainMenuButtons[MENU_BUTTON_ERASE_CHECK_SCORE]); break;
        case MENU_BUTTON_ERASE_COPY_FILE:   load_copy_menu_from_submenu (MENU_BUTTON_ERASE, sMainMenuButtons[MENU_BUTTON_ERASE_COPY_FILE  ]); break;

        case MENU_BUTTON_SOUND_MODE: check_sound_mode_menu_clicked_buttons(sMainMenuButtons[MENU_BUTTON_SOUND_MODE]); break;

#ifdef MULTILANG
        case MENU_BUTTON_OPTION_RETURN: return_to_main_menu(MENU_BUTTON_SOUND_MODE, sMainMenuButtons[MENU_BUTTON_OPTION_RETURN]); break;
#endif
        // STEREO, MONO and HEADSET buttons are undefined so they can be selected without
        // exiting the Options menu, as a result they added a return button
        case MENU_BUTTON_STEREO:  return_to_main_menu(MENU_BUTTON_SOUND_MODE, sMainMenuButtons[MENU_BUTTON_STEREO ]); break;
        case MENU_BUTTON_MONO:    return_to_main_menu(MENU_BUTTON_SOUND_MODE, sMainMenuButtons[MENU_BUTTON_MONO   ]); break;
#ifdef ENABLE_STEREO_HEADSET_EFFECTS
        case MENU_BUTTON_HEADSET: return_to_main_menu(MENU_BUTTON_SOUND_MODE, sMainMenuButtons[MENU_BUTTON_HEADSET]); break;
#endif
    }

    sClickPos[0] = -10000;
    sClickPos[1] = -10000;
}

/**
 * Cursor function that handles button inputs.
 * If the cursor is clicked, sClickPos uses the same value as sCursorPos.
 */
void handle_cursor_button_input(void) {
    // If scoring a file, pressing A just changes the coin score mode.
    if (sSelectedButtonID == MENU_BUTTON_SCORE_FILE_A || sSelectedButtonID == MENU_BUTTON_SCORE_FILE_B
        || sSelectedButtonID == MENU_BUTTON_SCORE_FILE_C
        || sSelectedButtonID == MENU_BUTTON_SCORE_FILE_D) {
        if (gPlayer1Controller->buttonPressed & (B_BUTTON | START_BUTTON | Z_TRIG)) {
            sClickPos[0] = sCursorPos[0];
            sClickPos[1] = sCursorPos[1];
            sCursorClickingTimer = 1;
        } else if (gPlayer1Controller->buttonPressed & A_BUTTON) {
            sScoreFileCoinScoreMode = 1 - sScoreFileCoinScoreMode;
            play_sound(SOUND_MENU_CLICK_FILE_SELECT, gGlobalSoundSource);
        }
    } else { // If cursor is clicked
        if (gPlayer1Controller->buttonPressed
            & (A_BUTTON | B_BUTTON | START_BUTTON)) {
            sClickPos[0] = sCursorPos[0];
            sClickPos[1] = sCursorPos[1];
            sCursorClickingTimer = 1;
        }
    }
}

/**
 * Cursor function that handles analog stick input and button presses with a function near the end.
 */
void handle_controller_cursor_input(void) {
    s16 rawStickX = gPlayer1Controller->rawStickX;
    s16 rawStickY = gPlayer1Controller->rawStickY;

    // Handle deadzone
    if (rawStickY > -2 && rawStickY < 2) {
        rawStickY = 0;
    }
    if (rawStickX > -2 && rawStickX < 2) {
        rawStickX = 0;
    }

    // Move cursor
    sCursorPos[0] += rawStickX / 8;
    sCursorPos[1] += rawStickY / 8;

    // Stop cursor from going offscreen
    if (sCursorPos[0] > 132.0f) {
        sCursorPos[0] = 132.0f;
    }
    if (sCursorPos[0] < -132.0f) {
        sCursorPos[0] = -132.0f;
    }

    if (sCursorPos[1] > 90.0f) {
        sCursorPos[1] = 90.0f;
    }
    if (sCursorPos[1] < -90.0f) {
        sCursorPos[1] = -90.0f;
    }

    if (sCursorClickingTimer == 0) {
        handle_cursor_button_input();
    }
}

/**
 * Prints the cursor (Mario Hand, different to the one in the Mario screen)
 * and loads it's controller inputs in handle_controller_cursor_input
 * to be usable on the file select.
 */
void print_menu_cursor(void) {
    handle_controller_cursor_input();
    create_dl_translation_matrix(MENU_MTX_PUSH, sCursorPos[0] + 160.0f - 5.0, sCursorPos[1] + 120.0f - 25.0, 0.0f);
    // Get the right graphic to use for the cursor.
    if (sCursorClickingTimer == 0) { // Idle
        gSPDisplayList(gDisplayListHead++, dl_menu_idle_hand);
    }
    if (sCursorClickingTimer != 0) { // Grabbing
        gSPDisplayList(gDisplayListHead++, dl_menu_grabbing_hand);
    }
    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
    if (sCursorClickingTimer != 0) {
        sCursorClickingTimer++; // This is a very strange way to implement a timer? It counts up and
                                // then resets to 0 instead of just counting down to 0.
        if (sCursorClickingTimer == 5) {
            sCursorClickingTimer = 0;
        }
    }
}

/**
 * Takes a number between 0 and 3 and formats the corresponding file letter A to D into a buffer.
 * If the language is set to Japanese, the letter is written in full-width digits.
 */
void string_format_file_letter(char *buf, char *str, s32 fileIndex) {
    char letterBuf[4];
#ifdef ENABLE_JAPANESE
    if (gInGameLanguage == LANGUAGE_JAPANESE) {
        // The UTF-8 encoding of "Ａ" is 0xEF, 0xBC, 0xA1
        letterBuf[0] = 0xEF;
        letterBuf[1] = 0xBC;
        letterBuf[2] = 0xA1 + fileIndex;
        letterBuf[3] = '\0';
        sprintf(buf, str, letterBuf);
        return;
    }
#endif

    letterBuf[0] = 'A' + fileIndex;
    letterBuf[1] = '\0';
    sprintf(buf, str, letterBuf);
}

/**
 * Prints a hud string with text fade properties.
 */
void print_hud_lut_string_fade(s16 x, s16 y, char *text, u32 alignment) {
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_begin);
    gDialogTextAlpha -= sTextFadeAlpha;
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
    print_hud_lut_string_aligned(x, y, text, alignment);
    gDialogTextAlpha += sTextFadeAlpha;
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_end);
}

/**
 * Prints a generic white string with text fade properties.
 */
void print_generic_string_fade(s16 x, s16 y, char *text, u32 alignment) {
    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);
    gDialogTextAlpha -= sTextFadeAlpha;
    set_text_color(255, 255, 255);
    print_generic_string_aligned(x, y, text, alignment);
    gDialogTextAlpha += sTextFadeAlpha;
    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);
}

/**
 * Updates text fade at the top of a menu.
 */
s32 update_text_fade_out(void) {
    if (sFadeOutText == TRUE) {
        sTextFadeAlpha += 50;
        if (sTextFadeAlpha == 250) {
            sFadeOutText = FALSE;
            return TRUE;
        }
    } else {
        if (sTextFadeAlpha > 0) {
            sTextFadeAlpha -= 50;
        }
    }
    return FALSE;
}

/**
 * Prints the amount of stars of a save file.
 * If a save doesn't exist, print "NEW" instead.
 */
LangArray textNew = DEFINE_LANGUAGE_ARRAY(
    "NEW",
    "VIDE",
    "FREI",
    "NEW",
    "NUEVO");

void print_save_file_star_count(s8 fileIndex, s16 x, s16 y) {
    char starCountText[10];

    if (save_file_exists(fileIndex)) {
        s16 starCount = save_file_get_total_star_count(fileIndex,
                                                       COURSE_NUM_TO_INDEX(COURSE_MIN),
                                                       COURSE_NUM_TO_INDEX(COURSE_MAX));

        if (starCount < 100) {
            sprintf(starCountText, "★×%d", starCount);
        } else {
            sprintf(starCountText, "★%d", starCount);
        }
        print_hud_lut_string(x, y, starCountText);
    } else {
        // Print "new" text
        print_hud_lut_string(x, y, LANG_ARRAY(textNew));
    }
}

LangArray textSelectFile = DEFINE_LANGUAGE_ARRAY(
    "SELECT FILE",
    "CHOISIR  FICHIER",
    "WÄHLE SPIEL",
    "ファイルセレクト",
    "ELIGE ARCHIVO");

LangArray textScore = DEFINE_LANGUAGE_ARRAY(
    "SCORE",
    "SCORE",
    "LEISTUNG",
    "スコア",
    "RÉCORDS");

LangArray textCopy = DEFINE_LANGUAGE_ARRAY(
    "COPY",
    "COPIER",
    "KOPIEREN",
    "コピー",
    "COPIAR");

LangArray textErase = DEFINE_LANGUAGE_ARRAY(
    "ERASE",
    "EFFACER",
    "LÖSCHEN",
    "けす",
    "BORRAR");

LangArray textMarioA = DEFINE_LANGUAGE_ARRAY(
    "MARIO A",
    "MARIO A",
    "MARIO A",
    "マリオＡ",
    "MARIO A");

LangArray textMarioB = DEFINE_LANGUAGE_ARRAY(
    "MARIO B",
    "MARIO B",
    "MARIO B",
    "マリオＢ",
    "MARIO B");

LangArray textMarioC = DEFINE_LANGUAGE_ARRAY(
    "MARIO C",
    "MARIO C",
    "MARIO C",
    "マリオＣ",
    "MARIO C");

LangArray textMarioD = DEFINE_LANGUAGE_ARRAY(
    "MARIO D",
    "MARIO D",
    "MARIO D",
    "マリオＤ",
    "MARIO D");

LangArray textSoundModeStereo = DEFINE_LANGUAGE_ARRAY(
    "STEREO",
    "STÉRÉO",
    "STEREO",
    "ステレオ",
    "ESTÉREO");

LangArray textSoundModeMono = DEFINE_LANGUAGE_ARRAY(
    "MONO",
    "MONO",
    "MONO",
    "モノラル",
    "MONO");

LangArray textSoundModeHeadset = DEFINE_LANGUAGE_ARRAY(
    "HEADSET",
    "CASQUE",
    "PHONES",
    "ヘッドホン",
    "CASCOS");

LangArray *textSoundModes[] = {
    &textSoundModeStereo,
    &textSoundModeMono,
#ifdef ENABLE_STEREO_HEADSET_EFFECTS
    &textSoundModeHeadset,
#endif
};

#ifdef MULTILANG
LangArray textOption = DEFINE_LANGUAGE_ARRAY(
    "OPTION",
    "OPTION",
    "OPTIONEN",
    "オプション",
    "OPCIONES");
#endif

/**
 * Prints main menu strings that shows on the yellow background menu screen.
 *
 * In EU this function acts like "print_save_file_strings" because
 * print_main_lang_strings is first called to render the strings for the 4 buttons.
 * Same rule applies for score, copy and erase strings.
 */
void print_main_menu_strings(void) {
    // Print "SELECT FILE" text
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
    print_hud_lut_string_aligned(SCREEN_CENTER_X, 35, LANG_ARRAY(textSelectFile), TEXT_ALIGN_CENTER);
    // Print file star counts
    print_save_file_star_count(SAVE_FILE_A, 92, 78);
    print_save_file_star_count(SAVE_FILE_B, 209, 78);
    print_save_file_star_count(SAVE_FILE_C, 92, 118);
    print_save_file_star_count(SAVE_FILE_D, 209, 118);
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_end);
    // Print menu names
    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);
    set_text_color(255, 255, 255);
    print_generic_string_aligned(67, 39, LANG_ARRAY(textScore), TEXT_ALIGN_CENTER);
    print_generic_string_aligned(130, 39, LANG_ARRAY(textCopy), TEXT_ALIGN_CENTER);
    print_generic_string_aligned(191, 39, LANG_ARRAY(textErase), TEXT_ALIGN_CENTER);
#ifdef MULTILANG
    print_generic_string_aligned(253, 39, LANG_ARRAY(textOption), TEXT_ALIGN_CENTER);
#else
    print_generic_string_aligned(253, 39, LANG_ARRAY(*textSoundModes[sSoundMode]), TEXT_ALIGN_CENTER);
#endif
    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);
    // Print file names
    gSPDisplayList(gDisplayListHead++, dl_menu_ia8_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
    print_menu_generic_string(92, 65, LANG_ARRAY(textMarioA));
    print_menu_generic_string(207, 65, LANG_ARRAY(textMarioB));
    print_menu_generic_string(92, 105, LANG_ARRAY(textMarioC));
    print_menu_generic_string(207, 105, LANG_ARRAY(textMarioD));
    gSPDisplayList(gDisplayListHead++, dl_menu_ia8_text_end);
}

LangArray textCheckFile = DEFINE_LANGUAGE_ARRAY(
    "CHECK FILE",
    "VOIR  SCORE",
    "VON WELCHEM SPIEL",
    "どのスコアをみる？",
    "VER ARCHIVO");

LangArray textNoSavedDataExists = DEFINE_LANGUAGE_ARRAY(
    "NO SAVED DATA EXISTS",
    "AUCUNE SAUVEGARDE DISPONIBLE",
    "KEIN SPIEL VORHANDEN",
    "ファイルにデータがありません",
    "NO HAY DATOS GUARDADOS");

/**
 * Defines IDs for the top message of the score menu and displays it if the ID is called in messageID.
 */
void score_menu_display_message(s8 messageID) {

    switch (messageID) {
        case SCORE_MSG_CHECK_FILE:
            print_hud_lut_string_fade(SCREEN_CENTER_X, 35, LANG_ARRAY(textCheckFile), TEXT_ALIGN_CENTER);
            break;
        case SCORE_MSG_NOSAVE_DATA:
            print_generic_string_fade(SCREEN_CENTER_X, 190, LANG_ARRAY(textNoSavedDataExists), TEXT_ALIGN_CENTER);
            break;
    }
}

#define SUBMENU_LEFT_BUTTON_X 62
#define SUBMENU_MIDDLE_BUTTON_X  160
#define SUBMENU_RIGHT_BUTTON_X 258

#define FADEOUT_TIMER 20

LangArray textReturn = DEFINE_LANGUAGE_ARRAY(
    "RETURN",
    "RETOUR",
    "ZURÜCK",
    "もどる",
    "VOLVER");

LangArray textCopyFileButton = DEFINE_LANGUAGE_ARRAY(
    "COPY FILE",
    "COPIER",
    "KOPIEREN",
    "ファイルコピー",
    "COPIAR ARCHIVO");

LangArray textEraseFileButton = DEFINE_LANGUAGE_ARRAY(
    "ERASE FILE",
    "EFFACER",
    "LÖSCHEN",
    "ファイルけす",
    "BORRAR ARCHIVO");

/**
 * Prints score menu strings that shows on the green background menu screen.
 */
void print_score_menu_strings(void) {

    // Update and print the message at the top of the menu.
    if (sMainMenuTimer == FADEOUT_TIMER) {
        sFadeOutText = TRUE;
    }
    if (update_text_fade_out()) {
        if (sStatusMessageID == SCORE_MSG_CHECK_FILE) {
            sStatusMessageID = SCORE_MSG_NOSAVE_DATA;
        } else {
            sStatusMessageID = SCORE_MSG_CHECK_FILE;
        }
    }
    // Print messageID called above
    score_menu_display_message(sStatusMessageID);

    // Print file star counts
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
    print_save_file_star_count(SAVE_FILE_A, 90, 76);
    print_save_file_star_count(SAVE_FILE_B, 211, 76);
    print_save_file_star_count(SAVE_FILE_C, 90, 119);
    print_save_file_star_count(SAVE_FILE_D, 211, 119);
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_end);

    // Print menu names
    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);
    set_text_color(255, 255, 255);
    print_generic_string_aligned(SUBMENU_LEFT_BUTTON_X, 35, LANG_ARRAY(textReturn), TEXT_ALIGN_CENTER);
    print_generic_string_aligned(SUBMENU_MIDDLE_BUTTON_X, 35, LANG_ARRAY(textCopyFileButton), TEXT_ALIGN_CENTER);
    print_generic_string_aligned(SUBMENU_RIGHT_BUTTON_X, 35, LANG_ARRAY(textEraseFileButton), TEXT_ALIGN_CENTER);
    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);

    // Print file names
    gSPDisplayList(gDisplayListHead++, dl_menu_ia8_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
    print_menu_generic_string(89, 62, LANG_ARRAY(textMarioA));
    print_menu_generic_string(211, 62, LANG_ARRAY(textMarioB));
    print_menu_generic_string(89, 105, LANG_ARRAY(textMarioC));
    print_menu_generic_string(211, 105, LANG_ARRAY(textMarioD));
    gSPDisplayList(gDisplayListHead++, dl_menu_ia8_text_end);
}

LangArray textCopyFile = DEFINE_LANGUAGE_ARRAY(
    "COPY FILE",
    "COPIER  FICHIER",
    "SPIEL KOPIEREN",
    "ファイルコピーする",
    "COPIAR ARCHIVO");

LangArray textCopyItToWhere = DEFINE_LANGUAGE_ARRAY(
    "COPY IT TO WHERE?",
    "COPIER SUR?",
    "WOHIN KOPIEREN?",
    "どこにコピーしますか？",
    "¿COPIARLO A DÓNDE?");

LangArray textCopyCompleted = DEFINE_LANGUAGE_ARRAY(
    "COPYING COMPLETED",
    "COPIE ACHEVEÉ",
    "SPIEL KOPIERT",
    "コピーおわりました",
    "COPIA COMPLETADA");

LangArray textSavedDataExists = DEFINE_LANGUAGE_ARRAY(
    "SAVED DATA EXISTS",
    "SAVEGARDE EXISTANTE",
    "BEREITS BELEGT",
    "ファイルにデータがはいってます",
    "YA EXISTEN DATOS GUARDADOS");

LangArray textNoFileToCopyFrom = DEFINE_LANGUAGE_ARRAY(
    "NO EMPTY FILE",
    "AUCUN FICHIER VIDE",
    "KEIN PLATZ VORHANDEN",
    "からのファイルがありません",
    "NO HAY NINGÚN ARCHIVO VACÍO");

/**
 * Defines IDs for the top message of the copy menu and displays it if the ID is called in messageID.
 */
void copy_menu_display_message(s8 messageID) {

    switch (messageID) {
        case COPY_MSG_MAIN_TEXT:
            if (sAllFilesExist) {
                print_generic_string_fade(SCREEN_CENTER_X, 190, LANG_ARRAY(textNoFileToCopyFrom), TEXT_ALIGN_CENTER);
            } else {
                print_hud_lut_string_fade(SCREEN_CENTER_X, 35, LANG_ARRAY(textCopyFile), TEXT_ALIGN_CENTER);
            }
            break;
        case COPY_MSG_COPY_WHERE:
            print_generic_string_fade(SCREEN_CENTER_X, 190, LANG_ARRAY(textCopyItToWhere), TEXT_ALIGN_CENTER);
            break;
        case COPY_MSG_NOSAVE_EXISTS:
            print_generic_string_fade(SCREEN_CENTER_X, 190, LANG_ARRAY(textNoSavedDataExists), TEXT_ALIGN_CENTER);
            break;
        case COPY_MSG_COPY_COMPLETE:
            print_generic_string_fade(SCREEN_CENTER_X, 190, LANG_ARRAY(textCopyCompleted), TEXT_ALIGN_CENTER);
            break;
        case COPY_MSG_SAVE_EXISTS:
            print_generic_string_fade(SCREEN_CENTER_X, 190, LANG_ARRAY(textSavedDataExists), TEXT_ALIGN_CENTER);
            break;
    }
}

/**
 * Updates messageIDs of the copy menu depending of the copy phase value defined.
 */
void copy_menu_update_message(void) {
    switch (sMainMenuButtons[MENU_BUTTON_COPY]->oMenuButtonActionPhase) {
        case COPY_PHASE_MAIN:
            if (sMainMenuTimer == FADEOUT_TIMER) {
                sFadeOutText = TRUE;
            }
            if (update_text_fade_out() == TRUE) {
                if (sStatusMessageID == COPY_MSG_MAIN_TEXT) {
                    sStatusMessageID = COPY_MSG_NOSAVE_EXISTS;
                } else {
                    sStatusMessageID = COPY_MSG_MAIN_TEXT;
                }
            }
            break;
        case COPY_PHASE_COPY_WHERE:
            if (sMainMenuTimer == FADEOUT_TIMER
                && sStatusMessageID == COPY_MSG_SAVE_EXISTS) {
                sFadeOutText = TRUE;
            }
            if (update_text_fade_out() == TRUE) {
                if (sStatusMessageID != COPY_MSG_COPY_WHERE) {
                    sStatusMessageID = COPY_MSG_COPY_WHERE;
                } else {
                    sStatusMessageID = COPY_MSG_SAVE_EXISTS;
                }
            }
            break;
        case COPY_PHASE_COPY_COMPLETE:
            if (sMainMenuTimer == FADEOUT_TIMER) {
                sFadeOutText = TRUE;
            }
            if (update_text_fade_out() == TRUE) {
                if (sStatusMessageID != COPY_MSG_COPY_COMPLETE) {
                    sStatusMessageID = COPY_MSG_COPY_COMPLETE;
                } else {
                    sStatusMessageID = COPY_MSG_MAIN_TEXT;
                }
            }
            break;
    }
}

LangArray textViewScore = DEFINE_LANGUAGE_ARRAY(
    "CHECK SCORE",
    "SCORE",
    "LEISTUNG",
    "スコアをみる",
    "VER RÉCORDS");

/**
 * Prints copy menu strings that shows on the blue background menu screen.
 */
void print_copy_menu_strings(void) {

    // Update and print the message at the top of the menu.
    copy_menu_update_message();
    // Print messageID called inside a copy_menu_update_message case
    copy_menu_display_message(sStatusMessageID);
    // Print file star counts
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
    print_save_file_star_count(SAVE_FILE_A, 90, 76);
    print_save_file_star_count(SAVE_FILE_B, 211, 76);
    print_save_file_star_count(SAVE_FILE_C, 90, 119);
    print_save_file_star_count(SAVE_FILE_D, 211, 119);
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_end);
    // Print menu names
    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);
    set_text_color(255, 255, 255);
    print_generic_string_aligned(SUBMENU_LEFT_BUTTON_X, 35, LANG_ARRAY(textReturn), TEXT_ALIGN_CENTER);
    print_generic_string_aligned(SUBMENU_MIDDLE_BUTTON_X, 35, LANG_ARRAY(textViewScore), TEXT_ALIGN_CENTER);
    print_generic_string_aligned(SUBMENU_RIGHT_BUTTON_X, 35, LANG_ARRAY(textEraseFileButton), TEXT_ALIGN_CENTER);
    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);
    // Print file names
    gSPDisplayList(gDisplayListHead++, dl_menu_ia8_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
    print_menu_generic_string(89, 62, LANG_ARRAY(textMarioA));
    print_menu_generic_string(211, 62, LANG_ARRAY(textMarioB));
    print_menu_generic_string(89, 105, LANG_ARRAY(textMarioC));
    print_menu_generic_string(211, 105, LANG_ARRAY(textMarioD));
    gSPDisplayList(gDisplayListHead++, dl_menu_ia8_text_end);
}

LangArray textYes = DEFINE_LANGUAGE_ARRAY(
    "YES",
    "OUI",
    "JA",
    "はい",
    "SÍ");

LangArray textNo = DEFINE_LANGUAGE_ARRAY(
    "NO",
    "NON",
    "NEIN",
    "いいえ",
    "NO");

/**
 * Prints the "YES NO" prompt and checks if one of the prompts are hovered to do it's functions.
 */
void print_erase_menu_prompt(s16 x, s16 y) {
    s16 colorFade = gGlobalTimer << 12;

    s16 cursorX = sCursorPos[0] + x + 70.f;
    s16 cursorY = sCursorPos[1] + 120.0f;

    if (cursorX < 169 && cursorX >= 140 &&
        cursorY < 210 && cursorY >= 191) {
        // Fade "YES" string color but keep "NO" gray
        sYesNoColor[0] = sins(colorFade) * 50.0f + 205.0f;
        sYesNoColor[1] = 150;
        sEraseYesNoHoverState = MENU_ERASE_HOVER_YES;
    } else if (cursorX < 218 && cursorX >= 189
        && cursorY < 210 && cursorY >= 191) {
        // Fade "NO" string color but keep "YES" gray
        sYesNoColor[0] = 150;
        sYesNoColor[1] = sins(colorFade) * 50.0f + 205.0f;
        sEraseYesNoHoverState = MENU_ERASE_HOVER_NO;
    } else {
        // Don't fade both strings and keep them gray
        sYesNoColor[0] = 150;
        sYesNoColor[1] = 150;
        sEraseYesNoHoverState = MENU_ERASE_HOVER_NONE;
    }
    // If the cursor is clicked...
    if (sCursorClickingTimer == 2) {
        // ..and is hovering "YES", delete file
        if (sEraseYesNoHoverState == MENU_ERASE_HOVER_YES) {
            play_sound(SOUND_MARIO_WAAAOOOW, gGlobalSoundSource);
#if ENABLE_RUMBLE
            queue_rumble_data(5, 80);
#endif
            sMainMenuButtons[MENU_BUTTON_ERASE]->oMenuButtonActionPhase = ERASE_PHASE_MARIO_ERASED;
            sFadeOutText = TRUE;
            sMainMenuTimer = 0;
            save_file_erase(sSelectedFileIndex);
            sMainMenuButtons[MENU_BUTTON_ERASE_MIN + sSelectedFileIndex]->header.gfx.sharedChild =
                gLoadedGraphNodes[MODEL_MAIN_MENU_MARIO_NEW_BUTTON_FADE];
            sMainMenuButtons[sSelectedFileIndex]->header.gfx.sharedChild =
                gLoadedGraphNodes[MODEL_MAIN_MENU_MARIO_NEW_BUTTON_FADE];
            sEraseYesNoHoverState = MENU_ERASE_HOVER_NONE;
            // ..and is hovering "NO", return back to main phase
        } else if (sEraseYesNoHoverState == MENU_ERASE_HOVER_NO) {
            play_sound(SOUND_MENU_CLICK_FILE_SELECT, gGlobalSoundSource);
#if ENABLE_RUMBLE
            queue_rumble_data(5, 80);
#endif
            sMainMenuButtons[MENU_BUTTON_ERASE_MIN + sSelectedFileIndex]->oMenuButtonState =
                MENU_BUTTON_STATE_ZOOM_OUT;
            sMainMenuButtons[MENU_BUTTON_ERASE]->oMenuButtonActionPhase = ERASE_PHASE_MAIN;
            sFadeOutText = TRUE;
            sMainMenuTimer = 0;
            sEraseYesNoHoverState = MENU_ERASE_HOVER_NONE;
        }
    }

    // Print "YES NO" strings
    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);
    set_text_color(sYesNoColor[0], sYesNoColor[0], sYesNoColor[0]);
    print_generic_string(x + 56, y, LANG_ARRAY(textYes));
    set_text_color(sYesNoColor[1], sYesNoColor[1], sYesNoColor[1]);
    print_generic_string(x + 98, y, LANG_ARRAY(textNo));
    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);
}

LangArray textEraseFile = DEFINE_LANGUAGE_ARRAY(
    "ERASE FILE",
    "EFFACER  FICHIER",
    "SPIEL LÖSCHEN",
    "ファイルけす",
    "BORRAR ARCHIVO");

LangArray textSure = DEFINE_LANGUAGE_ARRAY(
    "SURE?",
    "OK?",
    "SICHER?",
    "ほんと？",
    "¿SEGURO?");

LangArray textMarioXJustErased = DEFINE_LANGUAGE_ARRAY(
    "MARIO %s JUST ERASED",
    "MARIO %s EFFACÉ",
    "MARIO %s GELÖSCHT",
    "マリオ%sをけしました",
    "MARIO %s ELIMINADO");

/**
 * Defines IDs for the top message of the erase menu and displays it if the ID is called in messageID.
 */
void erase_menu_display_message(s8 messageID) {
    char str[50];
    switch (messageID) {
        case ERASE_MSG_MAIN_TEXT:
            print_hud_lut_string_fade(SCREEN_CENTER_X, 35, LANG_ARRAY(textEraseFile), TEXT_ALIGN_CENTER);
            break;
        case ERASE_MSG_PROMPT:
            print_generic_string_fade(90, 190, LANG_ARRAY(textSure), TEXT_ALIGN_LEFT);
            print_erase_menu_prompt(90, 190); // YES NO, has functions for it too
            break;
        case ERASE_MSG_NOSAVE_EXISTS:
            print_generic_string_fade(SCREEN_CENTER_X, 190, LANG_ARRAY(textNoSavedDataExists), TEXT_ALIGN_CENTER);
            break;
        case ERASE_MSG_MARIO_ERASED:
            string_format_file_letter(str, LANG_ARRAY(textMarioXJustErased), sSelectedFileIndex);
            print_generic_string_fade(SCREEN_CENTER_X, 190, str, TEXT_ALIGN_CENTER);
            break;
        case ERASE_MSG_SAVE_EXISTS: // unused
            print_generic_string_fade(SCREEN_CENTER_X, 190, LANG_ARRAY(textSavedDataExists), TEXT_ALIGN_CENTER);
            break;
    }
}

/**
 * Updates messageIDs of the erase menu depending of the erase phase value defined.
 */
void erase_menu_update_message(void) {
    switch (sMainMenuButtons[MENU_BUTTON_ERASE]->oMenuButtonActionPhase) {
        case ERASE_PHASE_MAIN:
            if (sMainMenuTimer == FADEOUT_TIMER
                && sStatusMessageID == ERASE_MSG_NOSAVE_EXISTS) {
                sFadeOutText = TRUE;
            }
            if (update_text_fade_out() == TRUE) {
                if (sStatusMessageID == ERASE_MSG_MAIN_TEXT) {
                    sStatusMessageID = ERASE_MSG_NOSAVE_EXISTS;
                } else {
                    sStatusMessageID = ERASE_MSG_MAIN_TEXT;
                }
            }
            break;
        case ERASE_PHASE_PROMPT:
            if (update_text_fade_out() == TRUE) {
                if (sStatusMessageID != ERASE_MSG_PROMPT) {
                    sStatusMessageID = ERASE_MSG_PROMPT;
                }
                sCursorPos[0] = 43.0f;
                sCursorPos[1] = 80.0f;
            }
            break;
        case ERASE_PHASE_MARIO_ERASED:
            if (sMainMenuTimer == FADEOUT_TIMER) {
                sFadeOutText = TRUE;
            }
            if (update_text_fade_out() == TRUE) {
                if (sStatusMessageID != ERASE_MSG_MARIO_ERASED) {
                    sStatusMessageID = ERASE_MSG_MARIO_ERASED;
                } else {
                    sStatusMessageID = ERASE_MSG_MAIN_TEXT;
                }
            }
            break;
    }
}

/**
 * Prints erase menu strings that shows on the red background menu screen.
 */
void print_erase_menu_strings(void) {

    // Update and print the message at the top of the menu.
    erase_menu_update_message();

    // Print messageID called inside a erase_menu_update_message case
    erase_menu_display_message(sStatusMessageID);

    // Print file star counts
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
    print_save_file_star_count(SAVE_FILE_A, 90, 76);
    print_save_file_star_count(SAVE_FILE_B, 211, 76);
    print_save_file_star_count(SAVE_FILE_C, 90, 119);
    print_save_file_star_count(SAVE_FILE_D, 211, 119);
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_end);

    // Print menu names
    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);
    set_text_color(255, 255, 255);
    print_generic_string_aligned(SUBMENU_LEFT_BUTTON_X, 35, LANG_ARRAY(textReturn), TEXT_ALIGN_CENTER);
    print_generic_string_aligned(SUBMENU_MIDDLE_BUTTON_X, 35, LANG_ARRAY(textViewScore), TEXT_ALIGN_CENTER);
    print_generic_string_aligned(SUBMENU_RIGHT_BUTTON_X, 35, LANG_ARRAY(textCopyFileButton), TEXT_ALIGN_CENTER);
    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);

    // Print file names
    gSPDisplayList(gDisplayListHead++, dl_menu_ia8_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
    print_menu_generic_string(89, 62, LANG_ARRAY(textMarioA));
    print_menu_generic_string(211, 62, LANG_ARRAY(textMarioB));
    print_menu_generic_string(89, 105, LANG_ARRAY(textMarioC));
    print_menu_generic_string(211, 105, LANG_ARRAY(textMarioD));
    gSPDisplayList(gDisplayListHead++, dl_menu_ia8_text_end);
}

LangArray textSoundSelect = DEFINE_LANGUAGE_ARRAY(
    "SOUND SELECT",
    "SELECTION SON",
    "WÄHLE SOUND",
    "サウンドセレクト",
    "MODO DE SONIDO");

#ifdef MULTILANG
LangArray textLanguageSelect = DEFINE_LANGUAGE_ARRAY(
    "LANGUAGE SELECT",
    "SELECTION LANGUE",
    "WÄHLE SPRACHE",
    "ランゲージセレクト",
    "IDIOMA");

LangArray textLanguage = DEFINE_LANGUAGE_ARRAY(
    "ENGLISH",
    "FRANÇAIS",
    "DEUTSCH",
    "にほんご",
    "ESPAÑOL");

#define SOUND_LABEL_Y 141
#define LANGUAGE_SELECT_Y 80
#else
#define SOUND_LABEL_Y 87
#endif

#ifdef ENABLE_STEREO_HEADSET_EFFECTS
#define OPTION_LABEL_SPACING 74
#else
#define OPTION_LABEL_SPACING 99
#endif

#define OPTION_LABEL_START_X (SCREEN_CENTER_X - ((ARRAY_COUNT(textSoundModes) - 1) * OPTION_LABEL_SPACING / 2))

/**
 * Prints sound mode menu strings that shows on the purple background menu screen.
 *
 * With multilang, this function acts like "print_option_mode_menu_strings" because of languages.
 */
void print_sound_mode_menu_strings(void) {
    s32 mode;
    s32 textX;

    // Print "SOUND SELECT" text
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);

    print_hud_lut_string(47, 32, LANG_ARRAY(textSoundSelect));
#ifdef MULTILANG
    print_hud_lut_string(47, 110, LANG_ARRAY(textLanguageSelect));
#endif

    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_end);

    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);

    // Print sound mode names
    for (mode = 0, textX = OPTION_LABEL_START_X; mode < ARRAY_COUNT(textSoundModes); textX += OPTION_LABEL_SPACING, mode++) {
        if (mode == sSoundMode) {
            set_text_color(255, 255, 255);
        } else {
            set_text_color(0, 0, 0);
        }
        print_generic_string_aligned(textX, SOUND_LABEL_Y, LANG_ARRAY(*textSoundModes[mode]), TEXT_ALIGN_CENTER);
    }

#ifdef MULTILANG
    // Handle changing the selected language
    if (sCursorClickingTimer == 2) {
        s16 cursorX = sCursorPos[0] + SCREEN_CENTER_X;
        s16 cursorY = sCursorPos[1] + SCREEN_CENTER_Y;

        s8 oldSelectedLanguageIndex = sSelectedLanguageIndex;

        if (cursorY < LANGUAGE_SELECT_Y + 20 && cursorY >= LANGUAGE_SELECT_Y) {
            if (cursorX < SCREEN_CENTER_X - 40 && cursorX >= SCREEN_CENTER_X - 60) {
                sSelectedLanguageIndex--;
            } else if (cursorX < SCREEN_CENTER_X + 60 && cursorX >= SCREEN_CENTER_X + 40) {
                sSelectedLanguageIndex++;
            }
            // Update language if the language has been changed
            if (sSelectedLanguageIndex != oldSelectedLanguageIndex) {
                play_sound(SOUND_MENU_CHANGE_SELECT, gGlobalSoundSource);
                sSelectedLanguageIndex = (sSelectedLanguageIndex + LANGUAGE_COUNT) % LANGUAGE_COUNT;
                multilang_set_language(gDefinedLanguages[sSelectedLanguageIndex]);
            }
        }
    }

    set_text_color(255, 255, 255);
    // Print current language
    print_generic_string_aligned(SCREEN_CENTER_X,      LANGUAGE_SELECT_Y, textLanguage[gInGameLanguage], TEXT_ALIGN_CENTER);
    print_generic_string_aligned(SCREEN_CENTER_X - 50, LANGUAGE_SELECT_Y, "◀", TEXT_ALIGN_CENTER);
    print_generic_string_aligned(SCREEN_CENTER_X + 50, LANGUAGE_SELECT_Y, "▶", TEXT_ALIGN_CENTER);

    // Print return text
    print_generic_string(184, 29, LANG_ARRAY(textReturn));
#endif

    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);
}

/**
 * Prints castle secret stars collected in a score menu save file.
 */
void print_score_file_castle_secret_stars(s8 fileIndex, s16 x, s16 y) {
    char secretStarsText[20];
    char secretStarsNum[8];
    // Print number of castle secret stars
    format_int_to_string(secretStarsNum, save_file_get_total_star_count(fileIndex,
                                                                  COURSE_NUM_TO_INDEX(COURSE_BONUS_STAGES),
                                                                  COURSE_NUM_TO_INDEX(COURSE_MAX)));
    sprintf(secretStarsText, "★×%s", secretStarsNum);
    print_menu_generic_string(x, y, secretStarsText);
}

LangArray text4Dashes = DEFINE_LANGUAGE_ARRAY(
    "----",
    "----",
    "----",
    "ーーーー",
    "----");

LangArray textMarioFace = DEFINE_LANGUAGE_ARRAY(
    "{}%s",
    "{}%s",
    "{}%s",
    "マリオ%s",
    "{}%s");

/**
 * Prints course coins collected in a score menu save file.
 */
void print_score_file_course_coin_score(s8 fileIndex, s16 courseIndex, s16 x, s16 y) {
    char str[20];
    char coinScoreText[10];
    u8 stars = save_file_get_star_flags(fileIndex, courseIndex);

    // MYSCORE
    if (sScoreFileCoinScoreMode == 0) {
        // Print coin score
        format_int_to_string(coinScoreText, save_file_get_course_coin_score(fileIndex, courseIndex));
        sprintf(str, "✪×%s", coinScoreText);
        print_menu_generic_string(x + 25, y, str);
        // If collected, print 100 coin star
        if (stars & STAR_FLAG_ACT_100_COINS) {
            print_menu_generic_string(x + 70, y, "★");
        }
    }
    // HISCORE
    else {
        u16 coinScoreFile;
        // Print coin highscore
        format_int_to_string(coinScoreText, (u16) save_file_get_max_coin_score(courseIndex) & 0xFFFF);
        sprintf(str, "✪×%s", coinScoreText);
        print_menu_generic_string(x + 18, y, str);
        // Print coin highscore file
        coinScoreFile = (save_file_get_max_coin_score(courseIndex) >> 16) & 0xFFFF;
        if (coinScoreFile == 0) {
            print_menu_generic_string(x + 60, y, LANG_ARRAY(text4Dashes));
        } else {
            string_format_file_letter(str, LANG_ARRAY(textMarioFace), coinScoreFile - 1);
            print_menu_generic_string(x + 60, y, str);
        }
    }
}

/**
 * Prints stars collected in a score menu save file.
 */
void print_score_file_star_score(s8 fileIndex, s16 courseIndex, s16 x, s16 y) {
    s16 i = 0;
    char starScoreText[30];
    char *entries[6];
    u8 stars = save_file_get_star_flags(fileIndex, courseIndex);
    s8 starCount = save_file_get_course_star_count(fileIndex, courseIndex);
    // Don't count 100 coin star
    if (stars & STAR_FLAG_ACT_100_COINS) {
        starCount--;
    }
    // Add 1 star character for every star collected
    for (i = 0; i < starCount; i++) {
        entries[i] = "★";
    }
    for (i = starCount; i < 6; i++) {
        entries[i] = "";
    }
    sprintf(starScoreText, "%s%s%s%s%s%s", entries[0], entries[1], entries[2], entries[3], entries[4], entries[5]);
    print_menu_generic_string(x, y, starScoreText);
}

LangArray textScoreMenuMarioX = DEFINE_LANGUAGE_ARRAY(
    "MARIO %c",
    "MARIO %c",
    "MARIO %c",
    "マリオ %c",
    "MARIO %c");

LangArray textHiScore = DEFINE_LANGUAGE_ARRAY(
    "HI SCORE",
    "MEILLEUR SCORE",
    "BESTLEISTUNG",
    "ハイスコア",
    "RÉCORDS");

extern LangArray textMyScore;

/**
 * Prints save file score strings that shows when a save file is chosen inside the score menu.
 */
void print_save_file_scores(s8 fileIndex) {
    u32 i;
    char str[20];
    char fileLetter;

#ifndef MULTILANG
    const char **levelNameTable = segmented_to_virtual(seg2_course_name_table);
#else
    const char ***levelNameLanguageTable = segmented_to_virtual(course_strings_language_table);
    const char **levelNameTable = segmented_to_virtual(levelNameLanguageTable[gInGameLanguage]);
#endif

    // Print file name at top
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
    fileLetter = 'A' + fileIndex;
    sprintf(str, LANG_ARRAY(textScoreMenuMarioX), fileLetter);
    print_hud_lut_string(25, 15, str);

    // Print save file star count at top
    print_save_file_star_count(fileIndex, 124, 15);
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_end);
    // Print course scores
    gSPDisplayList(gDisplayListHead++, dl_menu_ia8_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);

    for ((i = 0); (i < COURSE_STAGES_MAX); (i++)) {
        s32 lineY = 35 + (12 * i);
        format_int_to_string(str, i + 1);
        print_menu_generic_string(41, lineY, segmented_to_virtual(levelNameTable[i]));
        print_menu_generic_string_aligned(37, lineY, str, TEXT_ALIGN_RIGHT);
        print_score_file_star_score(       fileIndex, i, 171, lineY);
        print_score_file_course_coin_score(fileIndex, i, 213, lineY);
    }

    // Print castle secret stars text
    print_menu_generic_string(41, 215, segmented_to_virtual(levelNameTable[25]));
    // Print castle secret stars score
    print_score_file_castle_secret_stars(fileIndex, 171, 215);

    // Print current coin score mode
    if (sScoreFileCoinScoreMode == 0) {
        print_menu_generic_string_aligned(262, 24, LANG_ARRAY(textMyScore), TEXT_ALIGN_CENTER);
    } else {
        print_menu_generic_string_aligned(262, 24, LANG_ARRAY(textHiScore), TEXT_ALIGN_CENTER);
    }

    gSPDisplayList(gDisplayListHead++, dl_menu_ia8_text_end);
}

/**
 * Prints file select strings depending on the menu selected.
 * Also checks if all saves exists and defines text and main menu timers.
 */
void print_file_select_strings(void) {
    create_dl_ortho_matrix();
    switch (sSelectedButtonID) {
        case MENU_BUTTON_NONE:         print_main_menu_strings();                               break;
        case MENU_BUTTON_SCORE:        print_score_menu_strings(); sScoreFileCoinScoreMode = 0; break;
        case MENU_BUTTON_COPY:         print_copy_menu_strings();                               break;
        case MENU_BUTTON_ERASE:        print_erase_menu_strings();                              break;
        case MENU_BUTTON_SCORE_FILE_A: print_save_file_scores(SAVE_FILE_A); break;
        case MENU_BUTTON_SCORE_FILE_B: print_save_file_scores(SAVE_FILE_B); break;
        case MENU_BUTTON_SCORE_FILE_C: print_save_file_scores(SAVE_FILE_C); break;
        case MENU_BUTTON_SCORE_FILE_D: print_save_file_scores(SAVE_FILE_D); break;
        case MENU_BUTTON_SOUND_MODE:   print_sound_mode_menu_strings();     break;
    }
    // If all 4 save file exists, define true to sAllFilesExist to prevent more copies in copy menu
    if (save_file_exists(SAVE_FILE_A) == TRUE && save_file_exists(SAVE_FILE_B) == TRUE &&
        save_file_exists(SAVE_FILE_C) == TRUE && save_file_exists(SAVE_FILE_D) == TRUE) {
        sAllFilesExist = TRUE;
    } else {
        sAllFilesExist = FALSE;
    }
    // Timers for menu alpha text and the main menu itself
    if (gDialogTextAlpha < 250) {
        gDialogTextAlpha += 10;
    }
    if (sMainMenuTimer < 1000) {
        sMainMenuTimer++;
    }
}

/**
 * Geo function that prints file select strings and the cursor.
 */
Gfx *geo_file_select_strings_and_menu_cursor(s32 callContext, UNUSED struct GraphNode *node, UNUSED Mat4 mtx) {
    if (callContext == GEO_CONTEXT_RENDER) {
        print_file_select_strings();
        print_menu_cursor();
    }
    return NULL;
}

/**
 * Initiates file select values after Mario Screen.
 * Relocates cursor position of the last save if the game goes back to the Mario Screen
 * either completing a course choosing "SAVE & QUIT" or having a game over.
 */
s32 lvl_init_menu_values_and_cursor_pos(UNUSED s32 arg, UNUSED s32 unused) {
    sSelectedButtonID = MENU_BUTTON_NONE;
    sCurrentMenuLevel = MENU_LAYER_MAIN;
    gDialogTextAlpha = 0;
    // Place the cursor over the save file that was being played.
    // gCurrSaveFileNum is 1 by default when the game boots, as such
    // the cursor will point on Mario A save file.
    switch (gCurrSaveFileNum) {
        case SAVE_FILE_NUM_A: sCursorPos[0] = -94.0f; sCursorPos[1] = 46.0f; break;
        case SAVE_FILE_NUM_B: sCursorPos[0] =  24.0f; sCursorPos[1] = 46.0f; break;
        case SAVE_FILE_NUM_C: sCursorPos[0] = -94.0f; sCursorPos[1] =  5.0f; break;
        case SAVE_FILE_NUM_D: sCursorPos[0] =  24.0f; sCursorPos[1] =  5.0f; break;
    }
    sClickPos[0] = -10000;
    sClickPos[1] = -10000;
    sCursorClickingTimer = 0;
    sSelectedFileNum = 0;
    sSelectedFileIndex = MENU_BUTTON_NONE;
    sFadeOutText = FALSE;
    sStatusMessageID = 0;
    sTextFadeAlpha = 0;
    sMainMenuTimer = 0;
    sEraseYesNoHoverState = MENU_ERASE_HOVER_NONE;
    sSoundMode = save_file_get_sound_mode();
#ifdef MULTILANG
    sSelectedLanguageIndex = get_language_index(gInGameLanguage);

    for (u32 fileNum = 0; fileNum < NUM_SAVE_FILES; fileNum++) {
        if (save_file_exists(fileNum) == TRUE) {
            sOpenLangSettings = FALSE;
            break;
        } else {
            sOpenLangSettings = TRUE;
        }
    }
#endif
    gCurrLevelNum = LEVEL_UNKNOWN_1;
    return 0;
}

/**
 * Updates file select menu button objects so they can be interacted.
 * When a save file is selected, it returns fileNum value
 * defined in load_main_menu_save_file.
 */
s32 lvl_update_obj_and_load_file_selected(UNUSED s32 arg, UNUSED s32 unused) {
    area_update_objects();
    return sSelectedFileNum;
}

STATIC_ASSERT(SOUND_MODE_COUNT == MENU_BUTTON_SOUND_OPTION_MAX - MENU_BUTTON_SOUND_OPTION_MIN, "Mismatch between number of sound modes in audio code and file select!");
