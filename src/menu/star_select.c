#include <PR/ultratypes.h>

#include "audio/external.h"
#include "behavior_data.h"
#include "engine/behavior_script.h"
#include "engine/graph_node.h"
#include "game/area.h"
#include "game/game_init.h"
#include "game/ingame_menu.h"
#include "game/level_update.h"
#include "game/memory.h"
#include "game/object_helpers.h"
#include "game/object_list_processor.h"
#include "game/save_file.h"
#include "game/segment2.h"
#include "game/segment7.h"
#include "game/rumble_init.h"
#include "sm64.h"
#include "star_select.h"
#include "game/main.h"

/**
 * @file star_select.c
 * This file implements how the star select screen (act selector) function.
 * That includes handles what stars can be selected, star selector types,
 * strings, act values, and star selector model rendering if a star is collected or not.
 */

// Star Selector count models printed in the act selector menu.
static struct Object *sStarSelectorModels[8];

// The act the course is loaded as, affects whether some objects spawn.
static s8 sLoadedActNum;

// Number of obtained stars, excluding the coin star.
static u8 sObtainedStars;

// Total number of stars that appear in the act selector menu.
static s8 sVisibleStars;

// Act selected when the act menu is first opened.
static u8 sInitSelectedActNum;

// Index value of the act selected in the act menu.
static s8 sSelectedActIndex = 0;

// Index value of the star that is selectable in the act menu.
// Excluding the next star, it doesn't count other transparent stars.
static s8 sSelectableStarIndex = 0;

// Act Selector menu timer that keeps counting until you choose an act.
static s32 sActSelectorMenuTimer = 0;

#ifdef WIDE
#define ACT_SELECT_WIDESCREEN_SCALE (gConfig.widescreen ? (4.f / 3) : 1.f)
#else
#define ACT_SELECT_WIDESCREEN_SCALE 1.f
#endif

/**
 * Act Selector Star Type Loop Action
 * Defines a select type for a star in the act selector.
 */
void bhv_act_selector_star_type_loop(void) {
    switch (gCurrentObject->oStarSelectorType) {
        // If a star is not selected, don't rotate or change size
        case STAR_SELECTOR_NOT_SELECTED:
            gCurrentObject->oStarSelectorSize -= 0.1f;
            if (gCurrentObject->oStarSelectorSize < 1.0f) {
                gCurrentObject->oStarSelectorSize = 1.0f;
            }
            gCurrentObject->oFaceAngleYaw = 0;
            break;
        // If a star is selected, rotate and slightly increase size
        case STAR_SELECTOR_SELECTED:
            gCurrentObject->oStarSelectorSize += 0.1f;
            if (gCurrentObject->oStarSelectorSize > 1.3f) {
                gCurrentObject->oStarSelectorSize = 1.3f;
            }
            gCurrentObject->oFaceAngleYaw += 0x800;
            break;
        // If the 100 coin star is selected, rotate
        case STAR_SELECTOR_100_COINS:
            gCurrentObject->oFaceAngleYaw += 0x800;
            break;
    }
    // Scale act selector stars depending of the type selected
    cur_obj_scale(gCurrentObject->oStarSelectorSize);
    // Unused timer, only referenced here. Probably replaced by sActSelectorMenuTimer
    gCurrentObject->oStarSelectorTimer++;
}

/**
 * Renders the 100 coin star with an special star selector type.
 */
void render_100_coin_star(u8 stars) {
    if (stars & STAR_FLAG_ACT_100_COINS) {
        // If the 100 coin star has been collected, create a new star selector next to the coin score.
        sStarSelectorModels[6] = spawn_object_abs_with_rot(o, 0, MODEL_STAR,
                                                        bhvActSelectorStarType, 370 * ACT_SELECT_WIDESCREEN_SCALE, 24, -300, 0, 0, 0);

        sStarSelectorModels[6]->oStarSelectorSize = 0.8f;
        sStarSelectorModels[6]->oStarSelectorType = STAR_SELECTOR_100_COINS;
    }
}

/**
 * Act Selector Init Action
 * Checks how many stars has been obtained in a course, to render
 * the correct star models, the 100 coin star and also handles
 * checks of what star should be next in sInitSelectedActNum.
 */
void bhv_act_selector_init(void) {
    s16 i = 0;
    s32 selectorModelIDs[10];
    u8 stars = save_file_get_star_flags(gCurrSaveFileNum - 1, COURSE_NUM_TO_INDEX(gCurrCourseNum));

    sVisibleStars = 0;
    while (i != sObtainedStars) {
        if (stars & (1 << sVisibleStars)) { // Star has been collected
            selectorModelIDs[sVisibleStars] = MODEL_STAR;
            i++;
        } else { // Star has not been collected
            selectorModelIDs[sVisibleStars] = MODEL_TRANSPARENT_STAR;
            // If this is the first star that has not been collected, set
            // the default selection to this star.
            if (sInitSelectedActNum == 0) {
                sInitSelectedActNum = sVisibleStars + 1;
                sSelectableStarIndex = sVisibleStars;
            }
        }
        sVisibleStars++;
    }

    // If the stars have been collected in order so far, show the next star.
    if (sVisibleStars == sObtainedStars && sVisibleStars != 6) {
        selectorModelIDs[sVisibleStars] = MODEL_TRANSPARENT_STAR;
        sInitSelectedActNum = sVisibleStars + 1;
        sSelectableStarIndex = sVisibleStars;
        sVisibleStars++;
    }

    // If all stars have been collected, set the default selection to the last star.
    if (sObtainedStars == 6) {
        sInitSelectedActNum = sVisibleStars;
    }

    //! Useless, since sInitSelectedActNum has already been set in this
    //! scenario by the code that shows the next uncollected star.
    if (sObtainedStars == 0) {
        sInitSelectedActNum = 1;
    }

    // Render star selector objects
    for (i = 0; i < sVisibleStars; i++) {
        sStarSelectorModels[i] =
            spawn_object_abs_with_rot(o, 0, selectorModelIDs[i], bhvActSelectorStarType,
                                    (75 + (sVisibleStars * -75) + (i * 152)) * ACT_SELECT_WIDESCREEN_SCALE, 248, -300, 0, 0, 0);
        sStarSelectorModels[i]->oStarSelectorSize = 1.0f;
    }

    render_100_coin_star(stars);
}

/**
 * Act Selector Loop Action
 * Handles star selector scrolling depending of what stars are
 * selectable, whenever all 6 stars are obtained or not.
 * Also handles 2 star selector types whenever the star is selected
 * or not, the types are defined in bhv_act_selector_star_type_loop.
 */
void bhv_act_selector_loop(void) {
    s8 i;
    u8 starIndexCounter;
    u8 stars = save_file_get_star_flags(gCurrSaveFileNum - 1, COURSE_NUM_TO_INDEX(gCurrCourseNum));

    if (sObtainedStars != 6) {
        // Sometimes, stars are not selectable even if they appear on the screen.
        // This code filters selectable and non-selectable stars.
        sSelectedActIndex = 0;
        handle_menu_scrolling(MENU_SCROLL_HORIZONTAL, &sSelectableStarIndex, 0, sObtainedStars);
        starIndexCounter = sSelectableStarIndex;
        for (i = 0; i < sVisibleStars; i++) {
            // Can the star be selected (is it either already completed or the first non-completed mission)
            if ((stars & (1 << i)) || i + 1 == sInitSelectedActNum) {
                if (starIndexCounter == 0) { // We have reached the sSelectableStarIndex-th selectable star.
                    sSelectedActIndex = i;
                    break;
                }
                starIndexCounter--;
            }
        }
    } else {
        // If all stars are collected then they are all selectable.
        handle_menu_scrolling(MENU_SCROLL_HORIZONTAL, &sSelectableStarIndex, 0, sVisibleStars - 1);
        sSelectedActIndex = sSelectableStarIndex;
    }

    // Star selector type handler
    for (i = 0; i < sVisibleStars; i++) {
        if (sSelectedActIndex == i) {
            sStarSelectorModels[i]->oStarSelectorType = STAR_SELECTOR_SELECTED;
        } else {
            sStarSelectorModels[i]->oStarSelectorType = STAR_SELECTOR_NOT_SELECTED;
        }
    }
}

/**
 * Print the course number selected with the wood rgba16 course texture.
 */
void print_course_number(void) {
    char courseNum[4];

    create_dl_translation_matrix(MENU_MTX_PUSH, 158.0f, 81.0f, 0.0f);

    // Full wood texture in JP & US, lower part of it on EU
    gSPDisplayList(gDisplayListHead++, dl_menu_rgba16_wood_course);

#ifdef MULTILANG
    // Change upper part of the wood texture depending of the language defined
    switch (gInGameLanguage) {
        case LANGUAGE_ENGLISH:
#ifdef ENABLE_JAPANESE
        case LANGUAGE_JAPANESE:
#endif
            gSPDisplayList(gDisplayListHead++, dl_menu_texture_course_upper);
            break;
#ifdef ENABLE_FRENCH
        case LANGUAGE_FRENCH:
            gSPDisplayList(gDisplayListHead++, dl_menu_texture_niveau_upper);
            break;
#endif
#ifdef ENABLE_GERMAN
        case LANGUAGE_GERMAN:
            gSPDisplayList(gDisplayListHead++, dl_menu_texture_kurs_upper);
            break;
#endif
#ifdef ENABLE_SPANISH
        case LANGUAGE_SPANISH:
            gSPDisplayList(gDisplayListHead++, dl_menu_texture_nivel_upper);
            break;
#endif
    }

    gSPDisplayList(gDisplayListHead++, dl_menu_rgba16_wood_course_end);
#endif

    gSPPopMatrix(gDisplayListHead++, G_MTX_MODELVIEW);
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, 255);

    sprintf(courseNum, "%d", gCurrCourseNum);
    print_hud_lut_string_aligned(157, 158, courseNum, TEXT_ALIGN_CENTER);

    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_end);
}

LangArray textActMyScore = DEFINE_LANGUAGE_ARRAY(
    "MYSCORE",
    "MON SCORE",
    "LEISTUNG",
    "マイスコア",
    "MI RÉCORD");
/**
 * Print act selector strings, some with special checks.
 */
void print_act_selector_strings(void) {
    char **levelNameTbl = segmented_to_virtual(gLanguageTables[gInGameLanguage].course_name_table);
    char *currLevelName = segmented_to_virtual(levelNameTbl[COURSE_NUM_TO_INDEX(gCurrCourseNum)]);
    char **actNameTbl = segmented_to_virtual(gLanguageTables[gInGameLanguage].act_name_table);
    char *selectedActName;
    s8 i;

    create_dl_ortho_matrix();
    gDialogTextAlpha = 255;

    // Print the coin highscore.
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 255, 255, 255, gDialogTextAlpha);
    print_hud_my_score_coins(1, gCurrSaveFileNum - 1, COURSE_NUM_TO_INDEX(gCurrCourseNum), 155, 106);
    gSPDisplayList(gDisplayListHead++, dl_rgba16_text_end);

    gSPDisplayList(gDisplayListHead++, dl_ia_text_begin);
    set_text_color(0, 0, 0);
    // Print the "MY SCORE" text if the coin score is more than 0
    if (save_file_get_course_coin_score(gCurrSaveFileNum - 1, COURSE_NUM_TO_INDEX(gCurrCourseNum)) != 0) {
        print_generic_string_aligned(145, 118, LANG_ARRAY(textActMyScore), TEXT_ALIGN_RIGHT);
    }

    print_generic_string_aligned(SCREEN_CENTER_X, 33, currLevelName, TEXT_ALIGN_CENTER);

    gSPDisplayList(gDisplayListHead++, dl_ia_text_end);

    print_course_number();

    gSPDisplayList(gDisplayListHead++, dl_menu_ia8_text_begin);
    gDPSetEnvColor(gDisplayListHead++, 0, 0, 0, gDialogTextAlpha);
    // Print the name of the selected act.
    if (sVisibleStars != 0) {
        selectedActName = segmented_to_virtual(actNameTbl[COURSE_NUM_TO_INDEX(gCurrCourseNum) * 6 + sSelectedActIndex]);

        print_menu_generic_string_aligned(SCREEN_CENTER_X, 81, selectedActName, TEXT_ALIGN_CENTER);
    }

    // Print the numbers above each star.
    for (i = 1; i <= sVisibleStars; i++) {
        char str[4];
        format_int_to_string(str, i);
        print_menu_generic_string_aligned(SCREEN_CENTER_X + (i*2 - sVisibleStars - 1) * 17, 38, str, TEXT_ALIGN_CENTER);
    }

    gSPDisplayList(gDisplayListHead++, dl_menu_ia8_text_end);
 }

/**
 * Geo function that Print act selector strings.
 */
Gfx *geo_act_selector_strings(s16 callContext, UNUSED struct GraphNode *node, UNUSED void *context) {
    if (callContext == GEO_CONTEXT_RENDER) {
        print_act_selector_strings();
    }
    return NULL;
}

/**
 * Initiates act selector values before entering a main course.
 * Also load how much stars a course has, without counting the 100 coin star.
 */
s32 lvl_init_act_selector_values_and_stars(UNUSED s32 arg, UNUSED s32 unused) {
    u8 stars = save_file_get_star_flags(gCurrSaveFileNum - 1, COURSE_NUM_TO_INDEX(gCurrCourseNum));

    sLoadedActNum = 0;
    sInitSelectedActNum = 0;
    sVisibleStars = 0;
    sActSelectorMenuTimer = 0;
    sObtainedStars =
        save_file_get_course_star_count(gCurrSaveFileNum - 1, COURSE_NUM_TO_INDEX(gCurrCourseNum));

    // Don't count 100 coin star
    if (stars & STAR_FLAG_ACT_100_COINS) {
        sObtainedStars--;
    }

    return 0;
}

/**
 * Loads act selector button actions with selected act value checks.
 * Also updates objects and returns act number selected after is chosen.
 */
s32 lvl_update_obj_and_load_act_button_actions(UNUSED s32 arg, UNUSED s32 unused) {
    if (sActSelectorMenuTimer > 10) {
        // If any of these buttons are pressed, play sound and go to course act
        if ((gPlayer1Controller->buttonPressed & (A_BUTTON | START_BUTTON | B_BUTTON | Z_TRIG))) {
            play_sound(SOUND_MENU_STAR_SOUND_LETS_A_GO, gGlobalSoundSource);
#if ENABLE_RUMBLE
            queue_rumble_data(60, 70);
            queue_rumble_decay(1);
#endif
            if (sInitSelectedActNum >= sSelectedActIndex + 1) {
                sLoadedActNum = sSelectedActIndex + 1;
            } else {
                sLoadedActNum = sInitSelectedActNum;
            }
            gDialogCourseActNum = sSelectedActIndex + 1;
        }
    }

    area_update_objects();
    sActSelectorMenuTimer++;
    return sLoadedActNum;
}
