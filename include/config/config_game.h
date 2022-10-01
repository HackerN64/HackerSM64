#pragma once

/*****************
 * GAME SETTINGS *
 *****************/

/**
 * Enables some mechanics that change behavior depending on hardcoded level numbers.
 * You may also need to change sLevelsWithRooms in object_helpers.c.
 * TODO: separate this into separate defines, behavior params, or make these mechanics otherwise dynamic.
*/
// #define ENABLE_VANILLA_LEVEL_SPECIFIC_CHECKS

/**
 * Enable lives and the lives counter. The number next to the define is the number of lives you start with.
 */
// #define ENABLE_LIVES 4

/**
 * This can be 0..127.
 */
#define MAX_NUM_LIVES 100

/**
 * This can be 0..32767.
 */
#define MAX_NUM_COINS 999

/**
 * Air/breath meter is separate from health meter when underwater.
 */
// #define BREATH_METER

/**
 * Number of coins to spawn the "100 coin" star. If you remove the define altogether, then there won't be a 100 coin star at all.
 */
#define X_COIN_STAR 100

/**
 * Stars don't kick you out of the level (does not play nicely with vanilla).
 */
// #define NON_STOP_STARS

/**
 * Bowser keys always exit the level. Only has an effect if NON_STOP_STARS is enabled.
 */
// #define KEYS_EXIT_LEVEL

/**
 * Uncomment this if you want global star IDs (useful for creating an open world hack ala MVC).
 */
// #define GLOBAL_STAR_IDS

/**
 * Number of possible unique model ID's (keep it higher than 256).
 */
#define MODEL_ID_COUNT 256

/**
 * Number of supported areas per level.
 */
#define AREA_COUNT 8

/**
 * Makes signs and NPCs easier to talk to.
 */
// #define EASIER_DIALOG_TRIGGER

/**
 * Shows an "A" when Mario is able to talk [requires EASIER_DIALOG_TRIGGER].
 */
// #define DIALOG_INDICATOR

/**
 * Include the English characters that were missing from US segment2
 * J, Q, V, X, Z, ¨, !, !!, ?, &, %, ., and the beta key.
 * [MAKE SURE TO INCLUDE EU AND JP/SH BASEROMS IN THE REPO TO OBTAIN THE ASSETS]
 * If this is disabled, backup assets will be used.
 */
// #define COMPLETE_EN_US_SEGMENT2

/**
 * Removes multi-language cake screen.
 */
#define EU_CUSTOM_CAKE_FIX

/**
 * Adds multiple languages to the game. Just a placeholder for the most part, because it only works with EU, and must be enabled with EU.
 */
#define MULTILANG (0 || VERSION_EU)

/**
 * Prevents infinite death loops by always restoring Mario's health when he's warped to any kind of warp while dead.
 */
#define PREVENT_DEATH_LOOP

/**
 * The level that the game starts with immediately after file select.
 * The levelscript needs to have a MARIO_POS command for this to work.
 */
#define START_LEVEL LEVEL_CASTLE_GROUNDS
