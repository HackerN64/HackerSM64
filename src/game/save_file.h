#ifndef SAVE_FILE_H
#define SAVE_FILE_H

#include <PR/ultratypes.h>

#include "types.h"
#include "area.h"
#include "puppycam2.h"

#include "course_table.h"

#if defined(SRAM)
    #define EEPROM_SIZE 0x8000
#elif defined(EEP16K)
    #define EEPROM_SIZE 0x800
#else
    #define EEPROM_SIZE 0x200
#endif

#define NUM_SAVE_FILES 4

struct SaveBlockSignature {
    u16 magic;
    u16 chksum;
};

struct SaveFile {
    // Location of lost cap.
    // Note: the coordinates get set, but are never actually used, since the
    // cap can always be found in a fixed spot within the course
    u8 capLevel;
    u8 capArea;
    // Note: the coordinates get set, but are never actually used, since the
    // cap can always be found in a fixed spot within the course
    Vec3s capPos; // 48 bits

    u32 flags;

    // Star flags for each course.
    // The most significant bit of the byte *following* each course is set if the
    // cannon is open.
    u8 courseStars[COURSE_COUNT]; // 200 bits

    u8 courseCoinScores[COURSE_STAGES_COUNT]; // 120 bits

    struct SaveBlockSignature signature; // 32 bits
};

enum SaveFileIndex {
    SAVE_FILE_A,
    SAVE_FILE_B,
    SAVE_FILE_C,
    SAVE_FILE_D
};

struct MainMenuSaveData {
    // Each save file has a 2 bit "age" for each course. The higher this value,
    // the older the high score is. This is used for tie-breaking when displaying
    // on the high score screen.
    u32 coinScoreAges[NUM_SAVE_FILES];
    u8 soundMode: 2;
#ifdef WIDE
    u8 wideMode: 1;
#endif
#ifdef MULTILANG
    u8 language: 3;
#endif

#ifdef PUPPYCAM
    u8 firstBoot;
    struct gPuppyOptions saveOptions;
#endif
    struct SaveBlockSignature signature;
};

struct SaveBuffer {
    // Each of the four save files has two copies. If one is bad, the other is used as a backup.
    struct SaveFile files[NUM_SAVE_FILES][2];
    // Main menu data, storing config options.
    struct MainMenuSaveData menuData;
};

#ifdef PUPPYCAM
extern void puppycam_set_save(void);
extern void puppycam_get_save(void);
extern void puppycam_check_save(void);
#endif

STATIC_ASSERT(sizeof(struct SaveBuffer) <= EEPROM_SIZE, "ERROR: Save struct too big for specified save type");

extern u8 gLastCompletedCourseNum;
extern u8 gLastCompletedStarNum;
extern s8 sUnusedGotGlobalCoinHiScore;
extern u8 gGotFileCoinHiScore;
extern u8 gCurrCourseStarFlags;
extern u8 gSpecialTripleJump;
extern s8 gLevelToCourseNumTable[];

enum CourseFlags {
    COURSE_FLAG_CANNON_UNLOCKED      = (1 <<  7), /* 0x00000080 */
};

// game progress flags
enum SaveProgressFlags {
    SAVE_FLAG_FILE_EXISTS            = (1 <<  0), /* 0x00000001 */
    SAVE_FLAG_HAVE_WING_CAP          = (1 <<  1), /* 0x00000002 */
    SAVE_FLAG_HAVE_METAL_CAP         = (1 <<  2), /* 0x00000004 */
    SAVE_FLAG_HAVE_VANISH_CAP        = (1 <<  3), /* 0x00000008 */
    SAVE_FLAG_HAVE_KEY_1             = (1 <<  4), /* 0x00000010 */
    SAVE_FLAG_HAVE_KEY_2             = (1 <<  5), /* 0x00000020 */
    SAVE_FLAG_UNLOCKED_BASEMENT_DOOR = (1 <<  6), /* 0x00000040 */
    SAVE_FLAG_UNLOCKED_UPSTAIRS_DOOR = (1 <<  7), /* 0x00000080 */
    SAVE_FLAG_DDD_MOVED_BACK         = (1 <<  8), /* 0x00000100 */
    SAVE_FLAG_MOAT_DRAINED           = (1 <<  9), /* 0x00000200 */
    SAVE_FLAG_UNLOCKED_PSS_DOOR      = (1 << 10), /* 0x00000400 */
    SAVE_FLAG_UNLOCKED_WF_DOOR       = (1 << 11), /* 0x00000800 */
    SAVE_FLAG_UNLOCKED_CCM_DOOR      = (1 << 12), /* 0x00001000 */
    SAVE_FLAG_UNLOCKED_JRB_DOOR      = (1 << 13), /* 0x00002000 */
    SAVE_FLAG_UNLOCKED_BITDW_DOOR    = (1 << 14), /* 0x00004000 */
    SAVE_FLAG_UNLOCKED_BITFS_DOOR    = (1 << 15), /* 0x00008000 */
    SAVE_FLAG_CAP_ON_GROUND          = (1 << 16), /* 0x00010000 */
    SAVE_FLAG_CAP_ON_KLEPTO          = (1 << 17), /* 0x00020000 */
    SAVE_FLAG_CAP_ON_UKIKI           = (1 << 18), /* 0x00040000 */
    SAVE_FLAG_CAP_ON_MR_BLIZZARD     = (1 << 19), /* 0x00080000 */
    SAVE_FLAG_UNLOCKED_50_STAR_DOOR  = (1 << 20), /* 0x00100000 */
    SAVE_FLAG_COLLECTED_TOAD_STAR_1  = (1 << 24), /* 0x01000000 */
    SAVE_FLAG_COLLECTED_TOAD_STAR_2  = (1 << 25), /* 0x02000000 */
    SAVE_FLAG_COLLECTED_TOAD_STAR_3  = (1 << 26), /* 0x04000000 */
    SAVE_FLAG_COLLECTED_MIPS_STAR_1  = (1 << 27), /* 0x08000000 */
    SAVE_FLAG_COLLECTED_MIPS_STAR_2  = (1 << 28), /* 0x10000000 */
};

enum StarFlags {
    STAR_FLAGS_NONE         = (0 << 0), // 0x00
    STAR_FLAG_ACT_1         = (1 << 0), // 0x01
    STAR_FLAG_ACT_2         = (1 << 1), // 0x02
    STAR_FLAG_ACT_3         = (1 << 2), // 0x04
    STAR_FLAG_ACT_4         = (1 << 3), // 0x08
    STAR_FLAG_ACT_5         = (1 << 4), // 0x10
    STAR_FLAG_ACT_6         = (1 << 5), // 0x20
    STAR_FLAG_ACT_100_COINS = (1 << 6), // 0x40
    STAR_FLAG_LAST          = STAR_FLAG_ACT_100_COINS
};

#define SAVE_FLAG_TO_STAR_FLAG(cmd) (((cmd) >> 24) & 0x7F)
#define STAR_FLAG_TO_SAVE_FLAG(cmd) ((cmd) << 24)

// Variable for setting a warp checkpoint.

// possibly a WarpDest struct where arg is a union. TODO: Check?
struct WarpCheckpoint {
    /*0x00*/ u8 actNum;
    /*0x01*/ u8 courseNum;
    /*0x02*/ u8 levelID;
    /*0x03*/ u8 areaNum;
    /*0x04*/ u8 warpNode;
};

extern struct WarpCheckpoint gWarpCheckpoint;

extern s8 gMainMenuDataModified;
extern s8 gSaveFileModified;

void save_file_do_save(s32 fileIndex);
void save_file_erase(s32 fileIndex);
void save_file_copy(s32 srcFileIndex, s32 destFileIndex);
void save_file_load_all(void);
void save_file_reload(void);
void save_file_collect_star_or_key(s16 coinScore, s16 starIndex);
s32 save_file_exists(s32 fileIndex);
u32 save_file_get_max_coin_score(s32 courseIndex);
s32 save_file_get_course_star_count(s32 fileIndex, s32 courseIndex);
s32 save_file_get_total_star_count(s32 fileIndex, s32 minCourse, s32 maxCourse);
void save_file_set_flags(u32 flags);
void save_file_clear_flags(u32 flags);
u32 save_file_get_flags(void);
u32 save_file_get_star_flags(s32 fileIndex, s32 courseIndex);
void save_file_set_star_flags(s32 fileIndex, s32 courseIndex, u32 starFlags);
s32 save_file_get_course_coin_score(s32 fileIndex, s32 courseIndex);
s32 save_file_is_cannon_unlocked(void);
void save_file_set_cannon_unlocked(void);
void save_file_set_cap_pos(s16 x, s16 y, s16 z);
s32 save_file_get_cap_pos(Vec3s capPos);
void save_file_set_sound_mode(u16 mode);
u32 save_file_get_sound_mode(void);
#ifdef WIDE
u32 save_file_get_widescreen_mode(void);
void save_file_set_widescreen_mode(u8 mode);
#endif
void save_file_move_cap_to_default_location(void);

void disable_warp_checkpoint(void);
void check_if_should_set_warp_checkpoint(struct WarpNode *warpNode);
s32 check_warp_checkpoint(struct WarpNode *warpNode);

#ifdef MULTILANG
void multilang_set_language(u32 language);
u32 multilang_get_language(void);
u32 get_language_index(u32 language);
#endif

#endif // SAVE_FILE_H
