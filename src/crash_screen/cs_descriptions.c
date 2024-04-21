#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"
#include "segment_symbols.h"

#include "crash_screen/cs_main.h"
#include "util/insn_disasm.h"
#include "util/map_parser.h"
#include "util/memory_read.h"
#include "util/registers.h"

#include "cs_descriptions.h"

#include "buffers/buffers.h"
#include "engine/surface_load.h"
#include "game/emutest.h"
#include "game/game_init.h"
#include "game/level_update.h"
#include "game/version.h"


// Include the version number from VERSION.txt. Includes a newline at the end.
INCBIN(char, HackerSM64_version_txt, "VERSION.txt", 4);

// Crash screen version. Includes a newline at the end.
INCBIN(char, CrashScreen_version_txt, "src/crash_screen/VERSION.txt", 4);


// -- STRINGS --

// Region string:
#define DEF_REGION_NAME(_name) const char* gRegionName = TO_STRING2(_name);
#ifdef VERSION_JP
DEF_REGION_NAME(jp);
#elif VERSION_US
DEF_REGION_NAME(us);
#elif VERSION_EU
DEF_REGION_NAME(eu);
#elif VERSION_SH
DEF_REGION_NAME(sh);
#elif VERSION_CN
DEF_REGION_NAME(cn);
#elif BBPLAYER
DEF_REGION_NAME(bb);
#else
DEF_REGION_NAME(xx);
#endif

// osTvType strings:
const char* osTvTypeStrings[] = {
    [OS_TV_PAL ] = "pal",
    [OS_TV_NTSC] = "ntsc",
    [OS_TV_MPAL] = "mpal",
};

// Microcode string:
#define DEF_UCODE_NAME(_name) const char* gUcodeName = TO_STRING2(_name);
#ifdef L3DEX2_ALONE
DEF_UCODE_NAME(L3DEX2_alone);
#elif F3DZEX_GBI_2
DEF_UCODE_NAME(f3dzex2_PosLight);
#elif F3DZEX_NON_GBI_2
DEF_UCODE_NAME(f3dzex2_Non_PosLight);
#elif F3DEX2PL_GBI
DEF_UCODE_NAME(F3DEX2_PosLight);
#elif F3DEX_GBI_2
DEF_UCODE_NAME(F3DEX2);
#elif F3DEX_GBI
DEF_UCODE_NAME(F3DEX);
#elif SUPER3D_GBI
DEF_UCODE_NAME(Super3D);
#else
DEF_UCODE_NAME(Fast3D);
#endif

// Save type string:
#define DEF_SAVETYPE_NAME(_name) const char* gSaveTypeName = TO_STRING2(_name);
#ifdef EEP4K
DEF_SAVETYPE_NAME(eep4k); // 4kbit
#elif EEP16K
DEF_SAVETYPE_NAME(eep16k); // 16kbit
#elif SRAM
DEF_SAVETYPE_NAME(sram); // 256kbit
#else
DEF_SAVETYPE_NAME(unknown);
#endif

// Compression type string:
#define DEF_COMPRESSION_NAME(_name) const char* gCompressionName = TO_STRING2(_name);
#ifdef GZIP
DEF_COMPRESSION_NAME(gzip);
#elif RNC1
DEF_COMPRESSION_NAME(rnc1);
#elif RNC2
DEF_COMPRESSION_NAME(rnc2);
#elif YAYO
DEF_COMPRESSION_NAME(yay0);
#elif MIO0
DEF_COMPRESSION_NAME(mio0);
#elif UNCOMPRESSED
DEF_COMPRESSION_NAME(none);
#else
DEF_COMPRESSION_NAME(unk);
#endif


#define ID_LIST_END() { .id = -1, .name = NULL, }


typedef struct IdNamePair {
    /*0x00*/ const int id;
    /*0x04*/ const char* name;
} IdNamePair; /*0x08*/
static const char* get_name_from_id_list_impl(int id, const IdNamePair* list, size_t count) {
    for (size_t i = 0; i < count; i++) {
        if ((int)id == (int)list[i].id) {
            return list[i].name;
        }
    }

    return NULL;
}
#define get_name_from_id_list(_id, _list) get_name_from_id_list_impl((_id), (_list), ARRAY_COUNT(_list))
static const char* get_name_from_null_terminated_id_list(int id, const IdNamePair* list) {
    const u32 max_iterations = 32;
    u32 i = 0;

    const IdNamePair* entry = &list[0];
    while (entry->name != NULL) {
        if (i++ >= max_iterations) {
            return "error";
        }

        if ((int)id == (int)entry->id) {
            return entry->name;
        }

        entry++;
    }

    return NULL;
}

typedef struct RangeNamePair {
    /*0x00*/ const u32 start;
    /*0x04*/ const u32 end;
    /*0x08*/ const char* name;
} RangeNamePair; /*0x0C*/
static const char* get_name_from_range_list_impl(u32 id, const RangeNamePair* list, size_t count) {
    for (size_t i = 0; i < count; i++) {
        if (((u32)id >= (u32)list[i].start) && ((u32)id < (u32)list[i].end)) {
            return list[i].name;
        }
    }

    return NULL;
}
#define get_name_from_range_list(_id, _list) get_name_from_range_list_impl((_id), (_list), ARRAY_COUNT(_list))
UNUSED static const char* get_name_from_null_terminated_range_list(u32 id, const RangeNamePair* list) {
    const u32 max_iterations = 32;
    u32 i = 0;

    const RangeNamePair* entry = &list[0];
    while (entry->name != NULL) {
        if (i++ >= max_iterations) {
            return "error";
        }

        if (((u32)id >= (u32)entry->start) && ((u32)id < (u32)entry->end)) {
            return entry->name;
        }

        entry++;
    }

    return NULL;
}


const char* str_null_fallback(const char* str, const char* fallback) {
    return ((str != NULL) ? str : fallback);
}


// -- THREAD --

static const IdNamePair sThreadIDNames[] = {
    { .id = THREAD_0,                   .name = "libultra?",        }, // Uses sThreadPriNames.
    { .id = THREAD_1_IDLE,              .name = "idle",             },
    { .id = THREAD_2,                   .name = "unused",           },
    { .id = THREAD_3_MAIN,              .name = "main",             },
    { .id = THREAD_4_SOUND,             .name = "sound",            }, //! TODO: libultra scheduler also uses ID 4 and has no set priority. Is there a way to differentiate the two?
    { .id = THREAD_5_GAME_LOOP,         .name = "game loop",        },
    { .id = THREAD_6_RUMBLE,            .name = "rumble",           },
    { .id = THREAD_7_HVQM,              .name = "hvqm",             },
    { .id = THREAD_8_TIMEKEEPER,        .name = "timekeeper",       },
    { .id = THREAD_9_DA_COUNTER,        .name = "da counter",       },
    { .id = THREAD_13_FAULT,            .name = "unf fault",        },
    { .id = THREAD_14_USB,              .name = "unf usb",          },
    { .id = THREAD_1000_CRASH_SCREEN_0, .name = "crash screen 0",   },
    { .id = THREAD_1001_CRASH_SCREEN_1, .name = "crash screen 1",   },
    { .id = THREAD_1002_CRASH_SCREEN_2, .name = "crash screen 2",   },
};
static const IdNamePair sThreadPriNames[] = {
    { .id = OS_PRIORITY_SIMGR,          .name = "si manager", },
    { .id = 149,                        .name = "debug/usb?", }, //! TODO: Find out what this thread is. It only exists when running UNF.
    { .id = OS_PRIORITY_PIMGR,          .name = "pi manager", },
    { .id = OS_PRIORITY_VIMGR,          .name = "vi manager", },
    { .id = OS_PRIORITY_RMON,           .name = "rmon",       },
    { .id = OS_PRIORITY_RMONSPIN,       .name = "rmonspin",   },
};
// Returns a thread name from 'sThreadIDNames'.
const char* get_thread_name(OSThread* thread) {
    if (thread == NULL) {
        return NULL;
    }
    OSId id = osGetThreadId(thread);
    OSPri pri = osGetThreadPri(thread);
    const char* name = NULL;

    // Determine libultra threads on thread ID 0 by priority instead of ID:
    if ((id == THREAD_0) && (pri > OS_PRIORITY_APPMAX)) {
        name = get_name_from_id_list(pri, sThreadPriNames);
        if (name != NULL) {
            return name;
        }
    }

    name = get_name_from_id_list(id, sThreadIDNames);
    if (name != NULL) {
        return name;
    }

    if (IS_DEBUG_MAP_INCLUDED() && (name == NULL)) {
        const MapSymbol* symbol = get_map_symbol((Address)thread, SYMBOL_SEARCH_BACKWARD);
        if (symbol != NULL) {
            return get_map_symbol_name(symbol);
        }
    }

    return NULL;
}

static const char* sThreadStateStrings[] = {
    [CTZ(OS_STATE_STOPPED )] = "stopped",
    [CTZ(OS_STATE_RUNNABLE)] = "runnable",
    [CTZ(OS_STATE_RUNNING )] = "running",
    [CTZ(OS_STATE_WAITING )] = "waiting",
};
const char* get_thread_state_str(OSThread* thread) {
    u16 state = thread->state;
    if (state == 0x0000) return NULL;
    return sThreadStateStrings[CTZ(state)];
}

static const char* sThreadFlagStrings[] = {
    [CTZ(OS_FLAG_CPU_BREAK)] = "break", // CPU break
    [CTZ(OS_FLAG_FAULT    )] = "fault",
};
const char* get_thread_flags_str(OSThread* thread) {
    u16 flags = thread->flags;
    if (flags == 0x0000) return NULL;
    return sThreadFlagStrings[CTZ(flags)];
}


// -- WARP NODE PRESETS --

static const IdNamePair sWarpNodeSpecialIds[] = {
    { .id = WARP_NODE_MAIN_ENTRY,    .name = "main entry",    },
    { .id = WARP_NODE_DEFAULT,       .name = "default",       },
    { .id = WARP_NODE_DEATH,         .name = "death",         },
    { .id = WARP_NODE_LOOK_UP,       .name = "look up",       },
    { .id = WARP_NODE_WARP_FLOOR,    .name = "warp floor",    },
    { .id = WARP_NODE_CREDITS_MIN,   .name = "credits min",   },
    { .id = WARP_NODE_CREDITS_START, .name = "credits start", },
    { .id = WARP_NODE_CREDITS_NEXT,  .name = "credits next",  },
    { .id = WARP_NODE_CREDITS_END,   .name = "credits end",   },
};
const char* get_warp_node_name(enum WarpNodes id) {
    return str_null_fallback(get_name_from_id_list(id, sWarpNodeSpecialIds), "");
}


// -- COURSES --

static const char* sLevelNames[] = {
    [LEVEL_NONE] = "none",
#define STUB_LEVEL(_0, levelenum, _2, _3, _4, _5, _6, _7, _8) [levelenum] = "",
#define DEFINE_LEVEL(textname, levelenum, _2, folder, _4, _5, _6, _7, _8, _9, _10) [levelenum] = TO_STRING2(folder),
#include "levels/level_defines.h"
#undef STUB_LEVEL
#undef DEFINE_LEVEL
    [LEVEL_FILE_SELECT ] = "file_select",
    [LEVEL_LEVEL_SELECT] = "level_select",
};
const char* get_level_name(enum LevelNum levelNum) {
    if (levelNum < LEVEL_NONE) {
        return "restart_game";
    }
    return ((levelNum < ARRAY_COUNT(sLevelNames)) ? sLevelNames[levelNum] : "");
}


// -- SEGMENTS --

#define DEF_ROM_SEG_IMPL(_name, _str) { .id = (uintptr_t)_##_name##SegmentRomStart, .name = TO_STRING2(_str), } 
#define DEF_ROM_SEG(_name)            DEF_ROM_SEG_IMPL(_name,                      _name)
#define DEF_ROM_SEG_YAY0(_name)       DEF_ROM_SEG_IMPL(_name##_yay0,               _name)
#define DEF_ROM_SEG_GEO(_name)        DEF_ROM_SEG_IMPL(_name##_geo,                _name)
#define DEF_ROM_SEG_SKY_YAY0(_name)   DEF_ROM_SEG_IMPL(_name##_skybox_yay0,        _name)
#define DEF_ROM_SEG_SEG7(_name)       DEF_ROM_SEG_IMPL(_name##_segment_7,          _name)
#define DEF_ROM_SEG_LANG_YAY0(_name)  DEF_ROM_SEG_IMPL(translation_##_name##_yay0, _name)
#define DEF_ROM_SEG_GLOBAL_VAR(_name) { .id = (uintptr_t)_name, .name = TO_STRING2(_name), }

// 00: SEGMENT_MAIN
static const IdNamePair sROMSegNames_00_main[] = {
    DEF_ROM_SEG(main),
    ID_LIST_END(),
};
// 01: SEGMENT_RENDER
static const IdNamePair sROMSegNames_01_render[] = {
    { .id = OS_K0_TO_PHYSICAL(gGfxPools[0].buffer), .name = "gGfxPools[0]", },
    { .id = OS_K0_TO_PHYSICAL(gGfxPools[1].buffer), .name = "gGfxPools[1]", },
    ID_LIST_END(),
};
// 02: SEGMENT_SEGMENT2
static const IdNamePair sROMSegNames_02_segment2[] = {
    DEF_ROM_SEG_YAY0(segment2),
    ID_LIST_END(),
};
// 03: SEGMENT_COMMON1_YAY0
static const IdNamePair sROMSegNames_03_common1_gfx[] = {
    DEF_ROM_SEG_YAY0(common1),
    ID_LIST_END(),
};
// 04: SEGMENT_GROUP0_YAY0
static const IdNamePair sROMSegNames_04_group0_gfx[] = {
    DEF_ROM_SEG_YAY0(group0),
    DEF_ROM_SEG(boot),
    ID_LIST_END(),
};
// 05: SEGMENT_GROUPA_YAY0
static const IdNamePair sROMSegNames_05_groupA_gfx[] = {
    DEF_ROM_SEG_YAY0(group1),
    DEF_ROM_SEG_YAY0(group2),
    DEF_ROM_SEG_YAY0(group3),
    DEF_ROM_SEG_YAY0(group4),
    DEF_ROM_SEG_YAY0(group5),
    DEF_ROM_SEG_YAY0(group6),
    DEF_ROM_SEG_YAY0(group7),
    DEF_ROM_SEG_YAY0(group8),
    DEF_ROM_SEG_YAY0(group9),
    DEF_ROM_SEG_YAY0(group10),
    DEF_ROM_SEG_YAY0(group11),
    ID_LIST_END(),
};
// 06: SEGMENT_GROUPB_YAY0
static const IdNamePair sROMSegNames_06_groupB_gfx[] = {
    DEF_ROM_SEG_YAY0(group12),
    DEF_ROM_SEG_YAY0(group13),
    DEF_ROM_SEG_YAY0(group14),
    DEF_ROM_SEG_YAY0(group15),
    DEF_ROM_SEG_YAY0(group16),
    DEF_ROM_SEG_YAY0(group17),
    ID_LIST_END(),
};
// 07: SEGMENT_LEVEL_DATA
static const IdNamePair sROMSegNames_07_level_data[] = {
#define STUB_LEVEL(_0, _1, _2, _3, _4, _5, _6, _7, _8)
#define DEFINE_LEVEL(textname, _1, _2, folder, _4, _5, _6, _7, _8, _9, _10) DEF_ROM_SEG_SEG7(folder),
#include "levels/level_defines.h"
#undef STUB_LEVEL
#undef DEFINE_LEVEL
    DEF_ROM_SEG_YAY0(debug_level_select),
    ID_LIST_END(),
};
// 08: SEGMENT_COMMON0_YAY0
static const IdNamePair sROMSegNames_08_common0_gfx[] = {
    DEF_ROM_SEG_YAY0(common0),
    ID_LIST_END(),
};
// 09: SEGMENT_TEXTURE
static const IdNamePair sROMSegNames_09_texture[] = {
    DEF_ROM_SEG_YAY0(fire),
    DEF_ROM_SEG_YAY0(spooky),
    DEF_ROM_SEG_YAY0(generic),
    DEF_ROM_SEG_YAY0(water),
    DEF_ROM_SEG_YAY0(sky),
    DEF_ROM_SEG_YAY0(snow),
    DEF_ROM_SEG_YAY0(cave),
    DEF_ROM_SEG_YAY0(machine),
    DEF_ROM_SEG_YAY0(mountain),
    DEF_ROM_SEG_YAY0(grass),
    DEF_ROM_SEG_YAY0(outside),
    DEF_ROM_SEG_YAY0(inside),
    DEF_ROM_SEG_YAY0(effect),
    ID_LIST_END(),
};
// 10: SEGMENT_SKYBOX
static const IdNamePair sROMSegNames_10_skybox[] = {
    DEF_ROM_SEG_SKY_YAY0(water),
    DEF_ROM_SEG_SKY_YAY0(ccm),
    DEF_ROM_SEG_SKY_YAY0(clouds),
    DEF_ROM_SEG_SKY_YAY0(bitfs),
    DEF_ROM_SEG_SKY_YAY0(wdw),
    DEF_ROM_SEG_SKY_YAY0(cloud_floor),
    DEF_ROM_SEG_SKY_YAY0(ssl),
    DEF_ROM_SEG_SKY_YAY0(bbh),
    DEF_ROM_SEG_SKY_YAY0(bidw),
    DEF_ROM_SEG_SKY_YAY0(bits),
    DEF_ROM_SEG_YAY0(title_screen_bg), // For some reason the game uses the skybox segment for this.
    ID_LIST_END(),
};
// 11: SEGMENT_COMMON0_YAY0
static const IdNamePair sROMSegNames_11_effect_gfx[] = {
    DEF_ROM_SEG_YAY0(effect),
    ID_LIST_END(),
};
// 12: SEGMENT_GROUPA_GEO
static const IdNamePair sROMSegNames_12_groupA_geo[] = {
    DEF_ROM_SEG_GEO(group1),
    DEF_ROM_SEG_GEO(group2),
    DEF_ROM_SEG_GEO(group3),
    DEF_ROM_SEG_GEO(group4),
    DEF_ROM_SEG_GEO(group5),
    DEF_ROM_SEG_GEO(group6),
    DEF_ROM_SEG_GEO(group7),
    DEF_ROM_SEG_GEO(group8),
    DEF_ROM_SEG_GEO(group9),
    DEF_ROM_SEG_GEO(group10),
    DEF_ROM_SEG_GEO(group11),
    ID_LIST_END(),
};
// 13: SEGMENT_GROUPB_GEO
static const IdNamePair sROMSegNames_13_groupB_geo[] = {
    DEF_ROM_SEG_GEO(group12),
    DEF_ROM_SEG_GEO(group13),
    DEF_ROM_SEG_GEO(group14),
    DEF_ROM_SEG_GEO(group15),
    DEF_ROM_SEG_GEO(group16),
    DEF_ROM_SEG_GEO(group17),
    ID_LIST_END(),
};
// 14: SEGMENT_LEVEL_SCRIPT
static const IdNamePair sROMSegNames_14_level_script[] = {
#define STUB_LEVEL(_0, _1, _2, _3, _4, _5, _6, _7, _8)
#define DEFINE_LEVEL(textname, _1, _2, folder, _4, _5, _6, _7, _8, _9, _10) DEF_ROM_SEG(folder),
#include "levels/level_defines.h"
#undef STUB_LEVEL
#undef DEFINE_LEVEL
    DEF_ROM_SEG(ending),
    ID_LIST_END(),
};
// 15: SEGMENT_COMMON0_GEO
static const IdNamePair sROMSegNames_15_common0_geo[] = {
    DEF_ROM_SEG_GEO(common0),
    ID_LIST_END(),
};
// 16: SEGMENT_LEVEL_ENTRY
static const IdNamePair sROMSegNames_16_entry[] = {
    DEF_ROM_SEG(entry),
    ID_LIST_END(),
};
// 17: SEGMENT_MARIO_ANIMS
static const IdNamePair sROMSegNames_17_mario_anims[] = {
    DEF_ROM_SEG_GLOBAL_VAR(gMarioAnims),
    ID_LIST_END(),
};
// 18: SEGMENT_UNKNOWN_18
// 19: SEGMENT_BEHAVIOR_DATA
static const IdNamePair sROMSegNames_19_behavior[] = {
    DEF_ROM_SEG(behavior),
    ID_LIST_END(),
};
// 20: SEGMENT_MENU_INTRO
static const IdNamePair sROMSegNames_20_menu_intro[] = {
    DEF_ROM_SEG(intro),
    DEF_ROM_SEG(menu),
    DEF_ROM_SEG(ending),
    ID_LIST_END(),
};
// 21: SEGMENT_GLOBAL_LEVEL_SCRIPT
static const IdNamePair sROMSegNames_21_global_level_script[] = {
    DEF_ROM_SEG(scripts),
    ID_LIST_END(),
};
// 22: SEGMENT_COMMON1_GEO
static const IdNamePair sROMSegNames_22_common1_geo[] = {
    DEF_ROM_SEG_GEO(common1),
    ID_LIST_END(),
};
// 23: SEGMENT_GROUP0_GEO
static const IdNamePair sROMSegNames_23_group0_geo[] = {
    DEF_ROM_SEG_GEO(group0),
    ID_LIST_END(),
};
// 24: SEGMENT_DEMO_INPUTS
static const IdNamePair sROMSegNames_24_demo_inputs[] = {
    DEF_ROM_SEG_GLOBAL_VAR(gDemoInputs),
    ID_LIST_END(),
};
// 25: SEGMENT_EU_TRANSLATION
//! TODO: Update this when ASCII PR is merged:
static const IdNamePair sROMSegNames_25_eu_translation[] = {
#if MULTILANG
    DEF_ROM_SEG_IMPL(translation_en_yay0, english),
    DEF_ROM_SEG_IMPL(translation_fr_yay0, french),
    DEF_ROM_SEG_IMPL(translation_de_yay0, german),
#endif // MULTILANG
    ID_LIST_END(),
};
// 26: SEGMENT_UNKNOWN_26
// 27: SEGMENT_UNKNOWN_27
// 28: SEGMENT_UNKNOWN_28
// 29: SEGMENT_UNKNOWN_29
// 30: SEGMENT_UNKNOWN_30
// 31: SEGMENT_UNKNOWN_31

typedef struct SegmentInfo {
    const IdNamePair* list;
    const char* name;
} SegmentInfo;
static const SegmentInfo sSegmentInfos[32] = {
    [SEGMENT_MAIN               ] = { .list = sROMSegNames_00_main,                .name = "main",                },
    [SEGMENT_RENDER             ] = { .list = sROMSegNames_01_render,              .name = "render",              },
    [SEGMENT_SEGMENT2           ] = { .list = sROMSegNames_02_segment2,            .name = "hud gfx",             },
    [SEGMENT_COMMON1_YAY0       ] = { .list = sROMSegNames_03_common1_gfx,         .name = "common1 gfx",         },
    [SEGMENT_GROUP0_YAY0        ] = { .list = sROMSegNames_04_group0_gfx,          .name = "group0 gfx",          },
    [SEGMENT_GROUPA_YAY0        ] = { .list = sROMSegNames_05_groupA_gfx,          .name = "groupA gfx",          },
    [SEGMENT_GROUPB_YAY0        ] = { .list = sROMSegNames_06_groupB_gfx,          .name = "groupB gfx",          },
    [SEGMENT_LEVEL_DATA         ] = { .list = sROMSegNames_07_level_data,          .name = "level data",          },
    [SEGMENT_COMMON0_YAY0       ] = { .list = sROMSegNames_08_common0_gfx,         .name = "common0 gfx",         },
    [SEGMENT_TEXTURE            ] = { .list = sROMSegNames_09_texture,             .name = "textures bin",        },
    [SEGMENT_SKYBOX             ] = { .list = sROMSegNames_10_skybox,              .name = "skybox bin",          },
    [SEGMENT_EFFECT_YAY0        ] = { .list = sROMSegNames_11_effect_gfx,          .name = "effects gfx",         },
    [SEGMENT_GROUPA_GEO         ] = { .list = sROMSegNames_12_groupA_geo,          .name = "groupA geo",          },
    [SEGMENT_GROUPB_GEO         ] = { .list = sROMSegNames_13_groupB_geo,          .name = "groupB geo",          },
    [SEGMENT_LEVEL_SCRIPT       ] = { .list = sROMSegNames_14_level_script,        .name = "level script",        },
    [SEGMENT_COMMON0_GEO        ] = { .list = sROMSegNames_15_common0_geo,         .name = "common0 geo",         },
    [SEGMENT_LEVEL_ENTRY        ] = { .list = sROMSegNames_16_entry,               .name = "level entry",         },
    [SEGMENT_MARIO_ANIMS        ] = { .list = sROMSegNames_17_mario_anims,         .name = "mario anims",         },
    [SEGMENT_UNKNOWN_18         ] = { .list = NULL,                                .name = "unknown 18",          },
    [SEGMENT_BEHAVIOR_DATA      ] = { .list = sROMSegNames_19_behavior,            .name = "bhv data",            },
    [SEGMENT_MENU_INTRO         ] = { .list = sROMSegNames_20_menu_intro,          .name = "menu/intro",          },
    [SEGMENT_GLOBAL_LEVEL_SCRIPT] = { .list = sROMSegNames_21_global_level_script, .name = "global level script", },
    [SEGMENT_COMMON1_GEO        ] = { .list = sROMSegNames_22_common1_geo,         .name = "common1 geo",         },
    [SEGMENT_GROUP0_GEO         ] = { .list = sROMSegNames_23_group0_geo,          .name = "group0 geo",          },
    [SEGMENT_DEMO_INPUTS        ] = { .list = sROMSegNames_24_demo_inputs,         .name = "demo inputs",         },
    [SEGMENT_EU_TRANSLATION     ] = { .list = sROMSegNames_25_eu_translation,      .name = "languages",           },
    [SEGMENT_UNKNOWN_26         ] = { .list = NULL,                                .name = "unknown 26",          },
    [SEGMENT_UNKNOWN_27         ] = { .list = NULL,                                .name = "unknown 27",          },
    [SEGMENT_UNKNOWN_28         ] = { .list = NULL,                                .name = "unknown 28",          },
    [SEGMENT_UNKNOWN_29         ] = { .list = NULL,                                .name = "unknown 29",          },
    [SEGMENT_UNKNOWN_30         ] = { .list = NULL,                                .name = "unknown 30",          },
    [SEGMENT_UNKNOWN_31         ] = { .list = NULL,                                .name = "unknown 31",          },
};
// Get the name of a segment.
const char* get_segment_name(u8 segmentId) {
    return ((segmentId < ARRAY_COUNT(sSegmentInfos)) ? sSegmentInfos[segmentId].name : "unknown");
}
// Get the name of what is currently loaded in a segment.
const char* get_segment_sub_name(u8 segmentId) {
    const char* ret = NULL;
    Address romAddr = sSegmentROMTable[segmentId];
    if (romAddr == (uintptr_t)NULL) {
        return "unloaded";
    }

    const IdNamePair* list = sSegmentInfos[segmentId].list;
    if (list != NULL) {
        ret = get_name_from_null_terminated_id_list(romAddr, list);
    }

    return str_null_fallback(ret, "unknown");
}


// -- RAM POOLS --

static const RangeNamePair sHardcodedSegmentRanges[] = {
    { .start = (Address)_mainSegmentStart,            .end = (Address)_mainSegmentEnd,            .name = "main",          },
    { .start = (Address)_engineSegmentStart,          .end = (Address)_engineSegmentEnd,          .name = "engine",        },
    { .start = (Address)_crashscreenSegmentStart,     .end = (Address)_crashscreenSegmentBssEnd,  .name = "crash screeen", },
    { .start = (Address)_goddardSegmentStart,         .end = (Address)_goddardSegmentEnd,         .name = "goddard",       },
    { .start = (Address)_framebuffersSegmentBssStart, .end = (Address)_framebuffersSegmentBssEnd, .name = "framebuffers",  },
    { .start = (Address)_zbufferSegmentBssStart,      .end = (Address)_zbufferSegmentBssEnd,      .name = "zbuffer",       },
    { .start = (Address)_buffersSegmentBssStart,      .end = (Address)_buffersSegmentBssEnd,      .name = "buffers",       },
};
// For stuff that doesn't have a map symbol name.
const char* get_hardcoded_memory_str(Address addr) {
    if (addr == (Address)NULL) {
        return "NULL";
    }

    // Is addr in static surface pool?
    if (
        (addr >= (Address)gCurrStaticSurfacePool) &&
        (addr < (Address)gCurrStaticSurfacePoolEnd)
    ) {
        return "static surf pool";
    }

    // Is addr in dynamic surface pool?
    if (addr >= (Address)gDynamicSurfacePool) {
        if (addr < (Address)gDynamicSurfacePoolEnd) {
            return "dyn surf pool (active)";
        } else if (addr < (Address)((Byte*)gDynamicSurfacePool + DYNAMIC_SURFACE_POOL_SIZE)) {
            return "dyn surf pool (unused)";
        }
    }

#ifdef INCLUDE_DEBUG_MAP
    // Is addr in map symbol data?
    if ((gNumMapSymbols != 0) /* Checks whether the map data was loaded. */ && (addr >= (Address)_mapDataSegmentStart)) {
        if (addr < (Address)((Byte*)_mapDataSegmentStart + ((gMapSymbolsEnd - gMapSymbols) * sizeof(MapSymbol)))) {
            return "map symbols (data)";
        } else if (addr < (Address)_mapDataSegmentEnd) {
            return "map symbols (strings)";
        }
    }
#else // !INCLUDE_DEBUG_MAP
    // Inportant sections that had a map symbol:

    // Is addr in GFX pool?
    if (addr >= (Address)gGfxPool->buffer) {
        if (addr < (Address)gDisplayListHead) {
            return "gfx pool (used)";
        } else if (addr < (Address)gGfxPoolEnd) {
            return "gfx pool (allocated)";
        } else if (addr < (Address)((Byte*)gGfxPool->buffer + GFX_POOL_SIZE)) {
            return "gfx pool (unused)";
        }
    }

    //! TODO: audio heap, thread stacks, SPTasks, save buffer, both gGfxPools.
#endif // !INCLUDE_DEBUG_MAP

    s32 segment = get_segment_from_virtual_addr((void*)addr);
    if (segment != 0) {
        return get_segment_name(segment);
    }

    const char* hardcodedSegmentStr = get_name_from_range_list(addr, sHardcodedSegmentRanges);
    if (hardcodedSegmentStr != NULL) {
        return hardcodedSegmentStr;
    }

    // sPoolStart = SEG_POOL_START/__mainPoolStart + 0x10
    // sPoolEnd = 0x80800000 - 0x10 (POOL_SIZE)
    if (addr >= (Address)sPoolStart) {
        if (addr < ((Address)sPoolEnd - sPoolFreeSpace)) {
            return "main pool (used)";
        }
        if (addr < (Address)sPoolEnd) {
            return "main pool (unused)";
        }
    }

    return NULL;
}


// -- PROCESSOR --

// #define ENABLE_NON_N64_PRID

// https://en.wikichip.org/wiki/mips/prid_register
static const IdNamePair sPRId_names[] = {
    // LEGACY:
#ifdef ENABLE_NON_N64_PRID
    { .id = 0x01, .name = "r2000",  },
    { .id = 0x02, .name = "r3000",  },
    { .id = 0x03, .name = "r6000",  },
    { .id = 0x04, .name = "r4000",  },
    { .id = 0x06, .name = "r4000a", },
    { .id = 0x09, .name = "r10000", },
#endif // ENABLE_NON_N64_PRID
    { .id = 0x0B, .name = "vr4300", }, // r4300
#ifdef ENABLE_NON_N64_PRID
    { .id = 0x0C, .name = "vr41XX", },
    { .id = 0x0E, .name = "r12000", },
    { .id = 0x0F, .name = "r14000", },
    { .id = 0x10, .name = "r8000",  },
    { .id = 0x12, .name = "pr4450", },
    { .id = 0x20, .name = "r4600",  },
    { .id = 0x21, .name = "r4700",  },
    { .id = 0x22, .name = "tx39",   },
    { .id = 0x22, .name = "r4640",  }, // duplicate?
    { .id = 0x23, .name = "r5000",  },
    { .id = 0x2D, .name = "tx49",   },
    { .id = 0x24, .name = "sonic",  },
    { .id = 0x25, .name = "magic",  },
    { .id = 0x27, .name = "rm7000", },
    { .id = 0x28, .name = "nevada", }, // rm5260?
    { .id = 0x34, .name = "rm9000", },
    { .id = 0x42, .name = "LOONGSON1", },
    { .id = 0x54, .name = "r5432",  },
    { .id = 0x55, .name = "r5500",  },
    { .id = 0x63, .name = "LOONGSON2", },
    // MIPS:
    { .id = 0x80, .name = "4KC",     },
    { .id = 0x81, .name = "5KC",     },
    { .id = 0x82, .name = "20KC",    },
    { .id = 0x83, .name = "4Kx",     }, // 4Km or 4Kp
    { .id = 0x84, .name = "4KEC",    },
    { .id = 0x86, .name = "4KSC",    },
    { .id = 0x88, .name = "25KF",    },
    { .id = 0x89, .name = "5KE",     },
    { .id = 0x90, .name = "4KECR2",  },
    { .id = 0x91, .name = "4KEMPR2", },
    { .id = 0x92, .name = "4KSD",    },
    { .id = 0x93, .name = "24K",     },
    { .id = 0x95, .name = "34K",     },
    { .id = 0x96, .name = "24KE",    },
    { .id = 0x97, .name = "74K",     },
    { .id = 0x99, .name = "1004K",   },
    { .id = 0x9A, .name = "1074K",   },
    { .id = 0x9C, .name = "M14KC",   },
    { .id = 0x9E, .name = "M14KEC",  },
#endif // ENABLE_NON_N64_PRID
    { .id = 0xFF, .name = "unknown", },
};
const char* get_processor_name(u8 imp) {
    return str_null_fallback(get_name_from_id_list(imp, sPRId_names), "unknown");
}


// -- CAUSE --

static const char* sCauseDesc[NUM_CAUSE_DESC] = {
    [CAUSE_DESC_INT    ] = "Interrupt",
    [CAUSE_DESC_MOD    ] = "TLB modification",
    [CAUSE_DESC_RMISS  ] = "TLB exception on load or inst.",
    [CAUSE_DESC_WMISS  ] = "TLB exception on store",
    [CAUSE_DESC_RADE   ] = "Address error on load or inst.",
    [CAUSE_DESC_WADE   ] = "Address error on store",
    [CAUSE_DESC_IBE    ] = "Bus error on inst.",
    [CAUSE_DESC_DBE    ] = "Bus error on data",
    [CAUSE_DESC_SYSCALL] = "Syscall exception",
    [CAUSE_DESC_BREAK  ] = "Breakpoint exception",
    [CAUSE_DESC_II     ] = "Reserved instruction",
    [CAUSE_DESC_CPU    ] = "Coprocessor unusable",
    [CAUSE_DESC_OV     ] = "Arithmetic overflow",
    [CAUSE_DESC_TRAP   ] = "Trap exception",
    [CAUSE_DESC_VCEI   ] = "Virtual coherency on inst.",
    [CAUSE_DESC_FPE    ] = "Floating point exception",
    [CAUSE_DESC_WATCH  ] = "Watchpoint exception",
    [CAUSE_DESC_VCED   ] = "Virtual coherency on data",
};
_Bool check_for_empty_infinite_loop(Address pc, _Bool inBranchDelaySlot) {
    InsnData insn = { .raw = 0x00000000, };
    InsnData prev = { .raw = 0x00000000, };
    _Bool insnValid = try_read_word_aligned(&insn.raw, pc);
    #define INSN_IS_B_0(_insn) (((_insn).opcode == OPC_BEQ) && ((_insn).rs == (_insn).rt) && ((_insn).offset == (u16)-1))
    return (
        insnValid && (
            INSN_IS_B_0(insn) || (
                inBranchDelaySlot &&
                try_read_word_aligned(&prev.raw, (pc - sizeof(Word))) &&
                (insn.raw == 0x00000000) &&
                INSN_IS_B_0(prev)
            )
        )
    );
    #undef INSN_IS_B_0
}
// Returns a CAUSE description from 'sCauseDesc'.
const char* get_cause_desc(__OSThreadContext* tc, _Bool specific) {
    u32 cause = (tc->cause & CAUSE_EXCMASK);

    if (specific) {
        Address badvaddr = tc->badvaddr;
        Address pc = tc->pc;
        _Bool inBranchDelaySlot = ((Reg_CP0_Cause)tc->cause).BD;
        Address epc = (pc + (inBranchDelaySlot ? sizeof(Word) : 0)); // GET_EPC(pc);

        // Special crash heuristics, mostly from libdragon:
        switch (cause) {
            case EXC_INT: // Non-crash interrupts (can be shown after changing the inspected thread).
                if (pc == ADDR_INSN_WAITING_FOR_MESG) {
                    return "Waiting for mesg";
                }
                if (check_for_empty_infinite_loop(pc, inBranchDelaySlot)) {
                    return "Empty infinite loop";
                }
                break;
            case EXC_MOD:
                return "Write to read-only memory";
            case EXC_RMISS:
                if (epc == badvaddr) {
                    return "Invalid program counter address";
                } else if (badvaddr < 128) {
                    // This is probably a NULL pointer dereference, though it can go through a structure or an array,
                    // so leave some margin to the actual faulting address.
                    if (pc == ADDR_INSN_STRLEN_DEREFERENCE_ARG) { // 1st instruction of strlen
                        return "NULL string dereference (read)";
                    } else {
                        return "NULL pointer dereference (read)";
                    }
                } else {
                    return "Read from invalid memory address";
                }
                break;
            case EXC_WMISS:
                if (badvaddr < 128) {
                    return "NULL pointer dereference (write)";
                } else {
                    return "Write to invalid memory address";
                }
            case EXC_RADE:
                if (epc == badvaddr) {
                    if (is_unmapped_kx64(badvaddr)) {
                        return "Program counter in invalid 64-bit address";
                    } else {
                        return "Misaligned program counter address";
                    }
                } else {
                    if (is_unmapped_kx64(badvaddr)) {
                        return "Read from invalid 64-bit address";
                    } else {
                        return "Misaligned read from memory";
                    }
                }
                break;
            case EXC_WADE:
                return "Misaligned write to memory";
            case EXC_SYSCALL:
                if (pc == ADDR_INSN_ASSERT) {
                    return "Failed Assert (see below)";
                }
                break;
            case EXC_II:
                if (!addr_is_in_text_segment(pc)) {
                    return "Reading code in non-code segment";
                }
                break;
        }
    }

    switch (cause) {
        // Make the last two "cause" case indexes sequential for array access.
        case EXC_WATCH: cause = EXC_CODE(CAUSE_DESC_WATCH); break; // 23 -> 16
        case EXC_VCED:  cause = EXC_CODE(CAUSE_DESC_VCED ); break; // 31 -> 17
    }

    cause >>= CAUSE_EXCSHIFT;

    if (cause < ARRAY_COUNT(sCauseDesc)) {
        return sCauseDesc[cause];
    }

    return NULL;
}


// -- FPCSR/FPE --

static const char* sFpcsrDesc[NUM_FPCSR_DESC] = {
    [FPCSR_DESC_CE] = "Unimplemented operation",
    [FPCSR_DESC_CV] = "Invalid operation",
    [FPCSR_DESC_CZ] = "Division by zero",
    [FPCSR_DESC_CO] = "Overflow",
    [FPCSR_DESC_CU] = "Underflow",
    [FPCSR_DESC_CI] = "Inexact operation",
};
static const char* sFltErrDesc[NUM_FLT_ERR] = {
    [FLT_ERR_NONE  ] = "",
    [FLT_ERR_DENORM] = "Denormalized float",
    [FLT_ERR_NAN   ] = "NaN float",
};
enum FloatErrorType validate_floats_in_reg_buffer(void) {
    enum FloatErrorType fltErrType = FLT_ERR_NONE;

    for (int i = 0; i < gSavedRegBufSize; i++) {
        RegisterId reg = gSavedRegBuf[i];

        if (reg.valInfo.type == REG_VAL_TYPE_FLOAT) {
            IEEE754_f32 val = {
                .asU32 = get_reg_val(reg.cop, reg.idx)
            };
            fltErrType = validate_f32(val);

            if (fltErrType != FLT_ERR_NONE) {
                break;
            }
        }
    }

    return fltErrType;
}
// Returns a FPCSR description from 'sFpcsrDesc'.
// Only use 'specific' if disasm has just been run, because it checks saved registers with validate_floats_in_reg_buffer().
const char* get_fpcsr_desc(u32 fpcsr, _Bool specific) {
    u32 bit = BIT(FPCSR_SHIFT);

    for (u32 i = 0; i < NUM_FPCSR_DESC; i++) {
        if (fpcsr & bit) {
            if (specific && (i == FPCSR_DESC_CE)) {
                enum FloatErrorType fltErrType = validate_floats_in_reg_buffer();

                if (fltErrType != FLT_ERR_NONE) {
                    return sFltErrDesc[fltErrType];
                }
            }

            return sFpcsrDesc[i];
        }

        bit >>= 1;
    }

    return NULL;
}

// -- EMULATOR --

static const IdNamePair sEmulatorStrings[] = {
    { .id = EMU_WIIVC,            .name = "Wii VC",           },
    { .id = EMU_PROJECT64_1_OR_2, .name = "pj64 1 or 2",      },
    { .id = EMU_PROJECT64_3,      .name = "pj64 3",           },
    { .id = EMU_PROJECT64_4,      .name = "pj64 4",           },
    { .id = EMU_MUPEN_OLD,        .name = "mupen (old)",      },
    { .id = EMU_MUPEN64PLUS_NEXT, .name = "mupen64plus-next", },
    { .id = EMU_CEN64,            .name = "cen64",            },
    { .id = EMU_SIMPLE64,         .name = "simple64",         },
    { .id = EMU_PARALLELN64,      .name = "ParaLLEl N64",     },
    { .id = EMU_ARES,             .name = "ares",             },
    { .id = EMU_CONSOLE,          .name = "CONSOLE",          },
};
const char* get_emulator_name(enum Emulator emu) {
    return get_name_from_id_list(emu, sEmulatorStrings);
}


// -- MAP SYMBOL --

// https://sourceware.org/binutils/docs/binutils/nm.html
static const IdNamePair sMapSymbolTypes[] = {
    { .id = 'a', .name = "absolute (static)", }, // Local absolute symbol.
    { .id = 'A', .name = "absolute",          }, // Global absolute symbol.
    { .id = 'b', .name = ".bss (static)",     }, // Local bss symbol.
    { .id = 'B', .name = ".bss",              }, // Global bss symbol.
    { .id = 'd', .name = ".data (static)",    }, // Local data symbol.
    { .id = 'D', .name = ".data",             }, // Global data symbol.
    { .id = 'N', .name = "debug",             }, // Debugging symbol.
    { .id = 'r', .name = ".rodata (static)",  }, // Local read only symbol.
    { .id = 'R', .name = ".rodata",           }, // Global read only symbol.
    { .id = 't', .name = ".text (static)",    }, // Local text symbol.
    { .id = 'T', .name = ".text",             }, // Global text symbol.
    { .id = 'W', .name = "weak (untagged)",   }, // Untagged weak symbol.
    { .id = 'U', .name = "undefined",         }, // Undefined symbol.
};
const char* get_map_symbol_type_desc(char c) {
    return get_name_from_id_list(c, sMapSymbolTypes);
}
