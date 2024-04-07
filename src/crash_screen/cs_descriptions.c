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


typedef struct IdNamePair {
    /*0x00*/ const int id;
    /*0x04*/ const char* name;
} IdNamePair; /*0x08*/
static const char* get_name_from_id_list_impl(int id, const IdNamePair* list, size_t count) {
    for (size_t i = 0; i < count; i++) {
        if (id == list[i].id) {
            return list[i].name;
        }
    }

    return NULL;
}
#define get_name_from_id_list(_id, _list) get_name_from_id_list_impl((_id), (_list), ARRAY_COUNT(_list))

typedef struct RangeNamePair {
    /*0x00*/ const u32 start;
    /*0x04*/ const u32 end;
    /*0x08*/ const char* name;
} RangeNamePair; /*0x0C*/
static const char* get_name_from_range_list_impl(u32 id, const RangeNamePair* list, size_t count) {
    for (size_t i = 0; i < count; i++) {
        if ((id >= list[i].start) && (id < list[i].end)) {
            return list[i].name;
        }
    }

    return NULL;
}
#define get_name_from_range_list(_id, _list) get_name_from_range_list_impl((_id), (_list), ARRAY_COUNT(_list))


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
    [__builtin_ctz(OS_STATE_STOPPED )] = "stopped",
    [__builtin_ctz(OS_STATE_RUNNABLE)] = "runnable",
    [__builtin_ctz(OS_STATE_RUNNING )] = "running",
    [__builtin_ctz(OS_STATE_WAITING )] = "waiting",
};
const char* get_thread_state_str(OSThread* thread) {
    u16 state = thread->state;
    if (state == 0x0000) return NULL;
    return sThreadStateStrings[__builtin_ctz(state)];
}

static const char* sThreadFlagStrings[] = {
    [__builtin_ctz(OS_FLAG_CPU_BREAK)] = "break", // CPU break
    [__builtin_ctz(OS_FLAG_FAULT    )] = "fault",
};
const char* get_thread_flags_str(OSThread* thread) {
    u16 flags = thread->flags;
    if (flags == 0x0000) return NULL;
    return sThreadFlagStrings[__builtin_ctz(flags)];
}


// -- WARP NODE PRESETS --

static IdNamePair sWarpNodeSpecialIds[] = {
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
const char* get_warp_node_name(const enum WarpNodes id) {
    const char* ret = get_name_from_id_list(id, sWarpNodeSpecialIds);
    return ((ret != NULL) ? ret : "");
}


// -- SEGMENTS --

static const char* sSegmentNames[32] = {
    [SEGMENT_MAIN               ] = "main",
    [SEGMENT_RENDER             ] = "render",
    [SEGMENT_SEGMENT2           ] = "segment2",
    [SEGMENT_COMMON1_YAY0       ] = "common1 gfx",
    [SEGMENT_GROUP0_YAY0        ] = "group0 gfx",
    [SEGMENT_GROUPA_YAY0        ] = "groupA gfx",
    [SEGMENT_GROUPB_YAY0        ] = "groupB gfx",
    [SEGMENT_LEVEL_DATA         ] = "level data",
    [SEGMENT_COMMON0_YAY0       ] = "common0 gfx",
    [SEGMENT_TEXTURE            ] = "textures",
    [SEGMENT_SKYBOX             ] = "skybox",
    [SEGMENT_EFFECT_YAY0        ] = "effects",
    [SEGMENT_GROUPA_GEO         ] = "groupA geo",
    [SEGMENT_GROUPB_GEO         ] = "groupB geo",
    [SEGMENT_LEVEL_SCRIPT       ] = "level script",
    [SEGMENT_COMMON0_GEO        ] = "common0 geo",
    [SEGMENT_LEVEL_ENTRY        ] = "level entry",
    [SEGMENT_MARIO_ANIMS        ] = "mario anims",
    [SEGMENT_UNKNOWN_18         ] = "18",
    [SEGMENT_BEHAVIOR_DATA      ] = "bhv data",
    [SEGMENT_MENU_INTRO         ] = "menu",
    [SEGMENT_GLOBAL_LEVEL_SCRIPT] = "global level script",
    [SEGMENT_COMMON1_GEO        ] = "common1 geo",
    [SEGMENT_GROUP0_GEO         ] = "group0 geo",
    [SEGMENT_DEMO_INPUTS        ] = "demo inputs",
    [SEGMENT_EU_TRANSLATION     ] = "translations",
    [SEGMENT_UNKNOWN_26         ] = "26",
    [SEGMENT_UNKNOWN_27         ] = "27",
    [SEGMENT_UNKNOWN_28         ] = "28",
    [SEGMENT_UNKNOWN_29         ] = "29",
    [SEGMENT_UNKNOWN_30         ] = "30",
    [SEGMENT_UNKNOWN_31         ] = "31",
};
const char* get_segment_name(u8 segmentId) {
    return ((segmentId < ARRAY_COUNT(sSegmentNames)) ? sSegmentNames[segmentId] : NULL);
}

#define DEFINE_ROM_SEGMENT_START()

static const IdNamePair sLevelROMSegments[] = {
#define STUB_LEVEL(textname, _1, _2, _3, _4, _5, _6, _7, _8)
#define DEFINE_LEVEL(textname, _1, _2, folder, _4, _5, _6, _7, _8, _9, _10) { .id = (uintptr_t)(_##folder##_segment_7SegmentRomStart), .name = textname },
#include "levels/level_defines.h"
#undef STUB_LEVEL
#undef DEFINE_LEVEL
    { .id = (uintptr_t)_debug_level_select_yay0SegmentRomStart, .name = "level select", },
};
static const IdNamePair sTextureBinROMSegments[] = {
    { .id = (uintptr_t)_fire_yay0SegmentRomStart,     .name = "fire",     },
    { .id = (uintptr_t)_spooky_yay0SegmentRomStart,   .name = "spooky",   },
    { .id = (uintptr_t)_generic_yay0SegmentRomStart,  .name = "generic",  },
    { .id = (uintptr_t)_water_yay0SegmentRomStart,    .name = "water",    },
    { .id = (uintptr_t)_sky_yay0SegmentRomStart,      .name = "sky",      },
    { .id = (uintptr_t)_snow_yay0SegmentRomStart,     .name = "snow",     },
    { .id = (uintptr_t)_cave_yay0SegmentRomStart,     .name = "cave",     },
    { .id = (uintptr_t)_machine_yay0SegmentRomStart,  .name = "machine",  },
    { .id = (uintptr_t)_mountain_yay0SegmentRomStart, .name = "mountain", },
    { .id = (uintptr_t)_grass_yay0SegmentRomStart,    .name = "grass",    },
    { .id = (uintptr_t)_outside_yay0SegmentRomStart,  .name = "outside",  },
    { .id = (uintptr_t)_inside_yay0SegmentRomStart,   .name = "inside",   },
    { .id = (uintptr_t)_effect_yay0SegmentRomStart,   .name = "effect",   },
};
static const IdNamePair sSkyboxBinROMSegments[] = {
    { .id = (uintptr_t)_water_skybox_yay0SegmentRomStart,       .name = "water",        },
    { .id = (uintptr_t)_ccm_skybox_yay0SegmentRomStart,         .name = "ccm",          },
    { .id = (uintptr_t)_clouds_skybox_yay0SegmentRomStart,      .name = "clouds",       },
    { .id = (uintptr_t)_bitfs_skybox_yay0SegmentRomStart,       .name = "bitfs",        },
    { .id = (uintptr_t)_wdw_skybox_yay0SegmentRomStart,         .name = "wdw",          },
    { .id = (uintptr_t)_cloud_floor_skybox_yay0SegmentRomStart, .name = "cloud floor",  },
    { .id = (uintptr_t)_ssl_skybox_yay0SegmentRomStart,         .name = "ssl",          },
    { .id = (uintptr_t)_bbh_skybox_yay0SegmentRomStart,         .name = "bbh",          },
    { .id = (uintptr_t)_bidw_skybox_yay0SegmentRomStart,        .name = "bidw",         },
    { .id = (uintptr_t)_bits_skybox_yay0SegmentRomStart,        .name = "bits",         },
    { .id = (uintptr_t)_title_screen_bg_yay0SegmentRomStart,    .name = "title screen", },
};
static const IdNamePair sGroupA_yay0ROMSegments[] = {
    { .id = (uintptr_t)_group1_yay0SegmentRomStart,  .name = "group1",  },
    { .id = (uintptr_t)_group2_yay0SegmentRomStart,  .name = "group2",  },
    { .id = (uintptr_t)_group3_yay0SegmentRomStart,  .name = "group3",  },
    { .id = (uintptr_t)_group4_yay0SegmentRomStart,  .name = "group4",  },
    { .id = (uintptr_t)_group5_yay0SegmentRomStart,  .name = "group5",  },
    { .id = (uintptr_t)_group6_yay0SegmentRomStart,  .name = "group6",  },
    { .id = (uintptr_t)_group7_yay0SegmentRomStart,  .name = "group7",  },
    { .id = (uintptr_t)_group8_yay0SegmentRomStart,  .name = "group8",  },
    { .id = (uintptr_t)_group9_yay0SegmentRomStart,  .name = "group9",  },
    { .id = (uintptr_t)_group10_yay0SegmentRomStart, .name = "group10", },
    { .id = (uintptr_t)_group11_yay0SegmentRomStart, .name = "group11", },
};
static const IdNamePair sGroupA_geoROMSegments[] = {
    { .id = (uintptr_t)_group1_geoSegmentRomStart,  .name = "group1",  },
    { .id = (uintptr_t)_group2_geoSegmentRomStart,  .name = "group2",  },
    { .id = (uintptr_t)_group3_geoSegmentRomStart,  .name = "group3",  },
    { .id = (uintptr_t)_group4_geoSegmentRomStart,  .name = "group4",  },
    { .id = (uintptr_t)_group5_geoSegmentRomStart,  .name = "group5",  },
    { .id = (uintptr_t)_group6_geoSegmentRomStart,  .name = "group6",  },
    { .id = (uintptr_t)_group7_geoSegmentRomStart,  .name = "group7",  },
    { .id = (uintptr_t)_group8_geoSegmentRomStart,  .name = "group8",  },
    { .id = (uintptr_t)_group9_geoSegmentRomStart,  .name = "group9",  },
    { .id = (uintptr_t)_group10_geoSegmentRomStart, .name = "group10", },
    { .id = (uintptr_t)_group11_geoSegmentRomStart, .name = "group11", },
};
static const IdNamePair sGroupB_yay0ROMSegments[] = {
    { .id = (uintptr_t)_group12_yay0SegmentRomStart, .name = "group12", },
    { .id = (uintptr_t)_group13_yay0SegmentRomStart, .name = "group13", },
    { .id = (uintptr_t)_group14_yay0SegmentRomStart, .name = "group14", },
    { .id = (uintptr_t)_group15_yay0SegmentRomStart, .name = "group15", },
    { .id = (uintptr_t)_group16_yay0SegmentRomStart, .name = "group16", },
    { .id = (uintptr_t)_group17_yay0SegmentRomStart, .name = "group17", },
};
static const IdNamePair sGroupB_geoROMSegments[] = {
    { .id = (uintptr_t)_group12_geoSegmentRomStart, .name = "group12", },
    { .id = (uintptr_t)_group13_geoSegmentRomStart, .name = "group13", },
    { .id = (uintptr_t)_group14_geoSegmentRomStart, .name = "group14", },
    { .id = (uintptr_t)_group15_geoSegmentRomStart, .name = "group15", },
    { .id = (uintptr_t)_group16_geoSegmentRomStart, .name = "group16", },
    { .id = (uintptr_t)_group17_geoSegmentRomStart, .name = "group17", },
};
static const IdNamePair sMenuIntroROMSegments[] = {
    { .id = (uintptr_t)_introSegmentRomStart,  .name = "intro", },
    { .id = (uintptr_t)_menuSegmentRomStart,   .name = "intro", },
    { .id = (uintptr_t)_endingSegmentRomStart, .name = "intro", },
};
#if MULTILANG
//! TODO: Update this when ASCII PR is merged:
static const IdNamePair sTranslationROMSegments[] = {
    { .id = (uintptr_t)_translation_en_yay0SegmentRomStart, .name = "english", },
    { .id = (uintptr_t)_translation_fr_yay0SegmentRomStart, .name = "french",  },
    { .id = (uintptr_t)_translation_de_yay0SegmentRomStart, .name = "german",  },
};
#endif // MULTILANG

// Get the name of what is currently loaded in a segment.
const char* get_segment_sub_name(u8 segmentId) {
    Address romAddr = sSegmentROMTable[segmentId];
    if (romAddr == 0) {
        return NULL;
    }
    switch (segmentId) {
        //! TODO: Remaining segments.
        case SEGMENT_LEVEL_DATA:        return get_name_from_id_list(romAddr, sLevelROMSegments);
        case SEGMENT_TEXTURE:           return get_name_from_id_list(romAddr, sTextureBinROMSegments);
        case SEGMENT_SKYBOX:            return get_name_from_id_list(romAddr, sSkyboxBinROMSegments);
        case SEGMENT_COMMON0_YAY0:      return ((romAddr == (uintptr_t)_common0_yay0SegmentRomStart) ? "common0" : NULL);
        case SEGMENT_COMMON0_GEO:       return ((romAddr == (uintptr_t)_common0_geoSegmentRomStart ) ? "common0" : NULL);
        case SEGMENT_COMMON1_YAY0:      return ((romAddr == (uintptr_t)_common1_yay0SegmentRomStart) ? "common1" : NULL);
        case SEGMENT_COMMON1_GEO:       return ((romAddr == (uintptr_t)_common1_geoSegmentRomStart ) ? "common1" : NULL);
        case SEGMENT_GROUP0_YAY0:       return ((romAddr == (uintptr_t)_group0_yay0SegmentRomStart ) ? "group0"  : NULL);
        case SEGMENT_GROUP0_GEO:        return ((romAddr == (uintptr_t)_group0_geoSegmentRomStart  ) ? "group0"  : NULL);
        case SEGMENT_GROUPA_YAY0:       return get_name_from_id_list(romAddr, sGroupA_yay0ROMSegments);
        case SEGMENT_GROUPA_GEO:        return get_name_from_id_list(romAddr, sGroupA_geoROMSegments);
        case SEGMENT_GROUPB_YAY0:       return get_name_from_id_list(romAddr, sGroupB_yay0ROMSegments);
        case SEGMENT_GROUPB_GEO:        return get_name_from_id_list(romAddr, sGroupB_geoROMSegments);
        case SEGMENT_MENU_INTRO:        return get_name_from_id_list(romAddr, sMenuIntroROMSegments);
#if MULTILANG
        case SEGMENT_EU_TRANSLATION:    return get_name_from_id_list(romAddr, sTranslationROMSegments);
#endif // MULTILANG
    }

    return NULL;
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
const IdNamePair sPRId_names[] = {
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
    const char* ret = get_name_from_id_list(imp, sPRId_names);
    return ((ret != NULL) ? ret : "unknown");
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
    { .id = 'W', .name = "weak (untagged)",   },
    { .id = 'U', .name = "undefined",         }, // Undefined symbol.
};
const char* get_map_symbol_type_desc(char c) {
    return get_name_from_id_list(c, sMapSymbolTypes);
}
