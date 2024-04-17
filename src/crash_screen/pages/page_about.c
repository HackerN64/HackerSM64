#include <ultra64.h>

#include <stdarg.h>
#include <string.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/util/map_parser.h"
#include "crash_screen/cs_controls.h"
#include "crash_screen/cs_descriptions.h"
#include "crash_screen/cs_draw.h"
#include "crash_screen/cs_main.h"
#include "crash_screen/cs_pages.h"
#include "crash_screen/cs_print.h"
#include "crash_screen/cs_settings.h"

#include "page_about.h"

#include "config/config_world.h"
#include "engine/surface_load.h"
#include "game/emutest.h"
#include "game/save_file.h"
#include "game/version.h"
#ifdef UNF
#include "usb/usb.h"
#include "usb/debug.h"
#else // !UNF
#define debug_is_initialized() FALSE
#endif // !UNF
#ifdef LIBPL
#include "lib/libpl/libpl.h"
#endif // LIBPL


struct CSSetting cs_settings_group_page_about[] = {
    [CS_OPT_HEADER_PAGE_ABOUT       ] = { .type = CS_OPT_TYPE_HEADER,  .name = "ABOUT",                          .valNames = &gValNames_bool,          .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_END_ABOUT               ] = { .type = CS_OPT_TYPE_END, },
};


const enum ControlTypes cs_cont_list_about[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_PAGE_SELECT,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_HIDE_CRASH_SCREEN,
#ifdef UNF
    CONT_DESC_OS_PRINT,
#endif // UNF
    CONT_DESC_CURSOR_VERTICAL,
    CONT_DESC_LIST_END,
};


static u32 sAboutSelectedIndex = 0;
static u32 sAboutViewportIndex = 0;


const char gValNames_no_yes[][4] = {
    [FALSE] = "no",
    [TRUE ] = "yes",
};


extern const u8 gRomSize[];
extern const u8 gGoddardSize[];


f32 percent_of(f32 size, f32 totalSize) {
    return (size / totalSize * 100.0f);
}

f32 bytes_to_megabytes(size_t size) {
    return ((f32)size / (f32)RAM_1MB);
}

#define STR_SUFFIX_BYTES        "b"
#define STR_SUFFIX_KILOBYTES    "kb"
#define STR_SUFFIX_MEGABYTES    "mb"
#define STR_ABOUT_DECIMAL_FMT   "%.2g"
#define STR_PERCENT_OF_ROM      "(~"STR_ABOUT_DECIMAL_FMT"%%%%rom)" //! TODO: Why does the first "%%" get eaten when using a float here?
#define STR_MAJOR_MINOR_PATCH   "%d.%d.%d"
#define STR_LPL_VERSION         "v"STR_MAJOR_MINOR_PATCH

#define ABOUT_ENTRY_FUNC(_name, fmt, ...) void _cs_about_func_##_name(char* buf) { sprintf(buf, fmt, ##__VA_ARGS__); }

ABOUT_ENTRY_FUNC(empty,          "")
ABOUT_ENTRY_FUNC(hackersm64_v,   HackerSM64_version_txt)
ABOUT_ENTRY_FUNC(crash_screen_v, CrashScreen_version_txt)
ABOUT_ENTRY_FUNC(compiler,       __compiler__) // __VERSION__ // STR_MAJOR_MINOR_PATCH, __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__
ABOUT_ENTRY_FUNC(linker,         __linker__)
#ifdef __STDC__
ABOUT_ENTRY_FUNC(stdc_version,   "%luL", __STDC_VERSION__)
#endif // __STDC__
ABOUT_ENTRY_FUNC(rom_name,       INTERNAL_ROM_NAME)
ABOUT_ENTRY_FUNC(libultra,       "%s (patch %d)", OS_MAJOR_VERSION, OS_MINOR_VERSION)
ABOUT_ENTRY_FUNC(microcode,      gUcodeName)
#ifdef LIBDRAGON
//! TODO: Libdragon version:
ABOUT_ENTRY_FUNC(region,         "LIBDRAGON")
#else // !LIBDRAGON
ABOUT_ENTRY_FUNC(region,         "%s (%s)", gRegionName, osTvTypeStrings[osTvType])
#endif //! LIBDRAGON
void _cs_about_func_save_type(char* buf) {
    char* p = buf;
    p += sprintf(p, "%s (", gSaveTypeName);
    p += sprintf_int_with_commas(p, EEPROM_SIZE);
    p += sprintf(p, STR_SUFFIX_BYTES")");
}
ABOUT_ENTRY_FUNC(compression,    gCompressionName)
void _cs_about_func_crash_screen(char* buf) {
    size_t csSize = ((Address)_crashscreenSegmentRomEnd - (Address)_crashscreenSegmentRomStart);
    char* p = buf;
    p += sprintf_int_with_commas(p, csSize);
    p += sprintf(p, (STR_SUFFIX_BYTES" "STR_PERCENT_OF_ROM), percent_of(csSize, (size_t)gRomSize));
}
void _cs_about_func_symbols(char* buf) {
#ifdef INCLUDE_DEBUG_MAP
    size_t numSymbols = (gMapSymbolsEnd - gMapSymbols);
    size_t mapSize = ((Address)_mapDataSegmentRomEnd - (Address)_mapDataSegmentRomStart);
    char* p = buf;
    p += sprintf(p, "%i=", numSymbols);
    p += sprintf_int_with_commas(p, mapSize);
    p += sprintf(p, (STR_SUFFIX_BYTES" "STR_PERCENT_OF_ROM), percent_of(mapSize, (size_t)gRomSize));
#else  // !INCLUDE_DEBUG_MAP
    sprintf(buf, "NOT INCLUDED");
#endif // !INCLUDE_DEBUG_MAP
}
void _cs_about_func_rom_size(char* buf) {
    char* p = buf;
    size_t romSize = (size_t)gRomSize;
    p += sprintf_int_with_commas(p, romSize);
    p += sprintf(p, (STR_SUFFIX_BYTES" (~"STR_ABOUT_DECIMAL_FMT STR_SUFFIX_MEGABYTES")"), bytes_to_megabytes(romSize));
}
void _cs_about_func_ram_size(char* buf) {
    char* p = buf;
    p += sprintf_int_with_commas(p, osMemSize);
    p += sprintf(p, (STR_SUFFIX_BYTES" ("STR_ABOUT_DECIMAL_FMT STR_SUFFIX_MEGABYTES")"), bytes_to_megabytes(osMemSize));
}
ABOUT_ENTRY_FUNC(gfx_pool,       STR_HEX_PREFIX"%X (%d cmds)", GFX_POOL_SIZE, (GFX_POOL_SIZE / sizeof(Gfx)))
#ifdef KEEP_MARIO_HEAD
void _cs_about_func_goddard(char* buf) {
    size_t goddardSize = (size_t)gGoddardSize;
    char* p = buf;
    p += sprintf_int_with_commas(p, goddardSize);
    p += sprintf(p, STR_SUFFIX_BYTES" "STR_PERCENT_OF_ROM, percent_of(goddardSize, (size_t)gRomSize));
}
#else // !KEEP_MARIO_HEAD
ABOUT_ENTRY_FUNC(goddard,        gValNames_no_yes[FALSE])
#endif // !KEEP_MARIO_HEAD
ABOUT_ENTRY_FUNC(st_surf_pool,   (STR_HEX_PREFIX"%X"), ((Address)gCurrStaticSurfacePoolEnd - (Address)gCurrStaticSurfacePool))
ABOUT_ENTRY_FUNC(dyn_surf_pool,  (STR_HEX_PREFIX"%X/"STR_HEX_PREFIX"%X"), ((Address)gDynamicSurfacePoolEnd - (Address)gDynamicSurfacePool), (size_t)DYNAMIC_SURFACE_POOL_SIZE)
ABOUT_ENTRY_FUNC(level_bounds,   (STR_HEX_PREFIX"%04X (%dx)"), LEVEL_BOUNDARY_MAX, (LEVEL_BOUNDARY_MAX / 0x2000))
ABOUT_ENTRY_FUNC(cell_size,      (STR_HEX_PREFIX"%03X (%dx%d)"), CELL_SIZE, ((LEVEL_BOUNDARY_MAX * 2) / CELL_SIZE), ((LEVEL_BOUNDARY_MAX * 2) / CELL_SIZE))
ABOUT_ENTRY_FUNC(world_scale,    "(%dx)", WORLD_SCALE)
ABOUT_ENTRY_FUNC(rcvi_hack,      gValNames_no_yes[VI.comRegs.vSync == (525 * 20)])
#ifdef SILHOUETTE
ABOUT_ENTRY_FUNC(silhouette,     "opacity: %d", SILHOUETTE)
#else // !SILHOUETTE
ABOUT_ENTRY_FUNC(silhouette,     gValNames_no_yes[FALSE])
#endif // !SILHOUETTE
//! TODO: Update this to #ifdef when Input PR is merged.
#if ENABLE_RUMBLE
ABOUT_ENTRY_FUNC(rumble,         "%s%s", gValNames_no_yes[sRumblePakThreadActive], (sRumblePakActive ? " +pack" : ""))
#else // !ENABLE_RUMBLE
ABOUT_ENTRY_FUNC(rumble,         gValNames_no_yes[FALSE])
#endif // !ENABLE_RUMBLE
void _cs_about_func_debug_mode(char* buf) {
    char* p = buf;
#ifdef DEBUG
    p += sprintf(p, gValNames_no_yes[TRUE]);
#else // !DEBUG
    p += sprintf(p, gValNames_no_yes[FALSE]);
#endif // !DEBUG
#ifdef PUPPYPRINT_DEBUG
    p += sprintf(p, " +ppdebug");
#endif // PUPPYPRINT_DEBUG
    if (debug_is_initialized()) {
        p += sprintf(p, " +unf");
    }
}
void _cs_about_func_emulator(char* buf) {
    char* p = buf;
    p += sprintf(p, "%s", get_emulator_name(gEmulator));
    if (gEmulator & EMU_CONSOLE) {
        // u32 val = 0x00000000;
        // ASM_GET_REG_COP0(val, "$15"); // TODO: use get_reg_val
        Reg_CP0_PRId prid = {
            .raw = (u32)get_direct_reg_val(COP0, REG_CP0_PRID),
        };
        p += sprintf(p, " (%s rev%d.%d)", get_processor_name(prid.Imp), prid.Rev_.major, prid.Rev_.minor);
    } else if (gSupportsLibpl) {
        const lpl_version* v = libpl_get_core_version();
        p += sprintf(p, " "STR_LPL_VERSION, v->major, v->minor, v->patch);
    }
}
#ifdef LIBPL
ABOUT_ENTRY_FUNC(gfx_plugin,     libpl_get_graphics_plugin()->name);
void _cs_about_func_launcher(char* buf) {
    const lpl_version* v = libpl_get_launcher_version();
    sprintf(buf, STR_LPL_VERSION, v->major, v->minor, v->patch);
}
ABOUT_ENTRY_FUNC(libpl_version,  "%d", (gSupportsLibpl ? LPL_ABI_VERSION_CURRENT : 0));
void _cs_about_func_cheat_flags(char* buf) {
    char* p = buf;
    lpl_cheat_flags cheatFlags = libpl_get_cheat_flags();
    if (cheatFlags == 0) {
        p += sprintf(p, "none");
        return;
    }
    const char flags[][2] = {
        [CTZ(LPL_USED_CHEATS       )] = "gs",
        [CTZ(LPL_USED_SAVESTATES   )] = "ss",
        [CTZ(LPL_USED_SLOWDOWN     )] = "sl",
        [CTZ(LPL_USED_FRAME_ADVANCE)] = "fa",
        [CTZ(LPL_USED_SPEEDUP      )] = "sp",
    };
    u32 flag = 0x1;
    for (int i = 0; i < ARRAY_COUNT(flags); i++) {
        if (cheatFlags & flag) {
            p += sprintf(p, "[%s]", flags[i]);
        }
        flag <<= 1;
    }
}
ABOUT_ENTRY_FUNC(rhdc,           "%s", libpl_get_my_rhdc_username());
#endif // LIBPL


// Extra long string buffer for long entries to save space in sAboutEntries.
char gLongInfoBuffer[NUM_LONG_INFO_BUFFERS][LONG_INFO_BUFFER_SIZE];


#define ABOUT_ENTRY_GAP()                      { .desc = "",    .func = NULL,                   .info = "", .type = CS_ABOUT_ENTRY_TYPE_NONE,       }
#define ABOUT_ENTRY_TITLE(_name, _desc)        { .desc = _desc, .func = _cs_about_func_##_name, .info = "", .type = CS_ABOUT_ENTRY_TYPE_TITLE,      }
#define ABOUT_ENTRY_BUTTON(_desc)              { .desc = _desc, .func = NULL,                   .info = "", .type = CS_ABOUT_ENTRY_TYPE_BUTTON,     }
#define ABOUT_ENTRY_SINGLE(_name, _desc)       { .desc = _desc, .func = _cs_about_func_##_name, .info = "", .type = CS_ABOUT_ENTRY_TYPE_SINGLE,     }
#define ABOUT_ENTRY_SINGLE_LPL(_name, _desc)   { .desc = _desc, .func = _cs_about_func_##_name, .info = "", .type = CS_ABOUT_ENTRY_TYPE_SINGLE_LPL, }
#define ABOUT_ENTRY_LONG1(_name, _desc)        { .desc = _desc, .func = NULL,                   .info = "", .type = CS_ABOUT_ENTRY_TYPE_SINGLE,     }
#define ABOUT_ENTRY_LONG2(_name, _desc)        { .desc = "",    .func = _cs_about_func_##_name, .info = "", .type = CS_ABOUT_ENTRY_TYPE_LONG,       }
#define ABOUT_ENTRY_NULL()                     { .desc = "",    .func = NULL,                   .info = "", .type = CS_ABOUT_ENTRY_TYPE_NULL,       }
#define ABOUT_ENTRY_HEADER(_desc, _expandable) {    \
    .desc = _desc,                                  \
    .func = NULL,                                   \
    .flags = {                                      \
        .expanded  = TRUE,                          \
    },                                              \
    .type = CS_ABOUT_ENTRY_TYPE_HEADER,             \
}
CSAboutEntry sCSAboutEntries_title[CS_NUM_ABOUT_ENTRIES_TITLE] = {
    [CS_ABOUT_ENTRY_TITLE_HACKERSM64  ] = ABOUT_ENTRY_TITLE(hackersm64_v,   "HackerSM64"    ),
    [CS_ABOUT_ENTRY_TITLE_CRASH_SCREEN] = ABOUT_ENTRY_TITLE(crash_screen_v, "Crash Screen"  ),
    [CS_ABOUT_ENTRY_TITLE_END         ] = ABOUT_ENTRY_NULL(),
};
CSAboutEntry sCSAboutEntries_buttons[CS_NUM_ABOUT_ENTRIES_TITLE] = {
    [CS_ABOUT_ENTRY_BUTTONS_EXPAND_ALL  ] = ABOUT_ENTRY_BUTTON("expand all"  ),
    [CS_ABOUT_ENTRY_BUTTONS_COLLAPSE_ALL] = ABOUT_ENTRY_BUTTON("collapse all"),
    [CS_ABOUT_ENTRY_BUTTONS_END         ] = ABOUT_ENTRY_NULL(),
};
CSAboutEntry sCSAboutEntries_compiler[CS_NUM_ABOUT_ENTRIES_COMPILER] = {
    [CS_ABOUT_GROUP_HEADER_COMPILER      ] = ABOUT_ENTRY_HEADER("compiler", TRUE),
    [CS_ABOUT_ENTRY_COMPILER_COMPILER_1  ] = ABOUT_ENTRY_LONG1(compiler,       "COMPILER"      ),
    [CS_ABOUT_ENTRY_COMPILER_COMPILER_2  ] = ABOUT_ENTRY_LONG2(compiler,       "COMPILER"      ),
    [CS_ABOUT_ENTRY_COMPILER_LINKER_1    ] = ABOUT_ENTRY_LONG1(linker,         "LINKER"        ),
    [CS_ABOUT_ENTRY_COMPILER_LINKER_2    ] = ABOUT_ENTRY_LONG2(linker,         "LINKER"        ),
#ifdef __STDC__
    [CS_ABOUT_ENTRY_COMPILER_STDC_VERSION] = ABOUT_ENTRY_SINGLE(stdc_version,  "STDC VERSION"  ),
#endif // __STDC__
    [CS_ABOUT_ENTRY_COMPILER_END         ] = ABOUT_ENTRY_NULL(),
};
CSAboutEntry sCSAboutEntries_rom[CS_NUM_ABOUT_ENTRIES_ROM] = {
    [CS_ABOUT_GROUP_HEADER_ROM      ] = ABOUT_ENTRY_HEADER("rom", TRUE),
    [CS_ABOUT_ENTRY_ROM_ROM_NAME    ] = ABOUT_ENTRY_SINGLE(rom_name,      "ROM NAME"      ),
    [CS_ABOUT_ENTRY_ROM_LIBULTRA    ] = ABOUT_ENTRY_SINGLE(libultra,      "LIBULTRA"      ),
    [CS_ABOUT_ENTRY_ROM_MICROCODE   ] = ABOUT_ENTRY_SINGLE(microcode,     "MICROCODE"     ),
    [CS_ABOUT_ENTRY_ROM_REGION      ] = ABOUT_ENTRY_SINGLE(region,        "REGION"        ),
    [CS_ABOUT_ENTRY_ROM_SAVE_TYPE   ] = ABOUT_ENTRY_SINGLE(save_type,     "SAVE TYPE"     ),
    [CS_ABOUT_ENTRY_ROM_COMPRESSION ] = ABOUT_ENTRY_SINGLE(compression,   "COMPRESSION"   ),
    [CS_ABOUT_ENTRY_ROM_CRASH_SCREEN] = ABOUT_ENTRY_SINGLE(crash_screen,  "CRASH SCREEN"  ),
    [CS_ABOUT_ENTRY_ROM_SYMBOLS     ] = ABOUT_ENTRY_SINGLE(symbols,       "MAP SYMBOLS"   ),
    [CS_ABOUT_ENTRY_ROM_ROM_SIZE    ] = ABOUT_ENTRY_SINGLE(rom_size,      "ROM SIZE"      ),
    [CS_ABOUT_ENTRY_ROM_RAM_SIZE    ] = ABOUT_ENTRY_SINGLE(ram_size,      "RAM SIZE"      ),
    [CS_ABOUT_ENTRY_ROM_END         ] = ABOUT_ENTRY_NULL(),
};
CSAboutEntry sCSAboutEntries_collision[CS_NUM_ABOUT_ENTRIES_COLLISION] = {
    [CS_ABOUT_GROUP_HEADER_COLLISION       ] = ABOUT_ENTRY_HEADER("collision", TRUE),
    [CS_ABOUT_ENTRY_COLLISION_LEVEL_BOUNDS ] = ABOUT_ENTRY_SINGLE(level_bounds,  "LEVEL BOUND"   ),
    [CS_ABOUT_ENTRY_COLLISION_CELL_SIZE    ] = ABOUT_ENTRY_SINGLE(cell_size,     "CELL SIZE"     ),
    [CS_ABOUT_ENTRY_COLLISION_WORLD_SCALE  ] = ABOUT_ENTRY_SINGLE(world_scale,   "WORLD SCALE"   ),
    [CS_ABOUT_ENTRY_COLLISION_ST_SURF_POOL ] = ABOUT_ENTRY_SINGLE(st_surf_pool,  "STATIC SURF"   ),
    [CS_ABOUT_ENTRY_COLLISION_DYN_SURF_POOL] = ABOUT_ENTRY_SINGLE(dyn_surf_pool, "DYN SURF"      ),
    [CS_ABOUT_ENTRY_COLLISION_END          ] = ABOUT_ENTRY_NULL(),
};
CSAboutEntry sCSAboutEntries_misc[CS_NUM_ABOUT_ENTRIES_MISC] = {
    [CS_ABOUT_GROUP_HEADER_MISC    ] = ABOUT_ENTRY_HEADER("misc", TRUE),
    [CS_ABOUT_ENTRY_MISC_GFX_POOL  ] = ABOUT_ENTRY_SINGLE(gfx_pool,      "GFX POOL"      ),
    [CS_ABOUT_ENTRY_MISC_GODDARD   ] = ABOUT_ENTRY_SINGLE(goddard,       "GODDARD"       ),
    [CS_ABOUT_ENTRY_MISC_RCVI_HACK ] = ABOUT_ENTRY_SINGLE(rcvi_hack,     "RCVI HACK"     ),
    [CS_ABOUT_ENTRY_MISC_SILHOUETTE] = ABOUT_ENTRY_SINGLE(silhouette,    "SILHOUETTE"    ),
    [CS_ABOUT_ENTRY_MISC_RUMBLE    ] = ABOUT_ENTRY_SINGLE(rumble,        "RUMBLE"        ),
    [CS_ABOUT_ENTRY_MISC_DEBUG_MODE] = ABOUT_ENTRY_SINGLE(debug_mode,    "DEBUG MODE"    ),
    [CS_ABOUT_ENTRY_MISC_END       ] = ABOUT_ENTRY_NULL(),
};
CSAboutEntry sCSAboutEntries_emulator[CS_NUM_ABOUT_ENTRIES_EMULATOR] = {
    [CS_ABOUT_GROUP_HEADER_EMULATOR       ] = ABOUT_ENTRY_HEADER("emulator", TRUE),
    [CS_ABOUT_ENTRY_EMULATOR_EMULATOR     ] = ABOUT_ENTRY_SINGLE(emulator,      "EMULATOR"      ),
#ifdef LIBPL
    [CS_ABOUT_ENTRY_EMULATOR_GFX_PLUGIN   ] = ABOUT_ENTRY_SINGLE_LPL(gfx_plugin,    "GFX PLUGIN"    ),
    [CS_ABOUT_ENTRY_EMULATOR_LAUNCHER     ] = ABOUT_ENTRY_SINGLE_LPL(launcher,      "LAUNCHER"      ),
    [CS_ABOUT_ENTRY_EMULATOR_LIBPL_VERSION] = ABOUT_ENTRY_SINGLE_LPL(libpl_version, "LIBPL"         ),
    [CS_ABOUT_ENTRY_EMULATOR_CHEAT_FLAGS  ] = ABOUT_ENTRY_SINGLE_LPL(cheat_flags,   "CHEAT FLAGS"   ),
    [CS_ABOUT_ENTRY_EMULATOR_RHDC         ] = ABOUT_ENTRY_SINGLE_LPL(rhdc,          "RHDC"          ),
#endif // LIBPL
    [CS_ABOUT_ENTRY_EMULATOR_END          ] = ABOUT_ENTRY_NULL(),
};

CSAboutEntry* sCSAboutEntryGroups[CS_NUM_ABOUT_GROUPS] = {
    [CS_ABOUT_GROUP_TITLE    ] = sCSAboutEntries_title,
    [CS_ABOUT_GROUP_BUTTONS  ] = sCSAboutEntries_buttons,
    [CS_ABOUT_GROUP_COMPILER ] = sCSAboutEntries_compiler,
    [CS_ABOUT_GROUP_ROM      ] = sCSAboutEntries_rom,
    [CS_ABOUT_GROUP_COLLISION] = sCSAboutEntries_collision,
    [CS_ABOUT_GROUP_MISC     ] = sCSAboutEntries_misc,
    [CS_ABOUT_GROUP_EMULATOR ] = sCSAboutEntries_emulator,
};

CSAboutEntryDisplay sCSAboutDisplayedEntries[
    CS_NUM_ABOUT_ENTRIES_TITLE     +
    CS_NUM_ABOUT_ENTRIES_BUTTONS   +
    CS_NUM_ABOUT_ENTRIES_COMPILER  +
    CS_NUM_ABOUT_ENTRIES_ROM       +
    CS_NUM_ABOUT_ENTRIES_COLLISION +
    CS_NUM_ABOUT_ENTRIES_MISC      +
    CS_NUM_ABOUT_ENTRIES_EMULATOR
];
u32 sNumCSAboutDisplayedEntries = 0;


static CSAboutEntry* get_about_entry(enum CSAboutGroups groupID, int entryID) {
    return &sCSAboutEntryGroups[groupID][entryID];
}

void about_set_all_headers(_Bool expand) {
    for (enum CSAboutGroups groupID = 0; groupID < CS_NUM_ABOUT_GROUPS; groupID++) {
        CSAboutEntry* entry = get_about_entry(groupID, 0);
        if (entry->type == CS_ABOUT_ENTRY_TYPE_HEADER) {
            entry->flags.expanded = expand;
        }
    }
}

_Bool about_check_all_headers(_Bool expand) {
    for (enum CSAboutGroups groupID = 0; groupID < CS_NUM_ABOUT_GROUPS; groupID++) {
        CSAboutEntry* entry = get_about_entry(groupID, 0);
        if ((entry->type == CS_ABOUT_ENTRY_TYPE_HEADER) && (entry->flags.expanded == expand)) {
            return TRUE;
        }
    }

    return FALSE;
}

void append_to_displayed_entries(enum CSAboutGroups groupID, int entryID) {
    sCSAboutDisplayedEntries[sNumCSAboutDisplayedEntries++] = (CSAboutEntryDisplay){
        .groupID = groupID,
        .entryID = entryID,
    };
}

void fill_entry_info_buffers(void) {
    bzero(gLongInfoBuffer, sizeof(gLongInfoBuffer));
    u32 longBufferIndex = 0;

    // Fill entry buffers;
    for (enum CSAboutGroups groupID = 0; groupID < CS_NUM_ABOUT_GROUPS; groupID++) {
        CSAboutEntry* entry = get_about_entry(groupID, 0);
        while (entry->type != CS_ABOUT_ENTRY_TYPE_NULL) {
            char* buf = entry->info;

            // For subsequent crashes:
            if (entry->type > CS_ABOUT_ENTRY_TYPE_LONG) {
                entry->type = CS_ABOUT_ENTRY_TYPE_LONG;
            }

            if (entry->type == CS_ABOUT_ENTRY_TYPE_LONG) {
                entry->type = (CS_ABOUT_ENTRY_TYPE_LONG_N + longBufferIndex);
                buf = gLongInfoBuffer[longBufferIndex];
                longBufferIndex++;
            }

            if (entry->func != NULL) {
                entry->func(buf);
            }

            entry++;
        }
    }
}

void update_displayed_about_entries(void) {
    bzero(&sCSAboutDisplayedEntries, sizeof(sCSAboutDisplayedEntries));
    sNumCSAboutDisplayedEntries = 0;
    _Bool sectionShown = TRUE;

    for (enum CSAboutGroups groupID = 0; groupID < CS_NUM_ABOUT_GROUPS; groupID++) {
        int entryID = 0;

        CSAboutEntry* group = sCSAboutEntryGroups[groupID];
        CSAboutEntry* entry = &group[entryID];
        if (entry->type == CS_ABOUT_ENTRY_TYPE_HEADER) {
            sectionShown = entry->flags.expanded;

            append_to_displayed_entries(groupID, entryID);

            entryID++;
        }

        while (sectionShown) {
            entry = &group[entryID];

            if ((entry == NULL) || (entry->type == CS_ABOUT_ENTRY_TYPE_NULL)) {
                break;
            }

            if ((entry->type == CS_ABOUT_ENTRY_TYPE_SINGLE_LPL) && !gSupportsLibpl) {
                entryID++;
                continue;
            }

            append_to_displayed_entries(groupID, entryID);
            entryID++;
        }
    }
}

void page_about_init(void) {
    sAboutSelectedIndex = 0;
    sAboutViewportIndex = 0;

    fill_entry_info_buffers();
    update_displayed_about_entries();
}

void cs_print_about_entry(ScreenCoord_u32 y, s16 groupID, s16 entryID) {
    CSAboutEntry* entry = get_about_entry(groupID, entryID);
    if (entry == NULL) {
        return;
    }
    const CSTextCoord_u32 section_indent = STRLEN("> ");
    RGBA32 tempDefaultColor = gCSDefaultPrintColor; // CS_SET_DEFAULT_PRINT_COLOR_START()?

    switch (entry->type) {
        case CS_ABOUT_ENTRY_TYPE_NULL:
        case CS_ABOUT_ENTRY_TYPE_NONE:
            break;
        case CS_ABOUT_ENTRY_TYPE_HEADER:
            if (entry->desc != NULL) {
                gCSDefaultPrintColor = COLOR_RGBA32_CRASH_PAGE_NAME;
                _Bool expanded = entry->flags.expanded;
                cs_draw_triangle(TEXT_X(0), y, TEXT_WIDTH(1), TEXT_WIDTH(1), gCSDefaultPrintColor, (expanded ? CS_TRI_DOWN : CS_TRI_RIGHT));
                cs_print(TEXT_X(section_indent), y, "%s info%s", entry->desc, (expanded ? ":" : ""));
            }
            // Translucent divider.
            cs_draw_divider_translucent((y - 2) + TEXT_HEIGHT(1));
            break;
        case CS_ABOUT_ENTRY_TYPE_TITLE:
            if ((entry->desc != NULL) && (entry->info != NULL)) {
                gCSDefaultPrintColor = COLOR_RGBA32_CRASH_PAGE_NAME;
                cs_print(TEXT_X((CRASH_SCREEN_NUM_CHARS_X / 2) - (((strlen(entry->desc) + STRLEN(" ") + strlen(entry->info)) / 2))), y,
                    "%s %s", entry->desc, entry->info
                );
            }
            break;
        case CS_ABOUT_ENTRY_TYPE_BUTTON:
            _Bool buttonCond = FALSE;

            if (groupID == CS_ABOUT_GROUP_BUTTONS) {
                switch (entryID) {
                    case CS_ABOUT_ENTRY_BUTTONS_EXPAND_ALL:   buttonCond = about_check_all_headers(FALSE); break;
                    case CS_ABOUT_ENTRY_BUTTONS_COLLAPSE_ALL: buttonCond = about_check_all_headers(TRUE ); break;
                }
            }
            if (entry->desc != NULL) {
                CSTextCoord_u32 x = TEXT_X((CRASH_SCREEN_NUM_CHARS_X / 2) - (((STRLEN("<") + strlen(entry->desc) + STRLEN(">")) / 2)));
                if (buttonCond) {
                    cs_print(x, y,
                        STR_COLOR_PREFIX"<"STR_COLOR_PREFIX"%s"STR_COLOR_PREFIX">",
                        COLOR_RGBA32_CRASH_SELECT_ARROW,
                        COLOR_RGBA32_CRASH_SETTINGS_DESCRIPTION, entry->desc,
                        COLOR_RGBA32_CRASH_SELECT_ARROW
                    );
                } else {
                    cs_print(x, y,
                        STR_COLOR_PREFIX"<%s>",
                        COLOR_RGBA32_CRASH_SETTINGS_DISABLED, entry->desc
                    );
                }
            }
            break;
        case CS_ABOUT_ENTRY_TYPE_SINGLE_LPL:
            if (!gSupportsLibpl) {
                break;
            }
            FALL_THROUGH;
        case CS_ABOUT_ENTRY_TYPE_SINGLE:
            if (entry->desc != NULL) {
                cs_print(TEXT_X(section_indent), y, "%s:", entry->desc);
            }
            if (entry->info != NULL) {
                gCSDefaultPrintColor = COLOR_RGBA32_LIGHT_GRAY;
                cs_print(TEXT_X(section_indent + 16), y, entry->info);
            }
            break;
        default: // CS_ABOUT_ENTRY_TYPE_LONG_N
            if (entry->info != NULL) {
                u32 logInfoBufferIndex = (entry->type - CS_ABOUT_ENTRY_TYPE_LONG_N);
                gCSDefaultPrintColor = COLOR_RGBA32_LIGHT_GRAY;
                cs_print(TEXT_X(section_indent + 1), y, gLongInfoBuffer[logInfoBufferIndex]);
            }
            break;
    }

    gCSDefaultPrintColor = tempDefaultColor;
}

void page_about_draw(void) {
    CSTextCoord_u32 line = 1;
    u32 currViewIndex = sAboutViewportIndex;

    for (CSTextCoord_u32 i = 0; i < ABOUT_PAGE_NUM_SCROLLABLE_ENTRIES; i++, currViewIndex++) {
        if (currViewIndex >= sNumCSAboutDisplayedEntries) {
            break;
        }

        CSAboutEntryDisplay* display = &sCSAboutDisplayedEntries[currViewIndex];
        s16 groupID = display->groupID;
        s16 entryID = display->entryID;

        ScreenCoord_u32 y = TEXT_Y(line + i);

        if (currViewIndex == sAboutSelectedIndex) {
            cs_draw_row_selection_box(y);
        }

        cs_print_about_entry(y, groupID, entryID);
    }

    // Scroll Bar:
    if (sNumCSAboutDisplayedEntries > ABOUT_PAGE_NUM_SCROLLABLE_ENTRIES) {
        cs_draw_scroll_bar(
            (DIVIDER_Y(line) + 1), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y),
            ABOUT_PAGE_NUM_SCROLLABLE_ENTRIES, sNumCSAboutDisplayedEntries,
            sAboutViewportIndex,
            COLOR_RGBA32_CRASH_SCROLL_BAR, TRUE
        );

        cs_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y));
    }

    osWritebackDCacheAll();
}

void page_about_input(void) {
    u16 buttonPressed = gCSCompositeController->buttonPressed;

    if (buttonPressed & (A_BUTTON | B_BUTTON)) {
        CSAboutEntryDisplay* display = &sCSAboutDisplayedEntries[sAboutSelectedIndex];
        s16 groupID = display->groupID;
        s16 entryID = display->entryID;
        CSAboutEntry* entry = get_about_entry(groupID, entryID);

        switch (entry->type) {
            case CS_ABOUT_ENTRY_TYPE_BUTTON:
                if (groupID == CS_ABOUT_GROUP_BUTTONS) {
                    switch (entryID) {
                        case CS_ABOUT_ENTRY_BUTTONS_EXPAND_ALL:   about_set_all_headers(TRUE ); break;
                        case CS_ABOUT_ENTRY_BUTTONS_COLLAPSE_ALL: about_set_all_headers(FALSE); break;
                    }
                }
                break;
            case CS_ABOUT_ENTRY_TYPE_HEADER:
                entry->flags.expanded ^= TRUE;
                break;
            default:
                break;
        }
    }

    update_displayed_about_entries();

    s32 change = 0;
    if (gCSDirectionFlags.pressed.up  ) change = -1; // Scroll up.
    if (gCSDirectionFlags.pressed.down) change = +1; // Scroll down.
    sAboutSelectedIndex = WRAP(((s32)sAboutSelectedIndex + change), 0, (s32)(sNumCSAboutDisplayedEntries - 1));

    sAboutViewportIndex = cs_clamp_view_to_selection(sAboutViewportIndex, sAboutSelectedIndex, ABOUT_PAGE_NUM_SCROLLABLE_ENTRIES, 1);
}

void page_about_print(void) {
#ifdef UNF
    osSyncPrintf("\n");

    for (enum CSAboutGroups groupID = 0; groupID < CS_NUM_ABOUT_GROUPS; groupID++) {
        CSAboutEntry* entry = get_about_entry(groupID, 0);
        if (entry == NULL) {
            break;
        }
        while (entry->type != CS_ABOUT_ENTRY_TYPE_NULL) {
            switch (entry->type) {
                case CS_ABOUT_ENTRY_TYPE_NULL:
                case CS_ABOUT_ENTRY_TYPE_NONE:
                case CS_ABOUT_ENTRY_TYPE_BUTTON:
                    break;
                case CS_ABOUT_ENTRY_TYPE_HEADER:
                    if (entry->desc != NULL) {
                        osSyncPrintf("- [%s info]\n", entry->desc);
                    }
                    break;
                case CS_ABOUT_ENTRY_TYPE_TITLE:
                    if ((entry->desc != NULL) && (entry->info != NULL)) {
                        osSyncPrintf("- %s %s", entry->desc, entry->info); // Both entries that use this already have a newline at the end of their info.
                    }
                    break;
                case CS_ABOUT_ENTRY_TYPE_SINGLE_LPL:
                    if (!gSupportsLibpl) {
                        break;
                    }
                    FALL_THROUGH;
                case CS_ABOUT_ENTRY_TYPE_SINGLE:
                    if (entry->desc != NULL) {
                        osSyncPrintf("-- %s:", entry->desc);
                    }
                    if (entry->info != NULL) {
                        osSyncPrintf("\t\t%s\n", entry->info);
                    }
                    break;
                default: // CS_ABOUT_ENTRY_TYPE_LONG_N
                    if (entry->info != NULL) {
                        u32 logInfoBufferIndex = (entry->type - CS_ABOUT_ENTRY_TYPE_LONG_N);
                        osSyncPrintf("--- %s\n", gLongInfoBuffer[logInfoBufferIndex]);
                    }
                    break;
            }

            entry++;
        }
    }
#endif // UNF
}


struct CSPage gCSPage_about ={
    .name         = "ABOUT",
    .initFunc     = page_about_init,
    .drawFunc     = page_about_draw,
    .inputFunc    = page_about_input,
    .printFunc    = page_about_print,
    .contList     = cs_cont_list_about,
    .settingsList = cs_settings_group_page_about,
    .flags = {
        .initialized = FALSE,
        .crashed     = FALSE,
    },
};
