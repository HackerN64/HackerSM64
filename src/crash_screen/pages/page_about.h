#pragma once

#include <ultra64.h>

#include "types.h"

#include "crash_screen/crash_settings.h"

#include "game/emutest.h"


enum CSSettingsGroup_page_about {
    CS_OPT_HEADER_PAGE_ABOUT,
    CS_OPT_END_ABOUT,
};


enum AboutEntryTypes {
    ABOUT_ENTRY_TYPE_NONE,
    ABOUT_ENTRY_TYPE_TITLE,
    ABOUT_ENTRY_TYPE_SUBTITLE,
    ABOUT_ENTRY_TYPE_SINGLE,
    ABOUT_ENTRY_TYPE_LONG,
};

enum AboutEntries {
    ABOUT_ENTRY_GAP_1,
    ABOUT_ENTRY_HACKERSM64,
    ABOUT_ENTRY_CRASH_SCREEN,
    ABOUT_ENTRY_COMPILER_1,
    ABOUT_ENTRY_COMPILER_2,
    ABOUT_ENTRY_LINKER_1,
    ABOUT_ENTRY_LINKER_2,
    // ABOUT_ENTRY_GAP_2,
    ABOUT_ENTRY_ROM_NAME,
    ABOUT_ENTRY_LIBULTRA,
    ABOUT_ENTRY_MICROCODE,
    ABOUT_ENTRY_REGION,
    ABOUT_ENTRY_SAVE_TYPE,
    ABOUT_ENTRY_COMPRESSION,
    ABOUT_ENTRY_ROM_SIZE,
    ABOUT_ENTRY_RAM_SIZE,
    ABOUT_ENTRY_GFX_POOL_SIZE,
    ABOUT_ENTRY_DYN_SURF_POOL,
    ABOUT_ENTRY_LEVEL_BOUNDS,
    ABOUT_ENTRY_CELL_SIZE,
    ABOUT_ENTRY_WORLD_SCALE,
    ABOUT_ENTRY_RCVI_HACK,
    ABOUT_ENTRY_GODDARD,
    ABOUT_ENTRY_DEBUG_MODE,
    ABOUT_ENTRY_EMULATOR,
#ifdef LIBPL
    ABOUT_ENTRY_GFX_PLUGIN,
#define FIRST_LIBPL_ENTRY ABOUT_ENTRY_GFX_PLUGIN
    ABOUT_ENTRY_LAUNCHER,
    ABOUT_ENTRY_LIBPL_VERSION,
#endif // LIBPL
    NUM_ABOUT_ENTRIES,
};


typedef struct EmulatorName {
    /*0x00*/ const enum Emulator bits;
    /*0x04*/ const char* name;
} EmulatorName; /*0x08*/

typedef struct {
    /*0x00*/ enum AboutEntryTypes type;
    /*0x04*/ const char* desc;
    /*0x08*/ void (*func)(char* buf);
    /*0x0C*/ char info[32];
} AboutEntry; /*0x2C*/


#define ABOUT_PAGE_NUM_SCROLLABLE_ENTRIES 21


extern struct CSSetting cs_settings_group_page_about[];
extern struct CSPage gCSPage_about;
