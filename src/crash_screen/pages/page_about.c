#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/address_select.h"
#include "crash_screen/crash_controls.h"
#include "crash_screen/crash_draw.h"
#include "crash_screen/crash_main.h"
#include "crash_screen/crash_pages.h"
#include "crash_screen/crash_print.h"
#include "crash_screen/crash_settings.h"
#include "crash_screen/map_parser.h"

#include "page_about.h"

#include "config/config_world.h"
#include "game/emutest.h"
#include "game/version.h"
#ifdef UNF
#include "usb/debug.h"
#endif // UNF


struct CSSetting cs_settings_group_page_about[] = {
    [CS_OPT_HEADER_PAGE_ABOUT       ] = { .type = CS_OPT_TYPE_HEADER,  .name = "ABOUT",                          .valNames = &gValNames_bool,          .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_END_ABOUT               ] = { .type = CS_OPT_TYPE_END, },
};


const enum ControlTypes cs_cont_list_about[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_HIDE_CRASH_SCREEN,
#ifdef UNF
    CONT_DESC_OS_PRINT,
#endif // UNF
    CONT_DESC_LIST_END,
};


// Region string:
#define DEF_REGION_NAME(name) static const char region_name[3] = TO_STRING2(name);
#ifdef VERSION_JP
DEF_REGION_NAME(jp);
#elif VERSION_US
DEF_REGION_NAME(us);
#elif VERSION_EU
DEF_REGION_NAME(eu);
#elif VERSION_SH
DEF_REGION_NAME(sh);
#elif BBPLAYER
DEF_REGION_NAME(bb);
#else
DEF_REGION_NAME(xx);
#endif

// osTvType strings:
static const char osTvTypeStrings[][5] = {
    [OS_TV_PAL ] = "pal",
    [OS_TV_NTSC] = "ntsc",
    [OS_TV_MPAL] = "mpal",
};

// Microcode string:
#define DEF_UCODE_NAME(name) static const char* ucode_name = TO_STRING2(name);
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
#define DEF_SAVETYPE_NAME(name) static const char savetype_name[8] = TO_STRING2(name);
#ifdef EEP4K
DEF_SAVETYPE_NAME(eep4k);
#elif EEP16K
DEF_SAVETYPE_NAME(eep16k);
#elif SRAM
DEF_SAVETYPE_NAME(sram);
#else
DEF_SAVETYPE_NAME(unknown);
#endif

// Compression type string:
#define DEF_COMPRESSION_NAME(name) static const char compression_name[5] = TO_STRING2(name);
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

// Emulator strings:
static const EmulatorName sEmulatorStrings[] = {
    { .bits = EMU_WIIVC,            .name = "Wii VC",           },
    { .bits = EMU_PROJECT64_1_OR_2, .name = "pj64 1 or 2",      },
    { .bits = EMU_PROJECT64_3,      .name = "pj64 3",           },
    { .bits = EMU_PROJECT64_4,      .name = "pj64 4",           },
    { .bits = EMU_MUPEN_OLD,        .name = "mupen (old)",      },
    { .bits = EMU_MUPEN64PLUS_NEXT, .name = "mupen64plus-next", },
    { .bits = EMU_CEN64,            .name = "cen64",            },
    { .bits = EMU_SIMPLE64,         .name = "simple64",         },
    { .bits = EMU_PARALLELN64,      .name = "ParallelN64",      },
    { .bits = EMU_ARES,             .name = "ares",             },
    { .bits = EMU_CONSOLE,          .name = "CONSOLE",          },
};

const char* get_emulator_name(enum Emulator emu) {
    for (int i = 0; i < ARRAY_COUNT(sEmulatorStrings); i++) {
        if (emu == sEmulatorStrings[i].bits) {
            return sEmulatorStrings[i].name;
        }
    }

    return NULL;
}

void page_about_init(void) {

}

const char gValNames_no_yes[][4] = {
    [FALSE] = "NO",
    [TRUE ] = "YES",
};

extern const u8 gRomSize[];

#ifndef UNF
    #define debug_is_initialized() FALSE
#endif // !UNF

void page_about_draw(void) {
    _Bool debug_mode = FALSE;
#ifdef DEBUG
    debug_mode = TRUE;
#endif // DEBUG

    u32 line = 2;

    const s32 centerX = (CRASH_SCREEN_NUM_CHARS_X / 2);
    cs_print(TEXT_X(centerX - ((STRLEN("HackerSM64 ") + strlen(HackerSM64_version_txt) + 0) / 2)), TEXT_Y(line++), STR_COLOR_PREFIX"HackerSM64 %s", COLOR_RGBA32_CRASH_PAGE_NAME, HackerSM64_version_txt);
    cs_print(TEXT_X(centerX - ((STRLEN("Crash Screen ") + strlen(crash_screen_version) + 1) / 2)), TEXT_Y(line++), STR_COLOR_PREFIX"Crash Screen %s", COLOR_RGBA32_CRASH_PAGE_NAME, crash_screen_version);
    const RGBA32 valColor = COLOR_RGBA32_LIGHT_GRAY;
    cs_print(TEXT_X(0), TEXT_Y(line++), "COMPILER:\n  "STR_COLOR_PREFIX"%s",            valColor, __compiler__);
    line++;
    cs_print(TEXT_X(0), TEXT_Y(line++), "LINKER:\n  "STR_COLOR_PREFIX"%s",              valColor, __linker__);
    line++;

    line++;
    cs_print(TEXT_X(0), TEXT_Y(line++), "ROM NAME:\t\t"STR_COLOR_PREFIX"%s",            valColor, INTERNAL_ROM_NAME);
#ifdef LIBDRAGON
    cs_print(TEXT_X(0), TEXT_Y(line++), "LIBULTRA:\t\t"STR_COLOR_PREFIX"%s",            valColor, "LIBDRAGON");
#else // !LIBDRAGON
    cs_print(TEXT_X(0), TEXT_Y(line++), "LIBULTRA:\t\t"STR_COLOR_PREFIX"%s (patch %i)", valColor, OS_MAJOR_VERSION, OS_MINOR_VERSION);
#endif // !LIBDRAGON
    cs_print(TEXT_X(0), TEXT_Y(line++), "MICROCODE:\t\t"STR_COLOR_PREFIX"%s",           valColor, ucode_name);
    cs_print(TEXT_X(0), TEXT_Y(line++), "REGION:\t\t\t"STR_COLOR_PREFIX"%s (%s)",       valColor, region_name, osTvTypeStrings[osTvType]);
    cs_print(TEXT_X(0), TEXT_Y(line++), "SAVE TYPE:\t\t"STR_COLOR_PREFIX"%s",           valColor, savetype_name);
    cs_print(TEXT_X(0), TEXT_Y(line++), "COMPRESSION:\t"STR_COLOR_PREFIX"%s",           valColor, compression_name);
    cs_print(TEXT_X(0), TEXT_Y(line++), "ROM SIZE:\t\t"STR_COLOR_PREFIX"%i bytes",      valColor, (size_t)gRomSize);
    cs_print(TEXT_X(0), TEXT_Y(line++), "RAM SIZE:\t\t"STR_COLOR_PREFIX"%imb",          valColor, (TOTAL_RAM_SIZE / RAM_1MB));
    cs_print(TEXT_X(0), TEXT_Y(line++), "EXTBOUNDS MODE:\t"STR_COLOR_PREFIX"%d",        valColor, EXTENDED_BOUNDS_MODE);
    cs_print(TEXT_X(0), TEXT_Y(line++), "RCVI HACK:\t\t"STR_COLOR_PREFIX"%s",           valColor, gValNames_no_yes[VI.comRegs.vSync == (525 * 20)]);
    cs_print(TEXT_X(0), TEXT_Y(line++), "DEBUG MODE:\t\t"STR_COLOR_PREFIX"%s%s",        valColor, gValNames_no_yes[debug_mode], (debug_is_initialized() ? " +unf" : ""));
    cs_print(TEXT_X(0), TEXT_Y(line), "EMULATOR:\t\t"STR_COLOR_PREFIX"%s",              valColor, get_emulator_name(gEmulator));
#ifdef LIBPL
    if (gSupportsLibpl) {
        cs_print(TEXT_X(charX), TEXT_Y(line++), STR_COLOR_PREFIX"+ libpl v%d",          valColor, LPL_ABI_VERSION_CURRENT);
    }
#endif // LIBPL
}

void page_about_input(void) {

}

void page_about_print(void) {
#ifdef UNF
    debug_printf("\n");

    _Bool debug_mode = FALSE;
 #ifdef DEBUG
    debug_mode = TRUE;
 #endif // DEBUG

    debug_printf("- HackerSM64\t\t%s",              HackerSM64_version_txt);
    debug_printf("- Crash screen\t\t%s\n",          crash_screen_version);
    debug_printf("- COMPILER:\t\t%s\n",             __compiler__);
    debug_printf("- LINKER:\t\t%s\n",               __linker__);
    debug_printf("- ROM NAME:\t\t%s\n",             INTERNAL_ROM_NAME);
#ifdef LIBDRAGON
    debug_printf("- LIBULTRA:\t\t%s\n",             "LIBDRAGON");
#else // !LIBDRAGON
    debug_printf("- LIBULTRA:\t\t%s (patch %i)\n",  OS_MAJOR_VERSION, OS_MINOR_VERSION);
#endif // !LIBDRAGON
    debug_printf("- MICROCODE:\t\t%s\n",            ucode_name);
    debug_printf("- REGION:\t\t%s (%s)\n",          region_name, osTvTypeStrings[osTvType]);
    debug_printf("- SAVE TYPE:\t\t%s\n",            savetype_name);
    debug_printf("- COMPRESSION:\t\t%s\n",          compression_name);
    debug_printf("- ROM SIZE:\t\t%i bytes\n",       (size_t)gRomSize);
    debug_printf("- RAM SIZE:\t\t%imb\n",           (TOTAL_RAM_SIZE / RAM_1MB));
    debug_printf("- EXTBOUNDS MODE:\t%d\n",         EXTENDED_BOUNDS_MODE);
    debug_printf("- RCVI HACK:\t\t%s\n",            gValNames_no_yes[VI.comRegs.vSync == (525 * 20)]);
    debug_printf("- DEBUG MODE:\t\t%s%s\n",         gValNames_no_yes[debug_mode], (debug_is_initialized() ? " +UNF" : ""));
    debug_printf("- EMULATOR:\t\t%s\n",             get_emulator_name(gEmulator));
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
