#include <ultra64.h>

#include "types.h"
#include "sm64.h"

#include "crash_main.h"
#include "crash_pages.h"

#include "crash_settings.h"

#include "engine/math_util.h"


const char* sIsSettingHeader[] = {
    [FALSE] = "HIDDEN",
    [TRUE ] = "SHOWN",
};

const char* sValNames_bool[] = {
    [FALSE] = "FALSE",
    [TRUE ] = "TRUE",
};

const char* sValNames_print_num_fmt[] = {
    [PRINT_NUM_FMT_HEX] = "HEX",
    [PRINT_NUM_FMT_DEC] = "DECIMAL",
    [PRINT_NUM_FMT_SCI] = "SCIENTIFIC",
};

const char* sValNames_branch_arrow[] = {
    [DISASM_ARROW_MODE_OFF      ] = "OFF",
    [DISASM_ARROW_MODE_SELECTION] = "SELECTION",
#ifdef INCLUDE_DEBUG_MAP
    [DISASM_ARROW_MODE_FUNCTION ] = "FUNCTION",
#endif
    [DISASM_ARROW_MODE_OVERSCAN ] = "OVERSCAN", //! TODO: Implement this in page_disasm.c.
};

#ifdef INCLUDE_DEBUG_MAP
    #define SHOW_FUNC_NAMES_DEFAULT     TRUE
    #define DISASM_ARROW_MODE_DEFAULT   DISASM_ARROW_MODE_FUNCTION
#else
    #define SHOW_FUNC_NAMES_DEFAULT     FALSE
    #define DISASM_ARROW_MODE_DEFAULT   DISASM_ARROW_MODE_SELECTION
#endif

#define SECTION_EXPANDED_DEFAULT FALSE

struct CSSettingsEntry gCSSettings[NUM_CS_OPTS] = { //! TODO: Callback functions. //! TODO: Collapsible(?) page name non-setting entries (A+B to reset only that page to default)
    [CS_OPT_EXPAND_ALL          ] = { .name = "expand all",                 .valNames = &sValNames_bool,          .val = FALSE,                     .defaultVal = FALSE,                     .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_COLLAPSE_ALL        ] = { .name = "collapse all",               .valNames = &sValNames_bool,          .val = FALSE,                     .defaultVal = FALSE,                     .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_RESET_TO_DEFAULTS   ] = { .name = "reset all to defaults",      .valNames = &sValNames_bool,          .val = FALSE,                     .defaultVal = FALSE,                     .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    // GLOBAL:
    [CS_OPT_HEADER_GLOBAL       ] = { .name = "GLOBAL",                     .valNames = &sIsSettingHeader,        .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_DRAW_SCREENSHOT     ] = { .name = "Show screenshot background", .valNames = &sValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
#ifdef INCLUDE_DEBUG_MAP
    [CS_OPT_SYMBOL_NAMES        ] = { .name = "Print symbol names",         .valNames = &sValNames_bool,          .val = SHOW_FUNC_NAMES_DEFAULT,   .defaultVal = SHOW_FUNC_NAMES_DEFAULT,   .lowerBound = FALSE,                 .upperBound = TRUE,                       },
#endif
    // CONTROLS:
    [CS_OPT_HEADER_CONTROLS     ] = { .name = "CONTROLS",                   .valNames = &sIsSettingHeader,        .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_PRINT_SCROLL_SPEED  ] = { .name = "Text scroll speed",          .valNames = NULL,                     .val = 2,                         .defaultVal = 2,                         .lowerBound = 0,                     .upperBound = 5,                          },
    [CS_OPT_CURSOR_WAIT_FRAMES  ] = { .name = "Hold direction wait frames", .valNames = NULL,                     .val = 10,                        .defaultVal = 10,                        .lowerBound = 0,                     .upperBound = 1000,                       },
    [CS_OPT_ANALOG_DEADZONE     ] = { .name = "Analog deadzone",            .valNames = NULL,                     .val = 60,                        .defaultVal = 60,                        .lowerBound = 0,                     .upperBound = 128,                        },
    // CONTEXT:
    [CS_OPT_HEADER_PAGE_CONTEXT ] = { .name = "CONTEXT",                    .valNames = &sIsSettingHeader,        .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
#ifdef INCLUDE_DEBUG_MAP
    [CS_OPT_CONTEXT_PARSE_REG   ] = { .name = "Parse register addr names",  .valNames = &sValNames_bool,          .val = FALSE,                     .defaultVal = FALSE,                     .lowerBound = FALSE,                 .upperBound = TRUE,                       },
#endif
    [CS_OPT_CONTEXT_FLOATS_FMT  ] = { .name = "Floats print format",        .valNames = &sValNames_print_num_fmt, .val = PRINT_NUM_FMT_DEC,         .defaultVal = PRINT_NUM_FMT_DEC,         .lowerBound = PRINT_NUM_FMT_HEX,     .upperBound = PRINT_NUM_FMT_SCI,          },
    // LOG:
    [CS_OPT_HEADER_PAGE_LOG     ] = { .name = "LOG",                        .valNames = &sIsSettingHeader,        .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_LOG_INDEX_NUMBERS   ] = { .name = "Show index numbers",         .valNames = &sValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    // STACK TRACE:
    [CS_OPT_HEADER_PAGE_STACK   ] = { .name = "STACK TRACE",                .valNames = &sIsSettingHeader,        .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_STACK_SHOW_ADDRESS  ] = { .name = "Show addresses",             .valNames = &sValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_STACK_SHOW_OFFSET   ] = { .name = "Show offsets",               .valNames = &sValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    // RAM VIEW:
    [CS_OPT_HEADER_PAGE_MEMORY  ] = { .name = "RAM VIEW",                   .valNames = &sIsSettingHeader,        .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
#ifdef INCLUDE_DEBUG_MAP
    [CS_OPT_MEMORY_SHOW_SYMBOL  ] = { .name = "Show current symbol name",   .valNames = &sValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
#endif
    [CS_OPT_MEMORY_AS_ASCII     ] = { .name = "Show data as ascii",         .valNames = &sValNames_bool,          .val = FALSE,                     .defaultVal = FALSE,                     .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    // DISASM:
    [CS_OPT_HEADER_PAGE_DISASM  ] = { .name = "DISASM",                     .valNames = &sIsSettingHeader,        .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
#ifdef INCLUDE_DEBUG_MAP
    [CS_OPT_DISASM_SHOW_SYMBOL  ] = { .name = "Show current symbol name",   .valNames = &sValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
#endif
    [CS_OPT_DISASM_BINARY       ] = { .name = "Unknown as binary",          .valNames = &sValNames_bool,          .val = FALSE,                     .defaultVal = FALSE,                     .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_DISASM_PSEUDOINSNS  ] = { .name = "Pseudoinstructions",         .valNames = &sValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_DISASM_IMM_FMT      ] = { .name = "Immediates format",          .valNames = &sValNames_print_num_fmt, .val = PRINT_NUM_FMT_HEX,         .defaultVal = PRINT_NUM_FMT_HEX,         .lowerBound = PRINT_NUM_FMT_HEX,     .upperBound = PRINT_NUM_FMT_DEC,          },
    [CS_OPT_DISASM_OFFSET_ADDR  ] = { .name = "Offsets as addresses",       .valNames = &sValNames_bool,          .val = FALSE,                     .defaultVal = FALSE,                     .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_DISASM_ARROW_MODE   ] = { .name = "Branch arrow mode",          .valNames = &sValNames_branch_arrow,  .val = DISASM_ARROW_MODE_DEFAULT, .defaultVal = DISASM_ARROW_MODE_DEFAULT, .lowerBound = DISASM_ARROW_MODE_OFF, .upperBound = DISASM_ARROW_MODE_OVERSCAN, },
};

// Check whether 'settingID' is a header entry.
_Bool crash_screen_setting_is_header(enum CSSettings settingID) {
    return (gCSSettings[settingID].valNames == &sIsSettingHeader);
}

// Increment/wrap value.
void crash_screen_inc_setting(enum CSSettings settingID, SettingsType inc) {
    struct CSSettingsEntry* setting = &gCSSettings[settingID];

    setting->val = WRAP((setting->val + inc), setting->lowerBound, setting->upperBound);
}

// Reset a specific setting to its default.
void crash_screen_reset_setting(enum CSSettings settingID) {
    struct CSSettingsEntry* setting = &gCSSettings[settingID];

    setting->val = setting->defaultVal;
}

// Reset a whole section to defaults.
void crash_screen_reset_settings_section(enum CSSettings settingID) {
    for (settingID++; settingID < ARRAY_COUNT(gCSSettings); settingID++) {
        if (crash_screen_setting_is_header(settingID)) {
            break;
        }

        crash_screen_reset_setting(settingID);
    }
}

// Reset all settings to their defaults.
void crash_screen_reset_all_settings(void) {
    for (enum CSSettings settingID = 0; settingID < ARRAY_COUNT(gCSSettings); settingID++) {
        if (!crash_screen_setting_is_header(settingID)) {
            crash_screen_reset_setting(settingID);
        }
    }
}

// Returns whether any settings in gCSSettings have been changed from their default value.
_Bool crash_screen_check_for_changed_settings(void) {
    for (enum CSSettings settingID = 0; settingID < NUM_CS_OPTS; settingID++) {
        struct CSSettingsEntry* setting = &gCSSettings[settingID];

        if ((setting->val != setting->defaultVal) && !crash_screen_setting_is_header(settingID)) {
            return TRUE;
        }
    }

    return FALSE;
}

_Bool crash_screen_settings_check_for_header_state(_Bool expand) {
    for (enum CSSettings settingID = 0; settingID < ARRAY_COUNT(gCSSettings); settingID++) {
        if (crash_screen_setting_is_header(settingID) && gCSSettings[settingID].val == expand) {
            return TRUE;
        }
    }

    return FALSE;
}

void crash_screen_settings_set_all_headers(_Bool expand) {
    for (enum CSSettings settingID = 0; settingID < ARRAY_COUNT(gCSSettings); settingID++) {
        if (crash_screen_setting_is_header(settingID)) {
            gCSSettings[settingID].val = expand;
        }
    }
}
