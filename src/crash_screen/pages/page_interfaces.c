#include <ultra64.h>

#include <stdarg.h>
#include <string.h>

#include "types.h"
#include "sm64.h"

// #include "crash_screen/util/map_parser.h"
#include "crash_screen/util/interface.h"
#include "crash_screen/util/memory_read.h"
#include "crash_screen/cs_controls.h"
#include "crash_screen/cs_descriptions.h"
#include "crash_screen/cs_draw.h"
#include "crash_screen/cs_main.h"
#include "crash_screen/cs_pages.h"
#include "crash_screen/cs_print.h"
#include "crash_screen/cs_settings.h"

#include "page_interfaces.h"


struct CSSetting cs_settings_group_page_interfaces[] = {
    [CS_OPT_HEADER_PAGE_INTERFACES   ] = { .type = CS_OPT_TYPE_HEADER,  .name = "INTERFACES",                     .valNames = &gValNames_bool,          .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    //! TODO:
    [CS_OPT_INTERFACES_SHOW_ADDRESSES] = { .type = CS_OPT_TYPE_SETTING, .name = "Show register addresses",        .valNames = &gValNames_bool,          .val = TRUE,                      .defaultVal = TRUE,                      .lowerBound = FALSE,                 .upperBound = TRUE,                       },
    [CS_OPT_END_INTERFACES           ] = { .type = CS_OPT_TYPE_END, },
};


const enum ControlTypes cs_cont_list_interfaces[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_PAGE_SELECT,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_HIDE_CRASH_SCREEN,
#ifdef UNF
    CONT_DESC_OS_PRINT,
#endif // UNF
    CONT_DESC_SCROLL_LIST,
    // CONT_DESC_JUMP_TO_ADDRESS,
    CONT_DESC_LIST_END,
};


enum Interfaces sSelectedInterface = 0;
u8 sSelectedRegister = 0;
u8 sNumShownRegisters = 0;

void page_interfaces_init(void) {
    sSelectedInterface = 0;
    sSelectedRegister = 0;
    sNumShownRegisters = 0;
}

void draw_interface_regs(CSTextCoord_u32 line) {
    InterfaceReg* reg = &gInterfaceInfos[sSelectedInterface].list[0];
    u32 regId = 0;

    while (reg->name != NULL) {
        if (regId == sSelectedRegister) {
            cs_draw_row_selection_box(TEXT_Y(line));
        }
        CSTextCoord_u32 charX = 0;
        charX += cs_print(TEXT_X(charX), TEXT_Y(line), STR_HEX_WORD":", reg->addr);
        CSTextCoord_u32 regNameEnd = (CRASH_SCREEN_NUM_CHARS_X - STRLEN(":********"));

        cs_print_scroll(TEXT_X(charX), TEXT_Y(line), (regNameEnd - charX), STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_INTERFACE_REG, reg->name);
        Word data = 0x00000000;
        if (try_read_word_aligned(&data, reg->addr)) {
            cs_print(TEXT_X(regNameEnd), TEXT_Y(line), ":"STR_HEX_WORD, data);
        } else {
            cs_print(TEXT_X(regNameEnd), TEXT_Y(line), ":"STR_COLOR_PREFIX"********", COLOR_RGBA32_CRASH_OUT_OF_BOUNDS);
        }

        line++;
        reg++;
        regId++;
    }

    sNumShownRegisters = regId;
}

void page_interfaces_draw(void) {
    InterfaceInfo* selectedInterface = &gInterfaceInfos[sSelectedInterface];

    CSTextCoord_u32 line = 1;

    cs_draw_triangle((TEXT_X(0                       ) - 2), (TEXT_Y(line) - 1), 4, 8, COLOR_RGBA32_CRASH_SELECT_ARROW, CS_TRI_LEFT);
    cs_draw_triangle((TEXT_X(CRASH_SCREEN_NUM_CHARS_X) - 3), (TEXT_Y(line) - 1), 4, 8, COLOR_RGBA32_CRASH_SELECT_ARROW, CS_TRI_RIGHT);

    ScreenCoord_u32 x = TEXT_X(0) + 4;
    ScreenCoord_u32 y = TEXT_Y(line);
    cs_draw_rect(x, (y - 1), 1, TEXT_HEIGHT(1), COLOR_RGBA32_CRASH_DIVIDER);
    x += 2;
    for (int interfaceId = 0; interfaceId < NUM_INTERFACES; interfaceId++) {
        InterfaceInfo* interface = &gInterfaceInfos[interfaceId];
        if (interfaceId == sSelectedInterface) {
            cs_draw_row_selection_box_impl(x, (y + 1),
                TEXT_WIDTH(strlen(interface->name)), (TEXT_HEIGHT(1) - 1),
                COLOR_RGBA32_CRASH_SELECT_HIGHLIGHT
            );
        }
        CSTextCoord_u32 charX = cs_print(x, y, STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_INTERFACE, interface->name);
        x += TEXT_WIDTH(charX);
        cs_draw_rect(x, (y - 1), 1, TEXT_HEIGHT(1), COLOR_RGBA32_CRASH_DIVIDER);
        x += 2;
    }
    line++;

    cs_draw_divider(DIVIDER_Y(line));
    cs_print(TEXT_X(0), TEXT_Y(line++), STR_COLOR_PREFIX"%s: %s", COLOR_RGBA32_LIGHT_CYAN, selectedInterface->name, selectedInterface->desc);

    draw_interface_regs(line);

    cs_draw_divider(DIVIDER_Y(3));
}

void page_interfaces_input(void) {
    s32 change = 0;
    if (gCSDirectionFlags.pressed.up  ) change = -1; // Scroll up.
    if (gCSDirectionFlags.pressed.down) change = +1; // Scroll down.
    sSelectedRegister = WRAP(((s32)sSelectedRegister + change), 0, (s32)(sNumShownRegisters - 1));

    change = 0;
    if (gCSDirectionFlags.pressed.left ) change = -1; // Scroll left.
    if (gCSDirectionFlags.pressed.right) change = +1; // Scroll right.
    sSelectedInterface = WRAP(((s32)sSelectedInterface + change), 0, (s32)(NUM_INTERFACES - 1));

}

void page_interfaces_print(void) {

}


struct CSPage gCSPage_interfaces = {
    .name         = "INTERFACE REGISTERS",
    .initFunc     = page_interfaces_init,
    .drawFunc     = page_interfaces_draw,
    .inputFunc    = page_interfaces_input,
    .printFunc    = page_interfaces_print,
    .contList     = cs_cont_list_interfaces,
    .settingsList = cs_settings_group_page_interfaces,
    .flags = {
        .initialized = FALSE,
        .crashed     = FALSE,
    },
};
