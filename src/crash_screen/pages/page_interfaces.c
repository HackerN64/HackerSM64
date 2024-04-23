#include <ultra64.h>

#include <stdarg.h>
#include <string.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/util/memory_read.h"
#include "crash_screen/util/registers.h"
#include "crash_screen/popups/popup_reginspect.h"
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


enum Interfaces sSelectedInterfaceIndex = 0;
u8 sSelectedRegisterIndex = 0;
u8 sNumShownRegisters = 0;

void page_interfaces_init(void) {
    sSelectedInterfaceIndex = 0;
    sSelectedRegisterIndex = 0;
    sNumShownRegisters = 0;
}

void draw_interface_regs(CSTextCoord_u32 line) {
    const RegisterSource* src = get_interface_src(sSelectedInterfaceIndex);
    const RegisterInfo* reg = &src->infoList[0];
    u8 regId = 0;

    for (regId = 0; regId < src->numRegs; regId++) {
        if (regId == sSelectedRegisterIndex) {
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
    }

    sNumShownRegisters = regId;
}

void page_interfaces_draw(void) {
    CSTextCoord_u32 line = 1;

    cs_draw_triangle((TEXT_X(0                       ) - 2), (TEXT_Y(line) - 1), 4, 8, COLOR_RGBA32_CRASH_SELECT_ARROW, CS_TRI_LEFT);
    cs_draw_triangle((TEXT_X(CRASH_SCREEN_NUM_CHARS_X) - 3), (TEXT_Y(line) - 1), 4, 8, COLOR_RGBA32_CRASH_SELECT_ARROW, CS_TRI_RIGHT);

    ScreenCoord_u32 x = TEXT_X(0) + 4;
    ScreenCoord_u32 y = TEXT_Y(line);
    cs_draw_rect(x, (y - 1), 1, TEXT_HEIGHT(1), COLOR_RGBA32_CRASH_DIVIDER);
    x += 2;
    for (int interfaceID = 0; interfaceID < NUM_INTERFACES; interfaceID++) {
        const RegisterSource* interface = get_interface_src(interfaceID);
        if (interfaceID == sSelectedInterfaceIndex) {
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
    const RegisterSource* selectedInterface = get_interface_src(sSelectedInterfaceIndex);
    cs_print(TEXT_X(0), TEXT_Y(line++), STR_COLOR_PREFIX"%s: %s", COLOR_RGBA32_LIGHT_CYAN, selectedInterface->name, selectedInterface->desc);

    draw_interface_regs(line);

    cs_draw_divider(DIVIDER_Y(3));
}

void page_interfaces_input(void) {
    s32 change = 0;
    if (gCSDirectionFlags.pressed.left ) change = -1; // Scroll left.
    if (gCSDirectionFlags.pressed.right) change = +1; // Scroll right.
    sSelectedInterfaceIndex = WRAP(((s32)sSelectedInterfaceIndex + change), 0, (s32)(NUM_INTERFACES - 1));

    _Bool switched = (change != 0);
    const RegisterSource* src = get_interface_src(sSelectedInterfaceIndex);

    change = 0;
    if (gCSDirectionFlags.pressed.up  ) change = -1; // Scroll up.
    if (gCSDirectionFlags.pressed.down) change = +1; // Scroll down.
    s32 newIndex = (sSelectedRegisterIndex + change);
    s32 endIndex = (src->numRegs - 1);
    if (switched) {
        sSelectedRegisterIndex = CLAMP(newIndex, 0, endIndex);
    } else {
        sSelectedRegisterIndex = WRAP(newIndex, 0, endIndex);
    }

    _Bool reginspectOpen = (gCSPopupID == CS_POPUP_REGINSPECT);
    u16 buttonPressed = gCSCompositeController->buttonPressed;
    if (reginspectOpen || (buttonPressed & A_BUTTON)) {
        RegisterId regId = (RegisterId){
            .src = INTERFACE_TO_SRC(sSelectedInterfaceIndex),
            .idx = sSelectedRegisterIndex,
            .valInfo = {
                .type = REG_VAL_TYPE_BITS,
                .thr  = FALSE,
                .dbl  = FALSE,
                .out  = FALSE,
            },
        };
        cs_open_reginspect(regId);
    }
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
