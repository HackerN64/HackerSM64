#include <ultra64.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/util/map_parser.h"
#include "crash_screen/util/memory_read.h"
#include "crash_screen/util/registers.h"
#include "crash_screen/crash_controls.h"
#include "crash_screen/crash_draw.h"
#include "crash_screen/crash_descriptions.h"
#include "crash_screen/crash_main.h"
#include "crash_screen/crash_pages.h"
#include "crash_screen/crash_print.h"
#include "crash_screen/crash_settings.h"

#include "popup_register.h"


RegisterId sInspectedRegister = {
    // Default to R0.
    .cop = CPU,
    .idx = REG_CPU_R0,
    .flt = FALSE,
    .out = FALSE,
};


// Register popup box draw function.
void cs_popup_register_draw(void) {
    const s32 bgStartX = (CRASH_SCREEN_X1 + (TEXT_WIDTH(1) / 2));
    const s32 bgStartY = (CRASH_SCREEN_Y1 + (TEXT_HEIGHT(1) / 2));
    const s32 bgW = (CRASH_SCREEN_W - TEXT_WIDTH(1));
    const s32 bgH = (CRASH_SCREEN_H - TEXT_HEIGHT(1));
    cs_draw_dark_rect(
        bgStartX, bgStartY,
        bgW, bgH,
        cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_POPUP_OPACITY)
    );

    enum Coprocessors cop = sInspectedRegister.cop;
    int idx = sInspectedRegister.idx;

    const RegisterInfo* regInfo = get_reg_info(cop, idx);
    uint64_t threadValue = get_reg_val(cop, idx);
    uint64_t systemValue = get_direct_reg_val(cop, idx);
    _Bool is64Bit = (regInfo->size == sizeof(uint64_t));

    u32 line = 1;
    const RGBA32 dataColor = COLOR_RGBA32_VERY_LIGHT_GRAY;
    const RGBA32 descColor = COLOR_RGBA32_CRASH_PAGE_NAME;
    // "[register] REGISTER".
    cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"register:", COLOR_RGBA32_CRASH_PAGE_NAME);
    cs_print(TEXT_X(2), TEXT_Y(line++), STR_COLOR_PREFIX"$%s (%d bits)", COLOR_RGBA32_CRASH_VARIABLE, regInfo->name, (is64Bit ? 64 : 32));
    line++;
    cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"thread value:", descColor);
    if (is64Bit) {
        cs_print(TEXT_X(2), TEXT_Y(line++), (STR_COLOR_PREFIX STR_HEX_LONG), dataColor, threadValue);
    } else {
        cs_print(TEXT_X(2), TEXT_Y(line++), (STR_COLOR_PREFIX STR_HEX_WORD), dataColor, (uint32_t)threadValue);
    }
#ifdef INCLUDE_DEBUG_MAP
    cs_print_addr_location_info(TEXT_X(2), TEXT_Y(line++), (CRASH_SCREEN_NUM_CHARS_X - 4), (uintptr_t)threadValue, FALSE);
    // cs_print_symbol_name(TEXT_X(2), TEXT_Y(line++), (CRASH_SCREEN_NUM_CHARS_X - 4),
    //     get_map_symbol((uint32_t)threadValue, SYMBOL_SEARCH_BACKWARD)
    // );
#endif // INCLUDE_DEBUG_MAP
    
    cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"system value:", descColor);
    if (is64Bit) {
        cs_print(TEXT_X(2), TEXT_Y(line++), (STR_COLOR_PREFIX STR_HEX_LONG), dataColor, systemValue);
    } else {
        cs_print(TEXT_X(2), TEXT_Y(line++), (STR_COLOR_PREFIX STR_HEX_WORD), dataColor, (uint32_t)systemValue);
    }
#ifdef INCLUDE_DEBUG_MAP
    cs_print_addr_location_info(TEXT_X(2), TEXT_Y(line++), (CRASH_SCREEN_NUM_CHARS_X - 4), (uintptr_t)systemValue, FALSE);
    // cs_print_symbol_name(TEXT_X(2), TEXT_Y(line++), (CRASH_SCREEN_NUM_CHARS_X - 4),
    //     get_map_symbol((uint32_t)systemValue, SYMBOL_SEARCH_BACKWARD)
    // );
#endif // INCLUDE_DEBUG_MAP
    

    //! TODO: PC and FPCSR
    //! TODO: Float registers
    //! TODO: Specific register bits/flags
    //! TODO: Print symbol name if address

    cs_draw_outline(bgStartX, bgStartY, bgW, bgH, COLOR_RGBA32_CRASH_DIVIDER);

    osWritebackDCacheAll();
}

void cs_popup_register_input(void) {
    //! TODO: If register is address, goto address select. (return to this if cancelled).
    u16 buttonPressed = gCSCompositeController->buttonPressed;

    if (buttonPressed & (A_BUTTON | B_BUTTON | START_BUTTON)) {
        // Close the popup without jumping.
        cs_open_popup(CS_POPUP_NONE);
    }
}

// Open the register inspect popup box.
void cs_open_inspect_register(RegisterId regId) {
    cs_open_popup(CS_POPUP_REGISTER);
    sInspectedRegister = regId;
}

struct CSPopup gCSPopup_register = {
    .name      = "REGISTER",
    .initFunc  = NULL,
    .drawFunc  = cs_popup_register_draw,
    .inputFunc = cs_popup_register_input,
    .flags = {
        .allowPage = FALSE,
    },
};
