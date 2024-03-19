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

void cs_popup_register_draw_reg_value(u32 x, u32 y, RegisterId regId, uint64_t val64) {
    const RegisterInfo* regInfo = get_reg_info(regId.cop, regId.idx);
    _Bool is64Bit = (regInfo->size == sizeof(uint64_t));
    uint32_t val32 = (uint32_t)val64;
    enum FloatErrorType fltErrType = FLT_ERR_NONE;

    //! TODO: simplify this:
    if (regId.flt) {
        if (is64Bit) {
            IEEE754_f64 flt64 = {
                .asU64 = val64,
            };
            fltErrType = validate_f64(flt64);
            if (fltErrType != FLT_ERR_NONE) {
            } else {
                cs_print(x, y, (" "STR_HEX_PREFIX STR_HEX_LONG), val64);
                y += TEXT_HEIGHT(1);
                cs_print(x, y, "% g", flt64.asF64);
                y += TEXT_HEIGHT(1);
                cs_print(x, y, "% e", flt64.asF64);
            }
        } else {
            IEEE754_f32 flt32 = {
                .asU32 = val32,
            };
            fltErrType = validate_f32(flt32);
            if (fltErrType != FLT_ERR_NONE) {

            } else {
                cs_print(x, y, (" "STR_HEX_PREFIX STR_HEX_WORD), val32);
                y += TEXT_HEIGHT(1);
                cs_print(x, y, "% g", flt32.asF32);
                y += TEXT_HEIGHT(1);
                cs_print(x, y, "% e", flt32.asF32);
            }
        }
    } else {
        if (is64Bit) {
            cs_print(x, y, (STR_HEX_PREFIX STR_HEX_LONG), val64);
        } else {
            cs_print(x, y, (STR_HEX_PREFIX STR_HEX_WORD), val32);
        }
        y += TEXT_HEIGHT(1);
#ifdef INCLUDE_DEBUG_MAP
        cs_print_symbol_name(x, y, (CRASH_SCREEN_NUM_CHARS_X - 4),
            get_map_symbol(val32, SYMBOL_SEARCH_BACKWARD), FALSE
        );
        //! TODO: this is temporary, since direct register reads from asm put the low bits in the high bits of the result.
        if (is64Bit) {
            y += TEXT_HEIGHT(1);
            HiLo64 hiLoVal = {
                .raw = val64,
            };
            cs_print_symbol_name(x, y, (CRASH_SCREEN_NUM_CHARS_X - 4),
                get_map_symbol(hiLoVal.hi, SYMBOL_SEARCH_BACKWARD), FALSE
            );
        }
#endif // INCLUDE_DEBUG_MAP
    }
}

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

    RegisterId regId = sInspectedRegister;

    enum Coprocessors cop = regId.cop;
    int idx = regId.idx;

    const RegisterInfo* regInfo = get_reg_info(cop, idx);
    if (regInfo == NULL) {
        return;
    }
    _Bool is64Bit = (regInfo->size == sizeof(uint64_t));

    u32 line = 1;
    const RGBA32 descColor = COLOR_RGBA32_CRASH_PAGE_NAME;
    // "[register] REGISTER".
    cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"register:", COLOR_RGBA32_CRASH_PAGE_NAME);
    cs_print(TEXT_X(2), TEXT_Y(line++), STR_COLOR_PREFIX"$%s (%d bits)", COLOR_RGBA32_CRASH_VARIABLE, regInfo->name, (is64Bit ? 64 : 32));
    const char* regDesc = get_reg_desc(cop, idx);
    if (regDesc != NULL) {
        cs_print(TEXT_X(2), TEXT_Y(line++), STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_VARIABLE, regDesc);
    }
    const char* copName = get_coprocessor_name(cop);
    if (copName != NULL) {
        cs_print(TEXT_X(2), TEXT_Y(line++), STR_COLOR_PREFIX"in %s", COLOR_RGBA32_CRASH_VARIABLE, copName);
    }
    line++;

    cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"thread value:", descColor);
    uint64_t threadValue = get_reg_val(cop, idx);
    cs_popup_register_draw_reg_value(TEXT_X(2), TEXT_Y(line), regId, threadValue);
    line += 3;

    cs_print(TEXT_X(1), TEXT_Y(line++), STR_COLOR_PREFIX"system value:", descColor);
    uint64_t systemValue = get_direct_reg_val(cop, idx);
    cs_popup_register_draw_reg_value(TEXT_X(2), TEXT_Y(line), regId, systemValue);

    //! TODO: lo, hi, rcp, fpcsr
    //! TODO: Float registers
    //! TODO: Specific register bits/flags (cp0 and fpcsr)

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
