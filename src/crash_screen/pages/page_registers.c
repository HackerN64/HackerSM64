#include <ultra64.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/util/map_parser.h"
#include "crash_screen/util/memory_read.h"
#include "crash_screen/util/registers.h"
#include "crash_screen/cs_controls.h"
#include "crash_screen/cs_draw.h"
#include "crash_screen/cs_descriptions.h"
#include "crash_screen/cs_main.h"
#include "crash_screen/cs_pages.h"
#include "crash_screen/cs_print.h"
#include "crash_screen/cs_settings.h"

#include "crash_screen/popups/popup_reginspect.h"
#include "crash_screen/popups/popup_threads.h"

#include "page_registers.h"

#ifdef UNF
#include "usb/usb.h"
#include "usb/debug.h"
#endif // UNF


struct CSSetting cs_settings_group_page_registers[] = {
    [CS_OPT_HEADER_PAGE_REGISTERS   ] = { .type = CS_OPT_TYPE_HEADER,  .name = "THREAD REGISTERS",               .valNames = &gValNames_bool,          .val = SECTION_EXPANDED_DEFAULT,  .defaultVal = SECTION_EXPANDED_DEFAULT,  .lowerBound = FALSE,                 .upperBound = TRUE,                       },
#ifdef INCLUDE_DEBUG_MAP
    [CS_OPT_REGISTERS_PARSE_REG     ] = { .type = CS_OPT_TYPE_SETTING, .name = "Parse register addr names",      .valNames = &gValNames_bool,          .val = FALSE,                     .defaultVal = FALSE,                     .lowerBound = FALSE,                 .upperBound = TRUE,                       },
#endif // INCLUDE_DEBUG_MAP
    [CS_OPT_END_REGISTERS           ] = { .type = CS_OPT_TYPE_END, },
};


const enum ControlTypes cs_cont_list_registers[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_PAGE_SELECT,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_HIDE_CRASH_SCREEN,
#ifdef UNF
    CONT_DESC_OS_PRINT,
#endif // UNF
    CONT_DESC_CYCLE_FLOATS_MODE,
    CONT_DESC_LIST_END,
};


#define REG_LIST_TERMINATOR (u32)-1
#define LIST_REG(_cop, _idx) {  \
    .cop           = _cop,      \
    .idx           = _idx,      \
    .valInfo.raw   = 0,         \
}
#define LIST_REG_END() { .raw = REG_LIST_TERMINATOR, }
static const RegisterId sRegList[] = {
    LIST_REG(COP0, REG_CP0_EPC), LIST_REG(COP0, REG_CP0_SR ), LIST_REG(COP0, REG_CP0_CAUSE   ),
    LIST_REG(CPU,  REG_CPU_AT ), LIST_REG(CPU,  REG_CPU_V0 ), LIST_REG(CPU,  REG_CPU_V1      ),
    LIST_REG(CPU,  REG_CPU_A0 ), LIST_REG(CPU,  REG_CPU_A1 ), LIST_REG(CPU,  REG_CPU_A2      ),
    LIST_REG(CPU,  REG_CPU_A3 ), LIST_REG(CPU,  REG_CPU_T0 ), LIST_REG(CPU,  REG_CPU_T1      ),
    LIST_REG(CPU,  REG_CPU_T2 ), LIST_REG(CPU,  REG_CPU_T3 ), LIST_REG(CPU,  REG_CPU_T4      ),
    LIST_REG(CPU,  REG_CPU_T5 ), LIST_REG(CPU,  REG_CPU_T6 ), LIST_REG(CPU,  REG_CPU_T7      ),
    LIST_REG(CPU,  REG_CPU_S0 ), LIST_REG(CPU,  REG_CPU_S1 ), LIST_REG(CPU,  REG_CPU_S2      ),
    LIST_REG(CPU,  REG_CPU_S3 ), LIST_REG(CPU,  REG_CPU_S4 ), LIST_REG(CPU,  REG_CPU_S5      ),
    LIST_REG(CPU,  REG_CPU_S6 ), LIST_REG(CPU,  REG_CPU_S7 ), LIST_REG(CPU,  REG_CPU_T8      ),
    LIST_REG(CPU,  REG_CPU_T9 ), LIST_REG(CPU,  REG_CPU_GP ), LIST_REG(CPU,  REG_CPU_SP      ),
    LIST_REG(CPU,  REG_CPU_FP ), LIST_REG(CPU,  REG_CPU_RA ), LIST_REG(COP0, REG_CP0_BADVADDR),
    LIST_REG(SPC,  REG_SPC_HI ), LIST_REG(SPC,  REG_SPC_LO ), LIST_REG(SPC,  REG_SPC_RCP     ),
    LIST_REG_END(),
};

// Reg list:
#define REG_LIST_COLUMNS 3
#define REG_LIST_ROWS    DIV_CEIL((ARRAY_COUNT(sRegList) - 1), REG_LIST_COLUMNS)

// FP list:
#define FP_REG_SIZE     (sizeof(__OSfp) / sizeof(float))
#define FP_LIST_COLUMNS 3
#define FP_LIST_ROWS    DIV_CEIL((CP1_NUM_REGISTERS / FP_REG_SIZE), FP_LIST_COLUMNS)


enum RegisterPageSections {
    PAGE_REG_SECTION_THREAD,
    PAGE_REG_SECTION_REG,
    PAGE_REG_SECTION_FPCSR,
    PAGE_REG_SECTION_FP,
    NUM_REG_PAGE_SECTIONS,
};

typedef struct RegisterPageSelectionBounds {
    u8 cols;
    u8 rows;
} RegisterPageSelectionBounds;

RegisterPageSelectionBounds sRegPageSelectBounds[NUM_REG_PAGE_SECTIONS] = {
    [PAGE_REG_SECTION_THREAD] = { .cols =                1, .rows =             1, },
    [PAGE_REG_SECTION_REG   ] = { .cols = REG_LIST_COLUMNS, .rows = REG_LIST_ROWS, },
    [PAGE_REG_SECTION_FPCSR ] = { .cols =                1, .rows =             1, },
    [PAGE_REG_SECTION_FP    ] = { .cols =  FP_LIST_COLUMNS, .rows =  FP_LIST_ROWS, },
};

typedef struct RegisterPageSelection {
    u8 section;
    u8 pad[1];
    u8 selX;
    u8 selY;
} RegisterPageSelection;


RegisterPageSelection sRegisterSelectionCursor = {
    .section = PAGE_REG_SECTION_THREAD,
    .selX = 0,
    .selY = 0,
};


void page_registers_init(void) {

}

// Print a fixed-point register.
void cs_registers_print_reg(u32 x, u32 y, const char* name, Word val) {
    const MapSymbol* symbol = NULL;

    // "[register name]:"
    size_t charX = cs_print(x, y,
        (" "STR_COLOR_PREFIX"%s:"),
        COLOR_RGBA32_CRASH_VARIABLE, name
    );

#ifdef INCLUDE_DEBUG_MAP
    if (cs_get_setting_val(CS_OPT_GROUP_PAGE_REGISTERS, CS_OPT_REGISTERS_PARSE_REG)) {
        symbol = get_map_symbol(val, SYMBOL_SEARCH_BACKWARD);
    }
#endif // INCLUDE_DEBUG_MAP

    if (symbol != NULL) {
        // "[symbol name]"
        cs_print_symbol_name((x + TEXT_WIDTH(charX)), y, 10, symbol, FALSE);
    } else {
        // "[XXXXXXXX]"
        cs_print((x + TEXT_WIDTH(charX + STRLEN(" "))), y,
            STR_COLOR_PREFIX STR_HEX_WORD,
            COLOR_RGBA32_WHITE, val
        );
    }
}

// Print important fixed-point registers.
u32 cs_registers_print_registers(u32 line) {
    const u32 columns = REG_LIST_COLUMNS;
    const u32 rows = REG_LIST_ROWS;
    const size_t columnCharWidth = DIV_CEIL(CRASH_SCREEN_NUM_CHARS_X, columns);
    const RegisterId* reg = sRegList;
    RegisterPageSelection* sel = &sRegisterSelectionCursor;
    _Bool drawSel = (sel->section == PAGE_REG_SECTION_REG);
    _Bool listEnded = FALSE;

    for (u32 y = 0; y < rows; y++) {
        for (u32 x = 0; x < columns; x++) {
            if (reg->raw == REG_LIST_TERMINATOR) {
                listEnded = TRUE;
            }

            u32 charX = (x * columnCharWidth);
            u32 charY = (line + y);

            if (drawSel && (x == sel->selX) && (y == sel->selY)) {
                cs_draw_row_selection_box_impl(TEXT_X(charX), TEXT_Y(charY),
                    TEXT_WIDTH(STRLEN(" XX: 00000000 ")), TEXT_HEIGHT(1),
                    COLOR_RGBA32_CRASH_SELECT_HIGHLIGHT
                );
            }

            if (!listEnded) {
                const RegisterInfo* regInfo = get_reg_info(reg->cop, reg->idx);

                if (regInfo != NULL) {
                    cs_registers_print_reg(TEXT_X(charX), TEXT_Y(charY), regInfo->shortName, get_reg_val(reg->cop, reg->idx));
                }

                reg++;
            }
        }
    }

    return (line + rows);
}

void cs_print_fpcsr(u32 x, u32 y, u32 fpcsr) {
    if (sRegisterSelectionCursor.section == PAGE_REG_SECTION_FPCSR) {
        cs_draw_row_selection_box(y);
    }

    // "FPCSR:[XXXXXXXX]"
    size_t fpcsrSize = cs_print(x, y,
        STR_COLOR_PREFIX"FPCSR: "STR_COLOR_PREFIX STR_HEX_WORD" ",
        COLOR_RGBA32_CRASH_VARIABLE,
        COLOR_RGBA32_WHITE, fpcsr
    );
    x += TEXT_WIDTH(fpcsrSize);

    const char* fpcsrDesc = get_fpcsr_desc(fpcsr, FALSE);
    if (fpcsrDesc != NULL) {
        // "([float exception description])"
        cs_print(x, y, STR_COLOR_PREFIX"(%s)", COLOR_RGBA32_CRASH_DESCRIPTION, fpcsrDesc);
    }
}

// Print a floating-point register.
void cs_registers_print_float_reg(u32 x, u32 y, u32 regNum) {
    const RegisterInfo* regInfo = get_reg_info(COP1, regNum);

    if (regInfo == NULL) {
        return;
    }

    // "[register name]:"
    size_t charX = cs_print(x, y, STR_COLOR_PREFIX"%s:", COLOR_RGBA32_CRASH_VARIABLE, regInfo->name);
    x += TEXT_WIDTH(charX);

    u32 data = get_reg_val(COP1, regNum);

    cs_print_f32(x, y, (IEEE754_f32){ .asU32 = data, }, cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_FLOATS_FMT), FALSE);
}

void cs_registers_print_float_registers(u32 line, __OSThreadContext* tc) {
    const size_t columnCharWidth = DIV_CEIL(CRASH_SCREEN_NUM_CHARS_X, FP_LIST_COLUMNS);
    __OSfp* osfp = &tc->fp0; // The first float pointer.
    u32 regNum = 0;
    RegisterPageSelection* sel = &sRegisterSelectionCursor;
    _Bool drawSel = (sel->section == PAGE_REG_SECTION_FP);

    // cs_registers_print_fpcsr(TEXT_X(0), TEXT_Y(line), tc->fpcsr);
    cs_print_fpcsr(TEXT_X(0), TEXT_Y(line++), tc->fpcsr);

    osWritebackDCacheAll();

    for (u32 y = 0; y < FP_LIST_ROWS; y++) {
        for (u32 x = 0; x < FP_LIST_COLUMNS; x++) {
            u32 charX = (x * columnCharWidth);
            u32 charY = (line + y);
            
            if (drawSel && (x == sel->selX) && (y == sel->selY)) {
                cs_draw_row_selection_box_impl(TEXT_X(charX), TEXT_Y(charY),
                    TEXT_WIDTH(STRLEN("FXX: 00000000 ")), TEXT_HEIGHT(1),
                    COLOR_RGBA32_CRASH_SELECT_HIGHLIGHT
                );
            }

            if (regNum < CP1_NUM_REGISTERS) {
                cs_registers_print_float_reg(TEXT_X(charX), TEXT_Y(charY), regNum);

                osfp++;
                regNum += FP_REG_SIZE;
            }
        }
    }
}

void page_registers_draw(void) {
    OSThread* thread = gInspectThread;
    __OSThreadContext* tc = &thread->context;
    u32 line = 2;

    if (gCSPopupID != CS_POPUP_THREADS) {
        // Draw the thread box:
        // cs_thread_draw_highlight(thread, CS_POPUP_THREADS_Y1);
        if (sRegisterSelectionCursor.section == PAGE_REG_SECTION_THREAD) {
            cs_draw_row_box_thread(CS_POPUP_THREADS_BG_X1, CS_POPUP_THREADS_Y1, COLOR_RGBA32_CRASH_SELECT_HIGHLIGHT);
        }
        cs_print_thread_info(TEXT_X(CS_POPUP_THREADS_TEXT_X1), CS_POPUP_THREADS_Y1, CS_POPUP_THREADS_NUM_CHARS_X, thread);
    }
    line++;

    line = cs_registers_print_registers(line);

    osWritebackDCacheAll();

    cs_registers_print_float_registers(line, tc);
}

static int cs_page_reg_get_select_idx(RegisterPageSelection* sel, int columns) {
    return ((sel->selY * columns) + sel->selX);
}

void page_registers_input(void) {
    RegisterPageSelection* sel = &sRegisterSelectionCursor;
    CrashScreenDirections* dir = &gCSDirectionFlags;
    _Bool up    = dir->pressed.up;
    _Bool down  = dir->pressed.down;
    _Bool left  = dir->pressed.left;
    _Bool right = dir->pressed.right;

    RegisterPageSelectionBounds* bounds = &sRegPageSelectBounds[sel->section];

    if (up) {
        if (sel->selY > 0) {
            sel->selY--;
        } else if (sel->section > 0) {
            // Go to bottom of previous section.
            sel->section--;
            sel->selY = sRegPageSelectBounds[sel->section].rows - 1;
        } else {
            // Wrap vertically.
            sel->section = (NUM_REG_PAGE_SECTIONS - 1);
            sel->selY = sRegPageSelectBounds[sel->section].rows - 1;
        }
    }
    if (down) {
        if (sel->selY < (bounds->rows - 1)) {
            sel->selY++;
        } else if (sel->section < (NUM_REG_PAGE_SECTIONS - 1)) {
            // Go to top of next section;
            sel->section++;
            sel->selY = 0;
        } else {
            // Wrap vertically.
            sel->section = 0;
            sel->selY = 0;
        }
    }
    if (bounds->cols > 1) {
        if (left) {
            if (sel->selX > 0) {
                sel->selX--;
            } else {
                // Wrap horizontally.
                sel->selX = (bounds->cols - 1);
            }
        }
        if (right) {
            if (sel->selX < (bounds->cols - 1)) {
                sel->selX++;
            } else {
                // Wrap horizontally.
                sel->selX = 0;
            }
        }
    }

    bounds = &sRegPageSelectBounds[sel->section];

    u16 buttonPressed = gCSCompositeController->buttonPressed;
    if (buttonPressed & B_BUTTON) {
        // Cycle floats print mode.
        cs_inc_setting(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_FLOATS_FMT, 1);
    }
    if (buttonPressed & A_BUTTON) {
        u32 idx = 0;
        switch (sel->section) {
            case PAGE_REG_SECTION_THREAD:
                cs_open_threads();
                break;
            case PAGE_REG_SECTION_REG:
                idx = cs_page_reg_get_select_idx(sel, bounds->cols);
                if (idx < (ARRAY_COUNT(sRegList) - 1)) {
                    cs_open_reginspect(sRegList[idx]);
                }
                break;
            case PAGE_REG_SECTION_FPCSR:;
                RegisterId regId = {
                    .cop = FCR,
                    .idx = REG_FCR_CONTROL_STATUS,
                    .valInfo = {
                        .type = REG_VAL_TYPE_INT,
                        .dbl  = FALSE,
                        .out  = FALSE,
                    },
                };
                cs_open_reginspect(regId);
                break;
            case PAGE_REG_SECTION_FP:
                idx = (cs_page_reg_get_select_idx(sel, bounds->cols) * FP_REG_SIZE);
                if (idx < (CP1_NUM_REGISTERS - 1)) {
                    RegisterId regId = {
                        .cop = COP1,
                        .idx = idx,
                        .valInfo = {
                            .type = REG_VAL_TYPE_FLOAT,
                            .dbl  = FALSE, //! TODO:
                            .out  = FALSE,
                        },
                    };
                    cs_open_reginspect(regId);
                }
                break;
        }
    }
}

void page_registers_print(void) {
#ifdef UNF
    osSyncPrintf("\n");

    OSThread* thread = gInspectThread;
    __OSThreadContext* tc = &thread->context;

    osSyncPrintf("- REGISTERS IN:\n");
    // THREAD:
    osSyncPrintf("- THREAD:\t%d", osGetThreadId(thread));
    const char* threadName = get_thread_name(thread);

    if (threadName != NULL) {
        // "(thread name)"
        osSyncPrintf(" (%s)", threadName);
    }
    osSyncPrintf("\n");

    // Thread registers:
    const u32 columns = REG_LIST_COLUMNS;
    const u32 rows = REG_LIST_ROWS;
    const RegisterId* reg = sRegList;
    for (u32 y = 0; y < rows; y++) {
        osSyncPrintf("- ");
        for (u32 x = 0; x < columns; x++) {
            if (reg->raw == REG_LIST_TERMINATOR) {
                break;
            }

            const RegisterInfo* regInfo = get_reg_info(reg->cop, reg->idx);

            if (regInfo != NULL) {
                osSyncPrintf("%s "STR_HEX_PREFIX STR_HEX_LONG" ", regInfo->shortName, get_reg_val(reg->cop, reg->idx));
            }

            reg++;
        }
        osSyncPrintf("\n");
    }

    // Float registers:
    const u32 f_columns = 2;
    const u32 f_rows = DIV_CEIL((CP1_NUM_REGISTERS / FP_REG_SIZE), columns);
    __OSfp* osfp = &tc->fp0;
    u32 regNum = 0;
    for (u32 i = 0; i < f_rows; i++) {
        osSyncPrintf("- ");
        for (u32 j = 0; j < f_columns; j++) {
            if (regNum >= CP1_NUM_REGISTERS) {
                break;
            }

            osSyncPrintf("d%02d "STR_HEX_DECIMAL"\t", regNum, get_reg_val(COP1, regNum));

            osfp++;
            regNum += FP_REG_SIZE;
        }
        osSyncPrintf("\n");
    }
#endif // UNF
}

struct CSPage gCSPage_registers ={
    .name         = "THREAD REGISTERS",
    .initFunc     = page_registers_init,
    .drawFunc     = page_registers_draw,
    .inputFunc    = page_registers_input,
    .printFunc    = page_registers_print,
    .contList     = cs_cont_list_registers,
    .settingsList = cs_settings_group_page_registers,
    .flags = {
        .initialized = FALSE,
        .crashed     = FALSE,
    },
};
