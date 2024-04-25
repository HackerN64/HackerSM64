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


// List of all registers in __OSThreadContext:
#define REG_LIST_TERMINATOR (u32)-1
#define LIST_REG_IMPL(_src, _idx, _type) {  \
    .src           = _src,                  \
    .idx           = _idx,                  \
    .valInfo.type  = _type,                 \
    .valInfo.thr   = TRUE,                  \
    .valInfo.dbl   = FALSE,                 \
    .valInfo.out   = FALSE,                 \
}
#define LIST_REGI(_src, _idx) LIST_REG_IMPL(_src, _idx, REG_VAL_TYPE_INT)
#define LIST_REGA(_src, _idx) LIST_REG_IMPL(_src, _idx, REG_VAL_TYPE_ADDR)
#define LIST_REGF(_src, _idx) LIST_REG_IMPL(_src, _idx, REG_VAL_TYPE_FLOAT)
#define LIST_REGB(_src, _idx) LIST_REG_IMPL(_src, _idx, REG_VAL_TYPE_BITS)
#define LIST_REG_END() { .raw = REG_LIST_TERMINATOR, }
static const RegisterId sThreadRegList[] = {
    LIST_REGA(REGS_CP0, REG_CP0_EPC), LIST_REGA(REGS_CPU, REG_CPU_RA   ), LIST_REGA(REGS_CP0, REG_CP0_BADVADDR),
    LIST_REGI(REGS_CPU, REG_CPU_AT ), LIST_REGI(REGS_CPU, REG_CPU_V0   ), LIST_REGI(REGS_CPU, REG_CPU_V1      ),
    LIST_REGI(REGS_CPU, REG_CPU_A0 ), LIST_REGI(REGS_CPU, REG_CPU_A1   ), LIST_REGI(REGS_CPU, REG_CPU_A2      ),
    LIST_REGI(REGS_CPU, REG_CPU_A3 ), LIST_REGI(REGS_CPU, REG_CPU_T0   ), LIST_REGI(REGS_CPU, REG_CPU_T1      ),
    LIST_REGI(REGS_CPU, REG_CPU_T2 ), LIST_REGI(REGS_CPU, REG_CPU_T3   ), LIST_REGI(REGS_CPU, REG_CPU_T4      ),
    LIST_REGI(REGS_CPU, REG_CPU_T5 ), LIST_REGI(REGS_CPU, REG_CPU_T6   ), LIST_REGI(REGS_CPU, REG_CPU_T7      ),
    LIST_REGI(REGS_CPU, REG_CPU_S0 ), LIST_REGI(REGS_CPU, REG_CPU_S1   ), LIST_REGI(REGS_CPU, REG_CPU_S2      ),
    LIST_REGI(REGS_CPU, REG_CPU_S3 ), LIST_REGI(REGS_CPU, REG_CPU_S4   ), LIST_REGI(REGS_CPU, REG_CPU_S5      ),
    LIST_REGI(REGS_CPU, REG_CPU_S6 ), LIST_REGI(REGS_CPU, REG_CPU_S7   ), LIST_REGI(REGS_CPU, REG_CPU_T8      ),
    LIST_REGI(REGS_CPU, REG_CPU_T9 ), LIST_REGA(REGS_CPU, REG_CPU_GP   ), LIST_REGA(REGS_CPU, REG_CPU_SP      ),
    LIST_REGI(REGS_CPU, REG_CPU_FP ), LIST_REGI(REGS_SPC, REG_SPC_HI   ), LIST_REGI(REGS_SPC, REG_SPC_LO      ),
    LIST_REGI(REGS_CP0, REG_CP0_SR ), LIST_REGI(REGS_CP0, REG_CP0_CAUSE), LIST_REGI(REGS_SPC, REG_SPC_RCP     ),
    LIST_REG_END(),
};
static const RegisterId sThreadFPCSRList[] = { //! TODO: Use this for printing.
    LIST_REGB(REGS_FCR, REG_FCR_CONTROL_STATUS),
    LIST_REG_END(),
};
static const RegisterId sThreadFloatRegList[] = { //! TODO: Use this for printing.
    LIST_REGF(REGS_CP1, REG_CP1_F00), LIST_REGF(REGS_CP1, REG_CP1_F02), LIST_REGF(REGS_CP1, REG_CP1_F04),
    LIST_REGF(REGS_CP1, REG_CP1_F06), LIST_REGF(REGS_CP1, REG_CP1_F08), LIST_REGF(REGS_CP1, REG_CP1_F10),
    LIST_REGF(REGS_CP1, REG_CP1_F12), LIST_REGF(REGS_CP1, REG_CP1_F14), LIST_REGF(REGS_CP1, REG_CP1_F16),
    LIST_REGF(REGS_CP1, REG_CP1_F18), LIST_REGF(REGS_CP1, REG_CP1_F20), LIST_REGF(REGS_CP1, REG_CP1_F22),
    LIST_REGF(REGS_CP1, REG_CP1_F24), LIST_REGF(REGS_CP1, REG_CP1_F26), LIST_REGF(REGS_CP1, REG_CP1_F28),
    LIST_REGF(REGS_CP1, REG_CP1_F30),
    LIST_REG_END(),
};

//! TODO: autogenerate number of rows from columns and list

// Reg list:
#define REG_LIST_COLUMNS 3
#define REG_LIST_ROWS    DIV_CEIL((ARRAY_COUNT(sThreadRegList) - 1), REG_LIST_COLUMNS)

// FP list:
#define FP_REG_SIZE     (sizeof(__OSfp) / sizeof(float)) //! TODO: Move this elsewhere (no longer needed here after UNF is fixed)
#define FP_LIST_COLUMNS 3
#define FP_LIST_ROWS    DIV_CEIL((ARRAY_COUNT(sThreadFloatRegList) - 1), FP_LIST_COLUMNS)


enum RegisterPageSections {
    PAGE_REG_SECTION_THREAD,
    PAGE_REG_SECTION_REG,
    PAGE_REG_SECTION_FPCSR,
    PAGE_REG_SECTION_FP,
    NUM_REG_PAGE_SECTIONS,
};

typedef struct RegisterPageSection {
    /*0x00*/ const RegisterId* list; // Register list.
    /*0x04*/ const u8 size;          // (ARRAY_COUNT(list) - 1).
    /*0x05*/ const _Bool clamp;      // How to handle out of bounds accesses.
    /*0x06*/ const u8 cols;          // Number of columns.
    /*0x07*/ const u8 rows;          // Number of rows.
} RegisterPageSection; /*0x08*/
#define REG_LIST_BOUNDS(_rows, _cols, _list, _clamp) {  \
    .rows  = _rows,                                     \
    .cols  = _cols,                                     \
    .list  = _list,                                     \
    .size  = (ARRAY_COUNT(_list) - 1),                  \
    .clamp = _clamp,                                    \
}
const RegisterPageSection sRegPageSections[NUM_REG_PAGE_SECTIONS] = {
    [PAGE_REG_SECTION_THREAD] = { .rows = 1, .cols = 1, .list = NULL, .size = 0, .clamp = TRUE, },
    [PAGE_REG_SECTION_REG   ] = REG_LIST_BOUNDS(REG_LIST_ROWS, REG_LIST_COLUMNS, sThreadRegList,      FALSE),
    [PAGE_REG_SECTION_FPCSR ] = REG_LIST_BOUNDS(1,             1,                sThreadFPCSRList,    TRUE ),
    [PAGE_REG_SECTION_FP    ] = REG_LIST_BOUNDS(FP_LIST_ROWS,  FP_LIST_COLUMNS,  sThreadFloatRegList, FALSE),
};

typedef struct RegisterPageCursor {
    /*0x00*/ u8 sectionID;
    /*0x01*/ u8 pad[1];
    /*0x02*/ u8 selX;
    /*0x03*/ u8 selY;
} RegisterPageCursor; /*0x04*/
RegisterPageCursor sRegisterSelectionCursor = {
    .sectionID = PAGE_REG_SECTION_THREAD,
    .selX = 0,
    .selY = 0,
};


void page_registers_init(void) {

}

// Print a fixed-point register.
void cs_registers_print_reg(ScreenCoord_u32 x, ScreenCoord_u32 y, const char* name, Word val) {
    const MapSymbol* symbol = NULL;

    // "[register name]:"
    CSTextCoord_u32 charX = cs_print(x, y,
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

// Print the floating-point status/control register.
void cs_registers_print_bitfield(ScreenCoord_u32 x, ScreenCoord_u32 y, const char* name, Word val, _Bool isFPCSR) {
    // "[name]: [XXXXXXXX]"
    CSTextCoord_u32 regSize = cs_print(x, y,
        STR_COLOR_PREFIX"%s: "STR_COLOR_PREFIX STR_HEX_WORD" ",
        COLOR_RGBA32_CRASH_VARIABLE, name,
        COLOR_RGBA32_WHITE, val
    );
    x += TEXT_WIDTH(regSize);

    if (isFPCSR) {
        const char* fpcsrDesc = get_fpcsr_desc(val, FALSE);
        if (fpcsrDesc != NULL) {
            // "([float exception description])"
            cs_print(x, y, STR_COLOR_PREFIX"(%s)", COLOR_RGBA32_CRASH_DESCRIPTION, fpcsrDesc);
        }
    }
}

// Print a floating-point register.
void cs_registers_print_float_reg(ScreenCoord_u32 x, ScreenCoord_u32 y, const char* name, Word val) {
    // "[register name]:"
    CSTextCoord_u32 charX = cs_print(x, y, STR_COLOR_PREFIX"F%s:", COLOR_RGBA32_CRASH_VARIABLE, name);
    x += TEXT_WIDTH(charX);

    cs_print_f32(x, y, (IEEE754_f32){ .asU32 = val, }, cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_FLOATS_FMT), FALSE);
}

// Print important fixed-point registers.
CSTextCoord_u32 cs_registers_draw_register_list(CSTextCoord_u32 line, enum RegisterPageSections sectionID) {
    const RegisterPageSection* section = &sRegPageSections[sectionID];
    const u32 cols = section->cols;
    const u32 rows = section->rows;

    const CSTextCoord_u32 columnCharWidth = DIV_CEIL(CRASH_SCREEN_NUM_CHARS_X, cols);
    const RegisterId* reg = &section->list[0];
    RegisterPageCursor* cursor = &sRegisterSelectionCursor;
    _Bool drawSel = (cursor->sectionID == sectionID);
    _Bool listEnded = FALSE;

    for (u32 row = 0; row < rows; row++) {
        for (u32 col = 0; col < cols; col++) {
            if (reg->raw == REG_LIST_TERMINATOR) {
                // Don't break out of the loop right away,
                // so the cursor can be visible on empty entries.
                listEnded = TRUE;
            }

            CSTextCoord_u32 charX = (col * columnCharWidth);
            CSTextCoord_u32 charY = (line + row);

            _Bool fullRow = (cols == 1);

            if (drawSel && (fullRow || (col == cursor->selX)) && (row == cursor->selY)) {
                cs_draw_row_selection_box_impl(TEXT_X(charX), TEXT_Y(charY),
                    (TEXT_WIDTH(columnCharWidth - 1)), TEXT_HEIGHT(1),
                    COLOR_RGBA32_CRASH_SELECT_HIGHLIGHT
                );
            }

            if (!listEnded) {
                const RegisterInfo* regInfo = get_reg_info(reg->src, reg->idx);

                if (regInfo != NULL) {
                    ScreenCoord_u32 x = TEXT_X(charX);
                    ScreenCoord_u32 y = TEXT_Y(charY);
                    const char* name = (fullRow ? regInfo->name : regInfo->shortName);
                    Word value = get_reg_val(reg->src, reg->idx, reg->valInfo.thr);

                    switch (reg->valInfo.type) {
                        case REG_VAL_TYPE_INT:
                        case REG_VAL_TYPE_ADDR:
                            cs_registers_print_reg(x, y, name, value);
                            break;
                        case REG_VAL_TYPE_FLOAT:
                            cs_registers_print_float_reg(x, y, name, value);
                            break;
                        case REG_VAL_TYPE_BITS:
                            cs_registers_print_bitfield(x, y, name, value, ((reg->src == REGS_FCR) && (reg->idx == REG_FCR_CONTROL_STATUS)));
                            break;
                    }
                }

                reg++;
            }
        }
    }

    osWritebackDCacheAll();

    return (line + rows);
}

void page_registers_draw(void) {
    OSThread* thread = gInspectThread;
    CSTextCoord_u32 line = 2;
    enum RegisterPageSections sectionID = PAGE_REG_SECTION_THREAD;

    if (gCSPopupID != CS_POPUP_THREADS) {
        // Draw the thread box:
        // cs_thread_draw_highlight(thread, CS_POPUP_THREADS_Y1);
        if (sRegisterSelectionCursor.sectionID == sectionID) {
            cs_draw_row_box_thread(CS_POPUP_THREADS_BG_X1, CS_POPUP_THREADS_Y1, COLOR_RGBA32_CRASH_SELECT_HIGHLIGHT);
        }
        cs_print_thread_info(TEXT_X(CS_POPUP_THREADS_TEXT_X1), CS_POPUP_THREADS_Y1, CS_POPUP_THREADS_NUM_CHARS_X, thread);
    }
    line++;
    sectionID++;

    while (sectionID < NUM_REG_PAGE_SECTIONS) {
        line = cs_registers_draw_register_list(line, sectionID++);
    }
}

void page_registers_input(void) {
    RegisterPageCursor* cursor = &sRegisterSelectionCursor;
    CrashScreenDirections* dir = &gCSDirectionFlags;
    _Bool up    = dir->pressed.up;
    _Bool down  = dir->pressed.down;
    _Bool left  = dir->pressed.left;
    _Bool right = dir->pressed.right;

    const RegisterPageSection* section = &sRegPageSections[cursor->sectionID];

    _Bool reginspectOpen = (gCSPopupID == CS_POPUP_REGINSPECT);
    const enum RegisterPageSections firstSection = (reginspectOpen ? PAGE_REG_SECTION_REG : PAGE_REG_SECTION_THREAD);
    const enum RegisterPageSections lastSection = (NUM_REG_PAGE_SECTIONS - 1);

    if (up) {
        if (cursor->selY > 0) {
            cursor->selY--;
        } else if (cursor->sectionID > firstSection) {
            // Go to bottom of previous section.
            cursor->sectionID--;
            cursor->selY = sRegPageSections[cursor->sectionID].rows - 1;
        } else {
            // Wrap vertically.
            cursor->sectionID = lastSection;
            cursor->selY = sRegPageSections[cursor->sectionID].rows - 1;
        }
    }
    if (down) {
        if (cursor->selY < (section->rows - 1)) {
            cursor->selY++;
        } else if (cursor->sectionID < lastSection) {
            // Go to top of next section;
            cursor->sectionID++;
            cursor->selY = 0;
        } else {
            // Wrap vertically.
            cursor->sectionID = firstSection;
            cursor->selY = 0;
        }
    }
    if (section->cols > 1) {
        if (left) {
            if (cursor->selX > 0) {
                cursor->selX--;
            } else {
                // Wrap horizontally.
                cursor->selX = (section->cols - 1);
            }
        }
        if (right) {
            if (cursor->selX < (section->cols - 1)) {
                cursor->selX++;
            } else {
                // Wrap horizontally.
                cursor->selX = 0;
            }
        }
    }

    section = &sRegPageSections[cursor->sectionID];

    u16 buttonPressed = gCSCompositeController->buttonPressed;
    if (reginspectOpen || (buttonPressed & A_BUTTON)) {
        u32 idx = 0;
        if (cursor->sectionID == PAGE_REG_SECTION_THREAD) {
            cs_open_threads();
        } else {
            idx = ((cursor->selY * section->cols) + cursor->selX);
            if ((idx >= section->size) && section->clamp) {
                idx = (section->size - 1);
            }
            if (idx < section->size) {
                cs_open_reginspect(section->list[idx]);
            }
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
    const RegisterPageSection* section = &sRegPageSections[PAGE_REG_SECTION_REG];
    const u32 columns = section->cols;
    const u32 rows = section->rows;
    const RegisterId* reg = &section->list[0];
    for (u32 row = 0; row < rows; row++) {
        osSyncPrintf("- ");
        for (u32 col = 0; col < columns; col++) {
            if (reg->raw == REG_LIST_TERMINATOR) {
                break;
            }

            const RegisterInfo* regInfo = get_reg_info(reg->src, reg->idx);

            if (regInfo != NULL) {
                osSyncPrintf("%s "STR_HEX_PREFIX STR_HEX_LONG" ", regInfo->shortName, get_reg_val(reg->src, reg->idx, TRUE));
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
    for (u32 row = 0; row < f_rows; row++) {
        osSyncPrintf("- ");
        for (u32 col = 0; col < f_columns; col++) {
            if (regNum >= CP1_NUM_REGISTERS) {
                break;
            }

            osSyncPrintf("d%02d "STR_HEX_DECIMAL"\t", regNum, get_reg_val(REGS_CP1, regNum, TRUE));

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
