#include <ultra64.h>
#include <PR/os_internal_error.h>
#include <stdarg.h>
#include <string.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen.h"
#include "crash_screen_draw.h"
#include "crash_screen_print.h"
#include "insn_disasm.h"
#include "map_parser.h"
#include "audio/external.h"
#include "buffers/framebuffers.h"
#include "buffers/zbuffer.h"
#include "engine/colors.h"
#include "game/debug.h"
#include "game/game_init.h"
#include "game/main.h"
#include "game/printf.h"
#include "game/puppyprint.h"
#include "game/rumble_init.h"
#include "game/vc_check.h"


static const char *sCauseDesc[18] = {
    /*EXC_INT    */ "Interrupt",
    /*EXC_MOD    */ "TLB modification",
    /*EXC_RMISS  */ "TLB exception on load or inst.",
    /*EXC_WMISS  */ "TLB exception on store",
    /*EXC_RADE   */ "Address error on load or inst.",
    /*EXC_WADE   */ "Address error on store",
    /*EXC_IBE    */ "Bus error on inst.",
    /*EXC_DBE    */ "Bus error on data",
    /*EXC_SYSCALL*/ "Failed Assert: See Assert Page",
    /*EXC_BREAK  */ "Breakpoint exception",
    /*EXC_II     */ "Reserved instruction",
    /*EXC_CPU    */ "Coprocessor unusable",
    /*EXC_OV     */ "Arithmetic overflow",
    /*EXC_TRAP   */ "Trap exception",
    /*EXC_VCEI   */ "Virtual coherency on inst.",
    /*EXC_FPE    */ "Floating point exception",
    /*EXC_WATCH  */ "Watchpoint exception",
    /*EXC_VCED   */ "Virtual coherency on data",
};

static const char *sFpcsrDesc[6] = {
    /*FPCSR_CE*/ "Unimplemented operation",
    /*FPCSR_CV*/ "Invalid operation",
    /*FPCSR_CZ*/ "Division by zero",
    /*FPCSR_CO*/ "Overflow",
    /*FPCSR_CU*/ "Underflow",
    /*FPCSR_CI*/ "Inexact operation",
};

static const char *sRegNames[29] = {
    "AT", "V0", "V1",
    "A0", "A1", "A2",
    "A3", "T0", "T1",
    "T2", "T3", "T4",
    "T5", "T6", "T7",
    "S0", "S1", "S2",
    "S3", "S4", "S5",
    "S6", "S7", "T8",
    "T9", "GP", "SP",
    "S8", "RA",
};

static const char *sPageNames[NUM_PAGES] = {
    [PAGE_CONTEXT    ] = "CONTEXT",
    [PAGE_ASSERTS    ] = "ASSERT",
#ifdef PUPPYPRINT_DEBUG
    [PAGE_LOG        ] = "LOG",
#endif
    [PAGE_STACK_TRACE] = "STACK TRACE",
    [PAGE_RAM_VIEWER ] = "RAM VIEW",
    [PAGE_DISASM     ] = "DISASM",
};

static const struct ControlType sControlsDescriptions[] = {
    [CONT_DESC_SWITCH_PAGE      ] = { .control = "L/R",                .description = "switch page"},
    [CONT_DESC_SHOW_CONTROLS    ] = { .control = "START",              .description = "show/hide page controls"},
    [CONT_DESC_CYCLE_DRAW       ] = { .control = "Z",                  .description = "cycle drawing overlay and background"},
    [CONT_DESC_SCROLL_LIST      ] = { .control = "UP/DOWN",            .description = "scroll list"},
    [CONT_DESC_CURSOR           ] = { .control = "UP/DOWN/LEFT/RIGHT", .description = "move cursor"},
    [CONT_DESC_CURSOR_VERTICAL  ] = { .control = "UP/DOWN",            .description = "move cursor"},
    [CONT_DESC_CURSOR_HORIZONTAL] = { .control = "LEFT/RIGHT",         .description = "move cursor"},
    [CONT_DESC_JUMP_TO_ADDRESS  ] = { .control = "A",                  .description = "jump to specific address"},
    [CONT_DESC_TOGGLE_ASCII     ] = { .control = "B",                  .description = "toggle bytes as hex or ascii"},
    [CONT_DESC_TOGGLE_FUNCTIONS ] = { .control = "A",                  .description = "toggle function names"},
    [CONT_DESC_TOGGLE_UNKNOWNS  ] = { .control = "B",                  .description = "toggle unknowns in list"},
};


struct CrashScreen gCrashScreen;
#ifdef CRASH_SCREEN_CRASH_SCREEN
struct CrashScreen gCrashScreen2;
#endif


#ifdef INCLUDE_DEBUG_MAP
static const RGBA32 sBranchColors[] = {
    COLOR_RGBA32_ORANGE,
    COLOR_RGBA32_LIME,
    COLOR_RGBA32_CYAN,
    COLOR_RGBA32_MAGENTA,
    COLOR_RGBA32_YELLOW,
    COLOR_RGBA32_PINK,
    COLOR_RGBA32_LIGHT_GRAY,
    COLOR_RGBA32_LIGHT_BLUE,
};

uintptr_t gCrashAddress = 0x0;

static struct BranchArrow sBranchArrows[DISASM_BRANCH_BUFFER_SIZE];
static u32 sNumBranchArrows = 0;

static struct FunctionInStack sAllFunctionStack[STACK_SIZE];
static struct FunctionInStack sKnownFunctionStack[STACK_SIZE];
static u32 sNumKnownFunctions = 0;
static u32 sNumShownFunctions = STACK_SIZE;

static s8 sStackTraceSkipUnknowns = FALSE;
static s8 sShowFunctionNames = TRUE;
#endif

static OSTime sCrashScreenInputTimeY = 0;
static OSTime sCrashScreenInputTimeX = 0;

static u8 sCrashScreenDirectionFlags = CRASH_SCREEN_INPUT_DIRECTION_FLAGS_NONE;

static s8 sDrawCrashScreen = TRUE;
static s8 sDrawBackground = TRUE;
static s8 sDrawControls = FALSE;
static s8 sCrashScreenSwitchedPage = FALSE;
static s8 sAddressSelectMenuOpen = FALSE;
static s8 sShowRamAsAscii = FALSE;
static u8 sUpdateBuffer = TRUE;
static u8 sCrashPage = PAGE_CONTEXT;
static s8 sAddressSelecCharIndex = 2;
static uintptr_t sAddressSelectTarget = 0;
static uintptr_t sSelectedAddress = 0;
static uintptr_t sScrollAddress = 0;
static u32 sStackTraceIndex = 0;

s8 gCrashScreenQueueFramebufferUpdate = FALSE;


void crash_screen_sleep(u32 ms) {
    u64 cycles = (((ms * 1000LL) * osClockRate) / 1000000ULL);
    osSetTime(0);
    while (osGetTime() < cycles) {}
}

void crash_screen_print_float_reg(u32 x, u32 y, u32 regNum, void *addr) {
    uintptr_t bits = *(uintptr_t*) addr;
    s32 exponent = (((bits >> 23) & (uintptr_t)BITMASK(8)) - 0x7F);

    crash_screen_print(x, y, "@%08XF%02d:", COLOR_RGBA32_CRASH_REGISTER, regNum);

    if ((exponent >= -0x7E && exponent <= 0x7F) || (bits == 0x0)) {
        f32 val = *(f32 *) addr;
        crash_screen_print(x + TEXT_WIDTH(4), y, "%s%.3e", ((val < 0) ? "" : " "), val);
    } else {
        crash_screen_print(x + TEXT_WIDTH(4), y, "%08XD", bits);
    }
}

void crash_screen_print_fpcsr(u32 fpcsr) {
    u32 bit = BIT(17);

    crash_screen_print(TEXT_X(0), (TEXT_Y(14) + 5), "@%08X%s:", COLOR_RGBA32_CRASH_REGISTER, "FPCSR");
    crash_screen_print(TEXT_X(6), (TEXT_Y(14) + 5), "%08X", fpcsr);

    for (u32 i = 0; i < ARRAY_COUNT(sFpcsrDesc); i++) {
        if (fpcsr & bit) {
            crash_screen_print(TEXT_X(16), (TEXT_Y(14) + 5), "@%08X(%s)", COLOR_RGBA32_CRASH_DESCRIPTION, sFpcsrDesc[i]);
            return;
        }

        bit >>= 1;
    }
}

void crash_screen_print_reg(u32 x, u32 y, const char *name, uintptr_t addr) {
    crash_screen_print(x, y, "@%08X%s:", COLOR_RGBA32_CRASH_REGISTER, name);
    crash_screen_print((x + TEXT_WIDTH(3)), y, "%08X", addr);
}

void crash_screen_print_registers(__OSThreadContext *tc) {
    u32 regNum = 0;
    u64 *reg = &tc->at;

    crash_screen_print_reg(TEXT_X(0 * 15), TEXT_Y( 3), "PC", tc->pc);
    crash_screen_print_reg(TEXT_X(1 * 15), TEXT_Y( 3), "SR", tc->sr);
    crash_screen_print_reg(TEXT_X(2 * 15), TEXT_Y( 3), "VA", tc->badvaddr);

    crash_screen_print_reg(TEXT_X(2 * 15), TEXT_Y(13), "MM", *(uintptr_t*)tc->pc);

    osWritebackDCacheAll();

    for (u32 y = 0; y < 10; y++) {
        for (u32 x = 0; x < 3; x++) {
            crash_screen_print_reg(TEXT_X(x * 15), TEXT_Y(4 + y), sRegNames[regNum], *(reg + regNum));

            regNum++;

            if ((reg + regNum) > &tc->ra) {
                return;
            }
        }
    }
}

void crash_screen_print_float_registers(__OSThreadContext *tc) {
    u32 regNum = 0;
    __OSfp *osfp = &tc->fp0;

    crash_screen_print_fpcsr(tc->fpcsr);

    osWritebackDCacheAll();

    for (u32 y = 0; y < 6; y++) {
        for (u32 x = 0; x < 3; x++) {
            crash_screen_print_float_reg(TEXT_X(x * 15), TEXT_Y(16 + y), regNum, &osfp->f.f_even);

            osfp++;
            regNum += 2;

            if (regNum > 30) {
                return;
            }
        }
    }
}

void draw_crash_context(OSThread *thread) {
    __OSThreadContext *tc = &thread->context;

    s32 cause = ((tc->cause >> CAUSE_EXCSHIFT) & BITMASK(5));
    // Make the last two cause case indexes sequential for array access.
    if (cause == (EXC_WATCH >> CAUSE_EXCSHIFT)) cause = 16;
    if (cause == (EXC_VCED  >> CAUSE_EXCSHIFT)) cause = 17;

    u32 line = 1;

    crash_screen_print(TEXT_X(0), TEXT_Y(line), "@%08XTHREAD:%d", COLOR_RGBA32_CRASH_THREAD, thread->id);
    line += crash_screen_print(TEXT_X(10), TEXT_Y(line), "@%08X(%s)", COLOR_RGBA32_CRASH_DESCRIPTION, sCauseDesc[cause]);

    osWritebackDCacheAll();

#ifdef INCLUDE_DEBUG_MAP
    uintptr_t pc = tc->pc;
    char *fname = parse_map(&pc);
    crash_screen_print(TEXT_X(0), TEXT_Y(line), "@%08XCRASH AT:", COLOR_RGBA32_CRASH_AT);
    if (fname == NULL) {
        crash_screen_print(TEXT_X(10), TEXT_Y(line), "@%08X%s", COLOR_RGBA32_CRASH_UNKNOWN, "UNKNOWN");
    } else {
        crash_screen_print(TEXT_X(10), TEXT_Y(line), "@%08X^%d%s", COLOR_RGBA32_CRASH_FUNCTION_NAME, (CRASH_SCREEN_NUM_CHARS_X - 10), fname);
    }
#endif

    crash_screen_print_registers(tc);

    osWritebackDCacheAll();

    crash_screen_print_float_registers(tc);
}

void draw_assert(UNUSED OSThread *thread) {
    u32 line = 1;

    line += crash_screen_print(TEXT_X(0), TEXT_Y(line), "@%08X%s", COLOR_RGBA32_CRASH_PAGE_NAME, sPageNames[PAGE_ASSERTS]);

    crash_screen_draw_divider(DIVIDER_Y(line));

    if (__n64Assert_Filename != NULL) {
        line += crash_screen_print(TEXT_X(0), TEXT_Y(line), "@%08XFILE:%s", COLOR_RGBA32_CRASH_FILE_NAME, __n64Assert_Filename);
        crash_screen_draw_divider(DIVIDER_Y(line));
        line += crash_screen_print(TEXT_X(0), TEXT_Y(line), "@%08XLINE:%d", COLOR_RGBA32_CRASH_AT, __n64Assert_LineNum);
        crash_screen_draw_divider(DIVIDER_Y(line));
        line += crash_screen_print(TEXT_X(0), TEXT_Y(line), "@%08XMESSAGE:", COLOR_RGBA32_CRASH_HEADER);
        line += crash_screen_print(TEXT_X(0), (TEXT_Y(line) + 5), "%s", __n64Assert_Message);
    } else {
        crash_screen_print(TEXT_X(0), TEXT_Y(line), "no failed assert to report.");
    }

    osWritebackDCacheAll();
}

#ifdef PUPPYPRINT_DEBUG
void draw_crash_log(UNUSED OSThread *thread) {
    u32 i;

    crash_screen_print(TEXT_X(0), TEXT_Y(1), "@%08X%s", COLOR_RGBA32_CRASH_PAGE_NAME, sPageNames[PAGE_LOG]);
    crash_screen_draw_divider(DIVIDER_Y(2));

    osWritebackDCacheAll();

    for (i = 0; i < LOG_BUFFER_SIZE; i++) {
        crash_screen_print(TEXT_X(0), TEXT_Y(1 + (LOG_BUFFER_SIZE - i)), consoleLogTable[i]);
    }
}
#endif

void crash_screen_draw_scroll_bar(u32 topY, u32 bottomY, u32 numVisibleEntries, u32 numTotalEntries, u32 currEntry, u32 minScrollBarHeight, RGBA32 color) {
    // Start on the pixel below the divider.
    topY++;

    // Determine size of the scroll bar.
    u32 totalHeight = (bottomY - topY);

    u32 scrollBarHeight = (numVisibleEntries * ((f32) totalHeight / (f32) numTotalEntries));
    scrollBarHeight = CLAMP(scrollBarHeight, minScrollBarHeight, totalHeight);

    // Determine position of the scroll bar.
    f32 scrollableHeight = (totalHeight - scrollBarHeight);
    f32 numScrollableEntries = (numTotalEntries - numVisibleEntries);
    u32 scrollPos = (currEntry * (scrollableHeight / numScrollableEntries));

    // Draw the scroll bar rectangle.
    crash_screen_draw_rect((CRASH_SCREEN_X2 - 1), (topY + scrollPos), 1, scrollBarHeight, color);
}

// prints any function pointers it finds in the stack format:
// SP address: function name
void draw_stack_trace(OSThread *thread) {
    __OSThreadContext *tc = &thread->context;
    uintptr_t temp_sp = (tc->sp + 0x14);

    u32 line = 1;

    crash_screen_print(TEXT_X(0), TEXT_Y(line), "@%08X%s", COLOR_RGBA32_CRASH_PAGE_NAME, sPageNames[PAGE_STACK_TRACE]);
    line += crash_screen_print(TEXT_X(12), TEXT_Y(line), "FROM %08X", temp_sp);
    crash_screen_draw_divider(DIVIDER_Y(line));

#ifdef INCLUDE_DEBUG_MAP
    crash_screen_print(TEXT_X(0), TEXT_Y(line), "@%08XCURRFUNC:", COLOR_RGBA32_CRASH_AT);
    uintptr_t pc = tc->pc;
    char *fname = parse_map(&pc);
    if (fname == NULL) {
        line += crash_screen_print(TEXT_X(9), TEXT_Y(line), "@%08X%s", COLOR_RGBA32_CRASH_UNKNOWN, "UNKNOWN");
    } else {
        line += crash_screen_print(TEXT_X(9), TEXT_Y(line), "@%08X^%d%s", COLOR_RGBA32_CRASH_FUNCTION_NAME, (CRASH_SCREEN_NUM_CHARS_X - 9), fname);
    }

    crash_screen_draw_divider(DIVIDER_Y(line));

    osWritebackDCacheAll();

    struct FunctionInStack *functionList = (sStackTraceSkipUnknowns ? sKnownFunctionStack : sAllFunctionStack);
    struct FunctionInStack *function = NULL;

    // Print
    for (u32 i = 0; i < STACK_TRACE_NUM_ROWS; i++) {
        u32 y = TEXT_Y(line + i);

        u32 currIndex = (sStackTraceIndex + i);

        if (currIndex >= sNumShownFunctions) {
            break;
        }

        function = &functionList[currIndex];

        if (function != NULL) {
            uintptr_t faddr = function->addr;
            char *fname = function->name;

            crash_screen_print(TEXT_X(0), y, "%08X:", faddr);

            if (!sStackTraceSkipUnknowns && (fname == NULL)) {
                // Print unknown function
                crash_screen_print(TEXT_X(9), y, "@%08X%08X", COLOR_RGBA32_CRASH_UNKNOWN, *(uintptr_t*)faddr);
            } else {
                // Print known function
                if (sShowFunctionNames) {
                    crash_screen_print(TEXT_X(9), y, "@%08x^%d%s", COLOR_RGBA32_CRASH_FUNCTION_NAME_2, (CRASH_SCREEN_NUM_CHARS_X - 9), fname);
                } else {
                    crash_screen_print(TEXT_X(9), y, "@%08x%08X", COLOR_RGBA32_CRASH_FUNCTION_NAME_2, *(uintptr_t*)faddr);
                }
            }
        }
    }

    osWritebackDCacheAll();

    crash_screen_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y));

    // Scroll Bar
    crash_screen_draw_scroll_bar(DIVIDER_Y(3), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y), STACK_TRACE_NUM_ROWS, sNumShownFunctions, sStackTraceIndex, 4, COLOR_RGBA32_LIGHT_GRAY);
#else
    osWritebackDCacheAll();

    crash_screen_print(TEXT_X(0), TEXT_Y(line), "STACK TRACE DISABLED");
#endif

    osWritebackDCacheAll();
}

void draw_address_select(void) {
    crash_screen_draw_dark_rect((JUMP_MENU_X1 -  JUMP_MENU_MARGIN_X     ), (JUMP_MENU_Y1 -  JUMP_MENU_MARGIN_Y     ),
                                (JUMP_MENU_W  + (JUMP_MENU_MARGIN_X * 2)), (JUMP_MENU_H  + (JUMP_MENU_MARGIN_Y * 2)),
                                3);

    crash_screen_print(SCREEN_CENTER_X - TEXT_WIDTH(3), JUMP_MENU_Y1, "GO TO:");

    crash_screen_draw_vertical_triangle((SCREEN_CENTER_X - TEXT_WIDTH(4) + (sAddressSelecCharIndex * TEXT_WIDTH(1)) - 1), (JUMP_MENU_Y1 + TEXT_HEIGHT(1) + CRASH_SCREEN_CHAR_SPACING_Y),
                                        TEXT_WIDTH(1), TEXT_WIDTH(1),
                                        COLOR_RGBA32_CRASH_SELECT_ARROWS, FALSE);
    crash_screen_draw_vertical_triangle((SCREEN_CENTER_X - TEXT_WIDTH(4) + (sAddressSelecCharIndex * TEXT_WIDTH(1)) - 1), (JUMP_MENU_Y1 + TEXT_HEIGHT(3) - CRASH_SCREEN_CHAR_SPACING_Y + 1),
                                        TEXT_WIDTH(1), TEXT_WIDTH(1),
                                        COLOR_RGBA32_CRASH_SELECT_ARROWS, TRUE);
    crash_screen_print((SCREEN_CENTER_X - TEXT_WIDTH(8 / 2) - TEXT_WIDTH(2)), (JUMP_MENU_Y1 + TEXT_HEIGHT(2)), "0x%08X", sAddressSelectTarget);

    char *fname = NULL;
    uintptr_t checkAddr = sAddressSelectTarget;
    fname = parse_map(&checkAddr);
    if (fname != NULL) {
        crash_screen_print(JUMP_MENU_X1, (JUMP_MENU_Y1 + TEXT_HEIGHT(4)), "@%08X^%d%s", COLOR_RGBA32_CRASH_FUNCTION_NAME, JUMP_MENU_CHARS_X, fname);
    }

    osWritebackDCacheAll();
}

void clamp_view_to_selection(const u32 numRows, const u32 step) {
    u32 bound;
    const size_t size = (numRows * step);

    bound = sSelectedAddress - (step - 1);
    if (sScrollAddress > bound) {
        sScrollAddress = bound;
    }
    bound = sSelectedAddress - (size - 1);
    if (sScrollAddress < bound) {
        sScrollAddress = bound;
    }

    sScrollAddress = CLAMP(sScrollAddress, RAM_START, (RAM_END - size));
    sScrollAddress = ALIGN(sScrollAddress, step);
}

void draw_ram_viewer(OSThread *thread) {
    __OSThreadContext *tc = &thread->context;

    clamp_view_to_selection(RAM_VIEWER_NUM_ROWS, RAM_VIEWER_STEP);

    uintptr_t startAddr = sScrollAddress;
    u32 charX, charY;
    u32 line = 1;

    crash_screen_print(TEXT_X(0), TEXT_Y(line), "@%08X%s", COLOR_RGBA32_CRASH_PAGE_NAME, sPageNames[PAGE_RAM_VIEWER]);
    line += crash_screen_print(TEXT_X(9), TEXT_Y(line), "%08X in %08X-%08X", sSelectedAddress, startAddr, (startAddr + RAM_VIEWER_SHOWN_SECTION));
    crash_screen_draw_divider(DIVIDER_Y(line));

    charX = (TEXT_X(8) + 3);

    for (u32 i = 0; i < 16; i++) {
        if ((i % 4) == 0) {
            charX += 2;
        }

        crash_screen_print(charX, TEXT_Y(line), "@%08X%02X", ((i % 2) ? COLOR_RGBA32_CRASH_RAM_VIEW_H1 : COLOR_RGBA32_CRASH_RAM_VIEW_H2), i);

        charX += (TEXT_WIDTH(2) + 1);
    }

    crash_screen_draw_divider(DIVIDER_Y(3));

    crash_screen_draw_rect((TEXT_X(8) + 2), DIVIDER_Y(line), 1, TEXT_HEIGHT(line + RAM_VIEWER_NUM_ROWS - 1), COLOR_RGBA32_LIGHT_GRAY);

    line += crash_screen_print(TEXT_X(1), TEXT_Y(line), "MEMORY");

    charX = (TEXT_X(8) + 3);
    charY = TEXT_Y(line);

    RGBA32 color;
    uintptr_t currAddr;
    u8 byte;

    for (u32 y = 0; y < RAM_VIEWER_NUM_ROWS; y++) {
        uintptr_t rowAddr = startAddr + (y * RAM_VIEWER_STEP);
        crash_screen_print(TEXT_X(0), TEXT_Y(line + y), "@%08X%08X", ((y % 2) ? COLOR_RGBA32_CRASH_RAM_VIEW_B1 : COLOR_RGBA32_CRASH_RAM_VIEW_B2), rowAddr);

        charX = (TEXT_X(8) + 3);
        charY = TEXT_Y(line + y);
        for (u32 x = 0; x < 16; x++) {
            currAddr = (rowAddr + x);
            byte = *((u8 *)currAddr);

            if ((x % 4) == 0) {
                charX += 2;
            }

            color = ((sShowRamAsAscii || (x % 2)) ? COLOR_RGBA32_WHITE : COLOR_RGBA32_LIGHT_GRAY);

            if (currAddr == tc->pc) {
                crash_screen_draw_rect((charX - 1), (charY - 1), (TEXT_WIDTH(2) + 1), (TEXT_WIDTH(1) + 3), COLOR_RGBA32_RED);
            }
            if (currAddr == sSelectedAddress) {
                crash_screen_draw_rect((charX - 1), (charY - 1), (TEXT_WIDTH(2) + 1), (TEXT_WIDTH(1) + 3), COLOR_RGBA32_WHITE);
                color = COLOR_RGBA32_BLACK;
            }

            if (sShowRamAsAscii) {
                crash_screen_draw_glyph(charX + TEXT_WIDTH(1), charY, byte, color);
            } else {
                crash_screen_print(charX, charY, "@%08X%02X", color, byte);
            }

            charX += (TEXT_WIDTH(2) + 1);
        }
    }

    u32 line2 = (line + RAM_VIEWER_NUM_ROWS);

    crash_screen_draw_divider(DIVIDER_Y(line2));

    // Scroll bar
    crash_screen_draw_scroll_bar(DIVIDER_Y(line), DIVIDER_Y(line2), RAM_VIEWER_SHOWN_SECTION, TOTAL_RAM_SIZE, (sScrollAddress - RAM_VIEWER_SCROLL_MIN), 4, COLOR_RGBA32_LIGHT_GRAY);

    osWritebackDCacheAll();

    if (sAddressSelectMenuOpen) {
        draw_address_select();
    }

    osWritebackDCacheAll();
}

#ifdef INCLUDE_DEBUG_MAP
void crash_screen_fill_branch_buffer(const char *fname, const uintptr_t funcAddr) {
    struct BranchArrow *currArrow = NULL;
    uintptr_t addr, checkAddr;
    InsnData toDisasm;
    s16 branchOffset;
    s16 curBranchColorIndex = 0;
    s32 curBranchX = DISASM_BRANCH_ARROW_OFFSET;

    if (fname == NULL) {
        return;
    }

    bzero(sBranchArrows, sizeof(sBranchArrows));

    sNumBranchArrows = 0;

    if (fname != NULL) {
        s32 numBranchArrows = sNumBranchArrows;
        currArrow = sBranchArrows;

        for (addr = funcAddr; addr < (funcAddr + DISASM_FUNCTION_SEARCH_MAX_OFFSET); addr += DISASM_STEP) {
            checkAddr = addr;
            if (fname != parse_map(&checkAddr)) {
                break;
            }
            toDisasm.d = *(uintptr_t*)addr;

            branchOffset = get_branch_offset(toDisasm);

            if (branchOffset != 0) {
                currArrow->startAddr = addr;
                currArrow->branchOffset = branchOffset;
                currArrow->colorIndex = curBranchColorIndex;
                currArrow->xPos = curBranchX;

                curBranchX += DISASM_BRANCH_ARROW_SPACING;
                curBranchColorIndex = ((curBranchColorIndex + 1) % ARRAY_COUNT(sBranchColors));

                if ((DISASM_BRANCH_ARROW_START_X + curBranchX) > CRASH_SCREEN_TEXT_X2) {
                    curBranchX = DISASM_BRANCH_ARROW_OFFSET;
                }

                currArrow++;
                numBranchArrows++;
            }
        }

        sNumBranchArrows = numBranchArrows;
    }
}
#endif

void draw_disasm_branch_arrow(s32 startLine, s32 endLine, s32 dist, RGBA32 color, s32 printLine) {
    // Check to see if arrow is fully away from the screen.
    if (((startLine >= 0)               || (endLine >= 0))
     && ((startLine <  DISASM_NUM_ROWS) || (endLine <  DISASM_NUM_ROWS))) {
        s32 arrowStartHeight = (TEXT_Y(printLine + startLine) + 3);
        s32 arrowEndHeight   = (TEXT_Y(printLine +   endLine) + 3);

        if (startLine < 0) {
            arrowStartHeight = TEXT_Y(printLine) - 1;
        } else if (startLine >= DISASM_NUM_ROWS) {
            arrowStartHeight = (TEXT_Y(printLine + DISASM_NUM_ROWS) - 2);
        } else {
            crash_screen_draw_rect((DISASM_BRANCH_ARROW_START_X + 1), arrowStartHeight, dist, 1, color);
        }

        if (endLine < 0) {
            arrowEndHeight = TEXT_Y(printLine) - 1;
        } else if (endLine >= DISASM_NUM_ROWS) {
            arrowEndHeight = (TEXT_Y(printLine + DISASM_NUM_ROWS) - 2);
        } else {
            u32 x = ((DISASM_BRANCH_ARROW_START_X + dist) - DISASM_BRANCH_ARROW_OFFSET);
            crash_screen_draw_rect((x + 0), (arrowEndHeight - 0), (DISASM_BRANCH_ARROW_OFFSET + 1), 1, color);
            // Arrow head
            crash_screen_draw_rect((x + 1), (arrowEndHeight - 1), 1, 3, color);
            crash_screen_draw_rect((x + 2), (arrowEndHeight - 2), 1, 5, color);
        }

        s32 height = abss(arrowEndHeight - arrowStartHeight);

        // Middle of arrow
        crash_screen_draw_rect((DISASM_BRANCH_ARROW_START_X + dist), MIN(arrowStartHeight, arrowEndHeight), 1, height, color);
    }
}

void draw_disasm(OSThread *thread) {
    __OSThreadContext *tc = &thread->context;
    char *fname = NULL;
    uintptr_t alignedSelectedAddr = sSelectedAddress & ~(DISASM_STEP - 1);

#ifdef INCLUDE_DEBUG_MAP
    uintptr_t funcAddr = alignedSelectedAddr;
    fname = parse_map(&funcAddr);

    if (sCrashScreenSwitchedPage) {
        crash_screen_fill_branch_buffer(fname, funcAddr);
    }
#endif

    clamp_view_to_selection(DISASM_NUM_ROWS, DISASM_STEP);

    u32 line = 1;

    crash_screen_print(TEXT_X(0), TEXT_Y(line), "@%08X%s", COLOR_RGBA32_CRASH_PAGE_NAME, sPageNames[PAGE_DISASM]);
    line += crash_screen_print(TEXT_X(7), TEXT_Y(line), "%08X in %08X-%08X", alignedSelectedAddr, sScrollAddress, (sScrollAddress + DISASM_SHOWN_SECTION));
    crash_screen_draw_divider(DIVIDER_Y(line));

    if (fname == NULL) {
        line += crash_screen_print(TEXT_X(0), TEXT_Y(line), "NOT IN A FUNCTION");
    } else {
        crash_screen_print(TEXT_X(0), TEXT_Y(line), "IN:");
        line += crash_screen_print(TEXT_X(3), TEXT_Y(line), "@%08X^%d%s", COLOR_RGBA32_CRASH_FUNCTION_NAME, (CRASH_SCREEN_NUM_CHARS_X - 3), fname);
    }

    osWritebackDCacheAll();

#ifdef INCLUDE_DEBUG_MAP
    // Draw branch arrows from the buffer.
    struct BranchArrow *currArrow = NULL;
    for (u32 b = 0; b < sNumBranchArrows; b++) {
        currArrow = &sBranchArrows[b];
        s32 startLine = (((s32)currArrow->startAddr - (s32)sScrollAddress) / DISASM_STEP);
        s32 endLine = (startLine + currArrow->branchOffset + 1);
        draw_disasm_branch_arrow(startLine, endLine, currArrow->xPos, sBranchColors[currArrow->colorIndex], line);
    }

    osWritebackDCacheAll();
#endif

    gCrashScreenWordWrap = FALSE;

    u32 charX = TEXT_X(0);
    u32 charY = TEXT_Y(line);

    for (u32 y = 0; y < DISASM_NUM_ROWS; y++) {
        uintptr_t addr = (sScrollAddress + (y * DISASM_STEP));
        InsnData toDisasm;
        toDisasm.d = *(uintptr_t*)addr;

        charY = TEXT_Y(line + y);

        if (addr == tc->pc) {
            crash_screen_draw_rect((charX - 1), (charY - 2), (CRASH_SCREEN_TEXT_W + 1), (TEXT_HEIGHT(1) + 1), COLOR_RGBA32_CRASH_PC);
        } else if (addr == alignedSelectedAddr) {
            crash_screen_draw_rect((charX - 1), (charY - 2), (CRASH_SCREEN_TEXT_W + 1), (TEXT_HEIGHT(1) + 1), COLOR_RGBA32_CRASH_SELECT);
#ifndef INCLUDE_DEBUG_MAP
            if (is_in_code_segment(addr)) {
                s16 branchOffset = get_branch_offset(toDisasm);
                if (branchOffset != 0) {
                    s32 startLine = (((s32)addr - (s32)sScrollAddress) / DISASM_STEP);
                    s32 endLine = (startLine + branchOffset + 1);
                    draw_disasm_branch_arrow(startLine, endLine, DISASM_BRANCH_ARROW_OFFSET, COLOR_RGBA32_CRASH_FUNCTION_NAME_2, line);
                }
            }
#endif
        }

        if (is_in_code_segment(addr)) {
            crash_screen_print(charX, charY, "%s", insn_disasm(toDisasm, (addr == tc->pc)));
        } else if (sShowRamAsAscii) {
            // for (u32 c = 0; c < 4; c++) {
            //     crash_screen_draw_glyph(charX + (c * TEXT_WIDTH(1)), charY, (unsigned char)(toDisasm.d >> ((32 - 8) - (8 * c))), COLOR_RGBA32_WHITE);
            // }
            s32 bitX = charX;
            for (u32 c = 0; c < 32; c++) {
                if ((c % 8) == 0) {
                    bitX += TEXT_WIDTH(1);
                }
                crash_screen_draw_glyph(bitX, charY, ((toDisasm.d >> (32 - c)) % 2) ? '1' : '0', COLOR_RGBA32_WHITE);
                bitX += TEXT_WIDTH(1);
            }
        } else {
            crash_screen_print(charX, charY, "%08X", toDisasm.d);
        }
    }

    gCrashScreenWordWrap = TRUE;

    osWritebackDCacheAll();

    crash_screen_draw_divider(DIVIDER_Y(line));

    u32 line2 = (line + DISASM_NUM_ROWS);

    crash_screen_draw_divider(DIVIDER_Y(line2));

    // Scroll bar
    crash_screen_draw_scroll_bar(DIVIDER_Y(line), DIVIDER_Y(line2), DISASM_SHOWN_SECTION, TOTAL_RAM_SIZE, (sScrollAddress - DISASM_SCROLL_MIN), 4, COLOR_RGBA32_LIGHT_GRAY);

    // Scroll bar crash position marker
    crash_screen_draw_scroll_bar(DIVIDER_Y(line), DIVIDER_Y(line2), DISASM_SHOWN_SECTION, TOTAL_RAM_SIZE, (tc->pc - DISASM_SCROLL_MIN), 1, COLOR_RGBA32_CRASH_AT);

    osWritebackDCacheAll();

    if (sAddressSelectMenuOpen) {
        draw_address_select();
    }

    osWritebackDCacheAll();
}

void update_crash_screen_direction_input(void) {
    OSTime currTime = osGetTime();

    u8 prevHeld = (sCrashScreenDirectionFlags & BITMASK(4));
    u8 currPressed = CRASH_SCREEN_INPUT_DIRECTION_FLAGS_NONE;

    s16 rawStickX  = gPlayer1Controller->rawStickX;
    s16 rawStickY  = gPlayer1Controller->rawStickY;
    u16 buttonDown = gPlayer1Controller->buttonDown;

    u8 heldY = ((((buttonDown & (U_CBUTTONS | U_JPAD)) || (rawStickY >  60)) << 0)   // CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_UP
              | (((buttonDown & (D_CBUTTONS | D_JPAD)) || (rawStickY < -60)) << 1)); // CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_DOWN
    u8 heldX = ((((buttonDown & (L_CBUTTONS | L_JPAD)) || (rawStickX < -60)) << 2)   // CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_LEFT
              | (((buttonDown & (R_CBUTTONS | R_JPAD)) || (rawStickX >  60)) << 3)); // CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_RIGHT

    if (heldY && !(prevHeld & (CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_UP | CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_DOWN))) {
        // On press
        sCrashScreenInputTimeY = currTime;
        currPressed |= heldY;
    } else if (heldY) {
        // held
        OSTime diff = (currTime - sCrashScreenInputTimeY);
        if (diff > FRAMES_TO_CYCLES(10)) {
            currPressed |= heldY;
        }
    }

    if (heldX && !(prevHeld & (CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_LEFT | CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_RIGHT))) {
        // On press
        sCrashScreenInputTimeX = currTime;
        currPressed |= heldX;
    } else if (heldX) {
        // held
        OSTime diff = (currTime - sCrashScreenInputTimeX);
        if (diff > FRAMES_TO_CYCLES(10)) {
            currPressed |= heldX;
        }
    }

    sCrashScreenDirectionFlags = ((currPressed << 4) | heldX | heldY);
}

s32 update_crash_screen_page(void) {
    u8 prevPage = sCrashPage;

    if (gPlayer1Controller->buttonPressed & L_TRIG) {
        // Previous Page.
        sCrashPage--;
        sUpdateBuffer = TRUE;
    }
    if (gPlayer1Controller->buttonPressed & R_TRIG) {
        // Next page.
        sCrashPage++;
        sUpdateBuffer = TRUE;
    }

    if (sCrashPage != prevPage) {
        // Wrap pages.
        if ((sCrashPage >= NUM_PAGES) && (sCrashPage != PAGES_MAX)) {
            sCrashPage = PAGE_CONTEXT;
        }
        if (sCrashPage == PAGES_MAX) {
            sCrashPage = (NUM_PAGES - 1);
        }

        // Reset certain values when the page is changed.
        sStackTraceIndex = 0;
        sDrawControls = FALSE;
        sCrashScreenSwitchedPage = TRUE;

        return TRUE;
    }

    return sDrawControls;
}

void crash_screen_input_default(void) {
    update_crash_screen_page();
}

void crash_screen_input_stack_trace(void) {
    if (!update_crash_screen_page()) {
#ifdef INCLUDE_DEBUG_MAP
        if (gPlayer1Controller->buttonPressed & A_BUTTON) {
            // Toggle whether to display function names.
            sShowFunctionNames ^= TRUE;
            sUpdateBuffer = TRUE;
        }

        if (gPlayer1Controller->buttonPressed & B_BUTTON) {
            // Toggle whether entries without a name are skipped.
            sStackTraceSkipUnknowns ^= TRUE;
            sNumShownFunctions = (sStackTraceSkipUnknowns ? sNumKnownFunctions : STACK_SIZE);
            sStackTraceIndex = 0;
            sUpdateBuffer = TRUE;
        }

        if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_UP) {
            // Scroll up.
            if (sStackTraceIndex > 0) {
                sStackTraceIndex--;
                sUpdateBuffer = TRUE;
            }
        }
        if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_DOWN) {
            // Scroll down.
            if (sStackTraceIndex < (sNumShownFunctions - STACK_TRACE_NUM_ROWS)) {
                sStackTraceIndex++;
                sUpdateBuffer = TRUE;
            }
        }
#endif
    }
}

void crash_screen_select_address(void) {
    if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_LEFT) {
        sAddressSelecCharIndex = ((sAddressSelecCharIndex - 1) & 0x7); // % 8
        sUpdateBuffer = TRUE;
    }
    if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_RIGHT) {
        sAddressSelecCharIndex = ((sAddressSelecCharIndex + 1) & 0x7); // % 8
        sUpdateBuffer = TRUE;
    }

    uintptr_t nextSelectedAddress = sAddressSelectTarget;
    u32 shift = ((32 - 4) - (sAddressSelecCharIndex * 4));
    u8 digit = GET_HEX_DIGIT(sAddressSelectTarget, shift);
    s8 new = digit;

    if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_UP) {
        // Increment the selected digit.
        new = ((digit + 1) & BITMASK(4));
        if (!IS_IN_RAM(SET_HEX_DIGIT(sAddressSelectTarget, new, shift))) {
            // Find the digit to wrap to
            for (new = 0x0; new < 0xF; new++) {
                if (IS_IN_RAM(SET_HEX_DIGIT(sAddressSelectTarget, new, shift))) {
                    break;
                }
            }
        }
    }
    if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_DOWN) {
        // Decrement the selected digit.
        new = ((digit - 1) & BITMASK(4));
        if (!IS_IN_RAM(SET_HEX_DIGIT(sAddressSelectTarget, new, shift))) {
            // Find the digit to wrap to
            for (new = 0xF; new > 0x0; new--) {
                if (IS_IN_RAM(SET_HEX_DIGIT(sAddressSelectTarget, new, shift))) {
                    break;
                }
            }
        }
    }

    if (new != digit) {
        nextSelectedAddress = SET_HEX_DIGIT(sAddressSelectTarget, new, shift);

        if (IS_IN_RAM(nextSelectedAddress)) {
            sAddressSelectTarget = nextSelectedAddress;
            sUpdateBuffer = TRUE;
        }
    }

    if (gPlayer1Controller->buttonPressed & A_BUTTON) {
        // Jump to the address and close the popup.
        sAddressSelectMenuOpen = FALSE;
        sSelectedAddress = sAddressSelectTarget;
#ifdef INCLUDE_DEBUG_MAP
        uintptr_t funcAddr = sSelectedAddress;
        char *fname = parse_map(&funcAddr);
        crash_screen_fill_branch_buffer(fname, funcAddr);
#endif
        sUpdateBuffer = TRUE;
    }

    if (gPlayer1Controller->buttonPressed & B_BUTTON) {
        // Close the popup without jumping.
        sAddressSelectMenuOpen = FALSE;
        sUpdateBuffer = TRUE;
    }
}

void crash_screen_input_ram_viewer(void) {
    if (sAddressSelectMenuOpen) {
        crash_screen_select_address();
    } else if (!update_crash_screen_page()) {
        if ((sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_UP)
         && ((sSelectedAddress - RAM_VIEWER_STEP) >= RAM_START)) {
            sSelectedAddress -= RAM_VIEWER_STEP;
            sUpdateBuffer = TRUE;
        }
        if ((sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_DOWN)
         && ((sSelectedAddress + RAM_VIEWER_STEP) < RAM_END)) {
            sSelectedAddress += RAM_VIEWER_STEP;
            sUpdateBuffer = TRUE;
        }
        if ((sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_LEFT)
         && (((sSelectedAddress - 1) & 0xF) != 0xF)) {
            sSelectedAddress--;
            sUpdateBuffer = TRUE;
        }
        if ((sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_RIGHT)
         && (((sSelectedAddress + 1) & 0xF) != 0x0)) {
            sSelectedAddress++;
            sUpdateBuffer = TRUE;
        }
        if (gPlayer1Controller->buttonPressed & A_BUTTON) {
            // Open the jump to address popup.
            sAddressSelectMenuOpen = TRUE;
            sAddressSelectTarget = sSelectedAddress;
            sUpdateBuffer = TRUE;
        }
        if (gPlayer1Controller->buttonPressed & B_BUTTON) {
            // Toggle whether the memory is printed as hex values or as ASCII chars.
            sShowRamAsAscii ^= TRUE;
            sUpdateBuffer = TRUE;
        }
    }
}

void crash_screen_input_disasm(void) {
    if (sAddressSelectMenuOpen) {
        crash_screen_select_address();
    } else if (!update_crash_screen_page()) {
#ifdef INCLUDE_DEBUG_MAP
        uintptr_t oldPos = sSelectedAddress;
#endif
        // sSelectedAddress = ALIGN(sSelectedAddress, DISASM_STEP);
        if ((sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_UP)
         && ((sSelectedAddress - DISASM_STEP) >= RAM_START)) {
            // Scroll up.
            sSelectedAddress -= DISASM_STEP;
            sUpdateBuffer = TRUE;
        }
        if ((sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_DOWN)
         && ((sSelectedAddress + DISASM_STEP) < RAM_END)) {
            // Scroll down.
            sSelectedAddress += DISASM_STEP;
            sUpdateBuffer = TRUE;
        }
        if (gPlayer1Controller->buttonPressed & A_BUTTON) {
            // Open the jump to address box.
            sAddressSelectMenuOpen = TRUE;
            sAddressSelectTarget = get_branch_target_from_addr(sSelectedAddress);
            sUpdateBuffer = TRUE;
        }
        if (gPlayer1Controller->buttonPressed & B_BUTTON) {
            // Toggle whether the memory is printed as hex values or as ASCII chars.
            sShowRamAsAscii ^= TRUE;
            sUpdateBuffer = TRUE;
        }
#ifdef INCLUDE_DEBUG_MAP
        if (oldPos != sSelectedAddress) {
            sSelectedAddress &= ~(DISASM_STEP - 1);
            uintptr_t newFunc = sSelectedAddress;
            parse_map(&oldPos);
            char *fname = parse_map(&newFunc);
            if (oldPos != newFunc) {
                crash_screen_fill_branch_buffer(fname, newFunc);
            }
        }
#endif
    }
}

static const enum ControlTypes defaultPageControls[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_LIST_END,
};

static const enum ControlTypes stackTracePageControls[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_SCROLL_LIST,
    CONT_DESC_TOGGLE_FUNCTIONS,
    CONT_DESC_TOGGLE_UNKNOWNS,
    CONT_DESC_LIST_END,
};

static const enum ControlTypes ramViewerPageControls[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_CURSOR,
    CONT_DESC_JUMP_TO_ADDRESS,
    CONT_DESC_TOGGLE_ASCII,
    CONT_DESC_LIST_END,
};

static const enum ControlTypes disasmPageControls[] = {
    CONT_DESC_SWITCH_PAGE,
    CONT_DESC_SHOW_CONTROLS,
    CONT_DESC_CYCLE_DRAW,
    CONT_DESC_CURSOR_VERTICAL,
    CONT_DESC_JUMP_TO_ADDRESS,
    CONT_DESC_TOGGLE_ASCII,
    CONT_DESC_LIST_END,
};

struct CrashScreenPage sCrashScreenPages[] = {
    [PAGE_CONTEXT    ] = {.drawFunc = draw_crash_context, .inputFunc = crash_screen_input_default,     .pageControlsList = defaultPageControls   },
    [PAGE_ASSERTS    ] = {.drawFunc = draw_assert,        .inputFunc = crash_screen_input_default,     .pageControlsList = defaultPageControls   },
#ifdef PUPPYPRINT_DEBUG
    [PAGE_LOG        ] = {.drawFunc = draw_crash_log,     .inputFunc = crash_screen_input_default,     .pageControlsList = defaultPageControls   },
#endif
    [PAGE_STACK_TRACE] = {.drawFunc = draw_stack_trace,   .inputFunc = crash_screen_input_stack_trace, .pageControlsList = stackTracePageControls},
    [PAGE_RAM_VIEWER ] = {.drawFunc = draw_ram_viewer,    .inputFunc = crash_screen_input_ram_viewer,  .pageControlsList = ramViewerPageControls },
    [PAGE_DISASM     ] = {.drawFunc = draw_disasm,        .inputFunc = crash_screen_input_disasm,      .pageControlsList = disasmPageControls    },
};

void update_crash_screen_input(void) {
    // Global controls.
    if (gPlayer1Controller->buttonPressed & Z_TRIG) {
        sDrawCrashScreen ^= TRUE;
        if (sDrawCrashScreen) {
            sDrawBackground ^= TRUE;
        } else if (!sDrawBackground) {
            sDrawCrashScreen = TRUE;
            sDrawBackground = TRUE;
            sDrawControls = FALSE;
        }
        sUpdateBuffer = TRUE;
    }

    if (sDrawCrashScreen && (gPlayer1Controller->buttonPressed & START_BUTTON)) {
        sDrawControls ^= TRUE;
        sUpdateBuffer = TRUE;
    }

    if (sDrawCrashScreen) {
        update_crash_screen_direction_input();

        // Run the page-specific input function.
        sCrashScreenPages[sCrashPage].inputFunc();
    }
}

void draw_controls_box(void) {
    crash_screen_draw_dark_rect((CRASH_SCREEN_X1 + (TEXT_WIDTH(1) / 2)), (CRASH_SCREEN_Y1 + (TEXT_HEIGHT(1) / 2)),
                                (CRASH_SCREEN_W  -  TEXT_WIDTH(1)     ), (CRASH_SCREEN_H  -  TEXT_HEIGHT(1)     ),
                                3);
    crash_screen_print(TEXT_X(1), TEXT_Y(1), "@%08X%s PAGE CONTROLS", COLOR_RGBA32_CRASH_PAGE_NAME, sPageNames[sCrashPage]);

    const enum ControlTypes *list = sCrashScreenPages[sCrashPage].pageControlsList;
    const struct ControlType *desc = NULL;

    u32 line = 3;

    while (*list != CONT_DESC_LIST_END) {
        desc = &sControlsDescriptions[*list++];
        line += crash_screen_print(TEXT_X(2), TEXT_Y(line), "%s:\n @%08X%s", desc->control, COLOR_RGBA32_CRASH_CONTROLS, desc->description);
    }

    osWritebackDCacheAll();
}

void draw_crash_screen(OSThread *thread) {
    if (sUpdateBuffer) {
        crash_screen_reset_framebuffer(sDrawBackground);

        if (sDrawCrashScreen) {
            if (sDrawBackground) {
                // Draw the transparent background.
                crash_screen_draw_dark_rect(CRASH_SCREEN_X1, CRASH_SCREEN_Y1, CRASH_SCREEN_W, CRASH_SCREEN_H, 2);
            }

            // Draw the header.
            u32 line = 0;
            crash_screen_print(TEXT_X(0), TEXT_Y(line), "@%08XHackerSM64 v%s", COLOR_RGBA32_CRASH_HEADER, HACKERSM64_VERSION);
            crash_screen_print(TEXT_X(19), TEXT_Y(line), "@%08XSTART:controls", COLOR_RGBA32_CRASH_HEADER);
            line += crash_screen_print(TEXT_X(35), TEXT_Y(line), "@%08X<Page:%02d>", COLOR_RGBA32_CRASH_HEADER, (sCrashPage + 1));
            crash_screen_draw_divider(DIVIDER_Y(line));

            // Run the page-specific draw function.
            sCrashScreenPages[sCrashPage].drawFunc(thread);

            if (sDrawControls) {
                draw_controls_box();
            }
        }

        crash_screen_update_framebuffer();

        if (gCrashScreenQueueFramebufferUpdate)  {
            gCrashScreenQueueFramebufferUpdate = FALSE;
        } else {
            sUpdateBuffer = FALSE;
        }
    }
}

OSThread *get_crashed_thread(void) {
    OSThread *thread = __osGetCurrFaultedThread();

    while (thread && thread->priority != -1) {
        if (thread->priority >  OS_PRIORITY_IDLE
         && thread->priority <= OS_PRIORITY_APPMAX
         && (thread->flags & (BIT(0) | BIT(1)))) {
            return thread;
        }

        thread = thread->tlnext;
    }

    return NULL;
}

#ifdef INCLUDE_DEBUG_MAP
void fill_function_stack_trace(OSThread *thread) {
    __OSThreadContext *tc = &thread->context;
    uintptr_t temp_sp = (tc->sp + 0x14);
    struct FunctionInStack *function = NULL;
    char *fname;

    // Fill the stack buffer.
    for (u32 i = 0; i < STACK_SIZE; i++) {
        fname = find_function_in_stack(&temp_sp);

        function = &sAllFunctionStack[i];
        function->addr = temp_sp;
        function->name = fname;

        if (fname != NULL) {
            function = &sKnownFunctionStack[sNumKnownFunctions++];
            function->addr = temp_sp;
            function->name = fname;
        }
    }
}
#endif

#ifdef FUNNY_CRASH_SOUND
extern void audio_signal_game_loop_tick(void);
extern void stop_sounds_in_continuous_banks(void);
extern struct SequenceQueueItem sBackgroundMusicQueue[6];

void play_crash_sound(struct CrashScreen *crashScreen, s32 sound) {
    crashScreen->thread.priority = 15;
    stop_sounds_in_continuous_banks();
    stop_background_music(sBackgroundMusicQueue[0].seqId);
    audio_signal_game_loop_tick();
    crash_screen_sleep(200);
    play_sound(sound, gGlobalSoundSource);
    audio_signal_game_loop_tick();
    crash_screen_sleep(200);
}
#endif

extern void read_controller_inputs(s32 threadID);

#ifdef CRASH_SCREEN_CRASH_SCREEN
void thread20_crash_screen_crash_screen(UNUSED void *arg) {
    OSMesg mesg;
    OSThread *thread = NULL;

    osSetEventMesg(OS_EVENT_CPU_BREAK, &gCrashScreen2.mesgQueue, (OSMesg)CRASH_SCREEN_MSG_CPU_BREAK);
    osSetEventMesg(OS_EVENT_FAULT,     &gCrashScreen2.mesgQueue, (OSMesg)CRASH_SCREEN_MSG_FAULT);

    while (TRUE) {
        if (thread == NULL) {
            osRecvMesg(&gCrashScreen2.mesgQueue, &mesg, OS_MESG_BLOCK);
            thread = get_crashed_thread();
            if (thread) {
 #ifdef FUNNY_CRASH_SOUND
                play_crash_sound(&gCrashScreen2, SOUND_MARIO_MAMA_MIA);
 #endif
                crash_screen_reset_framebuffer(FALSE);
                draw_crashed_image_i4();
                draw_crash_context(thread);

                osWritebackDCacheAll();
                osViBlack(FALSE);
                osViSwapBuffer((void *) PHYSICAL_TO_VIRTUAL(gFramebuffers[sRenderingFramebuffer]));
            }
        }
    }
}

void crash_screen_crash_screen_init(void) {
    osCreateMesgQueue(&gCrashScreen2.mesgQueue, &gCrashScreen2.mesg, 1);
    osCreateThread(&gCrashScreen2.thread, THREAD_20_CRASH_SCREEN_CRASH_SCREEN, thread20_crash_screen_crash_screen, NULL,
                ((u8 *) gCrashScreen2.stack + sizeof(gCrashScreen2.stack)),
                OS_PRIORITY_APPMAX);
    osStartThread(&gCrashScreen2.thread);
}
#endif // CRASH_SCREEN_CRASH_SCREEN

void thread2_crash_screen(UNUSED void *arg) {
    OSMesg mesg;
    OSThread *thread = NULL;

    osSetEventMesg(OS_EVENT_CPU_BREAK, &gCrashScreen.mesgQueue, (OSMesg)CRASH_SCREEN_MSG_CPU_BREAK);
    osSetEventMesg(OS_EVENT_FAULT,     &gCrashScreen.mesgQueue, (OSMesg)CRASH_SCREEN_MSG_FAULT);

    while (TRUE) {
        if (thread == NULL) {
            osRecvMesg(&gCrashScreen.mesgQueue, &mesg, OS_MESG_BLOCK);

            osViSetEvent(&gCrashScreen.mesgQueue, (OSMesg)CRASH_SCREEN_MSG_VI_VBLANK, 1);

            // Save a screenshot of the game to the Z buffer's memory space.
            crash_screen_take_screenshot(gZBuffer);

            thread = get_crashed_thread();
            if (thread) {
#ifdef FUNNY_CRASH_SOUND
                play_crash_sound(&gCrashScreen, SOUND_MARIO_WAAAOOOW);
#endif
                __OSThreadContext *tc = &thread->context;
                // Default to the assert page if the crash was caused by an assert.
                if (tc->cause == EXC_SYSCALL) {
                    sCrashPage = PAGE_ASSERTS;
                }
                // If a position was specified, use that.
                if (gCrashAddress != 0x0) {
                    sCrashPage = PAGE_RAM_VIEWER;
                    tc->pc = gCrashAddress;
                }
                sSelectedAddress = tc->pc;
#ifdef INCLUDE_DEBUG_MAP
                map_data_init();
                fill_function_stack_trace(thread);
#endif
#ifdef CRASH_SCREEN_CRASH_SCREEN
                crash_screen_crash_screen_init();
#endif
            }
        } else {
            if (gControllerBits) {
#if ENABLE_RUMBLE
                block_until_rumble_pak_free();
#endif
                osContStartReadDataEx(&gSIEventMesgQueue);
            }
            read_controller_inputs(THREAD_2_CRASH_SCREEN);
            update_crash_screen_input();
            draw_crash_screen(thread);
            sCrashScreenSwitchedPage = FALSE;
        }
    }
}

void crash_screen_init(void) {
    osCreateMesgQueue(&gCrashScreen.mesgQueue, &gCrashScreen.mesg, 1);
    osCreateThread(&gCrashScreen.thread, THREAD_2_CRASH_SCREEN, thread2_crash_screen, NULL,
                   ((u8 *) gCrashScreen.stack + sizeof(gCrashScreen.stack)),
                   OS_PRIORITY_APPMAX);
    osStartThread(&gCrashScreen.thread);
}
