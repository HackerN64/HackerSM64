#include <ultra64.h>
#include <PR/os_internal_error.h>
#include <stdarg.h>
#include <string.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen.h"
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

// Crash screen font. Each row of the image fits in one u32 pointer.
ALIGNED32 static const FontRow gCrashScreenFont[CRASH_SCREEN_FONT_CHAR_HEIGHT * CRASH_SCREEN_FONT_NUM_ROWS] = {
    #include "textures/crash_screen/crash_screen_font.custom.ia1.inc.c"
};

#ifdef INCLUDE_DEBUG_MAP
static struct BranchArrow sBranchArrows[DISASM_BRANCH_BUFFER_SIZE];
static u32 sNumBranchArrows = 0;
#endif

static struct FunctionInStack sAllFunctionStack[STACK_SIZE];
static struct FunctionInStack sKnownFunctionStack[STACK_SIZE];
static u32 sNumKnownFunctions = 0;
static u32 sNumShownFunctions = STACK_SIZE;

static s8 sCrashScreenDirectionFlags = CRASH_SCREEN_INPUT_DIRECTION_FLAGS_NONE;

static s8 sDrawCrashScreen = TRUE;
static s8 sDrawBackground = TRUE;
static s8 sCrashScreenSwitchedPage = FALSE;
static s8 sCrashScreenWordWrap = TRUE;
static s8 sStackTraceShowNames = TRUE;
static s8 sStackTraceSkipUnknowns = FALSE;
static s8 sAddressSelectMenuOpen = FALSE;
static s8 sShowRamAsAscii = FALSE;
static s8 sAddressSelecCharIndex = 2;
static uintptr_t sAddressSelectTarget = 0;
static uintptr_t sSelectedAddress = 0;
static uintptr_t sScrollAddress = 0;
static u32 sStackTraceIndex = 0;

static u8 sCrashPage = PAGE_CONTEXT;
static u8 sUpdateBuffer = TRUE;


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

#ifdef INCLUDE_DEBUG_MAP
static const RGBA32 sBranchColors[] = {
    COLOR_RGBA32_ORANGE,
    COLOR_RGBA32_LIME,
    COLOR_RGBA32_CYAN,
    COLOR_RGBA32_MAGENTA,
    COLOR_RGBA32_YELLOW,
    COLOR_RGBA32_PINK,
    COLOR_RGBA32_LIGHT_GRAY
};
#endif

extern u64 osClockRate;

struct CrashScreen gCrashScreen;
#ifdef CRASH_SCREEN_CRASH_SCREEN
struct CrashScreen gCrashScreen2;
#endif

static ALWAYS_INLINE RGBA16 *crash_screen_get_framebuffer_pixel_ptr(u32 x, u32 y) {
    return (gFramebuffers[sRenderingFramebuffer] + (SCREEN_WIDTH * y) + x);
}

// Darkens a rectangular area. This is faster than the color blending done by
// crash_screen_draw_rect, so it's used for the large background rectangle.
// 0  - does nothing
// 1  - darken by 1/2
// 2  - darken by 3/4
// 3  - darken by 7/8
// 4  - darken by 15/16
// 5+ - darken to black
void crash_screen_draw_dark_rect(u32 startX, u32 startY, u32 w, u32 h, u32 darken) {
    if (darken == 0) {
        return;
    }

    const RGBA16Component componentMask = (MSK_RGBA16_C & ~BITMASK(darken));
    RGBA16 mask = 0;
    for (u32 i = SIZ_RGBA16_A; i < (SIZ_RGBA16_C * 3); i += SIZ_RGBA16_C) {
        mask |= (componentMask << i);
    }

    RGBA16 *dst = crash_screen_get_framebuffer_pixel_ptr(startX, startY);

    for (u32 y = 0; y < h; y++) {
        for (u32 x = 0; x < w; x++) {
            *dst = (((*dst & mask) >> darken) | MSK_RGBA16_A);
            dst++;
        }

        dst += (SCREEN_WIDTH - w);
    }
}

// Draws a rectangle.
void crash_screen_draw_rect(u32 startX, u32 startY, u32 w, u32 h, RGBA32 color) {
    const Alpha alpha = RGBA32_A(color);
    if (alpha == 0x00) {
        return;
    }
    const s8 opaque = (alpha == MSK_RGBA32_A);
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);

    RGBA16 *dst = crash_screen_get_framebuffer_pixel_ptr(startX, startY);

    for (u32 y = 0; y < h; y++) {
        for (u32 x = 0; x < w; x++) {
            if (opaque) {
                *dst = newColor;
            } else {
                *dst = rgba16_blend(*dst, newColor, alpha);
            }
            dst++;
        }

        dst += (SCREEN_WIDTH - w);
    }
}

// Draws a triangle pointing upwards or downwards.
void crash_screen_draw_vertical_triangle(u32 startX, u32 startY, u32 w, u32 h, RGBA32 color, s8 flip) {
    const Alpha alpha = RGBA32_A(color);
    if (alpha == 0x00) {
        return;
    }
    const s8 opaque = (alpha == MSK_RGBA32_A);
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);
    const f32 middle = (w / 2.0f);
    f32 d = 0.0f;
    f32 t = (middle / (f32) h);
    if (flip) {
        d = (middle - t);
        t = -t;
    }

    RGBA16 *dst = crash_screen_get_framebuffer_pixel_ptr(startX, startY);

    for (u32 y = 0; y < h; y++) {
        for (u32 x = 0; x < w; x++) {
            if (absf(middle - x) < d) {
                if (opaque) {
                    *dst = newColor;
                } else {
                    *dst = rgba16_blend(*dst, newColor, alpha);
                }
            }
            dst++;
        }

        d += t;
        dst += (SCREEN_WIDTH - w);
    }
}

// Draws a triangle pointing left or right.
void crash_screen_draw_horizontal_triangle(u32 startX, u32 startY, u32 w, u32 h, RGBA32 color, s8 flip) {
    const Alpha alpha = RGBA32_A(color);
    if (alpha == 0x00) {
        return;
    }
    const s8 opaque = (alpha == MSK_RGBA32_A);
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);
    const f32 middle = (h / 2.0f);
    const f32 t = ((f32) w / middle);
    f32 x1 = w;

    RGBA16 *dst = crash_screen_get_framebuffer_pixel_ptr(startX, startY);
    RGBA16 *start = dst;

    for (u32 y = 0; y < h; y++) {
        for (u32 x = x1; x < w; x++) {
            if (opaque) {
                *dst = newColor;
            } else {
                *dst = rgba16_blend(*dst, newColor, alpha);
            }
            dst++;
        }
        x1 -= (y < middle) ? t : -t;
        dst = start + (SCREEN_WIDTH * y);
        if (flip) {
            dst += (u32)x1;
        }
    }
}

// Draws a line from one point on the screen to another.
void crash_screen_draw_line(u32 x1, u32 y1, u32 x2, u32 y2, RGBA32 color) {
    const Alpha alpha = RGBA32_A(color);
    if (alpha == 0x00) {
        return;
    }
    const s8 opaque = (alpha == MSK_RGBA32_A);
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);

    RGBA16 *dst;

    if (x1 > x2) SWAP(x1, x2);
    if (y1 > y2) SWAP(y1, y2);

    const f32 slope = (f32)(y2 - y1) / (x2 - x1);

    f32 y, x = x1;
    while (x <= x2) {
        y = ((slope * (x - x1)) + y1);
        dst = crash_screen_get_framebuffer_pixel_ptr(x, y);
        if (opaque) {
            *dst = newColor;
        } else {
            *dst = rgba16_blend(*dst, newColor, alpha);
        }
        x++;
    }
}

ALWAYS_INLINE void crash_screen_draw_divider(u32 y) {
    crash_screen_draw_rect(CRASH_SCREEN_X1, y, CRASH_SCREEN_W, 1, COLOR_RGBA32_LIGHT_GRAY);
}

void crash_screen_draw_glyph(u32 startX, u32 startY, unsigned char glyph, RGBA32 color) {
    if (glyph == 0) {
        color = COLOR_RGBA32_GRAY;
    }
    const Alpha alpha = RGBA32_A(color);
    if (alpha == 0x00) {
        return;
    }
    const s8 opaque = (alpha == MSK_RGBA32_A);
    const RGBA16 newColor = RGBA32_TO_RGBA16(color);
    const FontRow startBit = ((FontRow)BIT(31) >> ((glyph % CRASH_SCREEN_FONT_CHARS_PER_ROW) * CRASH_SCREEN_FONT_CHAR_WIDTH));
    FontRow bit;
    FontRow rowMask;

    const FontRow *src = &gCrashScreenFont[(glyph / CRASH_SCREEN_FONT_CHARS_PER_ROW) * CRASH_SCREEN_FONT_CHAR_HEIGHT];
    RGBA16 *dst = crash_screen_get_framebuffer_pixel_ptr(startX, startY);

    for (u32 y = 0; y < CRASH_SCREEN_FONT_CHAR_HEIGHT; y++) {
        bit = startBit;
        rowMask = *src++;

        for (u32 x = 0; x < CRASH_SCREEN_FONT_CHAR_WIDTH; x++) {
            if (bit & rowMask) {
                if (opaque) {
                    *dst = newColor;
                } else {
                    *dst = rgba16_blend(*dst, newColor, alpha);
                }
            }
            dst++;
            bit >>= 1;
        }

        dst += (SCREEN_WIDTH - CRASH_SCREEN_FONT_CHAR_WIDTH);
    }
}

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

static char *write_to_buf(char *buffer, const char *data, size_t size) {
    return ((char *) memcpy(buffer, data, size) + size);
}

static s32 glyph_to_hex(char *dest, unsigned char glyph) {
    if (glyph >= '0' && glyph <= '9') {
        *dest = ((glyph - '0') & 0xF);
    } else if (glyph >= 'A' && glyph <= 'F') {
        *dest = (((glyph - 'A') + 10) & 0xF);
    } else if (glyph >= 'a' && glyph <= 'f') {
        *dest = (((glyph - 'a') + 10) & 0xF);
    } else {
        return FALSE;
    }

    return TRUE;
}

s32 crash_screen_parse_text_color(RGBA32 *color, const char *buf, u32 index, u32 size) {
    u32 byteIndex, digit;
    char hex = 0;
    Color component = 0;
    ColorRGBA rgba = { 0, 0, 0, 0 };
    *color = COLOR_RGBA32_WHITE;

    for (byteIndex = 0; byteIndex < sizeof(RGBA32); byteIndex++) {
        for (digit = 0; digit < 2; digit++) {
            if (index > size) {
                return FALSE;
            }

            if (!glyph_to_hex(&hex, buf[index])) {
                return FALSE;
            }

            if (!buf[++index]) {
                return FALSE;
            }

            component |= (hex << ((1 - digit) * sizeof(RGBA32)));
        }

        rgba[byteIndex] = component;
        component = 0;
    }

    *color = COLORRGBA_TO_RGBA32(rgba);

    return TRUE;
}

// Returns the number of chars to skip.
u32 crash_screen_parse_formatting_chars(const char *buf, u32 index, u32 size, unsigned char glyph, RGBA32 *color) {
    static u8 escape = FALSE;
    u32 skip = 0;

    if ((index + (1 + 8)) > size) {
        return 0;
    }

    // Use '\\@' to print '@' and its color code instead of trying to parse it.
    if (glyph == '\\' && buf[index + 1] && buf[index + 1] == '@') {
        escape = TRUE;
        skip = 1;
    } else if (glyph == '@' && !escape) { // @RRGGBBAA color prefix
        if (crash_screen_parse_text_color(color, buf, (index + 1), size)) {
            skip = 8;
        }
    }

    if (skip <= 0) {
        escape = FALSE;
    }

    return skip;
}

// Returns whether to wrap or not.
s32 crash_screen_parse_space(const char *buf, u32 index, u32 size, u32 x) {
    unsigned char glyph = buf[++index];
    u32 checkX = x + TEXT_WIDTH(1);
    UNUSED RGBA32 color;

    while (buf[index] && glyph != ' ' && index < size) { // check the next word after the space
        // New line if the next word is larger than the writable space.
        if ((index + 1) < size && checkX >= CRASH_SCREEN_TEXT_X2) {
            return TRUE;
        }

        index += (1 + crash_screen_parse_formatting_chars(buf, index, size, glyph, &color));
        glyph = buf[index];
        checkX += TEXT_WIDTH(1);
    }

    return FALSE;
}

enum CrashScreenPrintOp {
    CRASH_SCREEN_PRINT_OP_SPACE,
    CRASH_SCREEN_PRINT_OP_SKIP,
    CRASH_SCREEN_PRINT_OP_NEWLINE,
};

u32 crash_screen_print(u32 startX, u32 startY, const char *fmt, ...) {
    unsigned char glyph;
    char buf[CHAR_BUFFER_SIZE];
    bzero(&buf, sizeof(buf));

    va_list args;
    va_start(args, fmt);

    u32 size = _Printf(write_to_buf, buf, fmt, args);

    RGBA32 color = COLOR_RGBA32_WHITE;

    u32 x = startX;
    u32 y = startY;

    enum CrashScreenPrintOp printOp = CRASH_SCREEN_PRINT_OP_SPACE;
    u32 skip = 0;
    u32 numLines = 1;

    s8 wrap = sCrashScreenWordWrap;

    if (size > 0) {
        for (u32 index = 0; index < size && buf[index]; index += (1 + skip)) {
            glyph = buf[index];
            printOp = CRASH_SCREEN_PRINT_OP_SPACE;
            skip = 0;

            if (glyph == ' ') { // space
                if (wrap && crash_screen_parse_space(buf, index, size, x)) {
                    printOp = CRASH_SCREEN_PRINT_OP_NEWLINE;
                }
            } else if (glyph == '\r' || glyph == '\n') { // new line
                printOp = CRASH_SCREEN_PRINT_OP_NEWLINE;
            } else {
                // Check for formatting codes
                skip = crash_screen_parse_formatting_chars(buf, index, size, glyph, &color);

                if (skip > 0) {
                    printOp = CRASH_SCREEN_PRINT_OP_SKIP;
                } else { // normal char
                    crash_screen_draw_glyph(x, y, glyph, color);

                    if (wrap && (index + 1) < size && (x + TEXT_WIDTH(1)) >= CRASH_SCREEN_TEXT_X2) {
                        printOp = CRASH_SCREEN_PRINT_OP_NEWLINE;
                    }
                }
            }

            if (printOp == CRASH_SCREEN_PRINT_OP_SPACE) {
                x += TEXT_WIDTH(1);
            } else if (printOp == CRASH_SCREEN_PRINT_OP_NEWLINE) {
                x = startX;
                y += TEXT_HEIGHT(1);
                numLines++;
            }
        }
    }

    va_end(args);

    return numLines;
}

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

void crash_screen_print_fpcsr(uintptr_t fpcsr) {
    uintptr_t bit = BIT(17);

    crash_screen_print(TEXT_X(0), (TEXT_Y(14) + 5), "@%08X%s:", COLOR_RGBA32_CRASH_REGISTER, "FPCSR");
    crash_screen_print(TEXT_X(6), (TEXT_Y(14) + 5), "%08X", fpcsr);

    for (u32 i = 0; i < 6; i++) {
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

    s32 cause = ((tc->cause >> 2) & BITMASK(5));
    // Make the last two cause case indexes sequential for array access.
    if (cause == (EXC_WATCH >> 2)) cause = 16;
    if (cause == (EXC_VCED  >> 2)) cause = 17;

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
        crash_screen_print(TEXT_X(10), TEXT_Y(line), "@%08X%s", COLOR_RGBA32_CRASH_FUNCTION_NAME, fname);
    }
#endif

    crash_screen_print_registers(tc);

    osWritebackDCacheAll();

    crash_screen_print_float_registers(tc);
}

void draw_assert(UNUSED OSThread *thread) {
    u32 line = 1;

    line += crash_screen_print(TEXT_X(0), TEXT_Y(line), "@%08XASSERT", COLOR_RGBA32_CRASH_PAGE_NAME);

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

    crash_screen_print(TEXT_X(0), TEXT_Y(1), "@%08XLOG", COLOR_RGBA32_CRASH_PAGE_NAME);
    crash_screen_draw_divider(DIVIDER_Y(2));

    osWritebackDCacheAll();

    for (i = 0; i < LOG_BUFFER_SIZE; i++) {
        crash_screen_print(TEXT_X(0), TEXT_Y(1 + (LOG_BUFFER_SIZE - i)), consoleLogTable[i]);
    }
}
#endif

// prints any function pointers it finds in the stack format:
// SP address: function name
void draw_stacktrace(OSThread *thread) {
    __OSThreadContext *tc = &thread->context;
    uintptr_t temp_sp = (tc->sp + 0x14);

    u32 line = 1;

    crash_screen_print(TEXT_X(0), TEXT_Y(line), "@%08XSTACK TRACE", COLOR_RGBA32_CRASH_PAGE_NAME);
    line += crash_screen_print(TEXT_X(12), TEXT_Y(line), "FROM %08X", temp_sp);
    crash_screen_draw_divider(DIVIDER_Y(line));

#ifdef INCLUDE_DEBUG_MAP
    crash_screen_print(TEXT_X(0), TEXT_Y(line), "@%08XCURRFUNC:", COLOR_RGBA32_CRASH_AT);
    uintptr_t pc = tc->pc;
    line += crash_screen_print(TEXT_X(9), TEXT_Y(line), "@%08X%s", COLOR_RGBA32_CRASH_FUNCTION_NAME, parse_map(&pc));

    crash_screen_draw_divider(DIVIDER_Y(line));

    osWritebackDCacheAll();

    struct FunctionInStack *functionList = (sStackTraceSkipUnknowns ? sKnownFunctionStack : sAllFunctionStack);
    struct FunctionInStack *function = NULL;

    // Print
    for (u32 j = 0; j < STACK_TRACE_NUM_ROWS; j++) {
        u32 y = TEXT_Y(line + j);

        u32 currIndex = (sStackTraceIndex + j);

        if (currIndex >= sNumShownFunctions) {
            break;
        }

        function = &functionList[currIndex];

        if (function != NULL) {
            uintptr_t faddr = function->addr;
            char *fname = function->name;

            crash_screen_print(TEXT_X(0), y, "%08X:", faddr);

            if (!sStackTraceSkipUnknowns && ((fname == NULL) || ((*(uintptr_t*)faddr & 0x80000000) == 0))) {
                // Print unknown function
                crash_screen_print(TEXT_X(9), y, "@%08X%08X", COLOR_RGBA32_CRASH_UNKNOWN, *(uintptr_t*)faddr);
            } else {
                // Print known function
                if (sStackTraceShowNames) {
                    crash_screen_print(TEXT_X(9), y, "@%08X%s", COLOR_RGBA32_CRASH_FUNCTION_NAME_2, fname);
                } else {
                    crash_screen_print(TEXT_X(9), y, "@%08x%08X", COLOR_RGBA32_CRASH_FUNCTION_NAME_2, *(uintptr_t*)faddr);
                }
            }
        }
    }

    osWritebackDCacheAll();

    crash_screen_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y - 1));
    crash_screen_print(TEXT_X(0), TEXT_Y(CRASH_SCREEN_NUM_CHARS_Y - 1), "@%08Xup/down:scroll    toggle: a:names b:unknowns", COLOR_RGBA32_CRASH_CONTROLS);

    // Scroll Bar
    crash_screen_draw_scroll_bar(DIVIDER_Y(3), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y - 1), STACK_TRACE_NUM_ROWS, sNumShownFunctions, sStackTraceIndex, 4, COLOR_RGBA32_LIGHT_GRAY);
#else
    osWritebackDCacheAll();

    crash_screen_print(TEXT_X(0), TEXT_Y(line), "STACK TRACE DISABLED");
#endif

    osWritebackDCacheAll();
}

void draw_address_select(void) {
    crash_screen_draw_rect((JUMP_MENU_X -  JUMP_MENU_MARGIN_X     ), (JUMP_MENU_Y - TEXT_HEIGHT(2) -  JUMP_MENU_MARGIN_X     ),
                           (JUMP_MENU_W + (JUMP_MENU_MARGIN_Y * 2)), (JUMP_MENU_H + TEXT_HEIGHT(2) + (JUMP_MENU_MARGIN_Y * 2)),
                           COLOR_RGBA32_CRASH_BACKGROUND);

    crash_screen_print(JUMP_MENU_X + TEXT_WIDTH(1), JUMP_MENU_Y - TEXT_HEIGHT(2), "GO TO:");

    crash_screen_draw_vertical_triangle((JUMP_MENU_X + (sAddressSelecCharIndex * TEXT_WIDTH(1)) - 1), (JUMP_MENU_Y - TEXT_HEIGHT(1) + CRASH_SCREEN_CHAR_SPACING_Y),
                                        TEXT_WIDTH(1), TEXT_WIDTH(1),
                                        COLOR_RGBA32_CRASH_SELECT_ARROWS, FALSE);
    crash_screen_draw_vertical_triangle((JUMP_MENU_X + (sAddressSelecCharIndex * TEXT_WIDTH(1)) - 1), (JUMP_MENU_Y + TEXT_HEIGHT(1) - CRASH_SCREEN_CHAR_SPACING_Y + 1),
                                        TEXT_WIDTH(1), TEXT_WIDTH(1),
                                        COLOR_RGBA32_CRASH_SELECT_ARROWS, TRUE);
    crash_screen_print(JUMP_MENU_X, JUMP_MENU_Y, "%08X", sAddressSelectTarget);

    osWritebackDCacheAll();
}

void clamp_view_to_selection(const u32 numRows, const u32 step) {
    size_t size = (step * numRows);
    if (sSelectedAddress < sScrollAddress + (step - 1)) {
        sScrollAddress = sSelectedAddress - (step - 1);
    }
    if (sSelectedAddress > sScrollAddress + (size - 1)) {
        sScrollAddress = sSelectedAddress - (size - 1);
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

    crash_screen_print(TEXT_X(0), TEXT_Y(line), "@%08XRAM VIEW", COLOR_RGBA32_CRASH_PAGE_NAME);
    line += crash_screen_print(TEXT_X(9), TEXT_Y(line), "%08X in %08X-%08X", sSelectedAddress, startAddr, (startAddr + RAM_VIEWER_SHOWN_SECTION));
    crash_screen_draw_divider(DIVIDER_Y(line));

    charX = (TEXT_X(8) + 3);

    for (u32 i = 0; i < 16; i++) {
        if ((i & 0x3) == 0) {
            charX += 2;
        }

        crash_screen_print(charX, TEXT_Y(line), "@%08X%02X", ((i & 0x1) ? COLOR_RGBA32_CRASH_RAM_VIEW_H1 : COLOR_RGBA32_CRASH_RAM_VIEW_H2), i);

        charX += (TEXT_WIDTH(2) + 1);
    }

    crash_screen_draw_divider(DIVIDER_Y(3));

    crash_screen_draw_rect((TEXT_X(8) + 2), DIVIDER_Y(line), 1, TEXT_HEIGHT(19), COLOR_RGBA32_LIGHT_GRAY);

    line += crash_screen_print(TEXT_X(1), TEXT_Y(line), "MEMORY");

    charX = (TEXT_X(8) + 3);
    charY = TEXT_Y(line);

    RGBA32 color;

    for (u32 y = 0; y < RAM_VIEWER_NUM_ROWS; y++) {
        uintptr_t rowAddr = startAddr + (y * RAM_VIEWER_STEP);
        crash_screen_print(TEXT_X(0), TEXT_Y(line + y), "@%08X%08X", ((y & 0x1) ? COLOR_RGBA32_CRASH_RAM_VIEW_B1 : COLOR_RGBA32_CRASH_RAM_VIEW_B2), rowAddr);

        charX = (TEXT_X(8) + 3);
        charY = TEXT_Y(line + y);
        for (u32 x = 0; x < 16; x++) {
            uintptr_t currAddr = (rowAddr + x);
            u8 value = *((u8 *)currAddr);

            if ((x & 0x3) == 0) {
                charX += 2;
            }

            color = ((x & 0x1) ? COLOR_RGBA32_WHITE : COLOR_RGBA32_LIGHT_GRAY);
            
            if (currAddr == tc->pc) {
                crash_screen_draw_rect(charX - 1, charY - 1, TEXT_WIDTH(2) + 1, TEXT_WIDTH(1) + 3, COLOR_RGBA32_RED);
            }
            if (currAddr == sSelectedAddress) {
                crash_screen_draw_rect(charX - 1, charY - 1, TEXT_WIDTH(2) + 1, TEXT_WIDTH(1) + 3, COLOR_RGBA32_WHITE);
                color = COLOR_RGBA32_BLACK;
            }

            if (sShowRamAsAscii) {
                crash_screen_draw_glyph(charX + TEXT_WIDTH(1), charY, value, COLOR_RGBA32_WHITE);
            } else {
                crash_screen_print(charX, charY, "@%08X%02X", color, value);
            }

            charX += (TEXT_WIDTH(2) + 1);
        }
    }

    u32 line2 = (line + RAM_VIEWER_NUM_ROWS);

    crash_screen_draw_divider(DIVIDER_Y(line2));
    crash_screen_print(TEXT_X(0), TEXT_Y(line2), "@%08Xup/down:scroll        a:jump  b:toggle ascii", COLOR_RGBA32_CRASH_CONTROLS);

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
    u32 curBranchX = DISASM_BRANCH_ARROW_OFFSET;
    s16 branchOffset;
    s16 curBranchColorIndex = 0;

    if (fname == NULL) {
        return;
    }

    bzero(sBranchArrows, sizeof(sBranchArrows));

    sNumBranchArrows = 0;

    if (fname != NULL) {
        for (u32 i = 0; i < DISASM_FUNCTION_SEARCH_MAX_OFFSET; i += DISASM_STEP) {
            addr = (funcAddr + i);
            checkAddr = addr;
            if (fname != parse_map(&checkAddr)) {
                break;
            }
            toDisasm.d = *(uintptr_t*)addr;

            branchOffset = get_branch_offset(toDisasm);

            if (branchOffset != 0) {
                currArrow = &sBranchArrows[sNumBranchArrows++];
                currArrow->startAddr = addr;
                currArrow->branchOffset = branchOffset;
                currArrow->colorIndex = curBranchColorIndex;
                currArrow->xPos = curBranchX;

                curBranchX += DISASM_BRANCH_ARROW_SPACING;
                curBranchColorIndex = ((curBranchColorIndex + 1) % ARRAY_COUNT(sBranchColors));

                if (DISASM_BRANCH_ARROW_START_X + curBranchX > CRASH_SCREEN_TEXT_X2) {
                    curBranchX = DISASM_BRANCH_ARROW_OFFSET;
                }
            }
        }
    }
}

void draw_disasm_branch_arrow(s32 startLine, s32 endLine, s32 dist, RGBA32 color, s32 printLine) {
    s32 arrowStartHeight = (TEXT_Y(printLine + startLine) + 3);
    s32 arrowEndHeight   = (TEXT_Y(printLine +   endLine) + 3);

    if (startLine < 0) {
        arrowStartHeight = TEXT_Y(printLine);
    } else if (startLine >= DISASM_NUM_ROWS) {
        arrowStartHeight = (TEXT_Y(printLine + DISASM_NUM_ROWS) - 3);
    } else {
        crash_screen_draw_rect((DISASM_BRANCH_ARROW_START_X + 1), arrowStartHeight, dist, 1, color);
    }

    if (endLine < 0) {
        arrowEndHeight = TEXT_Y(printLine);
    } else if (endLine >= DISASM_NUM_ROWS) {
        arrowEndHeight = (TEXT_Y(printLine + DISASM_NUM_ROWS) - 3);
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
#endif

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

    crash_screen_print(TEXT_X(0), TEXT_Y(line), "@%08XDISASM", COLOR_RGBA32_CRASH_PAGE_NAME);
    line += crash_screen_print(TEXT_X(7), TEXT_Y(line), "%08X in %08X-%08X", alignedSelectedAddr, sScrollAddress, (sScrollAddress + DISASM_SHOWN_SECTION));
    crash_screen_draw_divider(DIVIDER_Y(line));

    if (((fname == NULL)/* || ((*(uintptr_t*)funcAddr & 0x80000000) == 0)*/)) {
        line += crash_screen_print(TEXT_X(0), TEXT_Y(line), "NOT IN A FUNCTION");
    } else {
        line += crash_screen_print(TEXT_X(0), TEXT_Y(line), "IN: @%08X%s", COLOR_RGBA32_CRASH_FUNCTION_NAME, fname);
    }

    osWritebackDCacheAll();

#ifdef INCLUDE_DEBUG_MAP
    // Draw branch arrows from the buffer.
    struct BranchArrow *currArrow = NULL;
    for (u32 b = 0; b < sNumBranchArrows; b++) {
        currArrow = &sBranchArrows[b];
        s32 startLine = (((s32)currArrow->startAddr - (s32)sScrollAddress) / DISASM_STEP);
        s32 endLine = (startLine + currArrow->branchOffset + 1);
        if (((startLine >= 0)               || (endLine >= 0))
         && ((startLine <  DISASM_NUM_ROWS) || (endLine <  DISASM_NUM_ROWS))) {
            draw_disasm_branch_arrow(startLine, endLine, currArrow->xPos, sBranchColors[currArrow->colorIndex], line);
        }
    }

    osWritebackDCacheAll();
#endif

    sCrashScreenWordWrap = FALSE;

    u32 charX = TEXT_X(0);
    u32 charY = TEXT_Y(line);

    for (u32 y = 0; y < DISASM_NUM_ROWS; y++) {
        uintptr_t addr = (sScrollAddress + (y * DISASM_STEP));
        InsnData toDisasm;
        toDisasm.d = *(uintptr_t*)(addr);

        charY = TEXT_Y(line + y);

        if (addr == tc->pc) {
            crash_screen_draw_rect(charX - 1, charY - 2, CRASH_SCREEN_TEXT_W + 1, TEXT_HEIGHT(1) + 1, COLOR_RGBA32_CRASH_PC);
        }
        if (addr == alignedSelectedAddr) {
            crash_screen_draw_rect(charX - 1, charY - 2, CRASH_SCREEN_TEXT_W + 1, TEXT_HEIGHT(1) + 1, COLOR_RGBA32_CRASH_SELECT);
        }

        if (is_in_code_segment(addr)) {
            crash_screen_print(charX, charY, "%s", insn_disasm(toDisasm, (addr == tc->pc)));
        } else if (sShowRamAsAscii) {
            char asText[8];
            bzero(asText, sizeof(asText));

            for (u32 c = 0; c < 4; c++) {
                asText[c] = (unsigned char)(toDisasm.d >> (24 - (8 * c)));
            }

            crash_screen_print(charX, charY, "%s", asText);
        } else {
            crash_screen_print(charX, charY, "%08X", toDisasm.d);
        }
    }

    sCrashScreenWordWrap = TRUE;

    osWritebackDCacheAll();

    crash_screen_draw_divider(DIVIDER_Y(line));

    u32 line2 = (line + DISASM_NUM_ROWS);

    crash_screen_draw_divider(DIVIDER_Y(line2));
    crash_screen_print(TEXT_X(0), TEXT_Y(line2), "@%08Xup/down:scroll        a:jump  b:toggle ascii", COLOR_RGBA32_CRASH_CONTROLS);

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

void draw_controls(UNUSED OSThread *thread) {
    u32 line = 1;

    line++;
    line += crash_screen_print(TEXT_X(1), TEXT_Y(line), "CRASH SCREEN CONTROLS");
    line++;
    line += crash_screen_print(TEXT_X(2), TEXT_Y(line), "START:");
    line += crash_screen_print(TEXT_X(2), TEXT_Y(line), "@%08Xtoggle framebuffer background", COLOR_RGBA32_CRASH_CONTROLS);
    line++;
    line += crash_screen_print(TEXT_X(2), TEXT_Y(line), "Z:");
    line += crash_screen_print(TEXT_X(2), TEXT_Y(line), "@%08Xtoggle framebuffer only view", COLOR_RGBA32_CRASH_CONTROLS);
    line++;
    line += crash_screen_print(TEXT_X(2), TEXT_Y(line), "ANALOG STICK, D-PAD, OR C BUTTONS:");
    line++;
    line += crash_screen_print(TEXT_X(3), TEXT_Y(line), "LEFT/RIGHT:");
    line += crash_screen_print(TEXT_X(3), TEXT_Y(line), "@%08Xswitch page", COLOR_RGBA32_CRASH_CONTROLS);
    line++;
    line += crash_screen_print(TEXT_X(3), TEXT_Y(line), "UP/DOWN:");
    line += crash_screen_print(TEXT_X(3), TEXT_Y(line), "@%08Xscroll page", COLOR_RGBA32_CRASH_CONTROLS);

    osWritebackDCacheAll();
}

void reset_crash_screen_framebuffer(void) {
    if (sDrawBackground) {
        bcopy(gZBuffer, (void *) PHYSICAL_TO_VIRTUAL(gFramebuffers[sRenderingFramebuffer]), FRAMEBUFFER_SIZE);
    } else {
        crash_screen_draw_rect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, COLOR_RGBA32_BLACK);
    }

    osWritebackDCacheAll();
}

void update_crash_screen_framebuffer(void) {
    osWritebackDCacheAll();

    osViBlack(FALSE);
    osRecvMesg(&gCrashScreen.mesgQueue, &gCrashScreen.mesg, OS_MESG_BLOCK);
    osViSwapBuffer((void *) PHYSICAL_TO_VIRTUAL(gFramebuffers[sRenderingFramebuffer]));
    osRecvMesg(&gCrashScreen.mesgQueue, &gCrashScreen.mesg, OS_MESG_BLOCK);

    if (++sRenderingFramebuffer == 3) {
        sRenderingFramebuffer = 0;
    }

    osWritebackDCacheAll();
}

void update_crash_screen_direction_input(void) {
    u8 prevHeld = (sCrashScreenDirectionFlags & BITMASK(4));
    u8 currHeld = (
        (((gPlayer1Controller->buttonDown & (U_CBUTTONS | U_JPAD))
         || (gPlayer1Controller->rawStickY >  60)) << 0) // CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_UP
      | (((gPlayer1Controller->buttonDown & (D_CBUTTONS | D_JPAD))
         || (gPlayer1Controller->rawStickY < -60)) << 1) // CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_DOWN
      | (((gPlayer1Controller->buttonDown & (L_CBUTTONS | L_JPAD))
         || (gPlayer1Controller->rawStickX < -60)) << 2) // CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_LEFT
      | (((gPlayer1Controller->buttonDown & (R_CBUTTONS | R_JPAD))
         || (gPlayer1Controller->rawStickX >  60)) << 3) // CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_RIGHT
    );

    u8 pressed = (~prevHeld & currHeld);
    sCrashScreenDirectionFlags = ((pressed << 4) | currHeld);
}

s32 update_crash_screen_page(void) {
    u8 prevPage = sCrashPage;

    // if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_RIGHT) {
    if (gPlayer1Controller->buttonPressed & R_TRIG) {
        // Next page.
        sCrashPage++;
        sUpdateBuffer = TRUE;
    }
    // if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_LEFT) {
    if (gPlayer1Controller->buttonPressed & L_TRIG) {
        // Previous Page.
        sCrashPage--;
        sUpdateBuffer = TRUE;
    }

    if (sCrashPage != prevPage) {
        // Wrap pages.
        if ((sCrashPage >= PAGE_COUNT) && (sCrashPage != PAGES_MAX)) {
            sCrashPage = PAGE_CONTEXT;
        }
        if (sCrashPage == PAGES_MAX) {
            sCrashPage = (PAGE_COUNT - 1);
        }

        // Reset certain values when the page is changed.
        sStackTraceIndex = 0;
        sCrashScreenSwitchedPage = TRUE;

        return TRUE;
    }

    return FALSE;
}

void crash_screen_input_default(void) {
    update_crash_screen_page();
}

void crash_screen_input_stacktrace(void) {
    if (!update_crash_screen_page()) {
        if (gPlayer1Controller->buttonPressed & A_BUTTON) {
            // Toggle whether to display function names.
            sStackTraceShowNames ^= TRUE;
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
    }
}

void crash_screen_select_address(size_t step) {
    if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_LEFT) {
        sAddressSelecCharIndex = ((sAddressSelecCharIndex - 1) & 0x7);
        sUpdateBuffer = TRUE;
    }
    if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_RIGHT) {
        sAddressSelecCharIndex = ((sAddressSelecCharIndex + 1) & 0x7);
        sUpdateBuffer = TRUE;
    }

    uintptr_t nextSelectedAddress = sAddressSelectTarget;

    if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_UP) {
        // Increment the selected digit.
        u32 shift = (28 - (sAddressSelecCharIndex * 4));
        u8 new = ((sAddressSelectTarget >> shift) & BITMASK(4));
        new = ((new + 1) & BITMASK(4));
        nextSelectedAddress = ((sAddressSelectTarget & ~(BITMASK(4) << shift)) | (new << shift));

        if (nextSelectedAddress >= RAM_VIEWER_SCROLL_MIN && nextSelectedAddress <= RAM_VIEWER_SCROLL_MAX) {
            sAddressSelectTarget = ALIGN(nextSelectedAddress, step);
            sUpdateBuffer = TRUE;
        }
    }
    if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_DOWN) {
        // Decrement the selected digit.
        u32 shift = (28 - (sAddressSelecCharIndex * 4));
        u8 new = ((sAddressSelectTarget >> shift) & BITMASK(4));
        new = ((new - 1) & BITMASK(4));
        nextSelectedAddress = ((sAddressSelectTarget & ~(BITMASK(4) << shift)) | (new << shift));

        if (nextSelectedAddress >= RAM_VIEWER_SCROLL_MIN && nextSelectedAddress <= RAM_VIEWER_SCROLL_MAX) {
            sAddressSelectTarget = ALIGN(nextSelectedAddress, step);
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
        crash_screen_select_address(1);
    } else {
        if (!update_crash_screen_page()) {
            if ((sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_UP)
             && ((sSelectedAddress - RAM_VIEWER_STEP) >= RAM_START)) {
                sSelectedAddress -= RAM_VIEWER_STEP;
                sUpdateBuffer = TRUE;
            }
            if ((sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_DOWN)
             && ((sSelectedAddress + RAM_VIEWER_STEP) < RAM_END)) {
                sSelectedAddress += RAM_VIEWER_STEP;
                sUpdateBuffer = TRUE;
            }
            if ((sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_LEFT)
             && (((sSelectedAddress - 1) & 0xF) != 0xF)) {
                sSelectedAddress--;
                sUpdateBuffer = TRUE;
            }
            if ((sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_RIGHT)
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
}

void crash_screen_input_disasm(void) {
    if (sAddressSelectMenuOpen) {
        crash_screen_select_address(DISASM_STEP);
    } else if (!update_crash_screen_page()) {
#ifdef INCLUDE_DEBUG_MAP
        uintptr_t oldPos = sSelectedAddress;
#endif
        // sSelectedAddress = ALIGN(sSelectedAddress, DISASM_STEP);
        if ((sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_UP)
         && ((sSelectedAddress - DISASM_STEP) >= RAM_START)) {
            // Scroll up.
            sSelectedAddress -= DISASM_STEP;
            sUpdateBuffer = TRUE;
        }
        if ((sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_DOWN)
         && ((sSelectedAddress + DISASM_STEP) < RAM_END)) {
            // Scroll down.
            sSelectedAddress += DISASM_STEP;
            sUpdateBuffer = TRUE;
        }
        if (gPlayer1Controller->buttonPressed & A_BUTTON) {
            // Open the jump to address box.
            sAddressSelectMenuOpen = TRUE;
            sAddressSelectTarget = sSelectedAddress;
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

struct CrashScreenPage sCrashScreenPages[] = {
    /*PAGE_CONTEXT   */ {draw_crash_context, crash_screen_input_default   },
    /*PAGE_ASSERTS   */ {draw_assert,        crash_screen_input_default   },
#ifdef PUPPYPRINT_DEBUG
    /*PAGE_LOG       */ {draw_crash_log,     crash_screen_input_default   },
#endif
    /*PAGE_STACKTRACE*/ {draw_stacktrace,    crash_screen_input_stacktrace},
    /*PAGE_RAM_VIEWER*/ {draw_ram_viewer,    crash_screen_input_ram_viewer},
    /*PAGE_DISASM    */ {draw_disasm,        crash_screen_input_disasm    },
    /*PAGE_CONTROLS  */ {draw_controls,      crash_screen_input_default   },
};

void update_crash_screen_input(void) {
    // Global controls.
    if (gPlayer1Controller->buttonPressed & Z_TRIG) {
        sDrawCrashScreen ^= TRUE;
        sUpdateBuffer = TRUE;
    }

    if (gPlayer1Controller->buttonPressed & START_BUTTON) {
        sDrawBackground ^= TRUE;
        sUpdateBuffer = TRUE;
    }

    if (!sDrawCrashScreen && !sDrawBackground) {
        sDrawCrashScreen = TRUE;
    }

    if (sDrawCrashScreen) {
        update_crash_screen_direction_input();

        // Run the page-specific input function.
        sCrashScreenPages[sCrashPage].inputFunc();
    }
}

void draw_crash_screen(OSThread *thread) {
    if (sUpdateBuffer) {
        reset_crash_screen_framebuffer();

        if (sDrawCrashScreen) {
            if (sDrawBackground) {
                // Draw the transparent background.
                crash_screen_draw_dark_rect(CRASH_SCREEN_X1, CRASH_SCREEN_Y1, CRASH_SCREEN_W, CRASH_SCREEN_H, 2);
            }

            // Draw the header.
            u32 line = 0;
            crash_screen_print(TEXT_X(0), TEXT_Y(line), "@%08XHackerSM64 v%s", COLOR_RGBA32_CRASH_HEADER, HACKERSM64_VERSION);
            line += crash_screen_print(TEXT_X(35), TEXT_Y(line), "@%08X<Page:%02d>", COLOR_RGBA32_CRASH_HEADER, (sCrashPage + 1));
            crash_screen_draw_divider(DIVIDER_Y(line));

            // Run the page-specific draw function.
            sCrashScreenPages[sCrashPage].drawFunc(thread);
        }

        update_crash_screen_framebuffer();
        sUpdateBuffer = FALSE;
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

        if (!((fname == NULL) || ((*(uintptr_t*)temp_sp & 0x80000000) == 0))) {
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
#endif
extern void read_controller_inputs(s32 threadID);

#ifdef CRASH_SCREEN_CRASH_SCREEN
extern u8 _crash_screen_crash_screenSegmentRomStart[];
extern u8 _crash_screen_crash_screenSegmentRomEnd[];
extern void dma_read(u8 *dest, u8 *srcStart, u8 *srcEnd);

void draw_crashed_image_i4(void) {
    u8 srcColor;
    Color color;
    RGBA16 *fb_u16 = gFramebuffers[sRenderingFramebuffer];

    u8 *segStart = _crash_screen_crash_screenSegmentRomStart;
    u8 *segEnd = _crash_screen_crash_screenSegmentRomEnd;
    size_t size = (uintptr_t) (segEnd - segStart);
    u8 *fb_u8 = (u8*) ((uintptr_t) fb_u16 + (SCREEN_SIZE * sizeof(RGBA16*)) - size);

    // Make sure the source image is the correct size.
    if (size != SRC_IMG_SIZE) {
        return;
    }

    // DMA the data directly onto the framebuffer.
    dma_read(fb_u8, segStart, segEnd);

    // Copy and convert the image data from the framebuffer to itself.
    for (u32 i = 0; i < SRC_IMG_SIZE; i++) {
        srcColor = *fb_u8++;

        color = (srcColor & (BITMASK(4) << 4));
        *fb_u16++ = ((color <<  8) | (color << 3) | (color >> 2) | 0x1); // GPACK_RGBA5551

        color = (srcColor & (BITMASK(4) << 0));
        *fb_u16++ = ((color << 12) | (color << 7) | (color << 2) | 0x1); // GPACK_RGBA5551
    }
}

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
                gCrashScreen2.thread.priority = 15;
                stop_sounds_in_continuous_banks();
                stop_background_music(sBackgroundMusicQueue[0].seqId);
                audio_signal_game_loop_tick();
                crash_screen_sleep(200);
                play_sound(SOUND_MARIO_MAMA_MIA, gGlobalSoundSource);
                audio_signal_game_loop_tick();
                crash_screen_sleep(200);
 #endif
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
                (u8 *) gCrashScreen2.stack + sizeof(gCrashScreen2.stack),
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
            bcopy(gFramebuffers[sRenderingFramebuffer], gZBuffer, FRAMEBUFFER_SIZE);

            thread = get_crashed_thread();
            if (thread) {
#ifdef INCLUDE_DEBUG_MAP
                map_data_init();
                fill_function_stack_trace(thread);
#endif
                // Default to the assert page if the crash was caused by an assert.
                if (thread->context.cause == EXC_SYSCALL) {
                    sCrashPage = PAGE_ASSERTS;
                }
                sSelectedAddress = thread->context.pc;
#ifdef CRASH_SCREEN_CRASH_SCREEN
                crash_screen_crash_screen_init();
#endif
#ifdef FUNNY_CRASH_SOUND
                gCrashScreen.thread.priority = 15;
                stop_sounds_in_continuous_banks();
                stop_background_music(sBackgroundMusicQueue[0].seqId);
                audio_signal_game_loop_tick();
                crash_screen_sleep(200);
                play_sound(SOUND_MARIO_WAAAOOOW, gGlobalSoundSource);
                audio_signal_game_loop_tick();
                crash_screen_sleep(200);
#endif
            }
        } else {
            if (gControllerBits) {
#if ENABLE_RUMBLE
                block_until_rumble_pak_free();
#endif
                osContStartReadData(&gSIEventMesgQueue);
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
                   (u8 *) gCrashScreen.stack + sizeof(gCrashScreen.stack),
                   OS_PRIORITY_APPMAX);
    osStartThread(&gCrashScreen.thread);
}
