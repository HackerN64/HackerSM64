#include <ultra64.h>
#include <PR/os_internal_error.h>
#include <stdarg.h>
#include <string.h>
#include "buffers/framebuffers.h"
#include "types.h"
#include "puppyprint.h"
#include "audio/external.h"
#include "farcall.h"
#include "game_init.h"
#include "main.h"
#include "debug.h"
#include "rumble_init.h"
#include "engine/colors.h"

#include "sm64.h"

#include "printf.h"

enum CrashPages {
    PAGE_CONTEXT,
    PAGE_ASSERTS,
#ifdef PUPPYPRINT_DEBUG
    PAGE_LOG,
#endif
    PAGE_STACKTRACE,
    PAGE_RAM_VIEWER,
    PAGE_DISASM,
    PAGE_CONTROLS,
    PAGE_COUNT,
    PAGES_MAX = 255,
};

enum CrashScreenDirectionFlags {
    CRASH_SCREEN_INPUT_DIRECTION_FLAGS_NONE         = 0x0,
    CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_UP       = BIT(0),
    CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_DOWN     = BIT(1),
    CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_LEFT     = BIT(2),
    CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_RIGHT    = BIT(3),
    CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_UP    = BIT(4),
    CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_DOWN  = BIT(5),
    CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_LEFT  = BIT(6),
    CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_RIGHT = BIT(7),
};

// Crash screen font properties.
#define CRASH_SCREEN_FONT_CHAR_WIDTH     5
#define CRASH_SCREEN_FONT_CHAR_HEIGHT    7
#define CRASH_SCREEN_FONT_CHARS_PER_ROW  6
#define CRASH_SCREEN_FONT_NUM_ROWS      43

// Spacing between chars.
#define CRASH_SCREEN_CHAR_SPACING_X      1
#define CRASH_SCREEN_CHAR_SPACING_Y      3

// The amount of space each char uses.
#define CRASH_SCREEN_LETTER_WIDTH       (CRASH_SCREEN_FONT_CHAR_WIDTH  + CRASH_SCREEN_CHAR_SPACING_X) //  6
#define CRASH_SCREEN_ROW_HEIGHT         (CRASH_SCREEN_FONT_CHAR_HEIGHT + CRASH_SCREEN_CHAR_SPACING_Y) // 10

// Width and height of crash screen.
#define CRASH_SCREEN_W 270
#define CRASH_SCREEN_H 222

// Number of chars that can fit in the crash screen.
#define CRASH_SCREEN_NUM_CHARS_X ((CRASH_SCREEN_W - 1) / CRASH_SCREEN_LETTER_WIDTH) // 44
#define CRASH_SCREEN_NUM_CHARS_Y ((CRASH_SCREEN_H - 1) / CRASH_SCREEN_ROW_HEIGHT)   // 22

// Macros for string size.
#define TEXT_WIDTH(numChars)  ((numChars) * CRASH_SCREEN_LETTER_WIDTH) // n *  6
#define TEXT_HEIGHT(numChars) ((numChars) * CRASH_SCREEN_ROW_HEIGHT  ) // n * 10

// Width and height of the text grid.
#define CRASH_SCREEN_TEXT_W TEXT_WIDTH( CRASH_SCREEN_NUM_CHARS_X) // 264
#define CRASH_SCREEN_TEXT_H TEXT_HEIGHT(CRASH_SCREEN_NUM_CHARS_Y) // 220

// Number of pixels between the text and the edge of the crash screen.
#define CRASH_SCREEN_TEXT_MARGIN_X ((CRASH_SCREEN_W - CRASH_SCREEN_TEXT_W) / 2) // 3
#define CRASH_SCREEN_TEXT_MARGIN_Y ((CRASH_SCREEN_H - CRASH_SCREEN_TEXT_H) / 2) // 1

// Top left corner of crash screen (round up).
#define CRASH_SCREEN_X1 (((SCREEN_WIDTH  - CRASH_SCREEN_W) / 2) - 0) // 25
#define CRASH_SCREEN_Y1 (((SCREEN_HEIGHT - CRASH_SCREEN_H) / 2) - 1) //  8

// Bottom right corner of crash screen.
#define CRASH_SCREEN_X2 (CRASH_SCREEN_X1 + CRASH_SCREEN_W) // 295
#define CRASH_SCREEN_Y2 (CRASH_SCREEN_Y1 + CRASH_SCREEN_H) // 230

// Top left corner of the text grid.
#define CRASH_SCREEN_TEXT_X1 (CRASH_SCREEN_X1 + CRASH_SCREEN_TEXT_MARGIN_X + 0) // 28
#define CRASH_SCREEN_TEXT_Y1 (CRASH_SCREEN_Y1 + CRASH_SCREEN_TEXT_MARGIN_Y + 1) // 10

// Bottom right corner of the text grid.
#define CRASH_SCREEN_TEXT_X2 (CRASH_SCREEN_TEXT_X1 + CRASH_SCREEN_TEXT_W) // 292
#define CRASH_SCREEN_TEXT_Y2 (CRASH_SCREEN_TEXT_Y1 + CRASH_SCREEN_TEXT_H) // 230

// Macros to convert a position on the text grid to screen coords.
#define TEXT_X(numChars) (CRASH_SCREEN_TEXT_X1 + TEXT_WIDTH(numChars) ) // 28 + (n *  6)
#define TEXT_Y(numChars) (CRASH_SCREEN_TEXT_Y1 + TEXT_HEIGHT(numChars)) // 10 + (n * 10)

#define DIVIDER_Y(numChars) (TEXT_Y(numChars) - 2)

typedef u32 CrashScreenFontRow;

// Crash screen font. Each row of the image fits in one u32 pointer.
ALIGNED32 CrashScreenFontRow gCrashScreenFont[CRASH_SCREEN_FONT_CHAR_HEIGHT * CRASH_SCREEN_FONT_NUM_ROWS] = {
    #include "textures/crash_screen/crash_screen_font.custom.ia1.inc.c"
};


#define STACK_SIZE 256 // (s32)(0x800 / sizeof(u64))

struct FunctionInStack {
    uintptr_t addr;
    char *name;
};

struct CrashScreenPage {
    void (*drawFunc)(OSThread *thread);
    void (*inputFunc)(void);
};

struct FunctionInStack sAllFunctionStack[STACK_SIZE];
struct FunctionInStack sKnownFunctionStack[STACK_SIZE];
static s32 sNumKnownFunctions = 0;
static s32 sNumShownFunctions = STACK_SIZE;

static s8 sCrashScreenDirectionFlags = CRASH_SCREEN_INPUT_DIRECTION_FLAGS_NONE;

static s8 sDrawCrashScreen = TRUE;
static s8 sDrawFrameBuffer = TRUE;
static s8 sStackTraceShowNames = TRUE;
static s8 sStackTraceSkipUnknowns = FALSE;
static s8 sAddressSelectMenuOpen = FALSE;
static s8 sAddressSelecCharIndex = 2;
static s8 sRamViewerShowAscii = FALSE;
static uintptr_t sAddressSelect = 0;
static uintptr_t sProgramPosition = 0;
static s32 sStackTraceIndex = 0;

u8 sCrashPage = PAGE_CONTEXT;
u8 sUpdateBuffer = TRUE;


char *gCauseDesc[18] = {
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

char *gFpcsrDesc[6] = {
    /*FPCSR_CE*/ "Unimplemented operation",
    /*FPCSR_CV*/ "Invalid operation",
    /*FPCSR_CZ*/ "Division by zero",
    /*FPCSR_CO*/ "Overflow",
    /*FPCSR_CU*/ "Underflow",
    /*FPCSR_CI*/ "Inexact operation",
};

char *gRegNames[29] = {
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


extern u64 osClockRate;
extern far char *parse_map(uintptr_t pc);
extern far void map_data_init(void);
extern far char *find_function_in_stack(uintptr_t *sp);

struct CrashScreen {
    /*0x000*/ OSThread thread;
    /*0x1B0*/ u64 stack[0x800 / sizeof(u64)];
    /*0x9B0*/ OSMesgQueue mesgQueue;
    /*0x9C8*/ OSMesg mesg;
}; /*0x9CC*/

struct CrashScreen gCrashScreen;
#ifdef CRASH_SCREEN_CRASH_SCREEN
struct CrashScreen gCrashScreen2;
#endif

extern u16 sRenderedFramebuffer;
extern u16 sRenderingFramebuffer;
u16 sScreenshotFrameBuffer;

#define CRASH_SCREEN_BACKGROUND_ALPHA 0xBF

RGBA16 *crash_screen_get_framebuffer_pixel_ptr(s32 x, s32 y) {
    return (gFramebuffers[sRenderingFramebuffer] + (SCREEN_WIDTH * y) + x);
}

void crash_screen_draw_rect(s32 startX, s32 startY, s32 w, s32 h, RGBA16 color, Alpha alpha) {
    s32 x, y;

    RGBA16 *ptr = crash_screen_get_framebuffer_pixel_ptr(startX, startY);

    for (y = 0; y < h; y++) {
        for (x = 0; x < w; x++) {
            if (alpha != 0xFF) {
                rgba16_blend(ptr, color, alpha);
            } else {
                *ptr = color;
            }
            ptr++;
        }

        ptr += (SCREEN_WIDTH - w);
    }
}

void crash_screen_draw_triangle(s32 startX, s32 startY, s32 w, s32 h, RGBA16 color, Alpha alpha, s8 flip) {
    const f32 t = ((f32) w / 2.0f) / (f32) h;
    f32 d = flip ? (w / 2.0f) - t : 0.0f;
    s32 x, y;

    RGBA16 *ptr = crash_screen_get_framebuffer_pixel_ptr(startX, startY);

    for (y = 0; y < h; y++) {
        for (x = 0; x < w; x++) {
            if (absf((w / 2.0f) - x) < d) {
                if (alpha != 0xFF) {
                    rgba16_blend(ptr, color, alpha);
                } else {
                    *ptr = color;
                }
            }
            ptr++;
        }

        d += flip ? -t : t;
        ptr += (SCREEN_WIDTH - w);
    }
}

void crash_screen_draw_divider(s32 y) {
    crash_screen_draw_rect(CRASH_SCREEN_X1, y, CRASH_SCREEN_W, 1, COLOR_RGBA16_LIGHT_GRAY, 0xFF);
}

void crash_screen_draw_glyph(s32 startX, s32 startY, s32 glyph, RGBA16 color, Alpha alpha) {
    CrashScreenFontRow bit;
    CrashScreenFontRow rowMask;
    s32 x, y;

    if (glyph == 0) {
        color = COLOR_RGBA16_GRAY;
    }

    CrashScreenFontRow *data = &gCrashScreenFont[(glyph / CRASH_SCREEN_FONT_CHARS_PER_ROW) * CRASH_SCREEN_FONT_CHAR_HEIGHT];

    RGBA16 *ptr = crash_screen_get_framebuffer_pixel_ptr(startX, startY);

    for (y = 0; y < CRASH_SCREEN_FONT_CHAR_HEIGHT; y++) {
        bit = ((CrashScreenFontRow)BIT(31) >> ((glyph % CRASH_SCREEN_FONT_CHARS_PER_ROW) * CRASH_SCREEN_FONT_CHAR_WIDTH));
        rowMask = *data++;

        for (x = 0; x < CRASH_SCREEN_FONT_CHAR_WIDTH; x++) {
            if (bit & rowMask) {
                if (alpha != 0xFF) {
                    rgba16_blend(ptr, color, alpha);
                } else {
                    *ptr = color;
                }
            }
            ptr++;
            bit >>= 1;
        }

        ptr += (SCREEN_WIDTH - CRASH_SCREEN_FONT_CHAR_WIDTH);
    }
}

void crash_screen_draw_scroll_bar(u32 topY, u32 bottomY, u32 numVisibleEntries, u32 numTotalEntries, u32 currEntry, u32 minScrollBarHeight, RGBA16 color) {
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
    crash_screen_draw_rect((CRASH_SCREEN_X2 - 1), (topY + scrollPos), 1, scrollBarHeight, color, 0xFF);
}

static char *write_to_buf(char *buffer, const char *data, size_t size) {
    return (char *) memcpy(buffer, data, size) + size;
}

u32 glyph_to_hex(u8 *dest, char glyph) {
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

u32 crash_screen_text_to_color(RGBA16 *color, char *ptr, s32 index) {
    s32 i, j;
    char glyph;
    u8 hex = 0;
    Color component = 0;
    ColorRGBA rgba = { 0, 0, 0, 0 };
    *color = COLOR_RGBA16_CRASH_WHITE;

    for (i = 0; i < 4; i++) {
        for (j = 0; j < 2; j++) {
            glyph = (ptr[index] & 0xFF);
            if (!glyph_to_hex(&hex, glyph)) {
                return FALSE;
            }
            component |= (hex << ((1 - j) * 4));

            index++;
            if (!ptr[index]) {
                return FALSE;
            }
        }
        if (!ptr[index]) {
            return FALSE;
        }
        rgba[i] = component;
        component = 0;
    }

    *color = GPACK_RGBA5551(rgba[0], rgba[1], rgba[2], rgba[3]);

    return TRUE;
}

// Returns how many chars to skip.
s32 crash_screen_process_formatting_char(char *ptr, s32 index, s32 size, char glyph, RGBA16 *color) {
    static s8 escape = FALSE;
    s32 skip = 0;

    if (index + (1 + 8) > size) {
        return 0;
    }

    if (glyph == '\\' && ptr[index + 1] && (ptr[index + 1] & 0xFF) == '@') { // Use '\\@' to print '@'
        escape = TRUE;
        skip = 1;
    } else if (glyph == '@' && !escape) { // RRGGBBAA color prefix
        if (crash_screen_text_to_color(color, ptr, index + 1)) {
            skip = 8;
        }
    }

    if (skip <= 0) {
        escape = FALSE;
    }

    return skip;
}

// Returns whether to wrap or not.
s32 crash_screen_process_space(char *ptr, s32 index, s32 x, s32 size) {
    s32 ci = index + 1;
    s32 checkX = x + TEXT_WIDTH(1);
    char glyph = (ptr[ci] & 0xFF);
    RGBA16 color;

    while (ptr[ci] && glyph != ' ' && ci < size) { // check the next word after the space
        // New line if the next word is larger than the writable space.
        if (ci + 1 < size && checkX >= CRASH_SCREEN_TEXT_X2) {
            return TRUE;
        }

        ci += 1 + crash_screen_process_formatting_char(ptr, ci, size, glyph, &color);
        glyph = (ptr[ci] & 0xFF);
        checkX += TEXT_WIDTH(1);
    }

    return FALSE;
}

enum CrashScreenPrintOp {
    CRASH_SCREEN_PRINT_OP_SPACE,
    CRASH_SCREEN_PRINT_OP_SKIP,
    CRASH_SCREEN_PRINT_OP_NEWLINE,
};

#define CHAR_BUFFER_SIZE 0x100

s32 crash_screen_print(s32 startX, s32 startY, const char *fmt, ...) {
    char glyph;
    char buf[CHAR_BUFFER_SIZE];

    bzero(&buf, sizeof(buf));

    va_list args;
    va_start(args, fmt);

    s32 size = _Printf(write_to_buf, buf, fmt, args);

    RGBA16 color = COLOR_RGBA16_CRASH_WHITE;

    s32 x = startX;
    s32 y = startY;

    s8 printOp = CRASH_SCREEN_PRINT_OP_SPACE;
    s32 skip = 0;
    s32 numLines = 1;

    if (size > 0) {
        for (s32 i = 0; i < size && buf[i]; i += (1 + skip)) {
            glyph = (buf[i] & 0xFF);
            printOp = CRASH_SCREEN_PRINT_OP_SPACE;
            skip = 0;

            if (glyph == ' ') { // space
                if (crash_screen_process_space(buf, i, x, size)) {
                    printOp = CRASH_SCREEN_PRINT_OP_NEWLINE;
                }
            } else if (glyph == '\r' || glyph == '\n') { // new line
                printOp = CRASH_SCREEN_PRINT_OP_NEWLINE;
            } else {
                // Check for formatting codes
                skip = crash_screen_process_formatting_char(buf, i, size, glyph, &color);

                if (skip > 0) {
                    printOp = CRASH_SCREEN_PRINT_OP_SKIP;
                } else { // normal char
                    crash_screen_draw_glyph(x, y, glyph, color, 0xFF);

                    if (i + 1 < size && x + TEXT_WIDTH(1) >= CRASH_SCREEN_TEXT_X2) {
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

void crash_screen_sleep(s32 ms) {
    u64 cycles = ((ms * 1000LL * osClockRate) / 1000000ULL);
    osSetTime(0);
    while (osGetTime() < cycles) { }
}

void crash_screen_print_float_reg(s32 x, s32 y, s32 regNum, void *addr) {
    uintptr_t bits = *(uintptr_t*) addr;
    s32 exponent = (((bits & 0x7f800000U) >> 0x17) - 0x7F);

    if ((exponent >= -0x7E && exponent <= 0x7F) || (bits == 0x0)) {
        f32 val = *(f32*) addr;
        crash_screen_print(x, y, "@3FC07FFFF%02d:%s@FFFFFFFF%.3e", regNum, ((val < 0) ? "" : " "), val);
    } else {
        crash_screen_print(x, y, "@3FC07FFFF%02d:@FFFFFFFF%08XD", regNum, bits);
    }
}

void crash_screen_print_fpcsr(uintptr_t fpcsr) {
    s32 i;
    uintptr_t bit = BIT(17);

    crash_screen_print(TEXT_X(0), (TEXT_Y(14) + 5), "@3FC07FFF%s:@FFFFFFFF%08X", "FPCSR", fpcsr);

    for (i = 0; i < 6; i++) {
        if (fpcsr & bit) {
            crash_screen_print(TEXT_X(16), (TEXT_Y(14) + 5), "@FF3F00FF(%s)", gFpcsrDesc[i]);
            return;
        }
        bit >>= 1;
    }
}

void crash_screen_print_registers(__OSThreadContext *tc) {
    s32 regNum = 0;
    u64 *reg = &tc->at;

    crash_screen_print(TEXT_X(0 * 15), TEXT_Y( 3), "@3FC07FFF%s:@FFFFFFFF%08X", "PC", (uintptr_t) tc->pc);
    crash_screen_print(TEXT_X(1 * 15), TEXT_Y( 3), "@3FC07FFF%s:@FFFFFFFF%08X", "SR", (uintptr_t) tc->sr);
    crash_screen_print(TEXT_X(2 * 15), TEXT_Y( 3), "@3FC07FFF%s:@FFFFFFFF%08X", "VA", (uintptr_t) tc->badvaddr);

    crash_screen_print(TEXT_X(2 * 15), TEXT_Y(13), "@3FC07FFF%s:@FFFFFFFF%08X", "MM", *(uintptr_t*)tc->pc);

    osWritebackDCacheAll();

    for (s32 y = 0; y < 10; y++) {
        for (s32 x = 0; x < 3; x++) {
            crash_screen_print(TEXT_X(x * 15), TEXT_Y(4 + y), "@3FC07FFF%s:@FFFFFFFF%08X", gRegNames[regNum], (uintptr_t) *(reg + regNum));

            regNum++;

            if ((reg + regNum) > &tc->ra) {
                return;
            }
        }
    }
}

void crash_screen_print_float_registers(__OSThreadContext *tc) {
    s32 regNum = 0;
    __OSfp *osfp = &tc->fp0;

    crash_screen_print_fpcsr(tc->fpcsr);

    osWritebackDCacheAll();

    for (s32 y = 0; y < 6; y++) {
        for (s32 x = 0; x < 3; x++) {
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

    // Make the last two cause cases sequential.
    s32 cause = ((tc->cause >> 2) & 0x1F);
    if (cause == (EXC_WATCH >> 2)) cause = 16;
    if (cause == (EXC_VCED  >> 2)) cause = 17;

    s32 line = 1;

    crash_screen_print(TEXT_X(0), TEXT_Y(line), "@7F7FFFFFTHREAD:%d", thread->id);
    line += crash_screen_print(TEXT_X(10), TEXT_Y(line), "@FF3F00FF(%s)", gCauseDesc[cause]);

    osWritebackDCacheAll();

    if ((uintptr_t) parse_map != MAP_PARSER_ADDRESS) {
        char *fname = parse_map(tc->pc);
        crash_screen_print(TEXT_X(0), TEXT_Y(line), "@FF7F7FFFCRASH AT:");
        if (fname == NULL) {
            crash_screen_print(TEXT_X(10), TEXT_Y(line), "@7F7F7FFF%s", "UNKNOWN");
        } else {
            crash_screen_print(TEXT_X(10), TEXT_Y(line), "@FFFF7FFF%s", fname);
        }
    }

    crash_screen_print_registers(tc);

    osWritebackDCacheAll();

    crash_screen_print_float_registers(tc);
}

void draw_assert(UNUSED OSThread *thread) {
    s32 line = 1;

    line += crash_screen_print(TEXT_X(0), TEXT_Y(line), "ASSERT PAGE");

    crash_screen_draw_divider(DIVIDER_Y(line));

    if (__n64Assert_Filename != NULL) {
        line += crash_screen_print(TEXT_X(0), TEXT_Y(line), "@007FFFFFFILE: %s @FF7F7FFFLINE %d", __n64Assert_Filename, __n64Assert_LineNum);
        crash_screen_draw_divider(DIVIDER_Y(line));
        line += crash_screen_print(TEXT_X(0), TEXT_Y(line), "@C0C0C0FFMESSAGE:");
        line += crash_screen_print(TEXT_X(0), (TEXT_Y(line) + 5), "%s", __n64Assert_Message);
    } else {
        crash_screen_print(TEXT_X(0), TEXT_Y(line), "no failed assert to report.");
    }

    osWritebackDCacheAll();
}

#ifdef PUPPYPRINT_DEBUG
void draw_crash_log(UNUSED OSThread *thread) {
    s32 i;
    osWritebackDCacheAll();
    for (i = 0; i < LOG_BUFFER_SIZE; i++) {
        crash_screen_print(TEXT_X(0), TEXT_Y(LOG_BUFFER_SIZE - i), consoleLogTable[i]);
    }
}
#endif

#define STACK_TRACE_NUM_ROWS 18

// prints any function pointers it finds in the stack format:
// SP address: function name
void draw_stacktrace(OSThread *thread) {
    __OSThreadContext *tc = &thread->context;
    uintptr_t temp_sp = (tc->sp + 0x14);
    s32 currIndex;
    uintptr_t faddr;
    char *fname;
    struct FunctionInStack *functionList = (sStackTraceSkipUnknowns ? sKnownFunctionStack : sAllFunctionStack);
    struct FunctionInStack *function = NULL;

    s32 line = 1;

    line += crash_screen_print(TEXT_X(0), TEXT_Y(line), "STACK TRACE FROM %08X:", temp_sp);
    crash_screen_print(TEXT_X(0), TEXT_Y(line), "@FF7F7FFFCURRFUNC:");
    crash_screen_draw_divider(DIVIDER_Y(line));

    if ((uintptr_t) parse_map == MAP_PARSER_ADDRESS) {
        line += crash_screen_print(TEXT_X(9), TEXT_Y(line), "NONE");
    } else {
        line += crash_screen_print(TEXT_X(9), TEXT_Y(line), "@FFFF7FFF%s", parse_map(tc->pc));
    }

    crash_screen_draw_divider(DIVIDER_Y(line));

    osWritebackDCacheAll();

    // Print
    for (s32 j = 0; j < STACK_TRACE_NUM_ROWS; j++) {
        s32 y = TEXT_Y(line + j);

        if ((uintptr_t) find_function_in_stack == MAP_PARSER_ADDRESS) {
            crash_screen_print(TEXT_X(0), y, "STACK TRACE DISABLED");
            break;
        }

        currIndex = sStackTraceIndex + j;

        if (currIndex >= sNumShownFunctions) {
            break;
        }

        function = &functionList[currIndex];

        faddr = function->addr;
        fname = function->name;

        crash_screen_print(TEXT_X(0), y, "%08X:", faddr);

        if (!sStackTraceSkipUnknowns && ((fname == NULL) || ((*(uintptr_t*)faddr & 0x80000000) == 0))) {
            // Print unknown function
            crash_screen_print(TEXT_X(9), y, "@C0C0C0FF%08X", *(uintptr_t*)faddr);
        } else {
            // Print known function
            if (sStackTraceShowNames) {
                crash_screen_print(TEXT_X(9), y, "@FFFFC0FF%s", fname);
            } else {
                crash_screen_print(TEXT_X(9), y, "@FFFFC0FF%08X", *(uintptr_t*)faddr);
            }
        }
    }

    osWritebackDCacheAll();

    crash_screen_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y - 1));
    crash_screen_print(TEXT_X(0), TEXT_Y(CRASH_SCREEN_NUM_CHARS_Y - 1), "@C0C0C0FFup/down:scroll    toggle: a:names b:unknowns");

    // Scroll Bar
    crash_screen_draw_scroll_bar(DIVIDER_Y(3), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y - 1), STACK_TRACE_NUM_ROWS, sNumShownFunctions, sStackTraceIndex, 4, COLOR_RGBA16_LIGHT_GRAY);

    osWritebackDCacheAll();
}

#define JUMP_MENU_W (TEXT_WIDTH(8))
#define JUMP_MENU_H (TEXT_HEIGHT(1))

#define JUMP_MENU_X (SCREEN_CENTER_X - (JUMP_MENU_W / 2))
#define JUMP_MENU_Y (SCREEN_CENTER_Y - (JUMP_MENU_H / 2))

#define JUMP_MENU_MARGIN_X 10
#define JUMP_MENU_MARGIN_Y 10

void draw_address_select(void) {
    crash_screen_draw_rect((JUMP_MENU_X -  JUMP_MENU_MARGIN_X     ), (JUMP_MENU_Y - TEXT_HEIGHT(2) -  JUMP_MENU_MARGIN_X     ),
                           (JUMP_MENU_W + (JUMP_MENU_MARGIN_Y * 2)), (JUMP_MENU_H + TEXT_HEIGHT(2) + (JUMP_MENU_MARGIN_Y * 2)),
                           COLOR_RGBA16_BLACK, CRASH_SCREEN_BACKGROUND_ALPHA);

    crash_screen_print(JUMP_MENU_X + TEXT_WIDTH(1), JUMP_MENU_Y - TEXT_HEIGHT(2), "GO TO:");

    crash_screen_draw_triangle((JUMP_MENU_X + (sAddressSelecCharIndex * TEXT_WIDTH(1)) - 1), (JUMP_MENU_Y - TEXT_HEIGHT(1) + CRASH_SCREEN_CHAR_SPACING_Y),
                               TEXT_WIDTH(1), TEXT_WIDTH(1),
                               COLOR_RGBA16_CRASH_SELECT, 0xFF, FALSE);
    crash_screen_draw_triangle((JUMP_MENU_X + (sAddressSelecCharIndex * TEXT_WIDTH(1)) - 1), (JUMP_MENU_Y + TEXT_HEIGHT(1) - CRASH_SCREEN_CHAR_SPACING_Y + 1),
                               TEXT_WIDTH(1), TEXT_WIDTH(1),
                               COLOR_RGBA16_CRASH_SELECT, 0xFF, TRUE);
    crash_screen_print(JUMP_MENU_X, JUMP_MENU_Y, "%08X", sAddressSelect);

    osWritebackDCacheAll();
}

#define RAM_VIEWER_NUM_ROWS 18
#define RAM_VIEWER_SHOWN_SECTION (RAM_VIEWER_NUM_ROWS * 0x10)

#define RAM_VIEWER_SCROLL_MIN RAM_START
#define RAM_VIEWER_SCROLL_MAX (RAM_END - RAM_VIEWER_SHOWN_SECTION)

void draw_ram_viewer(OSThread *thread) {
    __OSThreadContext *tc = &thread->context;

    if (sProgramPosition == 0) {
        sProgramPosition = (tc->pc & ~0xF);
    }

    uintptr_t addr = sProgramPosition;
    uintptr_t currAddr = addr;

    s32 charX, charY;

    s32 line = 1;

    crash_screen_print(TEXT_X( 0), TEXT_Y(line), "@FF7F00FFSELECTED ADDRESS:");
    line += crash_screen_print(TEXT_X(17), TEXT_Y(line), "%08X", addr);
    crash_screen_draw_divider(DIVIDER_Y(line));

    charX = TEXT_X(8) + 3;

    for (s32 i = 0; i < 16; i++) {
        if ((i & 0x3) == 0) {
            charX += 2;
        }
        crash_screen_print(charX, TEXT_Y(line), ((i & 0x1) ? "@00B7FFFF%02X" : "@0087FFFF%02X"), i);
        charX += TEXT_WIDTH(2) + 1;
    }
    crash_screen_draw_divider(DIVIDER_Y(3));

    crash_screen_draw_rect(TEXT_X(8) + 2, DIVIDER_Y(line), 1, TEXT_HEIGHT(19), COLOR_RGBA16_LIGHT_GRAY, 0xFF);

    line += crash_screen_print(TEXT_X( 1), TEXT_Y(line), "MEMORY");

    charX = TEXT_X(8) + 3;
    charY = TEXT_Y(line);

    for (s32 y = 0; y < RAM_VIEWER_NUM_ROWS; y++) {
        currAddr = addr + (y * 0x10);
        crash_screen_print(TEXT_X(0), TEXT_Y(line + y), ((y & 0x1) ? "@5FDF5FFF%08X" : "@1F9F1FFF%08X"), currAddr);

        charX = TEXT_X(8) + 3;
        charY = TEXT_Y(line + y);
        for (s32 x = 0; x < 16; x++) {
            u8 value = *((u8 *)currAddr + x);
            if ((x & 0x3) == 0) {
                charX += 2;
            }
            if (sRamViewerShowAscii) {
                crash_screen_draw_glyph(charX + TEXT_WIDTH(1), charY, value, COLOR_RGBA16_CRASH_WHITE, 0xFF);
            } else {
                crash_screen_print(charX, charY, ((x & 0x1) ? "@FFFFFFFF%02X" : "@BFBFBFFF%02X"), value);
            }
            charX += TEXT_WIDTH(2) + 1;
        }
    }

    crash_screen_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y - 1));
    crash_screen_print(TEXT_X(0), TEXT_Y(CRASH_SCREEN_NUM_CHARS_Y - 1), "@C0C0C0FFup/down:scroll        a:jump  b:toggle ascii");

    // Scroll bar
    crash_screen_draw_scroll_bar(DIVIDER_Y(3), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y - 1), RAM_VIEWER_SHOWN_SECTION, TOTAL_RAM_SIZE, (sProgramPosition - RAM_VIEWER_SCROLL_MIN), 4, COLOR_RGBA16_LIGHT_GRAY);

    osWritebackDCacheAll();

    if (sAddressSelectMenuOpen) {
        draw_address_select();
    }

    osWritebackDCacheAll();
}

#define DISASM_NUM_ROWS 19
#define DISASM_SHOWN_SECTION (DISASM_NUM_ROWS * 4)

#define DISASM_SCROLL_MIN RAM_START
#define DISASM_SCROLL_MAX (RAM_END - DISASM_SHOWN_SECTION)

extern char *insn_disasm(u32 insn, u32 isPC);
void draw_disasm(OSThread *thread) {
    __OSThreadContext *tc = &thread->context;

    if (sProgramPosition == 0) {
        sProgramPosition = (tc->pc - ((DISASM_NUM_ROWS / 2) * 4));
    }

    crash_screen_print(TEXT_X(0), TEXT_Y(1), "DISASM %08X-%08X", sProgramPosition, (sProgramPosition + DISASM_SHOWN_SECTION));

    osWritebackDCacheAll();

    for (int i = 0; i < DISASM_NUM_ROWS; i++) {
        uintptr_t addr = (sProgramPosition + (i * 4));
        uintptr_t toDisasm = *(uintptr_t*)(addr);

        crash_screen_print(TEXT_X(0), TEXT_Y(2 + i), "%s", insn_disasm(toDisasm, (addr == tc->pc)));
    }

    osWritebackDCacheAll();

    crash_screen_draw_divider(DIVIDER_Y( 2));
    crash_screen_draw_divider(DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y - 1));
    crash_screen_print(TEXT_X(0), TEXT_Y(CRASH_SCREEN_NUM_CHARS_Y - 1), "@C0C0C0FFup/down:scroll");

    // Scroll bar
    crash_screen_draw_scroll_bar(DIVIDER_Y(2), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y - 1), DISASM_SHOWN_SECTION, TOTAL_RAM_SIZE, (sProgramPosition - DISASM_SCROLL_MIN), 4, COLOR_RGBA16_LIGHT_GRAY);

    // Scroll bar crash position marker
    crash_screen_draw_scroll_bar(DIVIDER_Y(2), DIVIDER_Y(CRASH_SCREEN_NUM_CHARS_Y - 1), DISASM_SHOWN_SECTION, TOTAL_RAM_SIZE, (tc->pc - DISASM_SCROLL_MIN), 1, COLOR_RGBA16_CRASH_CURRFUNC);

    osWritebackDCacheAll();
}

void draw_controls(UNUSED OSThread *thread) {
    s32 line = 1;

    line++;
    line += crash_screen_print(TEXT_X(1), TEXT_Y(line), "CRASH SCREEN CONTROLS");
    line++;
    line += crash_screen_print(TEXT_X(2), TEXT_Y(line), "START:");
    line += crash_screen_print(TEXT_X(2), TEXT_Y(line), "@C0C0C0FFtoggle framebuffer background");
    line++;
    line += crash_screen_print(TEXT_X(2), TEXT_Y(line), "Z:");
    line += crash_screen_print(TEXT_X(2), TEXT_Y(line), "@C0C0C0FFtoggle framebuffer only view");
    line++;
    line += crash_screen_print(TEXT_X(2), TEXT_Y(line), "ANALOG STICK, D-PAD, OR C BUTTONS:");
    line++;
    line += crash_screen_print(TEXT_X(3), TEXT_Y(line), "LEFT/RIGHT:");
    line += crash_screen_print(TEXT_X(3), TEXT_Y(line), "@C0C0C0FFswitch page");
    line++;
    line += crash_screen_print(TEXT_X(3), TEXT_Y(line), "UP/DOWN:");
    line += crash_screen_print(TEXT_X(3), TEXT_Y(line), "@C0C0C0FFscroll page");

    osWritebackDCacheAll();
}

#define FRAMEBUFFER_SIZE (SCREEN_SIZE * sizeof(RGBA16))

void crash_screen_take_screenshot(void) {
    if (gIsConsole) {
        // Save a screenshot of the game to a framebuffer that's not sRenderedFramebuffer or sRenderingFramebuffer.
        sScreenshotFrameBuffer = ((sRenderingFramebuffer + 1) % 3);
        memcpy(gFramebuffers[sScreenshotFrameBuffer], gFramebuffers[sRenderingFramebuffer], FRAMEBUFFER_SIZE);
    } else {
        sScreenshotFrameBuffer = sRenderedFramebuffer;
        sRenderedFramebuffer = ((sScreenshotFrameBuffer + 1) % 3);
        sRenderingFramebuffer = ((sRenderedFramebuffer + 1) % 3);
        memcpy(gFramebuffers[sRenderingFramebuffer], gFramebuffers[sScreenshotFrameBuffer], FRAMEBUFFER_SIZE);
    }
}

void reset_crash_screen_framebuffer(void) {
    if (sDrawFrameBuffer) {
        memcpy(gFramebuffers[sRenderingFramebuffer], gFramebuffers[sScreenshotFrameBuffer], FRAMEBUFFER_SIZE);
    } else {
        crash_screen_draw_rect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, COLOR_RGBA16_BLACK, 0xFF);
    }
}

void update_crash_screen_framebuffer(void) {
    memcpy(gFramebuffers[sRenderedFramebuffer], gFramebuffers[sRenderingFramebuffer], FRAMEBUFFER_SIZE);
}

void update_crash_screen_direction_input(void) {
    u8 prevHeld = (sCrashScreenDirectionFlags & 0xF);
    u8 currHeld = 0x0;
    // Up
    COND_BIT(((gPlayer1Controller->buttonDown & (U_CBUTTONS | U_JPAD))
     || (gPlayer1Controller->rawStickY > 60)),
     currHeld, CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_UP);
    // Down
    COND_BIT(((gPlayer1Controller->buttonDown & (D_CBUTTONS | D_JPAD))
     || (gPlayer1Controller->rawStickY < -60)),
     currHeld, CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_DOWN);
    // Left
    COND_BIT(((gPlayer1Controller->buttonDown & (L_CBUTTONS | L_JPAD))
     || (gPlayer1Controller->rawStickX < -60)),
     currHeld, CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_LEFT);
    // Right
    COND_BIT(((gPlayer1Controller->buttonDown & (R_CBUTTONS | R_JPAD))
     || (gPlayer1Controller->rawStickX >  60)),
     currHeld, CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_RIGHT);
    u8 pressed = (~prevHeld & currHeld);
    sCrashScreenDirectionFlags = ((pressed << 4) | currHeld);
}

s32 update_crash_screen_page(void) {
    u8 prevPage = sCrashPage;

    if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_RIGHT) {
        sCrashPage++;
        sUpdateBuffer = TRUE;
    }
    if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_LEFT) {
        sCrashPage--;
        sUpdateBuffer = TRUE;
    }

    if ((sCrashPage >= PAGE_COUNT) && (sCrashPage != PAGES_MAX)) {
        sCrashPage = PAGE_CONTEXT;
    }
    if (sCrashPage == PAGES_MAX) {
        sCrashPage = (PAGE_COUNT - 1);
    }

    if (sCrashPage != prevPage) {
        sStackTraceIndex = 0;
        sProgramPosition = 0;
        sAddressSelecCharIndex = 2;
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
            sStackTraceShowNames ^= TRUE;
            sUpdateBuffer = TRUE;
        }

        if (gPlayer1Controller->buttonPressed & B_BUTTON) {
            sStackTraceSkipUnknowns ^= TRUE;
            sNumShownFunctions = (sStackTraceSkipUnknowns ? sNumKnownFunctions : STACK_SIZE);
            sStackTraceIndex = 0;
            sUpdateBuffer = TRUE;
        }

        if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_UP) {
            if (sStackTraceIndex > 0) {
                sStackTraceIndex--;
            }
            sUpdateBuffer = TRUE;
        }
        if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_DOWN) {
            if (sStackTraceIndex < (sNumShownFunctions - STACK_TRACE_NUM_ROWS)) {
                sStackTraceIndex++;
            }
            sUpdateBuffer = TRUE;
        }
    }
}

void crash_screen_input_ram_viewer(void) {
    if (sAddressSelectMenuOpen) {
        if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_LEFT) {
            sAddressSelecCharIndex = ((sAddressSelecCharIndex - 1) & 0x7);
            sUpdateBuffer = TRUE;
        }
        if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_RIGHT) {
            sAddressSelecCharIndex = ((sAddressSelecCharIndex + 1) & 0x7);
            sUpdateBuffer = TRUE;
        }

        uintptr_t nextSelectedAddress = sAddressSelect;

        if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_UP) {
            s32 shift = 28 - (sAddressSelecCharIndex * 4);
            u8 new = ((sAddressSelect >> shift) & 0xF);
            new = ((new + 1) & 0xF);
            nextSelectedAddress = ((sAddressSelect & ~(0xF << shift)) | (new << shift));

            if (nextSelectedAddress >= RAM_VIEWER_SCROLL_MIN && nextSelectedAddress <= RAM_VIEWER_SCROLL_MAX) {
                sAddressSelect = (nextSelectedAddress & ~0xF);
                sUpdateBuffer = TRUE;
            }
        }
        if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_DOWN) {
            s32 shift = 28 - (sAddressSelecCharIndex * 4);
            u8 new = ((sAddressSelect >> shift) & 0xF);
            new = ((new - 1) & 0xF);
            nextSelectedAddress = ((sAddressSelect & ~(0xF << shift)) | (new << shift));

            if (nextSelectedAddress >= RAM_VIEWER_SCROLL_MIN && nextSelectedAddress <= RAM_VIEWER_SCROLL_MAX) {
                sAddressSelect = (nextSelectedAddress & ~0xF);
                sUpdateBuffer = TRUE;
            }
        }

        if (gPlayer1Controller->buttonPressed & A_BUTTON) {
            sAddressSelectMenuOpen = FALSE;
            sProgramPosition = sAddressSelect;
            sUpdateBuffer = TRUE;
        }
    } else {
        if (!update_crash_screen_page()) {
            if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_UP) {
                sProgramPosition -= 0x10;
                if (sProgramPosition < RAM_VIEWER_SCROLL_MIN) {
                    sProgramPosition = RAM_VIEWER_SCROLL_MIN;
                }
                sUpdateBuffer = TRUE;
            }
            if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_DOWN) {
                sProgramPosition += 0x10;
                if (sProgramPosition > RAM_VIEWER_SCROLL_MAX) {
                    sProgramPosition = RAM_VIEWER_SCROLL_MAX;
                }
                sUpdateBuffer = TRUE;
            }
            if (gPlayer1Controller->buttonPressed & A_BUTTON) {
                sAddressSelectMenuOpen = TRUE;
                sAddressSelect = sProgramPosition;
                sUpdateBuffer = TRUE;
            }
            if (gPlayer1Controller->buttonPressed & B_BUTTON) {
                sRamViewerShowAscii ^= TRUE;
                sUpdateBuffer = TRUE;
            }
        }
    }
}

void crash_screen_input_disasm(void) {
    if (!update_crash_screen_page()) {
        if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_UP) {
            sProgramPosition -= 4;
            if (sProgramPosition < DISASM_SCROLL_MIN) {
                sProgramPosition = DISASM_SCROLL_MIN;
            }
            sUpdateBuffer = TRUE;
        }
        if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_HELD_DOWN) {
            sProgramPosition += 4;
            if (sProgramPosition > DISASM_SCROLL_MAX) {
                sProgramPosition = DISASM_SCROLL_MAX;
            }
            sUpdateBuffer = TRUE;
        }
    }
}

struct CrashScreenPage sCrashScreenPages[] = {
    /*PAGE_CONTEXT   */ {draw_crash_context, crash_screen_input_default},
    /*PAGE_ASSERTS   */ {draw_assert,        crash_screen_input_default},
#ifdef PUPPYPRINT_DEBUG
    /*PAGE_LOG       */ {draw_crash_log,     crash_screen_input_default},
#endif
    /*PAGE_STACKTRACE*/ {draw_stacktrace,    crash_screen_input_stacktrace},
    /*PAGE_RAM_VIEWER*/ {draw_ram_viewer,    crash_screen_input_ram_viewer},
    /*PAGE_DISASM    */ {draw_disasm,        crash_screen_input_disasm},
    /*PAGE_CONTROLS  */ {draw_controls,      crash_screen_input_default},
};

void update_crash_screen_input(void) {
    if (gPlayer1Controller->buttonPressed & Z_TRIG) {
        sDrawCrashScreen ^= TRUE;
        sUpdateBuffer = TRUE;
    }

    if (gPlayer1Controller->buttonPressed & START_BUTTON) {
        sDrawFrameBuffer ^= TRUE;
        sUpdateBuffer = TRUE;
    }

    if (!sDrawCrashScreen && !sDrawFrameBuffer) {
        sDrawCrashScreen = TRUE;
    }

    if (sDrawCrashScreen) {
        update_crash_screen_direction_input();

        // Page-specific inputs.
        sCrashScreenPages[sCrashPage].inputFunc();
    }
}

void draw_crash_screen(OSThread *thread) {
    update_crash_screen_input();

    if (sUpdateBuffer) {
        reset_crash_screen_framebuffer();

        if (sDrawCrashScreen) {
            if (sDrawFrameBuffer) {
                crash_screen_draw_rect(CRASH_SCREEN_X1, CRASH_SCREEN_Y1, CRASH_SCREEN_W, CRASH_SCREEN_H, COLOR_RGBA16_BLACK, CRASH_SCREEN_BACKGROUND_ALPHA);
            }
            s32 line = 0;
            crash_screen_print(TEXT_X(0), TEXT_Y(line), "@C0C0C0FFHackerSM64 v%s", HACKERSM64_VERSION);
            line += crash_screen_print(TEXT_X(35), TEXT_Y(line), "@C0C0C0FF<Page:%02d>", (sCrashPage + 1));
            crash_screen_draw_divider(DIVIDER_Y(line));
            sCrashScreenPages[sCrashPage].drawFunc(thread);
        }

        update_crash_screen_framebuffer();

        osWritebackDCacheAll();
        osViBlack(FALSE);
        osViSwapBuffer(gFramebuffers[sRenderedFramebuffer]);
        sUpdateBuffer = FALSE;
    }
}

OSThread *get_crashed_thread(void) {
    OSThread *thread = __osGetCurrFaultedThread();

    while (thread->priority != -1) {
        if (thread->priority >  OS_PRIORITY_IDLE
         && thread->priority <= OS_PRIORITY_APPMAX
         && (thread->flags & (BIT(0) | BIT(1)))) {
            return thread;
        }
        thread = thread->tlnext;
    }
    return NULL;
}

void fill_function_stack_trace(OSThread *thread) {
    __OSThreadContext *tc = &thread->context;
    uintptr_t temp_sp = (tc->sp + 0x14);
    struct FunctionInStack *function = NULL;
    char *fname;

    if ((uintptr_t) find_function_in_stack == MAP_PARSER_ADDRESS) {
        return;
    }

    // Fill the stack buffer.
    for (s32 i = 0; i < STACK_SIZE; i++) {
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

#define SRC_IMG_SIZE (SCREEN_SIZE / 2)

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
    for (s32 i = 0; i < SRC_IMG_SIZE; i++) {
        srcColor = *fb_u8++;

        color = (srcColor & 0xF0);
        *fb_u16++ = ((color <<  8) | (color << 3) | (color >> 2) | 0x1); // GPACK_RGBA5551

        color = (srcColor & 0x0F);
        *fb_u16++ = ((color << 12) | (color << 7) | (color << 2) | 0x1); // GPACK_RGBA5551
    }
}

#define MSG_CPU_BREAK 1
#define MSG_FAULT     2

void thread20_crash_screen_crash_screen(UNUSED void *arg) {
    OSMesg mesg;
    OSThread *thread = NULL;

    osSetEventMesg(OS_EVENT_CPU_BREAK, &gCrashScreen2.mesgQueue, (OSMesg)MSG_CPU_BREAK);
    osSetEventMesg(OS_EVENT_FAULT,     &gCrashScreen2.mesgQueue, (OSMesg)MSG_FAULT);

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

                memcpy(gFramebuffers[sRenderedFramebuffer], gFramebuffers[sRenderingFramebuffer], FRAMEBUFFER_SIZE);

                osWritebackDCacheAll();
                osViBlack(FALSE);
                osViSwapBuffer(gFramebuffers[sRenderedFramebuffer]);
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

    osSetEventMesg(OS_EVENT_CPU_BREAK, &gCrashScreen.mesgQueue, (OSMesg)MSG_CPU_BREAK);
    osSetEventMesg(OS_EVENT_FAULT,     &gCrashScreen.mesgQueue, (OSMesg)MSG_FAULT);

    while (TRUE) {
        if (thread == NULL) {
            osRecvMesg(&gCrashScreen.mesgQueue, &mesg, OS_MESG_BLOCK);
            crash_screen_take_screenshot();
            thread = get_crashed_thread();
            if (thread) {
                if ((uintptr_t) map_data_init != MAP_PARSER_ADDRESS) {
                    map_data_init();
                }
                fill_function_stack_trace(thread);

                // Default to the assert page if the crash was caused by an assert.
                if (thread->context.cause == EXC_SYSCALL) {
                    sCrashPage = PAGE_ASSERTS;
                }

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
#ifdef CRASH_SCREEN_CRASH_SCREEN
                crash_screen_crash_screen_init();
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
            draw_crash_screen(thread);
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
