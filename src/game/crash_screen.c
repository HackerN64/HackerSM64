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
#define CRASH_SCREEN_FONT_NUM_ROWS      22

// Spacing between chars.
#define CRASH_SCREEN_SPACING_HORIZONTAL  1
#define CRASH_SCREEN_SPACING_VERTICAL    3

// The amount of space each char uses.
#define CRASH_SCREEN_LETTER_WIDTH       (CRASH_SCREEN_FONT_CHAR_WIDTH + CRASH_SCREEN_SPACING_HORIZONTAL) //  6
#define CRASH_SCREEN_ROW_HEIGHT         (CRASH_SCREEN_FONT_CHAR_HEIGHT + CRASH_SCREEN_SPACING_VERTICAL)  // 10

// Width and height of crash screen.
#define CRASH_SCREEN_W 270
#define CRASH_SCREEN_H 222

// Number of chars that can fit in the crash screen.
#define CRASH_SCREEN_NUM_CHARS_X ((CRASH_SCREEN_W - 1) / CRASH_SCREEN_LETTER_WIDTH) // 44
#define CRASH_SCREEN_NUM_CHARS_Y ((CRASH_SCREEN_H - 1) / CRASH_SCREEN_ROW_HEIGHT)   // 22

// Width and height of the text grid.
#define CRASH_SCREEN_TEXT_W (CRASH_SCREEN_NUM_CHARS_X * CRASH_SCREEN_LETTER_WIDTH) // 264
#define CRASH_SCREEN_TEXT_H (CRASH_SCREEN_NUM_CHARS_Y * CRASH_SCREEN_ROW_HEIGHT)   // 220

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

// Macros for string size.
#define TEXT_WIDTH(numChars)  ((numChars) * CRASH_SCREEN_LETTER_WIDTH) // n *  6
#define TEXT_HEIGHT(numChars) ((numChars) * CRASH_SCREEN_ROW_HEIGHT  ) // n * 10

// Macros to convert a position on the text grid to screen coords.
#define TEXT_X(numChars) (CRASH_SCREEN_TEXT_X1 + TEXT_WIDTH(numChars) ) // 28 + (n *  6)
#define TEXT_Y(numChars) (CRASH_SCREEN_TEXT_Y1 + TEXT_HEIGHT(numChars)) // 10 + (n * 10)

#define DIVIDER_Y(numChars) (TEXT_Y(numChars) - 2)


// Crash screen font. Each row of the image fits in one u32 pointer.
ALIGNED32 u32 gCrashScreenFont[CRASH_SCREEN_FONT_CHAR_HEIGHT * CRASH_SCREEN_FONT_NUM_ROWS] = {
    #include "textures/crash_screen/crash_screen_font.custom.ia1.inc.c"
};


#define STACK_SIZE 256 // (s32)(0x800 / sizeof(u64))

struct FunctionInStack {
    u32 addr;
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
static s8 sAddressSelecCharIndex = 0;
static s8 sRamViewerShowAscii = FALSE;
static u32 sAddressSelect = 0;
static u32 sProgramPosition = 0;
static s32 sStackTraceIndex = 0;

u8 sCrashPage = PAGE_CONTEXT;
u8 sUpdateBuffer = TRUE;


char *gCauseDesc[18] = {
    "Interrupt",
    "TLB modification",
    "TLB exception on load or inst.",
    "TLB exception on store",
    "Address error on load or inst.",
    "Address error on store",
    "Bus error on inst.",
    "Bus error on data",
    "Failed Assert: See Assert Page",
    "Breakpoint exception",
    "Reserved instruction",
    "Coprocessor unusable",
    "Arithmetic overflow",
    "Trap exception",
    "Virtual coherency on inst.",
    "Floating point exception",
    "Watchpoint exception",
    "Virtual coherency on data",
};

char *gFpcsrDesc[6] = {
    "Unimplemented operation",
    "Invalid operation",
    "Division by zero",
    "Overflow",
    "Underflow",
    "Inexact operation",
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
extern far char *parse_map(u32 pc);
extern far void map_data_init(void);
extern far char *find_function_in_stack(u32 *sp);

struct CrashScreen {
    OSThread thread;
    u64 stack[0x800 / sizeof(u64)];
    OSMesgQueue mesgQueue;
    OSMesg mesg;
};

struct CrashScreen gCrashScreen;
#ifdef CRASH_SCREEN_CRASH_SCREEN
struct CrashScreen gCrashScreen2;
#endif

extern u16 sRenderedFramebuffer;
extern u16 sRenderingFramebuffer;
u16 sScreenshotFrameBuffer;


RGBA16 *crash_screen_get_framebuffer_pixel_ptr(s32 x, s32 y) {
    return (gFramebuffers[sRenderingFramebuffer] + (SCREEN_WIDTH * y) + x);
}

void crash_screen_draw_rect(s32 startX, s32 startY, s32 w, s32 h, RGBA16 color, s32 isTransparent) {
    s32 x, y;

    RGBA16 *ptr = crash_screen_get_framebuffer_pixel_ptr(startX, startY);

    for (y = 0; y < h; y++) {
        for (x = 0; x < w; x++) {
            if (isTransparent) {
                *ptr = (((*ptr & color) >> 2) | 0x1);
            } else {
                *ptr = color;
            }
            ptr++;
        }

        ptr += (SCREEN_WIDTH - w);
    }
}

void crash_screen_draw_triangle(s32 startX, s32 startY, s32 w, s32 h, RGBA16 color, s8 flip) {
    const f32 t = ((f32) w / 2.0f) / (f32) h;
    f32 d = flip ? (w / 2.0f) - t : 0.0f;
    s32 x, y;

    RGBA16 *ptr = crash_screen_get_framebuffer_pixel_ptr(startX, startY);

    for (y = 0; y < h; y++) {
        for (x = 0; x < w; x++) {
            if (absf((w / 2.0f) - x) < d) {
                *ptr = color;
            }
            ptr++;
        }

        d += flip ? -t : t;
        ptr += (SCREEN_WIDTH - w);
    }
}

void crash_screen_draw_divider(s32 y) {
    crash_screen_draw_rect(CRASH_SCREEN_X1, y, CRASH_SCREEN_W, 1, COLOR_RGBA16_LIGHT_GRAY, FALSE);
}

void crash_screen_draw_glyph(s32 startX, s32 startY, s32 glyph, RGBA16 color) {
    u32 bit;
    u32 rowMask;
    s32 x, y;

    if (glyph == 0) {
        color = COLOR_RGBA16_GRAY;
    }

    u32 *data = &gCrashScreenFont[(glyph / CRASH_SCREEN_FONT_CHARS_PER_ROW) * CRASH_SCREEN_FONT_CHAR_HEIGHT];

    RGBA16 *ptr = crash_screen_get_framebuffer_pixel_ptr(startX, startY);

    for (y = 0; y < CRASH_SCREEN_FONT_CHAR_HEIGHT; y++) {
        bit = (0x80000000U >> ((glyph % CRASH_SCREEN_FONT_CHARS_PER_ROW) * CRASH_SCREEN_FONT_CHAR_WIDTH));
        rowMask = *data++;

        for (x = 0; x < CRASH_SCREEN_FONT_CHAR_WIDTH; x++) {
            if (bit & rowMask) {
                *ptr = color;
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
    crash_screen_draw_rect((CRASH_SCREEN_X2 - 1), (topY + scrollPos), 1, scrollBarHeight, color, FALSE);
}

static char *write_to_buf(char *buffer, const char *data, size_t size) {
    return (char *) memcpy(buffer, data, size) + size;
}

u32 index_to_hex(u32 glyph) {
    u32 ret = 0;
    if (glyph >= '0' && glyph <= '9') {
        ret = (glyph - '0');
    } else if (glyph >= 'A' && glyph <= 'F') {
        ret = (glyph - 'A') + 10;
    } else if (glyph >= 'a' && glyph <= 'f') {
        ret = (glyph - 'a') + 10;
    }
    return (ret & 0xF);
}

void crash_screen_print(s32 x, s32 y, const char *fmt, ...) {
    u32 glyph;
    char buf[0x100];

    bzero(&buf, sizeof(buf));

    va_list args;
    va_start(args, fmt);

    s32 size = _Printf(write_to_buf, buf, fmt, args);

    RGBA16 color = COLOR_RGBA16_WHITE;

    if (size > 0) {
        char *ptr = buf;

        while (*ptr) {
            glyph = (*ptr & 0x7f);

            if (glyph == '@') {
                ptr++;
                if (!*ptr) {
                    break;
                }
                s32 i, j;
                Color component = 0;
                ColorRGBA rgba = { 0, 0, 0, 0 };
                color = 0;
                for (i = 0; i < 4; i++) {
                    for (j = 0; j < 2; j++) {
                        if (!*ptr) {
                            break;
                        }
                        glyph = (*ptr & 0x7f);
                        component |= (index_to_hex(glyph) << ((1 - j) * 4));
                        ptr++;
                    }
                    rgba[i] = component;
                    component = 0;
                }

                color = GPACK_RGBA5551(rgba[0], rgba[1], rgba[2], rgba[3]);
            } else {
                if (glyph != 0xff) {
                    crash_screen_draw_glyph(x, y, glyph, color);
                }

                ptr++;
                x += CRASH_SCREEN_LETTER_WIDTH;
            }
        }
    }

    va_end(args);
}

void crash_screen_sleep(s32 ms) {
    u64 cycles = ((ms * 1000LL * osClockRate) / 1000000ULL);
    osSetTime(0);
    while (osGetTime() < cycles) { }
}

void crash_screen_print_float_reg(s32 x, s32 y, s32 regNum, void *addr) {
    u32 bits = *(u32*) addr;
    s32 exponent = (((bits & 0x7f800000U) >> 0x17) - 0x7F);

    if ((exponent >= -0x7E && exponent <= 0x7F) || (bits == 0x0)) {
        f32 val = *(f32*) addr;
        crash_screen_print(x, y, "@3FC07FFFF%02d:%s@FFFFFFFF%.3e", regNum, ((val < 0) ? "" : " "), val);
    } else {
        crash_screen_print(x, y, "@3FC07FFFF%02d:@FFFFFFFF%08XD", regNum, bits);
    }
}

void crash_screen_print_fpcsr(u32 fpcsr) {
    s32 i;
    u32 bit = BIT(17);

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

    crash_screen_print(TEXT_X(0 * 15), TEXT_Y( 3), "@3FC07FFF%s:@FFFFFFFF%08X", "PC", (u32) tc->pc);
    crash_screen_print(TEXT_X(1 * 15), TEXT_Y( 3), "@3FC07FFF%s:@FFFFFFFF%08X", "SR", (u32) tc->sr);
    crash_screen_print(TEXT_X(2 * 15), TEXT_Y( 3), "@3FC07FFF%s:@FFFFFFFF%08X", "VA", (u32) tc->badvaddr);

    crash_screen_print(TEXT_X(2 * 15), TEXT_Y(13), "@3FC07FFF%s:@FFFFFFFF%08X", "MM", *(u32*)tc->pc);

    osWritebackDCacheAll();

    for (s32 y = 0; y < 10; y++) {
        for (s32 x = 0; x < 3; x++) {
            crash_screen_print(TEXT_X(x * 15), TEXT_Y(4 + y), "@3FC07FFF%s:@FFFFFFFF%08X", gRegNames[regNum], (u32) *(reg + regNum));

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

    s32 cause = ((tc->cause >> 2) & 0x1F);
    if (cause == (EXC_WATCH >> 2)) cause = 16;
    if (cause == (EXC_VCED  >> 2)) cause = 17;

    crash_screen_print(TEXT_X( 0), TEXT_Y(1), "@7F7FFFFFTHREAD:%d", thread->id);
    crash_screen_print(TEXT_X(10), TEXT_Y(1), "@FF3F00FF(%s)", gCauseDesc[cause]);

    osWritebackDCacheAll();

    if ((u32) parse_map != MAP_PARSER_ADDRESS) {
        char *fname = parse_map(tc->pc);
        crash_screen_print(TEXT_X(0), TEXT_Y(2), "@FF7F7FFFCRASH AT:");
        if (fname == NULL) {
            crash_screen_print(TEXT_X(10), TEXT_Y(2), "@7F7F7FFF%s", "UNKNOWN");
        } else {
            crash_screen_print(TEXT_X(10), TEXT_Y(2), "@FFFF7FFF%s", fname);
        }
    }

    crash_screen_print_registers(tc);

    osWritebackDCacheAll();

    crash_screen_print_float_registers(tc);
}

void draw_assert(UNUSED OSThread *thread) {
    crash_screen_print(TEXT_X(0), TEXT_Y(1), "ASSERT PAGE");

    crash_screen_draw_divider(DIVIDER_Y(2));

    if (__n64Assert_Filename != NULL) {
        crash_screen_print(TEXT_X(0), TEXT_Y(2), "FILE: %s LINE %d", __n64Assert_Filename, __n64Assert_LineNum);
        crash_screen_print(TEXT_X(0), TEXT_Y(4), "MESSAGE:");
        crash_screen_print(TEXT_X(0), (TEXT_Y(5) + 5), " %s", __n64Assert_Message);
    } else {
        crash_screen_print(TEXT_X(0), TEXT_Y(2), "no failed assert to report.");
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
    u32 temp_sp = (tc->sp + 0x14);
    s32 currIndex;
    u32 faddr;
    char *fname;
    struct FunctionInStack *functionList = (sStackTraceSkipUnknowns ? sKnownFunctionStack : sAllFunctionStack);
    struct FunctionInStack *function = NULL;

    crash_screen_print(TEXT_X(0), TEXT_Y(1), "STACK TRACE FROM %08X:", temp_sp);
    crash_screen_print(TEXT_X(0), TEXT_Y(2), "@FF7F7FFFCURRFUNC:");

    if ((u32) parse_map == MAP_PARSER_ADDRESS) {
        crash_screen_print(TEXT_X(9), TEXT_Y(2), "NONE");
    } else {
        crash_screen_print(TEXT_X(9), TEXT_Y(2), "@FFFF7FFF%s", parse_map(tc->pc));
    }

    osWritebackDCacheAll();

    // Print
    for (s32 j = 0; j < STACK_TRACE_NUM_ROWS; j++) {
        s32 y = TEXT_Y(3 + j);

        if ((u32) find_function_in_stack == MAP_PARSER_ADDRESS) {
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

        if (!sStackTraceSkipUnknowns && ((fname == NULL) || ((*(u32*)faddr & 0x80000000) == 0))) {
            // Print unknown function
            crash_screen_print(TEXT_X(9), y, "@C0C0C0FF%08X", *(u32*)faddr);
        } else {
            // Print known function
            if (sStackTraceShowNames) {
                crash_screen_print(TEXT_X(9), y, "@FFFFC0FF%s", fname);
            } else {
                crash_screen_print(TEXT_X(9), y, "@FFFFC0FF%08X", *(u32*)faddr);
            }
        }
    }

    osWritebackDCacheAll();

    crash_screen_draw_divider(DIVIDER_Y( 2));
    crash_screen_draw_divider(DIVIDER_Y( 3));
    crash_screen_draw_divider(DIVIDER_Y(21));
    crash_screen_print(TEXT_X(0), TEXT_Y(21), "@C0C0C0FFup/down:scroll    toggle: a:names b:unknowns");

    // Scroll Bar
    crash_screen_draw_scroll_bar(DIVIDER_Y(3), DIVIDER_Y(21), STACK_TRACE_NUM_ROWS, sNumShownFunctions, sStackTraceIndex, 4, COLOR_RGBA16_LIGHT_GRAY);

    osWritebackDCacheAll();
}

#define JUMP_MENU_W (TEXT_WIDTH(8))
#define JUMP_MENU_H (TEXT_HEIGHT(1))

#define JUMP_MENU_X (SCREEN_CENTER_X - (JUMP_MENU_W / 2))
#define JUMP_MENU_Y (SCREEN_CENTER_Y - (JUMP_MENU_H / 2))

void draw_address_select(void) {
    crash_screen_draw_rect(JUMP_MENU_X -  5, JUMP_MENU_Y - TEXT_HEIGHT(2) -  5,
                           JUMP_MENU_W + 10, JUMP_MENU_H + TEXT_HEIGHT(2) + 10,
                           COLOR_RGBA16_CRASH_BACKGROUND, TRUE);

    crash_screen_print(JUMP_MENU_X + TEXT_WIDTH(1), JUMP_MENU_Y - TEXT_HEIGHT(2), "GO TO:");

    crash_screen_draw_triangle(JUMP_MENU_X + (sAddressSelecCharIndex * TEXT_WIDTH(1)), JUMP_MENU_Y - TEXT_HEIGHT(1),
                               TEXT_WIDTH(1), TEXT_WIDTH(1),
                               COLOR_RGBA16_CRASH_CURRFUNC, FALSE);
    crash_screen_draw_triangle(JUMP_MENU_X + (sAddressSelecCharIndex * TEXT_WIDTH(1)), JUMP_MENU_Y + TEXT_HEIGHT(1),
                               TEXT_WIDTH(1), TEXT_WIDTH(1),
                               COLOR_RGBA16_CRASH_CURRFUNC, TRUE);
    crash_screen_print(JUMP_MENU_X, JUMP_MENU_Y, "%08X", sAddressSelect);
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

    u32 addr = sProgramPosition;
    u32 currAddr = addr;

    s32 charX, charY;

    crash_screen_print(TEXT_X( 0), TEXT_Y(1), "@FF7F00FFSELECTED ADDRESS:");
    crash_screen_print(TEXT_X(17), TEXT_Y(1), "%08X", addr);
    crash_screen_draw_divider(DIVIDER_Y(2));
    crash_screen_print(TEXT_X( 1), TEXT_Y(2), "MEMORY");

    charX = TEXT_X(8) + 3;

    for (s32 i = 0; i < 16; i++) {
        if ((i % 4) == 0) {
            charX += 2;
        }
        crash_screen_print(charX, TEXT_Y(2), ((i & 0x1) ? "@00B7FFFF%02X" : "@0087FFFF%02X"), i);
        charX += TEXT_WIDTH(2) + 1;
    }
    crash_screen_draw_divider(DIVIDER_Y(3));

    crash_screen_draw_rect(TEXT_X(8) + 2, DIVIDER_Y(2), 1, TEXT_HEIGHT(19), COLOR_RGBA16_LIGHT_GRAY, FALSE);

    charX = TEXT_X(8) + 3;
    charY = TEXT_Y(3);

    for (s32 y = 0; y < RAM_VIEWER_NUM_ROWS; y++) {
        currAddr = addr + (y * 0x10);
        crash_screen_print(TEXT_X(0), TEXT_Y(3 + y), ((y & 0x1) ? "@5FDF5FFF%08X" : "@1F9F1FFF%08X"), currAddr);

        charX = TEXT_X(8) + 3;
        charY = TEXT_Y(3 + y);
        for (s32 x = 0; x < 16; x++) {
            u8 value = *((u8 *)currAddr + x);
            if ((x % 4) == 0) {
                charX += 2;
            }
            if (sRamViewerShowAscii) {
                crash_screen_draw_glyph(charX, charY, value, COLOR_RGBA16_CRASH_WHITE);
            } else {
                crash_screen_print(charX, charY, ((x & 0x1) ? "@FFFFFFFF%02X" : "@BFBFBFFF%02X"), value);
            }
            charX += TEXT_WIDTH(2) + 1;
        }
    }

    crash_screen_draw_divider(DIVIDER_Y(21));
    crash_screen_print(TEXT_X(0), TEXT_Y(21), "@C0C0C0FFup/down:scroll        a:jump  b:toggle ascii");

    // Scroll bar
    crash_screen_draw_scroll_bar(DIVIDER_Y(3), DIVIDER_Y(21), RAM_VIEWER_SHOWN_SECTION, TOTAL_RAM_SIZE, (sProgramPosition - RAM_VIEWER_SCROLL_MIN), 4, COLOR_RGBA16_LIGHT_GRAY);

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
        u32 addr = (sProgramPosition + (i * 4));
        u32 toDisasm = *(u32*)(addr);

        crash_screen_print(TEXT_X(0), TEXT_Y(2 + i), "%s", insn_disasm(toDisasm, (addr == tc->pc)));
    }

    osWritebackDCacheAll();

    crash_screen_draw_divider(DIVIDER_Y( 2));
    crash_screen_draw_divider(DIVIDER_Y(21));
    crash_screen_print(TEXT_X(0), TEXT_Y(21), "@C0C0C0FFup/down:scroll");

    // Scroll bar
    crash_screen_draw_scroll_bar(DIVIDER_Y(2), DIVIDER_Y(21), DISASM_SHOWN_SECTION, TOTAL_RAM_SIZE, (sProgramPosition - DISASM_SCROLL_MIN), 4, COLOR_RGBA16_LIGHT_GRAY);

    // Scroll bar crash position marker
    crash_screen_draw_scroll_bar(DIVIDER_Y(2), DIVIDER_Y(21), DISASM_SHOWN_SECTION, TOTAL_RAM_SIZE, (tc->pc - DISASM_SCROLL_MIN), 1, COLOR_RGBA16_CRASH_CURRFUNC);

    osWritebackDCacheAll();
}

void draw_controls(UNUSED OSThread *thread) {
    crash_screen_print(TEXT_X(1), TEXT_Y( 2), "CRASH SCREEN CONTROLS");
    crash_screen_print(TEXT_X(2), TEXT_Y( 4), "START:");
    crash_screen_print(TEXT_X(2), TEXT_Y( 5), "@C0C0C0FFtoggle framebuffer background");
    crash_screen_print(TEXT_X(2), TEXT_Y( 7), "Z:");
    crash_screen_print(TEXT_X(2), TEXT_Y( 8), "@C0C0C0FFtoggle framebuffer only view");
    crash_screen_print(TEXT_X(2), TEXT_Y(10), "ANALOG STICK, D-PAD, OR C BUTTONS:");
    crash_screen_print(TEXT_X(3), TEXT_Y(12), "LEFT/RIGHT:");
    crash_screen_print(TEXT_X(3), TEXT_Y(13), "@C0C0C0FFswitch page");
    crash_screen_print(TEXT_X(3), TEXT_Y(15), "UP/DOWN:");
    crash_screen_print(TEXT_X(3), TEXT_Y(16), "@C0C0C0FFscroll page");

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
        crash_screen_draw_rect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, COLOR_RGBA16_BLACK, FALSE);
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

void update_crash_screen_page(void) {
    if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_RIGHT) {
        sCrashPage++;
        sUpdateBuffer = TRUE;
    }
    if (sCrashScreenDirectionFlags & CRASH_SCREEN_INPUT_DIRECTION_FLAG_PRESSED_LEFT) {
        sCrashPage--;
        sUpdateBuffer = TRUE;
    }
}

void crash_screen_input_stacktrace(void) {
    update_crash_screen_page();

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

        u32 nextSelectedAddress = sAddressSelect;

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
        update_crash_screen_page();

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

void crash_screen_input_disasm(void) {
    update_crash_screen_page();

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

struct CrashScreenPage sCrashScreenPages[] = {
    /*PAGE_CONTEXT   */ {draw_crash_context, update_crash_screen_page},
    /*PAGE_ASSERTS   */ {draw_assert,        update_crash_screen_page},
#ifdef PUPPYPRINT_DEBUG
    /*PAGE_LOG       */ {draw_crash_log,    update_crash_screen_page},
#endif
    /*PAGE_STACKTRACE*/ {draw_stacktrace,   crash_screen_input_stacktrace},
    /*PAGE_RAM_VIEWER*/ {draw_ram_viewer,   crash_screen_input_ram_viewer},
    /*PAGE_DISASM    */ {draw_disasm,       crash_screen_input_disasm},
    /*PAGE_CONTROLS  */ {draw_controls,     crash_screen_input_disasm},
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

        if ((sCrashPage >= PAGE_COUNT) && (sCrashPage != PAGES_MAX)) {
            sCrashPage = PAGE_CONTEXT;
        }
        if (sCrashPage == PAGES_MAX) {
            sCrashPage = (PAGE_COUNT - 1);
        }
    }
}

void draw_crash_screen(OSThread *thread) {
    u8 prevPage = sCrashPage;

    update_crash_screen_input();

    if (sUpdateBuffer) {
        if (sCrashPage != prevPage) {
            sProgramPosition = 0;
            sStackTraceIndex = 0;
        }

        reset_crash_screen_framebuffer();

        if (sDrawCrashScreen) {
            if (sDrawFrameBuffer) {
                crash_screen_draw_rect(CRASH_SCREEN_X1, CRASH_SCREEN_Y1, CRASH_SCREEN_W, CRASH_SCREEN_H, COLOR_RGBA16_CRASH_BACKGROUND, TRUE);
            }
            crash_screen_print(TEXT_X( 0), TEXT_Y(0), "@C0C0C0FFHackerSM64 v%s", HACKERSM64_VERSION);
            crash_screen_print(TEXT_X(35), TEXT_Y(0), "@C0C0C0FF<Page:%02d>", sCrashPage);
            crash_screen_draw_divider(DIVIDER_Y(1));
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
        if (thread->priority > OS_PRIORITY_IDLE
         && thread->priority <= OS_PRIORITY_APPMAX
         && ((thread->flags & (BIT(0) | BIT(1))) != 0)) {
            return thread;
        }
        thread = thread->tlnext;
    }
    return NULL;
}

void fill_function_stack_trace(OSThread *thread) {
    __OSThreadContext *tc = &thread->context;
    u32 temp_sp = (tc->sp + 0x14);
    struct FunctionInStack *function = NULL;
    char *fname;

    if ((u32) find_function_in_stack == MAP_PARSER_ADDRESS) {
        return;
    }

    // Fill the stack buffer.
    for (s32 i = 0; i < STACK_SIZE; i++) {
        fname = find_function_in_stack(&temp_sp);

        function = &sAllFunctionStack[i];
        function->addr = temp_sp;
        function->name = fname;

        if (!((fname == NULL) || ((*(u32*)temp_sp & 0x80000000) == 0))) {
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
    u32 size = (u32) (segEnd - segStart);
    u8 *fb_u8 = (u8*) ((u32) fb_u16 + (SCREEN_SIZE * sizeof(RGBA16*)) - size);

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
                if ((u32) map_data_init != MAP_PARSER_ADDRESS) {
                    map_data_init();
                }
                fill_function_stack_trace(thread);
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
