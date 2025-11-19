#include <ultra64.h>
#include <PR/os_internal_error.h>
#include <PR/os_system.h>
#include <stdarg.h>
#include <string.h>
#include "config/config_debug.h"
#include "buffers/framebuffers.h"
#include "types.h"
#include "game/puppyprint.h"
#include "audio/external.h"
#include "farcall.h"
#include "game/game_init.h"
#include "game/main.h"
#include "game/debug.h"
#include "game/rumble_init.h"
#include "game/printf.h"

#include "crash_screen.h"
#include "map_parser.h"
#include "disasm.h"
#include "assert.h"
#include "stacktrace.h"

#include "sm64.h"

extern u16 sRenderedFramebuffer;
extern struct SequenceQueueItem sBackgroundMusicQueue[6];
extern void audio_signal_game_loop_tick(void);
extern void stop_sounds_in_continuous_banks(void);
extern void read_controller_inputs(s32 threadID);
extern char *strstr(char *, char *);

static char *crashPageNames[] = {
    [CRASH_SCREEN_PAGE_SIMPLE] = "(Overview)",
    [CRASH_SCREEN_PAGE_CONTEXT] = "(Context)",
    [CRASH_SCREEN_PAGE_STACKTRACE] = "(Stack Trace)",
#ifdef PUPPYPRINT_DEBUG
    [CRASH_SCREEN_PAGE_LOG] = "(Log)",
#endif
    [CRASH_SCREEN_PAGE_DISASM] = "(Disassembly)",
    [CRASH_SCREEN_PAGE_ASSERTS] = "(Assert)",
};

static u8 sCrashScreenCharToGlyph[128] = {
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
    64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
};

static u32 sCrashScreenFont[CRASH_SCREEN_GLYPH_HEIGHT * CRASH_SCREEN_FONT_ROWS * 2 + 1] = {
    #include "textures/crash_custom/crash_screen_font.ia1.inc.c"
};

static u8 crashPage = 0;
static u8 updateBuffer = TRUE;

static char crashScreenBuf[0x200];

/**
 * Verbose descriptions for exception causes.
 */
char *gCauseDesc[18] = {
    [EXC_INT     >> 2] = "Interrupt",
    [EXC_MOD     >> 2] = "TLB modification",
    [EXC_RMISS   >> 2] = "TLB exception on load",
    [EXC_WMISS   >> 2] = "TLB exception on store",
    [EXC_RADE    >> 2] = "Address error on load",
    [EXC_WADE    >> 2] = "Address error on store",
    [EXC_IBE     >> 2] = "Bus error on inst.",
    [EXC_DBE     >> 2] = "Bus error on data",
    [EXC_SYSCALL >> 2] = "Failed Assert: See Assert Page",
    [EXC_BREAK   >> 2] = "Breakpoint exception",
    [EXC_II      >> 2] = "Reserved instruction",
    [EXC_CPU     >> 2] = "Coprocessor unusable",
    [EXC_OV      >> 2] = "Arithmetic overflow",
    [EXC_TRAP    >> 2] = "Trap exception",
    [EXC_VCEI    >> 2] = "Virtual coherency on inst.",
    [EXC_FPE     >> 2] = "Floating point exception",
    // These two exceptions are not enumerated like the ones above.
    //  They take values 23 and 31, respectively, so here they're just placed
    //   after the ones that do enumerate nicely.
    /*EXC_WATCH       */ "Watchpoint exception",
    /*EXC_VCED        */ "Virtual coherency on data",
};

char *gFpcsrDesc[6] = {
    "Unimplemented operation", "Invalid operation", "Division by zero", "Overflow", "Underflow",
    "Inexact operation",
};

static u32 sProgramPosition = 0;
static u32 sCrashScreenStackTraceCount = 0;

static u16 gCrashScreenTextColor = 0xFFFF;
static u32 sCrashScreenPrintRow_Pixels = 0;
static u32 sCrashScreenPrintLnHeight_Pixels = CRASH_SCREEN_GLYPH_HEIGHT;
static struct {
    OSThread thread;
    u64 stack[THREAD2_STACK / sizeof(u64)];
    OSMesgQueue mesgQueue;
    OSMesg mesg;
    u16 *framebuffer;
    u16 width;
    u16 height;
} gCrashScreen;



/**
 * Splits a path string by the containing folder and the name of the file itself.
 */
char *crash_screen_ellide_string(char *str, u32 truncateLength) {
    u32 string_length = strlen(str);

    if (truncateLength >= string_length) {
        return str;
    }

    str[string_length - truncateLength - 4] = '(';
    str[string_length - truncateLength - 3] = '.';
    str[string_length - truncateLength - 2] = '.';
    str[string_length - truncateLength - 1] = '.';
    str[string_length - truncateLength - 0] = ')';
    return &str[string_length - truncateLength - 4];
}

static void set_text_color(u32 r, u32 g, u32 b) {
    gCrashScreenTextColor = GPACK_RGBA5551(r, g, b, 255);
}

static void reset_text_color(void) {
    gCrashScreenTextColor = 0xFFFF;
}

/**
 * Sets the screen row, in pixels, at which the next print
 *  will be placed on the Y axis.
 */
static void crash_screen_set_print_top(u32 row) {
    sCrashScreenPrintRow_Pixels = row;
}

static void crash_screen_set_println_height(u32 height_pixels) {
    sCrashScreenPrintLnHeight_Pixels = height_pixels;
}

static void crash_screen_reset_println_height(void) {
    sCrashScreenPrintLnHeight_Pixels = CRASH_SCREEN_GLYPH_HEIGHT;
}

void crash_screen_draw_rect(s32 x, s32 y, s32 w, s32 h) {
    u16 *ptr;
    s32 i, j;

    ptr = gCrashScreen.framebuffer + gCrashScreen.width * y + x;
    for (i = 0; i < h; i++) {
        for (j = 0; j < w; j++) {
            /**
             * Instead of setting the framebuffer pixels fully dark,
             * SM64 "darkens" the RGBA5551 pixel. This is done by
             * shifting every RGB component right by 2 in one operation,
             * essentially setting the brightness to 1/4.
             */

            // 0xe738 = 0b1110011100111000
            *ptr = ((*ptr & 0xe738) >> 2) | 1;
            ptr++;
        }
        ptr += gCrashScreen.width - w;
    }
}

void crash_screen_draw_glyph(s32 x, s32 y, s32 glyph) {
    const u32 *data;
    u16 *ptr;
    u32 bit;
    u32 rowMask;
    s32 i, j;
    u32 startOfGlyphData = 0;

    if (glyph > 0x7F) return;

    startOfGlyphData = (glyph & 0xF) * CRASH_SCREEN_GLYPH_HEIGHT * 2;
    if (glyph >= 64) {
        startOfGlyphData++;
    }

    data = &sCrashScreenFont[startOfGlyphData];

    ptr = gCrashScreen.framebuffer + gCrashScreen.width * y + x;

    u16 color = gCrashScreenTextColor;

    for (i = 0; i < CRASH_SCREEN_GLYPH_HEIGHT; i++) {
        bit = 0x80000000U >> ((glyph >> 4) * CRASH_SCREEN_GLYPH_WIDTH);
        rowMask = *data++;
        data++;

        for (j = 0; j < (CRASH_SCREEN_GLYPH_WIDTH); j++) {
            if (bit & rowMask) {
                *ptr = color;
            }
            ptr++;
            bit >>= 1;
        }
        ptr += gCrashScreen.width - (CRASH_SCREEN_GLYPH_WIDTH);
    }
    osWritebackDCacheAll();
}

static char *write_to_buf(char *buffer, const char *data, size_t size) {
    return (char *) memcpy(buffer, data, size) + size;
}

/**
 * crash_screen_print, but automatically places a newline
 *  if text is xNewline units away from the right edge of the screen.
 *  Also respects newline characters in the evaluated string.
 * 
 * Returns the number of newlines that were in the string, or created.
 */
int crash_screen_print_with_newlines(s32 x, s32 y, const s32 xNewline, const char *fmt, ...) {
    char *ptr;
    u32 glyph;
    s32 size;
    s32 xOffset = x;
    int numNewlines = 0;

    va_list args;
    va_start(args, fmt);

    size = _Printf(write_to_buf, crashScreenBuf, fmt, args);

    if (size > 0) {
        ptr = crashScreenBuf;

        while (*ptr && size-- > 0) {
            if (xOffset >= SCREEN_WIDTH - (xNewline + CRASH_SCREEN_X_KERNING)) {
                y += 10;
                xOffset = xNewline;
                numNewlines += 1;
            }

            glyph = sCrashScreenCharToGlyph[*ptr & 0x7f];

            if (*ptr == '\n') {
                y += 10;
                xOffset = x;
                ptr++;
                numNewlines += 1;
                continue;
            } else if (*ptr == '\t') {
                osSyncPrintf("TAB %d ->", xOffset);
                xOffset += CRASH_SCREEN_GLYPH_WIDTH * (CRASH_SCREEN_TAB_WIDTH_CHARS - 1);
                osSyncPrintf("%d\n", xOffset);

                ptr++;
                continue;
            } else if (glyph != 0xff) {
                crash_screen_draw_glyph(xOffset, y, glyph);
            }

            ptr++;
            xOffset += CRASH_SCREEN_X_KERNING;
        }
    }

    va_end(args);

    return numNewlines;
}

void crash_screen_print(s32 x, s32 y, const char *fmt, ...) {
    char *ptr;
    u32 glyph;
    s32 size;

    va_list args;
    va_start(args, fmt);

    size = _Printf(write_to_buf, crashScreenBuf, fmt, args);

    if (size > 0) {
        ptr = crashScreenBuf;

        while (*ptr && size-- > 0) {
            if (*ptr == '\t') {
                ptr++;
                x += CRASH_SCREEN_GLYPH_WIDTH * (CRASH_SCREEN_TAB_WIDTH_CHARS - 1);
                continue;
            }
            glyph = sCrashScreenCharToGlyph[*ptr & 0x7f];

            if (glyph != 0xff) {
                crash_screen_draw_glyph(x, y, glyph);
            }

            ptr++;
            x += CRASH_SCREEN_X_KERNING;
        }
    }

    va_end(args);
}

void crash_screen_println(const char *fmt, ...) {
    char *ptr;
    u32 glyph;
    s32 size;
    s32 x = CRASH_SCREEN_LEFT_MARGIN;
    s32 y = sCrashScreenPrintRow_Pixels;

    va_list args;
    va_start(args, fmt);

    size = _Printf(write_to_buf, crashScreenBuf, fmt, args);

    if (size > 0) {
        ptr = crashScreenBuf;

        while (*ptr && size-- > 0) {
            if (*ptr == '\t') {
                ptr++;
                x += CRASH_SCREEN_GLYPH_WIDTH * (CRASH_SCREEN_TAB_WIDTH_CHARS - 1);
                continue;
            }

            glyph = sCrashScreenCharToGlyph[*ptr & 0x7f];

            if (glyph != 0xff) {
                crash_screen_draw_glyph(x, y, glyph);
            }

            ptr++;
            x += CRASH_SCREEN_X_KERNING;
        }
    }

    va_end(args);
    sCrashScreenPrintRow_Pixels += sCrashScreenPrintLnHeight_Pixels;
}

void crash_screen_sleep(s32 ms) {
    u64 cycles = ms * 1000LL * osClockRate / 1000000ULL;
    osSetTime(0);
    while (osGetTime() < cycles) { }
}

void crash_screen_print_float_reg(s32 x, s32 y, s32 regNum, void *addr) {
    u32 bits = *(u32 *) addr;
    s32 exponent = ((bits & 0x7f800000U) >> 0x17) - 0x7F;

    if ((exponent >= -0x7E && exponent <= 0x7F) || bits == 0x0) {
        crash_screen_print(x, y, "F%02d:%.3e",  regNum, *(f32 *) addr);
    } else {
        crash_screen_print(x, y, "F%02d:%08XD", regNum, *(u32 *) addr);
    }
}

void crash_screen_print_fpcsr(u32 fpcsr) {
    s32 i;
    u32 bit = BIT(17);

    crash_screen_print(CRASH_SCREEN_LEFT_MARGIN + 90, 220, "FPCSR:%08XH", fpcsr);
    for (i = 0; i < 6; i++) {
        if (fpcsr & bit) {
            crash_screen_print(222, 220, "(%s)", gFpcsrDesc[i]);
            return;
        }
        bit >>= 1;
    }
}

void draw_crash_overview(OSThread *thread, s32 cause) {
    __OSThreadContext *tc = &thread->context;

    int numNewlines = 0;

    crash_screen_draw_rect(0, CRASH_SCREEN_RECT_BOUNDARY_Y, SCREEN_WIDTH, SCREEN_HEIGHT);

    set_text_color(0xFF, 0, 0);
    crash_screen_println("Game crashed!");
    reset_text_color();

    set_text_color(241, 196, 15);
    crash_screen_println("Thread:");
    crash_screen_println("Cause:");
    reset_text_color();
    sCrashScreenPrintRow_Pixels -= (2 * CRASH_SCREEN_GLYPH_HEIGHT);
    crash_screen_println("        %d", thread->id);
    crash_screen_println("       %s", gCauseDesc[cause]);

#ifdef DEBUG_EXPORT_SYMBOLS
    symtable_info_t info = get_symbol_info(tc->pc);

    if (info.line != -1) {
        char file_line[CRASH_SCREEN_MAX_PATH];

        set_text_color(241, 196, 15);
#ifdef DEBUG_EXPORT_ALL_LINES
        sprintf(file_line, "%s:%d", info.file, info.line);
#else  // DEBUG_EXPORT_ALL_LINES
        sprintf(file_line, "%s", info.file);
#endif // DEBUG_EXPORT_ALL_LINES
        sCrashScreenPrintRow_Pixels += 5;
        crash_screen_println("File: ");
        reset_text_color();
        
        numNewlines = crash_screen_print_with_newlines(
                                    CRASH_SCREEN_LEFT_MARGIN + (CRASH_SCREEN_GLYPH_WIDTH * 5),
                                    sCrashScreenPrintRow_Pixels - CRASH_SCREEN_GLYPH_HEIGHT,
                                    CRASH_SCREEN_LEFT_MARGIN,
                                    file_line
                      );
        sCrashScreenPrintRow_Pixels += (numNewlines * CRASH_SCREEN_GLYPH_HEIGHT);
    }
#endif // DEBUG_EXPORT_SYMBOLS

#if defined(DEBUG_EXPORT_SYMBOLS) && defined(DEBUG_FULL_STACK_TRACE)
    if (stackTraceGenerated) {
        set_text_color(241, 196, 15);
        crash_screen_println("Stack Trace: ");
        reset_text_color();

        // print current func
        crash_screen_println("%08X: %s", tc->pc, info.func == NULL ? "Unknown" : info.func);
        // print last func
        u32 ret_addr = tc->ra;
        symtable_info_t ra_info = get_symbol_info(ret_addr);
        crash_screen_println("%08X: %s:%d", ret_addr, ra_info.func == NULL ? "Unknown" : ra_info.func, ra_info.line);
        // print up to 3 more
        for (u32 i = 0; i < MIN(3, sCrashScreenStackTraceCount); i++) {
            crash_screen_println(get_stack_entry(i));
        }
    }
#else // defined(DEBUG_EXPORT_SYMBOLS) && defined(DEBUG_FULL_STACK_TRACE)
    crash_screen_println("Address: 0x%08X", tc->pc);
#endif // defined(DEBUG_EXPORT_SYMBOLS) && defined(DEBUG_FULL_STACK_TRACE)
}

void draw_crash_context(OSThread *thread, s32 cause) {
    __OSThreadContext *tc = &thread->context;
    crash_screen_draw_rect(0, CRASH_SCREEN_RECT_BOUNDARY_Y, SCREEN_WIDTH, SCREEN_HEIGHT);

    crash_screen_println("Thread:%d (%s)", thread->id, gCauseDesc[cause]);
    crash_screen_set_println_height(10);
    crash_screen_println("PC:%08XH   SR:%08XH   VA:%08XH", tc->pc, tc->sr, tc->badvaddr);
    osWritebackDCacheAll();
#ifdef DEBUG_EXPORT_SYMBOLS
    char *fname = parse_map(tc->pc, TRUE);
    crash_screen_println("Crash at: %s", fname == NULL ? "Unknown" : fname);
#endif // DEBUG_EXPORT_SYMBOLS
    crash_screen_println("AT:%08XH   V0:%08XH   V1:%08XH", (u32) tc->at, (u32) tc->v0, (u32) tc->v1);
    crash_screen_println("A0:%08XH   A1:%08XH   A2:%08XH", (u32) tc->a0, (u32) tc->a1, (u32) tc->a2);
    crash_screen_println("A3:%08XH   T0:%08XH   T1:%08XH", (u32) tc->a3, (u32) tc->t0, (u32) tc->t1);
    crash_screen_println("T2:%08XH   T3:%08XH   T4:%08XH", (u32) tc->t2, (u32) tc->t3, (u32) tc->t4);
    crash_screen_println("T5:%08XH   T6:%08XH   T7:%08XH", (u32) tc->t5, (u32) tc->t6, (u32) tc->t7);
    crash_screen_println("S0:%08XH   S1:%08XH   S2:%08XH", (u32) tc->s0, (u32) tc->s1, (u32) tc->s2);
    crash_screen_println("S3:%08XH   S4:%08XH   S5:%08XH", (u32) tc->s3, (u32) tc->s4, (u32) tc->s5);
    crash_screen_println("S6:%08XH   S7:%08XH   T8:%08XH", (u32) tc->s6, (u32) tc->s7, (u32) tc->t8);
    crash_screen_println("T9:%08XH   GP:%08XH   SP:%08XH", (u32) tc->t9, (u32) tc->gp, (u32) tc->sp);
    crash_screen_println("S8:%08XH   RA:%08XH",            (u32) tc->s8, (u32) tc->ra);
#ifdef DEBUG_EXPORT_SYMBOLS
    fname = parse_map(tc->ra, TRUE);
    crash_screen_println("RA at: %s", fname == NULL ? "Unknown" : fname);
#endif // DEBUG_EXPORT_SYMBOLS

    crash_screen_print_fpcsr(tc->fpcsr);

    osWritebackDCacheAll();
    crash_screen_print_float_reg(CRASH_SCREEN_LEFT_MARGIN +   0, 170,  0, &tc->fp0.f.f_even);
    crash_screen_print_float_reg(CRASH_SCREEN_LEFT_MARGIN +  90, 170,  2, &tc->fp2.f.f_even);
    crash_screen_print_float_reg(CRASH_SCREEN_LEFT_MARGIN + 180, 170,  4, &tc->fp4.f.f_even);
    crash_screen_print_float_reg(CRASH_SCREEN_LEFT_MARGIN +   0, 180,  6, &tc->fp6.f.f_even);
    crash_screen_print_float_reg(CRASH_SCREEN_LEFT_MARGIN +  90, 180,  8, &tc->fp8.f.f_even);
    crash_screen_print_float_reg(CRASH_SCREEN_LEFT_MARGIN + 180, 180, 10, &tc->fp10.f.f_even);
    crash_screen_print_float_reg(CRASH_SCREEN_LEFT_MARGIN +   0, 190, 12, &tc->fp12.f.f_even);
    crash_screen_print_float_reg(CRASH_SCREEN_LEFT_MARGIN +  90, 190, 14, &tc->fp14.f.f_even);
    crash_screen_print_float_reg(CRASH_SCREEN_LEFT_MARGIN + 180, 190, 16, &tc->fp16.f.f_even);
    crash_screen_print_float_reg(CRASH_SCREEN_LEFT_MARGIN +   0, 200, 18, &tc->fp18.f.f_even);
    crash_screen_print_float_reg(CRASH_SCREEN_LEFT_MARGIN +  90, 200, 20, &tc->fp20.f.f_even);
    crash_screen_print_float_reg(CRASH_SCREEN_LEFT_MARGIN + 180, 200, 22, &tc->fp22.f.f_even);
    crash_screen_print_float_reg(CRASH_SCREEN_LEFT_MARGIN +   0, 210, 24, &tc->fp24.f.f_even);
    crash_screen_print_float_reg(CRASH_SCREEN_LEFT_MARGIN +  90, 210, 26, &tc->fp26.f.f_even);
    crash_screen_print_float_reg(CRASH_SCREEN_LEFT_MARGIN + 180, 210, 28, &tc->fp28.f.f_even);
    crash_screen_print_float_reg(CRASH_SCREEN_LEFT_MARGIN +   0, 220, 30, &tc->fp30.f.f_even);

    crash_screen_reset_println_height();
}


#ifdef PUPPYPRINT_DEBUG
void draw_crash_log(void) {
    s32 i;
    crash_screen_draw_rect(0, 20, SCREEN_WIDTH, 210);
    osWritebackDCacheAll();
#define LINE_HEIGHT (25 + ((LOG_BUFFER_SIZE - 1) * 10))
    for (i = 0; i < LOG_BUFFER_SIZE; i++) {
        crash_screen_println(consoleLogTable[i]);
    }
#undef LINE_HEIGHT
}
#endif

void draw_stacktrace(OSThread *thread, UNUSED s32 cause) {
    __OSThreadContext *tc = &thread->context;

    crash_screen_draw_rect(0, CRASH_SCREEN_RECT_BOUNDARY_Y, SCREEN_WIDTH, SCREEN_HEIGHT);
    crash_screen_println("Stack Trace from %08X:", (u32) tc->sp);

#if defined(DEBUG_EXPORT_SYMBOLS) && defined(DEBUG_FULL_STACK_TRACE)
    // Current Func (EPC)
    crash_screen_println("%08X (%s)", tc->pc, parse_map(tc->pc, TRUE));

    // Previous Func (RA)
    u32 ra = tc->ra;
    symtable_info_t info = get_symbol_info(ra);

    crash_screen_println("%08X (%s:%d)", ra, info.func, info.line);

    osWritebackDCacheAll();

    for (u32 i = 0; i < sCrashScreenStackTraceCount; i++) {
        crash_screen_println(get_stack_entry(i));
    }
#else // defined(DEBUG_EXPORT_SYMBOLS) && defined(DEBUG_FULL_STACK_TRACE)
    // simple stack trace
    u32 sp = tc->sp;

    for (int i = 0; i < STACK_LINE_COUNT; i++) {
        crash_screen_print(CRASH_SCREEN_LEFT_MARGIN, 55 + (i * 10), "%3d: %08X", i, *((u32*)(sp + (i * 4))));
        crash_screen_print(120, 55 + (i * 10), "%3d: %08X", i + STACK_LINE_COUNT, *((u32*)(sp + ((i + STACK_LINE_COUNT) * 4))));
    }
#endif // defined(DEBUG_EXPORT_SYMBOLS) && defined(DEBUG_FULL_STACK_TRACE)
}

void draw_disasm(OSThread *thread) {
    __OSThreadContext *tc = &thread->context;

    crash_screen_draw_rect(0, CRASH_SCREEN_RECT_BOUNDARY_Y, SCREEN_WIDTH, SCREEN_HEIGHT);
    if (sProgramPosition == 0) {
        sProgramPosition = (tc->pc - 36);
    }
    crash_screen_println("Program Counter: %08X", sProgramPosition);
    osWritebackDCacheAll();

    int skiplines = 0;
#ifdef DEBUG_EXPORT_SYMBOLS
    int currline = 0;
#endif // DEBUG_EXPORT_SYMBOLS

    u32 basePositionY = sCrashScreenPrintRow_Pixels;

    for (int i = 0; i < 19; i++) {
        u32 addr = (sProgramPosition + (i * 4));

        char *disasm = insn_disasm((InsnData *)addr);


        if (disasm[0] == 0) {
            crash_screen_print(CRASH_SCREEN_LEFT_MARGIN + 22, basePositionY + (skiplines * 10) + (i * 10), "%08X", addr);
        } else {
#ifdef DEBUG_EXPORT_SYMBOLS
            symtable_info_t info = get_symbol_info(addr);

            if (info.func_offset == 0 && info.distance == 0 && currline != info.line) {
                currline = info.line;
                set_text_color(239, 196, 15);
                crash_screen_print(CRASH_SCREEN_LEFT_MARGIN, basePositionY + (skiplines * 10) + (i * 10), "<%s:>", info.func);
                reset_text_color();
                skiplines++;
            }
#ifndef DEBUG_EXPORT_ALL_LINES
            // catch `jal` and `jalr` callsites
            if (disasm[0] == 'j' && disasm[1] == 'a') {
#endif // DEBUG_EXPORT_ALL_LINES
                if (info.line != -1) {
                    set_text_color(200, 200, 200);
                    crash_screen_print(CRASH_SCREEN_LEFT_MARGIN, basePositionY + (skiplines * 10) + (i * 10), "%d:", info.line);
                    reset_text_color();
                }
#ifndef DEBUG_EXPORT_ALL_LINES
            }
#endif // DEBUG_EXPORT_ALL_LINES

#endif // DEBUG_EXPORT_SYMBOLS
            if (addr == tc->pc) {
                set_text_color(255, 0, 0);
            } else {
                reset_text_color();
            }
            crash_screen_print(CRASH_SCREEN_LEFT_MARGIN + 22, basePositionY + (skiplines * 10) + (i * 10), "%s", disasm);
        }

    }

    reset_text_color();
    osWritebackDCacheAll();
}

void draw_assert(OSThread *thread) {
    __OSThreadContext *tc = &thread->context;
    crash_screen_draw_rect(0, CRASH_SCREEN_RECT_BOUNDARY_Y, SCREEN_WIDTH, SCREEN_HEIGHT);
    crash_screen_set_print_top(35);

    set_text_color(0xFF, 0, 0);
    crash_screen_println("Assert Failed!");
    reset_text_color();


    if (__n64Assert_Filename != NULL) {
        // print this on the same line as `File: ` but to its right
        char file_line[CRASH_SCREEN_MAX_PATH];
        sprintf(file_line, "%s:%d", __n64Assert_Filename, __n64Assert_LineNum);
        set_text_color(241, 196, 15);
        crash_screen_println("File/Line:");
        reset_text_color();
        int numNewlines = crash_screen_print_with_newlines(
                            CRASH_SCREEN_LEFT_MARGIN + (CRASH_SCREEN_GLYPH_WIDTH * 8),
                            sCrashScreenPrintRow_Pixels - CRASH_SCREEN_GLYPH_HEIGHT,
                            CRASH_SCREEN_LEFT_MARGIN,
                            file_line
                          );

        sCrashScreenPrintRow_Pixels += (numNewlines * CRASH_SCREEN_GLYPH_HEIGHT);

        // Print the assert condition that failed.
        set_text_color(241, 196, 15);
        crash_screen_println("Condition:");
        reset_text_color();
        numNewlines = crash_screen_print_with_newlines(
                              CRASH_SCREEN_LEFT_MARGIN + (CRASH_SCREEN_GLYPH_WIDTH * 8),
                              sCrashScreenPrintRow_Pixels - CRASH_SCREEN_GLYPH_HEIGHT,
                              CRASH_SCREEN_LEFT_MARGIN,
                              "(%s)",
                              __n64Assert_Condition
                          );

        sCrashScreenPrintRow_Pixels += (numNewlines * CRASH_SCREEN_GLYPH_HEIGHT);

        // Print the message, if assertf/aggressf/errorf were used.
        if (__n64Assert_MessageBuf[0] != 0) {
            set_text_color(241, 196, 15);
            crash_screen_println("Message:");
            reset_text_color();
            numNewlines = 
                crash_screen_print_with_newlines(
                    CRASH_SCREEN_LEFT_MARGIN + (CRASH_SCREEN_GLYPH_WIDTH * 7),
                    sCrashScreenPrintRow_Pixels - CRASH_SCREEN_GLYPH_HEIGHT,
                    CRASH_SCREEN_LEFT_MARGIN,
                    "%s",
                    __n64Assert_MessageBuf
                );
        }
        sCrashScreenPrintRow_Pixels += (numNewlines * CRASH_SCREEN_GLYPH_HEIGHT);
#if defined(DEBUG_EXPORT_SYMBOLS) && defined(DEBUG_FULL_STACK_TRACE)
        if (stackTraceGenerated) {
            set_text_color(241, 196, 15);
            crash_screen_println("Stack Trace:");
            reset_text_color();

            // Print last func (we know the current func is __n64Assert)
            u32 ret_addr = tc->ra;
            symtable_info_t ra_info = get_symbol_info(ret_addr);
            crash_screen_println("%08X: %s:%d", ret_addr, ra_info.func == NULL ? "Unknown" : ra_info.func, ra_info.line);
            // Print up to 3 more
            for (u32 i = 0; i < MIN(3, sCrashScreenStackTraceCount); i++) {
                crash_screen_println(get_stack_entry(i));
            }
        }
#else // defined(DEBUG_EXPORT_SYMBOLS) && defined(DEBUG_FULL_STACK_TRACE)
        // Print address of last func (we know the current func is __n64Assert)
        u32 ret_addr = tc->ra;
        set_text_color(241, 196, 15);
        crash_screen_println("Called From:");
        reset_text_color();
        crash_screen_println("\t0x%08X", ret_addr);
#endif // defined(DEBUG_EXPORT_SYMBOLS) && defined(DEBUG_FULL_STACK_TRACE)
    } else {
        crash_screen_println("No failed assert to report.");
    }

    osWritebackDCacheAll();
}

void draw_crash_screen(OSThread *thread) {
    __OSThreadContext *tc = &thread->context;

    s32 cause = ((tc->cause >> 2) & 0x1F);
    if (cause == 23) { // EXC_WATCH
        cause = 16;
    }
    if (cause == 31) { // EXC_VCED
        cause = 17;
    }

    if (gPlayer1Controller->buttonPressed & R_TRIG) {
        crashPage++;
        if (crashPage == CRASH_SCREEN_PAGE_ASSERTS && tc->cause != EXC_SYSCALL) crashPage++;
        updateBuffer = TRUE;
    }
    if (gPlayer1Controller->buttonPressed & (L_TRIG | Z_TRIG)) {
        crashPage--;
        if (crashPage == CRASH_SCREEN_PAGE_ASSERTS && tc->cause != EXC_SYSCALL) crashPage--;
        updateBuffer = TRUE;
    }

    if (crashPage == CRASH_SCREEN_PAGE_DISASM) {
        u32 sNewProgramPosition = sProgramPosition;
        if (gPlayer1Controller->buttonDown & D_CBUTTONS) {
            sNewProgramPosition += 4;
        }
        if (gPlayer1Controller->buttonDown & U_CBUTTONS) {
            sNewProgramPosition -= 4;
        }

        if (is_text_addr(sNewProgramPosition) && (sNewProgramPosition != sProgramPosition)) {
            // Hold B to speed up scrolling
            if (!(gPlayer1Controller->buttonDown & B_BUTTON)) {
                crash_screen_sleep(30);
            }
            sProgramPosition = sNewProgramPosition;
            updateBuffer = TRUE;
        }
    }

    if ((crashPage >= CRASH_SCREEN_PAGE_COUNT) && (crashPage != 255)) {
        crashPage = 0;
    }
    if (crashPage == 255) {
        crashPage = (CRASH_SCREEN_PAGE_COUNT - 1);
        // Do not navigate to the assert page if an assert didn't happen
        if (crashPage == CRASH_SCREEN_PAGE_ASSERTS && tc->cause != EXC_SYSCALL) crashPage--;
    }
    if (updateBuffer) {
        crash_screen_draw_rect(0, 0, SCREEN_WIDTH, CRASH_SCREEN_RECT_BOUNDARY_Y);
        crash_screen_set_print_top(CRASH_SCREEN_TOP_MARGIN);
        crash_screen_println("Page:%02d %-19s L/Z: Left   R: Right", crashPage, crashPageNames[crashPage]);
        switch (crashPage) {
            case CRASH_SCREEN_PAGE_SIMPLE:     draw_crash_overview(thread, cause); break;
            case CRASH_SCREEN_PAGE_CONTEXT:    draw_crash_context(thread, cause); break;
            case CRASH_SCREEN_PAGE_STACKTRACE: draw_stacktrace(thread, cause); break;
#ifdef PUPPYPRINT_DEBUG
            case CRASH_SCREEN_PAGE_LOG:        draw_crash_log(); break;
#endif
            case CRASH_SCREEN_PAGE_DISASM:     draw_disasm(thread); break;
            case CRASH_SCREEN_PAGE_ASSERTS:    draw_assert(thread); break;
        }

        osWritebackDCacheAll();
        osViBlack(FALSE);
        osViSwapBuffer(gCrashScreen.framebuffer);
        updateBuffer = FALSE;
    }
}

OSThread *get_crashed_thread(void) {
    OSThread *thread = __osGetCurrFaultedThread();

    while (thread->priority != -1) {
        if (thread->priority > OS_PRIORITY_IDLE && thread->priority < OS_PRIORITY_APPMAX
            && ((thread->flags & (BIT(0) | BIT(1))) != 0)) {
            return thread;
        }
        thread = thread->tlnext;
    }
    return NULL;
}

void thread2_crash_screen(UNUSED void *arg) {
    OSMesg mesg;
    OSThread *thread = NULL;

    osSetEventMesg(OS_EVENT_CPU_BREAK, &gCrashScreen.mesgQueue, (OSMesg) 1);
    osSetEventMesg(OS_EVENT_FAULT,     &gCrashScreen.mesgQueue, (OSMesg) 2);
    while (TRUE) {
        if (thread == NULL) {
            osRecvMesg(&gCrashScreen.mesgQueue, &mesg, 1);
            thread = get_crashed_thread();
            gCrashScreen.framebuffer = (RGBA16 *) gFramebuffers[sRenderedFramebuffer];
            if (thread) {
                gCrashScreen.thread.priority = 15;
                stop_sounds_in_continuous_banks();
                stop_background_music(sBackgroundMusicQueue[0].seqId);
                audio_signal_game_loop_tick();
                crash_screen_sleep(200);
                play_sound(SOUND_MARIO_WAAAOOOW, gGlobalSoundSource);
                audio_signal_game_loop_tick();
                crash_screen_sleep(200);
                if (stackTraceGenerated == FALSE) {
                    sCrashScreenStackTraceCount = generate_stack(thread);
                    stackTraceGenerated = TRUE;
                }
                // If an assert happened, go straight to that page
                if (thread->context.cause == EXC_SYSCALL) {
                    crashPage = CRASH_SCREEN_PAGE_ASSERTS;
                }
                continue;
            }
        } else {
            if (gControllerBits) {
#if ENABLE_RUMBLE
                block_until_rumble_pak_free();
#endif
                osContStartReadDataEx(&gSIEventMesgQueue);
            }
            read_controller_inputs(THREAD_2_CRASH_SCREEN);
            draw_crash_screen(thread);
        }
    }
}

void crash_screen_init(void) {
    gCrashScreen.framebuffer = (RGBA16 *) gFramebuffers[sRenderedFramebuffer];
    gCrashScreen.width = SCREEN_WIDTH;
    gCrashScreen.height = SCREEN_HEIGHT;
    osCreateMesgQueue(&gCrashScreen.mesgQueue, &gCrashScreen.mesg, 1);
    osCreateThread(&gCrashScreen.thread, THREAD_2_CRASH_SCREEN, thread2_crash_screen, NULL,
                   (u8 *) gCrashScreen.stack + sizeof(gCrashScreen.stack),
                   OS_PRIORITY_APPMAX
                  );
    osStartThread(&gCrashScreen.thread);
}

