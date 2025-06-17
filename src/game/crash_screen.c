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

#include "sm64.h"

#include "printf.h"

// Configurable Defines
#define X_KERNING 6
#define GLYPH_WIDTH 8
#define GLYPH_HEIGHT 12
#define FONT_ROWS 16
#define LEFT_MARGIN 10 // for crash screen prints

enum crashPages {
    PAGE_CONTEXT,
#ifdef PUPPYPRINT_DEBUG
    PAGE_LOG,
#endif
    PAGE_STACKTRACE,
    PAGE_DISASM,
    PAGE_ASSERTS,
    PAGE_COUNT
};

char *crashPageNames[] = {
    [PAGE_CONTEXT] = "(Context)",
#ifdef PUPPYPRINT_DEBUG
    [PAGE_LOG] = "(Log)",
#endif
    [PAGE_STACKTRACE] = "(Stack Trace)",
    [PAGE_DISASM] = "(Disassembly)",
    [PAGE_ASSERTS] = "(Assert)",
};

u8 gCrashScreenCharToGlyph[128] = {
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
    64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
};

u32 gCrashScreenFont[GLYPH_HEIGHT * FONT_ROWS * 2 + 1] = {
    #include "textures/crash_custom/crash_screen_font.ia1.inc.c"
};

u8 crashPage = 0;
u8 updateBuffer = TRUE;

static char crashScreenBuf[0x200];

char *gCauseDesc[18] = {
    "Interrupt",
    "TLB modification",
    "TLB exception on load",
    "TLB exception on store",
    "Address error on load",
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
    "Unimplemented operation", "Invalid operation", "Division by zero", "Overflow", "Underflow",
    "Inexact operation",
};



extern u64 osClockRate;
extern far char *parse_map(u32 pc);
extern far void map_data_init(void);
extern far char *find_function_in_stack(u32 *sp);

struct {
    OSThread thread;
    u64 stack[THREAD2_STACK / sizeof(u64)];
    OSMesgQueue mesgQueue;
    OSMesg mesg;
    u16 *framebuffer;
    u16 width;
    u16 height;
} gCrashScreen;

void crash_screen_draw_rect(s32 x, s32 y, s32 w, s32 h) {
    u16 *ptr;
    s32 i, j;

    ptr = gCrashScreen.framebuffer + gCrashScreen.width * y + x;
    for (i = 0; i < h; i++) {
        for (j = 0; j < w; j++) {
            *ptr = 0x0001;
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

    if (glyph > 0x7F) return;

    data = &gCrashScreenFont[((glyph&0xF)*GLYPH_HEIGHT * 2) + (glyph >= 64)];

    ptr = gCrashScreen.framebuffer + gCrashScreen.width * y + x;

    for (i = 0; i < GLYPH_HEIGHT; i++) {
        bit = 0x80000000U >> ((glyph >> 4) * GLYPH_WIDTH);
        rowMask = *data++;
        data ++;

        for (j = 0; j < (GLYPH_WIDTH); j++) {
            // *ptr++ = (bit & rowMask) ? 0xFFFF : 1;
            if (bit & rowMask) {
                *ptr = 0xFFFF;
            }
            ptr++;
            bit >>= 1;
        }
        ptr += gCrashScreen.width - (GLYPH_WIDTH);
    }
}

static char *write_to_buf(char *buffer, const char *data, size_t size) {
    return (char *) memcpy(buffer, data, size) + size;
}

void crash_screen_print_with_newlines(s32 x, s32 y, const s32 xNewline, const char *fmt, ...) {
    char *ptr;
    u32 glyph;
    s32 size;
    s32 xOffset = x;

    va_list args;
    va_start(args, fmt);

    size = _Printf(write_to_buf, crashScreenBuf, fmt, args);

    if (size > 0) {
        ptr = crashScreenBuf;

        while (*ptr && size-- > 0) {
            if (xOffset >= SCREEN_WIDTH - (xNewline + X_KERNING)) {
                y += 10;
                xOffset = xNewline;
            }

            glyph = gCrashScreenCharToGlyph[*ptr & 0x7f];

            if (*ptr == '\n') {
                y += 10;
                xOffset = x;
                ptr++;
                continue;
            } else if (glyph != 0xff) {
                crash_screen_draw_glyph(xOffset, y, glyph);
            }

            ptr++;
            xOffset += X_KERNING;
        }
    }

    va_end(args);
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
            glyph = gCrashScreenCharToGlyph[*ptr & 0x7f];

            if (glyph != 0xff) {
                crash_screen_draw_glyph(x, y, glyph);
            }

            ptr++;
            x += X_KERNING;
        }
    }

    va_end(args);
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

    crash_screen_print(100, 220, "FPCSR:%08XH", fpcsr);
    for (i = 0; i < 6; i++) {
        if (fpcsr & bit) {
            crash_screen_print(222, 220, "(%s)", gFpcsrDesc[i]);
            return;
        }
        bit >>= 1;
    }
}

void draw_crash_context(OSThread *thread, s32 cause) {
    __OSThreadContext *tc = &thread->context;
    crash_screen_draw_rect(0, 20, 320, 240);
    crash_screen_print(LEFT_MARGIN, 20, "THREAD:%d  (%s)", thread->id, gCauseDesc[cause]);
    crash_screen_print(LEFT_MARGIN, 30, "PC:%08XH   SR:%08XH   VA:%08XH", tc->pc, tc->sr, tc->badvaddr);
    osWritebackDCacheAll();
    if ((u32)parse_map != MAP_PARSER_ADDRESS) {
        char *fname = parse_map(tc->pc);
        crash_screen_print(LEFT_MARGIN, 40, "Crash at: %s", fname == NULL ? "Unknown" : fname);
    }
    crash_screen_print(LEFT_MARGIN,  52, "AT:%08XH   V0:%08XH   V1:%08XH", (u32) tc->at, (u32) tc->v0, (u32) tc->v1);
    crash_screen_print(LEFT_MARGIN,  62, "A0:%08XH   A1:%08XH   A2:%08XH", (u32) tc->a0, (u32) tc->a1, (u32) tc->a2);
    crash_screen_print(LEFT_MARGIN,  72, "A3:%08XH   T0:%08XH   T1:%08XH", (u32) tc->a3, (u32) tc->t0, (u32) tc->t1);
    crash_screen_print(LEFT_MARGIN,  82, "T2:%08XH   T3:%08XH   T4:%08XH", (u32) tc->t2, (u32) tc->t3, (u32) tc->t4);
    crash_screen_print(LEFT_MARGIN,  92, "T5:%08XH   T6:%08XH   T7:%08XH", (u32) tc->t5, (u32) tc->t6, (u32) tc->t7);
    crash_screen_print(LEFT_MARGIN, 102, "S0:%08XH   S1:%08XH   S2:%08XH", (u32) tc->s0, (u32) tc->s1, (u32) tc->s2);
    crash_screen_print(LEFT_MARGIN, 112, "S3:%08XH   S4:%08XH   S5:%08XH", (u32) tc->s3, (u32) tc->s4, (u32) tc->s5);
    crash_screen_print(LEFT_MARGIN, 122, "S6:%08XH   S7:%08XH   T8:%08XH", (u32) tc->s6, (u32) tc->s7, (u32) tc->t8);
    crash_screen_print(LEFT_MARGIN, 132, "T9:%08XH   GP:%08XH   SP:%08XH", (u32) tc->t9, (u32) tc->gp, (u32) tc->sp);
    crash_screen_print(LEFT_MARGIN, 142, "S8:%08XH   RA:%08XH",            (u32) tc->s8, (u32) tc->ra);
    if ((u32)parse_map != MAP_PARSER_ADDRESS) {
        char *fname = parse_map(tc->ra);
        crash_screen_print(LEFT_MARGIN, 152, "RA at: %s", fname);
    }

    crash_screen_print_fpcsr(tc->fpcsr);

    osWritebackDCacheAll();
    crash_screen_print_float_reg( 10, 170,  0, &tc->fp0.f.f_even);
    crash_screen_print_float_reg(100, 170,  2, &tc->fp2.f.f_even);
    crash_screen_print_float_reg(190, 170,  4, &tc->fp4.f.f_even);
    crash_screen_print_float_reg( 10, 180,  6, &tc->fp6.f.f_even);
    crash_screen_print_float_reg(100, 180,  8, &tc->fp8.f.f_even);
    crash_screen_print_float_reg(190, 180, 10, &tc->fp10.f.f_even);
    crash_screen_print_float_reg( 10, 190, 12, &tc->fp12.f.f_even);
    crash_screen_print_float_reg(100, 190, 14, &tc->fp14.f.f_even);
    crash_screen_print_float_reg(190, 190, 16, &tc->fp16.f.f_even);
    crash_screen_print_float_reg( 10, 200, 18, &tc->fp18.f.f_even);
    crash_screen_print_float_reg(100, 200, 20, &tc->fp20.f.f_even);
    crash_screen_print_float_reg(190, 200, 22, &tc->fp22.f.f_even);
    crash_screen_print_float_reg( 10, 210, 24, &tc->fp24.f.f_even);
    crash_screen_print_float_reg(100, 210, 26, &tc->fp26.f.f_even);
    crash_screen_print_float_reg(190, 210, 28, &tc->fp28.f.f_even);
    crash_screen_print_float_reg( 10, 220, 30, &tc->fp30.f.f_even);
}


#ifdef PUPPYPRINT_DEBUG
void draw_crash_log(void) {
    s32 i;
    crash_screen_draw_rect(0, 20, 320, 210);
    osWritebackDCacheAll();
#define LINE_HEIGHT (25 + ((LOG_BUFFER_SIZE - 1) * 10))
    for (i = 0; i < LOG_BUFFER_SIZE; i++) {
        crash_screen_print(LEFT_MARGIN, (LINE_HEIGHT - (i * 10)), consoleLogTable[i]);
    }
#undef LINE_HEIGHT
}
#endif


// prints any function pointers it finds in the stack format:
// SP address: function name
void draw_stacktrace(OSThread *thread, UNUSED s32 cause) {
    __OSThreadContext *tc = &thread->context;
    u32 temp_sp = (tc->sp + 0x14);

    crash_screen_draw_rect(0, 20, 320, 210);
    crash_screen_print(LEFT_MARGIN, 25, "STACK TRACE FROM %08X:", temp_sp);
    if ((u32) parse_map == MAP_PARSER_ADDRESS) {
        crash_screen_print(LEFT_MARGIN, 35, "CURRFUNC: NONE");
    } else {
        crash_screen_print(LEFT_MARGIN, 35, "CURRFUNC: %s", parse_map(tc->pc));
    }

    osWritebackDCacheAll();

    for (int i = 0; i < 18; i++) {
        if ((u32) find_function_in_stack == MAP_PARSER_ADDRESS) {
            crash_screen_print(LEFT_MARGIN, (45 + (i * 10)), "STACK TRACE DISABLED");
            break;
        } else {
            if ((u32) find_function_in_stack == MAP_PARSER_ADDRESS) {
                return;
            }

            char *fname = find_function_in_stack(&temp_sp);
            if ((fname == NULL) || ((*(u32*)temp_sp & 0x80000000) == 0)) {
                crash_screen_print(LEFT_MARGIN, (45 + (i * 10)), "%08X: Unknown", temp_sp);
            } else {
                crash_screen_print(LEFT_MARGIN, (45 + (i * 10)), "%08X: %s", temp_sp, fname);
            }
        }
    }
}

extern char *insn_disasm(u32 insn, u32 isPC);
static u32 sProgramPosition = 0;
void draw_disasm(OSThread *thread) {
    __OSThreadContext *tc = &thread->context;

    crash_screen_draw_rect(0, 20, 320, 240);
    if (sProgramPosition == 0) {
        sProgramPosition = (tc->pc - 36);
    }
    crash_screen_print(LEFT_MARGIN, 25, "Program Counter: %08X", sProgramPosition);
    osWritebackDCacheAll();


    for (int i = 0; i < 19; i++) {
        u32 addr = (sProgramPosition + (i * 4));

        char *disasm = insn_disasm(addr, (addr == tc->pc));
        if (disasm[0] == 0) {
            crash_screen_print(LEFT_MARGIN, (35 + (i * 10)), "%08X", addr);
        } else {
            crash_screen_print(LEFT_MARGIN, (35 + (i * 10)), "%s", disasm);
        }

    }

    osWritebackDCacheAll();
}

void draw_assert(UNUSED OSThread *thread) {
    crash_screen_draw_rect(0, 20, 320, 210);

    crash_screen_print(LEFT_MARGIN, 25, "ASSERT PAGE");

    if (__n64Assert_Filename != NULL) {
        crash_screen_print(LEFT_MARGIN, 35, "FILE: %s LINE %d", __n64Assert_Filename, __n64Assert_LineNum);
        crash_screen_print(LEFT_MARGIN, 55, "MESSAGE:");
        crash_screen_print(LEFT_MARGIN, 70, " %s", __n64Assert_Message);
    } else {
        crash_screen_print(LEFT_MARGIN, 35, "no failed assert to report.");
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
        updateBuffer = TRUE;
    }
    if (gPlayer1Controller->buttonPressed & (L_TRIG | Z_TRIG)) {
        crashPage--;
        updateBuffer = TRUE;
    }
    if (gPlayer1Controller->buttonDown & D_CBUTTONS) {
        sProgramPosition += 4;
        updateBuffer = TRUE;
    }
    if (gPlayer1Controller->buttonDown & U_CBUTTONS) {
        sProgramPosition -= 4;
        updateBuffer = TRUE;
    }

    if ((crashPage >= PAGE_COUNT) && (crashPage != 255)) {
        crashPage = 0;
    }
    if (crashPage == 255) {
        crashPage = (PAGE_COUNT - 1);
    }
    if (updateBuffer) {
        crash_screen_draw_rect(0, 0, 320, 20);
        crash_screen_print(LEFT_MARGIN, 5, "Page:%02d %-22s L/Z: Left   R: Right", crashPage, crashPageNames[crashPage]);
        switch (crashPage) {
            case PAGE_CONTEXT:    draw_crash_context(thread, cause); break;
#ifdef PUPPYPRINT_DEBUG
            case PAGE_LOG: 		  draw_crash_log(); break;
#endif
            case PAGE_STACKTRACE: draw_stacktrace(thread, cause); break;
            case PAGE_DISASM:     draw_disasm(thread); break;
            case PAGE_ASSERTS:    draw_assert(thread); break;
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

extern u16 sRenderedFramebuffer;
extern void audio_signal_game_loop_tick(void);
extern void stop_sounds_in_continuous_banks(void);
extern void read_controller_inputs(s32 threadID);
extern struct SequenceQueueItem sBackgroundMusicQueue[6];

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
                if ((u32) map_data_init != MAP_PARSER_ADDRESS) {
                    map_data_init();
                }
                gCrashScreen.thread.priority = 15;
                stop_sounds_in_continuous_banks();
                stop_background_music(sBackgroundMusicQueue[0].seqId);
                audio_signal_game_loop_tick();
                crash_screen_sleep(200);
                play_sound(SOUND_MARIO_WAAAOOOW, gGlobalSoundSource);
                audio_signal_game_loop_tick();
                crash_screen_sleep(200);
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

