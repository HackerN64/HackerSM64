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

enum crashPages {
    PAGE_CONTEXT,
#if PUPPYPRINT_DEBUG
    PAGE_LOG,
#endif
    PAGE_STACKTRACE,
    PAGE_DISASM,
    PAGE_ASSERTS,
    PAGE_CONTROLS,
    PAGE_COUNT
};

enum AnalogFlags {
    ANALOG_FLAGS_NONE = 0x0,
    ANALOG_FLAG_LEFT  = BIT(0),
    ANALOG_FLAG_RIGHT = BIT(1),
    ANALOG_FLAG_UP    = BIT(2),
    ANALOG_FLAG_DOWN  = BIT(3),
};

// A height of seven pixels for each Character * nine rows of characters + one row unused.
u32 gCrashScreenFont[7 * 26 + 1] = {
    #include "textures/crash_custom/crash_screen_font.ia1.inc.c"
};

static s8 sAnalogFlags = ANALOG_FLAGS_NONE;
static u8 sDrawCrashScreen = TRUE;
static u8 sDrawFrameBuffer = TRUE;
static u8 sSkipUnknownsInStackTrace = FALSE;
static u32 sProgramPosition = 0;

u8 crashPage = 0;
u8 updateBuffer = TRUE;


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
    u64 stack[0x800 / sizeof(u64)];
    OSMesgQueue mesgQueue;
    OSMesg mesg;
    RGBA16 *framebuffer;
    u16 width;
    u16 height;
} gCrashScreen;

void crash_screen_draw_rect(s32 x, s32 y, s32 w, s32 h, RGBA16 color, s32 isTransparent) {
    s32 i, j;

    RGBA16 *ptr = gCrashScreen.framebuffer + (gCrashScreen.width * y) + x;
    for (i = 0; i < h; i++) {
        for (j = 0; j < w; j++) {
            if (isTransparent) {
                *ptr = ((*ptr & color) >> 2) | 1;
            } else {
                *ptr = color;
            }
            ptr++;
        }
        ptr += gCrashScreen.width - w;
    }
}

void crash_screen_draw_glyph(s32 x, s32 y, s32 glyph, RGBA16 color) {
    u32 bit;
    u32 rowMask;
    s32 i, j;

    const u32 *data = &gCrashScreenFont[glyph / 5 * 7];

    RGBA16 *ptr = gCrashScreen.framebuffer + (gCrashScreen.width * y) + x;

    for (i = 0; i < 7; i++) {
        bit = 0x80000000U >> ((glyph % 5) * 6);
        rowMask = *data++;

        for (j = 0; j < 6; j++) {
            if (bit & rowMask) {
                *ptr = color;
            }
            ptr++;
            bit >>= 1;
        }
        ptr += gCrashScreen.width - 6;
    }
}

static char *write_to_buf(char *buffer, const char *data, size_t size) {
    return (char *) memcpy(buffer, data, size) + size;
}

void crash_screen_print_color(s32 x, s32 y, RGBA16 color, const char *fmt, ...) {
    u32 glyph;
    char buf[0x108];
    bzero(&buf, sizeof(buf));

    va_list args;
    va_start(args, fmt);

    s32 size = _Printf(write_to_buf, buf, fmt, args);

    if (size > 0) {
        char *ptr = buf;

        while (*ptr) {
            glyph = (*ptr & 0x7f);

            if (glyph != 0xff) {
                crash_screen_draw_glyph(x, y, glyph, color);
            }

            ptr++;
            x += 6;
        }
    }

    va_end(args);
}

#define crash_screen_print(x, y, ...) crash_screen_print_color(x, y, COLOR_RGBA16_CRASH_WHITE, __VA_ARGS__)

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

    crash_screen_print(30, 155, "FPCSR:%08X", fpcsr);
    for (i = 0; i < 6; i++) {
        if (fpcsr & bit) {
            crash_screen_print(132, 155, "(%s)", gFpcsrDesc[i]);
            return;
        }
        bit >>= 1;
    }
}

void draw_crash_context(OSThread *thread, s32 cause) {
    __OSThreadContext *tc = &thread->context;
    crash_screen_print_color(30, 20, COLOR_RGBA16_CRASH_THREAD, "THREAD:%d", thread->id);
    crash_screen_print_color(90, 20, COLOR_RGBA16_CRASH_CAUSE, "(%s)", gCauseDesc[cause]);
    crash_screen_print(30, 30, "PC:%08X    SR:%08X    VA:%08X ", tc->pc, tc->sr, tc->badvaddr);
    osWritebackDCacheAll();
    if ((u32)parse_map != MAP_PARSER_ADDRESS) {
        char *fname = parse_map(tc->pc);
        crash_screen_print_color(30, 40, COLOR_RGBA16_CRASH_CURRFUNC, "CRASH AT:");
        if (fname == NULL) {
            crash_screen_print_color(90, 40, COLOR_RGBA16_GRAY, "UNKNOWN");
        } else {
            crash_screen_print_color(90, 40, COLOR_RGBA16_CRASH_FUNCTION, "%s", fname);
        }
    }
    crash_screen_print(30,  50, "AT:%08X    V0:%08X    V1:%08X", (u32) tc->at, (u32) tc->v0, (u32) tc->v1);
    crash_screen_print(30,  60, "A0:%08X    A1:%08X    A2:%08X", (u32) tc->a0, (u32) tc->a1, (u32) tc->a2);
    crash_screen_print(30,  70, "A3:%08X    T0:%08X    T1:%08X", (u32) tc->a3, (u32) tc->t0, (u32) tc->t1);
    crash_screen_print(30,  80, "T2:%08X    T3:%08X    T4:%08X", (u32) tc->t2, (u32) tc->t3, (u32) tc->t4);
    crash_screen_print(30,  90, "T5:%08X    T6:%08X    T7:%08X", (u32) tc->t5, (u32) tc->t6, (u32) tc->t7);
    crash_screen_print(30, 100, "S0:%08X    S1:%08X    S2:%08X", (u32) tc->s0, (u32) tc->s1, (u32) tc->s2);
    crash_screen_print(30, 110, "S3:%08X    S4:%08X    S5:%08X", (u32) tc->s3, (u32) tc->s4, (u32) tc->s5);
    crash_screen_print(30, 120, "S6:%08X    S7:%08X    T8:%08X", (u32) tc->s6, (u32) tc->s7, (u32) tc->t8);
    crash_screen_print(30, 130, "T9:%08X    GP:%08X    SP:%08X", (u32) tc->t9, (u32) tc->gp, (u32) tc->sp);
    crash_screen_print(30, 140, "S8:%08X    RA:%08X",            (u32) tc->s8, (u32) tc->ra);
    crash_screen_print_fpcsr(tc->fpcsr);

    osWritebackDCacheAll();
    crash_screen_print_float_reg( 30, 170,  0, &tc->fp0.f.f_even);
    crash_screen_print_float_reg(120, 170,  2, &tc->fp2.f.f_even);
    crash_screen_print_float_reg(210, 170,  4, &tc->fp4.f.f_even);
    crash_screen_print_float_reg( 30, 180,  6, &tc->fp6.f.f_even);
    crash_screen_print_float_reg(120, 180,  8, &tc->fp8.f.f_even);
    crash_screen_print_float_reg(210, 180, 10, &tc->fp10.f.f_even);
    crash_screen_print_float_reg( 30, 190, 12, &tc->fp12.f.f_even);
    crash_screen_print_float_reg(120, 190, 14, &tc->fp14.f.f_even);
    crash_screen_print_float_reg(210, 190, 16, &tc->fp16.f.f_even);
    crash_screen_print_float_reg( 30, 200, 18, &tc->fp18.f.f_even);
    crash_screen_print_float_reg(120, 200, 20, &tc->fp20.f.f_even);
    crash_screen_print_float_reg(210, 200, 22, &tc->fp22.f.f_even);
    crash_screen_print_float_reg( 30, 210, 24, &tc->fp24.f.f_even);
    crash_screen_print_float_reg(120, 210, 26, &tc->fp26.f.f_even);
    crash_screen_print_float_reg(210, 210, 28, &tc->fp28.f.f_even);
    crash_screen_print_float_reg( 30, 220, 30, &tc->fp30.f.f_even);
}


#if PUPPYPRINT_DEBUG
void draw_crash_log(void) {
    s32 i;
    osWritebackDCacheAll();
#define LINE_HEIGHT (25 + ((LOG_BUFFER_SIZE - 1) * 10))
    for (i = 0; i < LOG_BUFFER_SIZE; i++) {
        crash_screen_print(30, (LINE_HEIGHT - (i * 10)), consoleLogTable[i]);
    }
#undef LINE_HEIGHT
}
#endif

// prints any function pointers it finds in the stack format:
// SP address: function name
void draw_stacktrace(OSThread *thread, UNUSED s32 cause) {
    __OSThreadContext *tc = &thread->context;
    u32 temp_sp = (tc->sp + 0x14);

    crash_screen_print(30, 20, "STACK TRACE FROM %08X:", temp_sp);
    crash_screen_print_color(30, 30, COLOR_RGBA16_CRASH_CURRFUNC, "CURRFUNC:");
    if ((u32) parse_map == MAP_PARSER_ADDRESS) {
        crash_screen_print(90, 30, "NONE");
    } else {
        crash_screen_print_color(90, 30, COLOR_RGBA16_CRASH_FUNCTION, "%s", parse_map(tc->pc));
    }

    osWritebackDCacheAll();

    if (sProgramPosition < temp_sp) {
        sProgramPosition = temp_sp;
    }

    u32 addr = sProgramPosition;

    for (int i = 0; i < 18; i++) {
        if ((u32) find_function_in_stack == MAP_PARSER_ADDRESS) {
            crash_screen_print(30, (40 + (i * 10)), "STACK TRACE DISABLED");
            break;
        } else {
            if ((u32) find_function_in_stack == MAP_PARSER_ADDRESS) {
                return;
            }

            char *fname = find_function_in_stack(&addr);
            if ((fname == NULL) || ((*(u32*)addr & 0x80000000) == 0)) {
                if (sSkipUnknownsInStackTrace) {
                    i--;
                } else {
                    crash_screen_print(30, (40 + (i * 10)), "%08X:", addr);
                    crash_screen_print_color(90, (40 + (i * 10)), COLOR_RGBA16_LIGHT_GRAY, "UNKNOWN");
                }
            } else {
                crash_screen_print(30, (40 + (i * 10)), "%08X:", addr);
                crash_screen_print_color(90, (40 + (i * 10)), COLOR_RGBA16_CRASH_FUNCTION_2, "%s", fname);
            }
        }
    }

    crash_screen_draw_rect(25, 218, 270, 1, COLOR_RGBA16_LIGHT_GRAY, FALSE);
    crash_screen_print_color( 30, 220, COLOR_RGBA16_LIGHT_GRAY, "up/down: scroll");
    crash_screen_print_color(180, 220, COLOR_RGBA16_LIGHT_GRAY, "a: toggle unknowns");

    osWritebackDCacheAll();
}

extern char *insn_disasm(u32 insn, u32 isPC);
void draw_disasm(OSThread *thread) {
    __OSThreadContext *tc = &thread->context;
    // u32 insn = *(u32*)tc->pc;

    if (sProgramPosition == 0) {
        sProgramPosition = (tc->pc - 36);
    }

    crash_screen_print(30, 20, "DISASM %08X", sProgramPosition);

    osWritebackDCacheAll();

    for (int i = 0; i < 19; i++) {
        u32 addr = (sProgramPosition + (i * 4));
        u32 toDisasm = *(u32*)(addr);

        crash_screen_print(30, (30 + (i * 10)), "%s", insn_disasm(toDisasm, (addr == tc->pc)));
    }

    crash_screen_draw_rect(25, 218, 270, 1, COLOR_RGBA16_LIGHT_GRAY, FALSE);
    crash_screen_print_color(30, 220, COLOR_RGBA16_LIGHT_GRAY, "up/down: scroll");

    osWritebackDCacheAll();
}

void draw_assert(UNUSED OSThread *thread) {
    crash_screen_print(30, 20, "ASSERT PAGE");

    if (__n64Assert_Filename != NULL) {
        crash_screen_print(30, 30, "FILE: %s LINE %d", __n64Assert_Filename, __n64Assert_LineNum);
        crash_screen_print(30, 50, "MESSAGE:");
        crash_screen_print(30, 65, " %s", __n64Assert_Message);
    } else {
        crash_screen_print(30, 30, "no failed assert to report.");
    }

    osWritebackDCacheAll();
}

extern void warp_special(s32 arg);

void draw_controls(UNUSED OSThread *thread) {
    s32 y = 15;
    crash_screen_print(      30, (y += 10), "CRASH SCREEN CONTROLS");
    crash_screen_print(      40, (y += 20), "START:");
    crash_screen_print_color(40, (y += 10), COLOR_RGBA16_LIGHT_GRAY, "toggle framebuffer background");
    crash_screen_print(      40, (y += 20), "Z:");
    crash_screen_print_color(40, (y += 10), COLOR_RGBA16_LIGHT_GRAY, "toggle framebuffer only view");
    crash_screen_print(      40, (y += 20), "ANALOG STICK, D-PAD, OR C BUTTONS:");
    crash_screen_print(      50, (y += 20), "LEFT/RIGHT:");
    crash_screen_print_color(50, (y += 10), COLOR_RGBA16_LIGHT_GRAY, "switch page");
    crash_screen_print(      50, (y += 20), "UP/DOWN:");
    crash_screen_print_color(50, (y += 10), COLOR_RGBA16_LIGHT_GRAY, "scroll page");

    osWritebackDCacheAll();
}

extern u16 sRenderedFramebuffer;

void reload_crash_screen_framebuffer(void) {
    if (sDrawFrameBuffer) {
        memcpy(gFramebuffers[2], gFramebuffers[sRenderedFramebuffer], ((SCREEN_WIDTH * SCREEN_HEIGHT) * sizeof(RGBA16)));
        gCrashScreen.framebuffer = (RGBA16 *) gFramebuffers[2];
    } else {
        crash_screen_draw_rect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, COLOR_RGBA16_BLACK, FALSE);
    }
}

void draw_crash_screen(OSThread *thread) {
    __OSThreadContext *tc = &thread->context;
    u8 prevPage = crashPage;

    s32 cause = ((tc->cause >> 2) & 0x1F);
    if (cause == 23) { // EXC_WATCH
        cause = 16;
    }
    if (cause == 31) { // EXC_VCED
        cause = 17;
    }

    if (gPlayer1Controller->buttonPressed & Z_TRIG) {
        sDrawCrashScreen ^= TRUE;
        updateBuffer = TRUE;
    }

    if (gPlayer1Controller->buttonPressed & START_BUTTON) {
        sDrawFrameBuffer ^= TRUE;
        updateBuffer = TRUE;
    }

    if (!sDrawCrashScreen && !sDrawFrameBuffer) {
        sDrawCrashScreen = TRUE;
    }

    if (sDrawCrashScreen) {
        if (gPlayer1Controller->buttonPressed & (R_CBUTTONS | R_JPAD)) {
            crashPage++;
            updateBuffer = TRUE;
        }
        if (gPlayer1Controller->buttonPressed & (L_CBUTTONS | L_JPAD)) {
            crashPage--;
            updateBuffer = TRUE;
        }

        if (gPlayer1Controller->rawStickX > 60 && !(sAnalogFlags & ANALOG_FLAG_RIGHT)) {
            crashPage++;
            updateBuffer = TRUE;
            sAnalogFlags |= ANALOG_FLAG_RIGHT;
        } else if (gPlayer1Controller->rawStickX < 10) {
            sAnalogFlags &= ~ANALOG_FLAG_RIGHT;
        }

        if (gPlayer1Controller->rawStickX < -60 && !(sAnalogFlags & ANALOG_FLAG_LEFT)) {
            crashPage--;
            updateBuffer = TRUE;
            sAnalogFlags |= ANALOG_FLAG_LEFT;
        } else if (gPlayer1Controller->rawStickX > -10) {
            sAnalogFlags &= ~ANALOG_FLAG_LEFT;
        }

        if (crashPage == PAGE_STACKTRACE || crashPage == PAGE_DISASM) {
            if ((gPlayer1Controller->buttonDown & (D_CBUTTONS | D_JPAD))
            || (gPlayer1Controller->rawStickY < -60)) {
                sProgramPosition += 4;
                updateBuffer = TRUE;
            }
            if ((gPlayer1Controller->buttonDown & (U_CBUTTONS | U_JPAD))
            || (gPlayer1Controller->rawStickY > 60)) {
                sProgramPosition -= 4;
                updateBuffer = TRUE;
            }
        }

        if (crashPage == PAGE_STACKTRACE && (gPlayer1Controller->buttonPressed & A_BUTTON)) {
            sSkipUnknownsInStackTrace ^= TRUE;
            sProgramPosition = 0;
            updateBuffer = TRUE;
        }

        if ((crashPage >= PAGE_COUNT) && (crashPage != 255)) {
            crashPage = 0;
        }
        if (crashPage == 255) {
            crashPage = (PAGE_COUNT - 1);
        }
    }

    if (updateBuffer) {
        if (crashPage != prevPage) {
            sProgramPosition = 0;
        }

        reload_crash_screen_framebuffer();

        if (sDrawCrashScreen) {
            if (sDrawFrameBuffer) {
                crash_screen_draw_rect(25, 8, 270, 222, COLOR_RGBA16_CRASH_BACKGROUND, TRUE);
            }
            crash_screen_print_color( 30, 10, COLOR_RGBA16_LIGHT_GRAY, "HackerSM64 v%s", HACKERSM64_VERSION);
            crash_screen_print_color(234, 10, COLOR_RGBA16_LIGHT_GRAY, "<Page:%02d>", crashPage);
            crash_screen_draw_rect(25, 18, 270, 1, COLOR_RGBA16_LIGHT_GRAY, FALSE);
            switch (crashPage) {
                case PAGE_CONTEXT:    draw_crash_context(thread, cause); break;
#if PUPPYPRINT_DEBUG
                case PAGE_LOG: 		  draw_crash_log(); break;
#endif
                case PAGE_STACKTRACE: draw_stacktrace(thread, cause); break;
                case PAGE_DISASM:     draw_disasm(thread); break;
                case PAGE_ASSERTS:    draw_assert(thread); break;
                case PAGE_CONTROLS:   draw_controls(thread); break;
            }
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
        if (thread->priority > OS_PRIORITY_IDLE
         && thread->priority < OS_PRIORITY_APPMAX
         && ((thread->flags & (BIT(0) | BIT(1))) != 0)) {
            return thread;
        }
        thread = thread->tlnext;
    }
    return NULL;
}

#ifdef FUNNY_CRASH_SOUND
extern void audio_signal_game_loop_tick(void);
extern void stop_sounds_in_continuous_banks(void);
extern struct SequenceQueueItem sBackgroundMusicQueue[6];
#endif
extern void read_controller_inputs(s32 threadID);

void thread2_crash_screen(UNUSED void *arg) {
    OSMesg mesg;
    OSThread *thread = NULL;

    osSetEventMesg(OS_EVENT_CPU_BREAK, &gCrashScreen.mesgQueue, (OSMesg) 1);
    osSetEventMesg(OS_EVENT_FAULT,     &gCrashScreen.mesgQueue, (OSMesg) 2);

    while (TRUE) {
#if PUPPYPRINT_DEBUG
        OSTime first = osGetTime();
#endif
        if (thread == NULL) {
            osRecvMesg(&gCrashScreen.mesgQueue, &mesg, 1);
            thread = get_crashed_thread();
            reload_crash_screen_framebuffer();
            if (thread) {
                if ((u32) map_data_init != MAP_PARSER_ADDRESS) {
                    map_data_init();
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
                continue;
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
#if PUPPYPRINT_DEBUG
        profiler_update(faultTime, first);
#endif
    }
}

void crash_screen_init(void) {
    reload_crash_screen_framebuffer();
    gCrashScreen.width = SCREEN_WIDTH;
    gCrashScreen.height = SCREEN_HEIGHT;
    osCreateMesgQueue(&gCrashScreen.mesgQueue, &gCrashScreen.mesg, 1);
    osCreateThread(&gCrashScreen.thread, THREAD_2_CRASH_SCREEN, thread2_crash_screen, NULL,
                   (u8 *) gCrashScreen.stack + sizeof(gCrashScreen.stack),
                   OS_PRIORITY_APPMAX);
    osStartThread(&gCrashScreen.thread);
}

