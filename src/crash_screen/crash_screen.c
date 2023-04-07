#include <ultra64.h>
#include <PR/os_internal_error.h>
#include <stdarg.h>
#include <string.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen.h"
#include "crash_controls.h"
#include "crash_draw.h"
#include "crash_print.h"
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
#include "pages/context.h"
#include "pages/asserts.h"
#include "pages/log.h"
#include "pages/stack_trace.h"
#include "pages/ram_viewer.h"
#include "pages/disasm.h"


struct CrashScreenPage gCrashScreenPages[] = {
    [PAGE_CONTEXT    ] = { .drawFunc = draw_crash_context, .inputFunc = NULL,                           .pageControlsList = defaultPageControls,    .name = "CONTEXT",     .printName = FALSE },
    [PAGE_ASSERTS    ] = { .drawFunc = draw_assert,        .inputFunc = NULL,                           .pageControlsList = defaultPageControls,    .name = "ASSERTS",     .printName = TRUE  },
#ifdef PUPPYPRINT_DEBUG
    [PAGE_LOG        ] = { .drawFunc = draw_crash_log,     .inputFunc = NULL,                           .pageControlsList = defaultPageControls,    .name = "LOG",         .printName = TRUE  },
#endif  
    [PAGE_STACK_TRACE] = { .drawFunc = draw_stack_trace,   .inputFunc = crash_screen_input_stack_trace, .pageControlsList = stackTracePageControls, .name = "STACK TRACE", .printName = TRUE  },
    [PAGE_RAM_VIEWER ] = { .drawFunc = draw_ram_viewer,    .inputFunc = crash_screen_input_ram_viewer,  .pageControlsList = ramViewerPageControls,  .name = "RAM VIEW",    .printName = TRUE  },
    [PAGE_DISASM     ] = { .drawFunc = draw_disasm,        .inputFunc = crash_screen_input_disasm,      .pageControlsList = disasmPageControls,     .name = "DISASM",      .printName = TRUE  },
};

enum CrashScreenPages gCrashPage = FIRST_PAGE;

struct CrashScreen gCrashScreen;
#ifdef CRASH_SCREEN_CRASH_SCREEN
struct CrashScreen gCrashScreen2;
#endif

_Bool gCrashScreenSwitchedPage      = FALSE;
_Bool gDrawCrashScreen              = TRUE;
_Bool gDrawBackground               = TRUE;
_Bool gCrashScreenUpdateFramebuffer = TRUE; // Sets the framebuffer to be updated.

uintptr_t gCrashAddress        = 0x00000000;
uintptr_t gScrollAddress       = 0x00000000;
uintptr_t gSelectedAddress     = 0x00000000;


void crash_screen_draw_scroll_bar(u32 topY, u32 bottomY, u32 numVisibleEntries, u32 numTotalEntries, u32 currEntry, u32 minScrollBarHeight, RGBA32 color) {
    // Determine size of the scroll bar, starting on the pixel below the divider.
    u32 totalHeight = (bottomY - (topY + 1));

    u32 scrollBarHeight = (numVisibleEntries * ((f32) totalHeight / (f32) numTotalEntries));
    scrollBarHeight = CLAMP(scrollBarHeight, minScrollBarHeight, totalHeight);

    // Determine position of the scroll bar.
    f32 scrollableHeight = (totalHeight - scrollBarHeight);
    f32 numScrollableEntries = (numTotalEntries - numVisibleEntries);
    u32 scrollPos = (currEntry * (scrollableHeight / numScrollableEntries));

    // Draw the scroll bar rectangle.
    crash_screen_draw_rect((CRASH_SCREEN_X2 - 1), (topY + scrollPos), 1, scrollBarHeight, color);
}

void toggle_display_var(_Bool* var) {
    *var ^= TRUE;
    gCrashScreenUpdateFramebuffer = TRUE;
}

void clamp_view_to_selection(const u32 numRows, const u32 step) {
    const size_t size = (numRows * step);

    gScrollAddress = CLAMP(gScrollAddress, (gSelectedAddress - (size - 1)), (gSelectedAddress - (step - 1)));
    gScrollAddress = CLAMP(gScrollAddress, RAM_START, (RAM_END - size));
    gScrollAddress = ALIGN(gScrollAddress, step);
}

// Draw the header.
void print_crash_screen_heaader(void) {
    u32 line = 0;
    // "HackerSM64 vX.X.X"
    crash_screen_print(TEXT_X(0), TEXT_Y(line),
        STR_COLOR_PREFIX"%s v%s",
        COLOR_RGBA32_CRASH_HEADER,
        "HackerSM64",
        HACKERSM64_VERSION
    );
    // "START:controls"
    _Bool start = (gPlayer1Controller->buttonDown & START_BUTTON);
    crash_screen_print(TEXT_X(19), TEXT_Y(line),
        STR_COLOR_PREFIX"%s"STR_COLOR_PREFIX":%s",
        start ? COLOR_RGBA32_WHITE : COLOR_RGBA32_CRASH_HEADER, gCrashControlsDescriptions[CONT_DESC_SHOW_CONTROLS].control,
        COLOR_RGBA32_CRASH_HEADER, "controls"
    );

    _Bool pageLeft  = (gPlayer1Controller->buttonDown & L_TRIG);
    _Bool pageRight = (gPlayer1Controller->buttonDown & R_TRIG);
    if (start || pageLeft || pageRight) {
        gCrashScreenUpdateFramebuffer = TRUE;
    }
    // "<Page:X>"
    line += crash_screen_print(TEXT_X(35), TEXT_Y(line),
        STR_COLOR_PREFIX"%c"STR_COLOR_PREFIX"%s:%02d"STR_COLOR_PREFIX"%c",
        pageLeft ? COLOR_RGBA32_WHITE : COLOR_RGBA32_CRASH_HEADER, '<',
        COLOR_RGBA32_CRASH_HEADER,
        "Page", (gCrashPage + 1),
        pageRight ? COLOR_RGBA32_WHITE : COLOR_RGBA32_CRASH_HEADER, '>'
    );

    crash_screen_draw_divider(DIVIDER_Y(line));

    if (gCrashScreenPages[gCrashPage].printName) {
        line += crash_screen_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_PAGE_NAME, gCrashScreenPages[gCrashPage].name);
    
        crash_screen_draw_divider(DIVIDER_Y(line));
    }

    osWritebackDCacheAll();
}

void draw_crash_screen_main(OSThread* thread) {
    if (gCrashScreenUpdateFramebuffer) {
        gCrashScreenUpdateFramebuffer = FALSE;
        crash_screen_reset_framebuffer(gDrawBackground);

        if (gDrawCrashScreen) {
            if (gDrawBackground) {
                // Draw the transparent background.
                crash_screen_draw_dark_rect(CRASH_SCREEN_X1, CRASH_SCREEN_Y1, CRASH_SCREEN_W, CRASH_SCREEN_H, 2);
            }

            print_crash_screen_heaader();

            // Run the page-specific draw function.
            if (gCrashScreenPages[gCrashPage].drawFunc != NULL) {
                gCrashScreenPages[gCrashPage].drawFunc(thread);
            } else {
                //! TODO: "Null page" print
            }

            if (gAddressSelectMenuOpen) {
                draw_address_select();
            }

            if (gDrawControls) {
                draw_controls_box();
            }
        }

        crash_screen_update_framebuffer();
    }
}

OSThread* get_crashed_thread(void) {
    OSThread* thread = __osGetCurrFaultedThread();

    while (thread != NULL && thread->priority != -1) {
        if (thread->priority >  OS_PRIORITY_IDLE
         && thread->priority <= OS_PRIORITY_APPMAX
         && (thread->flags & (BIT(0) | BIT(1)))) {
            return thread;
        }

        thread = thread->tlnext;
    }

    return NULL;
}

#ifdef FUNNY_CRASH_SOUND
void crash_screen_sleep(u32 ms) {
    u64 cycles = (((ms * 1000LL) * osClockRate) / 1000000ULL);
    osSetTime(0);
    while (osGetTime() < cycles) {}
}

extern struct SequenceQueueItem sBackgroundMusicQueue[6];
extern void audio_signal_game_loop_tick(void);
extern void stop_sounds_in_continuous_banks(void);

void play_crash_sound(struct CrashScreen* crashScreen, s32 sound) {
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
void thread20_crash_screen_crash_screen(UNUSED void* arg) {
    OSMesg mesg;
    OSThread* thread = NULL;

    osSetEventMesg(OS_EVENT_CPU_BREAK, &gCrashScreen2.mesgQueue, (OSMesg)CRASH_SCREEN_MSG_CPU_BREAK);
    osSetEventMesg(OS_EVENT_FAULT,     &gCrashScreen2.mesgQueue, (OSMesg)CRASH_SCREEN_MSG_FAULT);

    while (TRUE) {
        if (thread == NULL) {
            osRecvMesg(&gCrashScreen2.mesgQueue, &mesg, OS_MESG_BLOCK);
            thread = get_crashed_thread();

            if (thread != NULL) {
 #ifdef FUNNY_CRASH_SOUND
                play_crash_sound(&gCrashScreen2, SOUND_MARIO_MAMA_MIA);
 #endif
                crash_screen_reset_framebuffer(FALSE);
                draw_crashed_image_i4();
                draw_crash_context(thread);

                osWritebackDCacheAll();
                osViBlack(FALSE);
                osViSwapBuffer((void*) PHYSICAL_TO_VIRTUAL(gFramebuffers[sRenderingFramebuffer]));
            }
        }
    }
}

void crash_screen_crash_screen_init(void) {
    osCreateMesgQueue(&gCrashScreen2.mesgQueue, &gCrashScreen2.mesg, 1);
    osCreateThread(&gCrashScreen2.thread, THREAD_20_CRASH_SCREEN_CRASH_SCREEN, thread20_crash_screen_crash_screen, NULL,
                ((u8*)gCrashScreen2.stack + sizeof(gCrashScreen2.stack)),
                OS_PRIORITY_APPMAX);
    osStartThread(&gCrashScreen2.thread);
}
#endif // CRASH_SCREEN_CRASH_SCREEN

void thread2_crash_screen(UNUSED void* arg) {
    OSMesg mesg;
    OSThread* thread = NULL;

    osSetEventMesg(OS_EVENT_CPU_BREAK, &gCrashScreen.mesgQueue, (OSMesg)CRASH_SCREEN_MSG_CPU_BREAK);
    osSetEventMesg(OS_EVENT_FAULT,     &gCrashScreen.mesgQueue, (OSMesg)CRASH_SCREEN_MSG_FAULT);

    while (TRUE) {
        if (thread == NULL) {
            osRecvMesg(&gCrashScreen.mesgQueue, &mesg, OS_MESG_BLOCK);

            osViSetEvent(&gCrashScreen.mesgQueue, (OSMesg)CRASH_SCREEN_MSG_VI_VBLANK, 1);

            // Save a screenshot of the game to the Z buffer's memory space.
            crash_screen_take_screenshot(gZBuffer);

            thread = get_crashed_thread();

            if (thread != NULL) {
#ifdef FUNNY_CRASH_SOUND
                play_crash_sound(&gCrashScreen, SOUND_MARIO_WAAAOOOW);
#endif
                __OSThreadContext* tc = &thread->context;
                // Default to the assert page if the crash was caused by an assert.
                if (tc->cause == EXC_SYSCALL) {
                    gCrashPage = PAGE_ASSERTS;
                }
                // If a position was specified, use that.
                if (gCrashAddress != 0x0) {
                    gCrashPage = PAGE_RAM_VIEWER;
                    tc->pc = gCrashAddress;
                }
                gSelectedAddress = tc->pc;
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
#ifdef ENABLE_RUMBLE
                block_until_rumble_pak_free();
#endif
                osContStartReadDataEx(&gSIEventMesgQueue);
            }
            read_controller_inputs(THREAD_2_CRASH_SCREEN);
            update_crash_screen_input();
            draw_crash_screen_main(thread);
            gCrashScreenSwitchedPage = FALSE;
        }
    }
}

void crash_screen_init(void) {
    osCreateMesgQueue(&gCrashScreen.mesgQueue, &gCrashScreen.mesg, 1);
    osCreateThread(&gCrashScreen.thread, THREAD_2_CRASH_SCREEN, thread2_crash_screen, NULL,
                   ((u8*)gCrashScreen.stack + sizeof(gCrashScreen.stack)),
                   OS_PRIORITY_APPMAX);
    osStartThread(&gCrashScreen.thread);
}
