#include <ultra64.h>
#include <PR/os_internal_error.h>
#include <stdarg.h>
#include <string.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen.h"
#include "crash_controls.h"
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
    [PAGE_CONTEXT    ] = { .initFunc = NULL,             .drawFunc = draw_crash_context, .inputFunc = NULL,                           .pageControlsList = defaultPageControls,    .name = "CONTEXT",     .flags = { .initialized = FALSE, .skip = FALSE, .printName = FALSE, }, },
    [PAGE_ASSERTS    ] = { .initFunc = NULL,             .drawFunc = draw_assert,        .inputFunc = NULL,                           .pageControlsList = defaultPageControls,    .name = "ASSERTS",     .flags = { .initialized = FALSE, .skip = FALSE, .printName = TRUE,  }, },
#ifdef PUPPYPRINT_DEBUG
    [PAGE_LOG        ] = { .initFunc = NULL,             .drawFunc = draw_crash_log,     .inputFunc = NULL,                           .pageControlsList = defaultPageControls,    .name = "LOG",         .flags = { .initialized = FALSE, .skip = FALSE, .printName = TRUE,  }, },
#endif
    [PAGE_STACK_TRACE] = { .initFunc = stack_trace_init, .drawFunc = draw_stack_trace,   .inputFunc = crash_screen_input_stack_trace, .pageControlsList = stackTracePageControls, .name = "STACK TRACE", .flags = { .initialized = FALSE, .skip = FALSE, .printName = TRUE,  }, },
    [PAGE_RAM_VIEWER ] = { .initFunc = ram_viewer_init,  .drawFunc = draw_ram_viewer,    .inputFunc = crash_screen_input_ram_viewer,  .pageControlsList = ramViewerPageControls,  .name = "RAM VIEW",    .flags = { .initialized = FALSE, .skip = FALSE, .printName = TRUE,  }, },
    [PAGE_DISASM     ] = { .initFunc = disasm_init,      .drawFunc = draw_disasm,        .inputFunc = crash_screen_input_disasm,      .pageControlsList = disasmPageControls,     .name = "DISASM",      .flags = { .initialized = FALSE, .skip = FALSE, .printName = TRUE,  }, },
};

enum CrashScreenPages gCrashPage = FIRST_PAGE;

struct CrashScreen gCrashScreens[MAX_RECURSIVE_CRASH_SCREENS];

_Bool gGameCrashed                  = FALSE;
_Bool gCrashScreenSwitchedPage      = FALSE;
_Bool gDrawCrashScreen              = TRUE;
_Bool gDrawBackground               = TRUE;
_Bool gCrashScreenUpdateFramebuffer = TRUE; // Sets the framebuffer to be updated.

uintptr_t gCrashAddress    = 0x00000000;
uintptr_t gScrollAddress   = 0x00000000;
uintptr_t gSelectedAddress = 0x00000000;


void crash_screen_draw_scroll_bar(u32 topY, u32 bottomY, u32 numVisibleEntries, u32 numTotalEntries, u32 currEntry, u32 minScrollBarHeight, RGBA32 color) {
    // Determine size of the scroll bar, starting on the pixel below the divider.
    u32 totalHeight = (bottomY - (topY + 1));

    u32 scrollBarHeight = (numVisibleEntries * ((f32)totalHeight / (f32)numTotalEntries));
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
    gScrollAddress = CLAMP(gScrollAddress, VALID_RAM_START, (VALID_RAM_END - size));
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

    if (gCrashScreenPages[gCrashPage].flags.printName) {
        line += crash_screen_print(TEXT_X(0), TEXT_Y(line), STR_COLOR_PREFIX"%s", COLOR_RGBA32_CRASH_PAGE_NAME, gCrashScreenPages[gCrashPage].name);

        crash_screen_draw_divider(DIVIDER_Y(line));
    }

    osWritebackDCacheAll();
}

void draw_crash_screen_main(struct CrashScreen* crashScreen) {
    if (!gCrashScreenUpdateFramebuffer) {
        return;
    }

    gCrashScreenUpdateFramebuffer = FALSE;
    crash_screen_reset_framebuffer(gDrawBackground);

    if (!gDrawCrashScreen) {
        return;
    }

    if (gDrawBackground) {
        // Draw the transparent background.
        crash_screen_draw_dark_rect(CRASH_SCREEN_X1, CRASH_SCREEN_Y1, CRASH_SCREEN_W, CRASH_SCREEN_H, 2);
    }

    print_crash_screen_heaader();

    OSThread* thread = crashScreen->crashedThread;

    // Run the page-specific draw function.
    if (gCrashScreenPages[gCrashPage].drawFunc != NULL && !gCrashScreenPages[gCrashPage].flags.skip) {
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

    crash_screen_update_framebuffer(crashScreen);
}

/**
 * Iterates through the active thread queue for a user thread with either
 * the CPU break or Fault flag set.
 */
OSThread* get_crashed_thread(void) {
    OSThread* thread = __osGetCurrFaultedThread();

    // OS_PRIORITY_THREADTAIL indicates the end of the thread queue.
    while (thread != NULL && thread->priority != OS_PRIORITY_THREADTAIL) {
        if (
            thread->priority > OS_PRIORITY_IDLE   &&
            thread->priority < OS_PRIORITY_APPMAX &&
            (thread->flags & (OS_FLAG_CPU_BREAK | OS_FLAG_FAULT))
        ) {
            return thread;
        }

        thread = thread->tlnext;
    }

    return NULL;
}

#ifdef CRASH_SCREEN_CRASH_SCREEN
void thread20_crash_screen_crash_screen(UNUSED void* arg) {
    struct CrashScreen* crashScreen = &gCrashScreens[1];
    OSMesg mesg;
    crashScreen->crashedThread = NULL;

    osSetEventMesg(OS_EVENT_CPU_BREAK, &crashScreen->mesgQueue, (OSMesg)CRASH_SCREEN_MSG_CPU_BREAK);
    osSetEventMesg(OS_EVENT_FAULT,     &crashScreen->mesgQueue, (OSMesg)CRASH_SCREEN_MSG_FAULT);

    while (TRUE) {
        if (crashScreen->crashedThread == NULL) {
            // Wait for CPU break or fault.
            osRecvMesg(&crashScreen->mesgQueue, &mesg, OS_MESG_BLOCK);
            crashScreen->crashedThread = get_crashed_thread();

            if (crashScreen->crashedThread == NULL) {
                continue;
            }

#ifdef FUNNY_CRASH_SOUND
            play_crash_sound(crashScreen, SOUND_MARIO_MAMA_MIA);
#endif
            crash_screen_reset_framebuffer(FALSE);
            draw_crashed_image_i4();
            draw_crash_context(crashScreen->crashedThread);

            osWritebackDCacheAll();
            osViBlack(FALSE);
            osViSwapBuffer((void*)PHYSICAL_TO_VIRTUAL(gFramebuffers[sRenderingFramebuffer]));
        }
    }
}

void crash_screen_crash_screen_init(void) {
    struct CrashScreen* crashScreen = &gCrashScreens[1];

    osCreateMesgQueue(&crashScreen->mesgQueue, &crashScreen->mesg, 1);
    osCreateThread(
        &crashScreen->thread, THREAD_20_CRASH_SCREEN_CRASH_SCREEN,
        thread20_crash_screen_crash_screen, NULL,
        ((u8*)crashScreen->stack + sizeof(crashScreen->stack)),
        OS_PRIORITY_APPMAX
    );
    osStartThread(&crashScreen->thread);
}
#endif // CRASH_SCREEN_CRASH_SCREEN

#ifdef FUNNY_CRASH_SOUND
void crash_screen_sleep(u32 ms) {
    OSTime cycles = (((ms * 1000LL) * osClockRate) / 1000000ULL);
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

void crash_screen_loop(void) {
    struct CrashScreen* crashScreen = &gCrashScreens[0];
    OSMesg mesg;
    crashScreen->crashedThread = NULL;

    osSetEventMesg(OS_EVENT_CPU_BREAK, &crashScreen->mesgQueue, (OSMesg)CRASH_SCREEN_MSG_CPU_BREAK);
    osSetEventMesg(OS_EVENT_FAULT,     &crashScreen->mesgQueue, (OSMesg)CRASH_SCREEN_MSG_FAULT);

    while (TRUE) {
        if (crashScreen->crashedThread == NULL) {
            // Wait for CPU break or fault.
            osRecvMesg(&crashScreen->mesgQueue, &mesg, OS_MESG_BLOCK);
            crashScreen->crashedThread = get_crashed_thread();
            if (crashScreen->crashedThread == NULL) {
                continue;
            }
            gGameCrashed = TRUE;

            osViSetEvent(&crashScreen->mesgQueue, (OSMesg)CRASH_SCREEN_MSG_VI_VBLANK, 1);

#ifdef FUNNY_CRASH_SOUND
            play_crash_sound(crashScreen, SOUND_MARIO_WAAAOOOW);
#endif

            __OSThreadContext* tc = &crashScreen->crashedThread->context;

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
#endif
            // Save a screenshot of the game to the Z buffer's memory space.
            crash_screen_take_screenshot(gZBuffer);
#ifdef CRASH_SCREEN_CRASH_SCREEN
            crash_screen_crash_screen_init();
#endif

            draw_crash_screen_main(crashScreen);
        } else {
            if (gControllerBits) {
#ifdef ENABLE_RUMBLE
                block_until_rumble_pak_free();
#endif
                osContStartReadDataEx(&gSIEventMesgQueue);
            }
            extern void read_controller_inputs(s32 threadID);
            read_controller_inputs(THREAD_2_CRASH_SCREEN);
            update_crash_screen_input();
            draw_crash_screen_main(crashScreen);
            gCrashScreenSwitchedPage = FALSE;
        }
    }
}

void thread2_crash_screen(UNUSED void* arg) {
    crash_screen_loop();
}

void create_crash_screen_thread(void) {
    struct CrashScreen* crashScreen = &gCrashScreens[0];

    osCreateMesgQueue(&crashScreen->mesgQueue, &crashScreen->mesg, 1);
    osCreateThread(
        &crashScreen->thread, THREAD_2_CRASH_SCREEN,
        thread2_crash_screen, NULL,
        ((u8*)crashScreen->stack + sizeof(crashScreen->stack)),
        OS_PRIORITY_APPMAX
    );
    osStartThread(&crashScreen->thread);
}
