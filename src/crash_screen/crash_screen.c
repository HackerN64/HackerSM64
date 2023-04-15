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

struct CSThreadInfo  gCSThreadInfos[NUM_CRASH_SCREEN_BUFFERS];
struct CSThreadInfo* gActiveCSThreadInfo = NULL;

uintptr_t gCrashAddress    = 0x00000000;
uintptr_t gScrollAddress   = 0x00000000;
uintptr_t gSelectedAddress = 0x00000000;


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
    struct CSThreadInfo* threadInfo = &gCSThreadInfos[1];
    threadInfo->crashedThread = NULL;

    osSetEventMesg(OS_EVENT_CPU_BREAK, &threadInfo->mesgQueue, (OSMesg)CRASH_SCREEN_MSG_CPU_BREAK);
    osSetEventMesg(OS_EVENT_FAULT,     &threadInfo->mesgQueue, (OSMesg)CRASH_SCREEN_MSG_FAULT);

    while (TRUE) {
        // Wait for CPU break or fault.
        osRecvMesg(&threadInfo->mesgQueue, &threadInfo->mesg, OS_MESG_BLOCK);
        threadInfo->crashedThread = get_crashed_thread();
        if (threadInfo->crashedThread == NULL) {
            continue;
        }

#ifdef FUNNY_CRASH_SOUND
        play_crash_sound(threadInfo, SOUND_MARIO_MAMA_MIA);
#endif
        break;
    }

    while (TRUE) {
        crash_screen_update_input();
        crash_screen_draw_main();
        gCrashScreenSwitchedPage = FALSE;
    }
}

void crash_screen_crash_screen_init(void) {
    struct CSThreadInfo* threadInfo = &gCSThreadInfos[1];

    osCreateMesgQueue(&threadInfo->mesgQueue, &threadInfo->mesg, 1);
    osCreateThread(
        &threadInfo->thread, THREAD_20_CRASH_SCREEN_CRASH_SCREEN,
        thread20_crash_screen_crash_screen, NULL,
        ((u8*)threadInfo->stack + sizeof(threadInfo->stack)),
        OS_PRIORITY_APPMAX
    );
    osStartThread(&threadInfo->thread);
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

void play_crash_sound(struct CSThreadInfo* threadInfo, s32 sound) {
    threadInfo->thread.priority = 15;
    stop_sounds_in_continuous_banks();
    stop_background_music(sBackgroundMusicQueue[0].seqId);
    audio_signal_game_loop_tick();
    crash_screen_sleep(200);
    play_sound(sound, gGlobalSoundSource);
    audio_signal_game_loop_tick();
    crash_screen_sleep(200);
}
#endif

void crash_screen_thread_entry(UNUSED void* arg) {
    struct CSThreadInfo* threadInfo = &gCSThreadInfos[0];
    threadInfo->crashedThread = NULL;

    osSetEventMesg(OS_EVENT_CPU_BREAK, &threadInfo->mesgQueue, (OSMesg)CRASH_SCREEN_MSG_CPU_BREAK);
    osSetEventMesg(OS_EVENT_FAULT,     &threadInfo->mesgQueue, (OSMesg)CRASH_SCREEN_MSG_FAULT);

    while (TRUE) {
        // Wait for CPU break or fault.
        osRecvMesg(&threadInfo->mesgQueue, &threadInfo->mesg, OS_MESG_BLOCK);
        threadInfo->crashedThread = get_crashed_thread();
        if (threadInfo->crashedThread == NULL) {
            continue;
        }

        osViSetEvent(&threadInfo->mesgQueue, (OSMesg)CRASH_SCREEN_MSG_VI_VBLANK, 1);

#ifdef FUNNY_CRASH_SOUND
        play_crash_sound(threadInfo, SOUND_MARIO_WAAAOOOW);
#endif

        __OSThreadContext* tc = &threadInfo->crashedThread->context;

        // Default to certain pages depening on the crash type.
        switch (tc->cause) {
            case EXC_SYSCALL: gCrashPage = PAGE_ASSERTS; break;
            case EXC_II:      gCrashPage = PAGE_DISASM;  break;
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
        break;
    }

    while (TRUE) {
        crash_screen_update_input();
        crash_screen_draw_main();
        gCrashScreenSwitchedPage = FALSE;
    }
}

void create_crash_screen_thread(void) {
    struct CSThreadInfo* threadInfo = &gCSThreadInfos[0];

    osCreateMesgQueue(&threadInfo->mesgQueue, &threadInfo->mesg, 1);
    osCreateThread(
        &threadInfo->thread, THREAD_2_CRASH_SCREEN,
        crash_screen_thread_entry, NULL,
        ((u8*)threadInfo->stack + sizeof(threadInfo->stack)),
        OS_PRIORITY_APPMAX
    );
    osStartThread(&threadInfo->thread);
}
