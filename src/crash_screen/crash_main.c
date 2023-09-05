#include <ultra64.h>

#include <PR/os_internal_error.h>

#include <stdarg.h>
#include <string.h>

#include "types.h"
#include "sm64.h"

#include "address_select.h"
#include "crash_controls.h"
#include "crash_draw.h"
#include "crash_pages.h"
#include "crash_settings.h"
#include "map_parser.h"

#include "crash_main.h"

#include "audio/external.h"
#include "buffers/framebuffers.h"
#include "buffers/zbuffer.h"
#include "game/main.h"


ALIGNED16 static struct CSThreadInfo sCSThreadInfos[NUM_CRASH_SCREEN_BUFFERS];
static s32 sCSThreadIndex = 0;
static _Bool sFirstCrash = TRUE;

CSThreadInfo* gActiveCSThreadInfo = NULL;
OSThread* gCrashedThread = NULL;

Address gSetCrashAddress = 0x00000000; // Used by SET_CRASH_ADDR to set the crashed thread PC.
Address gSelectedAddress = 0x00000000; // Selected address for ram viewer and disasm pages.


static void crash_screen_reinitialize(void) {
    // If the crash screen has crashed, disable the page that crashed, unless it was an assert.
    if (!sFirstCrash && gCrashedThread->context.cause != EXC_SYSCALL) {
        gCSPages[gCSPageID]->flags.crashed = TRUE;
    }

    gCSPageID = CRASH_SCREEN_START_PAGE;

    gCSSwitchedPage        = FALSE;
    gCSDrawControls        = FALSE;
    gAddressSelectMenuOpen = FALSE;

    crash_screen_settings_apply_to_all(settings_func_reset);
    crash_screen_settings_set_all_headers(FALSE);

    gSetCrashAddress = 0x00000000;
    gSelectedAddress = 0x00000000;

    gCSDirectionFlags.raw = 0;

    for (int pageID = 0; pageID < ARRAY_COUNT(gCSPages); pageID++) {
        gCSPages[pageID]->flags.initialized = FALSE;
    }
}

/**
 * Iterates through the active thread queue for a user thread with either
 * the CPU break or Fault flag set.
 */
static OSThread* get_crashed_thread(void) {
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

static void on_crash(struct CSThreadInfo* threadInfo) {
    // Create another crash screen thread in case the current one crashes.
    create_crash_screen_thread();

    // Set the active thread info pointer.
    gActiveCSThreadInfo = threadInfo;

    crash_screen_reinitialize();

    osViSetEvent(&threadInfo->mesgQueue, (OSMesg)CRASH_SCREEN_MSG_VI_VBLANK, 1);

#ifdef FUNNY_CRASH_SOUND
    play_crash_sound(threadInfo, SOUND_MARIO_WAAAOOOW);
#endif

    __OSThreadContext* tc = &gCrashedThread->context;

    // Default to certain pages depening on the crash type.
    switch (tc->cause) {
        case EXC_SYSCALL: crash_screen_set_page(PAGE_LOG   ); break;
        case EXC_II:      crash_screen_set_page(PAGE_DISASM); break;
    }

    // Only on the first crash:
    if (sFirstCrash) {
        sFirstCrash = FALSE;

        // If a position was specified, use that.
        if (gSetCrashAddress != 0x0) {
            crash_screen_set_page(PAGE_RAM_VIEWER);
            tc->pc = gSetCrashAddress;
        }

        // Use the Z buffer's memory space to save a screenshot of the game.
        crash_screen_take_screenshot(gZBuffer);

#ifdef INCLUDE_DEBUG_MAP
        map_data_init();
#endif
    }

    gSelectedAddress = tc->pc;
}

void crash_screen_thread_entry(UNUSED void* arg) {
    struct CSThreadInfo* threadInfo = &sCSThreadInfos[sCSThreadIndex];

    sCSThreadIndex = ((sCSThreadIndex + 1) % ARRAY_COUNT(sCSThreadInfos));

    osSetEventMesg(OS_EVENT_CPU_BREAK, &threadInfo->mesgQueue, (OSMesg)CRASH_SCREEN_MSG_CPU_BREAK);
    osSetEventMesg(OS_EVENT_SP_BREAK,  &threadInfo->mesgQueue, (OSMesg)CRASH_SCREEN_MSG_SP_BREAK );
    osSetEventMesg(OS_EVENT_FAULT,     &threadInfo->mesgQueue, (OSMesg)CRASH_SCREEN_MSG_FAULT    );

    // Wait for CPU break or fault.
    while (TRUE) {
        osRecvMesg(&threadInfo->mesgQueue, &threadInfo->mesg, OS_MESG_BLOCK);
        gCrashedThread = get_crashed_thread();
        if (gCrashedThread != NULL) {
            break;
        }
    }

    // -- A thread has crashed --
    on_crash(threadInfo);

    // Crash screen open.
    while (TRUE) {
        crash_screen_update_input();
        crash_screen_draw_main();
    }
}

void create_crash_screen_thread(void) {
    struct CSThreadInfo* threadInfo = &sCSThreadInfos[sCSThreadIndex];
    bzero(threadInfo, sizeof(struct CSThreadInfo));

    osCreateMesgQueue(&threadInfo->mesgQueue, &threadInfo->mesg, 1);
    osCreateThread(
        &threadInfo->thread, (THREAD_1000_CRASH_SCREEN_0 + sCSThreadIndex),
        crash_screen_thread_entry, NULL,
        ((u8*)threadInfo->stack + sizeof(threadInfo->stack)), // Pointer to the end of the stack.
        (OS_PRIORITY_APPMAX - 1) //! TODO: Why shouldn't get_crashed_thread check for OS_PRIORITY_APPMAX threads?
    );
    osStartThread(&threadInfo->thread);
}
