#include <ultra64.h>

#include <PR/os_internal.h>

#include <stdarg.h>
#include <string.h>

#include "types.h"
#include "sm64.h"

#include "util/map_parser.h"
#include "cs_controls.h"
#include "cs_draw.h"
#include "cs_pages.h"
#include "cs_settings.h"

#include "cs_main.h"

#include "audio/external.h"
#include "buffers/framebuffers.h"
#include "buffers/zbuffer.h"
#include "game/main.h"
#ifdef UNF
#include "usb/usb.h"
#include "usb/debug.h"
#endif // UNF


ALIGNED16 static struct CSThreadInfo sCSThreadInfos[NUM_CRASH_SCREEN_BUFFERS]; // Crash screen threads.
static s32   sCSThreadIndex = 0;    // Crash screen thread index.
static _Bool sFirstCrash    = TRUE; // Used to make certain things only happen on the first crash.

CSThreadInfo* gActiveCSThreadInfo  = NULL; // Pointer to the current crash screen thread info.
CSThreadInfo* gWaitingCSThreadInfo = NULL; // Pointer to the next crash screen thread info.
OSThread*     gCrashedThread       = NULL; // Pointer to the most recently crashed thread.
OSThread*     gCrashedGameThread   = NULL; // Pointer to the most recently crashed non-crash-screen thread.
OSThread*     gInspectThread       = NULL; // Pointer to the thread the crash screen will be inspecting.

Address gSetCrashAddress       = 0x00000000; // Used by SET_CRASH_PTR to set the crashed thread PC. Externed in macros.h.
Address gSelectedAddress       = 0x00000000; // Selected address for ram viewer and disasm pages.
Address gLastCSSelectedAddress = 0x00000000; // Used for debugging crash screen crashes.


/**
 * @brief Reinitialize the crash screen's global variables, settings, buffers, etc.
 */
static void cs_reinitialize(void) {
    _Bool wasAssert = (gCrashedThread->context.cause == EXC_SYSCALL);

    // If the crash screen has crashed, disable the page that crashed, unless it was an assert.
    if (!sFirstCrash) {// && !wasAssert) {
        cs_get_current_page()->flags.crashed = TRUE;
    }

    if (sFirstCrash || wasAssert) {
        gCSPageID = CRASH_SCREEN_START_PAGE;
    }

    if (sFirstCrash) {
        cs_settings_apply_func_to_all(cs_setting_func_reset);
        cs_settings_set_all_headers(FALSE);
    }

    gCSSwitchedPage  = FALSE;
    gCSPopupID       = CS_POPUP_NONE;
    gCSSwitchedPopup = FALSE;

    gLastCSSelectedAddress = gSelectedAddress;
    gSelectedAddress = 0x00000000;

    gCSDirectionFlags.raw = 0b00000000;

    cs_reinitialize_pages();
}

#ifdef FUNNY_CRASH_SOUND
/**
 * @brief Pause the current thread for a specific amount of time.
 *
 * @param[in] ms Number of milliseconds to wait.
 */
void cs_sleep(u32 ms) {
    OSTime cycles = (((ms * 1000LL) * osClockRate) / 1000000ULL);
    osSetTime(0);
    while (osGetTime() < cycles) {}
}

extern struct SequenceQueueItem sBackgroundMusicQueue[6];
extern void audio_signal_game_loop_tick(void);
extern void stop_sounds_in_continuous_banks(void);

/**
 * @brief Play a sound.
 *
 * @param[out] threadInfo Pointer to the thread info.
 * @param[in ] sound      The sound ID to play.
 */
void cs_play_sound(struct CSThreadInfo* threadInfo, s32 sound) {
    threadInfo->thread.priority = 15;
    stop_sounds_in_continuous_banks();
    stop_background_music(sBackgroundMusicQueue[0].seqId);
    audio_signal_game_loop_tick();
    cs_sleep(200);
    play_sound(sound, gGlobalSoundSource);
    audio_signal_game_loop_tick();
    cs_sleep(200);
}
#endif // FUNNY_CRASH_SOUND

/**
 * @brief Runs once on every crash.
 *
 * @param[in,out] threadInfo Pointer to the thread info.
 */
static void on_crash(struct CSThreadInfo* threadInfo) {
    // Set the current inspected thread pointer.
    gInspectThread = gCrashedThread;

    // Create another crash screen thread in case the current one crashes.
    create_crash_screen_thread();

    // Set the active thread info pointer.
    gActiveCSThreadInfo = threadInfo;

    // Reinitialize global variables, settings, buffers, etc.
    cs_reinitialize();

    osViSetEvent(&threadInfo->mesgQueue, (OSMesg)CRASH_SCREEN_MSG_VI_VBLANK, 1);

#ifdef FUNNY_CRASH_SOUND
    cs_play_sound(threadInfo, SOUND_MARIO_WAAAOOOW);
#endif // FUNNY_CRASH_SOUND

    __OSThreadContext* tc = &gInspectThread->context;

    // Only on the first crash:
    if (sFirstCrash) {
        sFirstCrash = FALSE;

        // Set the crashed game thread pointer.
        gCrashedGameThread = gCrashedThread;

        // If a position was specified, use that.
        if (gSetCrashAddress != 0x0) {
            tc->pc = gSetCrashAddress;
            gSetCrashAddress = 0x00000000;
            cs_set_page(CS_PAGE_MEMORY);
        }

        // Use the Z buffer's memory space to save a screenshot of the game.
        cs_take_screenshot_of_game(gZBuffer, sizeof(gZBuffer));

#ifdef INCLUDE_DEBUG_MAP
        map_data_init();
#endif // INCLUDE_DEBUG_MAP

#ifdef UNF
        cs_os_print_page(cs_get_current_page());
#endif // UNF
    }

    gSelectedAddress = tc->pc;
}

/**
 * @brief Iterates through the active thread queue for a user thread with either
 *        the CPU break or Fault flag set.
 *
 * @return OSThread* A pointer to the thread that crashed.
 */
static OSThread* get_crashed_thread(void) {
    OSThread* thread = __osGetCurrFaultedThread();

    while (
        (thread != NULL) &&
        (thread->priority != OS_PRIORITY_THREADTAIL) // OS_PRIORITY_THREADTAIL indicates the end of the thread queue.
    ) {
        if (
            (thread->priority > OS_PRIORITY_IDLE  ) &&
            (thread->priority < OS_PRIORITY_APPMAX) && //! TODO: Should this include threads with priority OS_PRIORITY_APPMAX and higher? Official N64 games don't.
            (thread->flags & (OS_FLAG_CPU_BREAK | OS_FLAG_FAULT)) &&
            (thread != gCrashedThread)
        ) {
            return thread;
        }

        thread = thread->tlnext;
    }

    return NULL;
}

/**
 * @brief Pauses the current thread until another thread crashes.
 *
 * @param[in,out] mesgQueue The OSMesgQueue to use.
 * @param[in,out] mesg      The OSMesg to use.
 * @return OSThread* A pointer to the thread that crashed.
 */
OSThread* wait_until_thread_crash(OSMesgQueue* mesgQueue, OSMesg* mesg) {
    OSThread* crashedThread = NULL;

    // Check for CPU, SP, and MSG crashes.
    osSetEventMesg(OS_EVENT_CPU_BREAK, mesgQueue, (OSMesg)CRASH_SCREEN_MSG_CPU_BREAK);
    osSetEventMesg(OS_EVENT_SP_BREAK,  mesgQueue, (OSMesg)CRASH_SCREEN_MSG_SP_BREAK );
    osSetEventMesg(OS_EVENT_FAULT,     mesgQueue, (OSMesg)CRASH_SCREEN_MSG_FAULT    );

    // Wait for one of the above types of break or fault to occur.
    while (TRUE) {
        osRecvMesg(mesgQueue, mesg, OS_MESG_BLOCK);

        crashedThread = get_crashed_thread();
        if (crashedThread != NULL) {
            return crashedThread;
        }
    }
}

#define CS_GET_NEXT_THREAD_ID(_currentThreadId) (((_currentThreadId) + 1) % ARRAY_COUNT(sCSThreadInfos))

/**
 * @brief Crash screen tread function. Waits for a crash then loops the crash screen.
 *
 * @param[in] arg Unused arg.
 */
void crash_screen_thread_entry(UNUSED void* arg) {
    // Get the current thread info.
    struct CSThreadInfo* threadInfo = gWaitingCSThreadInfo;

    // Wait until a thread to crash.
    gCrashedThread = wait_until_thread_crash(&threadInfo->mesgQueue, &threadInfo->mesg);

    // -- A thread has crashed --

    on_crash(threadInfo);

    // Crash screen open.
    while (TRUE) {
        cs_update_input();
        cs_draw_main();
    }
}

/**
 * @brief Removes a thread from the active queue. Very similar to __osDequeueThread.
 * TODO: Does this need an iteration limit?
 *
 * @param thread The thread to remove from the active queue.
 */
void remove_thread_from_queue(OSThread* thread) {
    if (thread == NULL) {
        return;
    }

    OSThread* queue = __osGetActiveQueue();
    OSThread* prev = queue;

    while (
        (prev != NULL) &&
        (prev->priority != OS_PRIORITY_THREADTAIL)
    ) {
        if (prev->tlnext == thread) {
            prev->tlnext = thread->tlnext;
        }

        prev = prev->tlnext;
    }
}

/**
 * @brief Create a crash screen thread.
 */
void create_crash_screen_thread(void) {
    s32 threadIndex = sCSThreadIndex;
    sCSThreadIndex = CS_GET_NEXT_THREAD_ID(sCSThreadIndex);
    struct CSThreadInfo* threadInfo = &sCSThreadInfos[threadIndex];
    OSThread* thread = &threadInfo->thread;

    gWaitingCSThreadInfo = threadInfo;

    remove_thread_from_queue(thread);
    bzero(threadInfo, sizeof(struct CSThreadInfo));
    osCreateMesgQueue(&threadInfo->mesgQueue, &threadInfo->mesg, 1);
    osCreateThread(
        thread, (THREAD_1000_CRASH_SCREEN_0 + threadIndex),
        crash_screen_thread_entry, NULL,
        ((u8*)threadInfo->stack + sizeof(threadInfo->stack)), // Pointer to the end of the stack.
        (OS_PRIORITY_APPMAX - 1)
    );
    osStartThread(thread);
}
