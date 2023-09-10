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
#ifdef UNF
#include "usb/debug.h"
#endif


ALIGNED16 static struct CSThreadInfo sCSThreadInfos[NUM_CRASH_SCREEN_BUFFERS]; // Crash screen threads.
static s32   sCSThreadIndex = 0;    // Crash screen thread index.
static _Bool sFirstCrash    = TRUE; // Used to make certain things only happen on the first crash.

CSThreadInfo* gActiveCSThreadInfo = NULL; // Pointer to the current crash screen thread info.
OSThread*     gCrashedThread      = NULL; // Pointer to the most recently crashed thread.

Address gSetCrashAddress = 0x00000000; // Used by SET_CRASH_ADDR to set the crashed thread PC.
Address gSelectedAddress = 0x00000000; // Selected address for ram viewer and disasm pages.


/**
 * @brief Reinitialize the crash screen's global variables, settings, buffers, etc.
 */
static void cs_reinitialize(void) {
    // If the crash screen has crashed, disable the page that crashed, unless it was an assert.
    if (!sFirstCrash && (gCrashedThread->context.cause != EXC_SYSCALL)) {
        gCSPages[gCSPageID]->flags.crashed = TRUE;
    }

    gCSPageID = CRASH_SCREEN_START_PAGE;

    gCSSwitchedPage        = FALSE;
    gCSDrawControls        = FALSE;
    gAddressSelectMenuOpen = FALSE;

    cs_settings_apply_func_to_all(cs_setting_func_reset);
    cs_settings_set_all_headers(FALSE);

    gSetCrashAddress = 0x00000000;
    gSelectedAddress = 0x00000000;

    gCSDirectionFlags.raw = 0;

    for (int pageID = 0; pageID < ARRAY_COUNT(gCSPages); pageID++) {
        gCSPages[pageID]->flags.initialized = FALSE;
    }
}

/**
 * @brief Iterates through the active thread queue for a user thread with either
 *        the CPU break or Fault flag set.
 *
 * @return OSThread* The crashed thread.
 */
static OSThread* get_crashed_thread(void) {
    OSThread* thread = __osGetCurrFaultedThread();

    // OS_PRIORITY_THREADTAIL indicates the end of the thread queue.
    while ((thread != NULL) && (thread->priority != OS_PRIORITY_THREADTAIL)) {
        if (
            (thread->priority > OS_PRIORITY_IDLE  ) &&
            (thread->priority < OS_PRIORITY_APPMAX) && //! TODO: Why doesn't this include OS_PRIORITY_APPMAX threads?
            (thread->flags & (OS_FLAG_CPU_BREAK | OS_FLAG_FAULT))
        ) {
            return thread;
        }

        thread = thread->tlnext;
    }

    return NULL;
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

    __OSThreadContext* tc = &gCrashedThread->context;

    // Default to certain pages depening on the crash type.
    switch (tc->cause) {
        case EXC_SYSCALL: cs_set_page(PAGE_LOG   ); break;
        case EXC_II:      cs_set_page(PAGE_DISASM); break;
    }

    // Only on the first crash:
    if (sFirstCrash) {
        sFirstCrash = FALSE;

        // If a position was specified, use that.
        if (gSetCrashAddress != 0x0) {
            cs_set_page(PAGE_RAM_VIEWER);
            tc->pc = gSetCrashAddress;
        }

        // Use the Z buffer's memory space to save a screenshot of the game.
        cs_take_screenshot_of_game(gZBuffer);

#ifdef INCLUDE_DEBUG_MAP
        map_data_init();
#endif // INCLUDE_DEBUG_MAP
    }

    gSelectedAddress = tc->pc;

#ifdef UNF
 #ifdef INCLUDE_DEBUG_MAP
    const MapSymbol* symbol = get_map_symbol(tc->pc, SYMBOL_SEARCH_BACKWARD);
    if (symbol != NULL) {
        osSyncPrintf("func name\t%s\n", get_map_symbol_name(symbol)); //! TODO: only the name itself is printed.
    }
 #endif // INCLUDE_DEBUG_MAP
    debug_printcontext(gCrashedThread); //! TODO: fix line breaks and debug_printreg in usb/debug.c. Issue with UNFLoader itself?
#endif // UNF
}

/**
 * @brief Crash screen tread function. Waits for a crash then loops the crash screen.
 *
 * @param[in] arg Unused arg.
 */
void crash_screen_thread_entry(UNUSED void* arg) {
    struct CSThreadInfo* threadInfo = &sCSThreadInfos[sCSThreadIndex];

    // Increment the current thread index.
    sCSThreadIndex = ((sCSThreadIndex + 1) % ARRAY_COUNT(sCSThreadInfos));

    // Check for CPU, SP, and MSG crashes.
    osSetEventMesg(OS_EVENT_CPU_BREAK, &threadInfo->mesgQueue, (OSMesg)CRASH_SCREEN_MSG_CPU_BREAK);
    osSetEventMesg(OS_EVENT_SP_BREAK,  &threadInfo->mesgQueue, (OSMesg)CRASH_SCREEN_MSG_SP_BREAK );
    osSetEventMesg(OS_EVENT_FAULT,     &threadInfo->mesgQueue, (OSMesg)CRASH_SCREEN_MSG_FAULT    );

    // Wait for one of the above types of break or fault to occur.
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
        cs_update_input();
        cs_draw_main();
    }
}

/**
 * @brief Create a crash screen thread.
 */
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
