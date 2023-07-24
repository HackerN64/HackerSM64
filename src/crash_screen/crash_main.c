#include <ultra64.h>
#include <PR/os_internal_error.h>
#include <stdarg.h>
#include <string.h>
#include "types.h"
#include "sm64.h"
#include "crash_main.h"
#include "audio/external.h"
#include "buffers/framebuffers.h"
#include "buffers/zbuffer.h"
#include "engine/colors.h"
#include "game/debug.h"
#include "game/game_init.h"
#include "game/input.h"
#include "game/main.h"
#include "game/printf.h"
#include "game/puppyprint.h"
#include "game/rumble_init.h"
#include "game/vc_check.h"
#include "pages/page_context.h"
#include "pages/page_assert.h"
#include "pages/page_log.h"
#include "pages/page_map.h"
#include "pages/page_stack.h"
#include "pages/page_memory.h"
#include "pages/page_disasm.h"
#include "pages/page_settings.h"


const char* sValNames_bool[] = {
    [FALSE] = "FALSE",
    [TRUE ] = "TRUE",
};

const char* sValNames_print_num_fmt[] = {
    [PRINT_NUM_FMT_HEX] = "HEX",
    [PRINT_NUM_FMT_DEC] = "DECIMAL",
    [PRINT_NUM_FMT_SCI] = "SCIENTIFIC",
};

const char* sValNames_branch_arrow[] = {
    [DISASM_ARROW_MODE_OFF      ] = "OFF",
    [DISASM_ARROW_MODE_SELECTION] = "SELECTION",
    [DISASM_ARROW_MODE_FUNCTION ] = "FUNCTION",
    [DISASM_ARROW_MODE_OVERSCAN ] = "OVERSCAN",
};

struct CSSettingsEntry gCSSettings[NUM_CS_OPTS] = {
    [CS_OPT_DRAW_CRASH_SCREEN ] = { .name = "Draw crash screen",           .valNames = sValNames_bool,          .val =                       TRUE, .defaultVal =                       TRUE, .lowerBound =                 FALSE, .upperBound =                       TRUE, },
    [CS_OPT_DRAW_SCREENSHOT   ] = { .name = "Draw screenshot",             .valNames = sValNames_bool,          .val =                       TRUE, .defaultVal =                       TRUE, .lowerBound =                 FALSE, .upperBound =                       TRUE, },
    [CS_OPT_FUNCTION_NAMES    ] = { .name = "Show function names",         .valNames = sValNames_bool,          .val =    SHOW_FUNC_NAMES_DEFAULT, .defaultVal =    SHOW_FUNC_NAMES_DEFAULT, .lowerBound =                 FALSE, .upperBound =                       TRUE, },
    [CS_OPT_MEMORY_AS_ASCII   ] = { .name = "Memory as ascii",             .valNames = sValNames_bool,          .val =                       TRUE, .defaultVal =                       TRUE, .lowerBound =                 FALSE, .upperBound =                       TRUE, },
    [CS_OPT_DISASM_BINARY     ] = { .name = "Unknown disasm as binary",    .valNames = sValNames_bool,          .val =                      FALSE, .defaultVal =                      FALSE, .lowerBound =                 FALSE, .upperBound =                       TRUE, },
    [CS_OPT_PRINT_SCROLL_SPEED] = { .name = "Print overscan scroll speed", .valNames = NULL,                    .val =                          2, .defaultVal =                          2, .lowerBound =                     0, .upperBound =                          5, },
    [CS_OPT_FLOATS_FMT        ] = { .name = "Floats print format",         .valNames = sValNames_print_num_fmt, .val =          PRINT_NUM_FMT_DEC, .defaultVal =          PRINT_NUM_FMT_DEC, .lowerBound =     PRINT_NUM_FMT_HEX, .upperBound =          PRINT_NUM_FMT_SCI, },
    [CS_OPT_DISASM_PSEUDOINSNS] = { .name = "DISASM: pseudoinstructions",  .valNames = sValNames_bool,          .val =                       TRUE, .defaultVal =                       TRUE, .lowerBound =                 FALSE, .upperBound =                       TRUE, },
    [CS_OPT_DISASM_IMM_FMT    ] = { .name = "DISASM: immediate format",    .valNames = sValNames_print_num_fmt, .val =          PRINT_NUM_FMT_HEX, .defaultVal =          PRINT_NUM_FMT_HEX, .lowerBound =     PRINT_NUM_FMT_HEX, .upperBound =          PRINT_NUM_FMT_DEC, },
    [CS_OPT_BRANCH_ARROW_MODE ] = { .name = "DISASM: branch arrow mode",   .valNames = sValNames_branch_arrow,  .val = DISASM_ARROW_MODE_FUNCTION, .defaultVal = DISASM_ARROW_MODE_FUNCTION, .lowerBound = DISASM_ARROW_MODE_OFF, .upperBound = DISASM_ARROW_MODE_OVERSCAN, }, //! TODO: Implement this
};

struct CSPage gCSPages[NUM_PAGES] = {
    [PAGE_CONTEXT    ] = { .initFunc = context_init,     .drawFunc = context_draw,     .inputFunc = context_input,     .contList = contextContList,    .name = "CONTEXT",     .flags = { .initialized = FALSE, .crashed = FALSE, .printName = FALSE, }, },
    [PAGE_ASSERTS    ] = { .initFunc = assert_init,      .drawFunc = assert_draw,      .inputFunc = assert_input,      .contList = assertsContList,    .name = "ASSERTS",     .flags = { .initialized = FALSE, .crashed = FALSE, .printName = TRUE,  }, },
#ifdef PUPPYPRINT_DEBUG
    [PAGE_LOG        ] = { .initFunc = log_init,         .drawFunc = log_draw,         .inputFunc = log_input,         .contList = defaultContList,    .name = "LOG",         .flags = { .initialized = FALSE, .crashed = FALSE, .printName = TRUE,  }, },
#endif
    [PAGE_STACK_TRACE] = { .initFunc = stack_trace_init, .drawFunc = stack_trace_draw, .inputFunc = stack_trace_input, .contList = stackTraceContList, .name = "STACK TRACE", .flags = { .initialized = FALSE, .crashed = FALSE, .printName = TRUE,  }, },
#ifdef INCLUDE_DEBUG_MAP
    [PAGE_MAP_VIEWER ] = { .initFunc = map_view_init,    .drawFunc = map_view_draw,    .inputFunc = map_view_input,    .contList = mapViewerContList,  .name = "MAP VIEW",    .flags = { .initialized = FALSE, .crashed = FALSE, .printName = TRUE,  }, },
#endif
    [PAGE_RAM_VIEWER ] = { .initFunc = ram_view_init,    .drawFunc = ram_view_draw,    .inputFunc = ram_view_input,    .contList = ramViewerContList,  .name = "RAM VIEW",    .flags = { .initialized = FALSE, .crashed = FALSE, .printName = TRUE,  }, },
    [PAGE_DISASM     ] = { .initFunc = disasm_init,      .drawFunc = disasm_draw,      .inputFunc = disasm_input,      .contList = disasmContList,     .name = "DISASM",      .flags = { .initialized = FALSE, .crashed = FALSE, .printName = TRUE,  }, },
    [PAGE_SETTINGS   ] = { .initFunc = settings_init,    .drawFunc = settings_draw,    .inputFunc = settings_input,    .contList = settingsContList,   .name = "SETTINGS",    .flags = { .initialized = FALSE, .crashed = FALSE, .printName = TRUE,  }, },
};

enum CrashScreenPages gCSPageID = FIRST_PAGE;

ALIGNED16 static struct CSThreadInfo sCSThreadInfos[NUM_CRASH_SCREEN_BUFFERS];
static s32 sCSThreadIndex = 0;
static _Bool sFirstCrash = TRUE;

struct CSThreadInfo* gActiveCSThreadInfo = NULL;
OSThread* gCrashedThread = NULL;

Address gSetCrashAddress = 0x00000000; // Used by SET_CRASH_ADDR to set the crashed thread PC.
Address gSelectedAddress = 0x00000000; // Selected address for ram viewer and disasm pages.


void settings_reset_to_defaults(void) {
    for (int i = 0; i < ARRAY_COUNT(gCSSettings); i++) {
        struct CSSettingsEntry* setting = &gCSSettings[i];
        setting->val = setting->defaultVal;
    }
}

static void crash_screen_reinitialize(void) {
    // If the crash screen has crashed, disable the page that crashed, unless it was an assert.
    if (!sFirstCrash && gCrashedThread->context.cause != EXC_SYSCALL) {
        gCSPages[gCSPageID].flags.crashed = TRUE;
    }

    gCSPageID = FIRST_PAGE;

    gCSSwitchedPage        = FALSE;
    gCSDrawControls        = FALSE;
    gAddressSelectMenuOpen = FALSE;

    settings_reset_to_defaults();

    gSetCrashAddress = 0x00000000;
    gSelectedAddress = 0x00000000;

    gCSDirectionFlags.raw = 0;

    for (int i = 0; i < ARRAY_COUNT(gCSPages); i++) {
        gCSPages[i].flags.initialized = FALSE;
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
        case EXC_SYSCALL: crash_screen_set_page(PAGE_ASSERTS); break;
        case EXC_II:      crash_screen_set_page(PAGE_DISASM ); break;
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
    osSetEventMesg(OS_EVENT_SP_BREAK,  &threadInfo->mesgQueue, (OSMesg)CRASH_SCREEN_MSG_SP_BREAK);
    osSetEventMesg(OS_EVENT_FAULT,     &threadInfo->mesgQueue, (OSMesg)CRASH_SCREEN_MSG_FAULT);

    while (TRUE) {
        // Wait for CPU break or fault.
        osRecvMesg(&threadInfo->mesgQueue, &threadInfo->mesg, OS_MESG_BLOCK);
        gCrashedThread = get_crashed_thread();
        if (gCrashedThread == NULL) {
            continue;
        }

        // -- A thread has crashed --
        on_crash(threadInfo);
        create_crash_screen_thread();
        break;
    }

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
