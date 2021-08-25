#include <ultra64.h>
#include <PR/os_system.h>
#include <PR/os_vi.h>
#include <stdio.h>

#include "sm64.h"
#include "audio/external.h"
#include "game/game_init.h"
#include "game/memory.h"
#include "game/sound_init.h"
#include "game/profiler.h"
#include "buffers/buffers.h"
#include "segments.h"
#include "game/main.h"
#include "game/rumble_init.h"
#include "game/version.h"
#ifdef UNF
#include "usb/usb.h"
#include "usb/debug.h"
#endif
#include "game/puppyprint.h"

// Message IDs
#define MESG_SP_COMPLETE 100
#define MESG_DP_COMPLETE 101
#define MESG_VI_VBLANK 102
#define MESG_START_GFX_SPTASK 103
#define MESG_NMI_REQUEST 104

OSThread D_80339210; // unused?
OSThread gIdleThread;
OSThread gMainThread;
OSThread gGameLoopThread;
OSThread gSoundThread;

OSIoMesg gDmaIoMesg;
OSMesg gMainReceivedMesg;

OSMesgQueue gDmaMesgQueue;
OSMesgQueue gSIEventMesgQueue;
OSMesgQueue gPIMesgQueue;
OSMesgQueue gIntrMesgQueue;
OSMesgQueue gSPTaskMesgQueue;

OSMesg gDmaMesgBuf[1];
OSMesg gPIMesgBuf[32];
OSMesg gSIEventMesgBuf[1];
OSMesg gIntrMesgBuf[16];
OSMesg gUnknownMesgBuf[16];

OSViMode VI;

struct VblankHandler *gVblankHandler1 = NULL;
struct VblankHandler *gVblankHandler2 = NULL;
struct VblankHandler *gVblankHandler3 = NULL;
struct SPTask *gActiveSPTask = NULL;
struct SPTask *sCurrentAudioSPTask = NULL;
struct SPTask *sCurrentDisplaySPTask = NULL;
struct SPTask *sNextAudioSPTask = NULL;
struct SPTask *sNextDisplaySPTask = NULL;
s8 sAudioEnabled = TRUE;
u32 gNumVblanks = 0;
s8 gResetTimer = 0;
s8 gNmiResetBarsTimer = 0;
s8 gDebugLevelSelect = FALSE;

s8 gShowProfiler = FALSE;
s8 gShowDebugText = FALSE;

// unused
void handle_debug_key_sequences(void) {
    static u16 sProfilerKeySequence[] = {
        U_JPAD, U_JPAD, D_JPAD, D_JPAD, L_JPAD, R_JPAD, L_JPAD, R_JPAD
    };
    static u16 sDebugTextKeySequence[] = { D_JPAD, D_JPAD, U_JPAD, U_JPAD,
                                           L_JPAD, R_JPAD, L_JPAD, R_JPAD };
    static s16 sProfilerKey = 0;
    static s16 sDebugTextKey = 0;
    if (gPlayer3Controller->buttonPressed != 0) {
        if (sProfilerKeySequence[sProfilerKey++] == gPlayer3Controller->buttonPressed) {
            if (sProfilerKey == ARRAY_COUNT(sProfilerKeySequence)) {
                sProfilerKey = 0, gShowProfiler ^= 1;
            }
        } else {
            sProfilerKey = 0;
        }

        if (sDebugTextKeySequence[sDebugTextKey++] == gPlayer3Controller->buttonPressed) {
            if (sDebugTextKey == ARRAY_COUNT(sDebugTextKeySequence)) {
                sDebugTextKey = 0, gShowDebugText ^= 1;
            }
        } else {
            sDebugTextKey = 0;
        }
    }
}

void setup_mesg_queues(void) {
    osCreateMesgQueue(&gDmaMesgQueue, gDmaMesgBuf, ARRAY_COUNT(gDmaMesgBuf));
    osCreateMesgQueue(&gSIEventMesgQueue, gSIEventMesgBuf, ARRAY_COUNT(gSIEventMesgBuf));
    osSetEventMesg(OS_EVENT_SI, &gSIEventMesgQueue, NULL);

    osCreateMesgQueue(&gSPTaskMesgQueue, gUnknownMesgBuf, ARRAY_COUNT(gUnknownMesgBuf));
    osCreateMesgQueue(&gIntrMesgQueue, gIntrMesgBuf, ARRAY_COUNT(gIntrMesgBuf));
    osViSetEvent(&gIntrMesgQueue, (OSMesg) MESG_VI_VBLANK, 1);

    osSetEventMesg(OS_EVENT_SP, &gIntrMesgQueue, (OSMesg) MESG_SP_COMPLETE);
    osSetEventMesg(OS_EVENT_DP, &gIntrMesgQueue, (OSMesg) MESG_DP_COMPLETE);
    osSetEventMesg(OS_EVENT_PRENMI, &gIntrMesgQueue, (OSMesg) MESG_NMI_REQUEST);
}

void alloc_pool(void) {
    void *start = (void *) SEG_POOL_START;
    void *end = (void *) (SEG_POOL_START + POOL_SIZE);

    main_pool_init(start, end);
    gEffectsMemoryPool = mem_pool_init(0x4000, MEMORY_POOL_LEFT);
}

void create_thread(OSThread *thread, OSId id, void (*entry)(void *), void *arg, void *sp, OSPri pri) {
    thread->next = NULL;
    thread->queue = NULL;
    osCreateThread(thread, id, entry, arg, sp, pri);
}

#ifdef VERSION_SH
extern void func_sh_802f69cc(void);
#endif

void handle_nmi_request(void) {
    gResetTimer = 1;
    gNmiResetBarsTimer = 0;
    stop_sounds_in_continuous_banks();
    sound_banks_disable(SEQ_PLAYER_SFX, SOUND_BANKS_BACKGROUND);
    fadeout_music(90);
#ifdef VERSION_SH
    func_sh_802f69cc();
#endif
}

void receive_new_tasks(void) {
    struct SPTask *spTask;

    while (osRecvMesg(&gSPTaskMesgQueue, (OSMesg *) &spTask, OS_MESG_NOBLOCK) != -1) {
        spTask->state = SPTASK_STATE_NOT_STARTED;
        switch (spTask->task.t.type) {
            case 2:
                sNextAudioSPTask = spTask;
                break;
            case 1:
                sNextDisplaySPTask = spTask;
                break;
        }
    }

    if (sCurrentAudioSPTask == NULL && sNextAudioSPTask != NULL) {
        sCurrentAudioSPTask = sNextAudioSPTask;
        sNextAudioSPTask = NULL;
    }

    if (sCurrentDisplaySPTask == NULL && sNextDisplaySPTask != NULL) {
        sCurrentDisplaySPTask = sNextDisplaySPTask;
        sNextDisplaySPTask = NULL;
    }
}

void start_sptask(s32 taskType) {
    UNUSED s32 pad; // needed to pad the stack

    if (taskType == M_AUDTASK) {
        gActiveSPTask = sCurrentAudioSPTask;
    } else {
        gActiveSPTask = sCurrentDisplaySPTask;
    }

    osSpTaskLoad(&gActiveSPTask->task);
    osSpTaskStartGo(&gActiveSPTask->task);
    gActiveSPTask->state = SPTASK_STATE_RUNNING;
}

void interrupt_gfx_sptask(void) {
    if (gActiveSPTask->task.t.type == M_GFXTASK) {
        gActiveSPTask->state = SPTASK_STATE_INTERRUPTED;
        osSpTaskYield();
    }
}

void start_gfx_sptask(void) {
    if (gActiveSPTask == NULL && sCurrentDisplaySPTask != NULL
        && sCurrentDisplaySPTask->state == SPTASK_STATE_NOT_STARTED) {
        profiler_log_gfx_time(TASKS_QUEUED);
        #if PUPPYPRINT_DEBUG
        rspDelta = osGetTime();
        #endif
        start_sptask(M_GFXTASK);
    }
}

void pretend_audio_sptask_done(void) {
    gActiveSPTask = sCurrentAudioSPTask;
    gActiveSPTask->state = SPTASK_STATE_RUNNING;
    osSendMesg(&gIntrMesgQueue, (OSMesg) MESG_SP_COMPLETE, OS_MESG_NOBLOCK);
}

void handle_vblank(void) {

    gNumVblanks++;
#ifdef VERSION_SH
    if (gResetTimer > 0 && gResetTimer < 100) {
        gResetTimer++;
    }
#else
    if (gResetTimer > 0) {
        gResetTimer++;
    }
#endif

    receive_new_tasks();

    // First try to kick off an audio task. If the gfx task is currently
    // running, we need to asynchronously interrupt it -- handle_sp_complete
    // will pick up on what we're doing and start the audio task for us.
    // If there is already an audio task running, there is nothing to do.
    // If there is no audio task available, try a gfx task instead.
    if (sCurrentAudioSPTask != NULL) {
        if (gActiveSPTask != NULL) {
            interrupt_gfx_sptask();
        } else {
            profiler_log_vblank_time();
            if (sAudioEnabled) {
                start_sptask(M_AUDTASK);
            } else {
                pretend_audio_sptask_done();
            }
        }
    } else {
        if (gActiveSPTask == NULL && sCurrentDisplaySPTask != NULL
            && sCurrentDisplaySPTask->state != SPTASK_STATE_FINISHED) {
            profiler_log_gfx_time(TASKS_QUEUED);
            #if PUPPYPRINT_DEBUG
            rspDelta = osGetTime();
            #endif
            start_sptask(M_GFXTASK);
        }
    }
#if ENABLE_RUMBLE
    rumble_thread_update_vi();
#endif

    // Notify the game loop about the vblank.
    if (gVblankHandler1 != NULL) {
        osSendMesg(gVblankHandler1->queue, gVblankHandler1->msg, OS_MESG_NOBLOCK);
    }
    if (gVblankHandler2 != NULL) {
        osSendMesg(gVblankHandler2->queue, gVblankHandler2->msg, OS_MESG_NOBLOCK);
    }
    if (gVblankHandler3 != NULL) {
        osSendMesg(gVblankHandler3->queue, gVblankHandler3->msg, OS_MESG_NOBLOCK);
    }
}

void handle_sp_complete(void) {
    struct SPTask *curSPTask = gActiveSPTask;

    gActiveSPTask = NULL;

    if (curSPTask->state == SPTASK_STATE_INTERRUPTED) {
        // handle_vblank tried to start an audio task while there was already a
        // gfx task running, so it had to interrupt the gfx task. That interruption
        // just finished.
        if (osSpTaskYielded(&curSPTask->task) == 0) {
            // The gfx task completed before we had time to interrupt it.
            // Mark it finished, just like below.
            curSPTask->state = SPTASK_STATE_FINISHED;
            #if PUPPYPRINT_DEBUG
            profiler_update(rspGenTime, rspDelta);
            #endif
            profiler_log_gfx_time(RSP_COMPLETE);
        }

        // Start the audio task, as expected by handle_vblank.
        profiler_log_vblank_time();
        if (sAudioEnabled) {
            start_sptask(M_AUDTASK);
        } else {
            pretend_audio_sptask_done();
        }
    } else {
        curSPTask->state = SPTASK_STATE_FINISHED;
        if (curSPTask->task.t.type == M_AUDTASK) {
            // After audio tasks come gfx tasks.
            profiler_log_vblank_time();
            if (sCurrentDisplaySPTask != NULL
                && sCurrentDisplaySPTask->state != SPTASK_STATE_FINISHED) {
                if (sCurrentDisplaySPTask->state != SPTASK_STATE_INTERRUPTED) {
                    profiler_log_gfx_time(TASKS_QUEUED);
                }
                start_sptask(M_GFXTASK);
            }
            sCurrentAudioSPTask = NULL;
            if (curSPTask->msgqueue != NULL) {
                osSendMesg(curSPTask->msgqueue, curSPTask->msg, OS_MESG_NOBLOCK);
            }
        } else {
            // The SP process is done, but there is still a Display Processor notification
            // that needs to arrive before we can consider the task completely finished and
            // null out sCurrentDisplaySPTask. That happens in handle_dp_complete.
            #if PUPPYPRINT_DEBUG
            profiler_update(rspGenTime, rspDelta);
            #endif
            profiler_log_gfx_time(RSP_COMPLETE);
        }
    }
}

void handle_dp_complete(void) {
    // Gfx SP task is completely done.
    if (sCurrentDisplaySPTask->msgqueue != NULL) {
        osSendMesg(sCurrentDisplaySPTask->msgqueue, sCurrentDisplaySPTask->msg, OS_MESG_NOBLOCK);
    }
    profiler_log_gfx_time(RDP_COMPLETE);
    sCurrentDisplaySPTask->state = SPTASK_STATE_FINISHED_DP;
    sCurrentDisplaySPTask = NULL;
}
extern void crash_screen_init(void);

void thread3_main(UNUSED void *arg) {
    setup_mesg_queues();
    alloc_pool();
    load_engine_code_segment();
#ifndef UNF
    crash_screen_init();
#endif

#ifdef DEBUG
    osSyncPrintf("Super Mario 64\n");
    osSyncPrintf("Built by: %s\n", __username__);
    osSyncPrintf("Date    : %s\n", __datetime__);
    osSyncPrintf("Compiler: %s\n", __compiler__);
    osSyncPrintf("Linker  : %s\n", __linker__);
#endif

    create_thread(&gSoundThread, 4, thread4_sound, NULL, gThread4Stack + 0x2000, 20);
    osStartThread(&gSoundThread);

    create_thread(&gGameLoopThread, 5, thread5_game_loop, NULL, gThread5Stack + 0x2000, 10);
    osStartThread(&gGameLoopThread);

    while (TRUE) {
        OSMesg msg;

        osRecvMesg(&gIntrMesgQueue, &msg, OS_MESG_BLOCK);
        switch ((uintptr_t) msg) {
            case MESG_VI_VBLANK:
                handle_vblank();
                break;
            case MESG_SP_COMPLETE:
                handle_sp_complete();
                break;
            case MESG_DP_COMPLETE:
                handle_dp_complete();
                break;
            case MESG_START_GFX_SPTASK:
                start_gfx_sptask();
                break;
            case MESG_NMI_REQUEST:
                handle_nmi_request();
                break;
        }
    }
}

void set_vblank_handler(s32 index, struct VblankHandler *handler, OSMesgQueue *queue, OSMesg *msg) {
    handler->queue = queue;
    handler->msg = msg;

    switch (index) {
        case 1:
            gVblankHandler1 = handler;
            break;
        case 2:
            gVblankHandler2 = handler;
            break;
        case 3:
            gVblankHandler3 = handler;
            break;
    }
}

void send_sp_task_message(OSMesg *msg) {
    osWritebackDCacheAll();
    osSendMesg(&gSPTaskMesgQueue, msg, OS_MESG_NOBLOCK);
}

void dispatch_audio_sptask(struct SPTask *spTask) {
    if (sAudioEnabled && spTask != NULL) {
        osWritebackDCacheAll();
        osSendMesg(&gSPTaskMesgQueue, spTask, OS_MESG_NOBLOCK);
    }
}

void exec_display_list(struct SPTask *spTask) {
    if (spTask != NULL) {
        osWritebackDCacheAll();
        spTask->state = SPTASK_STATE_NOT_STARTED;
        if (sCurrentDisplaySPTask == NULL) {
            sCurrentDisplaySPTask = spTask;
            sNextDisplaySPTask = NULL;
            osSendMesg(&gIntrMesgQueue, (OSMesg) MESG_START_GFX_SPTASK, OS_MESG_NOBLOCK);
        } else {
            sNextDisplaySPTask = spTask;
        }
    }
}

void turn_on_audio(void) {
    sAudioEnabled = TRUE;
}

void turn_off_audio(void) {
    sAudioEnabled = FALSE;
    while (sCurrentAudioSPTask != NULL) {
        ;
    }
}

void change_vi(OSViMode *mode, int width, int height){

    mode->comRegs.width = width;
    mode->comRegs.xScale = (width*512)/320;
    if(height > 240)
    {
        mode->comRegs.ctrl |= 0x40;
        mode->fldRegs[0].origin = width*2;
        mode->fldRegs[1].origin = width*4;
        mode->fldRegs[0].yScale = 0x2000000|((height*1024)/240);
        mode->fldRegs[1].yScale = 0x2000000|((height*1024)/240);
        mode->fldRegs[0].vStart = mode->fldRegs[1].vStart-0x20002;
    }
    else
    {
        mode->fldRegs[0].origin = width*2;
        mode->fldRegs[1].origin = width*4;
        mode->fldRegs[0].yScale = ((height*1024)/240);
        mode->fldRegs[1].yScale = ((height*1024)/240);
    }
}

/**
 * Initialize hardware, start main thread, then idle.
 */
void thread1_idle(UNUSED void *arg) {

    osCreateViManager(OS_PRIORITY_VIMGR);
	switch ( osTvType ) {
	case OS_TV_NTSC:
		// NTSC
        //osViSetMode(&osViModeTable[OS_VI_NTSC_LAN1]);
        VI = osViModeTable[OS_VI_NTSC_LAN1];
		break;
	case OS_TV_MPAL:
		// MPAL
        //osViSetMode(&osViModeTable[OS_VI_MPAL_LAN1]);
        VI = osViModeTable[OS_VI_MPAL_LAN1];
		break;
	case OS_TV_PAL:
		// PAL
		//osViSetMode(&osViModeTable[OS_VI_PAL_LAN1]);
        VI = osViModeTable[OS_VI_PAL_LAN1];
		break;
	}
    change_vi(&VI, SCREEN_WIDTH, SCREEN_HEIGHT);
    osViSetMode(&VI);
    osViBlack(TRUE);
    osViSetSpecialFeatures(OS_VI_DITHER_FILTER_ON);
    osViSetSpecialFeatures(OS_VI_GAMMA_OFF);
    osCreatePiManager(OS_PRIORITY_PIMGR, &gPIMesgQueue, gPIMesgBuf, ARRAY_COUNT(gPIMesgBuf));
#ifdef UNF
    debug_initialize();
#endif
    create_thread(&gMainThread, 3, thread3_main, NULL, gThread3Stack + 0x2000, 100);
    osStartThread(&gMainThread);

    osSetThreadPri(NULL, 0);

    // halt
    while (TRUE) {
        ;
    }
}

#if CLEARRAM
void ClearRAM(void)
{
    bzero(_mainSegmentEnd, (size_t)osMemSize - (size_t)OS_K0_TO_PHYSICAL(_mainSegmentEnd));
}
#endif

#ifdef ISVPRINT
extern u32 gISVDbgPrnAdrs;
extern u32 gISVFlag;

void osInitialize_fakeisv() {
    /* global flag to skip `__checkHardware_isv` from being called. */
    gISVFlag = 0x49533634;  // 'IS64'

    /* printf writes go to this address, cen64(1) has this hardcoded. */
    gISVDbgPrnAdrs = 0x13FF0000;

    /* `__printfunc`, used by `osSyncPrintf` will be set. */
    __osInitialize_isv();
}
#endif

void main_func(void) {
#if CLEARRAM
    ClearRAM();
#endif
    __osInitialize_common();
#ifdef ISVPRINT
    osInitialize_fakeisv();
#endif

    create_thread(&gIdleThread, 1, thread1_idle, NULL, gIdleThreadStack + 0x800, 100);
    osStartThread(&gIdleThread);
}
