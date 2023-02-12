#include <ultra64.h>
#include "macros.h"
#include "config.h"

#include "buffers/buffers.h"
#include "main.h"
#include "rumble_init.h"
#include "level_update.h"

#ifdef ENABLE_RUMBLE

OSThread gRumblePakThread;

OSPfs gRumblePakPfs;

OSMesg gRumblePakSchedulerMesgBuf[1];
OSMesgQueue gRumblePakSchedulerMesgQueue;
OSMesg gRumbleThreadVIMesgBuf[1];
OSMesgQueue gRumbleThreadVIMesgQueue;

struct RumbleData gRumbleDataQueue[RUMBLE_DATA_QUEUE_SIZE];
struct RumbleSettings gCurrRumbleSettings;

s32 sRumblePakThreadActive = FALSE;
s32 sRumblePakActive = FALSE;
s32 sRumblePakErrorCount = 0;
s32 gRumblePakTimer = 0;

enum RumbleEvents {
    RUMBLE_EVENT_NOMESG,
    RUMBLE_EVENT_CONSTON,
    RUMBLE_EVENT_LEVELON,
};

void init_rumble_pak_scheduler_queue(void) {
    osCreateMesgQueue(&gRumblePakSchedulerMesgQueue, gRumblePakSchedulerMesgBuf, 1);
    osSendMesg(&gRumblePakSchedulerMesgQueue, (OSMesg) 0, OS_MESG_NOBLOCK);
}

/**
 * Locks controller input data while reading new inputs or another thread is using the current inputs.
 * This prevents new inputs overwriting the current inputs while they are in use.
 */
void block_until_rumble_pak_free(void) {
    OSMesg msg;
    osRecvMesg(&gRumblePakSchedulerMesgQueue, &msg, OS_MESG_BLOCK);
}

/**
 * Unlocks controller input data, allowing to read new inputs or another thread to access the most recently
 * polled inputs.
 */
void release_rumble_pak_control(void) {
    osSendMesg(&gRumblePakSchedulerMesgQueue, (OSMesg) 0, OS_MESG_NOBLOCK);
}

static void start_rumble(void) {
    if (!sRumblePakActive) {
        return;
    }

    block_until_rumble_pak_free();

    if (!osMotorStart(&gRumblePakPfs)) {
        sRumblePakErrorCount = 0;
    } else {
        sRumblePakErrorCount++;
    }

    release_rumble_pak_control();
}

static void stop_rumble(void) {
    if (!sRumblePakActive) {
        return;
    }

    block_until_rumble_pak_free();

    if (!osMotorStop(&gRumblePakPfs)) {
        sRumblePakErrorCount = 0;
    } else {
        sRumblePakErrorCount++;
    }

    release_rumble_pak_control();
}

static void update_rumble_pak(void) {
    if (gResetTimer > 0) {
        stop_rumble();
        return;
    }

    if (gCurrRumbleSettings.start > 0) {
        gCurrRumbleSettings.start--;
        start_rumble();
    } else if (gCurrRumbleSettings.timer > 0) {
        gCurrRumbleSettings.timer--;

        gCurrRumbleSettings.level -= gCurrRumbleSettings.decay;
        if (gCurrRumbleSettings.level < 0) {
            gCurrRumbleSettings.level = 0;
        }

        if (gCurrRumbleSettings.event == RUMBLE_EVENT_CONSTON) {
            start_rumble();
        } else if (gCurrRumbleSettings.count >= 0x100) {
            gCurrRumbleSettings.count -= 0x100;
            start_rumble();
        } else {
            gCurrRumbleSettings.count +=
                ((gCurrRumbleSettings.level * gCurrRumbleSettings.level * gCurrRumbleSettings.level) / (1 << 9)) + 4;

            stop_rumble();
        }
    } else {
        gCurrRumbleSettings.timer = 0;

        if (gCurrRumbleSettings.slip >= 5) {
            start_rumble();
        } else if ((gCurrRumbleSettings.slip >= 2) && ((gNumVblanks % gCurrRumbleSettings.vibrate) == 0)) {
            start_rumble();
        } else {
            stop_rumble();
        }
    }

    if (gCurrRumbleSettings.slip > 0) {
        gCurrRumbleSettings.slip--;
    }
}

static void update_rumble_data_queue(void) {
    if (gRumbleDataQueue[0].comm) {
        gCurrRumbleSettings.count = 0;
        gCurrRumbleSettings.start = 4;
        gCurrRumbleSettings.event = gRumbleDataQueue[0].comm;
        gCurrRumbleSettings.timer = gRumbleDataQueue[0].time;
        gCurrRumbleSettings.level = gRumbleDataQueue[0].level;
        gCurrRumbleSettings.decay = gRumbleDataQueue[0].decay;
    }

    for (int i = 0; i < ARRAY_COUNT(gRumbleDataQueue) - 1; i++) {
        gRumbleDataQueue[i] = gRumbleDataQueue[i + 1];
    }

    gRumbleDataQueue[RUMBLE_DATA_QUEUE_SIZE - 1].comm = RUMBLE_EVENT_NOMESG;
}

void queue_rumble_data(s16 time, s16 level) {
    if (gCurrDemoInput != NULL) {
        return;
    }

    gRumbleDataQueue[RUMBLE_DATA_QUEUE_SIZE - 1].comm = (level > 70) ? RUMBLE_EVENT_CONSTON : RUMBLE_EVENT_LEVELON;
    gRumbleDataQueue[RUMBLE_DATA_QUEUE_SIZE - 1].level = level;
    gRumbleDataQueue[RUMBLE_DATA_QUEUE_SIZE - 1].time = time;
    gRumbleDataQueue[RUMBLE_DATA_QUEUE_SIZE - 1].decay = 0;
}

void queue_rumble_decay(s16 decay) {
    gRumbleDataQueue[RUMBLE_DATA_QUEUE_SIZE - 1].decay = decay;
}

u32 is_rumble_finished_and_queue_empty(void) {
    if (gCurrRumbleSettings.start + gCurrRumbleSettings.timer >= 4) {
        return FALSE;
    }

    for (int i = 0; i < ARRAY_COUNT(gRumbleDataQueue); i++) {
        if (gRumbleDataQueue[i].comm != RUMBLE_EVENT_NOMESG) {
            return FALSE;
        }
    }

    return TRUE;
}

void reset_rumble_timers_slip(void) {
    if (gCurrDemoInput != NULL) {
        return;
    }

    if (gCurrRumbleSettings.slip == 0) {
        gCurrRumbleSettings.slip = 7;
    }

    if (gCurrRumbleSettings.slip < 4) {
        gCurrRumbleSettings.slip = 4;
    }

    gCurrRumbleSettings.vibrate = 7;
}

void reset_rumble_timers_vibrate(s32 level) {
    if (gCurrDemoInput != NULL) {
        return;
    }

    if (gCurrRumbleSettings.slip == 0) {
        gCurrRumbleSettings.slip = 7;
    }

    if (gCurrRumbleSettings.slip < 4) {
        gCurrRumbleSettings.slip = 4;
    }

    if (level < 5) {
        gCurrRumbleSettings.vibrate = (5 - level);
    }
}

void queue_rumble_submerged(void) {
    if (gCurrDemoInput != NULL) {
        return;
    }

    gCurrRumbleSettings.slip = 4;
    gCurrRumbleSettings.vibrate = 4;
}

static void thread6_rumble_loop(UNUSED void *arg) {
    OSMesg msg;

    // osSyncPrintf("start motor thread\n");
    cancel_rumble();

    sRumblePakThreadActive = TRUE;
    // osSyncPrintf("go motor thread\n");

    while (TRUE) {
        // Block until VI
        osRecvMesg(&gRumbleThreadVIMesgQueue, &msg, OS_MESG_BLOCK);

        update_rumble_data_queue();
        update_rumble_pak();

        if (sRumblePakActive) {
            if (sRumblePakErrorCount >= 30) {
                sRumblePakActive = FALSE;
            }
        } else if ((gNumVblanks % 60) == 0) { // Check Rumble Pak status once per second.
            struct Controller *controller = (gMarioState->controller != NULL) ? gMarioState->controller : &gControllers[0];
            sRumblePakActive = !osMotorInitEx(&gSIEventMesgQueue, &gRumblePakPfs, controller->port);
            sRumblePakErrorCount = 0;
        }

        if (gRumblePakTimer > 0) {
            gRumblePakTimer--;
        }
    }
}

void cancel_rumble(void) {
    struct Controller *controller = (gMarioState->controller != NULL) ? gMarioState->controller : &gControllers[0];
    sRumblePakActive = !osMotorInitEx(&gSIEventMesgQueue, &gRumblePakPfs, controller->port);

    if (sRumblePakActive) {
        osMotorStop(&gRumblePakPfs);
    }

    for (int i = 0; i < ARRAY_COUNT(gRumbleDataQueue); i++) {
        gRumbleDataQueue[i].comm = RUMBLE_EVENT_NOMESG;
    }

    gCurrRumbleSettings.timer = 0;
    gCurrRumbleSettings.slip = 0;

    gRumblePakTimer = 0;
}

void create_thread_6(void) {
    osCreateMesgQueue(&gRumbleThreadVIMesgQueue, gRumbleThreadVIMesgBuf, 1);
    osCreateThread(&gRumblePakThread, THREAD_6_RUMBLE, thread6_rumble_loop, NULL, (gThread6Stack + THREAD6_STACK), 30);
    osStartThread(&gRumblePakThread);
}

#define VRTC (('V' << 24) | ('R' << 16) | ('T' << 8) | ('C' << 0))

void rumble_thread_update_vi(void) {
    if (!sRumblePakThreadActive) {
        return;
    }

    osSendMesg(&gRumbleThreadVIMesgQueue, (OSMesg)VRTC, OS_MESG_NOBLOCK);
}

#undef VRTC

#endif // ENABLE_RUMBLE
