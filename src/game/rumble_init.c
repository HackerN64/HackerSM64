#include <ultra64.h>
#include "macros.h"

#include "buffers/buffers.h"
#include "main.h"
#include "rumble_init.h"
#include "config.h"

#if ENABLE_RUMBLE

OSThread gRumblePakThread;

OSPfs gRumblePakPfs;

OSMesg gRumblePakSchedulerMesgBuf[1];
OSMesgQueue gRumblePakSchedulerMesgQueue;
OSMesg gRumbleThreadVIMesgBuf[1];
OSMesgQueue gRumbleThreadVIMesgQueue;

struct RumbleData gRumbleDataQueue[3];
struct RumbleSettings gCurrRumbleSettings;

s32 sRumblePakThreadActive = FALSE;
s32 sRumblePakActive = FALSE;
s32 sRumblePakErrorCount = 0;
s32 gRumblePakTimer = 0;

void init_rumble_pak_scheduler_queue(void) {
    osCreateMesgQueue(&gRumblePakSchedulerMesgQueue, gRumblePakSchedulerMesgBuf, 1);
    osSendMesg(&gRumblePakSchedulerMesgQueue, (OSMesg) 0, OS_MESG_NOBLOCK);
}

void block_until_rumble_pak_free(void) {
    OSMesg msg;
    osRecvMesg(&gRumblePakSchedulerMesgQueue, &msg, OS_MESG_BLOCK);
}

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

        if (gCurrRumbleSettings.event == 1) {
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
        } else if ((gCurrRumbleSettings.slip >= 2) && (gNumVblanks % gCurrRumbleSettings.vibrate == 0)) {
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
        gCurrRumbleSettings.event  = gRumbleDataQueue[0].comm;
        gCurrRumbleSettings.timer = gRumbleDataQueue[0].time;
        gCurrRumbleSettings.level = gRumbleDataQueue[0].level;
        gCurrRumbleSettings.decay = gRumbleDataQueue[0].decay;
    }

    gRumbleDataQueue[0] = gRumbleDataQueue[1];
    gRumbleDataQueue[1] = gRumbleDataQueue[2];

    gRumbleDataQueue[2].comm = 0;
}

void queue_rumble_data(s16 time, s16 level) {
    if (gCurrDemoInput != NULL) {
        return;
    }

    if (level > 70) {
        gRumbleDataQueue[2].comm = 1;
    } else {
        gRumbleDataQueue[2].comm = 2;
    }

    gRumbleDataQueue[2].level = level;
    gRumbleDataQueue[2].time = time;
    gRumbleDataQueue[2].decay = 0;
}

void queue_rumble_decay(s16 decay) {
    gRumbleDataQueue[2].decay = decay;
}

u32 is_rumble_finished_and_queue_empty(void) {
    if (gCurrRumbleSettings.start + gCurrRumbleSettings.timer >= 4) {
        return FALSE;
    }

    if (gRumbleDataQueue[0].comm != 0) return FALSE;
    if (gRumbleDataQueue[1].comm != 0) return FALSE;
    if (gRumbleDataQueue[2].comm != 0) return FALSE;

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

    switch (level) {
        case 0: gCurrRumbleSettings.vibrate = 5; break;
        case 1: gCurrRumbleSettings.vibrate = 4; break;
        case 2: gCurrRumbleSettings.vibrate = 3; break;
        case 3: gCurrRumbleSettings.vibrate = 2; break;
        case 4: gCurrRumbleSettings.vibrate = 1; break;
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

	osSyncPrintf("start motor thread\n");
    cancel_rumble();

    sRumblePakThreadActive = TRUE;
	osSyncPrintf("go motor thread\n");

    while (TRUE) {
        // Block until VI
        osRecvMesg(&gRumbleThreadVIMesgQueue, &msg, OS_MESG_BLOCK);

        update_rumble_data_queue();
        update_rumble_pak();

        if (sRumblePakActive) {
            if (sRumblePakErrorCount >= 30) {
                sRumblePakActive = FALSE;
            }
        } else if (gNumVblanks % 60 == 0) {
            sRumblePakActive = osMotorInit(&gSIEventMesgQueue, &gRumblePakPfs, gPlayer1Controller->port) < 1;
            sRumblePakErrorCount = 0;
        }

        if (gRumblePakTimer > 0) {
            gRumblePakTimer--;
        }
    }
}

void cancel_rumble(void) {
    sRumblePakActive = osMotorInit(&gSIEventMesgQueue, &gRumblePakPfs, gPlayer1Controller->port) < 1;

    if (sRumblePakActive) {
        osMotorStop(&gRumblePakPfs);
    }

    gRumbleDataQueue[0].comm = 0;
    gRumbleDataQueue[1].comm = 0;
    gRumbleDataQueue[2].comm = 0;

    gCurrRumbleSettings.timer = 0;
    gCurrRumbleSettings.slip  = 0;

    gRumblePakTimer = 0;
}

void create_thread_6(void) {
    osCreateMesgQueue(&gRumbleThreadVIMesgQueue, gRumbleThreadVIMesgBuf, 1);
    osCreateThread(&gRumblePakThread, THREAD_6_RUMBLE, thread6_rumble_loop, NULL, gThread6Stack + 0x2000, 30);
    osStartThread(&gRumblePakThread);
}

#define VRTC 0x56525443

void rumble_thread_update_vi(void) {
    if (!sRumblePakThreadActive) {
        return;
    }

    osSendMesg(&gRumbleThreadVIMesgQueue, (OSMesg) VRTC, OS_MESG_NOBLOCK);
}

#undef VRTC

#endif
