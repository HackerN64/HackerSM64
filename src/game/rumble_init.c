#include <ultra64.h>
#include "macros.h"

#include "buffers/buffers.h"
#include "main.h"
#include "rumble_init.h"
#include "config.h"

#ifdef ENABLE_RUMBLE

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

void block_until_rumble_pak_free(void) {
    OSMesg msg;
    osRecvMesg(&gRumblePakSchedulerMesgQueue, &msg, OS_MESG_BLOCK);
}

void release_rumble_pak_control(void) {
    osSendMesg(&gRumblePakSchedulerMesgQueue, (OSMesg) 0, OS_MESG_NOBLOCK);
}

static void detect_rumble_pak(void) {
    if (sRumblePakActive) {
        if (sRumblePakErrorCount >= 30) {
            sRumblePakActive = FALSE;
        }
    } else if ((gNumVblanks % 60) == 0) {
        sRumblePakActive = (osMotorInitEx(&gSIEventMesgQueue, &gRumblePakPfs, gPlayer1Controller->port) == 0);
        sRumblePakErrorCount = 0;
    }

}

static void set_motor(s32 motorState) {
    if (!sRumblePakActive) {
        return;
    }

    block_until_rumble_pak_free();

    if (!__osMotorAccessEx(&gRumblePakPfs, motorState)) {
        sRumblePakErrorCount = 0;
    } else {
        sRumblePakErrorCount++;
    }

    release_rumble_pak_control();
}

static void update_rumble_pak(void) {
    if (gResetTimer > 0) {
        set_motor(MOTOR_STOP);
        return;
    }

    if (gCurrRumbleSettings.start > 0) {
        gCurrRumbleSettings.start--;
        set_motor(MOTOR_START);
    } else if (gCurrRumbleSettings.timer > 0) {
        gCurrRumbleSettings.timer--;

        gCurrRumbleSettings.level -= gCurrRumbleSettings.decay;
        if (gCurrRumbleSettings.level < 0) {
            gCurrRumbleSettings.level = 0;
        }

        if (gCurrRumbleSettings.event == RUMBLE_EVENT_CONSTON) {
            set_motor(MOTOR_START);
        } else if (gCurrRumbleSettings.count >= 0x100) {
            gCurrRumbleSettings.count -= 0x100;
            set_motor(MOTOR_START);
        } else {
            gCurrRumbleSettings.count +=
                ((gCurrRumbleSettings.level * gCurrRumbleSettings.level * gCurrRumbleSettings.level) / (1 << 9)) + 4;

            set_motor(MOTOR_STOP);
        }
    } else {
        gCurrRumbleSettings.timer = 0;

        if (gCurrRumbleSettings.slip >= 5) {
            set_motor(MOTOR_START);
        } else if ((gCurrRumbleSettings.slip >= 2) && (gNumVblanks % gCurrRumbleSettings.vibrate == 0)) {
            set_motor(MOTOR_START);
        } else {
            set_motor(MOTOR_STOP);
        }
    }

    if (gCurrRumbleSettings.slip > 0) {
        gCurrRumbleSettings.slip--;
    }

    if (gRumblePakTimer > 0) {
        gRumblePakTimer--;
    }
}

static void update_rumble_data_queue(void) {
    if (gRumbleDataQueue[0].comm != RUMBLE_EVENT_NOMESG) {
        gCurrRumbleSettings.count = 0;
        gCurrRumbleSettings.start = 4;
        gCurrRumbleSettings.event = gRumbleDataQueue[0].comm;
        gCurrRumbleSettings.timer = gRumbleDataQueue[0].time;
        gCurrRumbleSettings.level = gRumbleDataQueue[0].level;
        gCurrRumbleSettings.decay = gRumbleDataQueue[0].decay;
    }

    gRumbleDataQueue[0] = gRumbleDataQueue[1];
    gRumbleDataQueue[1] = gRumbleDataQueue[2];

    gRumbleDataQueue[2].comm = RUMBLE_EVENT_NOMESG;
}

void queue_rumble_data(UNUSED struct Controller *controller, s16 time, s16 level, s16 decay) {
    if (gCurrDemoInput != NULL) {
        return;
    }

    gRumbleDataQueue[2].comm = (level > 70) ? RUMBLE_EVENT_CONSTON : RUMBLE_EVENT_LEVELON;
    gRumbleDataQueue[2].level = level;
    gRumbleDataQueue[2].time  = time;
    gRumbleDataQueue[2].decay = decay;
}

u32 is_rumble_finished_and_queue_empty(UNUSED struct Controller *controller) {
    if (gCurrRumbleSettings.start + gCurrRumbleSettings.timer >= 4) {
        return FALSE;
    }

    if (gRumbleDataQueue[0].comm != RUMBLE_EVENT_NOMESG) return FALSE;
    if (gRumbleDataQueue[1].comm != RUMBLE_EVENT_NOMESG) return FALSE;
    if (gRumbleDataQueue[2].comm != RUMBLE_EVENT_NOMESG) return FALSE;

    return TRUE;
}

void reset_rumble_timers_slip(UNUSED struct Controller *controller) {
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

void reset_rumble_timers_vibrate(UNUSED struct Controller *controller, s32 level) {
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

void queue_rumble_submerged(UNUSED struct Controller *controller) {
    if (gCurrDemoInput != NULL) {
        return;
    }

    gCurrRumbleSettings.slip = 4;
    gCurrRumbleSettings.vibrate = 4;
}

static void thread6_rumble_loop(UNUSED void *arg) {
    OSMesg msg;

    cancel_rumble();

    sRumblePakThreadActive = TRUE;

    while (TRUE) {
        // Block until VI
        osRecvMesg(&gRumbleThreadVIMesgQueue, &msg, OS_MESG_BLOCK);

        update_rumble_data_queue();
        update_rumble_pak();
        detect_rumble_pak();
    }
}

void cancel_rumble(void) {
    sRumblePakActive = (osMotorInitEx(&gSIEventMesgQueue, &gRumblePakPfs, gPlayer1Controller->port) == 0);

    if (sRumblePakActive) {
        osMotorStop(&gRumblePakPfs);
    }

    gRumbleDataQueue[0].comm = RUMBLE_EVENT_NOMESG;
    gRumbleDataQueue[1].comm = RUMBLE_EVENT_NOMESG;
    gRumbleDataQueue[2].comm = RUMBLE_EVENT_NOMESG;

    gCurrRumbleSettings.timer = 0;
    gCurrRumbleSettings.slip  = 0;

    gRumblePakTimer = 0;
}

void create_thread_6_rumble(void) {
    // Create the Rumble Pak scheduler message queue.
    osCreateMesgQueue(&gRumblePakSchedulerMesgQueue, gRumblePakSchedulerMesgBuf, 1);
    osSendMesg(&gRumblePakSchedulerMesgQueue, (OSMesg) 0, OS_MESG_NOBLOCK);

    // Create the rumble thread.
    osCreateMesgQueue(&gRumbleThreadVIMesgQueue, gRumbleThreadVIMesgBuf, 1);
    osCreateThread(&gRumblePakThread, THREAD_6_RUMBLE, thread6_rumble_loop, NULL, (gThread6Stack + THREAD6_STACK), 30);
    osStartThread(&gRumblePakThread);
}

void rumble_thread_update_vi(void) {
    union {
        char asStr[sizeof("VRTC")];
        OSMesg asMesg;
    } VRTC = { .asStr = "VRTC" };

    if (!sRumblePakThreadActive) {
        return;
    }

    osSendMesg(&gRumbleThreadVIMesgQueue, VRTC.asMesg, OS_MESG_NOBLOCK);
}

#endif
