#include <ultra64.h>
#include "macros.h"
#include "config.h"

#include "buffers/buffers.h"
#include "main.h"
#include "rumble_init.h"
#include "level_update.h"
#include "joybus.h"
#include "game_input.h"

#ifdef ENABLE_RUMBLE

OSThread gRumblePakThread;

OSMesg gRumblePakSchedulerMesgBuf[1];
OSMesgQueue gRumblePakSchedulerMesgQueue;
OSMesg gRumbleThreadVIMesgBuf[1];
OSMesgQueue gRumbleThreadVIMesgQueue;

s32 sRumblePakThreadActive = FALSE;             // Set to TRUE when the rumble thread starts.

struct RumbleInfo gRumbleInfos[MAXCONTROLLERS] = { 0 };

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

/**
 * Turn the Rumble Pak motor on or off. This is called every frame.
 * flag = MOTOR_STOP, MOTOR_START, or MOTOR_STOP_HARD (MOTOR_STOP_HARD is for GameCube controllers only).
 */
static void set_rumble(int channel, s32 flag) {
    struct RumbleInfo *info = &gRumbleInfos[channel];
    OSPfs *pfs = &info->pfs;

    // Check the rumble pak's status and make sure it's initialized to the correct port.
    if (!(pfs->status & PFS_MOTOR_INITIALIZED) || pfs->channel != channel) {
        return;
    }

    // Don't run if motor state wouldn't change.
    // Bypass this once per second just to allow __osMotorAccessEx to run to check for errors (eg. the rumble pak being unplugged).
    if (flag == info->state && ((gNumVblanks % RUMBLE_PAK_CHECK_TIME) != 0)) {
        return;
    }

    info->state = flag;

    block_until_rumble_pak_free();

    // Equivalent to osMotorStart or osMotorStop.
    info->error = __osMotorAccessEx(pfs, flag);

    release_rumble_pak_control();
}

/**
 * Handle turning the motor on/off based on current rumble settings data.
 */
static void update_rumble_pak(int channel) {
    struct RumbleSettings *settings = &gRumbleInfos[channel].settings;

    // Stop rumble after pressing the reset button.
    if (gResetTimer > 0) {
        set_rumble(channel, MOTOR_STOP);
        return;
    }

    // Before doing anything else, rumble for the duration of RUMBLE_START_TIME.
    if (settings->start > 0) { // Start phase.
        settings->start--;

        set_rumble(channel, MOTOR_START);
    } else if (settings->timer > 0) { // Timer phase.
        // Handle rumbling during the duration of the timer.
        settings->timer--;

        // Reduce 'level' by 'decay' until 0.
        settings->level -= settings->decay;
        if (settings->level < 0) {
            settings->level = 0;
        }

        // Rumble event type.
        if (settings->event == RUMBLE_EVENT_CONSTON) {
            // Constant rumble for the duration of the timer phase.
            set_rumble(channel, MOTOR_START);
        } else { // RUMBLE_EVENT_LEVELON
            // Modulate rumble based on 'count' and 'level'.
            // Rumble when ((count + (((level^3) / 512) + RUMBLE_START_TIME)) >= 256).
            if (settings->count >= 0x100) {
                settings->count -= 0x100;

                set_rumble(channel, MOTOR_START);
            } else { // count < 256, stop rumbling until count >= 256 again.
                s16 level = settings->level;
                settings->count += ((level * level * level) / 0x200) + RUMBLE_START_TIME;

                set_rumble(channel, MOTOR_STOP);
            }
        }
    } else { // Slip phase.
        // Reached end of timer.
        settings->timer = 0;

        if (settings->slip >= 5) { // Rumble until 'slip' gets too low.
            set_rumble(channel, MOTOR_START);
        } else if ((settings->slip >= 2) && ((gNumVblanks % settings->vibrate) == 0)) { // Rumble every 'vibrate' frames.
            set_rumble(channel, MOTOR_START);
        } else { // Rumble fully ended.
            set_rumble(channel, MOTOR_STOP);
        }
    }

    // 'slip' decrements regardless of timer state.
    if (settings->slip > 0) {
        settings->slip--;
    }
}

// Rumble commands are written to the end of the queue, move down through the queue each frame, and trigger when they reach the beginning.
static void update_rumble_data_queue(int channel) {
    struct RumbleSettings *settings = &gRumbleInfos[channel].settings;
    struct RumbleData *queue = &gRumbleInfos[channel].queue[0];

    // If the first queue entry has a command
    if (queue[0].event != RUMBLE_EVENT_NOMESG) {
        settings->count = 0;
        settings->start = RUMBLE_START_TIME;

        // Copy the first command in the queue to current settings.
        settings->event = queue[0].event;
        settings->timer = queue[0].timer;
        settings->level = queue[0].level;
        settings->decay = queue[0].decay;
    }

    // Copy each queue entry to the previous one every frame.
    for (int i = 0; i < (RUMBLE_DATA_QUEUE_SIZE - 1); i++) {
        queue[i] = queue[i + 1];
    }

    // Disable the last command in the queue.
    queue[RUMBLE_DATA_QUEUE_SIZE - 1].event = RUMBLE_EVENT_NOMESG;
}

/**
 * Writes a rumble command to the end of the queue.
 */
void queue_rumble_data(struct Controller *controller, s16 timer, s16 level) {
    if (gCurrDemoInput != NULL) {
        return;
    }

    struct RumbleData *queueEnd = &gRumbleInfos[controller->port].queue[RUMBLE_DATA_QUEUE_SIZE - 1];

    // Write the rumble command.
    queueEnd->event = (level > 70) ? RUMBLE_EVENT_CONSTON : RUMBLE_EVENT_LEVELON;
    queueEnd->level = level;
    queueEnd->timer = timer;
    queueEnd->decay = 0;
}

/**
 * Sets the 'decay' parameter for the last (most recently set) command in the queue.
 * Called after queue_rumble_data.
 */
void queue_rumble_decay(struct Controller *controller, s16 decay) {
    gRumbleInfos[controller->port].queue[RUMBLE_DATA_QUEUE_SIZE - 1].decay = decay;
}

/**
 * Used after setting gRumblePakTimer to check if any rumble commands are being executed or queued.
 */
u32 is_rumble_finished_and_queue_empty(struct Controller *controller) {
    struct RumbleInfo *info = &gRumbleInfos[controller->port];

    // Check whether currently rumbling.
    if (info->settings.start + info->settings.timer >= RUMBLE_START_TIME) {
        return FALSE;
    }

    // Check the rumble command queue.
    for (int i = 0; i < RUMBLE_DATA_QUEUE_SIZE; i++) {
        if (info->queue[i].event != RUMBLE_EVENT_NOMESG) {
            return FALSE;
        }
    }

    return TRUE;
}

/**
 * Resets the 'slip' timer.
 */
static void reset_rumble_slip(struct RumbleSettings *settings) {
    if (settings->slip == 0) {
        settings->slip = 7;
    }

    if (settings->slip < RUMBLE_START_TIME) {
        settings->slip = RUMBLE_START_TIME;
    }
}

/**
 * Resets the 'slip' timer and sets 'vibrate' to 7.
 */
void reset_rumble_timers_slip(struct Controller *controller) {
    struct RumbleSettings *settings = &gRumbleInfos[controller->port].settings;

    if (gCurrDemoInput != NULL) {
        return;
    }

    reset_rumble_slip(settings);

    settings->vibrate = 7;
}

/**
 * Resets the 'slip' timer and sets 'vibrate' based on the arg.
 */
void reset_rumble_timers_vibrate(struct Controller *controller, s32 level) {
    struct RumbleSettings *settings = &gRumbleInfos[controller->port].settings;

    if (gCurrDemoInput != NULL) {
        return;
    }

    reset_rumble_slip(settings);

    if (level < 5) {
        settings->vibrate = (5 - level);
    }
}

/**
 * Bypasses the queue by changing rumble settings directly.
 * Called by act_breaststroke.
 */
void queue_rumble_submerged(struct Controller *controller) {
    struct RumbleSettings *settings = &gRumbleInfos[controller->port].settings;

    if (gCurrDemoInput != NULL) {
        return;
    }

    settings->slip    = RUMBLE_START_TIME;
    settings->vibrate = RUMBLE_START_TIME;
}

ALIGNED8 static const char *sPfsErrorDesc[] = {
    [PFS_ERR_SUCCESS     ] = "successful",                  /* no error                                     */
    [PFS_ERR_NOPACK      ] = "no pak",                      /* no memory card is plugged or                 */
    [PFS_ERR_NEW_PACK    ] = "changed pak",                 /* ram pack has been changed to a different one */
    [PFS_ERR_INCONSISTENT] = "inconsistent, run Pfs check", /* need to run Pfschecker                       */
    [PFS_ERR_CONTRFAIL   ] = "communication error",         /* CONT_OVERRUN_ERROR                           */
    [PFS_ERR_INVALID     ] = "invalid param or no file",    /* invalid parameter or file not exist          */
    [PFS_ERR_BAD_DATA    ] = "bad data read",               /* the data read from pack are bad              */
    [PFS_DATA_FULL       ] = "pages full",                  /* no free pages on ram pack                    */
    [PFS_DIR_FULL        ] = "directories full",            /* no free directories on ram pack              */
    [PFS_ERR_EXIST       ] = "file exists",                 /* file exists                                  */
    [PFS_ERR_ID_FATAL    ] = "dead pak",                    /* dead ram pack                                */
    [PFS_ERR_DEVICE      ] = "wrong type",                  /* wrong device type                            */
    [PFS_ERR_NO_GBCART   ] = "no GB cart",                  /* no gb cartridge (64GB-PAK)                   */
    [PFS_ERR_NEW_GBCART  ] = "changed GB cart",             /* gb cartridge may be changed                  */
};

/**
 * Rumble thread loop.
 */
static void thread6_rumble_loop(UNUSED void *arg) {
    OSMesg msg;

    cancel_rumble();

    sRumblePakThreadActive = TRUE;

    while (TRUE) {
        // Block until VI
        osRecvMesg(&gRumbleThreadVIMesgQueue, &msg, OS_MESG_BLOCK);

        for (int channel = 0; channel < __osMaxControllers; channel++) {
            update_rumble_data_queue(channel);
            update_rumble_pak(channel);

            struct RumbleInfo *info = &gRumbleInfos[channel];

            if (info->pfs.status & PFS_MOTOR_INITIALIZED) {
                // Disable the rumble pak if there were too many failed start/stop attempts without a success.
                if (info->error != PFS_ERR_SUCCESS) {
                    info->pfs.status = PFS_STATUS_NONE;
                    info->state = MOTOR_STOP;
                    osSyncPrintf("Rumble Pak error (%d): %s\n", info->error, sPfsErrorDesc[info->error]);
                }
            } else {
                if ((gNumVblanks % RUMBLE_PAK_CHECK_TIME) == 0) { // Check Rumble Pak status about once per second.
                    if (osMotorInitEx(&gSIEventMesgQueue, &info->pfs, channel) != PFS_ERR_SUCCESS) {
                        gRumbleInfos[channel].state = MOTOR_STOP;
                    }
                    info->error = PFS_ERR_SUCCESS;
                }
            }

            if (info->timer > 0) {
                info->timer--;
            }
        }
    }
}

/**
 * Reinitialize the Rumble Pak and stop the motor.
 */
void cancel_rumble(void) {
    for (int channel = 0; channel < __osMaxControllers; channel++) {
        struct RumbleInfo *info = &gRumbleInfos[channel];

        // Check the rumble pak status.
        osMotorInitEx(&gSIEventMesgQueue, &info->pfs, channel);

        // Stop the rumble pak if it's plugged in.
        set_rumble(channel, MOTOR_STOP);

        // Clear the command queue.
        for (int i = 0; i < RUMBLE_DATA_QUEUE_SIZE; i++) {
            info->queue[i].event = RUMBLE_EVENT_NOMESG;
        }

        // Reset timers.
        info->settings.timer = 0;
        info->settings.slip  = 0;

        info->timer = 0;
    }
}

/**
 * Creates the Rumble Pak scheduler message queue and thread.
 * Called by thread5_game_loop.
 */
void create_thread_6_rumble(void) {
    // Create the Rumble Pak scheduler message queue.
    osCreateMesgQueue(&gRumblePakSchedulerMesgQueue, gRumblePakSchedulerMesgBuf, 1);
    osSendMesg(&gRumblePakSchedulerMesgQueue, (OSMesg) 0, OS_MESG_NOBLOCK);

    // Create the rumble thread.
    osCreateMesgQueue(&gRumbleThreadVIMesgQueue, gRumbleThreadVIMesgBuf, 1);
    osCreateThread(&gRumblePakThread, THREAD_6_RUMBLE, thread6_rumble_loop, NULL, (gThread6Stack + THREAD6_STACK), 30);
    osStartThread(&gRumblePakThread);
}

/**
 * Sends a "VRTC" message on gRumbleThreadVIMesgQueue every vblank.
 * Called by handle_vblank.
 */
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

#endif // ENABLE_RUMBLE
