#include <ultra64.h>
#include "macros.h"
#include "config.h"

#include "buffers/buffers.h"
#include "main.h"
#include "rumble_init.h"

#ifdef ENABLE_RUMBLE

OSThread gRumblePakThread;

OSPfs gRumblePakPfs;

OSMesg gRumblePakSchedulerMesgBuf[1];
OSMesgQueue gRumblePakSchedulerMesgQueue;
OSMesg gRumbleThreadVIMesgBuf[1];
OSMesgQueue gRumbleThreadVIMesgQueue;

struct RumbleData gRumbleDataQueue[RUMBLE_QUEUE_SIZE];
struct RumbleSettings gCurrRumbleSettings;

s32 sRumblePakThreadActive = FALSE; // Set to TRUE when the rumble thread starts.
s32 sRumblePakActive = FALSE;
s32 sRumblePakErrorCount = 0;
s32 gRumblePakTimer = 0;

/**
 * @brief Locks controller input data while reading new inputs or another thread is using the current inputs.
 * This prevents new inputs overwriting the current inputs while they are in use.
 */
void block_until_rumble_pak_free(void) {
    OSMesg msg;
    osRecvMesg(&gRumblePakSchedulerMesgQueue, &msg, OS_MESG_BLOCK);
}

/**
 * @brief Unlocks controller input data, allowing to read new inputs or another thread to access the most recently
 * polled inputs.
 */
void release_rumble_pak_control(void) {
    osSendMesg(&gRumblePakSchedulerMesgQueue, (OSMesg) 0, OS_MESG_NOBLOCK);
}

/**
 * @brief Check the rumble pak status.
 * Called by thread6_rumble_loop.
 */
static void detect_rumble_pak(void) {
    if (sRumblePakActive) {
        // Disable the rumble pak if there were too many failed start/stop attempts without a success.
        if (sRumblePakErrorCount >= RUMBLE_MAX_ERRORS) {
            sRumblePakActive = FALSE;
        }
    } else if ((gNumVblanks % RUMBLE_PAK_CHECK_TIME) == 0) { // Check the Rumble Pak status about once per second.
        sRumblePakActive = (osMotorInitEx(&gSIEventMesgQueue, &gRumblePakPfs, gPlayer1Controller->port) == PFS_ERR_SUCCESS);
        sRumblePakErrorCount = 0;
    }

}

/**
 * @brief Turn the Rumble Pak motor on or off. This is called every frame.
 *
 * @param[in] motorState MOTOR_STOP = stop motor, MOTOR_START = start motor, MOTOR_STOP_HARD (GCN only) = motor brake.
 */
static void set_motor(s32 motorState) {
    // Check the rumble pak's status and make sure it's initialized.
    if (!sRumblePakActive) {
        return;
    }

    block_until_rumble_pak_free();

    // Equivalent to osMotorStart or osMotorStop.
    if (!__osMotorAccessEx(&gRumblePakPfs, motorState)) {
        sRumblePakErrorCount = 0;
    } else {
        sRumblePakErrorCount++;
    }

    release_rumble_pak_control();
}

/**
 * @brief Handle turning the motor on/off based on current rumble settings data.
 */
static void update_rumble_pak(void) {
    // Stop rumble after pressing the reset button.
    if (gResetTimer > 0) {
        set_motor(MOTOR_STOP);
        return;
    }

    // Before doing anything else, rumble for the duration of RUMBLE_START_TIME.
    if (gCurrRumbleSettings.start > 0) { // Start phase
        gCurrRumbleSettings.start--;

        set_motor(MOTOR_START);
    } else if (gCurrRumbleSettings.timer > 0) { // Timer phase.
        // Handle rumbling during the duration of the timer.
        gCurrRumbleSettings.timer--;

        // Reduce 'level' by 'decay' until 0.
        gCurrRumbleSettings.level -= gCurrRumbleSettings.decay;
        if (gCurrRumbleSettings.level < 0) {
            gCurrRumbleSettings.level = 0;
        }

        // Rumble event type.
        if (gCurrRumbleSettings.event == RUMBLE_EVENT_CONSTON) {
            // Constant rumble for the duration of the timer phase.
            set_motor(MOTOR_START);
        } else { // RUMBLE_EVENT_LEVELON
            // Modulate rumble based on 'count' and 'level'.
            // Rumble when ((count + (((level^3) / 512) + RUMBLE_START_TIME)) >= 256).
            if (gCurrRumbleSettings.count >= 0x100) {
                gCurrRumbleSettings.count -= 0x100;

                set_motor(MOTOR_START);
            } else { // count < 256, stop rumbling until count >= 256 again.
                s16 level = gCurrRumbleSettings.level;
                gCurrRumbleSettings.count += ((level * level * level) / 0x200) + RUMBLE_START_TIME;

                set_motor(MOTOR_STOP);
            }
        }
    } else { // Slip phase.
        // Reached end of timer.
        gCurrRumbleSettings.timer = 0;

        if (gCurrRumbleSettings.slip >= 5) { // Rumble until 'slip' gets too low.
            set_motor(MOTOR_START);
        } else if ((gCurrRumbleSettings.slip >= 2) && (gNumVblanks % gCurrRumbleSettings.vibrate == 0)) { // Rumble every 'vibrate' frames.
            set_motor(MOTOR_START);
        } else { // Rumble fully ended.
            set_motor(MOTOR_STOP);
        }
    }

    // 'slip' decrements regardless of timer state.
    if (gCurrRumbleSettings.slip > 0) {
        gCurrRumbleSettings.slip--;
    }

    // Update the timer for the rumble that occurs when nearly out of breath.
    if (gRumblePakTimer > 0) {
        gRumblePakTimer--;
    }
}

/**
 * @brief Rumble commands are written to the end of the queue, move down through the queue each frame, and trigger when they reach the beginning.
 */
static void update_rumble_data_queue(void) {
    // If the first queue entry has a command...
    if (gRumbleDataQueue[0].event != RUMBLE_EVENT_NOMESG) {
        gCurrRumbleSettings.count = 0;
        gCurrRumbleSettings.start = RUMBLE_START_TIME;

        // Copy the first command in the queue to current settings.
        gCurrRumbleSettings.event = gRumbleDataQueue[0].event;
        gCurrRumbleSettings.timer = gRumbleDataQueue[0].timer;
        gCurrRumbleSettings.level = gRumbleDataQueue[0].level;
        gCurrRumbleSettings.decay = gRumbleDataQueue[0].decay;
    }

    // Copy each queue entry to the previous one every frame.
    for (int i = 0; i < (RUMBLE_QUEUE_SIZE - 1); i++) {
        gRumbleDataQueue[i] = gRumbleDataQueue[i + 1];
    }

    // Disable the last command in the queue.
    gRumbleDataQueue[RUMBLE_QUEUE_SIZE - 1].event = RUMBLE_EVENT_NOMESG;
}

void queue_rumble_data(UNUSED struct Controller *controller, s16 timer, s16 level, s16 decay) {
    if (gCurrDemoInput != NULL) {
        return;
    }

    struct RumbleData *queueEnd = &gRumbleDataQueue[RUMBLE_QUEUE_SIZE - 1];

    // Write the rumble command.
    queueEnd->event = (level > 70) ? RUMBLE_EVENT_CONSTON : RUMBLE_EVENT_LEVELON;
    queueEnd->level = level;
    queueEnd->timer = timer;
    queueEnd->decay = decay;
}

/**
 * @brief Used after setting gRumblePakTimer to check if any rumble commands are being executed or queued.
 *
 * @param[in] controller A pointer to the controller to rumble.
 * @returns s32 Boolean, whether the controller is done rumbling.
 */
u32 is_rumble_finished_and_queue_empty(UNUSED struct Controller *controller) {
    // Check whether currently rumbling.
    if (gCurrRumbleSettings.start + gCurrRumbleSettings.timer >= RUMBLE_START_TIME) {
        return FALSE;
    }

    // Check the rumble command queue.
    for (int i = 0; i < RUMBLE_QUEUE_SIZE; i++) {
        if (gRumbleDataQueue[i].event != RUMBLE_EVENT_NOMESG) {
            return FALSE;
        }
    }

    return TRUE;
}

/**
 * @brief Resets the 'slip' timer.
 */
static void reset_rumble_slip(void) {
    if (gCurrRumbleSettings.slip == 0) {
        gCurrRumbleSettings.slip = 7;
    }

    if (gCurrRumbleSettings.slip < RUMBLE_START_TIME) {
        gCurrRumbleSettings.slip = RUMBLE_START_TIME;
    }
}

/**
 * @brief Resets the 'slip' timer and sets 'vibrate' to 7.
 *
 * @param[in] controller A pointer to the controller to rumble.
 */
void reset_rumble_timers_slip(UNUSED struct Controller *controller) {
    if (gCurrDemoInput != NULL) {
        return;
    }

    reset_rumble_slip();

    gCurrRumbleSettings.vibrate = 7;
}

/**
 * @brief Resets the 'slip' timer and sets 'vibrate' based on the arg.
 *
 * @param[in] controller A pointer to the controller to rumble.
 * @param[in] level      Used to modulate rumble when 'event' is RUMBLE_EVENT_LEVELON.
 */
void reset_rumble_timers_vibrate(UNUSED struct Controller *controller, s32 level) {
    if (gCurrDemoInput != NULL) {
        return;
    }

    reset_rumble_slip();

    if (level < 5) {
        gCurrRumbleSettings.vibrate = (5 - level);
    }
}

/**
 * @brief Bypasses the queue by changing the current rumble command directly.
 * Called by act_breaststroke.
 *
 * @param[in] controller A pointer to the controller to rumble.
 */
void queue_rumble_submerged(UNUSED struct Controller *controller) {
    if (gCurrDemoInput != NULL) {
        return;
    }

    gCurrRumbleSettings.slip    = RUMBLE_START_TIME;
    gCurrRumbleSettings.vibrate = RUMBLE_START_TIME;
}

/**
 * @brief Rumble thread loop.
 *
 * @param[in] arg Unused argument.
 */
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

/**
 * @brief Reinitialize the Rumble Pak and stops the motor.
 */
void cancel_rumble(void) {
    // Check the rumble pak status.
    sRumblePakActive = (osMotorInitEx(&gSIEventMesgQueue, &gRumblePakPfs, gPlayer1Controller->port) == PFS_ERR_SUCCESS);

    // Stop the rumble pak if it's plugged in.
    set_motor(MOTOR_STOP);

    // Clear the command queue.
    for (int i = 0; i < RUMBLE_QUEUE_SIZE; i++) {
        gRumbleDataQueue[i].event = RUMBLE_EVENT_NOMESG;
    }

    // Reset timers.
    gCurrRumbleSettings.timer = 0;
    gCurrRumbleSettings.slip  = 0;

    gRumblePakTimer = 0;
}

/**
 * @brief Creates the Rumble Pak scheduler message queue and thread.
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
 * @brief Sends a "VRTC" message on gRumbleThreadVIMesgQueue every vblank.
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
#endif
