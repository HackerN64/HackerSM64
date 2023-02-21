#include <ultra64.h>
#include "macros.h"
#include "config.h"

#include "buffers/buffers.h"
#include "main.h"
#include "rumble_init.h"
#include "level_update.h"
#include "controller.h"
#include "game_input.h"

#ifdef ENABLE_RUMBLE

OSThread gRumblePakThread;

OSPfs gRumblePakPfs; // Rumble Pak file system data.

OSMesg gRumblePakSchedulerMesgBuf[1];
OSMesgQueue gRumblePakSchedulerMesgQueue;
OSMesg gRumbleThreadVIMesgBuf[1];
OSMesgQueue gRumbleThreadVIMesgQueue;

struct RumbleData gRumbleDataQueue[RUMBLE_DATA_QUEUE_SIZE] = { 0 };
struct RumbleSettings gCurrRumbleSettings = { 0 };

s32 sRumblePakThreadActive = FALSE;             // Set to TRUE when the rumble thread starts.
s32 sRumblePakActive       = FALSE;             // Whether the rumble pak is plugged in.
s32 sRumblePakMotorState   = MOTOR_STOP;        // Current rumble motor state.
s32 sRumblePakError        = PFS_ERR_SUCCESS;   // The last error from a failed motor start/stop.
s32 sRumblePakErrorCount   = 0;                 // Number of failed motor start/stops.
s32 gRumblePakTimer        = 0;                 // Only used to time the drowning warning rumble.

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
 * Turn the Rumble Pak motor on or off.
 * flag = MOTOR_STOP, MOTOR_START, or MOTOR_STOP_HARD (GameCube controller only).
 */
static void set_rumble(s32 flag) {
    if (!sRumblePakActive) {
        return;
    }

    // Don't run if already set.
    if (flag == sRumblePakMotorState) {
        return;
    }

    sRumblePakMotorState = flag;

    block_until_rumble_pak_free();

    // Equivalent to osMotorStart or osMotorStop.
    s32 err = __osMotorAccessEx(&gRumblePakPfs, flag);

    if (err == PFS_ERR_SUCCESS) {
        sRumblePakErrorCount = 0;
    } else {
        sRumblePakErrorCount++;
        sRumblePakError = err;
    }

    release_rumble_pak_control();
}

/**
 * Handle turning the motor on/off based on current rumble settings data.
 */
static void update_rumble_pak(void) {
    // Stop rumble after pressing the reset button.
    if (gResetTimer > 0) {
        set_rumble(MOTOR_STOP);
        return;
    }

    // Before doing anything else, rumble for the duration of RUMBLE_START_TIME.
    if (gCurrRumbleSettings.start > 0) { // Start phase.
        gCurrRumbleSettings.start--;

        set_rumble(MOTOR_START);
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
            set_rumble(MOTOR_START);
        } else { // RUMBLE_EVENT_LEVELON
            // Modulate rumble based on 'count' and 'level'.
            // Rumble when ((count + (((level^3) / 512) + RUMBLE_START_TIME)) >= 256).
            if (gCurrRumbleSettings.count >= 0x100) {
                gCurrRumbleSettings.count -= 0x100;

                set_rumble(MOTOR_START);
            } else { // count < 256, stop rumbling until count >= 256 again.
                s16 level = gCurrRumbleSettings.level;
                gCurrRumbleSettings.count += ((level * level * level) / 0x200) + RUMBLE_START_TIME;

                set_rumble(MOTOR_STOP);
            }
        }
    } else { // Slip phase.
        // Reached end of timer.
        gCurrRumbleSettings.timer = 0;

        if (gCurrRumbleSettings.slip >= 5) { // Rumble until 'slip' gets too low.
            set_rumble(MOTOR_START);
        } else if ((gCurrRumbleSettings.slip >= 2) && ((gNumVblanks % gCurrRumbleSettings.vibrate) == 0)) { // Rumble every 'vibrate' frames.
            set_rumble(MOTOR_START);
        } else { // Rumble fully ended.
            set_rumble(MOTOR_STOP);
        }
    }

    // 'slip' decrements regardless of timer state.
    if (gCurrRumbleSettings.slip > 0) {
        gCurrRumbleSettings.slip--;
    }
}

// Rumble commands are written to the end of the queue, move down through the queue each frame, and trigger when they reach the beginning.
static void update_rumble_data_queue(void) {
    // If the first queue entry has a command
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
    for (int i = 0; i < (ARRAY_COUNT(gRumbleDataQueue) - 1); i++) {
        gRumbleDataQueue[i] = gRumbleDataQueue[i + 1];
    }

    // Disable the last command in the queue.
    gRumbleDataQueue[RUMBLE_DATA_QUEUE_SIZE - 1].event = RUMBLE_EVENT_NOMESG;
}

/**
 * Writes a rumble command to the end of the queue.
 */
void queue_rumble_data(s16 timer, s16 level) {
    if (gCurrDemoInput != NULL) {
        return;
    }

    // Write the rumble command.
    gRumbleDataQueue[RUMBLE_DATA_QUEUE_SIZE - 1].event = (level > 70) ? RUMBLE_EVENT_CONSTON : RUMBLE_EVENT_LEVELON;
    gRumbleDataQueue[RUMBLE_DATA_QUEUE_SIZE - 1].level = level;
    gRumbleDataQueue[RUMBLE_DATA_QUEUE_SIZE - 1].timer = timer;
    gRumbleDataQueue[RUMBLE_DATA_QUEUE_SIZE - 1].decay = 0;
}

/**
 * Sets the 'decay' parameter for the last command in the queue.
 * Called after queue_rumble_data.
 */
void queue_rumble_decay(s16 decay) {
    gRumbleDataQueue[RUMBLE_DATA_QUEUE_SIZE - 1].decay = decay;
}

/**
 * Used after setting gRumblePakTimer to check if any rumble commands are being executed or queued.
 */
u32 is_rumble_finished_and_queue_empty(void) {
    // Check whether currently rumbling.
    if (gCurrRumbleSettings.start + gCurrRumbleSettings.timer >= RUMBLE_START_TIME) {
        return FALSE;
    }

    // Check the rumble command queue.
    for (int i = 0; i < ARRAY_COUNT(gRumbleDataQueue); i++) {
        if (gRumbleDataQueue[i].event != RUMBLE_EVENT_NOMESG) {
            return FALSE;
        }
    }

    return TRUE;
}

/**
 * Resets the 'slip' timer.
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
 * Resets the 'slip' timer and sets 'vibrate' to 7.
 */
void reset_rumble_timers_slip(void) {
    if (gCurrDemoInput != NULL) {
        return;
    }

    reset_rumble_slip();

    gCurrRumbleSettings.vibrate = 7;
}

/**
 * Resets the 'slip' timer and sets 'vibrate' based on the arg.
 */
void reset_rumble_timers_vibrate(s32 level) {
    if (gCurrDemoInput != NULL) {
        return;
    }

    reset_rumble_slip();

    if (level < 5) {
        gCurrRumbleSettings.vibrate = (5 - level);
    }
}

/**
 * Called by act_breaststroke. Ignores the rumble queue.
 */
void queue_rumble_submerged(void) {
    if (gCurrDemoInput != NULL) {
        return;
    }

    gCurrRumbleSettings.slip    = RUMBLE_START_TIME;
    gCurrRumbleSettings.vibrate = RUMBLE_START_TIME;
}

/**
 * Initializes the Rumble Pak and checks its status.
 * Returns whether the Rumble Pak is active.
 * Called by thread6_rumble_loop and cancel_rumble.
 */
static s32 init_and_check_rumble_pak(void) {
    struct Controller *controller = (gMarioState->controller != NULL) ? gMarioState->controller : &gControllers[0];
    s32 err = osMotorInitEx(&gSIEventMesgQueue, &gRumblePakPfs, controller->port);
    s32 success = (err == PFS_ERR_SUCCESS);

    if (!success) {
        sRumblePakMotorState = MOTOR_STOP;
    }

    return success;
}

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

        update_rumble_data_queue();
        update_rumble_pak();

        if (sRumblePakActive) {
            // Disable the rumble pak if there were too many failed start/stop attempts without a success.
            if (sRumblePakErrorCount >= 30) {
                sRumblePakActive = FALSE;
                sRumblePakMotorState = MOTOR_STOP;
                osSyncPrintf("Rumble Pak error: %d\n", sRumblePakError);
            }
        } else if ((gNumVblanks % 60) == 0) { // Check Rumble Pak status about once per second.
            sRumblePakActive = init_and_check_rumble_pak();
            sRumblePakErrorCount = 0;
        }

        if (gRumblePakTimer > 0) {
            gRumblePakTimer--;
        }
    }
}

/**
 * Reinitialize the Rumble Pak and stop the motor.
 */
void cancel_rumble(void) {
    sRumblePakActive = init_and_check_rumble_pak();

    // Stop the rumble pak if it's plugged in.
    if (sRumblePakActive) {
        osMotorStop(&gRumblePakPfs);
        sRumblePakMotorState = MOTOR_STOP;
    }

    // Clear the command queue.
    for (int i = 0; i < ARRAY_COUNT(gRumbleDataQueue); i++) {
        gRumbleDataQueue[i].event = RUMBLE_EVENT_NOMESG;
    }

    // Reset timers.
    gCurrRumbleSettings.timer = 0;
    gCurrRumbleSettings.slip = 0;

    gRumblePakTimer = 0;
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
    union { char asStr[sizeof("VRTC")]; OSMesg asMesg; } VRTC = { .asStr = "VRTC" };

    if (!sRumblePakThreadActive) {
        return;
    }

    osSendMesg(&gRumbleThreadVIMesgQueue, VRTC.asMesg, OS_MESG_NOBLOCK);
}

#endif // ENABLE_RUMBLE
