#pragma once

#include <PR/ultratypes.h>

#include "config.h"

#ifdef ENABLE_RUMBLE

// Number of vblanks between each rumble pak check.
#define RUMBLE_PAK_CHECK_TIME   60
// Number of errors before the rumble pak is considered disconnected.
#define RUMBLE_MAX_ERRORS       30
// Number of frames to rumble before entering the 'timer' phase of a rumble.
#define RUMBLE_START_TIME       4
// Number of rumble commands that can be called per frane.
#define RUMBLE_QUEUE_SIZE       3

enum RumbleEvents {
    RUMBLE_EVENT_NOMESG,  // No command.
    RUMBLE_EVENT_CONSTON, // Constant rumble strength.
    RUMBLE_EVENT_LEVELON, // Modulate rumble using 'count' and 'level'.
};

struct RumbleData {
    /*0x00*/ s16 event; // The type of rumble command. see RumbleEvents enum.
    /*0x02*/ s16 level; // Used to modulate rumble when 'event' is RUMBLE_EVENT_LEVELON.
    /*0x04*/ s16 timer; // How many frames the main portion of the rumble lasts.
    /*0x06*/ s16 decay; // How much 'level' decreases each frame during the 'timer' phase.
}; /*0x08*/

struct RumbleSettings {
    /*0x00*/ s16 event;     // The type of rumble command. see RumbleEvents enum.
    /*0x02*/ s16 level;     // Used to modulate rumble when 'event' is RUMBLE_EVENT_LEVELON.
    /*0x04*/ s16 timer;     // How many frames the main portion of the rumble lasts.
    /*0x06*/ s16 decay;     // How much 'level' decreases each frame during the 'timer' phase.
    /*0x08*/ s16 count;     // Used to modulate rumble when 'event' is RUMBLE_EVENT_LEVELON.
    /*0x0A*/ s16 start;     // The time to initially rumble for before the 'timer' phase.
    /*0x0C*/ s16 slip;      // A second timer independent from 'timer', for after 'timer' runs out. Decrements regardless.
    /*0x0E*/ s16 vibrate;   // How often to rumble when 'timer' is 0 and 'slip' is between [2, 5).
}; /*0x10*/

extern OSThread gRumblePakThread;

extern OSPfs gRumblePakPfs;

extern OSMesg gRumblePakSchedulerMesgBuf[1];
extern OSMesgQueue gRumblePakSchedulerMesgQueue;
extern OSMesg gRumbleThreadVIMesgBuf[1];
extern OSMesgQueue gRumbleThreadVIMesgQueue;

extern struct RumbleData gRumbleDataQueue[RUMBLE_QUEUE_SIZE];
extern struct RumbleSettings gCurrRumbleSettings;

extern s32 gRumblePakTimer;

void block_until_rumble_pak_free(void);
void release_rumble_pak_control(void);
void queue_rumble_data(struct Controller* controller, s16 timer, s16 level, s16 decay);
_Bool is_rumble_finished_and_queue_empty(struct Controller* controller);
void reset_rumble_timers_slip(struct Controller* controller);
void reset_rumble_timers_vibrate(struct Controller* controller, s32 level);
void queue_rumble_submerged(struct Controller* controller);
void cancel_rumble(void);
void create_thread_6_rumble(void);
void rumble_thread_update_vi(void);

#else // !ENABLE_RUMBLE

#define block_until_rumble_pak_free()
#define release_rumble_pak_control()
#define queue_rumble_data(controller, timer, level, decay)
#define is_rumble_finished_and_queue_empty(controller)
#define reset_rumble_timers_slip(controller)
#define reset_rumble_timers_vibrate(controller, level)
#define queue_rumble_submerged(controller)
#define cancel_rumble()
#define create_thread_6_rumble()
#define rumble_thread_update_vi()

#endif // !ENABLE_RUMBLE
