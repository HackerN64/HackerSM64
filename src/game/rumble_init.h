#ifndef RUMBLE_INIT_H
#define RUMBLE_INIT_H

#include <PR/ultratypes.h>

#include "config.h"

#ifdef ENABLE_RUMBLE

// Rumble command.
struct RumbleData {
    /*0x00*/ s16 event;     // The type of rumble command. see RumbleEvents enum.
    /*0x02*/ s16 level;     // Used to modulate rumble when 'event' is RUMBLE_EVENT_LEVELON.
    /*0x04*/ s16 timer;     // How many frames the main portion of the rumble lasts.
    /*0x06*/ s16 decay;     // How much 'level' decreases each frame during the 'timer' phase.
}; /*0x08*/

// Current rumble data.
struct RumbleSettings {
    /*0x00*/ s16 event;     // The type of rumble command. see RumbleEvents enum.
    /*0x02*/ s16 level;     // Used to modulate rumble when 'event' is RUMBLE_EVENT_LEVELON.
    /*0x04*/ s16 timer;     // How many frames the main portion of the rumble lasts.
    /*0x06*/ s16 count;     // Used to modulate rumble when 'event' is RUMBLE_EVENT_LEVELON.
    /*0x08*/ s16 start;     // The time to initially rumble for before the 'timer' phase.
    /*0x0A*/ s16 slip;      // A second timer independent from 'timer', for after 'timer' runs out. Decrements regardless.
    /*0x0C*/ s16 vibrate;   // How often to rumble when 'timer' is 0 and 'slip' is between [2, 5).
    /*0x0E*/ s16 decay;     // How much 'level' decreases each frame during the 'timer' phase.
}; /*0x10*/

enum RumbleEvents {
    RUMBLE_EVENT_NOMESG,  // No command.
    RUMBLE_EVENT_CONSTON, // Constant rumble strength.
    RUMBLE_EVENT_LEVELON, // Modulate rumble using 'count' and 'level'.
};

// Number of frames to rumble before entering the 'timer' phase of a rumble.
#define RUMBLE_START_TIME 4

// Number of rumble commands that can be called per frane.
#define RUMBLE_DATA_QUEUE_SIZE 3

extern OSThread gRumblePakThread;

extern OSPfs gRumblePakPfs;

extern OSMesg gRumblePakSchedulerMesgBuf[1];
extern OSMesgQueue gRumblePakSchedulerMesgQueue;
extern OSMesg gRumbleThreadVIMesgBuf[1];
extern OSMesgQueue gRumbleThreadVIMesgQueue;

extern struct RumbleData gRumbleDataQueue[RUMBLE_DATA_QUEUE_SIZE];
extern struct RumbleSettings gCurrRumbleSettings;

extern s32 gRumblePakTimer;

void block_until_rumble_pak_free(void);
void release_rumble_pak_control(void);
void queue_rumble_data(s16 timer, s16 level);
void queue_rumble_decay(s16 decay);
u32 is_rumble_finished_and_queue_empty(void);
void reset_rumble_timers_slip(void);
void reset_rumble_timers_vibrate(s32 level);
void queue_rumble_submerged(void);
void cancel_rumble(void);
void create_thread_6_rumble(void);
void rumble_thread_update_vi(void);

#endif // ENABLE_RUMBLE

#endif // RUMBLE_INIT_H
