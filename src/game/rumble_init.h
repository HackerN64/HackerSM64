#ifndef RUMBLE_INIT_H
#define RUMBLE_INIT_H

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
    RUMBLE_EVENT_NOMESG,
    RUMBLE_EVENT_CONSTON,
    RUMBLE_EVENT_LEVELON,
};

struct RumbleData {
    s16 event;
    s16 level;
    s16 timer;
    s16 decay;
};

struct RumbleSettings {
    s16 event;
    s16 level;
    s16 timer;
    s16 count;
    s16 start;
    s16 slip;
    s16 vibrate;
    s16 decay;
};

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
void queue_rumble_data(struct Controller *controller, s16 timer, s16 level, s16 decay);
u32 is_rumble_finished_and_queue_empty(struct Controller *controller);
void reset_rumble_timers_slip(struct Controller *controller);
void reset_rumble_timers_vibrate(struct Controller *controller, s32 level);
void queue_rumble_submerged(struct Controller *controller);
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

#endif // RUMBLE_INIT_H
