#ifndef RUMBLE_INIT_H
#define RUMBLE_INIT_H

#include <PR/ultratypes.h>

#include "config.h"

#ifdef ENABLE_RUMBLE

struct RumbleData {
    /*0x00*/ u8  comm;
    /*0x01*/ u8  level;
    /*0x02*/ s16 time;
    /*0x04*/ s16 decay;
}; /*0x06*/

struct RumbleSettings {
    /*0x00*/ s16 event;
    /*0x02*/ s16 level;
    /*0x04*/ s16 timer;
    /*0x06*/ s16 count;
    /*0x08*/ s16 start;
    /*0x0A*/ s16 slip;
    /*0x0C*/ s16 vibrate;
    /*0x0E*/ s16 decay;
}; /*0x10*/

extern OSThread gRumblePakThread;

extern OSPfs gRumblePakPfs;

extern OSMesg gRumblePakSchedulerMesgBuf[1];
extern OSMesgQueue gRumblePakSchedulerMesgQueue;
extern OSMesg gRumbleThreadVIMesgBuf[1];
extern OSMesgQueue gRumbleThreadVIMesgQueue;

#define RUMBLE_DATA_QUEUE_SIZE 3

extern struct RumbleData gRumbleDataQueue[RUMBLE_DATA_QUEUE_SIZE];
extern struct RumbleSettings gCurrRumbleSettings;

extern s32 gRumblePakTimer;

void init_rumble_pak_scheduler_queue(void);
void block_until_rumble_pak_free(void);
void release_rumble_pak_control(void);
void queue_rumble_data(s16 time, s16 level);
void queue_rumble_decay(s16 decay);
u32 is_rumble_finished_and_queue_empty(void);
void reset_rumble_timers_slip(void);
void reset_rumble_timers_vibrate(s32 level);
void queue_rumble_submerged(void);
void cancel_rumble(void);
void create_thread_6(void);
void rumble_thread_update_vi(void);

#endif // ENABLE_RUMBLE

#endif // RUMBLE_INIT_H
