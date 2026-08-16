#ifndef RUMBLE_INIT_H
#define RUMBLE_INIT_H

#include <PR/ultratypes.h>

#include "config.h"

#if ENABLE_RUMBLE

extern s32 gRumblePakTimer;

void init_rumble_pak_scheduler_queue(void);
void block_until_rumble_pak_free(void);
void release_rumble_pak_control(void);
void queue_rumble_data(s16 time, s16 level);
void queue_rumble_decay(s16 decay);
u32  is_rumble_finished_and_queue_empty(void);
void reset_rumble_timers_slip(void);
void reset_rumble_timers_vibrate(s32 level);
void queue_rumble_submerged(void);
void cancel_rumble(void);
void create_thread_6(void);
void rumble_thread_update_vi(void);

#else

#define init_rumble_pak_scheduler_queue()
#define block_until_rumble_pak_free()
#define release_rumble_pak_control()
#define queue_rumble_data(time, level)
#define queue_rumble_decay(decay)
#define is_rumble_finished_and_queue_empty() FALSE
#define reset_rumble_timers_slip()
#define reset_rumble_timers_vibrate(level)
#define queue_rumble_submerged()
#define cancel_rumble()
#define create_thread_6()
#define rumble_thread_update_vi()

#endif // ENABLE_RUMBLE

#endif // RUMBLE_INIT_H
