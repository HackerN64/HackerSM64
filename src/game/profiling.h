#ifndef __PROFILING_H__
#define __PROFILING_H__

#include <ultra64.h>
#include "macros.h"
#include "config/config_debug.h"
#include "config/config_safeguards.h"

#define PROFILING_BUFFER_SIZE 64

enum ProfilerTime {
    PROFILER_TIME_FPS,
    PROFILER_TIME_CONTROLLERS,
    PROFILER_TIME_SPAWNER,
    PROFILER_TIME_DYNAMIC,
    PROFILER_TIME_BEHAVIOR_BEFORE_MARIO,
    PROFILER_TIME_MARIO,
    PROFILER_TIME_BEHAVIOR_AFTER_MARIO,
    PROFILER_TIME_GFX,
    PROFILER_TIME_COLLISION,
    PROFILER_TIME_CAMERA,
#ifdef PUPPYPRINT_DEBUG
    PROFILER_TIME_PUPPYPRINT1,
    PROFILER_TIME_PUPPYPRINT2,
#endif
    PROFILER_TIME_AUDIO,
    PROFILER_TIME_TOTAL,
    PROFILER_TIME_RSP_GFX,
    PROFILER_TIME_RSP_AUDIO,
    PROFILER_TIME_TMEM,
    PROFILER_TIME_PIPE,
    PROFILER_TIME_CMD,
    PROFILER_TIME_COUNT,
};

#ifndef PUPPYPRINT_DEBUG
    #define PROFILER_TIME_PUPPYPRINT1 0
    #define PROFILER_TIME_PUPPYPRINT2 0
    #define PROFILER_DELTA_PUPPYPRINT1 0
    #define PROFILER_DELTA_PUPPYPRINT2 0
#endif

enum ProfilerRSPTime {
    PROFILER_RSP_GFX,
    PROFILER_RSP_AUDIO,
    PROFILER_RSP_COUNT
};

enum ProfilerDeltaTime {
    PROFILER_DELTA_COLLISION,
#ifdef PUPPYPRINT_DEBUG
    PROFILER_DELTA_PUPPYPRINT1,
    PROFILER_DELTA_PUPPYPRINT2
#endif
};

#ifdef USE_PROFILER
void profiler_update(enum ProfilerTime which, u32 delta);
void profiler_print_times();
void profiler_frame_setup();
void profiler_rsp_started(enum ProfilerRSPTime which);
void profiler_rsp_completed(enum ProfilerRSPTime which);
void profiler_rsp_resumed();
void profiler_audio_started();
void profiler_audio_completed();
void profiler_collision_reset();
void profiler_collision_completed();
void profiler_collision_update(u32 time);
u32 profiler_get_delta(enum ProfilerDeltaTime which);
u32 profiler_get_cpu_microseconds();
u32 profiler_get_rsp_microseconds();
u32 profiler_get_rdp_microseconds();
// See profiling.c to see why profiler_rsp_yielded isn't its own function
static ALWAYS_INLINE void profiler_rsp_yielded() {
    profiler_rsp_resumed();
}
#else
#define profiler_update(which, delta)
#define profiler_print_times()
#define profiler_frame_setup()
#define profiler_rsp_started(which)
#define profiler_rsp_completed(which)
#define profiler_rsp_resumed()
#define profiler_audio_started()
#define profiler_audio_completed()
#define profiler_rsp_yielded()
#define profiler_collision_reset();
#define profiler_collision_completed();
#define profiler_collision_update(time);
#define profiler_update_delta(which, time);
#define profiler_get_delta(which) 0
#endif

#endif
