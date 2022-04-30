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
    PROFILER_TIME_AUDIO,
    PROFILER_TIME_TOTAL,
    PROFILER_TIME_RSP_GFX,
    PROFILER_TIME_RSP_AUDIO,
    PROFILER_TIME_TMEM,
    PROFILER_TIME_PIPE,
    PROFILER_TIME_CMD,
    PROFILER_TIME_COUNT,
};

enum ProfilerRSPTime {
    PROFILER_RSP_GFX,
    PROFILER_RSP_AUDIO,
    PROFILER_RSP_COUNT
};

#ifdef USE_PROFILER
void profiler_update(enum ProfilerTime which);
void profiler_print_times();
void profiler_frame_setup();
void profiler_rsp_started(enum ProfilerRSPTime which);
void profiler_rsp_completed(enum ProfilerRSPTime which);
void profiler_rsp_resumed();
void profiler_audio_started();
void profiler_audio_completed();
// See profiling.c to see why profiler_rsp_yielded isn't its own function
static ALWAYS_INLINE void profiler_rsp_yielded() {
    profiler_rsp_resumed();
}
#else
#define profiler_update(which)
#define profiler_print_times()
#define profiler_frame_setup()
#define profiler_rsp_started(which)
#define profiler_rsp_completed(which)
#define profiler_rsp_resumed()
#define profiler_audio_started()
#define profiler_audio_completed()
#define profiler_rsp_yielded()
#endif

#endif
