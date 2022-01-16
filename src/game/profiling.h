#ifndef __PROFILING_H__
#define __PROFILING_H__

#define PROFILING_BUFFER_SIZE 64

enum ProfilerTime {
    PROFILER_TIME_FPS,
    PROFILER_TIME_CONTROLLERS,
    PROFILER_TIME_LEVEL_SCRIPT,
    PROFILER_TIME_TERRAIN,
    PROFILER_TIME_OBJECTS,
    PROFILER_TIME_GFX,
    PROFILER_TIME_TOTAL,
    PROFILER_TIME_TMEM,
    PROFILER_TIME_PIPE,
    PROFILER_TIME_CMD,
    PROFILER_TIME_COUNT,
};

void fast_profiler_update(enum ProfilerTime which);
void fast_profiler_print_times();
void fast_profiler_frame_setup();

#endif
