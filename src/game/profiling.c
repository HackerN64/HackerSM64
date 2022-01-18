#include <ultra64.h>
#include <PR/os_internal_reg.h>
#include "game_init.h"

#include "profiling.h"
#include "fasttext.h"

#define RDP_CYCLE_CONV(x) ((10 * (x)) / 625) // 62.5 million cycles per frame

typedef struct {
    u32 counts[PROFILING_BUFFER_SIZE];
    u32 total;
} ProfileTimeData;

ProfileTimeData all_profiling_data[PROFILER_TIME_COUNT];

int profile_buffer_index = -1;
int rsp_buffer_indices[PROFILER_RSP_COUNT];
// Holds either the start time if the task is running, or the amount of time the task has run for so far if yielded
u32 rsp_pending_times[PROFILER_RSP_COUNT];
u32 prev_start;
u32 start;
u32 prev_time;
u32 audio_start;
u32 audio_buffer_index;
u32 preempted_time;

static void buffer_update(ProfileTimeData* data, u32 new, int buffer_index) {
    u32 old = data->counts[buffer_index];
    data->total -= old;
    data->total += new;
    data->counts[buffer_index] = new;
}

void fast_profiler_update(enum ProfilerTime which) {
    u32 cur_time = osGetCount();
    u32 diff;
    ProfileTimeData* cur_data = &all_profiling_data[which];

    diff = cur_time - prev_time;

    u32 saved = __osDisableInt();
    u32 cur_preempted_time = preempted_time;
    preempted_time = 0;
    __osRestoreInt(saved);
    if (cur_preempted_time > 0) {
        diff -= cur_preempted_time;
        start += cur_preempted_time;
    }
    
    buffer_update(cur_data, diff, profile_buffer_index);
    prev_time = cur_time;
}

void fast_profiler_rsp_started(enum ProfilerRSPTime which) {
    rsp_pending_times[which] = osGetCount();
}

void fast_profiler_rsp_completed(enum ProfilerRSPTime which) {
    ProfileTimeData* cur_data = &all_profiling_data[PROFILER_TIME_RSP_GFX + which];
    int cur_index = rsp_buffer_indices[which];
    u32 time = osGetCount() - rsp_pending_times[which];
    rsp_pending_times[which] = 0;

    buffer_update(cur_data, time, cur_index);
    cur_index++;
    if (cur_index >= PROFILING_BUFFER_SIZE) {
        cur_index = 0;
    }
    rsp_buffer_indices[which] = cur_index;
}

void fast_profiler_rsp_resumed() {
    rsp_pending_times[PROFILER_RSP_GFX] = osGetCount() - rsp_pending_times[PROFILER_RSP_GFX];
}

// This ends up being the same math as resumed, so we just use resumed for both
// void fast_profiler_rsp_yielded() {
//     rsp_pending_times[PROFILER_RSP_GFX] = osGetCount() - rsp_pending_times[PROFILER_RSP_GFX];
// }

void fast_profiler_audio_started() {
    audio_start = osGetCount();
}

void fast_profiler_audio_completed() {
    ProfileTimeData* cur_data = &all_profiling_data[PROFILER_TIME_AUDIO];
    u32 time = osGetCount() - audio_start;
    u32 cur_index = audio_buffer_index;

    preempted_time = time;
    buffer_update(cur_data, time, cur_index);
    cur_index++;
    if (cur_index >= PROFILING_BUFFER_SIZE) {
        cur_index = 0;
    }

    audio_buffer_index = cur_index;
}

static void update_fps_timer() {
    u32 diff = start - prev_start;

    buffer_update(&all_profiling_data[PROFILER_TIME_FPS], diff, profile_buffer_index);
    prev_start = start;
}

static void update_total_timer() {
    u32 saved = __osDisableInt();
    u32 cur_preempted_time = preempted_time;
    preempted_time = 0;
    __osRestoreInt(saved);

    prev_time = start + cur_preempted_time;
    fast_profiler_update(PROFILER_TIME_TOTAL);
}

static void update_rdp_timers() {
    u32 tmem = IO_READ(DPC_TMEM_REG);
    u32 cmd =  IO_READ(DPC_BUFBUSY_REG);
    u32 pipe = IO_READ(DPC_PIPEBUSY_REG);
    
    if (gGlobalTimer > 5) {
        IO_WRITE(DPC_STATUS_REG, (DPC_CLR_CLOCK_CTR | DPC_CLR_CMD_CTR | DPC_CLR_PIPE_CTR | DPC_CLR_TMEM_CTR));
    }

    buffer_update(&all_profiling_data[PROFILER_TIME_TMEM], tmem, profile_buffer_index);
    buffer_update(&all_profiling_data[PROFILER_TIME_CMD], cmd, profile_buffer_index);
    buffer_update(&all_profiling_data[PROFILER_TIME_PIPE], pipe, profile_buffer_index);
}

void fast_profiler_print_times() {
    u32 microseconds[PROFILER_TIME_COUNT];
    char text_buffer[196];

    update_fps_timer();
    update_total_timer();
    update_rdp_timers();

    for (int i = 0; i < PROFILER_TIME_COUNT; i++) {
        if (i < PROFILER_TIME_TMEM) {
            microseconds[i] = OS_CYCLES_TO_USEC(all_profiling_data[i].total / PROFILING_BUFFER_SIZE);
        } else {
            microseconds[i] = RDP_CYCLE_CONV(all_profiling_data[i].total / PROFILING_BUFFER_SIZE);
        }
    }

    sprintf(text_buffer,
        "FPS:    %5.2f\n"
        "CPU\n"
        "CONT: %7d\n"
        "LEVEL:%7d\n"
        "OBJ1: %7d\n"
        "OBJ2: %7d\n"
        "GFX:  %7d\n"
        "AUDIO:%7d\n"
        "TOTAL:%7d\n"
        "RDP\n"
        "TMEM: %7d\n"
        "CMD:  %7d\n"
        "PIPE: %7d\n"
        "RSP\n"
        "GFX:  %7d\n"
        "AUDIO:%7d\n"
        "TOTAL:%7d",
        1000000.0f / microseconds[PROFILER_TIME_FPS],
        microseconds[PROFILER_TIME_CONTROLLERS],
        microseconds[PROFILER_TIME_LEVEL_SCRIPT],
        microseconds[PROFILER_TIME_OBJECTS1],
        microseconds[PROFILER_TIME_OBJECTS2],
        microseconds[PROFILER_TIME_GFX],
        microseconds[PROFILER_TIME_AUDIO] * 2, // audio is 60Hz, so double the average
        microseconds[PROFILER_TIME_TOTAL] + microseconds[PROFILER_TIME_AUDIO] * 2, // audio time is removed from the main thread profiling, so add it back here
        microseconds[PROFILER_TIME_TMEM],
        microseconds[PROFILER_TIME_CMD],
        microseconds[PROFILER_TIME_PIPE],
        microseconds[PROFILER_TIME_RSP_GFX],
        microseconds[PROFILER_TIME_RSP_AUDIO],
        microseconds[PROFILER_TIME_RSP_GFX] + microseconds[PROFILER_TIME_RSP_AUDIO]
    );

    Gfx* dlHead = gDisplayListHead;
    gDPPipeSync(dlHead++);
    gDPSetCycleType(dlHead++, G_CYC_1CYCLE);
    gDPSetRenderMode(dlHead++, G_RM_TEX_EDGE, G_RM_TEX_EDGE2);
    gDPSetTexturePersp(dlHead++, G_TP_NONE);
    gDPSetTextureFilter(dlHead++, G_TF_POINT);
    gDPSetTextureLUT(dlHead++, G_TT_NONE);
    drawSmallStringCol(&dlHead, 10, 10, text_buffer, 0, 0, 0);
    gDisplayListHead = dlHead;
}

void fast_profiler_frame_setup() {
    profile_buffer_index++;
    preempted_time = 0;

    if (profile_buffer_index >= PROFILING_BUFFER_SIZE) {
        profile_buffer_index = 0;
    }

    prev_time = start = osGetCount();
}