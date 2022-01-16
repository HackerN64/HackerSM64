#include <ultra64.h>
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
u32 prev_start;
u32 start;
u32 prev_time;

static void buffer_update(ProfileTimeData* data, u32 new) {
    u32 old = data->counts[profile_buffer_index];
    data->total -= old;
    data->total += new;
    data->counts[profile_buffer_index] = new;
}

void fast_profiler_update(enum ProfilerTime which) {
    u32 cur_time = osGetCount();
    u32 diff;
    ProfileTimeData* cur_data = &all_profiling_data[which];

    diff = cur_time - prev_time;    
    buffer_update(cur_data, diff);
    prev_time = cur_time;
}

static void update_fps_timer() {
    u32 diff = start - prev_start;

    buffer_update(&all_profiling_data[PROFILER_TIME_FPS], diff);
    prev_start = start;
}

static void update_total_timer() {
    prev_time = start;
    fast_profiler_update(PROFILER_TIME_TOTAL);
}

static void update_rdp_timers() {
    u32 tmem = IO_READ(DPC_TMEM_REG);
    u32 cmd =  IO_READ(DPC_BUFBUSY_REG);
    u32 pipe = IO_READ(DPC_PIPEBUSY_REG);
    
    if (gGlobalTimer > 5) {
        IO_WRITE(DPC_STATUS_REG, (DPC_CLR_CLOCK_CTR | DPC_CLR_CMD_CTR | DPC_CLR_PIPE_CTR | DPC_CLR_TMEM_CTR));
    }

    buffer_update(&all_profiling_data[PROFILER_TIME_TMEM], tmem);
    buffer_update(&all_profiling_data[PROFILER_TIME_CMD], cmd);
    buffer_update(&all_profiling_data[PROFILER_TIME_PIPE], pipe);
}

void fast_profiler_print_times() {
    u32 microseconds[PROFILER_TIME_COUNT];
    char text_buffer[128];

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
        "TERR: %7d\n"
        "OBJ:  %7d\n"
        "GFX:  %7d\n"
        "TOTAL:%7d\n"
        "RDP\n"
        "TMEM: %7d\n"
        "CMD:  %7d\n"
        "PIPE: %7d",
        1000000.0f / microseconds[PROFILER_TIME_FPS],
        microseconds[PROFILER_TIME_CONTROLLERS],
        microseconds[PROFILER_TIME_LEVEL_SCRIPT],
        microseconds[PROFILER_TIME_TERRAIN],
        microseconds[PROFILER_TIME_OBJECTS],
        microseconds[PROFILER_TIME_GFX],
        microseconds[PROFILER_TIME_TOTAL],
        microseconds[PROFILER_TIME_TMEM],
        microseconds[PROFILER_TIME_CMD],
        microseconds[PROFILER_TIME_PIPE]
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

    if (profile_buffer_index >= PROFILING_BUFFER_SIZE) {
        profile_buffer_index = 0;
    }

    prev_time = start = osGetCount();
}