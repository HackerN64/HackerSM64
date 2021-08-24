#ifndef LOAD_THREAD_H
#define LOAD_THREAD_H

#define LOAD_QUEUE_SIZE 64

#define LOAD_QUEUE_SEGMENT_RAW 1
#define LOAD_QUEUE_SEGMENT_COMPRESSED 2
#define LOAD_QUEUE_DMA_RAW 3
#define LOAD_QUEUE_DMA_COMPRESSED 4
#define LOAD_ERROR_QUEUE_FULL 1
#define LOAD_ERROR_ALLOC_FAILED 2

struct LoadTaskStruct
{
    u8 type;
    u8 side;
    u8 segment;
    u8 *startAddr;
    u8 *endAddr;
    u8 *startBss;
    u8 *endBss;
    u8 priority;
};

extern void thread7_load(UNUSED void *arg);
extern u8 loadStatus;
extern struct LoadTaskStruct *loadQueueTable[LOAD_QUEUE_SIZE];
extern s32 append_load_dma(u8 *dest, u8 *startAddr, u8 *endAddr, s32 priority, s32 compressed);
extern s32 append_load_segment(s32 segment, u8 *startAddr, u8 *endAddr, s32 side, s32 priority, u8 *startBss, u8 *endBss, s32 compressed);

#endif
