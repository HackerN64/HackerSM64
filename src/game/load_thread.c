#include <ultra64.h>
#include "sm64.h"
#include "load_thread.h"
#include "memory.h"

u8 loadStatus = 0;
s32 gLoadQueueSize = 0;
struct LoadTaskStruct *loadQueueTable[LOAD_QUEUE_SIZE];
struct MemoryPool *loadHeap;

s32 append_load_segment(s32 segment, u8 *startAddr, u8 *endAddr, s32 side, s32 priority, u8 *startBss, u8 *endBss, s32 compressed)
{
    if (gLoadQueueSize >= LOAD_QUEUE_SIZE)
        return LOAD_ERROR_QUEUE_FULL;
    if ((loadQueueTable[gLoadQueueSize] = mem_pool_alloc(loadHeap, sizeof(struct LoadTaskStruct))) == NULL)
        return LOAD_ERROR_ALLOC_FAILED;

    loadQueueTable[gLoadQueueSize]->segment = segment;
    loadQueueTable[gLoadQueueSize]->startAddr = startAddr;
    loadQueueTable[gLoadQueueSize]->endAddr = endAddr;
    loadQueueTable[gLoadQueueSize]->side = side;
    loadQueueTable[gLoadQueueSize]->priority = priority;
    loadQueueTable[gLoadQueueSize]->startBss = startBss;
    loadQueueTable[gLoadQueueSize]->endBss = endBss;
    if (compressed > 1)
        compressed = 1;
    loadQueueTable[gLoadQueueSize]->type = LOAD_QUEUE_SEGMENT_RAW + compressed;

    gLoadQueueSize++;
    return 0;
}

s32 append_load_dma(u8 *dest, u8 *startAddr, u8 *endAddr, s32 priority, s32 compressed)
{
    if (gLoadQueueSize >= LOAD_QUEUE_SIZE)
        return LOAD_ERROR_QUEUE_FULL;
    if ((loadQueueTable[gLoadQueueSize] = mem_pool_alloc(loadHeap, sizeof(struct LoadTaskStruct))) == NULL)
        return LOAD_ERROR_ALLOC_FAILED;

    loadQueueTable[gLoadQueueSize]->startAddr = startAddr;
    loadQueueTable[gLoadQueueSize]->endAddr = endAddr;
    loadQueueTable[gLoadQueueSize]->priority = priority;
    loadQueueTable[gLoadQueueSize]->startBss = dest;
    if (compressed > 1)
        compressed = 1;
    loadQueueTable[gLoadQueueSize]->type = LOAD_QUEUE_DMA_RAW + compressed;

    gLoadQueueSize++;
    return 0;
}


void process_load_queue(void)
{
    s32 i, j, bestPrio;
    if (gLoadQueueSize == 0)
    {
        loadStatus = 0;
        return;
    }
    loadStatus = 1;
    for (i = 0; i < gLoadQueueSize; i++)
    {
        bestPrio = 0;
        for (j = 0; j < LOAD_QUEUE_SIZE; j++)
        {
            if (loadQueueTable[j] == NULL)
                continue;
            if (loadQueueTable[j]->priority > bestPrio)
                bestPrio = j;
        }
        if (loadQueueTable[bestPrio] == NULL)
            continue;
        switch (loadQueueTable[bestPrio]->type)
        {
        case LOAD_QUEUE_SEGMENT_RAW:
            load_segment(loadQueueTable[bestPrio]->segment, loadQueueTable[bestPrio]->startAddr, loadQueueTable[bestPrio]->endAddr,
            loadQueueTable[bestPrio]->side, loadQueueTable[bestPrio]->startBss, loadQueueTable[bestPrio]->endBss);
            break;
        case LOAD_QUEUE_SEGMENT_COMPRESSED:
            load_segment_decompress(loadQueueTable[bestPrio]->segment, loadQueueTable[bestPrio]->startAddr, loadQueueTable[bestPrio]->endAddr);
            break;
        case LOAD_QUEUE_DMA_RAW:
            dma_read(loadQueueTable[bestPrio]->startBss, loadQueueTable[bestPrio]->startAddr, loadQueueTable[bestPrio]->endAddr);
            break;
        }
        mem_pool_free(loadHeap, loadQueueTable[bestPrio]);
        gLoadQueueSize--;
    }
    loadStatus = 0;
}

void thread7_load(UNUSED void *arg)
{
    #ifdef PUPPYPRINT
    OSTime lastTime = 0;
    #endif
    loadHeap = main_pool_alloc(0x4000, MEMORY_POOL_LEFT);
    while (TRUE)
    {
        #ifdef PUPPYPRINT
        lastTime = osGetTime();
        #endif
        process_load_queue();
        #ifdef PUPPYPRINT
        //profiler_update(loadThreadTime, lastTime);
        #endif
    }
}
