#ifndef MEMORY_H
#define MEMORY_H

#include <PR/ultratypes.h>

#include "types.h"

enum MemoryPoolSide {
    MEMORY_POOL_LEFT,
    MEMORY_POOL_RIGHT
};

#define NUM_TLB_SEGMENTS 32

struct AllocOnlyPool {
    s32 totalSpace;
    s32 usedSpace;
    u8 *startPtr;
    u8 *freePtr;
};

struct MemoryPool;

struct OffsetSizePair {
    u32 offset;
    u32 size;
};

struct DmaTable {
    u32 count;
    u8 *srcAddr;
    struct OffsetSizePair anim[1]; // dynamic size
};

struct DmaHandlerList {
    struct DmaTable *dmaTable;
    void *currentAddr;
    void *bufTarget;
};

#define EFFECTS_MEMORY_POOL 0x4000

extern struct MemoryPool *gEffectsMemoryPool;

uintptr_t set_segment_base_addr(s32 segment, void *addr);
void *get_segment_base_addr(s32 segment);
void *segmented_to_virtual(const void *addr);
void *virtual_to_segmented(u32 segment, const void *addr);
void move_segment_table_to_dmem(void);

void main_pool_init(void *start, void *end);
void *main_pool_alloc(u32 size, u32 side);
u32 main_pool_free(void *addr);
void *main_pool_realloc(void *addr, u32 size);
u32 main_pool_available(void);
u32 main_pool_push_state(void);
u32 main_pool_pop_state(void);

#ifndef NO_SEGMENTED_MEMORY
void *load_segment(s32 segment, u8 *srcStart, u8 *srcEnd, u32 side, u8 *bssStart, u8 *bssEnd);
void *load_to_fixed_pool_addr(u8 *destAddr, u8 *srcStart, u8 *srcEnd);
void *load_segment_decompress(s32 segment, u8 *srcStart, u8 *srcEnd);
void *load_segment_decompress_heap(u32 segment, u8 *srcStart, u8 *srcEnd);
void load_engine_code_segment(void);
#else
#define load_segment(...)
#define load_to_fixed_pool_addr(...)
#define load_segment_decompress(...)
#define load_segment_decompress_heap(...)
#define load_engine_code_segment(...)
#endif

struct AllocOnlyPool *alloc_only_pool_init(u32 size, u32 side);
void *alloc_only_pool_alloc(struct AllocOnlyPool *pool, s32 size);
struct AllocOnlyPool *alloc_only_pool_resize(struct AllocOnlyPool *pool, u32 size);

struct MemoryPool *mem_pool_init(u32 size, u32 side);
void *mem_pool_alloc(struct MemoryPool *pool, u32 size);
void mem_pool_free(struct MemoryPool *pool, void *addr);

void *alloc_display_list(u32 size);
void setup_dma_table_list(struct DmaHandlerList *list, void *srcAddr, void *buffer);
s32 load_patchable_table(struct DmaHandlerList *list, s32 index);


extern uintptr_t sSegmentROMTable[32];
#endif // MEMORY_H
