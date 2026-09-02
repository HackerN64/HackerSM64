#ifndef MEMORY_H
#define MEMORY_H

#include <PR/ultratypes.h>

#include "types.h"

#define NUM_TLB_SEGMENTS 32

#ifndef DISABLE_ALL
#define DEBUG_MEMORY_ALLOC
#endif

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

struct MainPoolRegion {
    u8* start;
    u8* end;
};

enum RegionId {
    MAIN_POOL_REGION_MAIN = 0,

    MAIN_POOL_REGIONS_COUNT,
};

struct MainPoolContext {
    struct MainPoolRegion regions[MAIN_POOL_REGIONS_COUNT];
};

extern struct MainPoolContext gMainPool;

#define MAIN_POOL_ALIGNMENT_DISABLE -1
#define MAIN_POOL_ALLOC_TRY TRUE
#define MAIN_POOL_ALLOC_FORCE FALSE

// takes the first 'size' bytes from 'region'
void main_pool_alloc_failed(int region, u32 size, int freeable, int line, const char* file) __attribute__((noreturn));

// main_pool_region_alloc_from_start is a main entry point for all possible allocs in main pool.
#define main_pool_region_alloc_from_start(region_idx, size, alignment, try) _main_pool_region_alloc_from_start(region_idx, size, alignment, try, __LINE__, __FILE__)
static ALWAYS_INLINE void* _main_pool_region_alloc_from_start(int region_idx, u32 size, s32 alignment, int try, int line, const char* file) {
    struct MainPoolRegion* region = &gMainPool.regions[region_idx];
    u8* ret = alignment < 0 ? region->start : (u8*) ALIGN(region->start, alignment);
    u8* new_start = ret + size;

    if (try) {
        if (new_start > region->end)
            return NULL;
    } else {
#ifdef DEBUG_MEMORY_ALLOC
        if (new_start > region->end)
            main_pool_alloc_failed(region_idx, size, FALSE, line, file);
#else
        (void) line; (void) file;
#endif
    }

    region->start = new_start;
    if (!ret) __builtin_unreachable();
    return ret;
}

/*
 Main Pool is a trivial allocator that is managing multiple 'regions' of memory.
 'Region' is a contiguous block of memory available for main pool use.
 It behaves similarly to an array of AllocOnly pools from vanilla SM64 by
 "cutting" the start of the "region" when an allocation is made and returning the
 pointer to the start of the initial "region".

 Here is a simple visual example of how the memory is laid out in the main pool:

 Main pool initial state is a multiple regions of memory:
 |-------|   |----|  |-------------------|
 If alloc(sizeof(+++++)), first region is used and the state becomes:
 |+++++--|   |----|  |-------------------|
  ^
  returned pointer, no extra memory overhead

 The common usecase for multiple regions is using FB/ZB splitting.
 One of the layouts that can be used is have 2 regions: MAIN + COLD with the following layout:

   0x80000400+0x25800                  0x80700000-0x25800    0x80700000+0x25800
           |                                         |             |
           V                                         V             V
      | ZB | buffers+text+data+bss | fb0 | MAIN pool | fb 1 | fb 2 | COLD pool x godddard |
      ^                            ^                        ^                             ^
      |                            |                        |                             |
  0x80000400                  0x802xxxxx               0x80700000                    0x80800000

 Such layouts allows to have ZB in bank 0, FB0 in bank 2, FB1 in bank 6 and FB2 in bank 7.
 You need to adjust 'main_pool_init' to initialize COLD region using 'main_pool_region_init'.
 For cold data add wrapper to invoke 'main_pool_region_alloc_from_start(MAIN_POOL_REGION_COLD, size, alignment, try)'.
 An example of COLD data in SM64 engine are level scripts, warp nodes, unparsed geometry layouts (group segments), main pool states
 */
void main_pool_init(void);

#define main_pool_alloc(size) main_pool_region_alloc_from_start(MAIN_POOL_REGION_MAIN, ALIGN4(size), MAIN_POOL_ALIGNMENT_DISABLE, MAIN_POOL_ALLOC_FORCE)

void *main_pool_alloc_ex(int region, u32 size, u32 alignment);
static inline void *main_pool_alloc_aligned(u32 size, u32 alignment)
{
    if (!alignment)
        alignment = 16;

    void* buf = main_pool_alloc_ex(0, ALIGN4(size), alignment);
    if (!buf) __builtin_unreachable();
    return buf;
}

/*
 Main pool also provides a way to free the latest allocated memory for temporary memory use.
 In vanilla SM64, 'right side' alloc is used for it. This implementation abstracts it to 'main_pool_alloc_freeable'
 that behaves very similarly. Notice that 'main_pool_alloc_freeable' has overhead so
 it is recommended to use it only when necessary. Common usecase is a
 temporary buffer that is allocated, used and freed in the same function.
*/

void *main_pool_alloc_freeable(u32 size);
void *main_pool_alloc_aligned_freeable(u32 size, u32 alignment);
void main_pool_free(void *addr);

/*
 Main pool provides an ability to push/pop the current state of the allocator.
 For example, it is used by SM64 to allocate data bank buffers.
 Common usecase in levelscript is
 1) Push the state using 'main_pool_push_state'
 2) Use 'main_pool_alloc' to allocate data bank buffers
 3) When unnecessary, free all the data bank buffers at once using 'main_pool_pop_state'
 */
void main_pool_push_state(void);
void main_pool_pop_state(void);

/*
 Main pool provides an ability to get the current available memory.
 This is useful for debugging purposes. Please do not attempt to use this
 to predict the memory layout as regions in main pool might not be contiguous.
 */
u32 main_pool_available(void);

#ifndef NO_SEGMENTED_MEMORY
void *load_segment(s32 segment, u8 *srcStart, u8 *srcEnd, u8 *bssStart, u8 *bssEnd);
void *load_to_fixed_pool_addr(u8 *destAddr, u8 *srcStart, u8 *srcEnd);
void *load_segment_decompress(s32 segment, u8 *srcStart, u8 *srcEnd);
void load_engine_code_segment(void);
#else
#define load_segment(...)
#define load_to_fixed_pool_addr(...)
#define load_segment_decompress(...)
#define load_engine_code_segment(...)
#endif

struct MemoryPool *mem_pool_init(u32 size);
void *mem_pool_alloc(struct MemoryPool *pool, u32 size);
void mem_pool_free(struct MemoryPool *pool, void *addr);

void *alloc_display_list(u32 size);
void setup_dma_table_list(struct DmaHandlerList *list, void *srcAddr, void *buffer);
s32 load_patchable_table(struct DmaHandlerList *list, s32 index);

#endif // MEMORY_H
