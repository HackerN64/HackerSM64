#ifndef MEMORY_H
#define MEMORY_H

#include <PR/ultratypes.h>

#include "memory_layout.h"
#include "types.h"

#define NUM_TLB_SEGMENTS 32

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

#ifndef MAIN_POOL_SINGLE_REGION
extern struct MainPoolRegion* gMainPoolCurrentRegion;
#else
extern struct MainPoolContext sMainPool;
// There is only 1 region which is the first region
#define gMainPoolCurrentRegion ((struct MainPoolRegion*) &sMainPool)
#endif

// takes the first 'size' bytes from 'region'
static inline void* main_pool_region_try_alloc_from_start(struct MainPoolRegion* region, u32 size) {
    u8* buf = region->start;
    u8* newStart = buf + size;
#ifndef MAIN_POOL_SINGLE_REGION
    if (__builtin_expect(newStart > region->end, 0))
        return NULL;
#endif

    region->start = newStart;
    return buf;
}

/*
 Main Pool is a trivial allocator that is managing multiple 'regions' of memory.
 'Region' is a contiguous block of memory available for main pool use.
 For example, whe MEMORY_FRAGMENTATION_LEVEL 10 is used, there are 2 'regions':
 from engine end to zbuffer start and after framebuffer end to RAM end.
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
 If afterwards alloc(sizeof(+++)) is used, it does not fit in region 1, so region 2 is used:
 |+++++--|   |+++-|  |-------------------|
              ^
  returned pointer
 */
void main_pool_init(void);

/*
 When 'main_pool_alloc_slow' is used, regions are iterated till a region is found that
 can supply the necessary memory. Compared to vanilla SM64 main pool allocator,
 there is no extra cost in using 'main_pool_alloc' - it is has 0 bytes overhead.
 The only way to free memory returned by 'alloc' is to use 'main_pool_pop_state'.
 */
void *main_pool_alloc_slow(u32 size);

/*
 'main_pool_alloc' is a faster version of 'main_pool_alloc_slow' that can be inlined for small allocations.
 Its fast path for small size basically looks like "(return *ptr += size)" making it
 very quick for common tiny allocs used, for example, in surface code.
 */
static inline void *main_pool_alloc(u32 size) {
#ifndef MAIN_POOL_SINGLE_REGION
    size = ALIGN4(size);
    if (size < MAIN_POOL_SMALL_ALLOC_LIMIT) {
        void *buf = main_pool_region_try_alloc_from_start(gMainPoolCurrentRegion, size);
        if (__builtin_expect(!!buf, 1))
            return buf;
    }

    return main_pool_alloc_slow(size);
#else
    return main_pool_region_try_alloc_from_start(gMainPoolCurrentRegion, size);
#endif
}
void *main_pool_alloc_aligned(u32 size, u32 alignment);


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
