#include <PR/ultratypes.h>

#include "sm64.h"

#include "buffers/buffers.h"
#include "slidec.h"
#include "game/game_init.h"
#include "game/main.h"
#include "game/memory.h"
#include "segment_symbols.h"
#include "segments.h"
#ifdef GZIP
#include <gzip.h>
#endif
#if defined(RNC1) || defined(RNC2)
#include <rnc.h>
#endif
#ifdef UNF
#include "usb/usb.h"
#include "usb/debug.h"
#endif
#include "game/puppyprint.h"

// Uses alternative algorithm for allocating from main pool. Instead of first fit, it uses best fit
// It is not recommended to use because it tends to choose slower memory compared to the default order
// which is ordered from fastest to slowest memory.
// On the other hand, in theory it is possible to fill memory better with this algorithm, in reality it does not matter 
// #define MAIN_POOL_USE_BEST_FIT

#ifdef MAIN_POOL_USE_BEST_FIT
// For developing purposes, check if the size of the region is corrupted
// #define MAIN_POOL_DEBUG_CORRUPTION_SIZE

#ifdef MAIN_POOL_DEBUG_CORRUPTION_SIZE
#define MAIN_POOL_CORRUPTED() main_pool_corrupted(__LINE__, __func__)

__attribute__ ((noinline)) static void main_pool_corrupted(int line, const char* fn) {
    (void) line; (void) fn;
#ifdef CRASH_SCREEN_INCLUDED
    do { if (!(exp)) _n64_assert(fn, line, "Main Pool is corrupted", 1); } while (0)
#else
    while (1) {}
#endif
}

#define MAIN_POOL_DEBUG_CORRUPTION_SIZE_CHECK(reg) do{ if ((reg)->size > 0x800000) MAIN_POOL_CORRUPTED(); } while(0)
#else
#define MAIN_POOL_DEBUG_CORRUPTION_SIZE_CHECK(reg)
#endif
#endif

#define	DOWN(s, align)	(((u32)(s)) & ~((align)-1))
#define DOWN4(s) DOWN(s, 4)

struct MainPoolRegion {
    u8* start;
    u32 size;
};

#ifdef USE_EXT_RAM
#define MAIN_POOL_REGIONS_COUNT 3
#else
#define MAIN_POOL_REGIONS_COUNT 2
#endif

struct MainPoolContext {
    struct MainPoolRegion regions[MAIN_POOL_REGIONS_COUNT];
    u32 freeSpace;
#ifdef MAIN_POOL_USE_BEST_FIT
    u8 regionIds[MAIN_POOL_REGIONS_COUNT];
#endif
};

struct MainPoolState {
    struct MainPoolContext ctx;
    struct MainPoolState* prev;
};

// Multichar constants AL and AR
#define MAIN_POOL_FREEABLE_HEADER_MAGIC_LEFT 0x414c
#define MAIN_POOL_FREEABLE_HEADER_MAGIC_RIGHT 0x4152

struct MainPoolFreeableHeader {
    u16 magic;
    u16 id;
    // either a pointer to 'start' or 'end'
    u8* ptr;
    u8 data[0];
};

struct MemoryBlock {
    struct MemoryBlock *next;
    u32 size;
};

struct MemoryPool {
    u32 totalSpace;
    struct MemoryBlock *firstBlock;
    struct MemoryBlock freeList;
};

extern uintptr_t sSegmentTable[32];


/**
 * Memory pool for small graphical effects that aren't connected to Objects.
 * Used for colored text, paintings, and environmental snow and bubbles.
 */
struct MemoryPool *gEffectsMemoryPool;


uintptr_t sSegmentTable[32];
static struct MainPoolContext sMainPool;

static struct MainPoolState *gMainPoolState = NULL;

uintptr_t set_segment_base_addr(s32 segment, void *addr) {
    sSegmentTable[segment] = ((uintptr_t) addr & 0x1FFFFFFF);
    return sSegmentTable[segment];
}

UNUSED void *get_segment_base_addr(s32 segment) {
    return (void *) (sSegmentTable[segment] | 0x80000000);
}

#ifndef NO_SEGMENTED_MEMORY
void *segmented_to_virtual(const void *addr) {
    size_t segment = ((uintptr_t) addr >> 24);
    size_t offset  = ((uintptr_t) addr & 0x00FFFFFF);

    return (void *) ((sSegmentTable[segment] + offset) | 0x80000000);
}

void *virtual_to_segmented(u32 segment, const void *addr) {
    size_t offset = ((uintptr_t) addr & 0x1FFFFFFF) - sSegmentTable[segment];

    return (void *) ((segment << 24) + offset);
}

void move_segment_table_to_dmem(void) {
    s32 i;

    for (i = 0; i < 16; i++) {
        gSPSegment(gDisplayListHead++, i, sSegmentTable[i]);
    }
}
#else
void *segmented_to_virtual(const void *addr) {
    return (void *) addr;
}

void *virtual_to_segmented(u32 segment, const void *addr) {
    return (void *) addr;
}

void move_segment_table_to_dmem(void) {
}
#endif

static void main_pool_calculate_size(void) {
    sMainPool.freeSpace = 0;
    for (int i = 0; i < MAIN_POOL_REGIONS_COUNT; i++) {
        sMainPool.freeSpace += sMainPool.regions[i].size;
    }
}

#ifdef MAIN_POOL_USE_BEST_FIT
static void main_pool_region_swap(struct MainPoolRegion* r1, struct MainPoolRegion* r2) {
    struct MainPoolRegion tmp = *r1;
    *r1 = *r2;
    *r2 = tmp;
}

static void u8_swap(u8* r1, u8* r2) {
    u8 tmp = *r1;
    *r1 = *r2;
    *r2 = tmp;
}

// sort the 'sMainPool.regions' by size - from lowest size to biggest
#define COMPARE(l, r) do{ \
    if (sMainPool.regions[l].size > sMainPool.regions[r].size) { \
        main_pool_region_swap(&sMainPool.regions[l], &sMainPool.regions[r]); \
        u8_swap(&sMainPool.regionIds[l], &sMainPool.regionIds[r]); \
    } \
} while(0)

#define COMPARE_BREAK(l, r) do{ \
    if (sMainPool.regions[l].size > sMainPool.regions[r].size) { \
        main_pool_region_swap(&sMainPool.regions[l], &sMainPool.regions[r]); \
        u8_swap(&sMainPool.regionIds[l], &sMainPool.regionIds[r]); \
    } else { \
        break; \
    } \
} while(0)

static void main_pool_sort() {
    COMPARE(0, 1);
    COMPARE(1, 2);
    COMPARE(0, 1);
}

static void main_pool_sort_down(int from) {
    // If 'from' region was changed, we just need to bubble sort it to the bottom
    // Whenever the first time condition for 'COMPARE' fails, break away as all other conditions will fail
    for (int i = from; i > 0; i--)
        COMPARE_BREAK(i - 1, i);
}

static void main_pool_sort_up(int from) {
    for (int i = from + 1; i < MAIN_POOL_REGIONS_COUNT; i++)
        COMPARE_BREAK(i - 1, i);
}
#undef COMPARE
#undef COMPARE_BREAK
#endif

extern u8 _framebuffersSegmentBssStart[];
// extern u8 _framebuffersSegmentBssEnd[];
// extern u8 _zbufferSegmentBssStart[];
extern u8 _zbufferSegmentBssEnd[];
extern u8 _goddardSegmentStart[];

/**
 * Initialize the main memory pool. This pool is conceptually regions
 * that grow inward from the left and right. It therefore only supports
 * freeing the object that was most recently allocated from a side.
 */
void main_pool_init() {
#ifdef USE_EXT_RAM
    sMainPool.regions[0].start = (u8 *) 0x80600000;
    sMainPool.regions[0].size = (u8 *) DOWN4(_goddardSegmentStart) - sMainPool.regions[0].start;
    sMainPool.regions[1].start = (u8 *) ALIGN4((uintptr_t)_engineSegmentBssEnd);
    sMainPool.regions[1].size = (u8 *) DOWN4((uintptr_t)_framebuffersSegmentBssStart) - sMainPool.regions[1].start;
    sMainPool.regions[2].start = (u8 *) ALIGN4((uintptr_t)_zbufferSegmentBssEnd);
    sMainPool.regions[2].size = (u8*) 0x80600000 - sMainPool.regions[2].start;
#else
    sMainPool.regions[0].start = (u8 *) ALIGN4((uintptr_t)_zbufferSegmentBssEnd);
    sMainPool.regions[0].size = (u8*) DOWN4(_goddardSegmentStart) - sMainPool.regions[0].start;
    sMainPool.regions[1].start = (u8 *) ALIGN4((uintptr_t)_engineSegmentBssEnd);
    sMainPool.regions[1].size = (u8 *) DOWN4((uintptr_t)_framebuffersSegmentBssStart) - sMainPool.regions[1].start;
#endif

#ifdef MAIN_POOL_USE_BEST_FIT
    sMainPool.regionIds[1] = 1;
    sMainPool.regionIds[0] = 0;
#ifdef USE_EXT_RAM
    sMainPool.regionIds[2] = 2;
#endif
    main_pool_sort();
#endif

    main_pool_calculate_size();

#ifdef PUPPYPRINT_DEBUG
    mempool = sMainPool.freeSpace;
#endif
}

// all 'try_alloc' functions expect size to be ALIGN4

// takes the first 'size' bytes from 'region'
static void* main_pool_region_try_alloc_from_start(struct MainPoolRegion* region, u32 size) {
    if (region->size < size)
        return NULL;

    void* ret = region->start;
    region->start += size;
    region->size -= size;
    sMainPool.freeSpace -= size;
    return ret;
}

// takes the at least first 'size' bytes from 'region' and return pointer aligned on 'alignment'
static void* main_pool_region_try_alloc_from_start_aligned(struct MainPoolRegion* region, u32 size, u32 alignment) {
    u8* ret = (u8*) ALIGN(region->start, alignment);
    u8* newStart = ret + size;
    u8* regionEnd = region->start + region->size;
    if (newStart > regionEnd)
        return NULL;

    size = newStart - region->start;
    region->start = newStart;
    region->size -= size;
    sMainPool.freeSpace -= size;
    return ret;
}

static void* main_pool_region_try_alloc_from_end_freeable(struct MainPoolRegion* region, u8 id, u32 sizeWithHeader) {
    if (region->size < sizeWithHeader)
        return NULL;

    u8* regionEnd = region->start + region->size;

    struct MainPoolFreeableHeader* header = (struct MainPoolFreeableHeader*) (regionEnd - sizeWithHeader);
    header->magic = MAIN_POOL_FREEABLE_HEADER_MAGIC_RIGHT;
    header->id = id;
    header->ptr = regionEnd;

    region->size -= sizeWithHeader;
    sMainPool.freeSpace -= sizeWithHeader;

    return header->data;
}

static void* main_pool_region_try_alloc_from_end_aligned_freeable(struct MainPoolRegion* region, u8 id, u32 size, u32 alignment) {
    u8* region_end = region->start + region->size;
    u8* new_end = (u8*) DOWN(region_end - size, alignment) - sizeof(struct MainPoolFreeableHeader);
    if (new_end < region->start)
        return NULL;

    size = region_end - new_end;

    struct MainPoolFreeableHeader* header = (struct MainPoolFreeableHeader*) new_end;
    header->magic = MAIN_POOL_FREEABLE_HEADER_MAGIC_RIGHT;
    header->id = id;
    header->ptr = region_end;

    region->size -= size;
    sMainPool.freeSpace -= size;

    return header->data;
}

void *main_pool_alloc(u32 size) {
    size = ALIGN4(size);
    for (int i = 0; i < MAIN_POOL_REGIONS_COUNT; i++) {
        struct MainPoolRegion* region = &sMainPool.regions[i];
        void* ret = main_pool_region_try_alloc_from_start(region, size);
        if (!ret)
            continue;

#ifdef MAIN_POOL_USE_BEST_FIT
        // There is only a point in re-sorting if 2 conditions occur
        // 1) It is not the first region, it is already the smallest
        // 2) Final region size is smaller than size. If this is not true,
        //    'regions[i-1]->size < size <= regions[i]->size' satisfies
        if (i && region->size < size)
            main_pool_sort_down(i);
#endif

        return ret;
    }

    DEBUG_ASSERT("Failed to allocate memory");
    return NULL;
}

void *main_pool_alloc_aligned(u32 size, u32 alignment) {
    if (!alignment)
        alignment = 16;

    size = ALIGN4(size);
    for (int i = 0; i < MAIN_POOL_REGIONS_COUNT; i++) {
        struct MainPoolRegion* region = &sMainPool.regions[i];
        void* ret = main_pool_region_try_alloc_from_start_aligned(region, size, alignment);
        if (!ret)
            continue;

#ifdef MAIN_POOL_USE_BEST_FIT
        // Performing +(2 * alignment) check here to give some flexibility to ALIGN that could have caused some changes
        if (i && region->size < size + 2 * alignment)
            main_pool_sort_down(i);
#endif

        return ret;
    }

    DEBUG_ASSERT("Failed to allocate memory");
    return NULL;
}

void *main_pool_alloc_freeable(u32 size) {
    size = ALIGN4(size) + sizeof(struct MainPoolFreeableHeader);
    for (int i = 0; i < MAIN_POOL_REGIONS_COUNT; i++) {
        struct MainPoolRegion* region = &sMainPool.regions[i];
#ifdef MAIN_POOL_USE_BEST_FIT
        u8 regionId = sMainPool.regionIds[i];
#else
        u8 regionId = (u8) i;
#endif
        void* ret = main_pool_region_try_alloc_from_end_freeable(region, regionId, size);
        if (!ret)
            continue;

#ifdef MAIN_POOL_USE_BEST_FIT
        if (i && region->size < size)
            main_pool_sort_down(i);
#endif

        return ret;
    }

    DEBUG_ASSERT("Failed to allocate memory");
    return NULL;
}

void *main_pool_alloc_aligned_freeable(u32 size, u32 alignment) {
    if (!alignment)
        alignment = 16;

    size = ALIGN4(size);
    for (int i = 0; i < MAIN_POOL_REGIONS_COUNT; i++) {
        struct MainPoolRegion* region = &sMainPool.regions[i];
#ifdef MAIN_POOL_USE_BEST_FIT
        u8 regionId = sMainPool.regionIds[i];
#else
        u8 regionId = (u8) i;
#endif
        void* ret = main_pool_region_try_alloc_from_end_aligned_freeable(region, regionId, size, alignment);
        if (!ret)
            continue;

#ifdef MAIN_POOL_USE_BEST_FIT
        if (i && region->size < size + sizeof(struct MainPoolFreeableHeader) + 2 * alignment)
            main_pool_sort_down(i);
#endif

        return ret;
    }

    DEBUG_ASSERT("Failed to allocate memory");
    return NULL;
}

/**
 * Free a block of memory that was allocated from the pool. The block must be
 * the most recently allocated block from its end of the pool, otherwise all
 * newer blocks are freed as well.
 */
void main_pool_free(void *addr) {
    const struct MainPoolFreeableHeader* header = (struct MainPoolFreeableHeader*) ((u8*) addr - sizeof(struct MainPoolFreeableHeader));
    if (header->magic == MAIN_POOL_FREEABLE_HEADER_MAGIC_RIGHT) {
#ifdef MAIN_POOL_USE_BEST_FIT
        for (int i = 0; i < MAIN_POOL_REGIONS_COUNT; i++) {
            if (header->id == sMainPool.regionIds[i]) {
                struct MainPoolRegion* region = &sMainPool.regions[i];
                region->size = header->ptr - region->start;
                MAIN_POOL_DEBUG_CORRUPTION_SIZE_CHECK(region);
                main_pool_sort_up(i);
                break;
            }
        }
#else
    struct MainPoolRegion* region = &sMainPool.regions[header->id];
    region->size = header->ptr - region->start;
#endif
    } else {
        DEBUG_ASSERT("Incorrect magic for free");
    }
}

/**
 * Return the size of the largest block that can currently be allocated from the
 * pool.
 */
u32 main_pool_available(void) {
    return sMainPool.freeSpace;
}

/**
 * Push pool state, to be restored later. Return the amount of free space left
 * in the pool.
 */
void main_pool_push_state(void) {
    struct MainPoolState *prevState = gMainPoolState;
    struct MainPoolContext ctx = sMainPool;

    gMainPoolState = main_pool_alloc(sizeof(*gMainPoolState));
    gMainPoolState->ctx = ctx;
    gMainPoolState->prev = prevState;
}

/**
 * Restore pool state from a previous call to main_pool_push_state. Return the
 * amount of free space left in the pool.
 */
void main_pool_pop_state(void) {
    sMainPool = gMainPoolState->ctx;
    gMainPoolState = gMainPoolState->prev;
}

/**
 * Perform a DMA read from ROM. The transfer is split into 4KB blocks, and this
 * function blocks until completion.
 */
void dma_read(u8 *dest, u8 *srcStart, u8 *srcEnd) {
    u32 size = ALIGN16(srcEnd - srcStart);

    osInvalDCache(dest, size);
    while (size != 0) {
        u32 copySize = (size >= 0x1000) ? 0x1000 : size;

        osPiStartDma(&gDmaIoMesg, OS_MESG_PRI_NORMAL, OS_READ, (uintptr_t) srcStart, dest, copySize,
                     &gDmaMesgQueue);
        osRecvMesg(&gDmaMesgQueue, &gMainReceivedMesg, OS_MESG_BLOCK);

        dest += copySize;
        srcStart += copySize;
        size -= copySize;
    }
}

/**
 * Perform a DMA read from ROM, allocating space in the memory pool to write to.
 * Return the destination address.
 */
void *dynamic_dma_read(u8 *srcStart, u8 *srcEnd, u32 alignment, u32 bssLength) {
    u32 size = ALIGN16(srcEnd - srcStart);
    void* dest = main_pool_alloc_aligned(size + bssLength, alignment);
    if (dest != NULL) {
        dma_read(((u8 *)dest), srcStart, srcEnd);
        if (bssLength) {
            bzero(((u8 *)dest + size), bssLength);
        }
    }
    return dest;
}

static void *dynamic_dma_read_freeable(u8 *srcStart, u8 *srcEnd, u32 alignment, u32 bssLength) {
    u32 size = ALIGN16(srcEnd - srcStart);
    void* dest = main_pool_alloc_aligned_freeable(size + bssLength, alignment);
    if (dest != NULL) {
        dma_read(((u8 *)dest), srcStart, srcEnd);
        if (bssLength) {
            bzero(((u8 *)dest + size), bssLength);
        }
    }
    return dest;
}

#define TLB_PAGE_SIZE 4096 // Blocksize of TLB transfers. Larger values can be faster to transfer, but more wasteful of RAM.
s32 gTlbEntries = 0;
u8 gTlbSegments[NUM_TLB_SEGMENTS] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

void mapTLBPages(uintptr_t virtualAddress, uintptr_t physicalAddress, s32 length, s32 segment) {
    while (length > 0) {
        if (length > TLB_PAGE_SIZE) {
            osMapTLB(gTlbEntries++, OS_PM_4K, (void *)virtualAddress, physicalAddress, (physicalAddress + TLB_PAGE_SIZE), -1);
            virtualAddress  += TLB_PAGE_SIZE;
            physicalAddress += TLB_PAGE_SIZE;
            length          -= TLB_PAGE_SIZE;
            gTlbSegments[segment]++;
        } else {
            osMapTLB(gTlbEntries++, OS_PM_4K, (void *)virtualAddress, physicalAddress, -1, -1);
            gTlbSegments[segment]++;
        }
        virtualAddress  += TLB_PAGE_SIZE;
        physicalAddress += TLB_PAGE_SIZE;
        length          -= TLB_PAGE_SIZE;
    }
}

#ifndef NO_SEGMENTED_MEMORY
/**
 * Load data from ROM into a newly allocated block, and set the segment base
 * address to this block.
 */
void *load_segment(s32 segment, u8 *srcStart, u8 *srcEnd, u8 *bssStart, u8 *bssEnd) {
    void *addr;

    if ((bssStart != NULL)) {
        addr = dynamic_dma_read(srcStart, srcEnd, TLB_PAGE_SIZE, ((uintptr_t)bssEnd - (uintptr_t)bssStart));
        if (addr != NULL) {
            u8 *realAddr = (u8 *)ALIGN(addr, TLB_PAGE_SIZE);
            set_segment_base_addr(segment, realAddr);
            mapTLBPages((segment << 24), VIRTUAL_TO_PHYSICAL(realAddr), ((srcEnd - srcStart) + ((uintptr_t)bssEnd - (uintptr_t)bssStart)), segment);
        }
    } else {
        addr = dynamic_dma_read(srcStart, srcEnd, 0, 0);
        if (addr != NULL) {
            set_segment_base_addr(segment, addr);
        }
    }
#ifdef PUPPYPRINT_DEBUG
    u32 ppSize = ALIGN16(srcEnd - srcStart) + 16;
    set_segment_memory_printout(segment, ppSize);
#endif
    return addr;
}

/*
 * Allocate a block of memory starting at destAddr and ending at the end of
 * the memory pool. Then copy srcStart through srcEnd from ROM to this block.
 * If this block is not large enough to hold the ROM data, or that portion
 * of the pool is already allocated, return NULL.
 */
void *load_to_fixed_pool_addr(u8 *destAddr, u8 *srcStart, u8 *srcEnd) {
    u8* dest = (u8*) DOWN(destAddr, 16);
    u32 srcSize = ALIGN16(srcEnd - srcStart);
    u32 destSize = (u8*) RAM_END - srcStart;
    if (srcSize <= destSize) {
#ifdef USE_EXT_RAM
        bzero(dest, ((u8*)0x80800000) - dest);
#else
        bzero(dest, ((u8*)0x80400000) - dest);
#endif
        osWritebackDCacheAll();
        dma_read(dest, srcStart, srcEnd);
        osInvalICache(dest, destSize);
        osInvalDCache(dest, destSize);
    } else {
        DEBUG_ASSERT("Fixed addr is not be loaded");
    }
    return dest;
}

/**
 * Decompress the block of ROM data from srcStart to srcEnd and return a
 * pointer to an allocated buffer holding the decompressed data. Set the
 * base address of segment to this address.
 */
void *load_segment_decompress(s32 segment, u8 *srcStart, u8 *srcEnd) {
    void *dest = NULL;

#ifdef GZIP
    u32 compSize = (srcEnd - 4 - srcStart);
#else
    u32 compSize = ALIGN16(srcEnd - srcStart);
#endif
    u8 *compressed = main_pool_alloc_aligned_freeable(compSize, 0);
#ifdef GZIP
    // Decompressed size from end of gzip
    u32 *size = (u32 *) (compressed + compSize);
#else
    // Decompressed size from header (This works for non-mio0 because they also have the size in same place)
    u32 *size = (u32 *) (compressed + 4);
#endif
    if (compressed != NULL) {
#ifdef UNCOMPRESSED
        dest = main_pool_alloc_aligned(compSize, 0);
        dma_read(dest, srcStart, srcEnd);
#else
        dma_read(compressed, srcStart, srcEnd);
        dest = main_pool_alloc_aligned(*size, 0);
#endif
        if (dest != NULL) {
            osSyncPrintf("start decompress\n");
#ifdef GZIP
            expand_gzip(compressed, dest, compSize, (u32)size);
#elif RNC1
            Propack_UnpackM1(compressed, dest);
#elif RNC2
            Propack_UnpackM2(compressed, dest);
#elif YAY0
            slidstart(compressed, dest);
#elif MIO0
            decompress(compressed, dest);
#endif
            osSyncPrintf("end decompress\n");
            set_segment_base_addr(segment, dest);
            main_pool_free(compressed);
        }
    }
#ifdef PUPPYPRINT_DEBUG
    u32 ppSize = ALIGN16((u32)*size) + 16;
    set_segment_memory_printout(segment, ppSize);
#endif
    return dest;
}

void load_engine_code_segment(void) {
    void *startAddr = (void *) _engineSegmentStart;
    u32 totalSize = _engineSegmentEnd - _engineSegmentStart;
    // UNUSED u32 alignedSize = ALIGN16(_engineSegmentRomEnd - _engineSegmentRomStart);

    bzero(startAddr, totalSize);
    osWritebackDCacheAll();
    dma_read(startAddr, _engineSegmentRomStart, _engineSegmentRomEnd);
    osInvalICache(startAddr, totalSize);
    osInvalDCache(startAddr, totalSize);
}
#endif

/**
 * Allocate a memory pool from the main pool. This pool supports arbitrary
 * order for allocation/freeing.
 * Return NULL if there is not enough space in the main pool.
 */
struct MemoryPool *mem_pool_init(u32 size) {
    void *addr;
    struct MemoryBlock *block;
    struct MemoryPool *pool = NULL;

    size = ALIGN4(size);
    addr = main_pool_alloc(size + sizeof(struct MemoryPool));
    if (addr != NULL) {
        pool = (struct MemoryPool *) addr;

        pool->totalSpace = size;
        pool->firstBlock = (struct MemoryBlock *) ((u8 *) addr + sizeof(struct MemoryPool));
        pool->freeList.next = (struct MemoryBlock *) ((u8 *) addr + sizeof(struct MemoryPool));

        block = pool->firstBlock;
        block->next = NULL;
        block->size = pool->totalSpace;
    }
#ifdef PUPPYPRINT_DEBUG
    gPoolMem += ALIGN16(size) + 16;
#endif
    return pool;
}

/**
 * Allocate from a memory pool. Return NULL if there is not enough space.
 */
void *mem_pool_alloc(struct MemoryPool *pool, u32 size) {
    struct MemoryBlock *freeBlock = &pool->freeList;
    void *addr = NULL;

    size = ALIGN4(size) + sizeof(struct MemoryBlock);
    while (freeBlock->next != NULL) {
        if (freeBlock->next->size >= size) {
            addr = (u8 *) freeBlock->next + sizeof(struct MemoryBlock);
            if (freeBlock->next->size - size <= sizeof(struct MemoryBlock)) {
                freeBlock->next = freeBlock->next->next;
            } else {
                struct MemoryBlock *newBlock = (struct MemoryBlock *) ((u8 *) freeBlock->next + size);
                newBlock->size = freeBlock->next->size - size;
                newBlock->next = freeBlock->next->next;
                freeBlock->next->size = size;
                freeBlock->next = newBlock;
            }
            break;
        }
        freeBlock = freeBlock->next;
    }
    return addr;
}

/**
 * Free a block that was allocated using mem_pool_alloc.
 */
void mem_pool_free(struct MemoryPool *pool, void *addr) {
    struct MemoryBlock *block = (struct MemoryBlock *) ((u8 *) addr - sizeof(struct MemoryBlock));
    struct MemoryBlock *freeList = pool->freeList.next;

    if (pool->freeList.next == NULL) {
        pool->freeList.next = block;
        block->next = NULL;
    } else {
        if (block < pool->freeList.next) {
            if ((u8 *) pool->freeList.next == (u8 *) block + block->size) {
                block->size += freeList->size;
                block->next = freeList->next;
                pool->freeList.next = block;
            } else {
                block->next = pool->freeList.next;
                pool->freeList.next = block;
            }
        } else {
            while (freeList->next != NULL) {
                if (freeList < block && block < freeList->next) {
                    break;
                }
                freeList = freeList->next;
            }
            if ((u8 *) freeList + freeList->size == (u8 *) block) {
                freeList->size += block->size;
                block = freeList;
            } else {
                block->next = freeList->next;
                freeList->next = block;
            }
            if (block->next != NULL && (u8 *) block->next == (u8 *) block + block->size) {
                block->size = block->size + block->next->size;
                block->next = block->next->next;
            }
        }
    }
}

void *alloc_display_list(u32 size) {
    void *ptr = NULL;

    size = ALIGN8(size);
    if (gGfxPoolEnd - size >= (u8 *) gDisplayListHead) {
        gGfxPoolEnd -= size;
        ptr = gGfxPoolEnd;
    }
    return ptr;
}

static struct DmaTable *load_dma_table_address(u8 *srcAddr) {
    struct DmaTable *table = dynamic_dma_read_freeable(srcAddr, srcAddr + sizeof(u32), 0, 0);
    u32 size = table->count * sizeof(struct OffsetSizePair) +
        sizeof(struct DmaTable) - sizeof(struct OffsetSizePair);
    main_pool_free(table);

    table = dynamic_dma_read(srcAddr, srcAddr + size, 0, 0);
    table->srcAddr = srcAddr;
    return table;
}

void setup_dma_table_list(struct DmaHandlerList *list, void *srcAddr, void *buffer) {
    if (srcAddr != NULL) {
        list->dmaTable = load_dma_table_address(srcAddr);
    }
    list->currentAddr = NULL;
    list->bufTarget = buffer;
}

s32 load_patchable_table(struct DmaHandlerList *list, s32 index) {
    struct DmaTable *table = list->dmaTable;

    if ((u32)index < table->count) {
        u8 *addr = table->srcAddr + table->anim[index].offset;
        s32 size = table->anim[index].size;

        if (list->currentAddr != addr) {
            dma_read(list->bufTarget, addr, addr + size);
            list->currentAddr = addr;
            return TRUE;
        }
    }
    return FALSE;
}
