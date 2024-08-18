#include <PR/ultratypes.h>

#include "sm64.h"

#include "buffers/buffers.h"
#include "slidec.h"
#include "game/emutest.h"
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


struct MainPoolState {
    u32 freeSpace;
    struct MainPoolBlock *listHeadL;
    struct MainPoolBlock *listHeadR;
    struct MainPoolState *prev;
};

struct MainPoolBlock {
    struct MainPoolBlock *prev;
    struct MainPoolBlock *next;
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
extern u32 sPoolFreeSpace;
extern u8 *sPoolStart;
extern u8 *sPoolEnd;
extern struct MainPoolBlock *sPoolListHeadL;
extern struct MainPoolBlock *sPoolListHeadR;


/**
 * Memory pool for small graphical effects that aren't connected to Objects.
 * Used for colored text, paintings, and environmental snow and bubbles.
 */
struct MemoryPool *gEffectsMemoryPool;


uintptr_t sSegmentTable[32];
u32 sPoolFreeSpace;
u8 *sPoolStart;
u8 *sPoolEnd;
struct MainPoolBlock *sPoolListHeadL;
struct MainPoolBlock *sPoolListHeadR;


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
    Gfx *tempGfxHead = gDisplayListHead;

    for (s32 i = 0; i < 16; i++) {
        gSPSegment(tempGfxHead++, i, sSegmentTable[i]);
    }

    gDisplayListHead = tempGfxHead;
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

/**
 * Initialize the main memory pool. This pool is conceptually a pair of stacks
 * that grow inward from the left and right. It therefore only supports
 * freeing the object that was most recently allocated from a side.
 */
void main_pool_init(void *start, void *end) {
    sPoolStart = (u8 *) ALIGN16((uintptr_t) start) + 16;
    sPoolEnd = (u8 *) ALIGN16((uintptr_t) end - 15) - 16;
    sPoolFreeSpace = sPoolEnd - sPoolStart;

    sPoolListHeadL = (struct MainPoolBlock *) (sPoolStart - 16);
    sPoolListHeadR = (struct MainPoolBlock *) sPoolEnd;
    sPoolListHeadL->prev = NULL;
    sPoolListHeadL->next = NULL;
    sPoolListHeadR->prev = NULL;
    sPoolListHeadR->next = NULL;
#ifdef PUPPYPRINT_DEBUG
    mempool = sPoolFreeSpace;
#endif
}

/**
 * Allocate a block of memory from the pool of given size, and from the
 * specified side of the pool (MEMORY_POOL_LEFT or MEMORY_POOL_RIGHT).
 * If there is not enough space, return NULL.
 */
void *main_pool_alloc(u32 size, u32 side) {
    struct MainPoolBlock *newListHead;
    void *addr = NULL;

    size = ALIGN16(size) + 16;
    if (size != 0 && sPoolFreeSpace >= size) {
        sPoolFreeSpace -= size;
        if (side == MEMORY_POOL_LEFT) {
            newListHead = (struct MainPoolBlock *) ((u8 *) sPoolListHeadL + size);
            sPoolListHeadL->next = newListHead;
            newListHead->prev = sPoolListHeadL;
            newListHead->next = NULL;
            addr = (u8 *) sPoolListHeadL + 16;
            sPoolListHeadL = newListHead;
        } else {
            newListHead = (struct MainPoolBlock *) ((u8 *) sPoolListHeadR - size);
            sPoolListHeadR->prev = newListHead;
            newListHead->next = sPoolListHeadR;
            newListHead->prev = NULL;
            sPoolListHeadR = newListHead;
            addr = (u8 *) sPoolListHeadR + 16;
        }
    }
    return addr;
}

/**
 * Free a block of memory that was allocated from the pool. The block must be
 * the most recently allocated block from its end of the pool, otherwise all
 * newer blocks are freed as well.
 * Return the amount of free space left in the pool.
 */
u32 main_pool_free(void *addr) {
    struct MainPoolBlock *block = (struct MainPoolBlock *) ((u8 *) addr - 16);
    struct MainPoolBlock *oldListHead = (struct MainPoolBlock *) ((u8 *) addr - 16);

    if (oldListHead < sPoolListHeadL) {
        while (oldListHead->next != NULL) {
            oldListHead = oldListHead->next;
        }
        sPoolListHeadL = block;
        sPoolListHeadL->next = NULL;
        sPoolFreeSpace += (uintptr_t) oldListHead - (uintptr_t) sPoolListHeadL;
    } else {
        while (oldListHead->prev != NULL) {
            oldListHead = oldListHead->prev;
        }
        sPoolListHeadR = block->next;
        sPoolListHeadR->prev = NULL;
        sPoolFreeSpace += (uintptr_t) sPoolListHeadR - (uintptr_t) oldListHead;
    }
    return sPoolFreeSpace;
}

/**
 * Resize a block of memory that was allocated from the left side of the pool.
 * If the block is increasing in size, it must be the most recently allocated
 * block from the left side.
 * The block does not move.
 */
void *main_pool_realloc(void *addr, u32 size) {
    void *newAddr = NULL;
    struct MainPoolBlock *block = (struct MainPoolBlock *) ((u8 *) addr - 16);

    if (block->next == sPoolListHeadL) {
        main_pool_free(addr);
        newAddr = main_pool_alloc(size, MEMORY_POOL_LEFT);
    }
    return newAddr;
}

/**
 * Return the size of the largest block that can currently be allocated from the
 * pool.
 */
u32 main_pool_available(void) {
    return sPoolFreeSpace - 16;
}

/**
 * Push pool state, to be restored later. Return the amount of free space left
 * in the pool.
 */
u32 main_pool_push_state(void) {
    struct MainPoolState *prevState = gMainPoolState;
    u32 freeSpace = sPoolFreeSpace;
    struct MainPoolBlock *lhead = sPoolListHeadL;
    struct MainPoolBlock *rhead = sPoolListHeadR;

    gMainPoolState = main_pool_alloc(sizeof(*gMainPoolState), MEMORY_POOL_LEFT);
    gMainPoolState->freeSpace = freeSpace;
    gMainPoolState->listHeadL = lhead;
    gMainPoolState->listHeadR = rhead;
    gMainPoolState->prev = prevState;
    return sPoolFreeSpace;
}

/**
 * Restore pool state from a previous call to main_pool_push_state. Return the
 * amount of free space left in the pool.
 */
u32 main_pool_pop_state(void) {
    sPoolFreeSpace = gMainPoolState->freeSpace;
    sPoolListHeadL = gMainPoolState->listHeadL;
    sPoolListHeadR = gMainPoolState->listHeadR;
    gMainPoolState = gMainPoolState->prev;
    return sPoolFreeSpace;
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

struct DMAAsyncCtx {
    u8* srcStart;
    u8* dest;
    u32 size;
};

// Starts to DMA the first block
static void dma_async_ctx_init(struct DMAAsyncCtx* ctx, u8 *dest, u8 *srcStart, u8 *srcEnd) {
    u32 size = ALIGN16(srcEnd - srcStart);
    osInvalDCache(dest, size);

    u32 copySize = (size >= 0x1000) ? 0x1000 : size;

    osPiStartDma(&gDmaIoMesg, OS_MESG_PRI_NORMAL, OS_READ, (uintptr_t) srcStart, dest, copySize, &gDmaMesgQueue);

    dest += copySize;
    srcStart += copySize;
    size -= copySize;

    ctx->srcStart = srcStart;
    ctx->dest = dest;
    ctx->size = size;
}

// Starts to DMA the next block and waits for the previous block
void* dma_async_ctx_read(struct DMAAsyncCtx* ctx) {
    // wait for the previous DMA issued
    osRecvMesg(&gDmaMesgQueue, &gMainReceivedMesg, OS_MESG_BLOCK);

    // start the new DMA transfer
    u32 copySize = (ctx->size >= 0x1000) ? 0x1000 : ctx->size;
    if (copySize == 0) {
        // we are done, return a dummy address that is so gigantic that we will never be called again
        return (void*) 0x80800000;
    }
    osPiStartDma(&gDmaIoMesg, OS_MESG_PRI_NORMAL, OS_READ, (uintptr_t) ctx->srcStart, ctx->dest, copySize, &gDmaMesgQueue);

    const u32 margin = 16;
    void* ret = ctx->dest - margin;
    ctx->dest += copySize;
    ctx->srcStart += copySize;
    ctx->size -= copySize;

    return ret;
}

/**
 * Perform a DMA read from ROM, allocating space in the memory pool to write to.
 * Return the destination address.
 */
void *dynamic_dma_read(u8 *srcStart, u8 *srcEnd, u32 side, u32 alignment, u32 bssLength) {
    u32 size = ALIGN16(srcEnd - srcStart);
    u32 offset = 0;

    if (alignment && side == MEMORY_POOL_LEFT) {
        offset = ALIGN(((uintptr_t)sPoolListHeadL + 16), alignment) - ((uintptr_t)sPoolListHeadL + 16);
    }

    void *dest = main_pool_alloc((offset + size + bssLength), side);
    if (dest != NULL) {
        dma_read(((u8 *)dest + offset), srcStart, srcEnd);
        if (bssLength) {
            bzero(((u8 *)dest + offset + size), bssLength);
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
void *load_segment(s32 segment, u8 *srcStart, u8 *srcEnd, u32 side, u8 *bssStart, u8 *bssEnd) {
    void *addr;

    if ((bssStart != NULL) && (side == MEMORY_POOL_LEFT)) {
        addr = dynamic_dma_read(srcStart, srcEnd, side, TLB_PAGE_SIZE, ((uintptr_t)bssEnd - (uintptr_t)bssStart));
        if (addr != NULL) {
            u8 *realAddr = (u8 *)ALIGN((uintptr_t)addr, TLB_PAGE_SIZE);
            set_segment_base_addr(segment, realAddr);
            mapTLBPages((segment << 24), VIRTUAL_TO_PHYSICAL(realAddr), ((srcEnd - srcStart) + ((uintptr_t)bssEnd - (uintptr_t)bssStart)), segment);
        }
    } else {
        addr = dynamic_dma_read(srcStart, srcEnd, side, 0, 0);
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
    void *dest = NULL;
    u32 srcSize = ALIGN16(srcEnd - srcStart);
    u32 destSize = ALIGN16((u8 *) sPoolListHeadR - destAddr);

    if (srcSize <= destSize) {
        dest = main_pool_alloc(destSize, MEMORY_POOL_RIGHT);
        if (dest != NULL) {
            bzero(dest, destSize);
            osWritebackDCacheAll();
            dma_read(dest, srcStart, srcEnd);
            osInvalICache(dest, destSize);
            osInvalDCache(dest, destSize);
        }
    }
    return dest;
}

#define PACKED __attribute__((packed))

#define __GET_UNALIGNED_T(type, ptr) ({						\
	const struct { type x; } PACKED *__pptr = (typeof(__pptr))(ptr);	\
	__pptr->x;								\
})

#define __PUT_UNALIGNED_T(type, val, ptr) do {					\
	struct { type x; } PACKED *__pptr = (typeof(__pptr))(ptr);		\
	__pptr->x = (val);							\
} while (0)

#define GET_UNALIGNED8(ptr)	__GET_UNALIGNED_T(uint64_t, (ptr))
#define PUT_UNALIGNED8(val, ptr) __PUT_UNALIGNED_T(uint64_t, (val), (ptr))
#define GET_UNALIGNED4(ptr)	__GET_UNALIGNED_T(uint32_t, (ptr))
#define GET_UNALIGNED4S(ptr)	__GET_UNALIGNED_T(int32_t, (ptr))
#define PUT_UNALIGNED4(val, ptr) __PUT_UNALIGNED_T(uint32_t, (val), (ptr))

#define EXPECT(expr,value)    (__builtin_expect ((expr),(value)) )
#define LIKELY(expr)     EXPECT((expr) != 0, 1)
#define UNLIKELY(expr)   EXPECT((expr) != 0, 0)

#ifdef LZ4
static int lz4_read_length(const uint8_t** inbuf, const uint8_t** dmaLimit, struct DMAAsyncCtx* ctx)
{
    int size = 0;
    uint8_t byte;

    do
    {
        if (UNLIKELY(*inbuf > *dmaLimit)) { *dmaLimit = dma_async_ctx_read(ctx); }
        byte = **inbuf;
        (*inbuf)++;
        size += byte;
    }
    while (UNLIKELY(byte == 0xFF));

    return size;
}

static void lz4_unpack(const uint8_t* inbuf, u32 inbufSize, uint8_t* dst, struct DMAAsyncCtx* ctx)
{
    const uint8_t* inbufEnd = inbuf + inbufSize;
    const uint8_t* dmaLimit = inbuf - 16; 
    while (1)
    {
        if (UNLIKELY(inbuf > dmaLimit)) { dmaLimit = dma_async_ctx_read(ctx); }
        uint8_t token = *inbuf;
        inbuf++;

        {
            int literalSize = token >> 4;
            if (LIKELY(literalSize))
            {
                if (UNLIKELY(literalSize == 0xF))
                    literalSize += lz4_read_length(&inbuf, &dmaLimit, ctx);

                const uint8_t* copySrc = inbuf;
                inbuf += literalSize;
                do
                {
                    if (UNLIKELY((uint8_t*) copySrc > dmaLimit)) { dmaLimit = dma_async_ctx_read(ctx); }
                    const uint64_t data = GET_UNALIGNED8(copySrc);
                    copySrc += 8;
                    PUT_UNALIGNED8(data, dst);
                    dst += 8;
                    literalSize -= 8;
                } while (literalSize > 0);
                dst += literalSize;
            }
        }

        if (UNLIKELY(inbuf >= inbufEnd)) break;

        {
            if (UNLIKELY(inbuf > dmaLimit)) { dmaLimit = dma_async_ctx_read(ctx); }
            uint16_t b1 = inbuf[1];
            inbuf += 2;
            uint16_t b0 = inbuf[-2];
            b1 <<= 8;
            uint16_t matchOffset = b0 | b1;
            
            int matchSize = token & 0xF;
            if (UNLIKELY(matchSize == 0xF))
                matchSize += lz4_read_length(&inbuf, &dmaLimit, ctx);
            
            matchSize += 4;
            const uint8_t* copySrc = dst - matchOffset;
            if (matchOffset > matchSize || matchOffset >= 8)
            {
                do
                {
                    const uint64_t data = GET_UNALIGNED8(copySrc);
                    copySrc += 8;
                    PUT_UNALIGNED8(data, dst);
                    dst += 8;
                    matchSize -= 8;
                } while (matchSize > 0);
                dst += matchSize;
            }
            else
            {
                do
                {
                    uint8_t data = *copySrc;
                    copySrc++;
                    *dst = data;
                    dst++;
                    matchSize--;
                } while (matchSize > 0);
            }
        }
    }
}
#endif

#ifdef LZ4T
static void lz4t_unpack(const uint8_t* restrict inbuf, int32_t nibbles, uint8_t* restrict dst, struct DMAAsyncCtx* ctx)
{
#define LOAD_FRESH_NIBBLES() if (nibbles == 0) { nibbles = GET_UNALIGNED4S(inbuf); if (UNLIKELY(!nibbles)) { return; } inbuf += 4; }
    // DMA checks is checking whether dmaLimit will be exceeded after reading the data.
    // 'dma_async_ctx_read' will wait for the current DMA request and fire the next DMA request
#define DMA_CHECK(v) if (UNLIKELY(v > dmaLimit)) { dmaLimit = dma_async_ctx_read(ctx); }

    const uint8_t* dmaLimit = inbuf - 16;
    for (;; nibbles <<= 4)
    {
        // we will need to read the data (literal or offset) so might as well do it unconditionally from the start
        DMA_CHECK(inbuf);

        // matchLim will define the max amount of size encoded in a single nibble
        // If it is a match after guaranteed literal, it is 15, otherwise 7 (we checked for nibbles >= 0)
        int matchLim = 7;

        LOAD_FRESH_NIBBLES();

        // Each nibble is either 0xxx or 1xxx, xxx is a length
        int len = 7 & (nibbles >> 28);

        // If highest bit of current nibble is set to 1, then it is a literal load
        // Conveniently check for that using '<0' condition
        // Condition for matches will fallthru this check
        if (nibbles < 0)
        {
            // If length is 0, it is an extended match which will be bit encoded
            if (0 != len)
            {
                // Load full 8 byte literals, similar to LZ4 fast dec loop
                // No need to check for DMA limit here, we are still within the range
                const uint64_t data = GET_UNALIGNED8(inbuf);
                inbuf += len;
                PUT_UNALIGNED8(data, dst);
                dst += len;

                // It is unknown whether next nibble will be match or literal so continue
                if (len == matchLim)
                    continue;
                
                // ...otherwise fallthru to matches with extended matchLim
            }
            else
            {
                // Load more than 8 byte literals with a bit encoding loop
                len = 0;
                int shift = 0;
                while (1)
                {
                    int8_t next = *(int8_t*) (inbuf);
                    inbuf++;
                    len |= (next & 0x7f) << shift;
                    shift += 7;
                    if (next >= 0)
                    {
                        break;
                    }
                }

                len += 22;
                const uint8_t* copySrc = inbuf;
                inbuf += len;
                do
                {
                    // TODO: This is unnecessary for the first loop, we are already in the range
                    DMA_CHECK(copySrc);
                    const uint64_t data = GET_UNALIGNED8(copySrc);
                    copySrc += 8;
                    PUT_UNALIGNED8(data, dst);
                    dst += 8;
                    len -= 8;
                } while (len > 0);
                dst += len;

                // ... and fallthru to matches with extended limit
            }
        }
        else
        {
            // This is ugly, but it is the easiest way to do it
            goto matches;
        }

        // here is a fallthru for matches with extended limit so clear out the nibble
        nibbles <<= 4;
        LOAD_FRESH_NIBBLES();

        // match limit is 15 because it is after guaranteed literal
        matchLim = 15;
        // we are here after a literal was loaded so check DMA before loading offset in
        DMA_CHECK(inbuf);
        // cast like this is valid - signed to unsigned will just properly overflow
        len = (((uint32_t) nibbles) >> 28);

matches:
        // pull in offset and potentially the first size
        uint32_t matchCombo = GET_UNALIGNED4(inbuf);
        inbuf += 2;
        // It is 16 bit valid offset that fits in 'int', no need to do casts
        int matchOffset = matchCombo >> 16;
        // If it is 'regular' match, len='7 & (nibbles >> 28)', otherwise extended match '(nibbles >> 28)'
        // We need to start preparing the value for 'matchLen' which is 'matchLim + 3 + exSize'
        int matchLen = 3 + len;
        if (matchLim == len)
        {
            // we want extended matchLen so pull it from data
            // conveniently we have the first 'next' already
            // but I want a sign extended matchCombo 2nd byte so I am doing some ugly stuff
            int8_t next = ((((int) (matchCombo)) << 16) >> 24);
            int exLen = next & 0x7f;
            int shift = 7;
            inbuf++;
            while (next < 0)
            {
                next = *(int8_t*) (inbuf);
                inbuf++;
                exLen |= (next & 0x7f) << shift;
                shift += 7;
            }

            matchLen += exLen;
        }

        const uint8_t* copySrc = dst - matchOffset;
        if (matchOffset > matchLen || matchOffset >= 8)
        {
            do
            {
                const uint64_t data = GET_UNALIGNED8(copySrc);
                copySrc += 8;
                PUT_UNALIGNED8(data, dst);
                dst += 8;
                matchLen -= 8;
            } while (matchLen > 0);
            dst += matchLen;
        }
        else
        {
            if (1 == matchOffset)
            {
                uint64_t data = *copySrc;
                data |= data << 8;
                data |= data << 16;
                data |= data << 32;
                do
                {
                    PUT_UNALIGNED8(data, dst);
                    dst += 8;
                    matchLen -= 8;
                } while (matchLen > 0);
                dst += matchLen;            
            }
            else
            {
                do
                {
                    uint8_t data = *copySrc;
                    copySrc++;
                    *dst = data;
                    dst++;
                    matchLen--;
                } while (matchLen > 0);
            }
        }

        // repeat the loop...
    }

#undef DMA_CHECK
#undef LOAD_FRESH_NIBBLES
}
#endif

#if defined(LZ4) || defined(LZ4T)
#define DMA_ASYNC_HEADER_SIZE 16
#else
#define DMA_ASYNC_HEADER_SIZE 0
#endif

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
    u8 *compressed = main_pool_alloc(compSize, MEMORY_POOL_RIGHT);
#ifdef GZIP
    // Decompressed size from end of gzip
    u32 *size = (u32 *) (compressed + compSize);
#else
    // Decompressed size from header (This works for non-mio0 because they also have the size in same place)
    u32 *size = (u32 *) (compressed + 4);
#endif
    if (compressed != NULL) {
#ifdef UNCOMPRESSED
        dest = main_pool_alloc(compSize, MEMORY_POOL_LEFT);
        dma_read(dest, srcStart, srcEnd);
#else
# if DMA_ASYNC_HEADER_SIZE
        dma_read(compressed, srcStart, srcStart + DMA_ASYNC_HEADER_SIZE);
        struct DMAAsyncCtx asyncCtx;
        dma_async_ctx_init(&asyncCtx, compressed + DMA_ASYNC_HEADER_SIZE, srcStart + DMA_ASYNC_HEADER_SIZE, srcEnd);
# else
        dma_read(compressed, srcStart, srcEnd);
# endif
        dest = main_pool_alloc(*size, MEMORY_POOL_LEFT);
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
#elif LZ4
            u32 lz4CompSize = *(u32 *) (compressed + 8);
            extern int decompress_lz4_full_fast(const void *inbuf, int insize, void *outbuf, void* dmaCtx);
            if (LIKELY(gIsConsole))
                decompress_lz4_full_fast(compressed + DMA_ASYNC_HEADER_SIZE, lz4CompSize, dest, &asyncCtx);
            else
                lz4_unpack(compressed + DMA_ASYNC_HEADER_SIZE, lz4CompSize, dest, &asyncCtx);
#elif LZ4T
            extern int decompress_lz4t_full_fast(const void *inbuf, int insize, void *outbuf, void* dmaCtx);
            if (LIKELY(gIsConsole))
                decompress_lz4t_full_fast(compressed + DMA_ASYNC_HEADER_SIZE, *(int32_t *) (compressed + 12), dest, &asyncCtx);
            else
                lz4t_unpack(compressed + DMA_ASYNC_HEADER_SIZE, *(int32_t *) (compressed + 12), dest, &asyncCtx);
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
 * Allocate an allocation-only pool from the main pool. This pool doesn't
 * support freeing allocated memory.
 * Return NULL if there is not enough space in the main pool.
 */
struct AllocOnlyPool *alloc_only_pool_init(u32 size, u32 side) {
    void *addr;
    struct AllocOnlyPool *subPool = NULL;

    size = ALIGN4(size);
    addr = main_pool_alloc(size + sizeof(struct AllocOnlyPool), side);
    if (addr != NULL) {
        subPool = (struct AllocOnlyPool *) addr;
        subPool->totalSpace = size;
        subPool->usedSpace = 0;
        subPool->startPtr = (u8 *) addr + sizeof(struct AllocOnlyPool);
        subPool->freePtr = (u8 *) addr + sizeof(struct AllocOnlyPool);
    }
    return subPool;
}

/**
 * Allocate from an allocation-only pool.
 * Return NULL if there is not enough space.
 */
void *alloc_only_pool_alloc(struct AllocOnlyPool *pool, s32 size) {
    void *addr = NULL;

    size = ALIGN4(size);
    if (size > 0 && pool->usedSpace + size <= pool->totalSpace) {
        addr = pool->freePtr;
        pool->freePtr += size;
        pool->usedSpace += size;
    }
    return addr;
}

/**
 * Resize an allocation-only pool.
 * If the pool is increasing in size, the pool must be the last thing allocated
 * from the left end of the main pool.
 * The pool does not move.
 */
struct AllocOnlyPool *alloc_only_pool_resize(struct AllocOnlyPool *pool, u32 size) {
    struct AllocOnlyPool *newPool;

    size = ALIGN4(size);
    newPool = main_pool_realloc(pool, size + sizeof(struct AllocOnlyPool));
    if (newPool != NULL) {
        pool->totalSpace = size;
    }
    return newPool;
}

/**
 * Allocate a memory pool from the main pool. This pool supports arbitrary
 * order for allocation/freeing.
 * Return NULL if there is not enough space in the main pool.
 */
struct MemoryPool *mem_pool_init(u32 size, u32 side) {
    void *addr;
    struct MemoryBlock *block;
    struct MemoryPool *pool = NULL;

    size = ALIGN4(size);
    addr = main_pool_alloc(size + sizeof(struct MemoryPool), side);
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
    struct DmaTable *table = dynamic_dma_read(srcAddr, srcAddr + sizeof(u32),
                                                             MEMORY_POOL_LEFT, 0, 0);
    u32 size = table->count * sizeof(struct OffsetSizePair) +
        sizeof(struct DmaTable) - sizeof(struct OffsetSizePair);
    main_pool_free(table);

    table = dynamic_dma_read(srcAddr, srcAddr + size, MEMORY_POOL_LEFT, 0, 0);
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
