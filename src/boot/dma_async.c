#include "dma_async.h"

#include "game/main.h"

void dma_async_ctx_init(struct DMAAsyncCtx* ctx, u8 *dest, u8 *srcStart, u8 *srcEnd) {
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
