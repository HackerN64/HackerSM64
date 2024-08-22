#pragma once

#include "types.h"

struct DMAAsyncCtx {
    u8* srcStart;
    u8* dest;
    u32 size;
};

// Starts to DMA the first block
void dma_async_ctx_init(struct DMAAsyncCtx* ctx, u8 *dest, u8 *srcStart, u8 *srcEnd);

// Starts to DMA the next block and waits for the previous block
void* dma_async_ctx_read(struct DMAAsyncCtx* ctx);
