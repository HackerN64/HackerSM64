#pragma once

#include "dma_async.h"
#include "game/emutest.h"

extern void lz4t_unpack_slow(const uint8_t* restrict inbuf, uint8_t* restrict dst, struct DMAAsyncCtx* ctx);
extern void lz4t_unpack_fast(const uint8_t* restrict inbuf, uint8_t* restrict dst, struct DMAAsyncCtx* ctx);

static inline void lz4t_unpack(const uint8_t* restrict inbuf, uint8_t* restrict dst, struct DMAAsyncCtx* ctx)
{
    if (gIsConsole)
    {
        return lz4t_unpack_fast(inbuf, dst, ctx);
    }
    else
    {
        return lz4t_unpack_slow(inbuf, dst, ctx);
    }
}
