#ifdef LZ4T

#include "lz4t.h"

#include "macros.h"

void lz4t_unpack_slow(const uint8_t* restrict inbuf, uint8_t* restrict dst, struct DMAAsyncCtx* ctx)
{
#define LOAD_FRESH_NIBBLES() if (nibbles == 0) { nibbles = GET_UNALIGNED4S(inbuf); if (UNLIKELY(!nibbles)) { return; } inbuf += 4; }
    // DMA checks is checking whether dmaLimit will be exceeded after reading the data.
    // 'dma_async_ctx_read' will wait for the current DMA request and fire the next DMA request
#define DMA_CHECK(v) if (UNLIKELY(v > dmaLimit)) { dmaLimit = dma_async_ctx_read(ctx); }

    uint32_t shortOffsetMask = *(uint8_t*) (inbuf + 8);
    shortOffsetMask <<= 28;
    int matchMin = *(int8_t*) (inbuf + 9);
    int32_t nibbles = *(int32_t*) (inbuf + 12);
    const uint8_t* dmaLimit = inbuf;
    inbuf += 16;
    while (1)
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
                {
                    nibbles <<= 4;
                    continue;
                }
                
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

        if (shortOffsetMask)
        {
            matchOffset &= 0xfff;
            nibbles &= ~0xf0000000;
            nibbles |= (matchCombo & shortOffsetMask);
        }
        else
        {
            nibbles <<= 4;
        }
        matchOffset += 1;

        // If it is 'regular' match, len='7 & (nibbles >> 28)', otherwise extended match '(nibbles >> 28)'
        // We need to start preparing the value for 'matchLen' which is 'matchLim + 3 + exSize'
        int matchLen = matchMin + len;
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
                // I could use 64 bit integer here but generated code is disgusting
                uint32_t data = *copySrc;
                data |= data << 8;
                data |= data << 16;
                do
                {
                    PUT_UNALIGNED4(data, dst);
                    dst += 4;
                    matchLen -= 4;
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
