#define LZ4_HC_STATIC_LINKING_ONLY
#include "lz4hc.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define MAX_COMP_SIZE (8*1024*1024)

// #define FAVOR_DECOMPRESSION_SPEED
#define COMPRESSION_LEVEL LZ4HC_CLEVEL_MAX

#ifdef DEBUG
#define LOG(...) printf(__VA_ARGS__)
#else
#define LOG(...)
#endif

static size_t read_length(unsigned char** data)
{
    LOG("Extra unpack\n");
    size_t length = 0;
    unsigned char byte;
    while ((byte = **data) == 255)
    {
        LOG("Extra unpacked byte: %u\n", byte);
        length += 255;
        (*data)++;
    }

    length += byte;
    (*data)++;
    return length;
}

static void patch_lz4_block(unsigned char* data, size_t data_size, unsigned char* output, uint32_t decompressed_size)
{
    unsigned char* src = data;
    unsigned char* src_end = data + data_size;
    unsigned char* dst = output;
    size_t dst_size = 0;

    while (src < src_end)
    {
        unsigned char token = *src++;
        
        // Literals length
        size_t literal_length = token >> 4;
        if (literal_length == 0x0F)
        {
            literal_length += read_length(&src);
        }

        // Copy literals
        if (literal_length)
        {
            LOG("Literal length: %u\n", literal_length);
            memcpy(dst + dst_size, src, literal_length);
            src += literal_length;
            dst_size += literal_length;
        }

        if (src >= src_end) break;

        // Match offset
        void* pmatch_offset = src;
        unsigned int match_offset = src[0] | (src[1] << 8);
        src += 2;

        // Match length
        size_t match_length = token & 0x0F;
        if (match_length == 0x0F)
        {
            match_length += read_length(&src);
        }
        match_length += 4;

        LOG("Match offset: %u, match length: %u\n", match_offset, match_length);
        if (match_length > match_offset)
        {
            LOG("RLE match\n");
            {
                int looks_like_memset = 1;
                int i;
                unsigned char memset_byte = dst[dst_size - 1];
                for (i = 0; i < match_offset; i++)
                {
                    LOG("%d/%d: 0x%hhx\n", i, dst_size - match_offset + i, dst[dst_size - match_offset + i]);
                    if (memset_byte != dst[dst_size - match_offset + i])
                    {
                        looks_like_memset = 0;
                        break;
                    }
                }

                if (looks_like_memset)
                {
                    LOG("Looks like memset\n");
                    uint16_t off = 1;
                    memcpy(pmatch_offset, &off, 2);
                }
            }
#if 0
            if (0 == (match_offset % 2))
            {
                int looks_like_memset2 = 1;
                int i;
                unsigned char memset_byte0 = dst[dst_size - 1];
                unsigned char memset_byte1 = dst[dst_size - 2];
                for (i = 0; i < match_offset / 2; i++)
                {
                    LOG("%d/%d: 0x%hhx 0x%hhx\n", i, dst_size - match_offset + i * 2, dst_size - match_offset + i * 2 + 1, dst[dst_size - match_offset + i * 2], dst[dst_size - match_offset + i * 2 + 1]);
                    if (memset_byte1 != dst[dst_size - match_offset + i * 2]
                     || memset_byte0 != dst[dst_size - match_offset + i * 2 + 1])
                    {
                        looks_like_memset2 = 0;
                        break;
                    }
                }

                if (looks_like_memset2)
                {
                    LOG("Looks like memset2\n");
                    uint16_t off = 2;
                    memcpy(pmatch_offset, &off, 2);
                }
            }
#endif
        }

        // Copy match
        for (size_t i = 0; i < match_length; i++)
        {
            dst[dst_size + i] = dst[dst_size - match_offset + i];
        }
        dst_size += match_length;
    }
}

int main(int argc, char *argv[])
{
    if (argc < 3)
    {
        printf("Usage: %s [SRC_PATH] [DST_PATH]\n", argv[0]);
        return -1;
    }
    
    FILE *in = fopen(argv[1], "rb");
    if (in == NULL)
    {
        printf("Cannot open input file!\n");
        return -1;
    }
    fseek(in, 0, SEEK_END);
    int srcSize = ftell(in);
    fseek(in, 0, SEEK_SET);
    char* src = malloc(srcSize);
    size_t fread_result = fread(src, srcSize, 1, in);
    fclose(in);

    char* dst = malloc(MAX_COMP_SIZE);
    LZ4_streamHC_t* state = LZ4_createStreamHC();
#ifdef FAVOR_DECOMPRESSION_SPEED
    LZ4_favorDecompressionSpeed(state, 1);
#endif
    LZ4_setCompressionLevel(state, COMPRESSION_LEVEL);
    int compSize = LZ4_compress_HC_continue(state, (char*)src, dst, srcSize, MAX_COMP_SIZE);
    LZ4_freeStreamHC(state);
    if (0 == compSize)
    {
        printf("Compression failed!\n");
        return -1;
    }

#if 0
    char* buf = malloc(8 * 1024 * 1024);
    patch_lz4_block(dst, compSize, buf, 8 * 1024 * 1024);
    free(buf);
#endif

    FILE* out = fopen(argv[2], "wb");
    if (out == NULL)
    {
        printf("Cannot create output file!\n");
        return -1;
    }

    uint32_t compSizeBE = __builtin_bswap32(compSize);
    uint32_t srcSizeBE = __builtin_bswap32(srcSize);
    uint32_t magicHeader = 'LZ4H';
    uint32_t magicFooter = 'LZ4F';

    fwrite(&magicHeader, 1, sizeof(magicHeader), out);
    fwrite(&srcSizeBE  , 1, sizeof(srcSizeBE)  , out);
    fwrite(&compSizeBE , 1, sizeof(compSizeBE) , out);
    fwrite(&magicFooter, 1, sizeof(magicFooter), out);  

    fwrite(dst, compSize, 1, out);

    fclose(out);

    free(src);
    free(dst);

    return 0;
}
