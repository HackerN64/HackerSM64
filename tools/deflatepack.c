#include <libdeflate/libdeflate.h>

#include <stdio.h>
#include <stdlib.h>

// MARK: Tools to work with files and output blocks

struct SizedBuffer
{
    void* buffer;
    size_t size;
};

static void saveBufferToFile(const char* buffer, size_t size, const char* filename)
{
    FILE* out = fopen(filename, "wb");
    if (out == NULL)
    {
        printf("Cannot create output file '%s'!\n", filename);
        abort();
    }

    fwrite(buffer, size, 1, out);
    fclose(out);
}

static struct SizedBuffer readFile(const char* filename, int margin)
{
    FILE* in = fopen(filename, "rb");
    if (in == NULL)
    {
        printf("Cannot open input file '%s'\n", filename);
        abort();
    }

    fseek(in, 0, SEEK_END);
    long size = ftell(in);
    fseek(in, 0, SEEK_SET);
    char* buffer = malloc(size + margin);
    size_t fread_result = fread(buffer, size, 1, in);
    fclose(in);
    if (1 != fread_result)
    {
        printf("Failed to read input file '%s': %lu != %ld\n", filename, fread_result, size);
        abort();
    }

    return (struct SizedBuffer) { buffer, size };
}

static void freeSizedBuffer(struct SizedBuffer* data)
{
    free(data->buffer);
}

struct Header
{
    uint32_t magic;
    uint32_t size;
    uint32_t compressedSize;
    uint32_t _pad;
};

static struct SizedBuffer deflateCompress(const struct SizedBuffer* src)
{
    int lvl = 12;
    struct libdeflate_compressor* compressor = libdeflate_alloc_compressor(lvl);
    size_t sz = sizeof(struct Header) + libdeflate_deflate_compress_bound(compressor, src->size);
    uint8_t* buffer = malloc(sz);
    struct Header* header = (struct Header*) buffer;
    size_t compressedSize = libdeflate_deflate_compress(compressor, src->buffer, src->size, buffer + sizeof(struct Header), sz - sizeof(struct Header));
    libdeflate_free_compressor(compressor);
    
    header->magic = __builtin_bswap32(0x4C444654); // 'LDFT'
    header->size = __builtin_bswap32(src->size);
    header->compressedSize = __builtin_bswap32(compressedSize);
    header->_pad = 0;
    return (struct SizedBuffer) { buffer, compressedSize + sizeof(struct Header) };
}

int main(int argc, char *argv[])
{
    if (argc < 3)
    {
        printf("Usage: %s [SRC_PATH] [DST_PATH]\n", argv[0]);
        return -1;
    }

    struct SizedBuffer src = readFile(argv[1], 0);
    struct SizedBuffer data = deflateCompress(&src);
    saveBufferToFile(data.buffer, data.size, argv[2]);
    freeSizedBuffer(&data);
    freeSizedBuffer(&src);
    return 0;
}
