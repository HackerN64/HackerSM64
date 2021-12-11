#include "zlib.h"

/*
 * Local functions for allocating memory
 *
 * Since expand_gzip is only used for one shot inflation, the only memory needing to
 * be allocated is one copy of inflate_state, which in the current compilation is 7080 bytes
 */
#define GZIP_MEM_SIZE 8000
static char gzip_mem[GZIP_MEM_SIZE];
static int gzip_mem_next = 0;

static void *myalloc(voidpf opaque, unsigned int nItems, unsigned int size)
{
    void *ptr;

    ptr = &gzip_mem[gzip_mem_next];
    gzip_mem_next += nItems*size;

    if (gzip_mem_next < GZIP_MEM_SIZE) {
        return ptr;
    } else {
        return 0;
    }
}

/*
 * Not really free. Free, frees everything!
 */
static void myfree(voidpf opaque, void *address)
{
    gzip_mem_next = 0;
}

/*
 * Returns -ve value for error, or number of output bytes for success
 */
int
expand_gzip(char *in, char *outbuf, unsigned int inLength, unsigned int outbufLength)
{
    int err;
    z_stream d_stream; /* decompression stream */

    d_stream.zalloc = (alloc_func) myalloc;
    d_stream.zfree = (free_func) myfree;
    d_stream.opaque = (voidpf)0;

    d_stream.next_in  = in;
    d_stream.avail_in = inLength;
    d_stream.next_out = outbuf;
    d_stream.avail_out = outbufLength;

    /*
     * Must pass -ve window bits to tell inflate not to expect 
     * a gzip or zlib header
     */
    err = inflateInit2(&d_stream, -MAX_WBITS);
    if (err != Z_OK) {
        return err;
    }

    err = inflate(&d_stream, Z_FINISH);
    if (err != Z_OK && err != Z_STREAM_END) {
        return err;
    }

    err = inflateEnd(&d_stream);
    if (err != Z_OK) {
        return err;
    }

    return d_stream.total_out;

}
