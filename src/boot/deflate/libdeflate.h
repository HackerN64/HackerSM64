/*
 * libdeflate.h - public header for libdeflate
 */

#ifndef LIBDEFLATE_H
#define LIBDEFLATE_H

// HackerSM64 file replaced this file to be compatible with freestanding N64 environment
#include <PR/ultratypes.h>

#ifdef __cplusplus
extern "C" {
#endif

#define LIBDEFLATE_VERSION_MAJOR	1
#define LIBDEFLATE_VERSION_MINOR	23
#define LIBDEFLATE_VERSION_STRING	"1.23"

/*
 * Users of libdeflate.dll on Windows can define LIBDEFLATE_DLL to cause
 * __declspec(dllimport) to be used.  This should be done when it's easy to do.
 * Otherwise it's fine to skip it, since it is a very minor performance
 * optimization that is irrelevant for most use cases of libdeflate.
 */
#ifndef LIBDEFLATEAPI
#  if defined(LIBDEFLATE_DLL) && (defined(_WIN32) || defined(__CYGWIN__))
#    define LIBDEFLATEAPI	__declspec(dllimport)
#  else
#    define LIBDEFLATEAPI
#  endif
#endif

/* ========================================================================== */
/*                             Decompression                                  */
/* ========================================================================== */

struct libdeflate_decompressor;
struct libdeflate_options;

/*
 * libdeflate_alloc_decompressor() allocates a new decompressor that can be used
 * for DEFLATE, zlib, and gzip decompression.  The return value is a pointer to
 * the new decompressor, or NULL if out of memory.
 *
 * This function takes no parameters, and the returned decompressor is valid for
 * decompressing data that was compressed at any compression level and with any
 * sliding window size.
 *
 * A single decompressor is not safe to use by multiple threads concurrently.
 * However, different threads may use different decompressors concurrently.
 */
LIBDEFLATEAPI struct libdeflate_decompressor *
libdeflate_alloc_decompressor(void);

/*
 * Result of a call to libdeflate_deflate_decompress(),
 * libdeflate_zlib_decompress(), or libdeflate_gzip_decompress().
 */
enum libdeflate_result {
	/* Decompression was successful.  */
	LIBDEFLATE_SUCCESS = 0,

	/* Decompression failed because the compressed data was invalid,
	 * corrupt, or otherwise unsupported.  */
	LIBDEFLATE_BAD_DATA = 1,

	/* A NULL 'actual_out_nbytes_ret' was provided, but the data would have
	 * decompressed to fewer than 'out_nbytes_avail' bytes.  */
	LIBDEFLATE_SHORT_OUTPUT = 2,

	/* The data would have decompressed to more than 'out_nbytes_avail'
	 * bytes.  */
	LIBDEFLATE_INSUFFICIENT_SPACE = 3,
};

/*
 * libdeflate_deflate_decompress() decompresses a DEFLATE stream from the buffer
 * 'in' with compressed size up to 'in_nbytes' bytes.  The uncompressed data is
 * written to 'out', a buffer with size 'out_nbytes_avail' bytes.  If
 * decompression succeeds, then 0 (LIBDEFLATE_SUCCESS) is returned.  Otherwise,
 * a nonzero result code such as LIBDEFLATE_BAD_DATA is returned, and the
 * contents of the output buffer are undefined.
 *
 * Decompression stops at the end of the DEFLATE stream (as indicated by the
 * BFINAL flag), even if it is actually shorter than 'in_nbytes' bytes.
 *
 * libdeflate_deflate_decompress() can be used in cases where the actual
 * uncompressed size is known (recommended) or unknown (not recommended):
 *
 *   - If the actual uncompressed size is known, then pass the actual
 *     uncompressed size as 'out_nbytes_avail' and pass NULL for
 *     'actual_out_nbytes_ret'.  This makes libdeflate_deflate_decompress() fail
 *     with LIBDEFLATE_SHORT_OUTPUT if the data decompressed to fewer than the
 *     specified number of bytes.
 *
 *   - If the actual uncompressed size is unknown, then provide a non-NULL
 *     'actual_out_nbytes_ret' and provide a buffer with some size
 *     'out_nbytes_avail' that you think is large enough to hold all the
 *     uncompressed data.  In this case, if the data decompresses to less than
 *     or equal to 'out_nbytes_avail' bytes, then
 *     libdeflate_deflate_decompress() will write the actual uncompressed size
 *     to *actual_out_nbytes_ret and return 0 (LIBDEFLATE_SUCCESS).  Otherwise,
 *     it will return LIBDEFLATE_INSUFFICIENT_SPACE if the provided buffer was
 *     not large enough but no other problems were encountered, or another
 *     nonzero result code if decompression failed for another reason.
 */
LIBDEFLATEAPI enum libdeflate_result
libdeflate_deflate_decompress(struct libdeflate_decompressor *decompressor, const void *in, size_t in_nbytes, void *out, void* asyncCtx);

/*
 * libdeflate_free_decompressor() frees a decompressor that was allocated with
 * libdeflate_alloc_decompressor().  If a NULL pointer is passed in, no action
 * is taken.
 */
LIBDEFLATEAPI void
libdeflate_free_decompressor(struct libdeflate_decompressor *decompressor);

#ifdef __cplusplus
}
#endif

#endif /* LIBDEFLATE_H */
