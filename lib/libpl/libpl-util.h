#ifndef LIBPL_UTIL_
#define LIBPL_UTIL_

#include "libpl-types.h"

#ifdef __cplusplus
extern "C" {
#endif

/*! @defgroup page_util Utility Functions
 * 
 * The functions on this page are utility functions that do not use the libpl
 * emulator extension, but are provided for convenience. All functions on this
 * page can safely be used on console.
 * @{
 */

/*! Copies characters from src, stopping when a null character is reached or
 * @c max_chars have been copied. The null terminator is not copied.
 * 
 * @param[out] dest The destination to copy src to
 * @param[in] src The string to copy
 * @param max_chars The maximum number of characters to copy
 * @return the number of characters copied.
 * 
 * @note This function is safe to call on console
 */
unsigned short lpl_strncpy( char *dest, const char *src, unsigned short max_chars ) __attribute__((nonnull(1, 2)));

/*! Copies characters from src, stopping when a null character is reached or
 * <tt>max_chars - 1</tt> have been copied. The copy is then null terminated.
 * 
 * @param[out] dest The destination to copy src to
 * @param[in] src The string to copy
 * @param max_chars The maximum number of characters to write (including the null terminator)
 * 
 * @note This function is safe to call on console
 */
__attribute__((always_inline, nonnull(1, 2))) inline void lpl_strncpy2( char *dest, const char *src, unsigned short max_chars ) {
	dest[lpl_strncpy( dest, src, max_chars - 1 )] = '\0';
}

/*! @} */

#ifdef __cplusplus
}
#endif

#endif
