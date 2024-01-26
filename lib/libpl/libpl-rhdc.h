#ifndef LIBPL_RHDC_
#define LIBPL_RHDC_

#include "libpl-types.h"

#ifdef __cplusplus
extern "C" {
#endif

/*! @defgroup page_rhdc Romhacking.com Commands
 * @{
 */

/*! Returns a pointer to a null-terminated string containing the username of the
 * current RHDC user, or @c NULL if the user has not enabled RHDC integration.
 * The pointer is only valid until you next call a libpl function.
 * 
 * @return A pointer to a null-terminated string containing the username, or @c NULL on an error. \n
 * The returned pointer is only valid until you next invoke a libpl function.
 * 
 * @since LPL_ABI_VERSION_3
 */
const char *libpl_get_my_rhdc_username();

/*! Fetches the avatar for the given username in RHDC as a 32x32 pixel image in
 * RGB16 format and stores it at the provided memory address. If the operation
 * failed or the user is not found, the provided memory is not modified, and the
 * function returns a non-zero value. Otherwise, returns 0.
 * 
 * @param[in] username A null-terminated string containing the username or userId
 * @param[out] dest A buffer of at least 2048 bytes that the avatar will be written to
 * 
 * @return Returns 0 on success, or a non-zero value otherwise \n
 * A return value of 1 indicates that the user was not found or has no avatar \n
 * A return value of 2 indicates a network error (can retry) \n
 * A return value of 3 indicates an image processing error \n
 * A return value of 4 indicates that you are being rate limited (can retry)
 * 
 * @async
 * 
 * @since LPL_ABI_VERSION_3
 */
int libpl_get_rhdc_avatar_16_async( const char *username, void *dest );

/*! Fetches the avatar for the given username in RHDC as a 32x32 pixel image in
 * RGBA32 format and stores it at the provided memory address. If the operation
 * failed or the user is not found, the provided memory is not modified, and the
 * function returns a non-zero value. Otherwise, returns 0.
 * 
 * \bug This function is broken in emulator core version 2.14.0 and is fixed in 2.14.1
 * 
 * @param[in] username A null-terminated string containing the username or userId
 * @param[out] dest A buffer of at least 4096 bytes that the avatar will be written to
 * 
 * @return Returns 0 on success, or a non-zero value otherwise \n
 * A return value of 1 indicates that the user was not found or has no avatar \n
 * A return value of 2 indicates a network error (can retry) \n
 * A return value of 3 indicates an image processing error \n
 * A return value of 4 indicates that you are being rate limited (can retry)
 * 
 * @async
 * 
 * @since LPL_ABI_VERSION_3
 */
int libpl_get_rhdc_avatar_32_async( const char *username, void *dest );

/*! @} */

#ifdef __cplusplus
}
#endif

#endif
