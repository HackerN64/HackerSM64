#ifndef LIBPL_COMMON_
#define LIBPL_COMMON_

#include "libpl-types.h"

#ifdef __cplusplus
extern "C" {
#endif

/*! @defgroup page_common Core libpl defines and types
 * @{
 */

/*! @defgroup defines_errno lpl_errno values
 * Possible values for @ref lpl_errno
 * @{
 * 
 * @def LPL_OKAY
 * Indicates that the libpl command was accepted. This does not necessarily mean
 * that no errors occurred, but rather that it simply passed initial validation.
 * You may also need to check the return value of the libpl function to verify
 * that it completed successfully.
 * 
 * @def LPL_COMMAND_NOT_FOUND
 * Indicates that the emulator core or Parallel Launcher does not know how to
 * handle your request. If you correctly check that the emulator supports the
 * libpl version you need with ::libpl_is_supported, you should never get this
 * error code.
 * 
 * @def LPL_INVALID_PAYLOAD_SIZE
 * Indicates that a provided string or array has an invalid size.
 * 
 * @def LPL_BROKEN_PIPE
 * Indicates that the connection to Parallel Launcher has been lost. All future
 * requests to Parallel Launcher are likely to fail with the same error code.
 * 
 * @def LPL_BAD_STATE
 * Indicates that an unexpected sequence of libpl requests has left Parallel
 * Launcher in a bad state. This value is currently unused.
 * 
 * @def LPL_WAIT
 * Indicates that the libpl request has been acknowledged, but the response is
 * not ready yet. Call the function again later with the same arguments to check
 * if it is done yet.
 * 
 * @def LPL_INVALID_PAYLOAD
 * Indicates that something is wrong with one of the provided arguments, such as
 * an integer value that is not in the expected range or a character string that
 * contains unexpected characters.
 * 
 * @}
 */

#define LPL_OKAY                 0x00
#define LPL_COMMAND_NOT_FOUND    0x01
#define LPL_INVALID_PAYLOAD_SIZE 0x02
#define LPL_BROKEN_PIPE          0x03
#define LPL_BAD_STATE            0x04
#define LPL_WAIT                 0x05
#define LPL_INVALID_PAYLOAD      0x06

/*! The lpl_errno macro gets the status code of the last libpl request.
 * See @ref defines_errno for possible values.
 */
#define lpl_errno (*(volatile const unsigned char*)0xbffb0000u)

/*! The lpl_status macro gives a status code for the last libpl request.
 * The upper byte is the same as @ref lpl_errno, and the lower byte is
 * a status code whose meaning depends on the libpl function invoked.
 */
#define lpl_status (*(volatile const unsigned short*)0xbffb0000u)

/*! A structure representing a version */
typedef struct {
	unsigned short major;
	unsigned short minor;
	unsigned short patch;
} lpl_version;

/*! Compares two @ref lpl_version structs
 * @param[in] ls A pointer to the first version
 * @param[in] rs A pointer to the second version
 * @return Returns -1 if ls is an earlier version than rs, 1 if ls is an older version than rs, and 0 if they are the same version
 */
int lpl_version_compare( const lpl_version *ls, const lpl_version *rs ) __attribute__((pure, warn_unused_result, nonnull(1, 2)));

/*! @} */

#ifdef __cplusplus
}
#endif

#endif
