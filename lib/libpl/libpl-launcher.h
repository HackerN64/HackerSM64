#ifndef LIBPL_LAUNCHER_
#define LIBPL_LAUNCHER_

#include "libpl-common.h"

#ifdef __cplusplus
extern "C" {
#endif

/*! @defgroup page_launcher Parallel Launcher Commands
 * @{
 */

/*! A structure containing information about an SD card */
typedef struct {
	unsigned int size; /*!< The size of the SD card image in bytes */
	unsigned char format; /*!< The SD card format (12, 16, or 32 for FAT12/16/32, or 0 for unformatted) */
	char name[]; /*!< The null-terminated name of the SD card image */
} lpl_sd_card_info;

/*! Gets the version of Parallel Launcher
 * @return A pointer to a structure containing the Parallel Launcher version, or @c NULL if an error occurred\n
 * The pointer is only valid until you next call a libpl function.
 * 
 * @since LPL_ABI_VERSION_3
 * 
 * @see ::libpl_get_core_version
 */
const lpl_version *libpl_get_launcher_version();

/*! Fetches info about an SD card created with \ref libpl_create_sd_card. This can be used to check if a shared SD card
 * exits and has the expected size and format before loading it.
 * 
 * @param uid A unique string identifying the shared SD card (max 36 characters)
 * @return A pointer to a struct containing information about the requested SD card, or NULL on an error.
 * The pointer remains valid until the next libpl call.
 * \par
 * Some potential values for lpl_status on an error:\n
 * 4 - No card found with this uid\n
 * 0x200 or 0x600 - uid is not valid
 * 
 * @since LPL_ABI_VERSION_4
 */
const lpl_sd_card_info *libpl_get_sd_card_info( const char *uid ) __attribute__((nonnull(1)));

/*! Creates a new SD card and loads it. This function can only be used to create an SD card when the user has not
 * already provided one. If the user has already provided an SD card, this function will fail with status code 1.
 * Once the SD card has been loaded, you will still need to initialize it like usual.
 * 
 * Unlike \ref libpl_create_auto_sd_card, this function requires you to provide a unique identifier for the SD card.
 * Any other ROM can request this card by passing the same uid to \ref libpl_load_sd_card. This allows collaborating
 * romhacks to share a common SD card.
 * 
 * @param uid A unique string that the shared SD card can be identified with (max 36 characters)
 * @param name The filename of the SD card and the volume name for FAT formatted cards (max 11 characters)
 * @param format The format of the SD card (0 = unformatted, 12 = FAT12, 16 = FAT16, 32 = FAT32)
 * @param sizeMiB The desired size of the SD card in binary megabytes (2^20 bytes).\n
 * For FAT12, sizeMiB must be between 1 and 32\n
 * For FAT16 and FAT32, sizeMiB must be between 32 and 255
 * @return TRUE if an SD card was created and loaded. On a failure, returns FALSE and lpl_status is set to a non-zero value.
 * \par
 * Some potential values for lpl_status on an error:\n
 * 1 - An SD card image is already loaded\n
 * 2 - Emulator core failed to load SD card image\n
 * 3 - Failed to create SD card\n
 * 5 - A card with this uid already exists\n
 * 6 - A card with the given name already exists. Try again with a different name\n
 * 0x200 - uid and/or name is either empty or too long\n
 * 0x600 - one or more arguments is invalid
 * 
 * @since LPL_ABI_VERSION_4
 */
bool32_t libpl_create_sd_card( const char *uid, const char *name, unsigned char format, unsigned char sizeMiB ) __attribute__((nonnull(1, 2)));

/*! Creates a new SD card and loads it. This function can only be used to create an SD card when the user has not
 * already provided one. If the user has already provided an SD card, this function will fail with status code 1.
 * Once the SD card has been loaded, you will still need to initialize it like usual.
 * 
 * @param format The format of the SD card (0 = unformatted, 12 = FAT12, 16 = FAT16, 32 = FAT32)
 * @param sizeMiB The desired size of the SD card in binary megabytes (2^20 bytes)\n
 * For FAT12, sizeMiB must be between 1 and 32\n
 * For FAT16 and FAT32, sizeMiB must be between 32 and 255
 * @return TRUE if an SD card was created and loaded. On a failure, returns FALSE and lpl_status is set to a non-zero value.
 * \par
 * Some potential values for lpl_status on an error:\n
 * 1 - An SD card image is already loaded\n
 * 2 - Emulator core failed to load SD card image\n
 * 3 - Failed to create SD card\n
 * 0x600 - format or size is invalid
 * 
 * @since LPL_ABI_VERSION_4
 */
bool32_t libpl_create_auto_sd_card( unsigned char format, unsigned char sizeMiB );

/*! Loads an SD card that was created with \ref libpl_create_sd_card. This function can only be used to load an SD card
 * when the user has not provided one. If the user has already provided an SD card, this function will fail with status
 * code 1. Once the SD card has been loaded, you will still need to initialize it like usual.
 * 
 * @param uid A unique string identifying the shared SD card (max 36 characters)
 * @return TRUE if an SD card was loaded. On a failure, returns FALSE and lpl_status is set to a non-zero value.
 * \par
 * Some potential values for lpl_status on an error:\n
 * 1 - An SD card image is already loaded\n
 * 2 - Emulator core failed to load SD card image\n
 * 4 - No card found with this uid\n
 * 0x200 or 0x600 - invalid uid
 * 
 * @since LPL_ABI_VERSION_4
 */
bool32_t libpl_load_sd_card( const char *uid ) __attribute__((nonnull(1)));

/*! @} */

#ifdef __cplusplus
}
#endif

#endif
