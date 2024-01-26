#include "libpl-common.h"
#include "libpl-emu.h"
#include "libpl-launcher.h"
#include "libpl-rhdc.h"
#include "libpl-util.h"

/*! @mainpage
 * 
 * @section main_about About libpl
 * libpl is a helper library to enable roms to integrate with ParallelN64 and
 * Parallel Launcher, allowing for features such as savestate detection,
 * graphics plugin detection, and in-game romhacking.com integration.
 * 
 * @section main_modules Pages
 * -# @ref page_common
 * -# @ref page_emu
 * -# @ref page_launcher
 * -# @ref page_rhdc
 * -# @ref page_util
 * 
 * @attention The ::libpl_is_supported function is the only non-utility libpl
 * function that is safe to call on console. All other libpl functions assume
 * that the program is being run on an emulator that supports libpl.
 * Attempting to call these functions on console or other emulators is undefined
 * behaviour and may result in a crash. Before calling any other libpl function,
 * you should first call ::libpl_is_supported to ensure that you are running on
 * the expected emulator.
 * \n Example:
 * @code
 * bool32_t gSupportsLibpl;
 * 
 * gSupportsLibpl = libpl_is_supported( LPL_ABI_VERSION_CURRENT );
 * 
 * // ...
 * 
 * if( gSupportsLibpl ) {
 * 	// now it is safe to call other libpl functions here
 * }
 * @endcode
 * 
 */
