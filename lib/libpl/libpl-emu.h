#ifndef LIBPL_EMU_
#define LIBPL_EMU_

#include "libpl-common.h"

#ifdef __cplusplus
extern "C" {
#endif

/*! @defgroup page_emu Emulator Commands
 * 
 * The functions on this page are handled directly by the emulator core. They
 * will function even if the emulator core is extracted from Parallel Launcher
 * and put into a normal RetroArch installation.
 * 
 * @{
 */

/*! @name Graphics Plugin IDs
 * @{
 */
#define LPL_PARALLEL  1 /*!< ParaLLEl */
#define LPL_GLIDEN64  2 /*!< GLideN64 */
#define LPL_OGRE      3 /*!< OGRE */
#define LPL_GLIDE64   4 /*!< Glide64 */
#define LPL_ANGRYLION 5 /*!< Angrylion */
#define LPL_RICE      6 /*!< Rice */
#define LPL_GLN64     7 /*!< gln64 */
/*! @} */

/*! @name Graphics Plugin Capabilities
 * @{
 * 
 * @def LPL_UPSCALING
 * @details Indicates that the plugin can render at higher resolutions than an actual N64.
 * @note The ParaLLEl plugin will have this bit set to 0 if its upscaling factor is set to 1
 * 
 * @def LPL_FRAMEBUFFER_EMULATION
 * @details Indicates that framebuffer emulation is currently enabled on this plugin
 * 
 * @def LPL_ACCURATE_DEPTH_COMPARE
 * @details Indicates that the plugin is currently handling depth comparisons accurately (ie. decals will work properly)
 * 
 * @}
 */
#define LPL_UPSCALING              0x0001
#define LPL_FRAMEBUFFER_EMULATION  0x0002
#define LPL_ACCURATE_DEPTH_COMPARE 0x0004

/*! cheat status flags */
typedef enum {
	LPL_USED_CHEATS =        0x01, /*!< Gameshark codes have been used **/
	LPL_USED_SAVESTATES =    0x02, /*!< A savestate has been loaded **/
	LPL_USED_SLOWDOWN =      0x04, /*!< Emulator slowdown has been used **/
	LPL_USED_FRAME_ADVANCE = 0x08, /*!< The emulation has been advanced in frame by frame mode **/
	LPL_USED_SPEEDUP =       0x10, /*!< The emulation has been sped up @since LPL_ABI_VERSION_4 **/
	
	// convenience values for clearing flags
	LPL_ALL_CHEAT_FLAGS = 0xff, /*!< A mask that covers all flags */
	LPL_ALL_TAS_FLAGS = 0xfe /*!< A mask that covers all flags except for @ref LPL_USED_CHEATS */
} lpl_cheat_flags;

/*! A structure containing information about the current graphics plugin and its settings
 */
typedef struct {
	unsigned short plugin_id; /*!< A plugin id identifying the graphics plugin. (See Graphics Plugin IDs) */
	unsigned short capabilities; /*!< A bitfield containing the capabilities of the plugin (See Graphics Plugin Capabilities) */
	char name[]; /*!< A null terminated string containing a human readable name for the plugin */
} lpl_plugin_info;

/* libpl ABI versions */
#define LPL_ABI_VERSION_1   1 /*!< initial libpl prototype (2023-07-17) */
#define LPL_ABI_VERSION_2   2 /*!< revised prototype (2023-08-1) */
#define LPL_ABI_VERSION_3   3 /*!< first advertised public release (2023-08-1) */
#define LPL_ABI_VERSION_4   4 /*!< SD card update (2023-10-03) */
#define LPL_ABI_VERSION_CURRENT LPL_ABI_VERSION_4 /*!< the latest version */

/*! Checks if the emulator supports libpl
 * 
 * This function is safe to call on console and other emulators
 * 
 * @param minimumAbiVersion The required libpl ABI version
 * @return TRUE if the emulator supports the requested version of libpl or later
 */
bool32_t libpl_is_supported( int minimumAbiVersion );

/*! Gets the emulator core version
 * 
 * @return A pointer to a structure containing the emulator core version, or @c NULL if an error occurred\n
 * The pointer is only valid until you next call a libpl function.
 * 
 * @see ::libpl_get_launcher_version
 */
const lpl_version *libpl_get_core_version();

/*! Returns a number that only changes when a savestate is loaded
 * @deprecated Use libpl_get_cheat_flags() instead
 * @return a unique 32-bit number. Calls to this fuction will continue to return the save value until a savestate is loaded
 */
unsigned int libpl_legacy_get_savestate_token();

/*! Returns TRUE if cheats have been used, or -1 on an error
 * @deprecated Use libpl_get_cheat_flags() instead
 * @return 1 if gameshark codes have been used; 0 otherwise
 */
int libpl_legacy_get_cheats_used();

/*! Gets information about the current graphics plugin
 * @return A pointer to a @ref lpl_plugin_info structure containing information about the current graphics plugin, or @c NULL if an error occurred\n
 * The pointer is only valid until you next call a libpl function.
 */
const lpl_plugin_info *libpl_get_graphics_plugin();

/*! Check if any cheats or TAS tools have been used.
 * 
 * @return The current state of the cheat flags.
 * 
 * @since LPL_ABI_VERSION_2
 */
lpl_cheat_flags libpl_get_cheat_flags();

/*! Clears the specified cheat flags, meaning that the next call to libpl_get_graphics_plugin()
 * will no longer report this cheat as having been used. If the user is actively using cheats at
 * the time this is called, the flags for the active cheats will not be cleared.
 * 
 * @note clearing the @ref LPL_USED_CHEATS flag will NOT cause the deprecated
 * function libpl_legacy_get_cheats_used() to no longer return 1.
 * 
 * @param flags A bitmask of the cheat flags to clear
 * @return The new state of the cheat flags.
 * 
 * @since LPL_ABI_VERSION_2
 */
lpl_cheat_flags libpl_clear_cheat_flags( lpl_cheat_flags flags );

/*! @} */

#ifdef __cplusplus
}
#endif

#endif
