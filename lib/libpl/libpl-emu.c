#include "libpl-emu.h"

#ifdef LIBDRAGON
	#include <libdragon.h>
#else
	#include <ultra64.h>
#endif

#include "libpl-internal.h"
#include "libpl-launcher.h"

struct lpl_abi_requirements {
	lpl_version core_version;
	lpl_version launcher_version;
};

static const struct lpl_abi_requirements sAbiReqs[] = {
	{ { 2, 13, 0 }, { 0, 0, 0 } },  // LPL_ABI_VERSION_2
	{ { 2, 13, 0 }, { 6, 20, 0 } }, // LPL_ABI_VERSION_3
	{ { 2, 14, 3 }, { 6, 22, 0 } }  // LPL_ABI_VERSION_4
};

bool32_t libpl_is_supported( int minimumAbiVersion ) {
	if( minimumAbiVersion > LPL_ABI_VERSION_CURRENT ) return FALSE;
	
#ifdef LIBDRAGON
	io_write( 0x1ffb0000u, 0u );
	const unsigned int response = io_read( 0x1ffb0000u );
#else 
	unsigned int response = 0;
	osPiWriteIo( 0x1ffb0000u, 0u );
	osPiReadIo( 0x1ffb0000u, &response );
#endif
	if( response != 0x00500000u ) return FALSE;
	
	if( minimumAbiVersion <= 1 ) return TRUE;
	
	const lpl_version *coreVersion = libpl_get_core_version();
	if( coreVersion == NULL ) return FALSE;
	
	if( lpl_version_compare( coreVersion, &sAbiReqs[minimumAbiVersion - 2].core_version ) < 0 ) return FALSE;
	
	if( minimumAbiVersion == 2 ) return TRUE;
	
	const lpl_version *launcherVersion = libpl_get_launcher_version();
	if( launcherVersion == NULL ) return FALSE;
	
	return lpl_version_compare( launcherVersion, &sAbiReqs[minimumAbiVersion - 2].core_version ) >= 0;
}

const lpl_version *libpl_get_core_version() {
	const libpl_response *response = libpl_query_basic( 0x0001 );
	return (response->protocol_status == LPL_OKAY) ? (const lpl_version*)response->payload : NULL;
}

unsigned int libpl_legacy_get_savestate_token() {
	return *((const unsigned int*)libpl_query_basic( 0x0002 )->payload);
}

int libpl_legacy_get_cheats_used() {
	const libpl_response *response = libpl_query_basic( 0x0003 );
	return (response->protocol_status == LPL_OKAY) ? (int)response->command_status : -1;
}

const lpl_plugin_info *libpl_get_graphics_plugin() {
	const libpl_response *response = libpl_query_basic( 0x0004 );
	return response->status ? NULL : (const lpl_plugin_info*)response->payload;
}

lpl_cheat_flags libpl_get_cheat_flags() {
	const libpl_response *response = libpl_query_basic( 0x0005 );
	return (lpl_cheat_flags)response->command_status;
}

lpl_cheat_flags libpl_clear_cheat_flags( lpl_cheat_flags flags ) {
	const libpl_response *response = libpl_query_byte( 0x0006, (unsigned char)flags );
	return (lpl_cheat_flags)response->command_status;
}
