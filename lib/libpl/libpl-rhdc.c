#include "libpl-rhdc.h"

#include <string.h>
#include "libpl-internal.h"

const char *libpl_get_my_rhdc_username() {
	const libpl_response *response = libpl_query_basic( 0x0200 );
	return response->status ? NULL : (const char*)response->payload;
}

int libpl_get_rhdc_avatar_16_async( const char *username, void *dest ) {
	const libpl_response *response = libpl_query_string( 0x0201, username );
	if( !response->status ) memcpy( dest, response->payload, response->payload_size );
	return (int)response->status;
}

int libpl_get_rhdc_avatar_32_async( const char *username, void *dest ) {
	const libpl_response *response = libpl_query_string( 0x0202, username );
	if( !response->status ) memcpy( dest, response->payload, response->payload_size );
	return (int)response->status;
}
