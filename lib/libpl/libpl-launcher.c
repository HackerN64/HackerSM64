#include "libpl-launcher.h"

#include "libpl-internal.h"
#include "libpl-util.h"

struct lpl_create_sd_card_request {
	unsigned char sizeMiB;
	unsigned char format;
	char nameAndUid[];
};

const lpl_version *libpl_get_launcher_version() {
	const libpl_response *response = libpl_query_basic( 0x0100 );
	return response->status ? NULL : (const lpl_version*)response->payload;
}

const lpl_sd_card_info *libpl_get_sd_card_info( const char *uid ) {
	const libpl_response *response = libpl_query_string( 0x0101, uid );
	return response->status ? NULL : (const lpl_sd_card_info*)response->payload;
}

bool32_t libpl_create_sd_card( const char *uid, const char *name, unsigned char format, unsigned char sizeMiB ) {
	struct lpl_create_sd_card_request *payload = (struct lpl_create_sd_card_request*)libpl_payload_start();
	payload->sizeMiB = sizeMiB;
	payload->format = format;
	
	register unsigned short tailLen = lpl_strncpy( payload->nameAndUid, name, 11 );
	payload->nameAndUid[tailLen++] = '\0';
	tailLen += lpl_strncpy( &payload->nameAndUid[tailLen], uid, 36 );
	
	return libpl_payload_send( 0x0102, tailLen + 2 )->status == 0;
}

bool32_t libpl_create_auto_sd_card( unsigned char format, unsigned char sizeMiB ) {
	struct lpl_create_sd_card_request *payload = (struct lpl_create_sd_card_request*)libpl_payload_start();
	payload->sizeMiB = sizeMiB;
	payload->format = format;
	return libpl_payload_send( 0x0103, 2 )->status == 0;
}

bool32_t libpl_load_sd_card( const char *uid ) {
	const libpl_response *response = libpl_query_string( 0x0104, uid );
	return response->status == 0;
}
