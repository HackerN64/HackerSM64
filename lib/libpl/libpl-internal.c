#include "libpl-internal.h"

#include "libpl-util.h"

typedef struct {
    unsigned short command_id;
    unsigned short payload_size;
    char payload[];
} libpl_request;

static volatile libpl_request *g_request = (volatile libpl_request*)0xbffb0000u;

const libpl_response *libpl_query_basic( unsigned short commandId ) {
	g_request->payload_size = 0;
	g_request->command_id = commandId;
	return (libpl_response*)g_request;
}

const libpl_response *libpl_query_string( unsigned short commandId, const char *arg ) {
	g_request->payload_size = lpl_strncpy( (char*)g_request->payload, arg, 0xFFFC );
	g_request->command_id = commandId;
	return (libpl_response*)g_request;
}

const libpl_response *libpl_query_byte( unsigned short commandId, unsigned char arg ) {
	g_request->payload_size = 1;
	g_request->payload[0] = (char)arg;
	g_request->command_id = commandId;
	return (libpl_response*)g_request;
}

const libpl_response *libpl_query_uint( unsigned short commandId, unsigned int arg ) {
	g_request->payload_size = 4;
	*((volatile unsigned int*)g_request->payload) = arg;
	g_request->command_id = commandId;
	return (libpl_response*)g_request;
}

const libpl_response *libpl_payload_send( unsigned short commandId, unsigned short payloadSize ) {
	g_request->payload_size = payloadSize;
	g_request->command_id = commandId;
	return (libpl_response*)g_request;
}
