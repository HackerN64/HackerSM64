#ifndef LIBPL_INTERNAL_
#define LIBPL_INTERNAL_

#include "libpl-types.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
	union {
		struct {
			unsigned char protocol_status;
			unsigned char command_status;
		};
		unsigned short status;
	};
	unsigned short payload_size;
	char payload[];
} libpl_response;

const libpl_response *libpl_query_basic( unsigned short commandId ) __attribute__((returns_nonnull, assume_aligned(4)));
const libpl_response *libpl_query_string( unsigned short commandId, const char *arg ) __attribute__((returns_nonnull, assume_aligned(4)));
const libpl_response *libpl_query_byte( unsigned short commandId, unsigned char arg ) __attribute__((returns_nonnull, assume_aligned(4)));
const libpl_response *libpl_query_uint( unsigned short commandId, unsigned int arg ) __attribute__((returns_nonnull, assume_aligned(4)));

inline void *libpl_payload_start() { return (void*)0xbffb0004u; }
const libpl_response *libpl_payload_send( unsigned short commandId, unsigned short payloadSize ) __attribute__((returns_nonnull, assume_aligned(4)));

#ifdef __cplusplus
}
#endif

#endif
