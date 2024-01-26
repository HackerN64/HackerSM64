#include "libpl-util.h"

unsigned short lpl_strncpy( char *dest, const char *src, unsigned short max_chars ) {
	unsigned short len = 0;
	while( *src != '\0' && len < max_chars ) {
		dest[len++] = *(src++);
	}
	return len;
}
