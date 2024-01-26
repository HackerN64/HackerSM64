#include "libpl-common.h"

int lpl_version_compare( const lpl_version *ls, const lpl_version *rs ) {
	if( rs->major < ls->major ) return 1;
	if( ls->major < rs->major ) return -1;
	if( rs->minor < ls->minor ) return 1;
	if( ls->minor < rs->minor ) return -1;
	if( rs->patch < ls->patch ) return 1;
	if( ls->patch < rs->patch ) return -1;
	return 0;
}
