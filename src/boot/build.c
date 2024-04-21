#include <ultra64.h>
#include "config.h"
#include "macros.h"


#if defined(SAVETYPE)
const char gSaveTypeStr[] = TO_STRING2(SAVETYPE);
#else
const char gSaveTypeStr[] = "???";
#endif

#if defined(REGION)
const char gRegionStr[] = TO_STRING2(REGION);
#else
const char gRegionStr[] = "???";
#endif

#if defined(GRUCODE)
const char gGrucodeStr[] = TO_STRING2(GRUCODE);
#else
const char gGrucodeStr[] = "???";
#endif

#if defined(COMPRESSION_FORMAT)
const char gCompressionFormat[] = TO_STRING2(COMPRESSION_FORMAT);
#else
const char gCompressionFormat[] = "???";
#endif

#if defined(PACKAGE_VERSION)
const char gBuildGitVersion[] = TO_STRING2(PACKAGE_VERSION);
#else
const char gBuildGitVersion[] = "???";
#endif
