#include <ultra64.h>
#include "config.h"
#include "macros.h"


#if defined(SAVETYPE)
const char gSaveTypeStr[] = EXPAND_AND_STRINGIFY(SAVETYPE);
#else
const char gSaveTypeStr[] = "???";
#endif

#if defined(REGION)
const char gRegionStr[] = EXPAND_AND_STRINGIFY(REGION);
#else
const char gRegionStr[] = "???";
#endif

#if defined(GRUCODE)
const char gGrucodeStr[] = EXPAND_AND_STRINGIFY(GRUCODE);
#else
const char gGrucodeStr[] = "???";
#endif

#if defined(COMPRESSION_FORMAT)
const char gCompressionFormat[] = EXPAND_AND_STRINGIFY(COMPRESSION_FORMAT);
#else
const char gCompressionFormat[] = "???";
#endif

#if defined(PACKAGE_VERSION)
const char gBuildGitVersion[] = EXPAND_AND_STRINGIFY(PACKAGE_VERSION);
#else
const char gBuildGitVersion[] = "???";
#endif
