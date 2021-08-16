#ifndef SEGMENTS_H
#define SEGMENTS_H

#ifndef LINKER
#include "segment_symbols.h"
#endif
/*
 * Memory addresses for segments. Ideally, this header file would not be
 * needed, and the addresses would be defined in sm64.ld and linker-inserted
 * into C code. However, there are some cases where that would not match, where
 * addresses are loaded using lui/ori rather than lui/addiu.
 * To avoid duplication, this file is included from sm64.ld. We make sure not
 * to cast the addresses to pointers in this file, since that would be invalid
 * linker script syntax.
*/

#define USE_EXT_RAM
#ifndef USE_EXT_RAM
#define RAM_END          0x80400000
#else
#define RAM_END          0x80800000
#endif

/*
 * Workaround for running out of pool space due to
 * importing large custom content.
 */

#define SEG_POOL_START   _framebuffersSegmentBssEnd // 0x0165000 in size
#define POOL_SIZE        RAM_END - SEG_POOL_START

#endif // SEGMENTS_H
