#ifndef SEGMENTS_H
#define SEGMENTS_H

#include "segment_names.h"
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

// Starting address of RAM
#define RAM_START   0x80000000

// 1MB of RAM
#define RAM_1MB     0x00100000

#define USE_EXT_RAM
// Calculate total amount of RAM
#ifndef USE_EXT_RAM
#define TOTAL_RAM_SIZE  (RAM_1MB * 4)
#else
#define TOTAL_RAM_SIZE  (RAM_1MB * 8)
#endif

// Ending address of RAM
#define RAM_END (RAM_START + TOTAL_RAM_SIZE)

/*
 * Workaround for running out of pool space due to
 * importing large custom content.
 */

#define SEG_POOL_START   _framebuffersSegmentBssEnd // 0x0165000 in size
#define POOL_SIZE        RAM_END - SEG_POOL_START

#define MAP_PARSER_ADDRESS 0x80345678

#endif // SEGMENTS_H
