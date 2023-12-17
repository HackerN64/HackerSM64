#pragma once

/*******************
 * MEMORY SETTINGS *
 *******************/

/**
 * Enables use of 8MB RAM.
 * goddard segment is put in either last 8MB or 4MB depending on this option.
 * It is highly recommended to keep this option enabled as it greatly increases the amount of space available for custom content.
 */
#define USE_EXT_RAM

/**
 * Uses alternative algorithm for allocating from main pool. Instead of first fit, it uses best fit
 * It is not recommended to use because it tends to choose slower memory compared to the default order
 * which is ordered from fastest to slowest memory.
 * On the other hand, in theory it is possible to fill memory better with this algorithm, in reality it does not matter 
 */
// #define MAIN_POOL_USE_BEST_FIT

/**
 * Defines amount of allowed main pool memory fragmentation due to zbuffer/framebuffer relocations. The more fragmented the level, the better alignment of framebuffers but worse contig memory allocation.
 * 0  - no fragmentation allowed. This allows for roughly 5.5MB max contig alloc 
 * 10 - all framebuffers & zbuffer are moved to separate 1MB banks using 2 separate bank in the end of the RAM around the 'pivot' point. This allows for roughly 5MB max contig alloc
 * 11 - all framebuffers & zbuffer are moved to separate 1MB banks using 2 separate bank in the end of the RAM at the start of the bank. This allows for roughly 4MB max contig alloc
 * 15 - all framebuffers & zbuffer are moved to separate 1MB banks using 2 separate bank around the 'pivot' point at 0x80500000. Main pool first region starts at 0x80600000. This allows for roughly 2MB+3MB max contig alloc and gives nicer memory use.
 * 20 - each framebuffer and zbuffer are moved to separate 1MB banks around 2 pivot points 0x80700000 and 0x80500000. This allows for roughly 3MB max contig alloc
 * 21 - each framebuffer and zbuffer are moved to separate 1MB banks and put at the start of the bank. This allows for roughly 2MB max contig alloc
 */

#define MEMORY_FRAGMENTATION_LEVEL 10

// For nerds here are the results of measurements

/**
 * Raw numbers of available memory in regions based on the fragmentation level
 * 0  - 0x8025DF00-0x59B960
 * 10 - 0x801C7F40-0x5128C0 0x80770800-0x89060
 * 11 - 0x801C7F80-0x438080 0x80770800-0x89060  0x80625800-0xDA800
 * 15 - 0x80600000-0x1F9860 0x801C7F80-0x312880 0x80570800-0x8F800
 * 20 - 0x801C7F80-0x312880 0x80525800-0x1B5000 0x80725800-0xD4060
 * 21 - 0x801C7F80-0x238080 0x80425800-0xDA800  0x80525800-0xDA800 0x80625800-0xDA800 0x80725800-0xD4060
 */

/**
 * Performance test results for heavily RDP bounded level - JRB entrance with removed music
 * 0  - 23.78 FPS / RDP 125% / RSP 91%
 * 10 - 24.23 FPS / RDP 123% / RSP 89%
 * 11 - 24.08 FPS / RDP 123% / RSP 89%
 * 15 - 24.08 FPS / RDP 123% / RSP 88%
 * 20 - 25.02 FPS / RDP 119% / RSP 86%
 * 21 - 25.03 FPS / RDP 119% / RSP 86%
 */

// Notice that there is no real benefits from enabling mode 2x over 1x but 11 over 10 or 21 over 20 does not bring any benefits other than increased segmentation
// 10 only trims ~0.8MB contig memory while bringing good performance boost
