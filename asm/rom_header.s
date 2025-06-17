/*
 * Super Mario 64 ROM header
 * Only the first 0x18 bytes matter to the console.
 */
#include "config.h"
.byte  0x80, 0x37, 0x12, 0x40   /* PI BSD Domain 1 register */
.word  0x0000000F               /* Clockrate setting*/
.word  entry_point              /* Entrypoint */

/* Revision */
.word  0x0000144C

.word  0x00000000               /* Checksum 1 */
.word  0x00000000               /* Checksum 2 */
.word  0x00000000               /* Unknown */
.word  0x00000000               /* Unknown */
.ascii INTERNAL_ROM_NAME   /* Internal ROM name */
#if defined(EMU_DEFAULT_TO_GCN)
/* Advanced homebrew ROM header bytes: https://n64brew.dev/wiki/ROM_Header#Advanced_Homebrew_ROM_Header */
.word  0x82000000
#else
.word  0x00000000               /* Unknown */
#endif
.word  0x0000004E               /* Cartridge */
#if defined(EEP4K) && !defined(EMU_DEFAULT_TO_GCN) && !defined(USE_RTC)
.ascii "SM"                     /* Cartridge ID */
#else
.ascii "ED"                     /* Cartridge ID */
#endif

/* Region */
#if defined(VERSION_JP) || defined(VERSION_SH)
    .ascii "J"                  /* NTSC-J (Japan) */
#else
    .ascii "E"                  /* NTSC-U (North America) */
#endif

/* Savetype, region, and RTC */
#if defined(SRAM)
    #if defined(USE_RTC)
    .byte  0x33
    #else
    .byte  0x32
    #endif
#elif defined(EEP16K)
    #if defined(USE_RTC)
    .byte  0x23
    #else
    .byte  0x22
    #endif
#elif defined(SRAM768K)
    #if defined(USE_RTC)
    .byte  0x43
    #else
    .byte  0x42
    #endif
#elif defined(FLASHRAM)
    #if defined(USE_RTC)
    .byte  0x53
    #else
    .byte  0x52
    #endif
#else
    #if defined(USE_RTC)
    .byte  0x13
    #else
    .byte  0x12
    #endif
#endif
