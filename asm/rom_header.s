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
.ascii INTERNAL_ROM_NAME        /* Internal ROM name */
#if defined(EMU_DEFAULT_TO_GCN)
/* Advanced homebrew ROM header bytes: https://n64brew.dev/wiki/ROM_Header#Advanced_Homebrew_ROM_Header */
.word  0x82000000
#else
.word  0x00000000               /* Unknown */
#endif
.word  0x0000004E               /* Cartridge */

.ascii "ED"                     /* Cartridge ID */

/* Region */
#if defined(VERSION_JP) || defined(VERSION_SH)
    .ascii "J"                  /* NTSC-J (Japan) */
#else
    .ascii "E"                  /* NTSC-U (North America) */
#endif

#if defined(USE_RTC)
    #define RTC_BIT 0x1
#else
    #define RTC_BIT 0x0
#endif

/* Savetype, region, and RTC */
#if defined(SRAM)
    .byte  0x32 | RTC_BIT
#elif defined(EEP16K)
    .byte  0x22 | RTC_BIT
#elif defined(SRAM768K)
    .byte  0x42 | RTC_BIT
#elif defined(FLASHRAM)
    .byte  0x52 | RTC_BIT
#else
    .byte  0x12 | RTC_BIT
#endif
