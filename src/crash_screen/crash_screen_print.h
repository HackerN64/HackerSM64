#pragma once

#include <ultra64.h>

#include "types.h"


typedef struct __attribute__((packed)) {
    /*0x00*/ u16 newLine : 1;
    /*0x01*/ u16 shift   : 15;
    /*0x02*/ u16 print   : 1;
    /*0x03*/ u16 skip    : 15;
} PrintCommandData; /*0x04*/

typedef union {
    PrintCommandData d;
    u32 val;
} PrintCommand;

// Maximum number of chars to print at once.
#define CHAR_BUFFER_SIZE 0x100

// Spaces between localized horizontal scrolling sections.
#define TEXT_SCROLL_NUM_SPACES 2

// Char macros
#define IS_NUMERIC(c)   ((c) >= '0' && (c) <= '9')
#define IS_UPPERCASE(c) ((c) >= 'A' && (c) <= 'F')
#define IS_LOWERCASE(c) ((c) >= 'a' && (c) <= 'f')

#define IS_ALPHANUMERIC(c) (IS_NUMERIC(c) || IS_UPPERCASE(c) || IS_LOWERCASE(c))

extern s8 gCrashScreenWordWrap;

u32 crash_screen_parse_formatting(const char *buf, u32 index, u32 size, RGBA32 *color, u32 x, u32 y);
u32 crash_screen_print(u32 startX, u32 startY, const char *fmt, ...);
