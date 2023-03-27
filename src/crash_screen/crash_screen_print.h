#pragma once

#include <ultra64.h>

#include "types.h"


// Maximum number of chars to print at once.
#define CHAR_BUFFER_SIZE 0x100

// Spaces between localized horizontal scrolling sections.
#define TEXT_SCROLL_NUM_SPACES 2

// Char macros:
#define CHAR_NULL       (char)0x00
#define CHAR_NEWLINE    '\n'
#define CHAR_RETURN     '\r'
#define CHAR_SPACE      ' '
#define CHAR_COLOR      '@'
#define CHAR_ESCAPE     '\\'
#define CHAR_SCROLL     '^'

#define IS_NUMERIC(c)   ((c) >= '0' && (c) <= '9')
#define IS_UPPERCASE(c) ((c) >= 'A' && (c) <= 'F')
#define IS_LOWERCASE(c) ((c) >= 'a' && (c) <= 'f')

#define IS_ALPHANUMERIC(c) (IS_NUMERIC(c) || IS_UPPERCASE(c) || IS_LOWERCASE(c))

typedef union {
    struct PACKED {
        /*0x00*/ u16 newLine :  1; // Whether to move to the next line.
        /*0x01*/ u16 shift   : 15; // How many spaces after the previous char to move the index.
        /*0x02*/ u16 print   :  1; // Whether to print a char.
        /*0x03*/ u16 skip    : 15; // Number of chars in the string to skip.
    } d; /*0x04*/
    u32 raw;
} PrintCommand;

extern s8 gCrashScreenWordWrap;

u32 crash_screen_print(u32 startX, u32 startY, const char *fmt, ...);
