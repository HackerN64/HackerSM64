#pragma once

#include <ultra64.h>

#include <stdarg.h>
#include "types.h"


// Maximum number of chars to print at once.
#define CHAR_BUFFER_SIZE 128

// Spaces between localized horizontal scrolling sections.
#define TEXT_SCROLL_NUM_SPACES 2

// Char macros:
#define CHAR_NULL                   '\0'
#define CHAR_NEWLINE                '\n'
#define CHAR_RETURN                 '\r'
#define CHAR_SPACE                  ' '
#define CHAR_COLOR                  '@'
#define CHAR_ESCAPE                 '\\' // CHAR_BACKSLASH?

// Alphanumeric checks:
#define CHAR_NUMERIC_START          '0'
#define CHAR_NUMERIC_END            '9'

#define CHAR_UPPERCASE_START        'A'
#define CHAR_UPPERCASE_END          'Z'
#define CHAR_UPPERCASE_HEX_START    CHAR_UPPERCASE_START
#define CHAR_UPPERCASE_HEX_END      'F'

#define CHAR_LOWERCASE_START        'a'
#define CHAR_LOWERCASE_END          'z'
#define CHAR_LOWERCASE_HEX_START    CHAR_LOWERCASE_START
#define CHAR_LOWERCASE_HEX_END      'f'

// Alphanumeric check macros:
#define IS_NUMERIC(c)           ((c) >= CHAR_NUMERIC_START       && (c) <= CHAR_NUMERIC_END      )

#define IS_UPPERCASE(c)         ((c) >= CHAR_UPPERCASE_START     && (c) <= CHAR_UPPERCASE_END    )
#define IS_LOWERCASE(c)         ((c) >= CHAR_LOWERCASE_START     && (c) <= CHAR_LOWERCASE_END    )

#define IS_UPPERCASE_HEX(c)     ((c) >= CHAR_UPPERCASE_HEX_START && (c) <= CHAR_UPPERCASE_HEX_END)
#define IS_LOWERCASE_HEX(c)     ((c) >= CHAR_LOWERCASE_HEX_START && (c) <= CHAR_LOWERCASE_HEX_END)

#define IS_ALPHANUMERIC(c)      (IS_NUMERIC(c) || IS_UPPERCASE(c)     || IS_LOWERCASE(c)    )
#define IS_ALPHANUMERIC_HEX(c)  (IS_NUMERIC(c) || IS_UPPERCASE_HEX(c) || IS_LOWERCASE_HEX(c))

// Preset strings:
#define STR_HEX_PREFIX      "0x"

#define STR_HEX_WORD        "%08X"
#define STR_HEX_HALFWORD    "%04X"
#define STR_HEX_BYTE        "%02X"

#define STR_COLOR_PREFIX    "@"STR_HEX_WORD //! TODO: use CHAR_COLOR here

typedef struct PACKED {
    RGBA32 color; //! TODO: RGBA16
    _Bool isEscaped;
    char glyph;
} PrintBuffer; /*0x08*/

extern _Bool gCSWordWrap;

size_t crash_screen_print_impl(u32 x, u32 y, size_t charLimit, const char* fmt, ...);

//! TODO: change these to ALWAYS_INLINE functions for proper syntax highlighting (is this possible with variable args?)
#define crash_screen_print(x, y, ...)                   crash_screen_print_impl((x), (y),           0, __VA_ARGS__)
#define crash_screen_print_scroll(x, y, charLimit, ...) crash_screen_print_impl((x), (y), (charLimit), __VA_ARGS__)

void crash_screen_print_map_name(u32 x, u32 y, u32 maxWidth, RGBA32 color, const char* fname);
