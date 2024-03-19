#pragma once

#include <ultra64.h>

#include "types.h"

#include "util/map_parser.h"


// Maximum number of chars to print at once.
#define CHAR_BUFFER_SIZE 128

// Spaces between localized horizontal scrolling sections.
#define TEXT_SCROLL_NUM_SPACES 2

// Tab size.
#define TAB_WIDTH TEXT_WIDTH(4)

// Char macros:
#define CHAR_NULL                   '\0'
#define CHAR_TAB                    '\t'
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

#define STR_HEX_BYTE        "%02X"
#define STR_HEX_HALFWORD    "%04X"
#define STR_HEX_WORD        "%08X"
#define STR_HEX_LONG        "%016llX"
#define STR_HEX_DECIMAL     "%.15e"

#define STR_COLOR_PREFIX    "@"STR_HEX_WORD //! TODO: use CHAR_COLOR here

#define CHAR_FLT_PREFIX_NULL    '\0'
#define CHAR_FLT_PREFIX_DENORM  'D'
#define CHAR_FLT_PREFIX_NAN     'N'


typedef union PrintBuffer {
    struct PACKED {
        /*0x00*/ RGBA16 red    : 5;
        /*0x00*/ RGBA16 green  : 5;
        /*0x01*/ RGBA16 blue   : 5;
        /*0x01*/ u16 isEscaped : 1; // Repurpose the alpha bit of RGBA32 color as a boolean.
        /*0x02*/ Alpha alpha;
        /*0x03*/ char glyph;
    }; /*0x04*/
    u32 raw;
} PrintBuffer; /*0x04*/

// Input:
extern _Bool  gCSWordWrap;
extern u32    gCSWordWrapXLimit;
extern RGBA32 gCSDefaultPrintColor;

// Output:
extern u32 gCSNumLinesPrinted;

size_t cs_print_impl(u32 x, u32 y, size_t charLimit, const char* fmt, ...) __attribute__((format(printf, 4, 5)));

//! TODO: Change these to ALWAYS_INLINE functions for proper syntax highlighting (is this possible with variable args?).
#define cs_print_scroll(_x, _y, _charLimit, _fmt, ...) cs_print_impl((_x), (_y), (_charLimit), (_fmt), ##__VA_ARGS__)
#define cs_print(_x, _y, _fmt, ...) cs_print_scroll((_x), (_y), 0, (_fmt), ##__VA_ARGS__)
#define cs_print_color_scroll(_x, _y, _charLimit, _color, _fmt, ...)  { \
    RGBA32 __tempDefaultColor = gCSDefaultPrintColor;                   \
    gCSDefaultPrintColor = (_color);                                    \
    cs_print_impl((_x), (_y), (_charLimit), (_fmt), ##__VA_ARGS__);     \
    gCSDefaultPrintColor = __tempDefaultColor;                          \
}
#define cs_print_color(_x, _y, _color, _fmt, ...) cs_print_color_scroll((_x), (_y), 0, (_color), (_fmt), ##__VA_ARGS__)

void cs_print_symbol_name(u32 x, u32 y, u32 maxWidth, const MapSymbol* symbol, _Bool printUnknown);
void cs_print_addr_location_info(u32 x, u32 y, u32 maxWidth, Address addr, _Bool memoryLocationFallback);

typedef struct FloatErrorPrintFormat {
    /*0x00*/ Color r;
    /*0x01*/ Color g;
    /*0x02*/ Color b;
    /*0x03*/ char prefixChar;
    /*0x04*/ char* suffix;
} FloatErrorPrintFormat; /*0x08*/

size_t cs_print_f32(u32 x, u32 y, IEEE754_f32 val, const enum CSPrintNumberFormats format, _Bool includeSuffix);

int sprintf_int_with_commas(char* buf, int n);
void print_as_binary(const u32 x, const u32 y, const Word data, RGBA32 color);
