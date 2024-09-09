#pragma once

#include <ultra64.h>

#include "types.h"

#include "util/map_parser.h"
#include "cs_draw.h"
#include "cs_settings.h"


// Maximum number of chars to print at once.
#define CHAR_BUFFER_SIZE        256
#define CS_PRINT_BUFFER_SIZE    128
#define CS_SCROLL_BUFFER_SIZE   48

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
#define IS_NUMERIC(_c)          ((_c) >= CHAR_NUMERIC_START       && (_c) <= CHAR_NUMERIC_END      )

#define IS_UPPERCASE(_c)        ((_c) >= CHAR_UPPERCASE_START     && (_c) <= CHAR_UPPERCASE_END    )
#define IS_LOWERCASE(_c)        ((_c) >= CHAR_LOWERCASE_START     && (_c) <= CHAR_LOWERCASE_END    )

#define IS_UPPERCASE_HEX(_c)    ((_c) >= CHAR_UPPERCASE_HEX_START && (_c) <= CHAR_UPPERCASE_HEX_END)
#define IS_LOWERCASE_HEX(_c)    ((_c) >= CHAR_LOWERCASE_HEX_START && (_c) <= CHAR_LOWERCASE_HEX_END)

#define IS_ALPHANUMERIC(_c)     (IS_NUMERIC(_c) || IS_UPPERCASE(_c)     || IS_LOWERCASE(_c)    )
#define IS_ALPHANUMERIC_HEX(_c) (IS_NUMERIC(_c) || IS_UPPERCASE_HEX(_c) || IS_LOWERCASE_HEX(_c))

// Alphanumeric conversion macros:

// #define TO_UPPERCASE(_c) if (IS_LOWERCASE(_c)) { (_c) = (((_c) - CHAR_LOWERCASE_START) + CHAR_UPPERCASE_START); }
// #define TO_LOWERCASE(_c) if (IS_UPPERCASE(_c)) { (_c) = (((_c) - CHAR_UPPERCASE_START) + CHAR_LOWERCASE_START); }
#define TO_UPPERCASE(_c) if (IS_LOWERCASE(_c)) { (_c) = ((_c) - (CHAR_LOWERCASE_START - CHAR_UPPERCASE_START)); }
#define TO_LOWERCASE(_c) if (IS_UPPERCASE(_c)) { (_c) = ((_c) + (CHAR_LOWERCASE_START - CHAR_UPPERCASE_START)); }

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


#define CS_DEFAULT_PRINT_COLOR COLOR_RGBA32_WHITE


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
STATIC_ASSERT_STRUCT_SIZE_LE(PrintBuffer, sizeof(u32));

// Input:
extern _Bool           gCSWordWrap;
extern ScreenCoord_u32 gCSWordWrapXLimit;
extern RGBA32          gCSDefaultPrintColor;

// Output:
extern CSTextCoord_u32 gCSNumLinesPrinted;

size_t cs_print_impl(ScreenCoord_u32 x, ScreenCoord_u32 y, size_t charLimit, const char* fmt, ...) __attribute__((format(printf, 4, 5)));

//! TODO: Change these to ALWAYS_INLINE functions for proper syntax highlighting (is this possible with variable args?).
#define cs_print_scroll(_x, _y, _charLimit, _fmt, ...) cs_print_impl((_x), (_y), (_charLimit), (_fmt), ##__VA_ARGS__)
#define cs_print(_x, _y, _fmt, ...) cs_print_scroll((_x), (_y), 0, (_fmt), ##__VA_ARGS__)

#define CS_SET_DEFAULT_PRINT_COLOR_START(_color)        \
    RGBA32 __tempDefaultColor = gCSDefaultPrintColor;   \
    gCSDefaultPrintColor = (_color);                    \

#define CS_SET_DEFAULT_PRINT_COLOR_END()                \
    gCSDefaultPrintColor = __tempDefaultColor;          \

#define CS_IS_DEFAULT_PRINT_COLOR_DEFAULT() (gCSDefaultPrintColor == CS_DEFAULT_PRINT_COLOR)

#define cs_print_color_scroll(_x, _y, _charLimit, _color, _fmt, ...)  { \
    CS_SET_DEFAULT_PRINT_COLOR_START(_color);                           \
    cs_print_impl((_x), (_y), (_charLimit), (_fmt), ##__VA_ARGS__);     \
    CS_SET_DEFAULT_PRINT_COLOR_END();                                   \
}
#define cs_print_color(_x, _y, _color, _fmt, ...) cs_print_color_scroll((_x), (_y), 0, (_color), (_fmt), ##__VA_ARGS__)


size_t cs_print_symbol_name(ScreenCoord_u32 x, ScreenCoord_u32 y, u32 maxWidth, const MapSymbol* symbol, _Bool printUnknown);
size_t cs_print_addr_location_info(ScreenCoord_u32 x, ScreenCoord_u32 y, u32 maxWidth, Address addr, _Bool sureAddress);


typedef struct FloatErrorPrintFormat {
    /*0x00*/ Color r;
    /*0x01*/ Color g;
    /*0x02*/ Color b;
    /*0x03*/ char prefixChar;
    /*0x04*/ char* suffix;
} FloatErrorPrintFormat; /*0x08*/

size_t cs_print_f32(ScreenCoord_u32 x, ScreenCoord_u32 y, IEEE754_f32 val, const enum CSPrintNumberFormats format, _Bool includeSuffix);

int sprintf_int_with_commas(char* buf, int n);
size_t print_data_as_binary(const ScreenCoord_u32 x, const ScreenCoord_u32 y, void* data, size_t numBytes, RGBA32 color);
