#include <ultra64.h>

#include <stdarg.h>
#include <string.h>

#include "types.h"
#include "sm64.h"

#include "crash_draw.h"
#include "crash_main.h"
#include "crash_settings.h"

#include "crash_print.h"

#include "game/printf.h"


static char sCSCharBuffer[CHAR_BUFFER_SIZE];
PrintBuffer gCSPrintBuffer[CHAR_BUFFER_SIZE];
PrintBuffer gCSScrollBuffer[CHAR_BUFFER_SIZE];

// Input:
_Bool  gCSWordWrap          = FALSE;
u32    gCSWordWrapXLimit    = CRASH_SCREEN_TEXT_X2;
RGBA32 gCSDefaultPrintColor = COLOR_RGBA32_WHITE;

// Output:
u32 gCSNumLinesPrinted = 0;


static _Bool glyph_to_hex(char* dest, char glyph) {
    if (IS_NUMERIC(glyph)) {
        *dest = ((glyph - CHAR_NUMERIC_START) & BITMASK(BITS_PER_HEX));
    } else if (IS_UPPERCASE_HEX(glyph)) {
        *dest = (((glyph - CHAR_UPPERCASE_HEX_START) + 10) & BITMASK(BITS_PER_HEX));
    } else if (IS_LOWERCASE_HEX(glyph)) {
        *dest = (((glyph - CHAR_LOWERCASE_HEX_START) + 10) & BITMASK(BITS_PER_HEX));
    } else {
        return FALSE;
    }

    return TRUE;
}

static _Bool read_str_to_bytes(Byte dest[], const char* buf, u32 index, size_t numBytes) {
    for (u32 byteIndex = 0; byteIndex < numBytes; byteIndex++) {
        Byte retByte = 0x00;

        for (size_t digit = 0; digit < SIZEOF_HEX(Byte); digit++) {
            char glyph = buf[index];
            char hex = 0x0;

            if (glyph == CHAR_NULL) {
                return FALSE;
            }

            if (!glyph_to_hex(&hex, glyph)) {
                return FALSE;
            }

            retByte |= (hex << ((1 - digit) * BITS_PER_HEX));
            index++;
        }

        dest[byteIndex] = retByte;
    }

    return TRUE;
}

static _Bool is_special_char(char glyph) {
    return (
        (glyph == CHAR_ESCAPE ) ||
        (glyph == CHAR_TAB    ) ||
        (glyph == CHAR_NEWLINE) ||
        (glyph == CHAR_RETURN ) ||
        (glyph == CHAR_COLOR  )
    );
}

static _Bool is_space_char(char glyph) {
    return (
        (glyph == CHAR_NULL   ) ||
        (glyph == CHAR_TAB    ) ||
        (glyph == CHAR_NEWLINE) ||
        (glyph == CHAR_RETURN ) ||
        (glyph == CHAR_SPACE  )
    );
}

static u32 cs_format_print_buffer(const char* buf, size_t totalSize) {
    u32 bufferCount = 0;
    ColorRGBA32 textColor = { .rgba32 = gCSDefaultPrintColor };
    _Bool escaped = FALSE;

    // Pass 1: control characters and formatting
    for (u32 index = 0; index < totalSize; index++) {
        PrintBuffer* data = &gCSPrintBuffer[bufferCount];
        _Bool print = FALSE;
        char glyph = buf[index];

        if (glyph == CHAR_NULL) {
            break;
        }

        // Handle special characters.
        if (escaped) {
            print = TRUE;
            data->isEscaped = TRUE;
            escaped = FALSE;
        } else {
            switch (glyph) {
                case CHAR_ESCAPE:
                    if ((index + 1) >= totalSize) {
                        print = TRUE;
                        break;
                    }
                    if (!is_special_char(buf[index + 1])) {
                        print = TRUE;
                        break;
                    }
                    escaped = TRUE;
                    break;
                case CHAR_COLOR: // @RRGGBBAA
                    if ((index + SIZEOF_HEX(RGBA32)) >= totalSize) {
                        print = TRUE;
                        break;
                    }
                    // Only set 'color' if 'read_str_to_bytes' is successful.
                    ColorRGBA32 tempColor = { .rgba32 = gCSDefaultPrintColor };
                    if (!read_str_to_bytes(tempColor.raw.asU8, buf, (index + 1), sizeof(tempColor.raw.asU8))) {
                        print = TRUE;
                        break;
                    }
                    textColor.rgba32 = tempColor.rgba32;
                    index += SIZEOF_HEX(RGBA32);
                    break;
                default:
                    print = TRUE;
                    break;
            }
        }

        // Write the print data for this char.
        if (print) {
            data->red   = C32_TO_C16(textColor.red  );
            data->green = C32_TO_C16(textColor.green);
            data->blue  = C32_TO_C16(textColor.blue );
            data->alpha = textColor.alpha;
            data->glyph = glyph;

            bufferCount++;
        }
    }

    return bufferCount;
}

static size_t cs_get_next_word_length(PrintBuffer* buf, u32 index, size_t bufferCount) {
    size_t count = 0;

    while (index < bufferCount) {
        char glyph = buf[index].glyph;

        if (is_space_char(glyph)) {
            break;
        }

        index++;
        count++;
    }

    return count;
}

static _Bool cs_can_wrap(u32 x) {
    return (gCSWordWrap && (x >= gCSWordWrapXLimit));
}

static size_t cs_print_from_buffer(size_t bufferCount, u32 x, u32 y) {
    size_t numChars = 0;
    u32 startX = x;

    // Pass 3: whitespace, newlines, and print
    for (size_t index = 0; index < bufferCount; index++) {
        PrintBuffer* data = &gCSPrintBuffer[index];
        char glyph = data->glyph;
        _Bool print = FALSE;
        _Bool newline = FALSE;
        _Bool space = FALSE;
        _Bool tab = FALSE;

        switch (glyph) {
            case CHAR_TAB:
                if (data->isEscaped) {
                    print = TRUE;
                } else {
                    space = TRUE;
                    tab = TRUE;
                }
                break;
            case CHAR_NEWLINE:
            case CHAR_RETURN:
                if (data->isEscaped) {
                    print = TRUE;
                } else {
                    newline = TRUE;
                }
                break;
            case CHAR_SPACE:
                space = TRUE;
                break;
            default:
                print = TRUE;
                break;
        }

        if (space && index < (bufferCount - 1)) {
            size_t nextWordLength = cs_get_next_word_length(gCSPrintBuffer, (index + 1), bufferCount);

            if (cs_can_wrap(x + TEXT_WIDTH(nextWordLength))) {
                newline = TRUE;
                tab = FALSE;
            }
        } else if (print) {
            if (cs_can_wrap(x)) {
                newline = TRUE;
                index--;
            } else {
                cs_draw_glyph(x, y,
                    data->glyph,
                    RGBA_TO_RGBA32(
                        C16_TO_C32(data->red  ),
                        C16_TO_C32(data->green),
                        C16_TO_C32(data->blue ),
                        data->alpha
                    )
                );
            }
        }

        if (newline) {
            x = startX;
            y += TEXT_HEIGHT(1);
            if (y > (u32)gCSScissorBox.y2) {
                break;
            }
            gCSNumLinesPrinted++;
        } else if (tab) {
            int tabCount = (((x - startX) + TAB_WIDTH) / TAB_WIDTH);
            numChars += (tabCount * TAB_WIDTH) - x;
            x = (tabCount * TAB_WIDTH) + startX;
        } else {
            x += TEXT_WIDTH(1);
            numChars++;
        }
    }

    return numChars;
}

static void cs_scroll_buffer(size_t bufferCount, size_t charLimit) {
    bzero(&gCSScrollBuffer, sizeof(gCSScrollBuffer));

    const SettingsType scrollSpeed = cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_PRINT_SCROLL_SPEED);
    const size_t offset = (CYCLES_TO_FRAMES(osGetTime()) >> (5 - scrollSpeed));
    const size_t size = (bufferCount + TEXT_SCROLL_NUM_SPACES);

    PrintBuffer* bufChar = &gCSScrollBuffer[0];

    for (size_t index = 0; index < bufferCount; index++) {
        *bufChar = gCSPrintBuffer[(index + offset) % size];

        if (bufChar->glyph == CHAR_NULL) {
            bufChar->glyph = CHAR_SPACE;
        }

        bufChar++;
    }

    memcpy(&gCSPrintBuffer, &gCSScrollBuffer, (charLimit * sizeof(PrintBuffer)));
}

size_t cs_print_impl(u32 x, u32 y, size_t charLimit, const char* fmt, ...) {
    bzero(&sCSCharBuffer, sizeof(sCSCharBuffer));
    gCSNumLinesPrinted = 0;

    va_list args;
    va_start(args, fmt);

    size_t totalSize = _Printf(write_to_buf, sCSCharBuffer, fmt, args);
    ASSERTF((totalSize < (CHAR_BUFFER_SIZE - 1)), STR_COLOR_PREFIX"CRASH SCREEN PRINT BUFFER EXCEEDED", COLOR_RGBA32_RED);
    size_t numChars = 0;

    if (totalSize > 0) {
        bzero(&gCSPrintBuffer, sizeof(gCSPrintBuffer));

        size_t bufferCount = cs_format_print_buffer(sCSCharBuffer, totalSize);

        if (0 < charLimit && charLimit < bufferCount) {
            if (cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_PRINT_SCROLL_SPEED) > 0) {
                cs_scroll_buffer(bufferCount, charLimit);
            }

            bufferCount = charLimit;
        }

        numChars = cs_print_from_buffer(bufferCount, x, y);
    }

    va_end(args);

    return numChars;
}

void cs_print_symbol_name_impl(u32 x, u32 y, u32 maxWidth, RGBA32 color, const char* fname) {
    if (fname == NULL) {
        // "UNKNOWN"
        cs_print(x, y, STR_COLOR_PREFIX"UNKNOWN", COLOR_RGBA32_CRASH_UNKNOWN);
    } else {
        // "[name from map data]"
        cs_print_scroll(x, y, maxWidth,
            STR_COLOR_PREFIX"%s",
            color, fname
        );
    }
}

void cs_print_symbol_name(u32 x, u32 y, u32 maxWidth, const MapSymbol* symbol) {
    cs_print_symbol_name_impl(x, y, maxWidth,
        (((symbol != NULL) && is_in_code_segment(symbol->addr)) ? COLOR_RGBA32_CRASH_FUNCTION_NAME : COLOR_RGBA32_CRASH_VARIABLE),
        get_map_symbol_name(symbol)
    );
}
