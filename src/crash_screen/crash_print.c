#include <ultra64.h>

#include <stdarg.h>
#include <string.h>
#include "types.h"
#include "sm64.h"
#include "crash_main.h"
#include "crash_print.h"
#include "game/printf.h"


PrintBuffer gCSPrintBuffer[CHAR_BUFFER_SIZE];
PrintBuffer gCSScrollBuffer[CHAR_BUFFER_SIZE];

_Bool gCSWordWrap = FALSE;

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
        (glyph == CHAR_NEWLINE) ||
        (glyph == CHAR_RETURN ) ||
        (glyph == CHAR_COLOR  )
    );
}

static u32 format_print_buffer(const char* buf, size_t totalSize) {
    u32 bufferCount = 0;
    RGBA32 color = COLOR_RGBA32_WHITE; // Initial color.
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
                    //! TODO: ColorRGBA/u32 union
                    ColorRGBA tempColor; // Only set 'color' if 'read_str_to_bytes' is successful.
                    if (!read_str_to_bytes(tempColor, buf, (index + 1), sizeof(color))) {
                        print = TRUE;
                        break;
                    }
                    color = *(u32*)tempColor;
                    // color = COLORRGBA_TO_RGBA16(tempColor);
                    index += SIZEOF_HEX(RGBA32);
                    break;
                default:
                    print = TRUE;
                    break;
            }
        }

        if (print) {
            data->color = color;
            data->glyph = glyph;
            bufferCount++;
        }
    }

    return bufferCount;
}

static u32 get_next_word_length(PrintBuffer* buf, u32 index, size_t size) {
    u32 count = 0;

    while (index < size) {
        char glyph = buf[index].glyph;
        if (
            (glyph == CHAR_NULL   ) ||
            (glyph == CHAR_SPACE  ) ||
            (glyph == CHAR_NEWLINE) ||
            (glyph == CHAR_RETURN )
        ) {
            break;
        }

        index++;
        count++;
    }

    return count;
}

static _Bool can_wrap(u32 x) {
    return (gCSWordWrap && (x >= CRASH_SCREEN_TEXT_X2));
}

static size_t print_from_buffer(size_t bufferCount, u32 x, u32 y) {
    size_t numChars = 0;
    u32 startX = x;

    // Pass 3: whitespace, newlines, and print
    for (size_t index = 0; index < bufferCount; index++) {
        _Bool print = FALSE;
        _Bool newline = FALSE;
        PrintBuffer* data = &gCSPrintBuffer[index];
        char glyph = data->glyph;

        switch (glyph) {
            case CHAR_NEWLINE:
            case CHAR_RETURN:
                if (data->isEscaped) {
                    print = TRUE;
                } else {
                    newline = TRUE;
                }
                break;
            case CHAR_SPACE:
                if (can_wrap(x + TEXT_WIDTH(get_next_word_length(data, index, bufferCount)))) {
                    newline = TRUE;
                }
                break;
            default:
                print = TRUE;
                break;
        }

        if (print) {
            if (can_wrap(x)) {
                newline = TRUE;
                index--;
            } else {
                crash_screen_draw_glyph(x, y, data->glyph, data->color);
            }
        }

        if (newline) {
            x = startX;
            y += TEXT_HEIGHT(1);
            if (y > (u32)gCSScissorBox.y2) {
                break;
            }
            gCSNumLinesPrinted++;
        } else {
            x += TEXT_WIDTH(1);
            numChars++;
        }
    }

    return numChars;
}

static void scroll_buffer(size_t bufferCount, size_t charLimit) {
    bzero(&gCSScrollBuffer, sizeof(gCSScrollBuffer));

    size_t offset = (CYCLES_TO_FRAMES(osGetTime()) >> (5 - gCSSettings[CS_OPT_PRINT_SCROLL_SPEED].val));
    size_t size = (bufferCount + TEXT_SCROLL_NUM_SPACES);

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

static char* write_to_buf(char* buffer, const char* data, size_t size) {
    return ((char*)memcpy(buffer, data, size) + size);
}

size_t crash_screen_print_impl(u32 x, u32 y, size_t charLimit, const char* fmt, ...) {
    char buf[CHAR_BUFFER_SIZE] = "";
    bzero(&buf, sizeof(buf));
    gCSNumLinesPrinted = 0;

    va_list args;
    va_start(args, fmt);

    size_t totalSize = _Printf(write_to_buf, buf, fmt, args);
    ASSERT((totalSize < CHAR_BUFFER_SIZE), "@FF0000FFCRASH SCREEN PRINT BUFFER EXCEEDED");
    size_t numChars = 0;

    if (totalSize > 0) {
        bzero(&gCSPrintBuffer, sizeof(gCSPrintBuffer));

        size_t bufferCount = format_print_buffer(buf, totalSize);

        if (0 < charLimit && charLimit < bufferCount) {
            if (gCSSettings[CS_OPT_PRINT_SCROLL_SPEED].val > 0) {
                scroll_buffer(bufferCount, charLimit);
            }

            bufferCount = charLimit;
        }

        numChars = print_from_buffer(bufferCount, x, y);
    }

    va_end(args);

    return numChars;
}

void crash_screen_print_symbol_name_impl(u32 x, u32 y, u32 maxWidth, RGBA32 color, const char* fname) {
    if (fname == NULL) {
        // "UNKNOWN"
        crash_screen_print(x, y, STR_COLOR_PREFIX"UNKNOWN", COLOR_RGBA32_CRASH_UNKNOWN);
    } else {
        // "[name from map data]"
        crash_screen_print_scroll(x, y, maxWidth,
            STR_COLOR_PREFIX"%s",
            color, fname
        );
    }
}

void crash_screen_print_symbol_name(u32 x, u32 y, u32 maxWidth, const struct MapSymbol* symbol) {
    crash_screen_print_symbol_name_impl(x, y, maxWidth,
        ((symbol != NULL && is_in_code_segment(symbol->addr)) ? COLOR_RGBA32_CRASH_FUNCTION_NAME : COLOR_RGBA32_CRASH_VARIABLE),
        get_map_symbol_name(symbol)
    );
}
