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


/**
 * @brief Tries to extract a hexadecimal value from a char.
 * 
 * @param[out] dest The resulting hex value.
 * @param[in ] c    The char to check.
 * @return _Bool Whether the char was a valid hexadecimal value.
 */
static _Bool char_to_hex(Byte* dest, char c) {
    if (IS_NUMERIC(c)) {
        *dest = ((c - CHAR_NUMERIC_START) & BITMASK(BITS_PER_HEX));
    } else if (IS_UPPERCASE_HEX(c)) {
        *dest = (((c - CHAR_UPPERCASE_HEX_START) + 10) & BITMASK(BITS_PER_HEX));
    } else if (IS_LOWERCASE_HEX(c)) {
        *dest = (((c - CHAR_LOWERCASE_HEX_START) + 10) & BITMASK(BITS_PER_HEX));
    } else {
        return FALSE;
    }

    return TRUE;
}

/**
 * @brief Reads a hexadecimal value from a string buffer and writes it to a byte array.
 * 
 * @param[out] dest     The byte array to write to.
 * @param[in ] buf      The source string buffer.
 * @param[in ] index    The starting index of the source string buffer.
 * @param[in ] numBytes The number of bytes to write.
 * @return _Bool Whether the read/write was successful.
 */
static _Bool read_str_to_bytes(Byte dest[], const char* buf, u32 index, size_t numBytes) {
    for (u32 byteIndex = 0; byteIndex < numBytes; byteIndex++) {
        Byte retByte = 0x00;

        for (size_t digit = 0; digit < SIZEOF_HEX(Byte); digit++) {
            char glyph = buf[index];
            Byte hex = 0x0;

            if (glyph == CHAR_NULL) {
                return FALSE;
            }

            if (!char_to_hex(&hex, glyph)) {
                return FALSE;
            }

            retByte |= (hex << ((1 - digit) * BITS_PER_HEX));
            index++;
        }

        dest[byteIndex] = retByte;
    }

    return TRUE;
}

/**
 * @brief Checks whether a char is a control character with specific handling.
 * 
 * @param[in] c The char to check.
 * @return _Bool Whether the check was successful.
 */
static _Bool is_special_char(char c) {
    return (
        (c == CHAR_ESCAPE ) ||
        (c == CHAR_TAB    ) ||
        (c == CHAR_NEWLINE) ||
        (c == CHAR_RETURN ) ||
        (c == CHAR_COLOR  )
    );
}

/**
 * @brief Checks whether a char is a type of space.
 * 
 * @param[in] c The char to check.
 * @return _Bool Whether the check was successful.
 */
static _Bool is_space_char(char c) {
    return (
        (c == CHAR_NULL   ) ||
        (c == CHAR_TAB    ) ||
        (c == CHAR_NEWLINE) ||
        (c == CHAR_RETURN ) ||
        (c == CHAR_SPACE  )
    );
}

/**
 * @brief Formats a string into gCSPrintBuffer.
 * 
 * @param[in] buf       The original string.
 * @param[in] totalSize The size of the original string.
 * @return size_t The size of the resulting formatted string.
 */
static size_t cs_format_print_buffer(const char* buf, size_t totalSize) {
    size_t bufferCount = 0;
    ColorRGBA32 textColor = {
        .rgba32 = gCSDefaultPrintColor,
    };
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
                    //! TODO: Update this formatting when the ASCII PR is merged.
                    if ((index + SIZEOF_HEX(RGBA32)) >= totalSize) {
                        print = TRUE;
                        break;
                    }
                    // Only set 'color' if 'read_str_to_bytes' is successful.
                    ColorRGBA32 tempColor = {
                        .rgba32 = gCSDefaultPrintColor,
                    };
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

        // Write the PrintBuffer data for this char.
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

/**
 * @brief Gets the length of the next word (used for text wrapping).
 * 
 * @param[in] pBuf        The PrintBuffer (formatted string) pointer.
 * @param[in] index       The starting index in the formatted string.
 * @param[in] bufferCount The total number of chars in the string after formatting.
 * @return size_t The length of the next word. 0 if none were found.
 */
static size_t cs_get_next_word_length(PrintBuffer* pBuf, u32 index, size_t bufferCount) {
    size_t count = 0;

    while (index < bufferCount) {
        char glyph = pBuf[index].glyph;

        if (is_space_char(glyph)) {
            break;
        }

        index++;
        count++;
    }

    return count;
}

/**
 * @brief Checks whether the text at the x char position should wrap.
 * 
 * @param[in] x The x char position.
 * @return _Bool Whether to wrap the text.
 */
static _Bool cs_should_wrap(u32 x) {
    return (gCSWordWrap && (x >= gCSWordWrapXLimit));
}

/**
 * @brief Prints a formatted string buffer.
 * 
 * @param[in] x,y         The starting position on the screen to print to.
 * @param[in] bufferCount The total number of chars in the string after formatting.
 * @return size_t The total number of chars printed to the screen.
 */
static size_t cs_print_from_buffer(u32 x, u32 y, size_t bufferCount) {
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

            if (cs_should_wrap(x + TEXT_WIDTH(nextWordLength))) {
                newline = TRUE;
                tab = FALSE;
            }
        } else if (print) {
            if (cs_should_wrap(x)) {
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
            u32 tempX = x;
            u32 tabCount = (((x - startX) + TAB_WIDTH) / TAB_WIDTH);
            x = (tabCount * TAB_WIDTH) + startX;
            numChars += ((x - tempX) / TEXT_WIDTH(1));
        } else {
            x += TEXT_WIDTH(1);
            numChars++;
        }
    }

    return numChars;
}

/**
 * @brief Handles text scrolling.
 * 
 * @param[in] bufferCount The total number of chars in the string after formatting.
 * @param[in] charLimit   The maximum number of chars to print.
 */
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

/**
 * @brief General text printing function.
 * 
 * @param[in] x,y       The starting position on the screen to print to.
 * @param[in] charLimit The maximum number of chars to print.
 * @param[in] fmt       The string to print.
 * @return size_t The total number of chars printed to the screen.
 */
size_t cs_print_impl(u32 x, u32 y, size_t charLimit, const char* fmt, ...) {
    bzero(&sCSCharBuffer, sizeof(sCSCharBuffer));
    gCSNumLinesPrinted = 0;

    va_list args;
    va_start(args, fmt);

    size_t totalSize = _Printf(write_to_buf, sCSCharBuffer, fmt, args);
    ASSERTF((totalSize < (CHAR_BUFFER_SIZE - 1)), STR_COLOR_PREFIX"CRASH SCREEN PRINT BUFFER EXCEEDED", COLOR_RGBA32_RED);
    size_t numChars = 0;

    if (totalSize > 0) {
        // Clear Print Buffer.
        bzero(&gCSPrintBuffer, sizeof(gCSPrintBuffer));

        // Handle non-shaping formatting like color.
        size_t bufferCount = cs_format_print_buffer(sCSCharBuffer, totalSize);

        // Handle wrapping if applicable.
        if (0 < charLimit && charLimit < bufferCount) {
            if (cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_PRINT_SCROLL_SPEED) > 0) {
                cs_scroll_buffer(bufferCount, charLimit);
            }

            bufferCount = charLimit;
        }

        // Handle shaping formatting like tabs and wrapping.
        numChars = cs_print_from_buffer(x, y, bufferCount);
    }

    va_end(args);

    return numChars;
}

/**
 * @brief Formats a symbol name.
 * 
 * @param[in] x,y      The starting position on the screen to print to.
 * @param[in] maxWidth The maximum number of chars to print.
 * @param[in] color    The color of the printed text.
 * @param[in] fname    The symbol's name.
 */
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

/**
 * @brief Gets a name from a symbol and prints it.
 * 
 * @param[in] x,y      The starting position on the screen to print to.
 * @param[in] maxWidth The maximum number of chars to print.
 * @param[in] symbol   The symbol pointer.
 */
void cs_print_symbol_name(u32 x, u32 y, u32 maxWidth, const MapSymbol* symbol) {
    cs_print_symbol_name_impl(x, y, maxWidth,
        (((symbol != NULL) && is_in_code_segment(symbol->addr)) ? COLOR_RGBA32_CRASH_FUNCTION_NAME : COLOR_RGBA32_CRASH_VARIABLE),
        get_map_symbol_name(symbol)
    );
}
