#include <ultra64.h>

#include <stdarg.h>
#include <string.h>

#include "types.h"
#include "sm64.h"

#include "util/memory_read.h"
#include "util/registers.h"
#include "util/floats.h"
#include "cs_descriptions.h"
#include "cs_draw.h"
#include "cs_main.h"
#include "cs_settings.h"

#include "cs_print.h"

#include "game/printf.h"


static char sCSCharBuffer[CHAR_BUFFER_SIZE];
//! TODO: Can print buffer be reused as scroll buffer?
PrintBuffer gCSPrintBuffer[CS_PRINT_BUFFER_SIZE];
PrintBuffer gCSScrollBuffer[CS_SCROLL_BUFFER_SIZE];

// Input:
_Bool           gCSWordWrap          = FALSE;                    // Whether to wrap the text if it passes the x position defined by 'gCSWordWrapXLimit'.
ScreenCoord_u32 gCSWordWrapXLimit    = CRASH_SCREEN_TEXT_X2;     // The x position that causes the text to wrap when 'gCSWordWrap' is TRUE.
RGBA32          gCSDefaultPrintColor = CS_DEFAULT_PRINT_COLOR;   // The default text color. Use the macros in cs_print.h to change this.

// Output:
CSTextCoord_u32 gCSNumLinesPrinted = 0; // The number of lines printed by the last cs_print function.


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

            if (bufferCount > (CS_PRINT_BUFFER_SIZE - 1)) {
                break;
            }
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
static _Bool cs_should_wrap(ScreenCoord_u32 x) {
    return (gCSWordWrap && (x >= gCSWordWrapXLimit));
}

/**
 * @brief Prints a formatted string buffer.
 *
 * @param[in] x,y         The starting position on the screen to print to.
 * @param[in] bufferCount The total number of chars in the string after formatting.
 * @return size_t The total number of chars printed to the screen.
 */
static size_t cs_print_from_buffer(ScreenCoord_u32 x, ScreenCoord_u32 y, size_t bufferCount) {
    size_t numChars = 0;
    ScreenCoord_u32 startX = x;

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

        if (space && (index < (bufferCount - 1))) {
            CSTextCoord_u32 nextWordLength = cs_get_next_word_length(gCSPrintBuffer, (index + 1), bufferCount);

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
            if (y > (ScreenCoord_u32)gCSScissorBox.y2) {
                break;
            }
            gCSNumLinesPrinted++;
        } else if (tab) {
            ScreenCoord_u32 tempX = x;
            CSTextCoord_u32 tabCount = (((x - startX) + TAB_WIDTH) / TAB_WIDTH);
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
static void cs_scroll_buffer(size_t bufferCount, size_t charLimit, u32 scrollSpeed) {
    if (charLimit > (CS_SCROLL_BUFFER_SIZE - 1)) {
        charLimit = (CS_SCROLL_BUFFER_SIZE - 1);
    }
    bzero(&gCSScrollBuffer, sizeof(gCSScrollBuffer));

    const size_t offset = (scrollSpeed / CRASH_SCREEN_LETTER_WIDTH);
    const size_t size = (bufferCount + TEXT_SCROLL_NUM_SPACES);

    PrintBuffer* bufChar = &gCSScrollBuffer[0];

    for (size_t index = 0; index < bufferCount; index++) {
        *bufChar = gCSPrintBuffer[(index + offset) % size];

        if (bufChar->glyph == CHAR_NULL) {
            bufChar->glyph = CHAR_SPACE;
        }

        bufChar++;
    }

    bcopy(&gCSScrollBuffer, &gCSPrintBuffer, (charLimit * sizeof(PrintBuffer)));
}

/**
 * @brief General text printing function.
 *
 * @param[in] x,y       The starting position on the screen to print to.
 * @param[in] charLimit The maximum number of chars to print.
 * @param[in] fmt       The string to print.
 * @return size_t The total number of chars printed to the screen.
 */
size_t cs_print_impl(ScreenCoord_u32 x, ScreenCoord_u32 y, size_t charLimit, const char* fmt, ...) {
    bzero(&sCSCharBuffer, sizeof(sCSCharBuffer));
    gCSNumLinesPrinted = 1;

    va_list args;
    va_start(args, fmt);

    //! TODO: Is there a way to prevent this from writing outside the buffer?
    size_t totalSize = _Printf(write_to_buf, sCSCharBuffer, fmt, args);
    //! TODO: Do something visually to show that size was exceeded on any of the buffers.
    // ASSERTF((totalSize < (CHAR_BUFFER_SIZE - 1)), STR_COLOR_PREFIX"CRASH SCREEN PRINT BUFFER EXCEEDED", COLOR_RGBA32_RED);
    if (totalSize > (CHAR_BUFFER_SIZE - 1)) {
        sCSCharBuffer[CHAR_BUFFER_SIZE - 1] = '\0'; // Prevent writing outside the buffers with 'CHAR_BUFFER_SIZE' entries.
    }
    size_t numChars = 0;

    if (totalSize > 0) {
        // Clear Print Buffer.
        bzero(&gCSPrintBuffer, sizeof(gCSPrintBuffer));

        // Handle non-shaping formatting like color.
        size_t phase1FormattedSize = cs_format_print_buffer(sCSCharBuffer, totalSize);

        // Handle text scroll wrapping if applicable.
        ScreenCoord_u32 tx = x;
        CSScissorBox tempScissorBox = gCSScissorBox;
        size_t extraChar = 0;
        if (0 < charLimit && charLimit < phase1FormattedSize) {
            const SettingsType scrollSpeedSetting = cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_PRINT_SCROLL_SPEED);
            if (scrollSpeedSetting > 0) {
                cs_add_scissor_box(x, y, (x + TEXT_WIDTH(charLimit) - 1), (y + TEXT_HEIGHT(1)));
                // Print an extra character when scrolling to overlap the end of the scissor box.
                extraChar = 1;
                charLimit += extraChar;
                // Floats are used here to prevent overflow from directly multiplying gCSFrameCounter.
                //! TODO: Is the count factor thing correct?
                u32 scrollSpeed = ((f32)(gCSFrameCounter / gTimingDiv) * ((f32)scrollSpeedSetting / (f32)CRASH_SCREEN_LETTER_WIDTH));
                cs_scroll_buffer(phase1FormattedSize, charLimit, scrollSpeed);
                tx -= (scrollSpeed % CRASH_SCREEN_LETTER_WIDTH);
            }

            phase1FormattedSize = charLimit;
        }

        // Handle shaping formatting like tabs and wrapping, and draw the chars to the screen.
        numChars = cs_print_from_buffer(tx, y, phase1FormattedSize) - extraChar;
        gCSScissorBox = tempScissorBox;
    }

    va_end(args);

    return numChars;
}

/**
 * @brief Prints "UNKNOWN" in place of an unknown symbol name.
 *
 * @param[in] x,y The starting position on the screen to print to.
 * @return size_t The total number of chars printed to the screen.
 */
ALWAYS_INLINE static size_t cs_print_symbol_unknown(ScreenCoord_u32 x, ScreenCoord_u32 y) {
    // "UNKNOWN"
    return cs_print(x, y, STR_COLOR_PREFIX"UNKNOWN", COLOR_RGBA32_CRASH_UNKNOWN);
}

/**
 * @brief Gets the name of a symbol and prints it.
 *
 * @param[in] x,y          The starting position on the screen to print to.
 * @param[in] maxWidth     The maximum number of chars to print.
 * @param[in] symbol       The symbol pointer.
 * @param[in] printUnknown Whether to print "UNKNOWN" when no symbol name is found.
 * @return size_t The total number of chars printed to the screen.
 */
size_t cs_print_symbol_name(ScreenCoord_u32 x, ScreenCoord_u32 y, u32 maxWidth, const MapSymbol* symbol, _Bool printUnknown) {
    if (symbol == NULL) {
        return (printUnknown ? cs_print_symbol_unknown(x, y) : 0);
    }
    const char* name = get_map_symbol_name(symbol);
    if (name == NULL) {
        return (printUnknown ? cs_print_symbol_unknown(x, y) : 0);
    }
    return cs_print_scroll(x, y, maxWidth,
        STR_COLOR_PREFIX"%s",
        (symbol_is_function(symbol) ? COLOR_RGBA32_CRASH_FUNCTION_NAME : COLOR_RGBA32_CRASH_VARIABLE), name
    );
}

/**
 * @brief Prints information about an address location.
 *
 * @param[in] x,y         The starting position on the screen to print to.
 * @param[in] maxWidth    The maximum number of chars to print.
 * @param[in] addr        The address location.
 * @param[in] sureAddress Whether 'addr' is guaranteed to be a memory address.
 * @return size_t The total number of chars printed to the screen.
 */
size_t cs_print_addr_location_info(ScreenCoord_u32 x, ScreenCoord_u32 y, u32 maxWidth, Address addr, _Bool sureAddress) {
    if (sureAddress && (addr == (Address)NULL)) {
        return cs_print(x, y, STR_COLOR_PREFIX"NULL", COLOR_RGBA32_VSC_DEFINE);
    }

    if (IS_DEBUG_MAP_ENABLED() && cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_SYMBOL_NAMES)) {
        const MapSymbol* symbol = get_map_symbol(addr, SYMBOL_SEARCH_BACKWARD);
        if (symbol != NULL) {
            size_t charX = cs_print_symbol_name(x, y, (maxWidth - STRLEN(" +0000")), symbol, sureAddress);
            charX += cs_print((x + TEXT_WIDTH(charX + 1)), y, (STR_COLOR_PREFIX"+"STR_HEX_HALFWORD), COLOR_RGBA32_CRASH_OFFSET, (addr - symbol->addr));
            return charX;
        }
    }

    if (sureAddress) {
        const char* memStr = get_hardcoded_memory_str(addr);
        if (memStr != NULL) {
            return cs_print_scroll(x, y, maxWidth, STR_COLOR_PREFIX"%s", COLOR_RGBA32_LIGHT_GRAY, memStr);
        } else {
            memStr = get_memory_string_from_addr(addr);
            if (memStr != NULL) {
                return cs_print_scroll(x, y, maxWidth, STR_COLOR_PREFIX"%s", COLOR_RGBA32_GRAY, memStr);
            } else {
                return cs_print_symbol_unknown(x, y);
            }
        }
    }

    return 0;
}

static const FloatErrorPrintFormat sFltErrFmt[] = {
    [FLT_ERR_NONE  ] = { .r = 0xFF, .g = 0xFF, .b = 0xFF, .prefixChar = CHAR_FLT_PREFIX_NULL,   .suffix = "",             },
    [FLT_ERR_DENORM] = { .r = 0xFF, .g = 0x9F, .b = 0x9F, .prefixChar = CHAR_FLT_PREFIX_DENORM, .suffix = "denormalized", },
    [FLT_ERR_NAN   ] = { .r = 0xFF, .g = 0x7F, .b = 0x7F, .prefixChar = CHAR_FLT_PREFIX_NAN,    .suffix = "NaN",          },
};

size_t cs_print_f32(ScreenCoord_u32 x, ScreenCoord_u32 y, IEEE754_f32 val, const CSPrintNumberFormats format, _Bool includeSuffix) {
    const FloatError fltErrType = validate_f32(val);
    size_t numChars = 0;

    if (fltErrType != FLT_ERR_NONE) {
        const FloatErrorPrintFormat* p = &sFltErrFmt[fltErrType];
        RGBA32 color = RGBA_TO_RGBA32(p->r, p->g, p->b, MSK_RGBA32_A);

        if (includeSuffix) {
            // "[XXXXXXXX] ([suffix])"
            numChars += cs_print(x, y, (STR_COLOR_PREFIX STR_HEX_WORD" (%s)"), color, val.asU32, p->suffix);
        } else {
            // "[prefix][XXXXXXXX]"
            numChars += cs_print(x, y, (STR_COLOR_PREFIX"%c"STR_HEX_WORD), color, p->prefixChar, val.asU32);
        }
    } else {
        switch (format) {
            case PRINT_NUM_FMT_HEX: numChars += cs_print(x, y, (" "STR_HEX_WORD), val.asU32); break; // "[XXXXXXXX]"
            default:
            case PRINT_NUM_FMT_DEC: numChars += cs_print(x, y, "% g",   val.asF32); break; // "[±][exponent]"
            case PRINT_NUM_FMT_SCI: numChars += cs_print(x, y, "% .3e", val.asF32); break; // "[scientific notation]"
        }
    }

    return numChars;
}

//! TODO:
// size_t cs_print_f64(u32 x, u32 y, IEEE754_f64 val, const enum CSPrintNumberFormats format, _Bool includeSuffix) {
//     const FloatError fltErrType = validate_f64(val);
//     size_t numChars = 0;

//     return numChars;
// }

int sprintf_int_with_commas(char* buf, int n) {
    char* p = buf;
    int n2 = 0;
    int scale = 1;

    if (n < 0) {
        p += sprintf(p, "-");
        n = -n;
    }
    while (n >= 1000) {
        n2 += (scale * (n % 1000));
        n /= 1000;
        scale *= 1000;
    }
    p += sprintf(p, "%d", n);
    while (scale != 1) {
        scale /= 1000;
        n = (n2 / scale);
        n2 = (n2 % scale);
        p += sprintf(p, ",%03d", n);
    }

    return (p - buf);
}

// Big Endian.
size_t print_data_as_binary(const ScreenCoord_u32 x, const ScreenCoord_u32 y, void* data, size_t numBytes, RGBA32 color) { //! TODO: make this a custom formatting specifier?, maybe \%b?
    CSTextCoord_u32 numChars = 0;
    Byte* dataPtr = (Byte*)data;

    for (size_t byte = 0; byte < numBytes; byte++){
        for (size_t bit = 0; bit < BITS_PER_BYTE; bit++) {
            char c = (((*dataPtr >> ((BITS_PER_BYTE - 1) - bit)) & 0b1) ? '1' : '0');

            cs_draw_glyph((x + TEXT_WIDTH(numChars)), y, c, color);

            numChars++;
        }

        dataPtr++;
        numChars++;
    }

    return numChars;
}
