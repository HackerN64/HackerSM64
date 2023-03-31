#include <ultra64.h>

#include <stdarg.h>
#include <string.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen.h"
#include "crash_draw.h"
#include "crash_print.h"
#include "buffers/framebuffers.h"
#include "buffers/zbuffer.h"
#include "engine/colors.h"
#include "game/printf.h"


PrintBuffer gCrashScreenPrintBuffer[CHAR_BUFFER_SIZE];
PrintBuffer gCrashScreenScrollBuffer[CHAR_BUFFER_SIZE];

_Bool gCrashScreenWordWrap = TRUE;


static _Bool glyph_to_hex(char *dest, unsigned char glyph) {
    if (IS_NUMERIC(glyph)) {
        *dest = ((glyph - CHAR_NUMERIC_START) & BITMASK(4));
    } else if (IS_UPPERCASE_HEX(glyph)) {
        *dest = (((glyph - CHAR_UPPERCASE_HEX_START) + 10) & BITMASK(4));
    } else if (IS_LOWERCASE_HEX(glyph)) {
        *dest = (((glyph - CHAR_LOWERCASE_HEX_START) + 10) & BITMASK(4));
    } else {
        return FALSE;
    }

    return TRUE;
}

static _Bool read_str_to_bytes(u8 dest[], const char *buf, u32 index, size_t numBytes) {
    for (u32 byteIndex = 0; byteIndex < numBytes; byteIndex++) {
        u8 retByte = 0x00;

        for (int digit = 0; digit < 2; digit++) {
            unsigned char glyph = buf[index];
            char hex = 0x0;

            if (glyph == CHAR_NULL) {
                return FALSE;
            }

            if (!glyph_to_hex(&hex, glyph)) {
                return FALSE;
            }

            retByte |= (hex << ((1 - digit) * 4));
            index++;
        }

        dest[byteIndex] = retByte;
    }

    return TRUE;
}

static _Bool is_special_char(unsigned char glyph) {
    return (
        glyph == CHAR_ESCAPE  ||
        glyph == CHAR_NEWLINE ||
        glyph == CHAR_RETURN  ||
        glyph == CHAR_COLOR
    );
}

static u32 format_print_buffer(const char *buf, size_t totalSize) {
    u32 bufferCount = 0;
    // uRGBA32 color = { .raw32 = COLOR_RGBA32_WHITE }; // Initial color
    RGBA32 color = COLOR_RGBA32_WHITE;
    _Bool escaped = FALSE;

    // Pass 1: control characters and formatting
    for (u32 index = 0; index < totalSize; index++) {
        PrintBuffer *data = &gCrashScreenPrintBuffer[bufferCount];
        _Bool print = FALSE;
        unsigned char glyph = buf[index];

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
                    if ((index + 8) >= totalSize) {
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
                    index += 8;
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

static u32 get_next_word_length(PrintBuffer *buf, u32 index, size_t size) {
    u32 count = 0;

    while (index < size) {
        unsigned char glyph = buf[index].glyph;
        if (glyph == CHAR_NULL
         || glyph == CHAR_SPACE
         || glyph == CHAR_NEWLINE
         || glyph == CHAR_RETURN) {
            break;
        }

        index++;
        count++;
    }

    return count;
}

static u32 print_from_buffer(size_t bufferCount, u32 x, u32 y) {
    u32 startX = x;
    u32 numLines = 1;

    // Pass 3: whitespace, newlines, and print
    for (u32 index = 0; index < bufferCount; index++) {
        _Bool print = FALSE;
        _Bool newline = FALSE;
        PrintBuffer *data = &gCrashScreenPrintBuffer[index];
        unsigned char glyph = data->glyph;

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
                if ((x + TEXT_WIDTH(get_next_word_length(data, index, bufferCount))) >= CRASH_SCREEN_TEXT_X2) {
                    newline = TRUE;
                }
                break;
            default:
                print = TRUE;
                break;
        }

        if (print) {
            if (gCrashScreenWordWrap && (x >= CRASH_SCREEN_TEXT_X2)) {
                newline = TRUE;
                index--;
            } else {
                crash_screen_draw_glyph(x, y, data->glyph, data->color);
            }
        }

        if (gCrashScreenWordWrap && newline) {
            x = startX;
            y += TEXT_HEIGHT(1);
            numLines++;
        } else {
            x += TEXT_WIDTH(1);
        }
    }

    return numLines;
}

static void scroll_buffer(u32 bufferCount, u32 charLimit) {
    bzero(&gCrashScreenScrollBuffer, sizeof(gCrashScreenScrollBuffer));

    u32 offset = (CYCLES_TO_FRAMES(osGetTime()) >> 3);
    u32 size = (bufferCount + TEXT_SCROLL_NUM_SPACES);

    for (u32 index = 0; index < bufferCount; index++) {
        gCrashScreenScrollBuffer[index] = gCrashScreenPrintBuffer[((index + offset) % size)];
        if (gCrashScreenScrollBuffer[index].glyph == CHAR_NULL) {
            gCrashScreenScrollBuffer[index].glyph = CHAR_SPACE;
        }
    }

    memcpy(&gCrashScreenPrintBuffer, &gCrashScreenScrollBuffer, (charLimit * sizeof(PrintBuffer)));

    gCrashScreenUpdateFramebuffer = TRUE;
}

static char *write_to_buf(char *buffer, const char *data, size_t size) {
    return ((char *) memcpy(buffer, data, size) + size);
}

u32 crash_screen_print_impl(u32 x, u32 y, u32 charLimit, const char *fmt, ...) {
    char buf[CHAR_BUFFER_SIZE] = "";
    bzero(&buf, sizeof(buf));

    va_list args;
    va_start(args, fmt);

    size_t totalSize = _Printf(write_to_buf, buf, fmt, args);
    u32 numLines = 0;

    if (totalSize > 0) {
        bzero(&gCrashScreenPrintBuffer, sizeof(gCrashScreenPrintBuffer));

        size_t bufferCount = format_print_buffer(buf, totalSize);

        if (0 < charLimit && charLimit < bufferCount) {
            scroll_buffer(bufferCount, charLimit);
            bufferCount = charLimit;
        }

        numLines = print_from_buffer(bufferCount, x, y);
    }

    va_end(args);

    return numLines;
}
