#include <ultra64.h>

#include <stdarg.h>
#include <string.h>
#include "types.h"
#include "sm64.h"
#include "crash_screen.h"
#include "crash_screen_draw.h"
#include "crash_screen_print.h"
#include "buffers/framebuffers.h"
#include "buffers/zbuffer.h"
#include "engine/colors.h"
#include "game/printf.h"


PrintCommand gCrashScreenPrintBuffer[CHAR_BUFFER_SIZE];

s8 gCrashScreenWordWrap = TRUE;


void crash_screen_parse_formatting(PrintCommand *cmd, const char *buf, u32 index, u32 size, RGBA32 *color, u32 x, u32 y, u8* printEscape, s32 isMain);


void crash_screen_format_space(PrintCommand *cmd, const char *buf, u32 index, u32 size, u32 x, u32 y) {
    PrintCommand cmd2 = { .raw = 0 };
    UNUSED RGBA32 checkColor = COLOR_RGBA32_WHITE;

    // Parse space:

    // Count the number of spaces
    u32 numSpaces = 0;
    while (buf[index++] == CHAR_SPACE) {
        if (index > size) {
            break;
        }

        numSpaces++;
    }

    // index is now the first non-space
    x += TEXT_WIDTH(numSpaces);
    if (x >= CRASH_SCREEN_TEXT_X2) {
        cmd->newLine = TRUE;
        cmd->skip = numSpaces;
        return;
    }

    u32 remainingSize = (size - index);
    u32 checkX = x;
    u32 checkY = y;
    u8 printEscape2 = FALSE;
    if (remainingSize > 0) {
        for (u32 i = 0; i < remainingSize; i += (1 + cmd2.skip)) {
            if (buf[index + i] == CHAR_SPACE) {
                break;
            }

            crash_screen_parse_formatting(&cmd2, buf, (index + i), size, &checkColor, checkX, checkY, &printEscape2, FALSE);

            if (cmd2.newLine) {
                break;
            }

            checkX += TEXT_WIDTH(cmd2.shift);
        }
    }

    if (gCrashScreenWordWrap && (checkX >= CRASH_SCREEN_TEXT_X2)) {
        cmd->newLine = TRUE;
    } else {
        cmd->shift = (1 + numSpaces);
    }

    cmd->skip = numSpaces;
}

static s32 glyph_to_hex(char *dest, unsigned char glyph) {
    if (IS_NUMERIC(glyph)) {
        *dest = ((glyph - '0') & BITMASK(4));
    } else if (IS_UPPERCASE(glyph)) {
        *dest = (((glyph - 'A') + 10) & BITMASK(4));
    } else if (IS_LOWERCASE(glyph)) {
        *dest = (((glyph - 'a') + 10) & BITMASK(4));
    } else {
        return FALSE;
    }

    return TRUE;
}

static s32 read_str_to_bytes(u8 dest[], const char *buf, u32 index, size_t numBytes) {
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


void crash_screen_format_text_color(PrintCommand *cmd, const char *buf, u32 index, size_t size, RGBA32 *color) {
    ColorRGBA rgba = { 0x00, 0x00, 0x00, 0x00 };
    *color = COLOR_RGBA32_WHITE;
    const u32 numCharsToRead = (sizeof(RGBA32) * 2);

    if ((index + numCharsToRead) > size) {
        return;
    }

    if (!read_str_to_bytes(rgba, buf, index, sizeof(RGBA32))) {
        return;
    }
    index += numCharsToRead;

    *color = COLORRGBA_TO_RGBA32(rgba);
    cmd->skip = numCharsToRead;
}

void crash_screen_format_escape(PrintCommand *cmd, const char *buf, u32 index, u8* printEscape) {
    unsigned char glyph = buf[index];

    if (!*printEscape
     && glyph != CHAR_NULL
     && (glyph == CHAR_NEWLINE
      || glyph == CHAR_RETURN
      || glyph == CHAR_COLOR
      || glyph == CHAR_SCROLL
      || glyph == CHAR_ESCAPE)) {
        *printEscape = TRUE;
    } else {
        cmd->print = TRUE;
        cmd->shift = 1;
    }
}

void crash_screen_format_local_scroll(PrintCommand *cmd, const char *buf, u32 index, u32 size, RGBA32 *color, u32 x, u32 y, s32 isMain) {
    unsigned char glyph = buf[index];
    const size_t numCharsToRead = (sizeof(u8) * 2);

    if ((index + numCharsToRead) > size) {
        return;
    }

    u8 maxNumChars = 0x00;

    if (!read_str_to_bytes(&maxNumChars, buf, index, sizeof(u8))) {
        return;
    }
    index += numCharsToRead;

    maxNumChars = MIN(maxNumChars, (size - index));

    if ((index + maxNumChars) > size) {
        return;
    }

    u8 numChars = 0;

    // Count how many total characters will need to be scrolled if they exceed the limit.
    while (glyph != CHAR_NULL && glyph != CHAR_SPACE) {
        numChars++;
        glyph = buf[index + numChars];
    }

    if (isMain && numChars > 0) {
        if (numChars > maxNumChars) {
            // Scroll text
            u32 offset = (CYCLES_TO_FRAMES(osGetTime()) >> 3);
            for (u32 i = 0; i < maxNumChars; i++) {
                glyph = buf[index + ((i + offset) % (numChars + TEXT_SCROLL_NUM_SPACES))];
                if (glyph != CHAR_NULL) {
                    crash_screen_draw_glyph(x, y, glyph, *color);
                }

                x += TEXT_WIDTH(1);
            }

            gCrashScreenQueueFramebufferUpdate = TRUE;
        } else {
            // Print "normally"
            for (u32 i = 0; i < numChars; i++) {
                glyph = buf[index + i];
                if (glyph != CHAR_NULL) {
                    crash_screen_draw_glyph(x, y, glyph, *color);
                }

                x += TEXT_WIDTH(1);
            }
        }
    }

    cmd->shift = cmd->skip = (numCharsToRead + numChars);
}

void crash_screen_format_normal_glyph(PrintCommand *cmd, u32 index, u32 size, u32 x) {
    cmd->print = TRUE;
    if (gCrashScreenWordWrap && (index < size) && ((x + TEXT_WIDTH(1)) >= CRASH_SCREEN_TEXT_X2)) {
        cmd->newLine = TRUE;
    } else {
        cmd->shift = 1;
    }
}

void crash_screen_parse_formatting(PrintCommand *cmd, const char *buf, u32 index, u32 size, RGBA32 *color, u32 x, u32 y, u8 *printEscape, s32 isMain) {
    unsigned char glyph = buf[index++];

    if (*printEscape) {
        *printEscape = FALSE;
        crash_screen_format_normal_glyph(cmd, index, size, x);
    } else {
        switch (glyph) {
            case CHAR_NEWLINE:
            case CHAR_RETURN:
                cmd->newLine = TRUE;
                break;
            case CHAR_SPACE:
                crash_screen_format_space(cmd, buf, index, size, x, y);
                break;
            case CHAR_COLOR: // @RRGGBBAA color prefix
                crash_screen_format_text_color(cmd, buf, index, size, color);
                break;
            case CHAR_ESCAPE:
                crash_screen_format_escape(cmd, buf, index, printEscape);
                break;
            case CHAR_SCROLL:
                crash_screen_format_local_scroll(cmd, buf, index, size, color, x, y, isMain);
                break;
            default:
                crash_screen_format_normal_glyph(cmd, index, size, x);
                break;
        }
    }

    if (glyph != CHAR_ESCAPE) {
        *printEscape = FALSE;
    }
}

static char *write_to_buf(char *buffer, const char *data, size_t size) {
    return ((char *) memcpy(buffer, data, size) + size);
}

u32 crash_screen_print(u32 startX, u32 startY, const char *fmt, ...) {
    char buf[CHAR_BUFFER_SIZE] = "";
    bzero(&buf, sizeof(buf));

    va_list args;
    va_start(args, fmt);

    u32 size = _Printf(write_to_buf, buf, fmt, args);

    RGBA32 color = COLOR_RGBA32_WHITE;

    u32 x = startX;
    u32 y = startY;

    u32 numLines = 1;

    PrintCommand cmd = { .raw = 0 };

    u8 printEscape = FALSE;

    if (size > 0) {
        for (u32 index = 0; index < size; index += (1 + cmd.skip)) {
            if (buf[index] == CHAR_NULL) {
                break;
            }

            bzero(&cmd, sizeof(cmd));

            crash_screen_parse_formatting(&cmd, buf, index, size, &color, x, y, &printEscape, TRUE);

            if (cmd.print) {
                crash_screen_draw_glyph(x, y, buf[index], color);
            }

            if (cmd.newLine) {
                x = startX;
                y += TEXT_HEIGHT(1);
                numLines++;
            }

            x += TEXT_WIDTH(cmd.shift);
        }
    }

    va_end(args);

    return numLines;
}
