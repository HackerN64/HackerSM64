#include <ultra64.h>

#include <stdarg.h>
#include <string.h>

#include "types.h"
#include "sm64.h"

#include "assert.h"

#include "game/printf.h"


void __n64Assertf(char* condition, char* fileName, u32 lineNum, char* message, ...) {
    char buffer[ASSERTF_BUFFER_SIZE] = "";
    va_list args;
    va_start(args, message);

    size_t size = _Printf(write_to_buf, buffer, message, args);

    __n64Assert(condition, fileName, lineNum, ((size < (ASSERTF_BUFFER_SIZE - 1)) ? buffer : "ASSERTF ERROR: BUFFER SIZE EXCEEDED"));

    va_end(args);
}
