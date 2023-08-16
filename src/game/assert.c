#include <ultra64.h>

#include "types.h"
#include "sm64.h"

#include "assert.h"

// Non-floats:
AssertArg gAssertArgs[NUM_ASSERT_ARGS];
int gAssertArgsCount = 0;

void* get_assert_arg(int i) {
    return gAssertArgs[i].val;
}

void set_assert_arg(int i, char* fmt, void* val) {
    gAssertArgs[i] = (AssertArg){ .fmt = fmt, .val = val };
}

void append_assert_arg(char* fmt, void* val) {
    if (gAssertArgsCount < NUM_ASSERT_ARGS) {
        set_assert_arg(gAssertArgsCount, fmt, val);
        gAssertArgsCount++;
    }
}

// Floats:
AssertArg_float gAssertArgs_float[NUM_ASSERT_ARGS];
int gAssertArgCount_float = 0;

float get_assert_arg_float(int i) {
    return gAssertArgs_float[i].val;
}

void set_assert_arg_float(int i, char* fmt, float val) {
    gAssertArgs_float[i] = (AssertArg_float){ .fmt = fmt, .val = val };
}

void append_assert_arg_float(char* fmt, float val) {
    if (gAssertArgCount_float < NUM_ASSERT_ARGS) {
        set_assert_arg_float(gAssertArgCount_float, fmt, val);
        gAssertArgCount_float++;
    }
}
