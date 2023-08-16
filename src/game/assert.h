#pragma once

#include <ultra64.h>

#include "types.h"


#define NUM_ASSERT_ARGS 8


// Non-floats:

typedef struct {
    /*0x00*/ char* fmt;
    /*0x04*/ void* val;
} AssertArg; /*0x08*/

extern AssertArg gAssertArgs[NUM_ASSERT_ARGS];
extern int gAssertArgsCount;

void* get_assert_arg(int i);
void set_assert_arg(int i, char* fmt, void* val);
void append_assert_arg(char* fmt, void* val);


// Floats:

typedef struct {
    /*0x00*/ char* fmt;
    /*0x04*/ float val;
} AssertArg_float;

extern AssertArg_float gAssertArgs_float[NUM_ASSERT_ARGS];
extern int gAssertArgCount_float;

float get_assert_arg_float(int i);
void set_assert_arg_float(int i, char* fmt, float val);
void append_assert_arg_float(char* fmt, float val);
