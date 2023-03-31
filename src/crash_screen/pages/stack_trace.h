#pragma once

#include <ultra64.h>

#include "types.h"

// Stack Trace constants
#define STACK_TRACE_NUM_ROWS 19

struct FunctionInStack {
    /*0x00*/ uintptr_t addr;
    /*0x04*/ const char *name;
}; /*0x08*/

extern u32 gStackTraceIndex;

extern const enum ControlTypes stackTracePageControls[];

#ifdef INCLUDE_DEBUG_MAP
void fill_function_stack_trace(OSThread *thread);
#endif
void draw_stack_trace(OSThread *thread);
void crash_screen_input_stack_trace(void);
