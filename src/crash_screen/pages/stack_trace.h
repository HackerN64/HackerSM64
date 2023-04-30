#pragma once

#include <ultra64.h>

#include "types.h"


// Number of rows to print:
#define STACK_TRACE_NUM_ROWS 19

// The number of functions to save to the stack trace buffer.
#define STACK_TRACE_BUFFER_SIZE 64


struct FunctionInStack {
    /*0x00*/ Address stackAddr;
    /*0x04*/ Address curAddr;
    /*0x08*/ Address faddr;
    /*0x0C*/ const char* fname;
}; /*0x10*/


extern const enum ControlTypes stackTraceContList[];


void stack_trace_init(void);
void stack_trace_draw(void);
void stack_trace_input(void);
