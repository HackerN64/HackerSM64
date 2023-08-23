#pragma once

#include <ultra64.h>

#include "types.h"


// Number of rows to print:
#define STACK_TRACE_NUM_ROWS 20

// The number of functions to save to the stack trace buffer.
#define STACK_TRACE_BUFFER_SIZE 64


typedef struct FunctionInStack {
    /*0x00*/ Address stackAddr;
    /*0x04*/ Address curAddr;
    /*0x08*/ Address faddr;
    /*0x0C*/ const char* fname;
} FunctionInStack; /*0x10*/


extern CSPage gCSPage_stack;
