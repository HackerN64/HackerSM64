#pragma once

#include <ultra64.h>

#include "types.h"

#include "crash_screen/crash_settings.h"


enum CSSettingsGroup_page_stack {
    CS_OPT_HEADER_PAGE_STACK,
    CS_OPT_STACK_SHOW_ADDRESSES,
    CS_OPT_STACK_SHOW_OFFSETS,
    CS_OPT_END_STACK,
};


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


extern struct CSSetting cs_settings_group_page_stack[];
extern struct CSPage gCSPage_stack;
