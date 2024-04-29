#pragma once

#include <ultra64.h>

#include "types.h"

#include "crash_screen/cs_settings.h"


enum CSSettingsGroup_page_stack {
    CS_OPT_HEADER_PAGE_STACK,
    CS_OPT_STACK_SHOW_ADDRESSES,
    CS_OPT_STACK_SHOW_OFFSETS,
    CS_OPT_END_STACK,
};


// Number of rows to print:
#define STACK_TRACE_NUM_ROWS 20

// The number of functions to save to the stack trace buffer.
#define STACK_TRACE_BUFFER_SIZE 32


typedef struct FunctionInStack {
    /*0x00*/ Address stackAddr;
    /*0x04*/ Address currAddr;
} FunctionInStack; /*0x08*/


extern struct CSSetting cs_settings_group_page_stack[];
extern struct CSPage gCSPage_stack;
