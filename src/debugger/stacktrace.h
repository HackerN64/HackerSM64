#pragma once

#include <PR/os_thread.h>

typedef struct {
    u32 func;
    u32 offset;
    u32 ra;
    int line;
    char funcname[100];
} StackFrame;

#define STACK_TRAVERSAL_LIMIT 100
#define STACK_LINE_COUNT 17
// RA points to 2 instructions past any given callsite
#define CALLSITE_OFFSET -8

extern u32 stackTraceGenerated;

extern far u32 generate_stack(OSThread *);
extern far char *get_stack_entry(u32 idx);
