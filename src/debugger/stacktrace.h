#pragma once

#include <PR/os_thread.h>

#include "segment_symbols.h"

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

// libultra import
extern void __osCleanupThread();

extern far u32 generate_stack(OSThread *);
extern far char *get_stack_entry(u32 idx);

static u8 is_text_addr(u32 addr) {
    if ((addr >= (u32)_mainSegmentStart) && (addr <= (u32)_mainSegmentTextEnd)) {
        return TRUE;
    }
    else if ((addr >= (u32)_engineSegmentStart) && (addr <= (u32)_engineSegmentTextEnd)) {
        return TRUE;
    }
    else if ((addr >= (u32)_goddardSegmentStart) && (addr <= (u32)_goddardSegmentTextEnd)) {
        return TRUE;
    }

    return FALSE;
}
