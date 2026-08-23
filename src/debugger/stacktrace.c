#include <ultra64.h>
#include <macros.h>
#include <string.h>

#include "config/config_debug.h"

#include "map_parser.h"
#include "symtable.h"
#include "stacktrace.h"
#include "disasm.h"

/**
 * Uncomment to help diagnose issues with stacktrace implementation
 **/
// #define STACKTRACE_DEBUG

#if defined(DEBUG_EXPORT_SYMBOLS) && defined(DEBUG_FULL_STACK_TRACE)

static StackFrame stack[STACK_LINE_COUNT];
static u32 stackIdx;
u32 stackTraceGenerated = FALSE;

#define STACK_END_STR "[End of stack]"

static u8 is_top_of_stack(u32 ra) {
    return (ra == ((u32)__osCleanupThread));
}

static void add_entry_to_stack(u32 addr, u32 ra, symtable_info_t *info) {
    StackFrame *frame = &stack[stackIdx++];

    frame->func = addr;
    frame->offset = info->func_offset;
    frame->ra = ra;
    frame->line = info->line;
    sprintf(frame->funcname, "%s", info->func);
}

char *get_stack_entry(u32 idx) {
    static char stackbuf[100];

    sprintf(stackbuf, "%08X: %s:%d", stack[idx].func, stack[idx].funcname, stack[idx].line);

    return stackbuf;
}

u32 generate_stack(OSThread *thread) {
    static u32 breadcrumb = 0;
    symtable_header_t symt = symt_open();

    __OSThreadContext *tc = &thread->context;

    u32 sp = tc->sp;
    breadcrumb = tc->ra;

    while (1) { // dont know the end goal yet
        sp += 4;

        u32 val = *(u32*)sp;

        // make sure we're working on an actual address
        if (is_text_addr(val + CALLSITE_OFFSET)) {
            int idx = 0;
            symtable_info_t info = get_symbol_info(val + CALLSITE_OFFSET);
            addrtable_entry_t addr_search = symt_addrtab_search(&symt, breadcrumb, &idx);

            // If we can't logically go further, we're done!
            if (is_top_of_stack(val)) {
                symtable_info_t info = get_symbol_info(val + CALLSITE_OFFSET);
                sprintf(info.func, STACK_END_STR);
                add_entry_to_stack(val + CALLSITE_OFFSET, breadcrumb, &info);
                breadcrumb = val;
                return stackIdx;
            }

            // get the start of the current frame's func
            u32 funcstart = get_start_of_func(addr_search);

            // Make sure the address is an actual callsite
            if (info.distance == 0) {
                u32 jal = *(u32*)(val + CALLSITE_OFFSET);
#ifdef STACKTRACE_DEBUG
                osSyncPrintf("Candidate: %08X %s\n", val + CALLSITE_OFFSET, insn_disasm(&(InsnData){.d = jal}));
#endif // STACKTRACE_DEBUG

                if (insn_is_jal((Insn *) &jal)) {
                    u32 jalTarget = 0x80000000 | ((jal & 0x03FFFFFF) * 4);
#ifdef STACKTRACE_DEBUG
                    char jaltarget_buf[256];
                    char funcname_buf[256];
                    osSyncPrintf("  Jal Target: %08X %s (We want %08X %s)\n", 
                        jalTarget,
                        parse_map(jaltarget_buf, sizeof(jaltarget_buf), jalTarget, FALSE),
                        funcstart,
                        parse_map(funcname_buf, sizeof(funcname_buf), funcstart, FALSE)
                    );
#endif // STACKTRACE_DEBUG

                    // make sure JAL is to the current func
                    if (jalTarget == ADDRENTRY_ADDR(funcstart)) {
                        add_entry_to_stack(val + CALLSITE_OFFSET, breadcrumb, &info);
                        breadcrumb = val;
                    } else {
                        // Just in case we're on a weird boundary, find the _previous_
                        //  function start and see if we jal'd to there instead.
                        funcstart = get_start_of_func(addr_search - 4);

                        if (jalTarget == ADDRENTRY_ADDR(funcstart)) {
                            add_entry_to_stack(val + CALLSITE_OFFSET, breadcrumb, &info);
                            breadcrumb = val;
                        }
                    }
                } else if (insn_is_jalr((Insn *) &jal)) {
                    // Always add a JALR to the stack, in absence of a better heuristic
                    if (stack[stackIdx - 1].func != (val + CALLSITE_OFFSET)) {
                        add_entry_to_stack(val + CALLSITE_OFFSET, breadcrumb, &info);
                        breadcrumb = val;
                    }
                } else if (insn_is_j((Insn *) &jal)) {
                    if (stack[stackIdx - 1].func != (val + CALLSITE_OFFSET)) {
                        add_entry_to_stack(val + CALLSITE_OFFSET, breadcrumb, &info);
                        breadcrumb = val;
                    }
                }
            }

            if (stackIdx >= STACK_LINE_COUNT) {
                break;
            }
        }
    }

    return stackIdx;
}

#endif // DEBUG_EXPORT_SYMBOLS && DEBUG_FULL_STACK_TRACE
