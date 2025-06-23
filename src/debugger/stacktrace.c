#include <ultra64.h>
#include <macros.h>

#include "map_parser.h"
#include "symtable.h"
#include "segment_symbols.h"
#include "stacktrace.h"
#include "disasm.h"

static StackFrame stack[STACK_LINE_COUNT];
static u32 stackIdx;
u32 stackTraceGenerated = FALSE;
extern void __osCleanupThread();
extern void thread5_game_loop(void *);
extern void __osDispatchThread();

static u8 is_top_of_stack(u32 ra) {
    return (ra == ((u32)__osCleanupThread));
}

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

/**
 * HOW TO TRACE THE STACK
 * - Store breadcrumb starting with RA
 * - walk back
 * - Find address 
 * - Poke (address-8) and get info to see if it's a valid callsite
 * - get the frame func address by doing RA - func_offset
 * - if the function JAL points to the func address, we have a chain!
 * - set RA to this address
 * - if it's a JalR:
 *   - record register
 *   - walk back to find what the address is loaded to
 *   - read memory and then check the func address
 **/

u32 generate_stack(OSThread *thread) {
    static u32 breadcrumb = 0;
    symtable_header_t symt = symt_open();

    __OSThreadContext *tc = &thread->context;

    u32 sp = tc->sp;
    breadcrumb = tc->ra;

    while (1) { // dont know the end goal yet
        sp++;

        u32 val = *(u32*)sp;

        if (is_text_addr(val + CALLSITE_OFFSET)) {
            int idx = 0;
            symtable_info_t info = get_symbol_info(val + CALLSITE_OFFSET);
            // look for the func in RA
            addrtable_entry_t a = symt_addrtab_search(&symt, breadcrumb, &idx);

            // If we can't logically go further, we're done!
            if (is_top_of_stack(val)) {
                symtable_info_t info = get_symbol_info(val + CALLSITE_OFFSET);
                add_entry_to_stack(val + CALLSITE_OFFSET, breadcrumb, &info);
                breadcrumb = val;
                return stackIdx;
            }

            while (!ADDRENTRY_IS_FUNC(a)) {
                a = symt_addrtab_entry(&symt, --idx);
            }

            if (info.distance == 0) {
                u32 jal = *(u32*)(val + CALLSITE_OFFSET);

                if (insn_is_jal((Insn *) &jal)) {
                    jal = 0x80000000 | ((jal & 0x03FFFFFF) * 4);
                    if (jal == ADDRENTRY_ADDR(a)) {
                        add_entry_to_stack(val + CALLSITE_OFFSET, breadcrumb, &info);
                        breadcrumb = val;
                    }
                } else if (insn_is_jalr((Insn *) &jal)) {
                    // Always add a JALR to the stack, in absence of a better heuristic
                    add_entry_to_stack(val + CALLSITE_OFFSET, breadcrumb, &info);
                    breadcrumb = val;
                }
            }

            if (stackIdx >= STACK_LINE_COUNT) {
                break;
            }
        }
    }

    return stackIdx;
}
