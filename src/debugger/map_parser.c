#include <ultra64.h>
#include <macros.h>
#include <PR/os_internal_error.h>
#include <stdarg.h>
#include <string.h>
#include "segments.h"
#include "game/memory.h"

#include "map_parser.h"
#include "symtable.h"

#define STACK_TRAVERSAL_LIMIT 100
#define UNKNOWN_SYMBOL "???"

extern u8 _mainSegmentStart[];
extern u8 _mainSegmentTextEnd[];
extern u8 _engineSegmentStart[];
extern u8 _engineSegmentTextEnd[];
extern u8 _goddardSegmentStart[];
extern u8 _goddardSegmentTextEnd[];

char* __symbolize(void *vaddr, char *buf, int size, u32 andOffset) {
    symtable_header_t symt = symt_open();
    if (symt.head[0]) {
        u32 addr = (u32)vaddr;
        int idx = 0;
        addrtable_entry_t a = symt_addrtab_search(&symt, addr, &idx);
        while (!ADDRENTRY_IS_FUNC(a)) {
            a = symt_addrtab_entry(&symt, --idx);
        }

        symtable_entry_t ALIGNED16 entry;
        // Read the symbol name
        symt_entry_fetch(&symt, &entry, idx);
        char *func = symt_entry_func(&symt, &entry, addr, buf, size-12);
        char lbuf[12];
        if (andOffset) {
            sprintf(lbuf, "+0x%lx", addr - ADDRENTRY_ADDR(a));
        } else {
            lbuf[0] = 0;
        }
        return strcat(func, lbuf);
    }
    sprintf(buf, "%s", UNKNOWN_SYMBOL);
    return buf;
}

char *parse_map(u32 addr, u32 andOffset) {
    static char map_name[64] ALIGNED16;
    __symbolize((u32*)addr, map_name, sizeof(map_name), andOffset);

    return map_name;
}

char *find_function_in_stack(u32 *sp, int *line) {
    char *ret = NULL;
    u32 val = *sp;

    #define CALLSITE_OFFSET 8
    for (int i = 0; i < STACK_TRAVERSAL_LIMIT; i++) {
        val = *sp;
        val = *(u32 *)val;
        *sp += 4;


        if ((val >= (u32)_mainSegmentStart) && (val <= (u32)_mainSegmentTextEnd)) {
            ret = parse_map(val + CALLSITE_OFFSET, FALSE);
            break;
        }
        else if ((val >= (u32)_engineSegmentStart) && (val <= (u32)_engineSegmentTextEnd)) {
            ret = parse_map(val + CALLSITE_OFFSET, FALSE);
            break;
        }
        else if ((val >= (u32)_goddardSegmentStart) && (val <= (u32)_goddardSegmentTextEnd)) {
            ret = parse_map(val + CALLSITE_OFFSET, FALSE);
            break;
        }
    }

    if (ret) {
        symtable_info_t info = get_symbol_info(val + CALLSITE_OFFSET);
        *line = info.line;
    }

    return ret;
}

symtable_info_t get_symbol_info(u32 addr) {
    static char filebuf[100];
    void *vaddr = (void *)addr;
    symtable_header_t symt = symt_open();


    if (symt.head[0]) {
        symtable_info_t info;
        u32 addr = (u32)vaddr;
        int idx = 0;
        symt_addrtab_search(&symt, addr, &idx);

        symtable_entry_t ALIGNED16 entry;

        // Read the symbol name
        filebuf[0] = 0;
        symt_entry_fetch(&symt, &entry, idx);
        info.line = entry.line;
        info.func_offset = entry.func_off;
        info.file = symt_entry_file(&symt, &entry, filebuf, sizeof(filebuf));
        info.func = parse_map(addr, TRUE);

        return info;
    }
    return (symtable_info_t){.line = -1};
}
