#include <ultra64.h>
#include <macros.h>
#include <PR/os_internal_error.h>
#include <stdarg.h>
#include <string.h>
#include "game/memory.h"

#include "map_parser.h"
#include "symtable.h"

#ifdef DEBUG_EXPORT_SYMBOLS

#define UNKNOWN_SYMBOL "???"

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
        entry.func_off = addr - ADDRENTRY_ADDR(a);

        return strcat(func, lbuf);
    }
    sprintf(buf, "%s", UNKNOWN_SYMBOL);
    return buf;
}

char *parse_map(u32 addr, u32 andOffset) {
    static char map_name[64] ALIGNED16;
    char *ret = map_name;

    __symbolize((u32*)addr, map_name, sizeof(map_name), andOffset);

    if (ret[0] == ' ') {
        ret++;
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
        addrtable_entry_t a = symt_addrtab_entry(&symt, idx);
        info.distance = addr - ADDRENTRY_ADDR(a);
        info.file = symt_entry_file(&symt, &entry, filebuf, sizeof(filebuf));
        info.func = parse_map(addr, FALSE);

        return info;
    }
    return (symtable_info_t){.line = -1};
}

#endif // DEBUG_EXPORT_SYMBOLS
