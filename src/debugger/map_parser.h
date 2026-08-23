#pragma once

#include "farcall.h"

typedef struct {
    char *file;
    char *func;
    int line;
    u16 distance;
    u16 func_offset;
} symtable_info_t;

symtable_info_t get_symbol_info(u32 vaddr);
u32 get_start_of_func(u32 addr);
extern far char *parse_map(u32 pc, u32 andOffset);
extern far symtable_info_t walk_stack(u32 *addr);
extern far char* __symbolize(void *vaddr, char *buf, int size, u32 andOffset);
