#pragma once

#include "farcall.h"

typedef struct {
    char *file;
    char *func;
    int line;
    u16 func_offset;
} symtable_info_t;

symtable_info_t get_symbol_info(u32 vaddr);
extern far char *parse_map(u32 pc, u32 andOffset);
extern far char *find_function_in_stack(u32 *sp, int *line);
