#pragma once

#include <PR/ultratypes.h>

#include "types.h"


struct MapEntry {
    uintptr_t addr;
    size_t nm_offset;
    size_t nm_len;
    size_t pad;
};

s32 is_in_code_segment(uintptr_t addr);
char *parse_map(uintptr_t addr);
char *parse_map_return(uintptr_t *addr);
void map_data_init(void);
char *find_function_in_stack(uintptr_t *sp);
