#pragma once

#include <ultra64.h>

#include "types.h"


struct MapEntry {
    /*0x00*/ uintptr_t addr;
    /*0x04*/ size_t nm_offset;
    /*0x08*/ size_t nm_len;
    /*0x0C*/ size_t pad;
}; /*0x10*/

void map_data_init(void);
s32 is_in_code_segment(uintptr_t addr);
char *parse_map(uintptr_t *addr);
char *parse_map_exact(uintptr_t addr);
char *find_function_in_stack(uintptr_t *sp);
