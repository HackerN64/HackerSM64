#pragma once

#include <ultra64.h>

#include "types.h"


struct MapEntry {
    /*0x00*/ uintptr_t addr;
    /*0x04*/ uintptr_t name_offset;
    /*0x08*/ size_t name_len;
    /*0x0C*/ size_t pad;
}; /*0x10*/


#define IS_IN_RAM(addr)              (((addr) >= RAM_START) && ((addr) < RAM_END))
#define IS_IN_SEGMENT(addr, segment) (((addr) >= (uintptr_t)_##segment##SegmentStart) && ((addr) <= (uintptr_t)_##segment##SegmentTextEnd))


void map_data_init(void);
_Bool is_in_code_segment(uintptr_t addr);
char *parse_map(uintptr_t *addr);
char *parse_map_exact(uintptr_t addr);
char *find_function_in_stack(uintptr_t *sp);
