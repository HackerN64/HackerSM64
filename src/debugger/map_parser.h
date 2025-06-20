#pragma once

#include "farcall.h"

char *search_symbol(u32 vaddr, int *line);
extern far char *parse_map(u32 pc, u32 andOffset);
extern far char *find_function_in_stack(u32 *sp, int *line);
