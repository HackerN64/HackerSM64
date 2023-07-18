#pragma once

#include <ultra64.h>

#include "types.h"


#define ASSERTS_NUM_ROWS 16


extern const enum ControlTypes assertsContList[];


void assert_init(void);
void assert_draw(void);
void assert_input(void);
