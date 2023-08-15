#pragma once

#include <ultra64.h>

#include "types.h"


// #define ASSERTS_NUM_ROWS 16


extern const enum ControlTypes logContList[];


void log_init(void);
void log_draw(void);
void log_input(void);
