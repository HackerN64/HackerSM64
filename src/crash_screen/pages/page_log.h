#pragma once

#include <ultra64.h>

#include "types.h"


#define LOG_NUM_ROWS MIN(LOG_BUFFER_SIZE, 20)


void log_init(void);
void log_draw(void);
void log_input(void);
