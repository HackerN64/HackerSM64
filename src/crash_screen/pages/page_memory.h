#pragma once

#include <ultra64.h>

#include "types.h"


// RAM Viewer constants
#define RAM_VIEWER_STEP (ssize_t)(4 * sizeof(Word))


extern const enum ControlTypes ramViewerContList[];


void ram_view_init(void);
void ram_view_draw(void);
void ram_view_input(void);
