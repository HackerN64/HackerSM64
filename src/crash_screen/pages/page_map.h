#pragma once

#include <ultra64.h>

#include "types.h"


// Number of rows to print:
#define MAP_VIEWER_NUM_ROWS 20


extern const enum ControlTypes mapViewerContList[];


void map_view_init(void);
void map_view_draw(void);
void map_view_input(void);
