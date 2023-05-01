#pragma once

#include <ultra64.h>

#include "types.h"


// Number of rows to print:
#define MAP_VIEWER_NUM_ROWS 20


extern const enum ControlTypes mapViewerContList[];


void map_viewer_init(void);
void map_viewer_draw(void);
void map_viewer_input(void);
