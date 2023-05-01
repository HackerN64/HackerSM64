#pragma once

#include <ultra64.h>

#include "types.h"


// RAM Viewer constants
#define RAM_VIEWER_STEP             (ssize_t)(4 * sizeof(Word))

#define RAM_VIEWER_NUM_ROWS         19
#define RAM_VIEWER_SHOWN_SECTION    ((RAM_VIEWER_NUM_ROWS - 1) * RAM_VIEWER_STEP)

#define RAM_VIEWER_SCROLL_MIN       VIRTUAL_RAM_START
#define RAM_VIEWER_SCROLL_MAX       (VIRTUAL_RAM_END - RAM_VIEWER_SHOWN_SECTION)


extern const enum ControlTypes ramViewerContList[];


void ram_viewer_init(void);
void ram_viewer_draw(void);
void ram_viewer_input(void);
