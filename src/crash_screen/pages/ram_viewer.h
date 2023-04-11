#pragma once

#include <ultra64.h>

#include "types.h"


// RAM Viewer constants
#define RAM_VIEWER_STEP (s32)(4 * sizeof(uintptr_t))

#define RAM_VIEWER_NUM_ROWS         19
#define RAM_VIEWER_SHOWN_SECTION    ((RAM_VIEWER_NUM_ROWS - 1) * RAM_VIEWER_STEP)

#define RAM_VIEWER_SCROLL_MIN       VALID_RAM_START
#define RAM_VIEWER_SCROLL_MAX       (VALID_RAM_END - RAM_VIEWER_SHOWN_SECTION)


extern const enum ControlTypes ramViewerPageControls[];


void draw_ram_viewer(OSThread* thread);
void crash_screen_input_ram_viewer(void);
