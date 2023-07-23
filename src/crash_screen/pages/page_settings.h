#pragma once

#include <ultra64.h>

#include "types.h"


#define SETTINGS_NUM_ROWS 20


extern const enum ControlTypes settingsContList[];


void settings_init(void);
void settings_draw(void);
void settings_input(void);
