#pragma once

#include <ultra64.h>

#include "types.h"


#define SETTINGS_NUM_ROWS 20

#define VALUE_NAME_SIZE 10


extern const enum ControlTypes settingsContList[];


void settings_init(void);
void settings_draw(void);
void settings_input(void);
