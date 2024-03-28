#pragma once

#include <ultra64.h>

#include "types.h"



extern RegisterId gInspectedRegister;


extern struct CSPopup gCSPopup_reginspect;


void cs_open_reginspect(RegisterId regId);
