#pragma once

#include <ultra64.h>

#include "crash_types.h"

#include "address_select.h"
#include "crash_controls.h"
#include "crash_draw.h"
#include "crash_print.h"
#include "insn_disasm.h"
#include "map_parser.h"
#include "memory_read.h"


extern struct CSPage gCSPages[NUM_PAGES];
extern enum CrashScreenPages gCSPageID;

extern struct CSThreadInfo* gActiveCSThreadInfo;
extern OSThread* gCrashedThread;

extern Address gSelectedAddress;


void create_crash_screen_thread(void);
