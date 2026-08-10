#pragma once

#include "types.h"
#include "level_update.h"
#include "level_table.h"

#define DEMO_BANK_INPUT_CAPACITY (DEMO_INPUTS_POOL_SIZE / sizeof(struct DemoInput))
#define PRESS_START_DEMO_TIMER 800

struct DemoFile {
    void *romStart;
    void *romEnd;
};

u8 player_action_reads_stick(struct MarioState *m);
void apply_demo_inputs_to_player(struct MarioState *m);

s32 run_level_id_or_demo(s32 level);
s32 print_demo_footer(UNUSED s32 arg);

void print_demo_header();
void record_demo();
void run_demo_inputs(void);

extern void *demoInputsMalloc;
extern u32 gCurrentDemoSize;
extern u32 gCurrentDemoIdx;
extern struct DemoFile gDemos[LEVEL_COUNT];
extern u8 demoFile[], demoFileEnd[];
extern u16 gDemoLevel;
extern u16 gFinalDemoLevel;
extern u8 gDemoActive;

