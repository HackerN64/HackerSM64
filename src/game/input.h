#pragma once

#include "types.h"
#include <PR/os_internal.h>

#include "types.h"
#include "config.h"
#include "rumble.h"

// Controller Status Polling:

// This button combo should be standard, so don't change it unless you have a very good reason.
#define TOGGLE_CONT_STATUS_POLLING_COMBO    (Z_TRIG | R_TRIG | A_BUTTON | START_BUTTON)
// How many extra frames to wait between controller status polls when status polling is on, in frames (0 = status poll every frame).
// [0..], default is 15.
#define CONT_STATUS_POLLING_TIME            15

// Deazones for the main analog stick:
#define ANALOG_DEADZONE_N64  8
#define ANALOG_DEADZONE_GCN 12

// Deadzone for status repolling (only assigns controller number in boot mode).
// Also used for input display.
#define ANALOG_DEADZONE_STATUS_POLLING 20


struct DemoInput {
    /*0x00*/ u8 timer; // Time until next input. If this value is 0, it means the demo is over
    /*0x01*/ s8 rawStickX;
    /*0x02*/ s8 rawStickY;
    /*0x03*/ u8 buttonMask;
}; /*0x04*/


// Player Controllers.
extern struct Controller gControllers[MAXCONTROLLERS];
// Defined controller slots. Anything above MAX_NUM_PLAYERS will be unused.
extern struct Controller* const gPlayer1Controller;
extern struct Controller* const gPlayer2Controller;
extern struct Controller* const gPlayer3Controller;
extern struct Controller* const gPlayer4Controller;

// OS Controllers.
extern OSContStatus gControllerStatuses[MAXCONTROLLERS];
extern OSContPadEx gControllerPads[MAXCONTROLLERS];

extern u8    gNumPlayers;
extern u8    gMaxNumPlayers;
extern u8    gControllerBits;
extern _Bool gContStatusPolling;
extern _Bool gContStatusPollingIsBootMode;
extern _Bool gContStatusPollingReadyForInput;
extern u32   gContStatusPollTimer;

extern struct DemoInput* gCurrDemoInput;
#ifndef DISABLE_DEMO
extern u16 gDemoInputListID;

extern struct Controller* const gDemoController;
#endif

void start_controller_status_polling(_Bool isBootMode);
void stop_controller_status_polling(OSContPadEx* pad);
void handle_input(OSMesg* mesg);
void handle_input_simple(OSMesg* mesg);
void init_controllers(void);
