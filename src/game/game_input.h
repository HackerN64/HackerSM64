#ifndef GAME_INPUT_H
#define GAME_INPUT_H

#include "types.h"
#include <PR/os_internal.h>

#include "types.h"
#include "config.h"

// Controller Status Polling:

// This button combo should be standard, so don't change it unless you have a very good reason.
#define TOGGLE_CONT_STATUS_POLLING_COMBO    (A_BUTTON | B_BUTTON | START_BUTTON)
// How often to poll for controller status when status polling is on, in frames.
// Default is 15.
#define CONT_STATUS_POLLING_TIME            15

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

// OS Controllers
extern OSContStatus gControllerStatuses[MAXCONTROLLERS];
extern OSContPadEx gControllerPads[MAXCONTROLLERS];

extern u8    gNumPlayers;
extern u8    gControllerBits;
extern _Bool gContStatusPolling;
extern _Bool gContStatusPollingIsBootMode;
extern _Bool gContStatusPollingReadyForInput;
extern u32   gContStatusPollTimer;

extern struct DemoInput* gCurrDemoInput;

void start_controller_status_polling(s32 isBootMode);
void stop_controller_status_polling(OSContPadEx* pad);
void handle_input(OSMesg* mesg);
void init_controllers(void);

#endif /* GAME_INPUT */
