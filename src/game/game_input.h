#ifndef GAME_INPUT_H
#define GAME_INPUT_H

#include "types.h"
#include <PR/os_internal.h>

#include "types.h"
#include "config.h"

// Controller Status Polling:

// This button combo should be standard, so don't change it unless you have a very good reason.
#define TOGGLE_CONT_STATUS_POLLING_COMBO            (A_BUTTON | B_BUTTON | START_BUTTON)
// How long after exiting polling to start accepting inputs.
// The purpose of this is to allow the player to release all the combo buttons before the game would recognize them.
// Default is 3.
#define CONT_STATUS_POLLING_EXIT_INPUT_COOLDOWN      3
// How often to poll for controller status when gContStatusPolling is true, in frames.
// Default is 15.
#define CONT_STATUS_POLLING_TIME                    15

struct DemoInput {
    u8 timer; // time until next input. if this value is 0, it means the demo is over
    s8 rawStickX;
    s8 rawStickY;
    u8 buttonMask;
};

// Player Controllers
extern struct Controller gControllers[MAXCONTROLLERS];
// Defined controller slots. Anything above NUM_SUPPORTED_CONTROLLERS will be unused.
extern struct Controller *gPlayer1Controller;
extern struct Controller *gPlayer2Controller;
extern struct Controller *gPlayer3Controller;
extern struct Controller *gPlayer4Controller;

// OS Controllers
extern OSContStatus gControllerStatuses[MAXCONTROLLERS];
extern OSContPadEx gControllerPads[MAXCONTROLLERS];

extern u8 gNumPlayers;
extern u8 gControllerBits;
extern u8 gContStatusPolling;
extern u8 gContStatusPollingReadyForInput;
extern u32 gContStatusPollTimer;

extern struct DemoInput *gCurrDemoInput;

void start_controller_status_polling(void);
void stop_controller_status_polling(void);
void handle_input(void);
void init_controllers(void);

#endif /* GAME_INPUT */
