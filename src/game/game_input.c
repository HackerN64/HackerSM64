#include <ultra64.h>
#include "config.h"

#include "joybus.h"
#include "engine/math_util.h"
#include "main.h"
#include "game_init.h"
#include "rumble_init.h"
#include "game_input.h"
#include "profiling.h"

// Player Controllers
struct Controller gControllers[MAXCONTROLLERS];
// Defined controller slots. Anything above NUM_SUPPORTED_CONTROLLERS will be unused.
struct Controller *gPlayer1Controller = &gControllers[0];
struct Controller *gPlayer2Controller = &gControllers[1];
struct Controller *gPlayer3Controller = &gControllers[2];
struct Controller *gPlayer4Controller = &gControllers[3];

// OS Controllers
OSContStatus gControllerStatuses[MAXCONTROLLERS];
OSContPadEx gControllerPads[MAXCONTROLLERS];

u8 gNumPlayers = 1;
u8 gControllerBits = 0x0; // Which ports have a controller connected to them.
u8 gContStatusPolling = FALSE;
u8 gContStatusPollingReadyForInput = TRUE;
u32 gContStatusPollTimer = 0;

// Title Screen Demo Handler
struct DemoInput *gCurrDemoInput = NULL;

#if !defined(DISABLE_DEMO) && defined(KEEP_MARIO_HEAD)
/**
 * If a demo sequence exists, this will run the demo input list until it is complete.
 */
void run_demo_inputs(void) {
    // Eliminate the unused bits.
    gPlayer1Controller->controllerData->button &= VALID_BUTTONS;

    // Check if a demo inputs list exists and if so,
    // run the active demo input list.
    if (gCurrDemoInput != NULL) {
        // The timer variable being 0 at the current input means the demo is over.
        // Set the button to the END_DEMO mask to end the demo.
        if (gCurrDemoInput->timer == 0) {
            gPlayer1Controller->controllerData->stick_x = 0;
            gPlayer1Controller->controllerData->stick_y = 0;
            gPlayer1Controller->controllerData->button = END_DEMO;
        } else {
            // Backup the start button if it is pressed, since we don't want the
            // demo input to override the mask where start may have been pressed.
            u16 startPushed = (gPlayer1Controller->controllerData->button & START_BUTTON);

            // Perform the demo inputs by assigning the current button mask and the stick inputs.
            gPlayer1Controller->controllerData->stick_x = gCurrDemoInput->rawStickX;
            gPlayer1Controller->controllerData->stick_y = gCurrDemoInput->rawStickY;

            // To assign the demo input, the button information is stored in
            // an 8-bit mask rather than a 16-bit mask. this is because only
            // A, B, Z, Start, and the C-Buttons are used in a demo, as bits
            // in that order. In order to assign the mask, we need to take the
            // upper 4 bits (A, B, Z, and Start) and shift then left by 8 to
            // match the correct input mask. We then add this to the masked
            // lower 4 bits to get the correct button mask.
            gPlayer1Controller->controllerData->button =
                ((gCurrDemoInput->buttonMask & 0xF0) << 8) | ((gCurrDemoInput->buttonMask & 0x0F));

            // If start was pushed, put it into the demo sequence being input to end the demo.
            gPlayer1Controller->controllerData->button |= startPushed;

            // Run the current demo input's timer down. if it hits 0, advance the demo input list.
            if (--gCurrDemoInput->timer == 0) {
                gCurrDemoInput++;
            }
        }
    }
}
#endif // !defined(DISABLE_DEMO) && defined(KEEP_MARIO_HEAD)

ALWAYS_INLINE s32 check_button_pressed_combo(u16 buttonDown, u16 buttonPressed, u16 combo) {
    return (((buttonDown & combo) == combo) && (buttonPressed & combo));
}

/**
 * Link the controller struct to the appropriate status and pad.
 */
void assign_controller_data(struct Controller *controller, int port) {
    controller->statusData = &gControllerStatuses[port];
    controller->controllerData = &gControllerPads[port];
    controller->port = port;
}

/**
 * Initialize the controller structs to point at the OSCont information.
 * Automatically assignins controller numbers based on port order.
 */
void assign_controllers_auto(void) {
#ifdef PRIORITIZE_GAMECUBE_CONTROLLERS_ON_BOOT
    const s32 prioritizeGCN = TRUE;
#else
    const s32 prioritizeGCN = FALSE;
#endif
    OSPortInfo *portInfo = NULL;
    int port, cont = 0;
    int lastUsedPort = -1;

    // Two passes if PRIORITIZE_GAMECUBE_CONTROLLERS_ON_BOOT is enabled.
    // The first pass is for only GameCube controllers and the second pass for everything else.
    for (int pass = 0; pass < (1 + prioritizeGCN); pass++) {
        // Loop over the 4 ports and link the controller structs to the appropriate status and pad.
        // The game allows you to have a controller plugged into any port in order to play the game.
        for (port = 0; port < MAXCONTROLLERS; port++) {
            if (cont >= NUM_SUPPORTED_CONTROLLERS) {
                break;
            }

            portInfo = &gPortInfo[port];

            // Is the controller plugged in, and is it a GameCube controller on the first pass?
            if (portInfo->plugged && (!prioritizeGCN || (pass == !(portInfo->type & CONT_CONSOLE_GCN)))) {
                assign_controller_data(&gControllers[cont], port);

                portInfo->playerNum = (cont + 1);

                if (lastUsedPort < port) {
                    lastUsedPort = port;
                }

                cont++;
            }
        }
    }

    // Disable the ports after the last used one.
    osContSetCh(lastUsedPort + 1);
}

/**
 * Initialize the controller structs to point at the OSCont information.
 * Assigns controllers based on assigned data from status polling.
 */
void assign_controllers_by_player_num(void) {
    OSPortInfo *portInfo = NULL;
    int port;
    int lastUsedPort = -1;

    // Loop over the 4 ports and link the controller structs to the appropriate status and pad.
    // The game allows you to have a controller plugged into any port in order to play the game.
    for (port = 0; port < MAXCONTROLLERS; port++) {
        portInfo = &gPortInfo[port];
        // Is controller plugged in and assigned to a player?
        if (portInfo->plugged && portInfo->playerNum) {
            assign_controller_data(&gControllers[portInfo->playerNum - 1], port);

            lastUsedPort = port;
        }
    }

    // Disable the ports after the last used one.
    osContSetCh(lastUsedPort + 1);
}

/**
 * Read raw controller input data.
 */
static void poll_controller_inputs(OSMesg *mesg) {
#ifdef ENABLE_RUMBLE
    block_until_rumble_pak_free();
#endif
    osContStartReadDataEx(&gSIEventMesgQueue);
    osRecvMesg(&gSIEventMesgQueue, mesg, OS_MESG_BLOCK);
    osContGetReadDataEx(gControllerPads);
#ifdef ENABLE_RUMBLE
    release_rumble_pak_control();
#endif
}

/**
 * Check for new controller data on all ports.
 */
static void poll_controller_statuses(OSMesg *mesg) {
#ifdef ENABLE_RUMBLE
    block_until_rumble_pak_free();
#endif
    osContSetCh(MAXCONTROLLERS);
    osContStartQuery(&gSIEventMesgQueue);
    osRecvMesg(&gSIEventMesgQueue, mesg, OS_MESG_BLOCK);
    osContGetQueryEx(&gControllerBits, gControllerStatuses);
#ifdef ENABLE_RUMBLE
    release_rumble_pak_control();
#endif
}

/**
 * Start polling for new controllers and open the UI.
 */
void start_controller_status_polling(void) {
    gContStatusPolling = TRUE;
    gContStatusPollTimer = 0;
    gNumPlayers = 1;
    bzero(gPortInfo, sizeof(gPortInfo));
    bzero(gControllers, sizeof(gControllers));
#ifdef ENABLE_RUMBLE
    cancel_rumble();
#endif
}

/**
 * Stop polling for new controllers and assign them to their player numbers.
 */
void stop_controller_status_polling(void) {
    gContStatusPolling = FALSE;
    gContStatusPollTimer = 0;
    gNumPlayers--;
    assign_controllers_by_player_num();
#ifdef ENABLE_RUMBLE
    cancel_rumble();
#endif
}

/**
 * Assign player numbers to controllers based on player input.
 */
void read_controller_inputs_status_polling(void) {
    OSPortInfo *portInfo = NULL;
    u16 totalInput = 0x0;

    // Read inputs from all four ports when status polling.
    for (int port = 0; port < MAXCONTROLLERS; port++) {
        portInfo = &gPortInfo[port];

        if (portInfo->plugged) {
            u16 button = gControllerPads[port].button;
#if (NUM_SUPPORTED_CONTROLLERS > 1)
            u16 pressed = (~portInfo->pollingInput & button);
#endif
            portInfo->pollingInput = button;
            totalInput |= button;

            if (gContStatusPollingReadyForInput) {
                // If a button is pressed on an unassigned controller, assign it the current player number.
                if (button && !portInfo->playerNum) {
                    portInfo->playerNum = gNumPlayers;
                    gNumPlayers++;
                }

                // If the combo is pressed, stop polling and assign the current controllers.
                if (gNumPlayers > __builtin_popcount(gControllerBits)
                 || gNumPlayers > NUM_SUPPORTED_CONTROLLERS
#if (NUM_SUPPORTED_CONTROLLERS > 1)
                 || check_button_pressed_combo(button, pressed, TOGGLE_CONT_STATUS_POLLING_COMBO)
#endif
                ) {
                    gContStatusPollingReadyForInput = FALSE;
                    stop_controller_status_polling();
                    return;
                }
            }
        } else {
            portInfo->pollingInput = 0x0;
        }
    }

    // Wait for all inputs to be realseased before checking for player assignment inputs or the combo being entered again.
    if (totalInput == 0) {
        gContStatusPollingReadyForInput = TRUE;
    }
}


/**
 * Take the updated controller struct and calculate the new x, y, and distance floats.
 */
static void adjust_analog_stick(struct Controller *controller) {
    s16 deadzone = (controller->statusData->type & CONT_CONSOLE_GCN) ? 12 : 8;
    s16 offset = (deadzone - 2);

    // Reset the controller's x and y floats.
    controller->stickX = 0;
    controller->stickY = 0;

    // Modulate the rawStickX and rawStickY to be the new f32 values by adding/subtracting 6.
    if (controller->rawStickX <= -deadzone) controller->stickX = (controller->rawStickX + offset);
    if (controller->rawStickX >=  deadzone) controller->stickX = (controller->rawStickX - offset);
    if (controller->rawStickY <= -deadzone) controller->stickY = (controller->rawStickY + offset);
    if (controller->rawStickY >=  deadzone) controller->stickY = (controller->rawStickY - offset);

    // Calculate f32 magnitude from the center by vector length.
    controller->stickMag = sqrtf(sqr(controller->stickX) + sqr(controller->stickY));

    // Magnitude cannot exceed 64.0f: if it does, modify the values
    // appropriately to flatten the values down to the allowed maximum value.
    if (controller->stickMag > 64) {
        controller->stickX *= (64 / controller->stickMag);
        controller->stickY *= (64 / controller->stickMag);
        controller->stickMag = 64;
    }
}

/**
 * Update the controller struct with available inputs if present.
 */
void read_controller_inputs_normal(void) {
    s32 cont;

#if !defined(DISABLE_DEMO) && defined(KEEP_MARIO_HEAD)
    run_demo_inputs();
#endif

    for (cont = 0; cont < NUM_SUPPORTED_CONTROLLERS; cont++) {
        struct Controller *controller = &gControllers[cont];
        OSContPadEx *controllerData = controller->controllerData;
        // If we're receiving inputs, update the controller struct with the new button info.
        if (controllerData != NULL && gContStatusPollTimer > CONT_STATUS_POLLING_EXIT_INPUT_COOLDOWN) {
            controller->rawStickX = controllerData->stick_x;
            controller->rawStickY = controllerData->stick_y;
            controller->buttonPressed  = (~controller->buttonDown & controllerData->button);
            controller->buttonReleased = (~controllerData->button & controller->buttonDown);
            // 0.5x A presses are a good meme
            controller->buttonDown = controllerData->button;
            adjust_analog_stick(controller);

            if (gContStatusPollingReadyForInput) {
                if (check_button_pressed_combo(controller->buttonDown, controller->buttonPressed, TOGGLE_CONT_STATUS_POLLING_COMBO)) {
                    gContStatusPollingReadyForInput = FALSE;
                    start_controller_status_polling();
                }
            } else if ((controller->buttonDown & TOGGLE_CONT_STATUS_POLLING_COMBO) != TOGGLE_CONT_STATUS_POLLING_COMBO) { //! this should only check the controller that pressed the combo to exit
                gContStatusPollingReadyForInput = TRUE;
            }
        } else {
            // Otherwise, if controllerData is NULL or the cooldown hasn't finished, zero out all of the inputs.
            controller->rawStickX      = 0;
            controller->rawStickY      = 0;
            controller->buttonPressed  = 0;
            controller->buttonReleased = 0;
            controller->buttonDown     = 0;
            controller->stickX         = 0;
            controller->stickY         = 0;
            controller->stickMag       = 0;
        }
    }
}

/**
 * General input handling function.
 */
void handle_input(OSMesg *mesg) {
    // If any controllers are plugged in, update the controller information.
    if (gControllerBits) {
        // Read the raw input data from the controllers.
        poll_controller_inputs(mesg);

        if (gContStatusPolling) {
            // Input handling for status polling.
            read_controller_inputs_status_polling();
        } else {
            // Input handling for normal gameplay.
            read_controller_inputs_normal();
        }
    } else if (!gContStatusPolling) {
        // Start controller status polling if all ports are empty and we're not already polling.
        start_controller_status_polling();
    }

    // Only poll controller status about twice per second.
    if (gContStatusPolling && ((gContStatusPollTimer % CONT_STATUS_POLLING_TIME) == 0)) {
        poll_controller_statuses(mesg);
    }

    gContStatusPollTimer++;

    profiler_update(PROFILER_TIME_CONTROLLERS);
}

/**
 * Initializes controllers on boot.
 */
void init_controllers(void) {
    // Init the controllers.
    osContInit(&gSIEventMesgQueue, &gControllerBits, gControllerStatuses);

    // Automatically assign controllers based on port order.
    assign_controllers_auto();
}
