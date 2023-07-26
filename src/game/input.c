#include <ultra64.h>
#include "config.h"

#include "joybus.h"
#include "engine/math_util.h"
#include "main.h"
#include "game_init.h"
#include "rumble.h"
#include "input.h"
#include "profiling.h"
#include "vc_check.h"
#include "vc_ultra.h"

// Player Controllers.
struct Controller gControllers[MAXCONTROLLERS];
// Defined controller slots. Anything above MAX_NUM_PLAYERS will be unused.
struct Controller* const gPlayer1Controller = &gControllers[0];
struct Controller* const gPlayer2Controller = &gControllers[1];
struct Controller* const gPlayer3Controller = &gControllers[2];
struct Controller* const gPlayer4Controller = &gControllers[3];

// OS Controllers.
OSContStatus gControllerStatuses[MAXCONTROLLERS];
OSContPadEx gControllerPads[MAXCONTROLLERS];

u8    gNumPlayers                     = 0;      // The number of controllers currently assigned to a player.
u8    gControllerBits                 = 0b0000; // Which ports have a controller connected to them (low to high).
_Bool gContStatusPolling              = FALSE;  // Whether controller status polling is enabled.
_Bool gContStatusPollingIsBootMode    = FALSE;  // Whether controller status polling was triggered on boot and should be invisible.
_Bool gContStatusPollingReadyForInput = TRUE;   // Whether all inputs have been released after starting status repolling.
u32   gContStatusPollTimer            = 0;      // Time since controller status repolling has started.

// Title Screen Demo Handler.
struct DemoInput* gCurrDemoInput = NULL;
#ifndef DISABLE_DEMO
u16 gDemoInputListID = 0;

// Demo controller.
OSContStatus gDemoStatusData = {
    .type = CONT_TYPE_NORMAL,
};
OSContPadEx gDemoControllerData;
struct Controller gDemoControllers[1] = {
    {
        .statusData     = &gDemoStatusData,
        .controllerData = &gDemoControllerData,
        .port           = 0,
    }
};
struct Controller* const gDemoController = &gDemoControllers[0];
#endif

/**
 * @brief Takes the updated controller struct and calculate the new x, y, and distance floats.
 *
 * @param[in,out] controller The controller to operate on.
 */
static void adjust_analog_stick(struct Controller* controller) {
    const s16 deadzone = ((controller->statusData->type & CONT_CONSOLE_GCN) ? 12 : 8);
    const s16 offset = (deadzone - 2);
    const f32 max_stick_mag = 64.0f;

    // Reset the controller's x and y floats.
    controller->stickX = 0.0f;
    controller->stickY = 0.0f;

    // Modulate the rawStickX and rawStickY to be the new f32 values by adding/subtracting 6.
    if (controller->rawStickX <= -deadzone) controller->stickX = (controller->rawStickX + offset);
    if (controller->rawStickX >=  deadzone) controller->stickX = (controller->rawStickX - offset);
    if (controller->rawStickY <= -deadzone) controller->stickY = (controller->rawStickY + offset);
    if (controller->rawStickY >=  deadzone) controller->stickY = (controller->rawStickY - offset);

    // Calculate f32 magnitude from the center by vector length.
    controller->stickMag = sqrtf(sqr(controller->stickX) + sqr(controller->stickY));

    // Magnitude cannot exceed max_stick_mag (64.0f). If it does, modify the values
    // appropriately to flatten the values down to the allowed maximum value.
    if (controller->stickMag > max_stick_mag) {
        controller->stickX *= (max_stick_mag / controller->stickMag);
        controller->stickY *= (max_stick_mag / controller->stickMag);
        controller->stickMag = max_stick_mag;
    }
}

/**
 * @brief Updates the controller struct.
 *
 * @param[in,out] controller The controller to operate on.
 */
void process_controller_data(struct Controller* controller) {
    OSContPadEx* controllerData = controller->controllerData;

    controller->rawStickX = controllerData->stick.x;
    controller->rawStickY = controllerData->stick.y;
    // Lock buttons that were used in the combo to exit status polling until they are released.
    controllerData->lockedButton &= controllerData->button;
    u16 button = controllerData->button &= ~controllerData->lockedButton;
    controller->buttonPressed = (~controller->buttonDown & button);
    controller->buttonReleased = (~button & controller->buttonDown);
    // 0.5x A presses are a good meme.
    controller->buttonDown = button;

    adjust_analog_stick(controller);
}

#ifndef DISABLE_DEMO
/**
 * @brief If a demo sequence exists, this will run the demo input list until it is complete.
 */
void run_demo_inputs(void) {
    struct Controller* controller = gDemoController;
    OSContPadEx* controllerData = controller->controllerData;

    // Eliminate the unused bits.
    controllerData->button &= VALID_BUTTONS;

    // Check if a demo inputs list exists and if so,
    // run the active demo input list.
    if (gCurrDemoInput != NULL) {
        // The timer variable being 0 at the current input means the demo is over.
        // Set the button to the INPUT_END_DEMO mask to end the demo.
        if (gCurrDemoInput->timer == 0) {
            controllerData->stick = (Analog_s8){ 0x00, 0x00 };
            controllerData->button = INPUT_END_DEMO;
        } else {
            // Perform the demo inputs by assigning the current button mask and the stick inputs.
            controllerData->stick.x = gCurrDemoInput->rawStickX;
            controllerData->stick.y = gCurrDemoInput->rawStickY;

            // To assign the demo input, the button information is stored in
            // an 8-bit mask rather than a 16-bit mask. this is because only
            // A, B, Z, Start, and the C-Buttons are used in a demo, as bits
            // in that order. In order to assign the mask, we need to take the
            // upper 4 bits (A, B, Z, and Start) and shift then left by 8 to
            // match the correct input mask. We then add this to the masked
            // lower 4 bits to get the correct button mask.
            controllerData->button = (
                ((gCurrDemoInput->buttonMask & 0xF0) << 8) |
                ((gCurrDemoInput->buttonMask & 0x0F) << 0)
            );

            // Run the current demo input's timer down. if it hits 0, advance the demo input list.
            gCurrDemoInput->timer--;
            if (gCurrDemoInput->timer == 0) {
                gCurrDemoInput++;
            }
        }

        process_controller_data(controller);
    }
}
#endif // !DISABLE_DEMO

/**
 * @brief Check if a combo has finished being pressed on this frame.
 *
 * @param[in ] buttonDown    The buttons that are currently held down.
 * @param[in ] buttonPressed The buttons that are freshly pressed on this frame.
 * @param[out] combo         The button combo to check for.
 * @returns Boolean whether the check is successful.
 */
ALWAYS_INLINE _Bool check_button_pressed_combo(u16 buttonDown, u16 buttonPressed, u16 combo) {
    return (((buttonDown & combo) == combo) && (buttonPressed & combo));
}

/**
 * @brief Links a controller struct to the appropriate status and pad.
 *
 * @param[out] controller The controller to link.
 * @param[in ] port The port to get the data from.
 */
void assign_controller_data(struct Controller* controller, int port) {
    controller->statusData = &gControllerStatuses[port];
    controller->controllerData = &gControllerPads[port];
    controller->port = port;
}

/**
 * @brief Initialize the controller structs to point at the OSCont information.
 * Automatically assignins controller numbers based on port order.
 */
void assign_controllers_by_port_order(void) {
    OSPortInfo* portInfo = NULL;
    int port, cont = 0;
    int lastUsedPort = -1;

    // Loop over the 4 ports and link the controller structs to the appropriate status and pad.
    // The game allows you to have a controller plugged into any port in order to play the game.
    for (port = 0; port < MAXCONTROLLERS; port++) {
        if (cont >= MAX_NUM_PLAYERS) {
            break;
        }

        portInfo = &gPortInfo[port];

        // Is the controller plugged in?
        if (portInfo->plugged) {
            portInfo->playerNum = (cont + 1);

            assign_controller_data(&gControllers[cont], port);

            lastUsedPort = port;

            cont++;
        }
    }

    // Disable the ports after the last used one.
    osContSetCh(lastUsedPort + 1);
}

/**
 * @brief Initializes the controller structs to point at the OSCont information.
 * Assigns controllers based on assigned data from status polling.
 */
void assign_controllers_by_player_num(void) {
    OSPortInfo* portInfo = NULL;
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
 * @brief Reads raw controller input data.
 *
 * @param[in] mesg The SI message to wait for.
 * Called by
 */
static void poll_controller_inputs(OSMesg* mesg) {
    block_until_rumble_pak_free();

    osStartRead_impl(&gSIEventMesgQueue, CONT_CMD_READ_BUTTON);
    osRecvMesg(&gSIEventMesgQueue, mesg, OS_MESG_BLOCK);
    osContGetReadDataEx(gControllerPads);

    release_rumble_pak_control();
}

/**
 * @brief Reads GCN controller analog origins data.
 *
 * @param[in] mesg The SI message to wait for.
 */
static void poll_controller_gcn_origins(OSMesg* mesg) {
    block_until_rumble_pak_free();

    osStartRead_impl(&gSIEventMesgQueue, CONT_CMD_GCN_READ_ORIGIN);
    osRecvMesg(&gSIEventMesgQueue, mesg, OS_MESG_BLOCK);
    osContGetReadDataEx(gControllerPads);

    release_rumble_pak_control();
}

/**
 * @brief Checks for new controller status data on all ports.
 *
 * @param[in] mesg The SI message to wait for.
 */
static void poll_controller_statuses(OSMesg* mesg) {
    block_until_rumble_pak_free();

    osContSetCh(MAXCONTROLLERS);
    osContStartQuery(&gSIEventMesgQueue);
    osRecvMesg(&gSIEventMesgQueue, mesg, OS_MESG_BLOCK);
    osContGetQueryEx(&gControllerBits, gControllerStatuses);

    release_rumble_pak_control();
}

/**
 * @brief Starts polling for new controllers and open the UI.
 * @param[in] isBootMode Boolean. Only used when MAX_SUPPORTED_CONTROLLERS is 1. Triggers a separate mode where the UI is
 *   invisible and the controller with the first detected input (including analog sticks) becomes player 1.
 */
void start_controller_status_polling(_Bool isBootMode) {
    if (isBootMode) {
        gContStatusPollingReadyForInput = TRUE;
    }

    gContStatusPollingIsBootMode = isBootMode;
    gContStatusPolling = TRUE;
    gContStatusPollTimer = 0;
    gNumPlayers = 0;

    bzero(gPortInfo,           sizeof(gPortInfo          ));
    bzero(gControllers,        sizeof(gControllers       ));
    bzero(gControllerStatuses, sizeof(gControllerStatuses));

    cancel_rumble();
}

/**
 * @brief Stop polling for new controllers and assign them to their player numbers.
 *
 * @param[in,out] pad The controller that stopped the polling.
 */
void stop_controller_status_polling(OSContPadEx* pad) {
    if (gContStatusPollingIsBootMode) {
        gContStatusPollingIsBootMode = FALSE;
    } else {
        // Lock pressed buttons when exiting the UI until they are released
        //   to prevent interfering with gameplay.
        pad->lockedButton = pad->button;
    }
    gContStatusPolling = FALSE;
    gContStatusPollTimer = 0;
    assign_controllers_by_player_num();
#ifdef EEP
    // EEPROM probe for save data.
    gEepromProbe = gIsVC
                 ? osEepromProbeVC(&gSIEventMesgQueue)
                 : osEepromProbe  (&gSIEventMesgQueue);
#endif
    cancel_rumble();
}

/**
 * @brief Checks whether any analog sticks on a controller pad are outside a deadzone.
 *
 * @param[in] pad      The controller pad to check.
 * @param[in] deadzone The deadzone to compare with.
 * @returns Boolean whether any input was detected.
 */
static _Bool detect_analog_stick_input(OSContPadEx* pad, const s8 deadzone) {
    return (
        abss(pad->stick.x  ) > deadzone ||
        abss(pad->stick.y  ) > deadzone ||
        abss(pad->c_stick.x) > deadzone ||
        abss(pad->c_stick.y) > deadzone
    );
}

/**
 * @brief Assign player numbers to controllers based on player input.
 */
void read_controller_inputs_status_polling(void) {
    OSPortInfo* portInfo = NULL;
    u16 totalInput = 0x0;

    // Read inputs from all four ports when status polling.
    for (int port = 0; port < MAXCONTROLLERS; port++) {
        portInfo = &gPortInfo[port];

        if (portInfo->plugged) {
            OSContPadEx* pad = &gControllerPads[port];
            u16 button = pad->button;
            totalInput |= button;

            if (gContStatusPollingReadyForInput) {
                // If a button is pressed on an unassigned controller, assign it the current player number.
                if (
                    !portInfo->playerNum &&
                    (
                        button ||
                        (
                            gContStatusPollingIsBootMode &&
                            detect_analog_stick_input(pad, 20)
                        ) // Only check analog sticks in boot mode.
                    )
                ) {
                    portInfo->playerNum = ++gNumPlayers;
                }
#if (defined(ALLOW_STATUS_REPOLLING_COMBO) && (MAX_NUM_PLAYERS > 1))
                u16 pressed = (~portInfo->statusPollButtons & button);

                // If the combo is pressed, stop polling and assign the current controllers.
                if (
                    !gContStatusPollingIsBootMode &&
                    portInfo->playerNum &&
                    check_button_pressed_combo(button, pressed, TOGGLE_CONT_STATUS_POLLING_COMBO)
                ) {
                    gContStatusPollingReadyForInput = FALSE;
                    stop_controller_status_polling(pad);
                    return;
                }
#endif
                // If we've exceeded the number of controllers, stop polling and assign the current controllers.
                if (
                    gNumPlayers >= __builtin_popcount(gControllerBits) ||
                    gNumPlayers >= MAX_NUM_PLAYERS
                ) {
                    stop_controller_status_polling(pad);
                    return;
                }

                portInfo->statusPollButtons = button;
            }
        } else {
            portInfo->statusPollButtons = 0x0000;
        }
    }

    // Wait for all inputs to be realseased before checking for player assignment inputs or the combo being entered again.
    if (!totalInput) {
        gContStatusPollingReadyForInput = TRUE;
    }
}

/**
 * @brief Updates the controller struct with available inputs if present.
 */
void read_controller_inputs_normal(void) {
    for (s32 cont = 0; cont < MAX_NUM_PLAYERS; cont++) {
        struct Controller* controller = &gControllers[cont];

        // If we're receiving inputs, update the controller struct with the new button info.
        if (controller->controllerData != NULL) {
            process_controller_data(controller);
#ifdef ALLOW_STATUS_REPOLLING_COMBO
            if (check_button_pressed_combo(controller->buttonDown, controller->buttonPressed, TOGGLE_CONT_STATUS_POLLING_COMBO)) {
                gContStatusPollingReadyForInput = FALSE;
                start_controller_status_polling(FALSE);
            }
#endif
        } else {
            // Otherwise, if controllerData is NULL or the cooldown hasn't finished, zero out all of the inputs.
            controller->rawStickX      = 0;
            controller->rawStickY      = 0;
            controller->buttonPressed  = 0x0000;
            controller->buttonReleased = 0x0000;
            controller->buttonDown     = 0x0000;
            controller->stickX         = 0.0f;
            controller->stickY         = 0.0f;
            controller->stickMag       = 0.0f;
        }
    }
}

/**
 * @brief Checks whether to repoll for GCN analog origins data, and repolls if so.
 *
 * @param[in] mesg The SI message to wait for.
 */
void check_repoll_gcn_origins(OSMesg* mesg) {
    _Bool updateOrigins = FALSE;

    for (int port = 0; port < __osMaxControllers; port++) {
        if (gControllerPads[port].origins.updateOrigins) {
            updateOrigins = TRUE;
            break;
        }
    }

    if (updateOrigins) {
        poll_controller_gcn_origins(mesg);
    }
}

/**
 * @brief General input handling function.
 *
 * @param[in] mesg The SI message to wait for.
 */
void handle_input(OSMesg* mesg) {
#ifndef DISABLE_DEMO
    run_demo_inputs();
#endif

    // If any controllers are plugged in, update the controller information.
    if (gControllerBits) {
        // Read GCN controller origins.
        check_repoll_gcn_origins(mesg);

        // Read the raw input data from the controllers.
        poll_controller_inputs(mesg);

        // Separate input handling functions for status polling and normal gameplay.
        if (gContStatusPolling) {
            // Input handling for status polling.
            read_controller_inputs_status_polling();
        }

        // Check this separately so input can start on the same frame.
        // This is not an else because 'gContStatusPolling' can get set in 'read_controller_inputs_status_polling'
        if (!gContStatusPolling) {
            // Input handling for normal gameplay.
            read_controller_inputs_normal();
        }
    } else if (!gContStatusPolling) {
        // Start controller status polling if all ports are empty and we're not already polling.
        start_controller_status_polling(FALSE);
    }

    if (gContStatusPolling) {
        // Only poll controller status about twice per second.
        if (gContStatusPollTimer == 0) {
            gContStatusPollTimer = CONT_STATUS_POLLING_TIME;
            poll_controller_statuses(mesg);
        } else {
            gContStatusPollTimer--;
        }
    }
}

/**
 * @brief Initializes controllers on boot.
 */
void init_controllers(void) {
    // Init the controllers.
    osContInit(&gSIEventMesgQueue, &gControllerBits, gControllerStatuses);

#if (MAX_NUM_PLAYERS > 1)
    // Automatically assign controllers based on port order.
    assign_controllers_by_port_order();
#else
    // The controller with the first detected input becomes player 1.
    start_controller_status_polling(TRUE);
#endif
}
