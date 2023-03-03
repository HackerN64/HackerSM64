#include <ultra64.h>
#include <PR/os_internal.h>
#include "config.h"
#include "string.h"
#include "controller.h"
#include "engine/math_util.h"
#include "game_init.h"
#include "game_input.h"
#include "rumble_init.h"

OSPortInfo gPortInfo[MAXCONTROLLERS] = { 0 };

void __osSiGetAccess(void);
void __osSiRelAccess(void);

////////////////////
// contreaddata.c //
////////////////////

static void __osPackReadData(void);

/**
 * @brief Sets up PIF commands to poll controller inputs.
 * Unmodified from vanilla libultra, but __osPackReadData is modified.
 * Called by poll_controller_inputs.
 */
s32 osContStartReadDataEx(OSMesgQueue* mq) {
    s32 ret = 0;

    __osSiGetAccess();

    // If this was called twice in a row, there is no need to write the command again.
    if (__osContLastCmd != CONT_CMD_READ_BUTTON) {
        // Write the command to __osContPifRam.
        __osPackReadData();

        // Write __osContPifRam to the PIF RAM.
        ret = __osSiRawStartDma(OS_WRITE, &__osContPifRam);

        // Wait for the command to execute.
        osRecvMesg(mq, NULL, OS_MESG_BLOCK);
    }

    // Read the resulting __osContPifRam from the PIF RAM.
    ret = __osSiRawStartDma(OS_READ, &__osContPifRam);

    __osContLastCmd = CONT_CMD_READ_BUTTON;

    __osSiRelAccess();

    return ret;
}

/**
 * @brief Writes controller data to OSContPadEx and stores the controller center on first run.
 * Called by osContGetReadDataEx.
 */
static void __osContWriteGCNInputData(OSContPadEx* data, OSContCenter* contCenter, GCNButtons gcn, Analog16 stick, Analog16 c_stick, Analog16 trig) {
    N64Buttons n64 = { .raw = 0x0 };

    // The first time the controller is connected, store the origins for the controller's analog sticks.
    if (!contCenter->initialized) {
        contCenter->initialized = TRUE;
        contCenter->stick.x     = stick.x;
        contCenter->stick.y     = stick.y;
        contCenter->c_stick.x   = c_stick.x;
        contCenter->c_stick.y   = c_stick.y;
    }

    // Write the analog data.
    data->stick_x   = CLAMP_S8((s32)stick.x   - contCenter->stick.x  );
    data->stick_y   = CLAMP_S8((s32)stick.y   - contCenter->stick.y  );
    data->c_stick_x = CLAMP_S8((s32)c_stick.x - contCenter->c_stick.x);
    data->c_stick_y = CLAMP_S8((s32)c_stick.y - contCenter->c_stick.y);
    data->l_trig    = trig.l;
    data->r_trig    = trig.r;

    // Map GCN button bits to N64 button bits.
    n64.standard.A       = gcn.standard.A;
    n64.standard.B       = gcn.standard.B;
    n64.standard.Z       = (trig.l > GCN_TRIGGER_THRESHOLD);
    n64.standard.START   = gcn.standard.START;
    n64.standard.D_UP    = gcn.standard.D_UP;
    n64.standard.D_DOWN  = gcn.standard.D_DOWN;
    n64.standard.D_LEFT  = gcn.standard.D_LEFT;
    n64.standard.D_RIGHT = gcn.standard.D_RIGHT;
    n64.standard.RESET   = gcn.standard.X; // This bit normally gets set when L+R+START is pressed on an N64 controller to recalibrate the analog stick (which also unsets the START bit).
    n64.standard.unused  = gcn.standard.Y; // The N64 controller's unused bit.
    n64.standard.L       = gcn.standard.Z;
    n64.standard.R       = gcn.standard.R;
    n64.standard.C_UP    = (data->c_stick_y >  GCN_C_STICK_THRESHOLD);
    n64.standard.C_DOWN  = (data->c_stick_y < -GCN_C_STICK_THRESHOLD);
    n64.standard.C_LEFT  = (data->c_stick_x < -GCN_C_STICK_THRESHOLD);
    n64.standard.C_RIGHT = (data->c_stick_x >  GCN_C_STICK_THRESHOLD);

    // Write the button data.
    data->button = n64.raw;
}

/**
 * @brief Reads PIF command result written by __osPackReadData and converts it into OSContPadEx data.
 * Modified from vanilla libultra to handle GameCube controllers, skip empty/unassigned ports,
 *   and trigger status polling if an active controller is unplugged.
 * Called by poll_controller_inputs.
 */
void osContGetReadDataEx(OSContPadEx* data) {
    u8* ptr = (u8*)__osContPifRam.ramarray;
    __OSContReadFormat *readformatptr = NULL;
    OSPortInfo* portInfo = NULL;
    OSContCenter* contCenter = NULL;
    N64InputData n64Input;
    GCNInputData gcnInput;
    int port;

    for (port = 0; port < __osMaxControllers; port++) {
        portInfo = &gPortInfo[port];

        // Make sure this port has a controller plugged in, and if not status repolling, only poll assigned ports.
        if (portInfo->plugged && (gContStatusPolling || portInfo->playerNum)) {
            readformatptr = (__OSContReadFormat*)ptr;
            data->errno = CHNL_ERR(readformatptr->cmd);

            // If a controller being read was unplugged, start status polling on all 4 ports.
            if (data->errno & (CHNL_ERR_NORESP >> 4)) {
                start_controller_status_polling();
                return;
            }

            switch (readformatptr->send.cmdID) {
                case CONT_CMD_READ_BUTTON:
                    if (data->errno == (CONT_CMD_RX_SUCCESSFUL >> 4)) {
                        n64Input = (*(__OSContReadFormat*)ptr).recv.input;

                        data->button    = n64Input.buttons.raw;
                        data->stick_x   = n64Input.stick.x;
                        data->stick_y   = n64Input.stick.y;
                        data->c_stick_x = 0;
                        data->c_stick_y = 0;
                        data->l_trig    = 0;
                        data->r_trig    = 0;
                    }

                    ptr += sizeof(__OSContReadFormat);
                    break;

                case CONT_CMD_GCN_SHORT_POLL:
                    contCenter = &gPortInfo[port].contCenter;

                    if (data->errno == (CONT_CMD_RX_SUCCESSFUL >> 4)) {
                        gcnInput = (*(__OSContGCNShortPollFormat*)ptr).recv.input;
                        Analog16 c_stick, trig;

                        // The GameCube controller has various modes for returning the lower analog bits (4-bit vs. 8-bit).
                        switch ((*(__OSContGCNShortPollFormat*)ptr).send.analog_mode) {
                            default: // GCN_MODE_0_211, GCN_MODE_5_211, GCN_MODE_6_211, GCN_MODE_7_211
                                c_stick = gcnInput.m0.c_stick;
                                trig    = ANALOG8_TO_16(gcnInput.m0.trig);
                                break;
                            case GCN_MODE_1_121:
                                c_stick = ANALOG8_TO_16(gcnInput.m1.c_stick);
                                trig    = gcnInput.m1.trig;
                                break;
                            case GCN_MODE_2_112:
                                c_stick = ANALOG8_TO_16(gcnInput.m2.c_stick);
                                trig    = ANALOG8_TO_16(gcnInput.m2.trig);
                                break;
                            case GCN_MODE_3_220:
                                c_stick = gcnInput.m3.c_stick;
                                trig    = gcnInput.m3.trig;
                                break;
                            case GCN_MODE_4_202:
                                c_stick = gcnInput.m3.c_stick;
                                trig    = (Analog16){ 0x00, 0x00 };
                                break;
                        }

                        __osContWriteGCNInputData(data, contCenter, gcnInput.buttons, gcnInput.stick, c_stick, trig);
                    } else {
                        contCenter->initialized = FALSE;
                    }

                    ptr += sizeof(__OSContGCNShortPollFormat);
                    break;

                case CONT_CMD_GCN_LONG_POLL:
                    contCenter = &gPortInfo[port].contCenter;

                    if (data->errno == (CONT_CMD_RX_SUCCESSFUL >> 4)) {
                        gcnInput = (*(__OSContGCNLongPollFormat*)ptr).recv.input;

                        __osContWriteGCNInputData(data, contCenter, gcnInput.buttons, gcnInput.stick, gcnInput.c_stick, gcnInput.trig);
                    } else {
                        contCenter->initialized = FALSE;
                    }

                    ptr += sizeof(__OSContGCNLongPollFormat);
                    break;

                default:
                    osSyncPrintf("osContGetReadDataEx: Unknown input poll command: %.02X\n", readformatptr->send.cmdID);
                    return;
            }
        } else {
            // Skip empty channels/ports.
            ptr++;
        }

        data++;
    }
}

// Default N64 Controller Input Poll command:
static const __OSContReadFormat sN64WriteFormat = {
    .cmd.txsize         = sizeof(((__OSContReadFormat*)0)->send),
    .cmd.rxsize         = sizeof(((__OSContReadFormat*)0)->recv),
    .send.cmdID         = CONT_CMD_READ_BUTTON,
    .recv.input.raw     = -1, // 0xFFFF
};

// Default GCN Controller Input Short Poll command:
static const __OSContGCNShortPollFormat sGCNWriteFormatShort = {
    .cmd.txsize         = sizeof(((__OSContGCNShortPollFormat*)0)->send),
    .cmd.rxsize         = sizeof(((__OSContGCNShortPollFormat*)0)->recv),
    .send.cmdID         = CONT_CMD_READ_BUTTON,
    .send.analog_mode   = GCN_MODE_3_220,
    .send.rumble        = MOTOR_STOP,
    .recv.input.raw     = -1, // 0xFFFFFFFF
};

/**
 * @brief Writes PIF commands to poll controller inputs.
 * Modified from vanilla libultra to handle GameCube controllers and skip empty/unassigned ports.
 * Called by osContStartReadData and osContStartReadDataEx.
 */
static void __osPackReadData(void) {
    u8* ptr = (u8*)__osContPifRam.ramarray;
    OSPortInfo* portInfo = NULL;
    int port;

    bzero(__osContPifRam.ramarray, sizeof(__osContPifRam.ramarray));
    __osContPifRam.pifstatus = PIF_STATUS_EXE;

    for (port = 0; port < __osMaxControllers; port++) {
        portInfo = &gPortInfo[port];

        // Make sure this port has a controller plugged in, and if not status repolling, only poll assigned ports.
        if (portInfo->plugged && (gContStatusPolling || portInfo->playerNum)) {
            if (portInfo->type & CONT_CONSOLE_GCN) {
                (*(__OSContGCNShortPollFormat*)ptr)= sGCNWriteFormatShort;
                (*(__OSContGCNShortPollFormat*)ptr).send.rumble = portInfo->gcRumble;

                ptr += sizeof(__OSContGCNShortPollFormat);
            } else {
                (*(__OSContReadFormat*)ptr) = sN64WriteFormat;

                ptr += sizeof(__OSContReadFormat);
            }
        } else {
            // Empty channel/port, so leave a CONT_CMD_SKIP_CHNL (0x00) byte to tell the PIF to skip it.
            ptr++;
        }
    }

    *ptr = CONT_CMD_END;
}

/////////////////
// contquery.c //
/////////////////

void __osContGetInitDataEx(u8* pattern, OSContStatus* data);

/**
 * @brief Read status query data written by osContStartQuery.
 * odified from vanilla libultra to return bitpattern, similar to osContInit.
 * Called by poll_controller_statuses.
 */
void osContGetQueryEx(u8* bitpattern, OSContStatus* data) {
    __osContGetInitDataEx(bitpattern, data);
}

//////////////////
// controller.c //
//////////////////

/**
 * @brief Reads PIF command result written by __osPackRequestData and converts it into OSContStatus data.
 * Linker script will resolve references to the original function with this one instead.
 * Modified from vanilla libultra to set gPortInfo type and plugged status.
 * Called by osContInit, osContGetQuery, osContGetQueryEx, and osContReset.
 */
void __osContGetInitDataEx(u8* pattern, OSContStatus* data) {
    u8* ptr = (u8*)__osContPifRam.ramarray;
    __OSContRequestFormat requestHeader;
    OSPortInfo* portInfo = NULL;
    u8 bits = 0x0;
    int port;

    for (port = 0; port < __osMaxControllers; port++) {
        requestHeader = *(__OSContRequestFormat*)ptr;
        data->error = CHNL_ERR(requestHeader.cmd);

        if (data->error == (CONT_CMD_RX_SUCCESSFUL >> 4)) {
            portInfo = &gPortInfo[port];

            // Byteswap the SI identifier. This is done in vanilla libultra.
            data->type = ((requestHeader.recv.type.l << 8) | requestHeader.recv.type.h);

            // Check the type of controller device connected to the port.
            // Some mupen cores seem to send back a controller type of CONT_TYPE_NULL (0xFFFF) if the core doesn't initialize the input plugin quickly enough,
            //   so check for that and set the input type to N64 controller if so.
            portInfo->type = ((s16)data->type == (s16)CONT_TYPE_NULL) ? CONT_TYPE_NORMAL : data->type;

            // Set this port's status.
            data->status = requestHeader.recv.status.raw;
            portInfo->plugged = TRUE;
            bits |= (1 << port);
        }

        ptr += sizeof(requestHeader);
        data++;
    }

    *pattern = bits;
}

/////////////
// motor.c //
/////////////

// A buffer to hold the rumble commands for each port.
static OSPifRam __MotorDataBuf[MAXCONTROLLERS];

/**
 * @brief Turns controller rumble on or off.
 * Modified from vanilla libultra to handle GameCube controller rumble.
 * Called by osMotorStart, osMotorStop, and osMotorStopHard via macro.
 */
s32 __osMotorAccessEx(OSPfs* pfs, s32 flag) {
    s32 err = PFS_ERR_SUCCESS;
    int channel = pfs->channel;
    u8* ptr = (u8*)&__MotorDataBuf[channel];

    if (!(pfs->status & PFS_MOTOR_INITIALIZED)) {
        return PFS_ERR_INVALID;
    }

    if (gPortInfo[channel].type & CONT_CONSOLE_GCN) { // GCN Controllers.
        gPortInfo[channel].gcRumble = flag;
        __osContLastCmd = CONT_CMD_END;
    } else { // N64 Controllers.
        // N64 Controllers don't have MOTOR_STOP_HARD.
        if (flag == MOTOR_STOP_HARD) {
            flag = MOTOR_STOP;
        }

        __osSiGetAccess();

        // Set the PIF to be ready to run a command.
        __MotorDataBuf[channel].pifstatus = PIF_STATUS_EXE;

        // Leave a CONT_CMD_SKIP_CHNL (0x00) byte in __MotorDataBuf for each skipped channel.
        ptr += channel;

        __OSContRamWriteFormat* readformat = (__OSContRamWriteFormat*)ptr;

        // Set the entire block to either MOTOR_STOP or MOTOR_START.
        memset(readformat->send.data, flag, sizeof(readformat->send.data));

        __osContLastCmd = CONT_CMD_END;

        // Write __MotorDataBuf to the PIF RAM and then wait for the command to execute.
        __osSiRawStartDma(OS_WRITE, &__MotorDataBuf[channel]);
        osRecvMesg(pfs->queue, NULL, OS_MESG_BLOCK);
        // Read the resulting __MotorDataBuf from the PIF RAM and then wait for the command to execute.
        __osSiRawStartDma(OS_READ, &__MotorDataBuf[channel]);
        osRecvMesg(pfs->queue, NULL, OS_MESG_BLOCK);

        err = (readformat->cmd.rxsize & CHNL_ERR_MASK);
        if (!err) {
            if (!flag) {
                // MOTOR_STOP
                if (readformat->recv.datacrc != 0) { // 0xFF = Disconnected.
                    err = PFS_ERR_CONTRFAIL; // "Controller pack communication error"
                }
            } else {
                // MOTOR_START
                if (readformat->recv.datacrc != 0xEB) { // 0x14 = Uninitialized.
                    err = PFS_ERR_CONTRFAIL; // "Controller pack communication error"
                }
            }
        }

        __osSiRelAccess();
    }

    return err;
}

u8 __osContAddressCrc(u16 addr);
s32 __osPfsSelectBank(OSPfs* pfs, u8 bank);
s32 __osContRamRead(OSMesgQueue* mq, int channel, u16 address, u8* buffer);

/**
 * @brief Writes PIF commands to control the rumble pak.
 * Unmodified from vanilla libultra.
 * Called by osMotorInit and osMotorInitEx.
 */
static void _MakeMotorData(int channel, OSPifRam* mdata) {
    u8* ptr = (u8*)mdata->ramarray;
    __OSContRamWriteFormat ramwriteformat;
    int i;

    ramwriteformat.align       = CONT_CMD_NOP;
    ramwriteformat.cmd.txsize  = sizeof(ramwriteformat.send);
    ramwriteformat.cmd.rxsize  = sizeof(ramwriteformat.recv);
    ramwriteformat.send.cmdID  = CONT_CMD_WRITE_MEMPAK;
    ramwriteformat.send.addr.h = (CONT_BLOCK_RUMBLE >> 3);
    ramwriteformat.send.addr.l = (u8)(__osContAddressCrc(CONT_BLOCK_RUMBLE) | (CONT_BLOCK_RUMBLE << 5));

    // Leave a CONT_CMD_SKIP_CHNL (0x00) byte in mdata->ramarray for each skipped channel.
    if (channel != 0) {
        for (i = 0; i < channel; i++) {
            *ptr++ = CONT_CMD_SKIP_CHNL;
        }
    }

    *(__OSContRamWriteFormat*)ptr = ramwriteformat;
    ptr += sizeof(__OSContRamWriteFormat);
    *ptr = CONT_CMD_END;
}

/**
 * @brief Initializes the Rumble Pak.
 * Modified from vanilla libultra to ignore GameCube controllers.
 * Called by thread6_rumble_loop and cancel_rumble.
 */
s32 osMotorInitEx(OSMesgQueue* mq, OSPfs* pfs, int channel) {
    s32 err;
    u8 data[BLOCKSIZE];

    pfs->status     = PFS_STATUS_NONE;
    pfs->queue      = mq;
    pfs->channel    = channel;
    pfs->activebank = ACCESSORY_ID_NULL;

    if (!(gPortInfo[channel].type & CONT_CONSOLE_GCN)) {
        // Write probe value (ensure Transfer Pak is turned off).
        err = __osPfsSelectBank(pfs, ACCESSORY_ID_TRANSFER_OFF);
        if (err == PFS_ERR_NEW_PACK) {
            // Write probe value (Rumble bank).
            err = __osPfsSelectBank(pfs, ACCESSORY_ID_RUMBLE);
        }
        if (err != PFS_ERR_SUCCESS) {
            return err;
        }

        // Read probe value (1).
        err = __osContRamRead(mq, channel, CONT_BLOCK_DETECT, data);
        if (err == PFS_ERR_NEW_PACK) {
            err = PFS_ERR_CONTRFAIL; // "Controller pack communication error"
        }
        if (err != PFS_ERR_SUCCESS) {
            return err;
        }

        // Ensure the accessory is not a turned off Transfer Pak.
        if (data[BLOCKSIZE - 1] == ACCESSORY_ID_TRANSFER_OFF) {
            return PFS_ERR_DEVICE; // Wrong device
        }

        // Write probe value (Rumble bank).
        err = __osPfsSelectBank(pfs, ACCESSORY_ID_RUMBLE);
        if (err == PFS_ERR_NEW_PACK) {
            err = PFS_ERR_CONTRFAIL; // "Controller pack communication error"
        }
        if (err != PFS_ERR_SUCCESS) {
            return err;
        }

        // Read probe value (2).
        err = __osContRamRead(mq, channel, CONT_BLOCK_DETECT, data);
        if (err == PFS_ERR_NEW_PACK) {
            err = PFS_ERR_CONTRFAIL; // "Controller pack communication error"
        }
        if (err != PFS_ERR_SUCCESS) {
            return err;
        }

        // Ensure the accessory is a Rumble Pak.
        if (data[BLOCKSIZE - 1] != ACCESSORY_ID_RUMBLE) {
            return PFS_ERR_DEVICE; // Wrong device
        }

        // Write the PIF command.
        if (!(pfs->status & PFS_MOTOR_INITIALIZED)) {
            _MakeMotorData(channel, &__MotorDataBuf[channel]);
        }
    }

    pfs->status = PFS_MOTOR_INITIALIZED;

    return PFS_ERR_SUCCESS;
}
