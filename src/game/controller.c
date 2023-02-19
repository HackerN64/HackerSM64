#include <ultra64.h>
#include "config.h"

#include "string.h"
#include "PR/os_internal.h"
#include "controller.h"
#include "engine/math_util.h"
#include "game_init.h"
#include "game_input.h"
#include "rumble_init.h"

// Joybus commands
// from: http://en64.shoutwiki.com/wiki/SI_Registers_Detailed#CONT_CMD_Usage
static const OSContCmdData sContCmds[] = {
    // N64 Controller
    [CONT_CMD_REQUEST_STATUS    ] = { .tx =  1, .rx =  3 }, // Read Controller type/status.
    [CONT_CMD_READ_BUTTON       ] = { .tx =  1, .rx =  4 }, // Read Input Status.
    // Controller Accessory
    [CONT_CMD_READ_MEMPAK       ] = { .tx =  3, .rx = 33 }, // Read Controller Accessory.
    [CONT_CMD_WRITE_MEMPAK      ] = { .tx = 35, .rx =  1 }, // Write Controller Accessory.
    // EEPROM
    [CONT_CMD_READ_EEPROM       ] = { .tx =  2, .rx =  8 }, // Read EEPROM.
    [CONT_CMD_WRITE_EEPROM      ] = { .tx = 10, .rx =  1 }, // Write EEPROM.
    // RTC
    [CONT_CMD_READ_RTC_STATUS   ] = { .tx =  1, .rx =  3 }, // RTC Info.
    [CONT_CMD_READ_RTC_BLOCK    ] = { .tx =  2, .rx =  9 }, // Read RTC Block.
    [CONT_CMD_WRITE_RTC_BLOCK   ] = { .tx = 10, .rx =  1 }, // Write RTC Block.
    // VRU
    [CONT_CMD_READ36_VOICE      ] = { .tx =  3, .rx = 37 }, // Read from VRx.
    [CONT_CMD_WRITE20_VOICE     ] = { .tx = 23, .rx =  1 }, // Write to VRx.
    [CONT_CMD_READ2_VOICE       ] = { .tx =  3, .rx =  3 }, // Read Status VRx.
    [CONT_CMD_WRITE4_VOICE      ] = { .tx =  7, .rx =  1 }, // Write Config VRx.
    [CONT_CMD_SWRITE_VOICE      ] = { .tx =  3, .rx =  1 }, // Write Init VRx (Clear Dictionary).
    // Randnet Keyboard
    [CONT_CMD_KEY_PRESS_REQUEST ] = { .tx =  2, .rx =  7 }, // Randnet Keyboard Read Keypress.
    //! No room for 64GB read/write commands 0x13 and 0x14 (https://pastebin.com/06VzdT3w).
    // GBA
    [CONT_CMD_READ_GBA          ] = { .tx =  3, .rx = 33 }, // Read GBA.
    [CONT_CMD_WRITE_GBA         ] = { .tx = 35, .rx =  1 }, // Write GBA.
    // Game ID https://gitlab.com/pixelfx-public/n64-game-id.
    [CONT_CMD_WRITE_GAME_ID     ] = { .tx = 10, .rx =  1 }, // The EverDrive sends the game ID on the first controller port on boot using this..
    // GCN Steering Wheel
    [CONT_CMD_GCN_WHEEL_FEEDBACK] = { .tx =  3, .rx =  8 }, // Force Feedback (unverified tx/rx).
    // GCN CONTROLLER
    [CONT_CMD_GCN_SHORT_POLL    ] = { .tx =  3, .rx =  8 }, // GameCube Shortpoll (input).
    [CONT_CMD_GCN_READ_ORIGIN   ] = { .tx =  1, .rx = 10 }, // GameCube Read Origin.
    [CONT_CMD_GCN_CALIBRATE     ] = { .tx =  3, .rx = 10 }, // GameCube Recalibrate.
    [CONT_CMD_GCN_LONG_POLL     ] = { .tx =  3, .rx = 10 }, // GameCube Longpoll (input).
    // GCN Keyboard
    [CONT_CMD_GCN_POLL_KEYBOARD ] = { .tx =  3, .rx =  8 }, // GameCube Keyboard Poll.

    [CONT_CMD_RESET             ] = { .tx =  1, .rx =  3 }, // Reset/Info.
};

OSPortInfo gPortInfo[MAXCONTROLLERS] = { 0 };

void __osSiGetAccess(void);
void __osSiRelAccess(void);

////////////////////
// contreaddata.c //
////////////////////

static void __osPackReadData(void);
static u16 __osTranslateGCNButtons(u16 buttons, s32 c_stick_x, s32 c_stick_y);

/**
 * @brief Sets up PIF commands to poll controller inputs.
 * Unmodified from vanilla libultra, but __osPackReadData is modified.
 * Called by poll_controller_inputs (thread5_game_loop) and thread2_crash_screen.
 */
s32 osContStartReadDataEx(OSMesgQueue* mq) {
    s32 ret = 0;

    __osSiGetAccess();

    if (__osContLastCmd != CONT_CMD_READ_BUTTON) {
        __osPackReadData();
        ret = __osSiRawStartDma(OS_WRITE, &__osContPifRam);
        osRecvMesg(mq, NULL, OS_MESG_BLOCK);
    }

    ret = __osSiRawStartDma(OS_READ, &__osContPifRam);
    __osContLastCmd = CONT_CMD_READ_BUTTON;
    __osSiRelAccess();

    return ret;
}

/**
 * @brief Reads PIF command result written by __osPackReadData and converts it into OSContPadEx data.
 * Modified from vanilla libultra to handle GameCube controllers, skip empty/unassigned ports,
 *   and trigger status polling if an active controller is unplugged.
 * Called by poll_controller_inputs (thread5_game_loop) and thread2_crash_screen.
 */
void osContGetReadDataEx(OSContPadEx* data) {
    u8* ptr = (u8*)__osContPifRam.ramarray;
    __OSContReadFormat readformat;
    __OSContGCNShortPollFormat readformatgcn;
    OSPortInfo* portInfo = NULL;
    int port;

    for (port = 0; port < __osMaxControllers; port++, data++) {
        portInfo = &gPortInfo[port];

        if (portInfo->plugged && (gContStatusPolling || portInfo->playerNum)) {
            // If a controller being read was unplugged, start status polling on all 4 ports.
            if (CHNL_ERR((*(__OSContReadFormat*)ptr).cmd) & (CHNL_ERR_NORESP >> 4)) {
                start_controller_status_polling();
                return;
            }

            if (portInfo->type & CONT_CONSOLE_GCN) {
                OSContCenter* contCenter = &gPortInfo[port].contCenter;
                s32 stick_x, stick_y, c_stick_x, c_stick_y;
                readformatgcn = *(__OSContGCNShortPollFormat*)ptr;
                data->errno = CHNL_ERR(readformatgcn.cmd);

                if (data->errno == 0) {
                    if (!contCenter->initialized) {
                        contCenter->initialized = TRUE;
                        contCenter->stick_x     = readformatgcn.stick_x;
                        contCenter->stick_y     = readformatgcn.stick_y;
                        contCenter->c_stick_x   = readformatgcn.c_stick_x;
                        contCenter->c_stick_y   = readformatgcn.c_stick_y;
                    }

                    data->stick_x   = stick_x   = CLAMP_S8(((s32)readformatgcn.stick_x  ) - contCenter->stick_x  );
                    data->stick_y   = stick_y   = CLAMP_S8(((s32)readformatgcn.stick_y  ) - contCenter->stick_y  );
                    data->c_stick_x = c_stick_x = CLAMP_S8(((s32)readformatgcn.c_stick_x) - contCenter->c_stick_x);
                    data->c_stick_y = c_stick_y = CLAMP_S8(((s32)readformatgcn.c_stick_y) - contCenter->c_stick_y);
                    data->button    = __osTranslateGCNButtons(readformatgcn.button, c_stick_x, c_stick_y);
                    data->l_trig    = readformatgcn.l_trig;
                    data->r_trig    = readformatgcn.r_trig;
                } else {
                    contCenter->initialized = FALSE;
                }

                ptr += sizeof(__OSContGCNShortPollFormat);
            } else {
                readformat = *(__OSContReadFormat*)ptr;
                data->errno = CHNL_ERR(readformat.cmd);

                if (data->errno == 0) {
                    data->button    = readformat.button;
                    data->stick_x   = readformat.stick_x;
                    data->stick_y   = readformat.stick_y;
                    data->c_stick_x = 0;
                    data->c_stick_y = 0;
                    data->l_trig    = 0;
                    data->r_trig    = 0;
                }

                ptr += sizeof(__OSContReadFormat);
            }
        } else {
            // Skip empty channel/ports.
            ptr++;
        }
    }
}

/**
 * @brief Formats a single aligned PIF command.
 * Called by __osPackReadData and _MakeMotorData.
 */
static void __osMakeRequestData(OSPifRamChCmd* readformat, enum ContCmds cmd) {
    readformat->txsize = sContCmds[cmd].tx;
    readformat->rxsize = sContCmds[cmd].rx;
    readformat->cmd = cmd;
}

/**
 * @brief Writes PIF commands to poll controller inputs.
 * Modified from vanilla libultra to handle GameCube controllers and skip empty/unassigned ports.
 * Called by osContStartReadData and osContStartReadDataEx.
 */
static void __osPackReadData(void) {
    u8* ptr = (u8*)__osContPifRam.ramarray;
    __OSContReadFormat readformat;
    __OSContGCNShortPollFormat readformatgcn;
    OSPortInfo* portInfo = NULL;
    int port;

    bzero(__osContPifRam.ramarray, sizeof(__osContPifRam.ramarray));

    __osContPifRam.pifstatus = PIF_STATUS_EXE;

    __osMakeRequestData(&readformat.cmd, CONT_CMD_READ_BUTTON);
    readformat.button         = 0xFFFF;
    readformat.stick_x        = -1;
    readformat.stick_y        = -1;

    __osMakeRequestData(&readformatgcn.cmd, CONT_CMD_GCN_SHORT_POLL);
    // The GameCube controller has various modes for returning the lower analog bits (4-bit vs. 8-bit).
    // Mode 3 uses 8 bits for both c-stick and shoulder triggers.
    // https://github.com/dolphin-emu/dolphin/blob/master/Source/Core/Core/HW/SI/SI_DeviceGCController.cpp
    // https://github.com/extremscorner/gba-as-controller/blob/gc/controller/source/main.iwram.c
    readformatgcn.analog_mode = 3;
    readformatgcn.rumble      = 0;
    readformatgcn.button      = 0xFFFF;
    readformatgcn.stick_x     = -1;
    readformatgcn.stick_y     = -1;

    for (port = 0; port < __osMaxControllers; port++) {
        portInfo = &gPortInfo[port];

        if (portInfo->plugged && (gContStatusPolling || portInfo->playerNum)) {
            if (portInfo->type & CONT_CONSOLE_GCN) {
                readformatgcn.rumble = portInfo->gcRumble;
                *(__OSContGCNShortPollFormat*)ptr = readformatgcn;
                ptr += sizeof(__OSContGCNShortPollFormat);
            } else {
                *(__OSContReadFormat*)ptr = readformat;
                ptr += sizeof(__OSContReadFormat);
            }
        } else {
            // Empty channel/port, so Leave a CONT_CMD_SKIP_CHNL (0x00) byte to tell the PIF to skip it.
            ptr++;
        }
    }

    *ptr = CONT_CMD_END;
}

/**
 * @brief Maps GCN input bits to N64 input bits.
 * Called by osContGetReadData and osContGetReadDataEx.
 */
static u16 __osTranslateGCNButtons(u16 buttons, s32 c_stick_x, s32 c_stick_y) {
    N64Buttons n64 = { .raw = 0x0     };
    GCNButtons gcn = { .raw = buttons };

    n64.buttons.A       = gcn.buttons.A;
    n64.buttons.B       = gcn.buttons.B;
    n64.buttons.Z       = gcn.buttons.Z;
    n64.buttons.START   = gcn.buttons.START;
    n64.buttons.D_UP    = gcn.buttons.D_UP;
    n64.buttons.D_DOWN  = gcn.buttons.D_DOWN;
    n64.buttons.D_LEFT  = gcn.buttons.D_LEFT;
    n64.buttons.D_RIGHT = gcn.buttons.D_RIGHT;
    n64.buttons.X       = gcn.buttons.X; // N64 reset bit.
    n64.buttons.Y       = gcn.buttons.Y; // N64 unused bit.
    n64.buttons.L       = gcn.buttons.L;
    n64.buttons.R       = gcn.buttons.R;
    n64.buttons.C_UP    = (c_stick_y >  GCN_C_STICK_THRESHOLD);
    n64.buttons.C_DOWN  = (c_stick_y < -GCN_C_STICK_THRESHOLD);
    n64.buttons.C_LEFT  = (c_stick_x < -GCN_C_STICK_THRESHOLD);
    n64.buttons.C_RIGHT = (c_stick_x >  GCN_C_STICK_THRESHOLD);

    return n64.raw;
}
/////////////////
// contquery.c //
/////////////////

void __osContGetInitDataEx(u8* pattern, OSContStatus* data);

/**
 * @brief Read status query data written by osContStartQuery.
 * odified from vanilla libultra to return bitpattern, similar to osContInit.
 * Called by poll_controller_status.
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
    __OSContRequesFormat requestHeader;
    OSPortInfo* portInfo = NULL;
    u8 bits = 0x0;
    int port;

    for (port = 0; port < __osMaxControllers; port++, ptr += sizeof(requestHeader), data++) {
        requestHeader = *(__OSContRequesFormat*)ptr;
        data->error = CHNL_ERR(requestHeader.cmd);

        if (data->error == 0) {
            portInfo = &gPortInfo[port];

            // Byteswap the SI identifier.
            data->type = ((requestHeader.typel << 8) | requestHeader.typeh);

            // Check the type of controller
            // Some mupen cores seem to send back a controller type of 0xFFFF (null) if the core doesn't initialize the input plugin quickly enough,
            //   so check for that and set the input type to N64 controller if so.
            portInfo->type = ((s16)data->type == -1) ? CONT_TYPE_NORMAL : data->type;

            // Set this port's status.
            data->status = requestHeader.status;
            portInfo->plugged = TRUE;
            bits |= (1 << port);
        }
    }

    *pattern = bits;
}

/////////////
// motor.c //
/////////////

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

    if (gPortInfo[channel].type & CONT_CONSOLE_GCN) {
        gPortInfo[channel].gcRumble = flag;
        __osContLastCmd = CONT_CMD_END;
    } else {
        // N64 Controllers don't have MOTOR_STOP_HARD.
        if (flag == MOTOR_STOP_HARD) {
            flag = MOTOR_STOP;
        }

        __osSiGetAccess();
        __MotorDataBuf[channel].pifstatus = PIF_STATUS_EXE;
        ptr += channel;

        __OSContRamReadFormat* readformat = (__OSContRamReadFormat*)ptr;

        memset(readformat->data, flag, sizeof(readformat->data));

        __osContLastCmd = CONT_CMD_END;
        __osSiRawStartDma(OS_WRITE, &__MotorDataBuf[channel]);
        osRecvMesg(pfs->queue, NULL, OS_MESG_BLOCK);
        __osSiRawStartDma(OS_READ, &__MotorDataBuf[channel]);
        osRecvMesg(pfs->queue, NULL, OS_MESG_BLOCK);

        err = (readformat->cmd.rxsize & CHNL_ERR_MASK);
        if (!err) {
            if (!flag) {
                // MOTOR_STOP
                if (readformat->datacrc != 0) {
                    err = PFS_ERR_CONTRFAIL;
                }
            } else {
                // MOTOR_START
                if (readformat->datacrc != 0xEB) {
                    err = PFS_ERR_CONTRFAIL;
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
    __OSContRamReadFormat ramreadformat;
    int i;

    ramreadformat.align = CONT_CMD_NOP;
    __osMakeRequestData(&ramreadformat.cmd, CONT_CMD_WRITE_MEMPAK);
    ramreadformat.addrh = (CONT_BLOCK_RUMBLE >> 3);
    ramreadformat.addrl = (u8)(__osContAddressCrc(CONT_BLOCK_RUMBLE) | (CONT_BLOCK_RUMBLE << 5));

    if (channel != 0) {
        for (i = 0; i < channel; i++) {
            *ptr++ = CONT_CMD_SKIP_CHNL;
        }
    }

    *(__OSContRamReadFormat*)ptr = ramreadformat;
    ptr += sizeof(__OSContRamReadFormat);
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

    pfs->queue = mq;
    pfs->channel = channel;
    pfs->activebank = ACCESSORY_ID_NULL;
    pfs->status = PFS_STATUS_NONE;

    if (!(gPortInfo[channel].type & CONT_CONSOLE_GCN)) {
        err = __osPfsSelectBank(pfs, ACCESSORY_ID_TRANSFER_OFF);
        if (err == PFS_ERR_NEW_PACK) {
            err = __osPfsSelectBank(pfs, ACCESSORY_ID_RUMBLE);
        }
        if (err != PFS_ERR_SUCCESS) {
            return err;
        }

        err = __osContRamRead(mq, channel, CONT_BLOCK_DETECT, data);
        if (err == PFS_ERR_NEW_PACK) {
            err = PFS_ERR_CONTRFAIL; // "Controller pack communication error"
        }
        if (err != PFS_ERR_SUCCESS) {
            return err;
        }

        if (data[BLOCKSIZE - 1] == ACCESSORY_ID_TRANSFER_OFF) {
            return PFS_ERR_DEVICE; // Wrong device
        }

        err = __osPfsSelectBank(pfs, ACCESSORY_ID_RUMBLE);
        if (err == PFS_ERR_NEW_PACK) {
            err = PFS_ERR_CONTRFAIL; // "Controller pack communication error"
        }
        if (err != PFS_ERR_SUCCESS) {
            return err;
        }

        err = __osContRamRead(mq, channel, CONT_BLOCK_DETECT, data);
        if (err == PFS_ERR_NEW_PACK) {
            err = PFS_ERR_CONTRFAIL; // "Controller pack communication error"
        }
        if (err != PFS_ERR_SUCCESS) {
            return err;
        }

        if (data[BLOCKSIZE - 1] != ACCESSORY_ID_RUMBLE) {
            return PFS_ERR_DEVICE; // Wrong device
        }

        if (!(pfs->status & PFS_MOTOR_INITIALIZED)) {
            _MakeMotorData(channel, &__MotorDataBuf[channel]);
        }
    }

    pfs->status = PFS_MOTOR_INITIALIZED;

    return PFS_ERR_SUCCESS;
}
