#include "config.h"
#include "PR/os_internal.h"
#include "controller.h"
#include "engine/math_util.h"
#include "game_init.h"
#include "rumble_init.h"

// Joybus commands
// from: http://en64.shoutwiki.com/wiki/SI_Registers_Detailed#CONT_CMD_Usage
static const OSContCmdData sContCmds[] = {
    // N64 Controller
    [CONT_CMD_REQUEST_STATUS    ] = { .tx =  1, .rx =  3 }, // Info
    [CONT_CMD_READ_BUTTON       ] = { .tx =  1, .rx =  4 }, // Input Status //! .rx is 8 for GCN rumble only
    // Controller Accessory
    [CONT_CMD_READ_MEMPAK       ] = { .tx =  3, .rx = 33 }, // Read Controller Accessory
    [CONT_CMD_WRITE_MEMPAK      ] = { .tx = 35, .rx =  1 }, // Write Controller Accessory
    // EEPROM
    [CONT_CMD_READ_EEPROM       ] = { .tx =  2, .rx =  8 }, // Read EEPROM
    [CONT_CMD_WRITE_EEPROM      ] = { .tx = 10, .rx =  1 }, // Write EEPROM
    // RTC
    [CONT_CMD_READ_RTC_STATUS   ] = { .tx =  1, .rx =  3 }, // RTC Info
    [CONT_CMD_READ_RTC_BLOCK    ] = { .tx =  2, .rx =  9 }, // Read RTC Block
    [CONT_CMD_WRITE_RTC_BLOCK   ] = { .tx = 10, .rx =  1 }, // Write RTC Block
    // VRU
    [CONT_CMD_READ36_VOICE      ] = { .tx =  3, .rx = 37 }, // Read from VRx
    [CONT_CMD_WRITE20_VOICE     ] = { .tx = 23, .rx =  1 }, // Write to VRx
    [CONT_CMD_READ2_VOICE       ] = { .tx =  3, .rx =  3 }, // Read Status VRx
    [CONT_CMD_WRITE4_VOICE      ] = { .tx =  7, .rx =  1 }, // Write Config VRx
    [CONT_CMD_SWRITE_VOICE      ] = { .tx =  3, .rx =  1 }, // Write Init VRx
    // Randnet Keyboard
    [CONT_CMD_KEY_PRESS_REQUEST ] = { .tx =  2, .rx =  7 }, // Randnet Keyboard Read Keypress
    // GBA //! No room for 64GB read/write commands (https://pastebin.com/06VzdT3w)
    [CONT_CMD_READ_GBA          ] = { .tx =  3, .rx = 33 }, // Read GBA
    [CONT_CMD_WRITE_GBA         ] = { .tx = 35, .rx =  1 }, // Write GBA
    // Game ID (BlueRetro controller adapter)
    [CONT_CMD_SET_GAME_ID       ] = { .tx =  0, .rx =  0 }, // Configure BlueRetro controller adapter (unverified tx/rx)
    // GCN Steering Wheel
    [CONT_CMD_GCN_WHEEL_FEEDBACK] = { .tx =  3, .rx =  8 }, // Force Feedback (unverified tx/rx)
    // GCN CONTROLLER
    [CONT_CMD_GCN_SHORT_POLL    ] = { .tx =  3, .rx =  8 }, // GameCube Shortpoll (status)
    [CONT_CMD_GCN_READ_ORIGIN   ] = { .tx =  1, .rx = 10 }, // GameCube Read Origin
    [CONT_CMD_GCN_CALIBRATE     ] = { .tx =  3, .rx = 10 }, // GameCube Recalibrate
    [CONT_CMD_GCN_LONG_POLL     ] = { .tx =  3, .rx = 10 }, // GameCube Longpoll (input)
    // GCN Keyboard
    [CONT_CMD_GCN_POLL_KEYBOARD ] = { .tx =  3, .rx =  8 }, // GameCube Keyboard Poll

    [CONT_CMD_RESET             ] = { .tx =  1, .rx =  3 }, // Reset/Info
};

OSPortInfo gPortInfo[MAXCONTROLLERS] = { 0 };

void __osSiGetAccess(void);
void __osSiRelAccess(void);

////////////////////
// contreaddata.c //
////////////////////

static void __osPackReadData(void);
static u16 __osTranslateGCNButtons(u16, s32, s32);

/**
 * @brief Sets up PIF commands to poll controller inputs.
 * Only __osPackReadData is modified from vanilla.
 * Called by handle_input (thread2_crash_screen), thread_5_game_loop and gd_init_controllers
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
 * Called by thread2_crash_screen and handle_input (thread5_game_loop).
 */
void osContGetReadDataEx(OSContPadEx* data) {
    u8* ptr = (u8*)__osContPifRam.ramarray;
    __OSContReadFormat readformat;
    __OSContGCNShortPollFormat readformatgcn;
    OSPortInfo *portInfo = NULL;
    int i;

    for (i = 0; i < __osMaxControllers; i++, data++) {
        portInfo = &gPortInfo[i];

        if (portInfo->plugged && (gContStatusPolling || portInfo->playerNum)) {
            // Go to the next 4-byte boundary.
            ptr = (u8 *)ALIGN4(ptr);

            // If a controller being read was unplugged, start status polling on all 4 ports.
            if (CHNL_ERR(*(__OSContReadFormat*)ptr) & (CHNL_ERR_NORESP >> 4)) {
                start_controller_status_polling();
                return;
            }

            if (portInfo->type & CONT_CONSOLE_GCN) {
                OSContCenter *contCenter = &gPortInfo[i].contCenter;
                s32 stick_x, stick_y, c_stick_x, c_stick_y;
                readformatgcn = *(__OSContGCNShortPollFormat*)ptr;
                data->errno = CHNL_ERR(readformatgcn);

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
                data->errno = CHNL_ERR(readformat);

                if (data->errno == 0) {
                    data->stick_x   = readformat.stick_x;
                    data->stick_y   = readformat.stick_y;
                    data->button    = readformat.button;
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
void __osMakeRequestData(void *readformat, enum ContCmds cmd) {
    OSPifRamChCmd *data = (OSPifRamChCmd *)readformat;
    data->align = CONT_CMD_NOP;
    data->txsize = sContCmds[cmd].tx;
    data->rxsize = sContCmds[cmd].rx;
    data->cmd = cmd;
}

/**
 * @brief Writes PIF commands to poll controller inputs.
 * Called by osContStartReadDataEx
 */
static void __osPackReadData(void) {
    u8* ptr = (u8*)__osContPifRam.ramarray;
    __OSContReadFormat readformat;
    __OSContGCNShortPollFormat readformatgcn;
    OSPortInfo *portInfo = NULL;
    int numSkipped = 0;
    int i;

    bzero(__osContPifRam.ramarray, sizeof(__osContPifRam.ramarray));

    __osContPifRam.pifstatus = CONT_CMD_EXE;

    __osMakeRequestData(&readformat, CONT_CMD_READ_BUTTON);
    readformat.button         = 0xFFFF;
    readformat.stick_x        = -1;
    readformat.stick_y        = -1;

    __osMakeRequestData(&readformatgcn, CONT_CMD_GCN_SHORT_POLL);
    readformatgcn.analog_mode = 3;
    readformatgcn.rumble      = 0;
    readformatgcn.button      = 0xFFFF;
    readformatgcn.stick_x     = -1;
    readformatgcn.stick_y     = -1;

    for (i = 0; i < __osMaxControllers; i++) {
        portInfo = &gPortInfo[i];

        if (portInfo->plugged && (gContStatusPolling || portInfo->playerNum)) {
            // Go to the next 4-byte boundary.
            ptr = (u8 *)ALIGN4(ptr);

            if (numSkipped) {
                // Channel skipping:
                // If channels were skipped, fill the previous 4 bytes with a CONT_CMD_SKIP_CHNL (0x00) byte for
                //   each skipped channel, and set the rest of those bytes to CONT_CMD_NOP (0xFF) for alignment.
                // The PIF chip ignores bytes that are 0xFF without incrementing the channel counter,
                //   while bytes of 0x00 increment the channel counter.
                *(u32 *)(ptr - 4) = ~BITMASK(numSkipped * 8);
                numSkipped = 0;
            }

            if (portInfo->type & CONT_CONSOLE_GCN) {
                readformatgcn.rumble = portInfo->gcRumble;
                *(__OSContGCNShortPollFormat*)ptr = readformatgcn;
                ptr += sizeof(__OSContGCNShortPollFormat);
            } else {
                *(__OSContReadFormat*)ptr = readformat;
                ptr += sizeof(__OSContReadFormat);
            }
        } else {
            // Skip empty channel/ports.
            ptr++;
            numSkipped++;
        }
    }

    *ptr = CONT_CMD_END;
}

/**
 * @brief Maps GCN input bits to N64 input bits.
 * Called by osContGetReadDataEx.
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
    n64.buttons.X       = gcn.buttons.X;
    n64.buttons.Y       = gcn.buttons.Y;
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

void __osContGetInitDataEx(u8*, OSContStatus*);

/**
 * @brief Modified version of osContGetQuer to return bitpattern like osContInit.
 * Called by poll_controller_status.
 */
void osContGetQueryEx(u8 *bitpattern, OSContStatus* data) {
    __osContGetInitDataEx(bitpattern, data);
}

//////////////////
// controller.c //
//////////////////

/**
 * @brief Reads PIF command result written by __osPackRequestData and converts it into OSContStatus data.
 * Linker script will resolve references to the original function with this one instead.
 * Called by osContInit, osContGetQuery, and osContReset
 */
void __osContGetInitDataEx(u8* pattern, OSContStatus* data) {
    u8* ptr = (u8*)__osContPifRam.ramarray;
    __OSContRequesFormat requestHeader;
    OSPortInfo *portInfo = NULL;
    u8 bits = 0x0;
    int i;

    for (i = 0; i < __osMaxControllers; i++, ptr += sizeof(requestHeader), data++) {
        requestHeader = *(__OSContRequesFormat*)ptr;
        data->error = CHNL_ERR(requestHeader);

        if (data->error == 0) {
            portInfo = &gPortInfo[i];

            // Byteswap the SI identifier.
            data->type = ((requestHeader.typel << 8) | requestHeader.typeh);

            // Check the type of controller
            // Some mupen cores seem to send back a controller type of 0xFFFF (null) if the core doesn't initialize the input plugin quickly enough,
            //   so check for that and set the input type to N64 controller if so.
            portInfo->type = ((s16)data->type == -1) ? CONT_TYPE_NORMAL : data->type;

            // Set this port's status.
            data->status = requestHeader.status;
            portInfo->plugged = TRUE;
            bits |= (1 << i);
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
 * Called by osMotorStart, osMotorStop, and osMotorStopHard via macro.
 */
s32 __osMotorAccessEx(OSPfs* pfs, s32 vibrate) {
    s32 ret = 0;
    int channel = pfs->channel;
    u8* ptr = (u8*)&__MotorDataBuf[channel];
    int i;

    if (!(pfs->status & PFS_MOTOR_INITIALIZED)) {
        return PFS_ERR_INVALID;
    }

    if (gPortInfo[channel].type & CONT_CONSOLE_GCN) {
        gPortInfo[channel].gcRumble = vibrate;
        __osContLastCmd = CONT_CMD_END;
    } else {
        // N64 Controllers don't have MOTOR_STOP_HARD.
        if (vibrate == MOTOR_STOP_HARD) {
            vibrate = MOTOR_STOP;
        }

        __osSiGetAccess();
        __MotorDataBuf[channel].pifstatus = CONT_CMD_EXE;
        ptr += channel;

        for (i = 0; i < BLOCKSIZE; i++) {
            READFORMAT(ptr)->data[i] = vibrate;
        }

        __osContLastCmd = CONT_CMD_END;
        __osSiRawStartDma(OS_WRITE, &__MotorDataBuf[channel]);
        osRecvMesg(pfs->queue, NULL, OS_MESG_BLOCK);
        __osSiRawStartDma(OS_READ, &__MotorDataBuf[channel]);
        osRecvMesg(pfs->queue, NULL, OS_MESG_BLOCK);

        ret = (READFORMAT(ptr)->rxsize & CHNL_ERR_MASK);
        if (!ret) {
            if (!vibrate) {
                // MOTOR_STOP
                if (READFORMAT(ptr)->datacrc != 0) {
                    ret = PFS_ERR_CONTRFAIL;
                }
            } else {
                // MOTOR_START
                if (READFORMAT(ptr)->datacrc != 0xEB) {
                    ret = PFS_ERR_CONTRFAIL;
                }
            }
        }
        __osSiRelAccess();
    }

    return ret;
}

u8 __osContAddressCrc(u16 addr);
s32 __osPfsSelectBank(OSPfs *pfs, u8 bank);
s32 __osContRamRead(OSMesgQueue *mq, int channel, u16 address, u8 *buffer);

/**
 * @brief Writes PIF commands to control the rumble pak.
 * Called by osMotorInitEx.
 */
static void _MakeMotorData(int channel, OSPifRam *mdata) {
    u8 *ptr = (u8 *)mdata->ramarray;
    __OSContRamReadFormat ramreadformat;
    int i;

    __osMakeRequestData(&ramreadformat, CONT_CMD_WRITE_MEMPAK);
    ramreadformat.addrh = (CONT_BLOCK_RUMBLE >> 3);
    ramreadformat.addrl = (u8)(__osContAddressCrc(CONT_BLOCK_RUMBLE) | (CONT_BLOCK_RUMBLE << 5));

    if (channel != 0) {
        for (i = 0; i < channel; i++) {
            *ptr++ = CONT_CMD_SKIP_CHNL;
        }
    }

    *READFORMAT(ptr) = ramreadformat;
    ptr += sizeof(__OSContRamReadFormat);
    *ptr = CONT_CMD_END;
}

/**
 * @brief Initializes the Rumble Pak.
 * Called by thread6_rumble_loop and cancel_rumble.
 */
s32 osMotorInitEx(OSMesgQueue *mq, OSPfs *pfs, int channel) {
    s32 ret;
    u8 temp[BLOCKSIZE];

    pfs->queue = mq;
    pfs->channel = channel;
    pfs->activebank = ACCESSORY_ID_NULL;
    pfs->status = 0;

    if (!(gPortInfo[channel].type & CONT_CONSOLE_GCN)) {
        ret = __osPfsSelectBank(pfs, ACCESSORY_ID_TRANSFER_OFF);

        if (ret == PFS_ERR_NEW_PACK) {
            ret = __osPfsSelectBank(pfs, ACCESSORY_ID_RUMBLE);
        }

        if (ret != 0) {
            return ret;
        }

        ret = __osContRamRead(mq, channel, CONT_BLOCK_DETECT, temp);

        if (ret == PFS_ERR_NEW_PACK) {
            ret = PFS_ERR_CONTRFAIL; // "Controller pack communication error"
        }

        if (ret != 0) {
            return ret;
        }

        if (temp[BLOCKSIZE - 1] == ACCESSORY_ID_TRANSFER_OFF) {
            return PFS_ERR_DEVICE;
        }

        ret = __osPfsSelectBank(pfs, ACCESSORY_ID_RUMBLE);
        if (ret == PFS_ERR_NEW_PACK) {
            ret = PFS_ERR_CONTRFAIL; // "Controller pack communication error"
        }

        if (ret != 0) {
            return ret;
        }

        ret = __osContRamRead(mq, channel, CONT_BLOCK_DETECT, temp);
        if (ret == PFS_ERR_NEW_PACK) {
            ret = PFS_ERR_CONTRFAIL; // "Controller pack communication error"
        }

        if (ret != 0) {
            return ret;
        }

        if (temp[BLOCKSIZE - 1] != ACCESSORY_ID_RUMBLE) {
            return PFS_ERR_DEVICE;
        }

        if (!(pfs->status & PFS_MOTOR_INITIALIZED)) {
            _MakeMotorData(channel, &__MotorDataBuf[channel]);
        }
    }

    pfs->status = PFS_MOTOR_INITIALIZED;

    return 0;
}
