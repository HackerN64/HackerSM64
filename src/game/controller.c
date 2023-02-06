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
    // GCN CONTROLLER
    [CONT_CMD_GCN_SHORT_POLL    ] = { .tx =  3, .rx =  8 }, // GameCube Shortpoll
    [CONT_CMD_GCN_READ_ORIGIN   ] = { .tx =  1, .rx = 10 }, // GameCube Read Origin
    [CONT_CMD_GCN_CALIBRATE     ] = { .tx =  3, .rx = 10 }, // GameCube Calibrate
    [CONT_CMD_GCN_LONG_POLL     ] = { .tx =  3, .rx = 10 }, // GameCube Longpoll

    [CONT_CMD_RESET             ] = { .tx =  1, .rx =  3 }, // Reset/Info
};

void __osSiGetAccess(void);
void __osSiRelAccess(void);

////////////////////////
// Gamecube additions //
////////////////////////

ControllerCenter gGamecubeControllerCenters[MAXCONTROLLERS] = { 0 };
OSPortInfo gPortInfo[MAXCONTROLLERS] = { 0 };

////////////////////
// contreaddata.c //
////////////////////

static void __osPackReadData(void);
static u16 __osTranslateGCNButtons(u16, s32, s32);


// Called by threads
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

// Called by read_controller_inputs
void osContGetReadDataEx(OSContPadEx* data) {
    u8* ptr = (u8*)__osContPifRam.ramarray;
    __OSContReadFormat readformat;
    __OSContGCNShortPollFormat readformatgcn;
    int i;

    for (i = 0; i < __osMaxControllers; i++, data++) {
        OSPortInfo *portInfo = &gPortInfo[i];

        if (portInfo->plugged && (gRepollingControllers || portInfo->playerNum)) {
            // Go to the next 4-byte boundary.
            ptr = (u8 *)ALIGN4(ptr);

            if (CHNL_ERR(*(__OSContReadFormat*)ptr) & (CHNL_ERR_NORESP >> 4)) {
                start_repolling_controllers();
                return;
            }

            if (portInfo->type & CONT_GCN) {
                s32 stick_x, stick_y, c_stick_x, c_stick_y;
                readformatgcn = *(__OSContGCNShortPollFormat*)ptr;
                data->errno = CHNL_ERR(readformatgcn);

                if (data->errno == 0) {
                    if (!gGamecubeControllerCenters[i].initialized) {
                        gGamecubeControllerCenters[i].initialized = TRUE;
                        gGamecubeControllerCenters[i].stick_x     = readformatgcn.stick_x;
                        gGamecubeControllerCenters[i].stick_y     = readformatgcn.stick_y;
                        gGamecubeControllerCenters[i].c_stick_x   = readformatgcn.c_stick_x;
                        gGamecubeControllerCenters[i].c_stick_y   = readformatgcn.c_stick_y;
                    }

                    data->stick_x   = stick_x   = CLAMP_S8(((s32)readformatgcn.stick_x  ) - gGamecubeControllerCenters[i].stick_x  );
                    data->stick_y   = stick_y   = CLAMP_S8(((s32)readformatgcn.stick_y  ) - gGamecubeControllerCenters[i].stick_y  );
                    data->c_stick_x = c_stick_x = CLAMP_S8(((s32)readformatgcn.c_stick_x) - gGamecubeControllerCenters[i].c_stick_x);
                    data->c_stick_y = c_stick_y = CLAMP_S8(((s32)readformatgcn.c_stick_y) - gGamecubeControllerCenters[i].c_stick_y);
                    data->button    = __osTranslateGCNButtons(readformatgcn.button, c_stick_x, c_stick_y);
                    data->l_trig    = readformatgcn.l_trig;
                    data->r_trig    = readformatgcn.r_trig;
                } else {
                    gGamecubeControllerCenters[i].initialized = FALSE;
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

void __osMakeRequestData(void *readformat, enum ContCmds cmd) {
    OSPifRamChCmd *data = (OSPifRamChCmd *)readformat;
    data->align = CONT_CMD_NOP;
    data->txsize = sContCmds[cmd].tx;
    data->rxsize = sContCmds[cmd].rx;
    data->cmd = cmd;
}

// Called by osContStartReadDataEx
static void __osPackReadData(void) {
    u8* ptr = (u8*)__osContPifRam.ramarray;
    __OSContReadFormat readformat;
    __OSContGCNShortPollFormat readformatgcn;
    int skipped = 0;
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
        OSPortInfo *portInfo = &gPortInfo[i];

        if (portInfo->plugged && (gRepollingControllers || portInfo->playerNum)) {
            // Go to the next 4-byte boundary.
            ptr = (u8 *)ALIGN4(ptr);

            if (skipped) {
                // If channels were skipped, fill the previous 4 bytes with a CONT_CMD_SKIP_CHNL (0x00)
                //   byte for each skipped channel, and CONT_CMD_NOP (0xFF) for alignment.
                // The PIF chip ignores bytes that are 0xFF without incrementing the channel counter,
                //   while bytes of 0x00 increment the channel counter.
                *(u32 *)(ptr - 4) = ~BITMASK(skipped * 8);
                skipped = 0;
            }

            if (portInfo->type & CONT_GCN) {
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
            skipped++;
        }
    }

    *ptr = CONT_CMD_END;
}

// Called by osContGetReadDataEx
static u16 __osTranslateGCNButtons(u16 buttons, s32 c_stick_x, s32 c_stick_y) {
    u16 ret = 0x0;

    // Face buttons
    if (buttons & CONT_GCN_A    ) ret |= A_BUTTON;
    if (buttons & CONT_GCN_B    ) ret |= B_BUTTON;
    if (buttons & CONT_GCN_START) ret |= START_BUTTON;
    if (buttons & CONT_GCN_X    ) ret |= GCN_X_BUTTON;
    if (buttons & CONT_GCN_Y    ) ret |= GCN_Y_BUTTON;

    // Triggers & Z
    if (buttons & CONT_GCN_Z    ) ret |= Z_TRIG;
    if (buttons & CONT_GCN_R    ) ret |= R_TRIG;
    if (buttons & CONT_GCN_L    ) ret |= L_TRIG;

    // D-Pad
    if (buttons & CONT_GCN_UP   ) ret |= U_JPAD;
    if (buttons & CONT_GCN_DOWN ) ret |= D_JPAD;
    if (buttons & CONT_GCN_LEFT ) ret |= L_JPAD;
    if (buttons & CONT_GCN_RIGHT) ret |= R_JPAD;

    // C-stick to C-buttons
    if (c_stick_x >  GCN_C_STICK_THRESHOLD) ret |= R_CBUTTONS;
    if (c_stick_x < -GCN_C_STICK_THRESHOLD) ret |= L_CBUTTONS;
    if (c_stick_y >  GCN_C_STICK_THRESHOLD) ret |= U_CBUTTONS;
    if (c_stick_y < -GCN_C_STICK_THRESHOLD) ret |= D_CBUTTONS;

    return ret;
}

/////////////////
// contquery.c //
/////////////////

void __osContGetInitDataEx(u8*, OSContStatus*);

void osContGetQueryEx(u8 *bitpattern, OSContStatus* data) {
    __osContGetInitDataEx(bitpattern, data);
}

//////////////////
// controller.c //
//////////////////

// Linker script will resolve references to the original function with this one instead.
// Called by osContInit, osContGetQuery, and osContReset
void __osContGetInitDataEx(u8* pattern, OSContStatus* data) {
    u8* ptr = (u8*)__osContPifRam.ramarray;
    __OSContRequesFormat requestHeader;
    u8 bits = 0;
    int i;

    for (i = 0; i < __osMaxControllers; i++, ptr += sizeof(requestHeader), data++) {
        requestHeader = *(__OSContRequesFormat*)ptr;
        data->error = CHNL_ERR(requestHeader);
        if (data->error == 0) {
            data->type = ((requestHeader.typel << 8) | requestHeader.typeh);
            OSPortInfo *portInfo = &gPortInfo[i];

            // Check the type of controller
            // Some mupen cores seem to send back a controller type of 0xFFFF if the core doesn't initialize the input plugin quickly enough,
            //   so check for that and set the input type to N64 controller if so.
            if ((s16)data->type == -1) {
                portInfo->type = CONT_TYPE_NORMAL;
            } else {
                portInfo->type = data->type;
            }

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

// osMotorStart & osMotorStop
s32 __osMotorAccessEx(OSPfs* pfs, s32 vibrate) {
    s32 ret = 0;
    u8* ptr = (u8*)&__MotorDataBuf[pfs->channel];
    int i;

    if (!(pfs->status & PFS_MOTOR_INITIALIZED)) {
        return PFS_ERR_INVALID;
    }

    if (gPortInfo[pfs->channel].type & CONT_GCN) {
        gPortInfo[pfs->channel].gcRumble = vibrate;
        __osContLastCmd = CONT_CMD_END;
    } else {
        if (vibrate == MOTOR_STOP_HARD) {
            vibrate = MOTOR_STOP;
        }

        __osSiGetAccess();
        __MotorDataBuf[pfs->channel].pifstatus = CONT_CMD_EXE;
        ptr += pfs->channel;

        for (i = 0; i < BLOCKSIZE; i++) {
            READFORMAT(ptr)->data[i] = vibrate;
        }

        __osContLastCmd = CONT_CMD_END;
        __osSiRawStartDma(OS_WRITE, &__MotorDataBuf[pfs->channel]);
        osRecvMesg(pfs->queue, NULL, OS_MESG_BLOCK);
        __osSiRawStartDma(OS_READ, &__MotorDataBuf[pfs->channel]);
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

s32 osMotorInitEx(OSMesgQueue *mq, OSPfs *pfs, int channel) {
    s32 ret;
    u8 temp[BLOCKSIZE];

    pfs->queue = mq;
    pfs->channel = channel;
    pfs->activebank = 0xFF;
    pfs->status = 0;

    if (gPortInfo[channel].type == CONT_TYPE_NORMAL) {
        ret = __osPfsSelectBank(pfs, 0xFE);

        if (ret == PFS_ERR_NEW_PACK) {
            ret = __osPfsSelectBank(pfs, MOTOR_ID);
        }

        if (ret != 0) {
            return ret;
        }

        ret = __osContRamRead(mq, channel, CONT_BLOCK_DETECT, temp);

        if (ret == PFS_ERR_NEW_PACK) {
            ret = PFS_ERR_CONTRFAIL;
        }

        if (ret != 0) {
            return ret;
        }

        if (temp[BLOCKSIZE - 1] == 0xFE) {
            return PFS_ERR_DEVICE;
        }

        ret = __osPfsSelectBank(pfs, MOTOR_ID);
        if (ret == PFS_ERR_NEW_PACK) {
            ret = PFS_ERR_CONTRFAIL;
        }

        if (ret != 0) {
            return ret;
        }

        ret = __osContRamRead(mq, channel, CONT_BLOCK_DETECT, temp);
        if (ret == PFS_ERR_NEW_PACK) {
            ret = PFS_ERR_CONTRFAIL;
        }

        if (ret != 0) {
            return ret;
        }

        if (temp[31] != 0x80) {
            return PFS_ERR_DEVICE;
        }

        if (!(pfs->status & PFS_MOTOR_INITIALIZED)) {
            _MakeMotorData(channel, &__MotorDataBuf[channel]);
        }
    }

    pfs->status = PFS_MOTOR_INITIALIZED;

    return 0;
}
