#include "config.h"
#include "PR/os_internal.h"
#include "engine/math_util.h"
#include "game_init.h"

/////////////////////////////////////////////////
// Libultra structs and macros (from ultralib) //
/////////////////////////////////////////////////

#define CHNL_ERR(format) (((format).rxsize & CHNL_ERR_MASK) >> 4)

typedef struct
{
    /* 0x00 */ u32 ramarray[15];
    /* 0x3C */ u32 pifstatus;
} OSPifRam; // size = 0x40

typedef struct {
    /* 0x00 */ u8 align;
    /* 0x01 */ u8 txsize;
    /* 0x02 */ u8 rxsize;
    /* 0x03 */ u8 cmd;
} OSPifRamChCmd; // size = 0x04

typedef struct
{
    /* 0x00 */ u8 align;
    /* 0x01 */ u8 txsize;
    /* 0x02 */ u8 rxsize;
    /* 0x03 */ u8 cmd;
    /* 0x04 */ u16 button;
    /* 0x06 */ s8 stick_x;
    /* 0x07 */ s8 stick_y;
} __OSContReadFormat; // size = 0x08

typedef struct
{
    /* 0x00 */ u8 align;
    /* 0x01 */ u8 txsize;
    /* 0x02 */ u8 rxsize;
    /* 0x03 */ u8 cmd;
    /* 0x04 */ u8 typeh;
    /* 0x05 */ u8 typel;
    /* 0x06 */ u8 status;
    /* 0x07 */ u8 dummy1;
} __OSContRequesFormat; // size = 0x08

typedef struct
{
    /* 0x00 */ u8 txsize;
    /* 0x01 */ u8 rxsize;
    /* 0x02 */ u8 cmd;
    /* 0x03 */ u8 typeh;
    /* 0x04 */ u8 typel;
    /* 0x05 */ u8 status;
} __OSContRequesFormatShort; // size = 0x06

typedef struct
{
    /* 0x00 */ u8 align;
    /* 0x01 */ u8 txsize;
    /* 0x02 */ u8 rxsize;
    /* 0x03 */ u8 cmd;
    /* 0x04 */ u8 addrh;
    /* 0x05 */ u8 addrl;
    /* 0x06 */ u8 data[BLOCKSIZE];
    /* 0x26 */ u8 datacrc;
} __OSContRamReadFormat; // size = 0x27

extern OSPifRam __osContPifRam;
extern u8 __osMaxControllers;
extern u8 __osContLastCmd;

// Controller accessory addresses
// https://github.com/joeldipops/TransferBoy/blob/master/docs/TransferPakReference.md

// Accesory detection
#define CONT_ADDR_DETECT    0x8000
// Rumble
#define CONT_ADDR_RUMBLE    0xC000
// Controller Pak / Transfer Pak
#define CONT_ADDR_GB_POWER  0x8000 // Same as the detection address, but semantically different
#define CONT_ADDR_GB_BANK   0xA000
#define CONT_ADDR_GB_STATUS 0xB000

// Addresses sent to controller accessories are in blocks, not bytes
#define CONT_BLOCKS(x) ((x) / BLOCKSIZE)

// Block addresses of the above
#define CONT_BLOCK_DETECT    CONT_BLOCKS(CONT_ADDR_DETECT)
#define CONT_BLOCK_RUMBLE    CONT_BLOCKS(CONT_ADDR_RUMBLE)
#define CONT_BLOCK_GB_POWER  CONT_BLOCKS(CONT_ADDR_GB_POWER)
#define CONT_BLOCK_GB_BANK   CONT_BLOCKS(CONT_ADDR_GB_BANK)
#define CONT_BLOCK_GB_STATUS CONT_BLOCKS(CONT_ADDR_GB_STATUS)

typedef struct {
    /* 0x00 */ u8 tx;
    /* 0x01 */ u8 rx;
} ContCmdData; // size = 0x02

enum ContCmds {
    // N64 Controller
    CONT_CMD_REQUEST_STATUS,
    CONT_CMD_READ_BUTTON,
    // Controller Accessory
    CONT_CMD_READ_MEMPAK,
    CONT_CMD_WRITE_MEMPAK,
    // EEPROM
    CONT_CMD_READ_EEPROM,
    CONT_CMD_WRITE_EEPROM,
    // RTC
    CONT_CMD_READ_RTC_STATUS,
    CONT_CMD_READ_RTC_BLOCK,
    CONT_CMD_WRITE_RTC_BLOCK,
    // VRU
    CONT_CMD_READ36_VOICE,
    CONT_CMD_WRITE20_VOICE,
    CONT_CMD_READ2_VOICE,
    CONT_CMD_WRITE4_VOICE,
    CONT_CMD_SWRITE_VOICE,
    // Randnet Keyboard
    CONT_CMD_KEY_PRESS_REQUEST = 0x13,
    // GCN Controller
    CONT_CMD_GCN_SHORT_POLL = 0x40,
    CONT_CMD_GCN_READ_ORIGIN,
    CONT_CMD_GCN_CALIBRATE,
    CONT_CMD_GCN_LONG_POLL,

    CONT_CMD_RESET = 0xFF,
};

// Joybus commands
// from: http://en64.shoutwiki.com/wiki/SI_Registers_Detailed#CONT_CMD_Usage
static const ContCmdData sContCmds[] = {
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
    [CONT_CMD_READ36_VOICE      ] = { .tx =  3, .rx = 37 }, // Read from VRx //! is .rx 25 or 37?
    [CONT_CMD_WRITE20_VOICE     ] = { .tx = 23, .rx =  1 }, // Write to VRx //! is .tx 17 or 23?
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

// RX Error flags
#define CONT_CMD_RX_SUCCESSFUL      0x00
#define CONT_CMD_RX_ERROR_IO        0x40
#define CONT_CMD_RX_ERROR_NO_DEVICE 0x80
#define CONT_CMD_RX_ERROR_MASK      0xC0

#define CONT_CMD_NOP                0xFF
#define CONT_CMD_END                0xFE // indicates end of a command
#define CONT_CMD_EXE                0x01 // set pif ram status byte to this to do a command
#define CONT_CMD_SKIP_CHNL          0x00 // Skip channel

void __osSiGetAccess(void);
void __osSiRelAccess(void);

////////////////////////
// Gamecube additions //
////////////////////////

typedef struct
{
    /* 0x00 */ u8 align;
    /* 0x01 */ u8 txsize;
    /* 0x02 */ u8 rxsize;
    /* 0x03 */ u8 cmd;
    /* 0x04 */ u8 analog_mode;
    /* 0x05 */ u8 rumble;
    /* 0x06 */ u16 button;
    /* 0x08 */ u8 stick_x;
    /* 0x09 */ u8 stick_y;
    /* 0x0A */ u8 c_stick_x;
    /* 0x0B */ u8 c_stick_y;
    /* 0x0C */ u8 l_trig;
    /* 0x0D */ u8 r_trig;
} __OSContGCNShortPollFormat; // size = 0x0E

typedef struct
{
    /* 0x00 */ s8 initialized;
    /* 0x01 */ u8 stick_x;
    /* 0x02 */ u8 stick_y;
    /* 0x03 */ u8 c_stick_x;
    /* 0x04 */ u8 c_stick_y;
} ControllerCenters; // size = 0x05

u8 __osControllerTypes[MAXCONTROLLERS] = { DEVICE_NONE };
u8 __osGamecubeRumbleEnabled[MAXCONTROLLERS] = { MOTOR_STOP };
ControllerCenters gGamecubeControllerCenters[MAXCONTROLLERS] = { 0 };

#define GCN_C_STICK_THRESHOLD 38

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
        if (gControllerBits & (1 << i)) {
            ptr = (u8 *)ALIGN(ptr, 4);

            if (__osControllerTypes[i] == DEVICE_GCN_CONTROLLER) {
                s32 stick_x, stick_y, c_stick_x, c_stick_y;
                readformatgcn = *(__OSContGCNShortPollFormat*)ptr;
                data->errno = CHNL_ERR(readformatgcn);

                if (data->errno == 0) {
                    if (!gGamecubeControllerCenters[i].initialized) {
                        gGamecubeControllerCenters[i].initialized = TRUE;
                        gGamecubeControllerCenters[i].stick_x   = readformatgcn.stick_x;
                        gGamecubeControllerCenters[i].stick_y   = readformatgcn.stick_y;
                        gGamecubeControllerCenters[i].c_stick_x = readformatgcn.c_stick_x;
                        gGamecubeControllerCenters[i].c_stick_y = readformatgcn.c_stick_y;
                    }

                    stick_x = CLAMP_S8(((s32)readformatgcn.stick_x) - gGamecubeControllerCenters[i].stick_x);
                    stick_y = CLAMP_S8(((s32)readformatgcn.stick_y) - gGamecubeControllerCenters[i].stick_y);
                    data->stick_x = stick_x;
                    data->stick_y = stick_y;
                    c_stick_x = CLAMP_S8(((s32)readformatgcn.c_stick_x) - gGamecubeControllerCenters[i].c_stick_x);
                    c_stick_y = CLAMP_S8(((s32)readformatgcn.c_stick_y) - gGamecubeControllerCenters[i].c_stick_y);
                    data->c_stick_x = c_stick_x;
                    data->c_stick_y = c_stick_y;
                    data->button = __osTranslateGCNButtons(readformatgcn.button, c_stick_x, c_stick_y);
                    data->l_trig = readformatgcn.l_trig;
                    data->r_trig = readformatgcn.r_trig;
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
            ptr++;
        }
    }
}

void __osMakeRequestData(OSPifRamChCmd *readformat, enum ContCmds cmd) {
    readformat->align = CONT_CMD_NOP;
    readformat->txsize = sContCmds[cmd].tx;
    readformat->rxsize = sContCmds[cmd].rx;
    readformat->cmd = cmd;
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

    __osMakeRequestData((OSPifRamChCmd *)&readformat, CONT_CMD_READ_BUTTON);
    readformat.button         = 0xFFFF;
    readformat.stick_x        = -1;
    readformat.stick_y        = -1;

    __osMakeRequestData((OSPifRamChCmd *)&readformatgcn, CONT_CMD_GCN_SHORT_POLL);
    readformatgcn.analog_mode = 3;
    readformatgcn.rumble      = 0;
    readformatgcn.button      = 0xFFFF;
    readformatgcn.stick_x     = -1;
    readformatgcn.stick_y     = -1;

    for (i = 0; i < __osMaxControllers; i++) {
        if (gControllerBits & (1 << i)) {
            ptr = (u8 *)ALIGN(ptr, 4);
            if (skipped) {
                *(u32 *)(ptr - 4) = ~BITMASK(skipped * 8);
                skipped = 0;
            }

            if (__osControllerTypes[i] == DEVICE_GCN_CONTROLLER) {
                readformatgcn.rumble = __osGamecubeRumbleEnabled[i];
                *(__OSContGCNShortPollFormat*)ptr = readformatgcn;
                ptr += sizeof(__OSContGCNShortPollFormat);
            } else {
                *(__OSContReadFormat*)ptr = readformat;
                ptr += sizeof(__OSContReadFormat);
            }
        } else {
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

//////////////////
// controller.c //
//////////////////

// Linker script will resolve references to the original function with this one instead.
// Called by osContInit and osContGetQuery
void __osContGetInitDataEx(u8* pattern, OSContStatus* data) {
    u8* ptr;
    __OSContRequesFormat requestHeader;
    s32 i;
    u8 bits;

    bits = 0;
    ptr = (u8*)__osContPifRam.ramarray;

    for (i = 0; i < __osMaxControllers; i++, ptr += sizeof(requestHeader), data++) {
        requestHeader = *(__OSContRequesFormat*)ptr;
        data->error = CHNL_ERR(requestHeader);
        if (data->error == 0) {
            data->type = ((requestHeader.typel << 8) | requestHeader.typeh);

            __osControllerTypes[i] = DEVICE_N64_CONTROLLER;

            // Check if the input type is a gamecube controller
            // Some mupen cores seem to send back a controller type of 0xFFFF if the core doesn't initialize the input plugin quickly enough,
            //   so check for that and set the input type as N64 controller if so.
            if ((s16)data->type != -1) {
                if (data->type & CONT_GCN) {
                    __osControllerTypes[i] = DEVICE_GCN_CONTROLLER;
                } else if (data->type & CONT_TYPE_MOUSE) {
                    __osControllerTypes[i] = DEVICE_MOUSE;
                } else if (data->type & CONT_TYPE_VOICE) {
                    __osControllerTypes[i] = DEVICE_VRU;
                }
            }

            data->status = requestHeader.status;

            bits |= (1 << i);
        }
    }

    *pattern = bits;
}

/////////////
// motor.c //
/////////////

static OSPifRam __MotorDataBuf[MAXCONTROLLERS];

#define READFORMAT(ptr) ((__OSContRamReadFormat*)(ptr))

// osMotorStart & osMotorStop
s32 __osMotorAccessEx(OSPfs* pfs, s32 flag) {
    int i;
    s32 ret = 0;
    u8* ptr = (u8*)&__MotorDataBuf[pfs->channel];

    if (!(pfs->status & PFS_MOTOR_INITIALIZED)) {
        return 5;
    }

    if (__osControllerTypes[pfs->channel] == DEVICE_GCN_CONTROLLER) {
        __osGamecubeRumbleEnabled[pfs->channel] = flag;
        __osContLastCmd = CONT_CMD_END;
    } else {
        __osSiGetAccess();
        __MotorDataBuf[pfs->channel].pifstatus = CONT_CMD_EXE;
        ptr += pfs->channel;

        for (i = 0; i < BLOCKSIZE; i++) {
            READFORMAT(ptr)->data[i] = flag;
        }

        __osContLastCmd = CONT_CMD_END;
        __osSiRawStartDma(OS_WRITE, &__MotorDataBuf[pfs->channel]);
        osRecvMesg(pfs->queue, NULL, OS_MESG_BLOCK);
        __osSiRawStartDma(OS_READ, &__MotorDataBuf[pfs->channel]);
        osRecvMesg(pfs->queue, NULL, OS_MESG_BLOCK);

        ret = (READFORMAT(ptr)->rxsize & CHNL_ERR_MASK);
        if (!ret) {
            if (!flag) {
                if (READFORMAT(ptr)->datacrc != 0) {
                    ret = PFS_ERR_CONTRFAIL;
                }
            } else {
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

    __osMakeRequestData((OSPifRamChCmd *)&ramreadformat, CONT_CMD_WRITE_MEMPAK);
    ramreadformat.addrh  = (CONT_BLOCK_RUMBLE >> 3);
    ramreadformat.addrl  = (u8)(__osContAddressCrc(CONT_BLOCK_RUMBLE) | (CONT_BLOCK_RUMBLE << 5));

    if (channel != 0) {
        for (i = 0; i < channel; i++) {
            *ptr++ = CONT_CMD_SKIP_CHNL;
        }
    }

    *READFORMAT(ptr) = ramreadformat;
    ptr += sizeof(__OSContRamReadFormat);
    ptr[0] = CONT_CMD_END;
}

s32 osMotorInitEx(OSMesgQueue *mq, OSPfs *pfs, int channel) {
    s32 ret;
    u8 temp[32];

    pfs->queue = mq;
    pfs->channel = channel;
    pfs->activebank = 0xFF;
    pfs->status = 0;

    if (__osControllerTypes[pfs->channel] != DEVICE_GCN_CONTROLLER) {
        ret = __osPfsSelectBank(pfs, 0xFE);

        if (ret == PFS_ERR_NEW_PACK) {
            ret = __osPfsSelectBank(pfs, 0x80);
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

        if (temp[31] == 0xFE) {
            return PFS_ERR_DEVICE;
        }

        ret = __osPfsSelectBank(pfs, 0x80);
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
