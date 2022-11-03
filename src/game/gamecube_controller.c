#include "PR/os_internal.h"

/////////////////////////////////////////////////
// Libultra structs and macros (from ultralib) //
/////////////////////////////////////////////////

#define ARRLEN(x) ((s32)(sizeof(x) / sizeof(x[0])))
#define CHNL_ERR(format) (((format).rxsize & CHNL_ERR_MASK) >> 4)

#define CLAMP(x, low, high)  (((x) > (high)) ? (high) : (((x) < (low)) ? (low) : (x)))
#define S8_MAX __SCHAR_MAX__
#define S8_MIN (-S8_MAX - 1)
#define CLAMP_S8( x)        CLAMP((x),  S8_MIN,  S8_MAX)

#define CHNL_ERR_MASK		0xC0	/* Bit 6-7: channel errors */

typedef struct
{
    /* 0x0 */ u32 ramarray[15];
    /* 0x3C */ u32 pifstatus;
} OSPifRam;

typedef struct
{
    /* 0x0 */ u8 dummy;
    /* 0x1 */ u8 txsize;
    /* 0x2 */ u8 rxsize;
    /* 0x3 */ u8 cmd;
    /* 0x4 */ u16 button;
    /* 0x6 */ s8 stick_x;
    /* 0x7 */ s8 stick_y;
} __OSContReadFormat;

typedef struct
{
    /* 0x0 */ u8 dummy;
    /* 0x1 */ u8 txsize;
    /* 0x2 */ u8 rxsize;
    /* 0x3 */ u8 cmd;
    /* 0x4 */ u8 typeh;
    /* 0x5 */ u8 typel;
    /* 0x6 */ u8 status;
    /* 0x7 */ u8 dummy1;
} __OSContRequesFormat;

typedef struct
{
    /* 0x0 */ u8 txsize;
    /* 0x1 */ u8 rxsize;
    /* 0x2 */ u8 cmd;
    /* 0x3 */ u8 typeh;
    /* 0x4 */ u8 typel;
    /* 0x5 */ u8 status;
} __OSContRequesFormatShort;

typedef struct
{
    /* 0x0 */ u8 dummy;
    /* 0x1 */ u8 txsize;
    /* 0x2 */ u8 rxsize;
    /* 0x3 */ u8 cmd;
    /* 0x4 */ u8 addrh;
    /* 0x5 */ u8 addrl;
    /* 0x6 */ u8 data[BLOCKSIZE];
    /* 0x26 */ u8 datacrc;
} __OSContRamReadFormat;

extern OSPifRam __osContPifRam;
extern u8 __osMaxControllers;

#define CONT_CMD_READ_BUTTON    1

// Controller accessory addresses
// https://github.com/joeldipops/TransferBoy/blob/master/docs/TransferPakReference.md

// Accesory detection
#define CONT_ADDR_DETECT    0x8000
// Rumble
#define CONT_ADDR_RUMBLE    0xC000
// Controller Pak
// Transfer Pak
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

// Joybus commands
//from: http://en64.shoutwiki.com/wiki/SI_Registers_Detailed#CONT_CMD_Usage
#define CONT_CMD_REQUEST_STATUS 0
#define CONT_CMD_READ_BUTTON    1
#define CONT_CMD_READ_PAK       2
#define CONT_CMD_WRITE_PAK      3
#define CONT_CMD_READ_EEPROM    4
#define CONT_CMD_WRITE_EEPROM   5
#define CONT_CMD_READ36_VOICE   9
#define CONT_CMD_WRITE20_VOICE  10
#define CONT_CMD_READ2_VOICE    11
#define CONT_CMD_WRITE4_VOICE   12
#define CONT_CMD_SWRITE_VOICE   13
#define CONT_CMD_CHANNEL_RESET  0xFD
#define CONT_CMD_RESET          0xFF
#define CONT_CMD_GCN_SHORTPOLL  0x40

// Bytes transmitted for each joybus command
#define CONT_CMD_REQUEST_STATUS_TX 1
#define CONT_CMD_READ_BUTTON_TX    1
#define CONT_CMD_READ_PAK_TX       3
#define CONT_CMD_WRITE_PAK_TX      35
#define CONT_CMD_READ_EEPROM_TX    2
#define CONT_CMD_WRITE_EEPROM_TX   10
#define CONT_CMD_READ36_VOICE_TX   3
#define CONT_CMD_WRITE20_VOICE_TX  23
#define CONT_CMD_READ2_VOICE_TX    3
#define CONT_CMD_WRITE4_VOICE_TX   7
#define CONT_CMD_SWRITE_VOICE_TX   3
#define CONT_CMD_RESET_TX          1
#define CONT_CMD_GCN_SHORTPOLL_TX  3

// Bytes received for each joybus command
#define CONT_CMD_REQUEST_STATUS_RX 3
#define CONT_CMD_READ_BUTTON_RX    4
#define CONT_CMD_READ_PAK_RX       33
#define CONT_CMD_WRITE_PAK_RX      1
#define CONT_CMD_READ_EEPROM_RX    8
#define CONT_CMD_WRITE_EEPROM_RX   1
#define CONT_CMD_READ36_VOICE_RX   37
#define CONT_CMD_WRITE20_VOICE_RX  1
#define CONT_CMD_READ2_VOICE_RX    3
#define CONT_CMD_WRITE4_VOICE_RX   1
#define CONT_CMD_SWRITE_VOICE_RX   1
#define CONT_CMD_RESET_RX          3
#define CONT_CMD_GCN_SHORTPOLL_RX  8

#define CONT_CMD_NOP 0xff
#define CONT_CMD_END 0xfe //indicates end of a command
#define CONT_CMD_EXE 1    //set pif ram status byte to this to do a command

void __osSiGetAccess(void);
void __osSiRelAccess(void);

////////////////////////
// Gamecube additions //
////////////////////////

typedef struct
{
    /* 0x0 */ u8 dummy;
    /* 0x1 */ u8 txsize;
    /* 0x2 */ u8 rxsize;
    /* 0x3 */ u8 cmd;
    /* 0x4 */ u8 analog_mode;
    /* 0x5 */ u8 rumble;
    /* 0x6 */ u16 button;
    /* 0x8 */ u8 stick_x;
    /* 0x9 */ u8 stick_y;
    /* 0xA */ u8 c_stick_x;
    /* 0xB */ u8 c_stick_y;
    /* 0xC */ u8 l_trig;
    /* 0xD */ u8 r_trig;
} __OSContGCNShortPollFormat;
extern u8 __osContLastCmd;
u8 __osControllerTypes[MAXCONTROLLERS];
u8 __osGamecubeRumbleEnabled[MAXCONTROLLERS];

typedef struct
{
    s8 initialized;
    u8 stick_x;
    u8 stick_y;
    u8 c_stick_x;
    u8 c_stick_y;
} ControllerCenters;

#define GCN_C_STICK_THRESHOLD 38

static void __osPackReadData(void);
static u16 __osTranslateGCNButtons(u16, s32, s32);

////////////////////
// contreaddata.c //
////////////////////

s32 osContStartReadDataEx(OSMesgQueue* mq) {
    s32 ret = 0;

    __osSiGetAccess();

    if (__osContLastCmd != CONT_CMD_READ_BUTTON) {
        __osPackReadData();
        ret = __osSiRawStartDma(OS_WRITE, __osContPifRam.ramarray);
        osRecvMesg(mq, NULL, OS_MESG_BLOCK);
    }

    ret = __osSiRawStartDma(OS_READ, __osContPifRam.ramarray);
    __osContLastCmd = CONT_CMD_READ_BUTTON;
    __osSiRelAccess();

    return ret;
}

ControllerCenters gGamecubeControllerCenters[MAXCONTROLLERS] = { 0 };

void osContGetReadDataEx(OSContPadEx* data) {
    u8* ptr = (u8*)__osContPifRam.ramarray;
    __OSContReadFormat readformat;
    __OSContGCNShortPollFormat readformatgcn;
    int i;

    for (i = 0; i < __osMaxControllers; i++, data++) {
        if (__osControllerTypes[i] == CONT_TYPE_GCN) {
            s32 stick_x, stick_y, c_stick_x, c_stick_y;
            readformatgcn = *(__OSContGCNShortPollFormat*)ptr;
            data->errno = CHNL_ERR(readformatgcn);
            if (data->errno != 0) {
                gGamecubeControllerCenters[i].initialized = FALSE;
                continue;
            }

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
            ptr += sizeof(__OSContGCNShortPollFormat);
        } else {
            readformat = *(__OSContReadFormat*)ptr;
            data->errno = CHNL_ERR(readformat);
            
            if (data->errno != 0) {
                continue;
            }

            data->stick_x = readformat.stick_x;
            data->stick_y = readformat.stick_y;
            data->button = readformat.button;
            data->c_stick_x = 0;
            data->c_stick_y = 0;
            data->l_trig = 0;
            data->r_trig = 0;
            ptr += sizeof(__OSContReadFormat);
        }
    }
}

static void __osPackReadData(void) {
    u8* ptr = (u8*)__osContPifRam.ramarray;
    __OSContReadFormat readformat;
    __OSContGCNShortPollFormat readformatgcn;
    int i;

    for (i = 0; i < ARRLEN(__osContPifRam.ramarray); i++) {
        __osContPifRam.ramarray[i] = 0;
    }

    __osContPifRam.pifstatus = CONT_CMD_EXE;
    readformat.dummy = CONT_CMD_NOP;
    readformat.txsize = CONT_CMD_READ_BUTTON_TX;
    readformat.rxsize = CONT_CMD_READ_BUTTON_RX;
    readformat.cmd = CONT_CMD_READ_BUTTON;
    readformat.button = 0xFFFF;
    readformat.stick_x = -1;
    readformat.stick_y = -1;

    readformatgcn.dummy = CONT_CMD_NOP;
    readformatgcn.txsize = CONT_CMD_GCN_SHORTPOLL_TX;
    readformatgcn.rxsize = CONT_CMD_GCN_SHORTPOLL_RX;
    readformatgcn.cmd = CONT_CMD_GCN_SHORTPOLL;
    readformatgcn.analog_mode = 3;
    readformatgcn.rumble = 0;
    readformatgcn.button = 0xFFFF;
    readformatgcn.stick_x = -1;
    readformatgcn.stick_y = -1;

    for (i = 0; i < __osMaxControllers; i++) {
        if (__osControllerTypes[i] == CONT_TYPE_GCN) {
            readformatgcn.rumble = __osGamecubeRumbleEnabled[i];
            *(__OSContGCNShortPollFormat*)ptr = readformatgcn;
            ptr += sizeof(__OSContGCNShortPollFormat);
        } else {
            *(__OSContReadFormat*)ptr = readformat;
            ptr += sizeof(__OSContReadFormat);
        }
    }
    
    *ptr = CONT_CMD_END;
}

static u16 __osTranslateGCNButtons(u16 input, s32 c_stick_x, s32 c_stick_y) {
    u16 ret = 0;

    // Face buttons
    if (input & CONT_GCN_A) {
        ret |= A_BUTTON;
    }
    if (input & CONT_GCN_B) {
        ret |= B_BUTTON;
    }
    if (input & CONT_GCN_START) {
        ret |= START_BUTTON;
    }
    if (input & CONT_GCN_X) {
        ret |= GCN_X_BUTTON;
    }
    if (input & CONT_GCN_Y) {
        ret |= GCN_Y_BUTTON;
    }

    // Triggers & Z
    if (input & CONT_GCN_Z) {
        ret |= Z_TRIG;
    }
    if (input & CONT_GCN_R) {
        ret |= R_TRIG;
    }
    if (input & CONT_GCN_L) {
        ret |= L_TRIG;
    }

    // D-Pad
    if (input & CONT_GCN_UP) {
        ret |= U_JPAD;
    }
    if (input & CONT_GCN_DOWN) {
        ret |= D_JPAD;
    }
    if (input & CONT_GCN_LEFT) {
        ret |= L_JPAD;
    }
    if (input & CONT_GCN_RIGHT) {
        ret |= R_JPAD;
    }

    // C-stick to C-buttons
    if (c_stick_x > GCN_C_STICK_THRESHOLD) {
        ret |= R_CBUTTONS;
    }
    if (c_stick_x < -GCN_C_STICK_THRESHOLD) {
        ret |= L_CBUTTONS;
    }
    if (c_stick_y > GCN_C_STICK_THRESHOLD) {
        ret |= U_CBUTTONS;
    }
    if (c_stick_y < -GCN_C_STICK_THRESHOLD) {
        ret |= D_CBUTTONS;
    }

    return ret;
}

//////////////////
// controller.c //
//////////////////

extern s32 __osContinitialized;

extern OSPifRam __osContPifRam;
extern u8 __osContLastCmd;
extern u8 __osMaxControllers;
extern u8 __osControllerTypes[MAXCONTROLLERS];
extern u8 __osGamecubeRumbleEnabled[MAXCONTROLLERS];

extern OSTimer __osEepromTimer;
extern OSMesgQueue __osEepromTimerQ;
extern OSMesg __osEepromTimerMsg;

// Linker script will resolve references to the original function with this one instead
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
            data->type = requestHeader.typel << 8 | requestHeader.typeh;
            
            // Check if the input type is a gamecube controller
            // Some mupen cores seem to send back a controller type of 0xFFFF if the core doesn't initialize the input plugin quickly enough,
            //   so check for that and set the input type as N64 controller if so.
            if ((data->type & CONT_GCN) && (s16)data->type != -1) {
                __osControllerTypes[i] = CONT_TYPE_GCN;
            } else {
                __osControllerTypes[i] = CONT_TYPE_N64;
            }

            data->status = requestHeader.status;

            bits |= 1 << i;
        }
    }
    *pattern = bits;
}

/////////////
// motor.c //
/////////////

static OSPifRam __MotorDataBuf[MAXCONTROLLERS];

#define READFORMAT(ptr) ((__OSContRamReadFormat*)(ptr))

s32 __osMotorAccessEx(OSPfs* pfs, s32 flag) {
    int i;
    s32 ret = 0;
    u8* ptr = (u8*)&__MotorDataBuf[pfs->channel];

    if (!(pfs->status & PFS_MOTOR_INITIALIZED)) {
        return 5;
    }

    if (__osControllerTypes[pfs->channel] == CONT_TYPE_GCN) {
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

        ret = READFORMAT(ptr)->rxsize & CHNL_ERR_MASK;
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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wimplicit-function-declaration"
static void _MakeMotorData(int channel, OSPifRam *mdata) {
    u8 *ptr = (u8 *)mdata->ramarray;
    __OSContRamReadFormat ramreadformat;
    int i;

    ramreadformat.dummy = CONT_CMD_NOP;
    ramreadformat.txsize = CONT_CMD_WRITE_PAK_TX;
    ramreadformat.rxsize = CONT_CMD_WRITE_PAK_RX;
    ramreadformat.cmd = CONT_CMD_WRITE_PAK;
    ramreadformat.addrh = CONT_BLOCK_RUMBLE >> 3;
    ramreadformat.addrl = (u8)(__osContAddressCrc(CONT_BLOCK_RUMBLE) | (CONT_BLOCK_RUMBLE << 5));
    
    if (channel != 0) {
        for (i = 0; i < channel; i++) {
            *ptr++ = CONT_CMD_REQUEST_STATUS;
        }
    }

    *READFORMAT(ptr) = ramreadformat;
    ptr += sizeof(__OSContRamReadFormat);
    ptr[0] = CONT_CMD_END;
}

s32 osMotorInitEx(OSMesgQueue *mq, OSPfs *pfs, int channel)
{
    s32 ret;
    u8 temp[32];

    pfs->queue = mq;
    pfs->channel = channel;
    pfs->activebank = 0xFF;
    pfs->status = 0;

    if (__osControllerTypes[pfs->channel] != CONT_TYPE_GCN) {
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

        if (temp[31] == 254) {
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
#pragma GCC diagnostic pop
