#include "PR/os_internal.h"
#include "config.h"
#include "string.h"
#include "engine/math_util.h"

#include "game_init.h"

/////////////////////////////////////////////////
// Libultra structs and macros (from ultralib) //
/////////////////////////////////////////////////

#define CHNL_ERR(format) (((format).rxsize & CHNL_ERR_MASK) >> 4)

typedef struct
{
    /* 0x00 */ u32 ramarray[16 - 1];
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

// Controller accessory probe IDs:
#define ACCESSORY_ID_NULL           0xFF
#define ACCESSORY_ID_RUMBLE         0x80
#define ACCESSORY_ID_BIO_PULSE      0x81
#define ACCESSORY_ID_UNKNOWN_82     0x82
#define ACCESSORY_ID_64GB           0x83
#define ACCESSORY_ID_TRANSFER_ON    0x84
#define ACCESSORY_ID_PRINTER        0x85
#define ACCESSORY_ID_TRANSFER_OFF   0xFE

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

#define CONT_CMD_SKIP_CHNL  0x00
#define CONT_CMD_NOP        0xff
#define CONT_CMD_END        0xfe //indicates end of a command
#define CONT_CMD_EXE        1    //set pif ram status byte to this to do a command

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
        if ((gControllerStatuses[i].type & CONT_CONSOLE_MASK) == CONT_CONSOLE_GCN) {
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
                data->stick_x = readformat.stick_x;
                data->stick_y = readformat.stick_y;
                data->button = readformat.button;
                data->c_stick_x = 0;
                data->c_stick_y = 0;
                data->l_trig = 0;
                data->r_trig = 0;
            }
            ptr += sizeof(__OSContReadFormat);
        }
    }
}

static void __osPackReadData(void) {
    u8* ptr = (u8*)__osContPifRam.ramarray;
    __OSContReadFormat readformat;
    __OSContGCNShortPollFormat readformatgcn;
    int i;

    bzero(__osContPifRam.ramarray, sizeof(__osContPifRam.ramarray));

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
    readformatgcn.rumble = MOTOR_STOP;
    readformatgcn.button = 0xFFFF;
    readformatgcn.stick_x = -1;
    readformatgcn.stick_y = -1;

    for (i = 0; i < __osMaxControllers; i++) {
        if ((gControllerStatuses[i].type & CONT_CONSOLE_MASK) == CONT_CONSOLE_GCN) {
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
extern u8 __osGamecubeRumbleEnabled[MAXCONTROLLERS];

extern OSTimer __osEepromTimer;
extern OSMesgQueue __osEepromTimerQ;
extern OSMesg __osEepromTimerMsg;


/////////////
// motor.c //
/////////////

// A buffer to hold separate rumble commands for each port.
ALIGNED64 static OSPifRam __MotorDataBuf[MAXCONTROLLERS];

/**
 * @brief Turns controller rumble on or off.
 * Modified from vanilla libultra to handle GameCube controller rumble.
 * Called by osMotorStart, osMotorStop, and osMotorStopHard via macro.
 *
 * @param[in] pfs        A pointer to a buffer for the controller pak (AKA rumble pak) file system.
 * @param[in] motorState MOTOR_STOP = stop motor, MOTOR_START = start motor, MOTOR_STOP_HARD (GCN only) = motor brake.
 * @return s32 PIF error status.
 */
s32 __osMotorAccessEx(OSPfs* pfs, s32 motorState) {
    s32 err = PFS_ERR_SUCCESS;
    int channel = pfs->channel;
    u8* ptr = (u8*)&__MotorDataBuf[channel];

    // Make sure the rumble pak pfs was initialized.
    if (!(pfs->status & PFS_MOTOR_INITIALIZED)) {
        return PFS_ERR_INVALID;
    }

    // Check whether the controller is a GCN controller.
    if ((gControllerStatuses[channel].type & CONT_CONSOLE_MASK) == CONT_CONSOLE_GCN) { // GCN Controllers.
        __osGamecubeRumbleEnabled[channel] = motorState;

        // Change the last command ID so that input poll command (which includes rumble) gets written again.
        __osContLastCmd = CONT_CMD_END;
    } else { // N64 Controllers.
        // N64 rumble pak can only use MOTOR_STOP or MOTOR_START.
        motorState &= MOTOR_MASK_N64;

        __osSiGetAccess();

        // Set the PIF to be ready to run a command.
        __MotorDataBuf[channel].pifstatus = CONT_CMD_EXE;

        // Leave a PIF_CMD_SKIP_CHNL (0x00) byte in __MotorDataBuf for each skipped channel.
        ptr += channel;

        __OSContRamReadFormat* readformat = (__OSContRamReadFormat*)ptr;

        // Set the entire block to either MOTOR_STOP or MOTOR_START.
        memset(readformat->data, motorState, sizeof(readformat->data));

        __osContLastCmd = CONT_CMD_END;

        // Write __MotorDataBuf to the PIF RAM and then wait for the command to execute.
        __osSiRawStartDma(OS_WRITE, &__MotorDataBuf[channel]);
        osRecvMesg(pfs->queue, NULL, OS_MESG_BLOCK);

        // Read the resulting __MotorDataBuf from the PIF RAM and then wait for the command to execute.
        __osSiRawStartDma(OS_READ, &__MotorDataBuf[channel]);
        osRecvMesg(pfs->queue, NULL, OS_MESG_BLOCK);

        // Check for errors.
        err = (readformat->rxsize & CHNL_ERR_MASK);
        if (err == (CHNL_ERR_SUCCESS >> 4)) {
            if (motorState == MOTOR_STOP) {
                if (readformat->datacrc != 0x00) { // 0xFF = Disconnected.
                    err = PFS_ERR_CONTRFAIL; // "Controller pack communication error"
                }
            } else { // MOTOR_START
                if (readformat->datacrc != 0xEB) { // 0x14 = Uninitialized.
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
 *
 * @param[in] channel The port ID to operate on.
 * @param[in] mdata   A pointer to a buffer for the PIF RAM command data.
 */
static void _MakeMotorData(int channel, OSPifRam* mdata) {
    u8* ptr = (u8*)mdata->ramarray;
    __OSContRamReadFormat ramwriteformat;
    int i;

    ramwriteformat.dummy  = CONT_CMD_NOP;
    ramwriteformat.txsize = CONT_CMD_WRITE_PAK_TX;
    ramwriteformat.rxsize = CONT_CMD_WRITE_PAK_RX;
    ramwriteformat.cmd    = CONT_CMD_WRITE_PAK;
    ramwriteformat.addrh  = (CONT_BLOCK_RUMBLE >> 3);
    ramwriteformat.addrl  = (u8)(__osContAddressCrc(CONT_BLOCK_RUMBLE) | (CONT_BLOCK_RUMBLE << 5));

    // Leave a PIF_CMD_SKIP_CHNL (0x00) byte in mdata->ramarray for each skipped channel.
    if (channel != 0) {
        for (i = 0; i < channel; i++) {
            *ptr++ = CONT_CMD_SKIP_CHNL;
        }
    }

    *(__OSContRamReadFormat*)ptr = ramwriteformat;
    ptr += sizeof(__OSContRamReadFormat);
    *ptr = CONT_CMD_END;
}

/**
 * @brief Initializes the Rumble Pak.
 * Modified from vanilla libultra to ignore GameCube controllers.
 * Called by thread6_rumble_loop and cancel_rumble.
 *
 * @param[in ] mq      The SI event message queue.
 * @param[out] pfs     A pointer to a buffer for the controller pak (AKA rumble pak) file system.
 * @param[in ] channel The port ID to operate on.
 * @return s32 PFS error status.
 */
s32 osMotorInitEx(OSMesgQueue* mq, OSPfs* pfs, int channel) {
    s32 err = PFS_ERR_SUCCESS;
    u8 data[BLOCKSIZE];

    pfs->status     = PFS_STATUS_NONE;
    pfs->queue      = mq;
    pfs->channel    = channel;
    pfs->activebank = ACCESSORY_ID_NULL;

    // Make sure the controller is not a GCN controller.
    if ((gControllerStatuses[channel].type & CONT_CONSOLE_MASK) == CONT_CONSOLE_N64) {
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
            return PFS_ERR_DEVICE; // Wrong device.
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
            return PFS_ERR_DEVICE; // Wrong device.
        }

        // Write the PIF command.
        if (!(pfs->status & PFS_MOTOR_INITIALIZED)) {
            _MakeMotorData(channel, &__MotorDataBuf[channel]);
        }
    }

    pfs->status = PFS_MOTOR_INITIALIZED;

    return PFS_ERR_SUCCESS;
}
