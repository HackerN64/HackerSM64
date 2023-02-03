#ifndef CONTROLLER_H
#define CONTROLLER_H

#include "types.h"
#include "PR/os_internal.h"

/////////////////////////////////////////////////
// Libultra structs and macros (from ultralib) //
/////////////////////////////////////////////////

#define CHNL_ERR(format) (((format).rxsize & CHNL_ERR_MASK) >> 4)

/**
 * 00000000 00000000
 * 00000000 00000000
 * 00000000 00000000
 * 00000000 00000000
 * 00000000 00000000
 * 00000000 00000000
 *                ^^
 *         pifstatus
 */
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
} OSContCmdData; // size = 0x02

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

#define CONT_CMD_NOP                0xFF
#define CONT_CMD_END                0xFE // indicates end of a command
#define CONT_CMD_EXE                0x01 // set pif ram status byte to this to do a command
#define CONT_CMD_SKIP_CHNL          0x00 // Skip channel

// RX Error flags
#define CONT_CMD_RX_SUCCESSFUL      0x00
#define CONT_CMD_RX_ERROR_IO        0x40
#define CONT_CMD_RX_ERROR_NO_DEVICE 0x80
#define CONT_CMD_RX_ERROR_MASK      0xC0

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
} ControllerCenter; // size = 0x05

typedef struct
{
    /* 0x00 */ u8 plugged : 1;  // Whether a controller is plugged in.
    /* 0x01 */ u16 type;        // Device type
    /* 0x03 */ u8 playerNum;    // 0-4. 0 = not assigned to a player.
    /* 0x04 */ u8 gcRumble : 1; // GameCube Rumble
} OSPortInfo; // size = 0x05

extern OSPortInfo gPortInfo[MAXCONTROLLERS];

/////////////
// motor.c //
/////////////

#define MOTOR_ID 0x80

#define READFORMAT(ptr) ((__OSContRamReadFormat*)(ptr))

#endif /* CONTROLLER_H */
