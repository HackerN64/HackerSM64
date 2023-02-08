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
 * 00000000 00000000
 * 00000000 00000001
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

// Controller accessory addresses:
// https://github.com/joeldipops/TransferBoy/blob/master/docs/TransferPakReference.md

// Accesory detection:
#define CONT_ADDR_DETECT    0x8000
// Rumble / Bio Sensor / Snap Station Printer:
#define CONT_ADDR_RUMBLE    0xC000
#define CONT_ADDR_BIO_PULSE 0xC000
#define CONT_ADDR_PRINTER   0xC000
// Controller Pak / Transfer Pak:
#define CONT_ADDR_GB_POWER  0x8000 // Same as the detection address, but semantically different
#define CONT_ADDR_GB_BANK   0xA000
#define CONT_ADDR_GB_STATUS 0xB000
#define CONT_ADDR_GB_CART   0xC000

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
#define ACCESSORY_ID_UNKNOWN        0x82
#define ACCESSORY_ID_64GB           0x83
#define ACCESSORY_ID_TRANSFER_ON    0x84
#define ACCESSORY_ID_PRINTER        0x85
#define ACCESSORY_ID_TRANSFER_OFF   0xFE

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
    // GBA //! No room for 64GB read/write commands (https://pastebin.com/06VzdT3w)
    CONT_CMD_READ_GBA,
    CONT_CMD_WRITE_GBA,
    // Game ID (BlueRetro controller adapter)
    CONT_CMD_SET_GAME_ID = 0x1D,
    // GCN Steering Wheel
    CONT_CMD_GCN_WHEEL_FEEDBACK = 0x30,
    // GCN Controller
    CONT_CMD_GCN_SHORT_POLL = 0x40,
    CONT_CMD_GCN_READ_ORIGIN,
    CONT_CMD_GCN_CALIBRATE,
    CONT_CMD_GCN_LONG_POLL,
    // GCN Keyboard
    CONT_CMD_GCN_POLL_KEYBOARD = 0x54,

    CONT_CMD_RESET = 0xFF,
};

#define CONT_CMD_SKIP_CHNL          0x00 // Skip channel.
#define CONT_CMD_NOP                0xFF // Deos nothing, used for alignment.
#define CONT_CMD_END                0xFE // Indicates end of a command.
#define CONT_CMD_EXE                0x01 // Set pif ram status byte to this to do a command.

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
    /* 0x00 */ s8 initialized;
    /* 0x01 */ u8 stick_x;
    /* 0x02 */ u8 stick_y;
    /* 0x03 */ u8 c_stick_x;
    /* 0x04 */ u8 c_stick_y;
} OSContCenter; // size = 0x05

typedef struct
{
    /* 0x00 */ u8 plugged : 1;          // Whether a controller is plugged in.
    /* 0x01 */ u16 type;                // Device type.
    /* 0x03 */ u8 playerNum;            // 0-4. 0 = not assigned to a player.
    /* 0x04 */ OSContCenter contCenter; // Gamecube Controller Center.
    /* 0x09 */ u8 gcRumble : 1;         // GameCube Rumble.
} OSPortInfo; // size = 0x0A

extern OSPortInfo gPortInfo[MAXCONTROLLERS];

// -- N64 Controller buttons --

typedef struct PACKED
{
    /* 0x0 */ u16 A          : 1; // CONT_A
    /* 0x0 */ u16 B          : 1; // CONT_B
    /* 0x0 */ u16 Z          : 1; // CONT_G
    /* 0x0 */ u16 START      : 1; // CONT_START
    /* 0x0 */ u16 D_UP       : 1; // CONT_UP
    /* 0x0 */ u16 D_DOWN     : 1; // CONT_DOWN
    /* 0x0 */ u16 D_LEFT     : 1; // CONT_LEFT
    /* 0x0 */ u16 D_RIGHT    : 1; // CONT_RIGHT
    union {
        u16 RESET   : 1; // CONT_RESET
        u16 X       : 1;
    };
    union {
        u16 unused  : 1; // CONT_UNUSED
        u16 Y       : 1;
    };
    // /* 0x1 */ u16 RESET      : 1; // CONT_RESET
    // /* 0x1 */ u16 unused     : 1; // CONT_UNUSED
    /* 0x1 */ u16 L          : 1; // CONT_L
    /* 0x1 */ u16 R          : 1; // CONT_R
    /* 0x1 */ u16 C_UP       : 1; // CONT_E
    /* 0x1 */ u16 C_DOWN     : 1; // CONT_D
    /* 0x1 */ u16 C_LEFT     : 1; // CONT_C
    /* 0x1 */ u16 C_RIGHT    : 1; // CONT_F
} N64Buttons_split; // size = 0x2

typedef union {
    N64Buttons_split buttons;
    u16 raw;
} N64Buttons; // size = 0x2

// -- GCN Controller buttons --

typedef struct PACKED
{
    /* 0x0 */ u16 ERRSTAT    : 1; // CONT_GCN_ERRSTAT
    /* 0x0 */ u16 ERRLATCH   : 1; // CONT_GCN_ERRLATCH
    /* 0x0 */ u16 GET_ORIGIN : 1; // CONT_GCN_GET_ORIGIN
    /* 0x0 */ u16 START      : 1; // CONT_GCN_START
    /* 0x0 */ u16 Y          : 1; // CONT_GCN_Y
    /* 0x0 */ u16 X          : 1; // CONT_GCN_X
    /* 0x0 */ u16 B          : 1; // CONT_GCN_B
    /* 0x0 */ u16 A          : 1; // CONT_GCN_A
    /* 0x1 */ u16 USE_ORIGIN : 1; // CONT_GCN_USE_ORIGIN
    /* 0x1 */ u16 L          : 1; // CONT_GCN_L
    /* 0x1 */ u16 R          : 1; // CONT_GCN_R
    /* 0x1 */ u16 Z          : 1; // CONT_GCN_Z
    /* 0x1 */ u16 D_UP       : 1; // CONT_GCN_UP
    /* 0x1 */ u16 D_DOWN     : 1; // CONT_GCN_DOWN
    /* 0x1 */ u16 D_RIGHT    : 1; // CONT_GCN_LEFT
    /* 0x1 */ u16 D_LEFT     : 1; // CONT_GCN_RIGHT
} GCNButtons_split; // size = 0x2

typedef union {
    GCNButtons_split buttons;
    u16 raw;
} GCNButtons; // size = 0x2

#define READFORMAT(ptr) ((__OSContRamReadFormat*)(ptr))

#endif /* CONTROLLER_H */
