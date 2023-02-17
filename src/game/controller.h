#ifndef CONTROLLER_H
#define CONTROLLER_H

#include <PR/ultratypes.h>

#include "types.h"
#include "PR/os_internal.h"

////////////
// Config //
////////////

// How far the player has to move the C-stick for it to register as a C button when converting to N64 input. Default is 38.
#define GCN_C_STICK_THRESHOLD 38
// How far the player has to press the L trigger for it to be considered a Z press. 64 is about 25%. 127 would be about 50%. Default is 85.
#define GCN_TRIGGER_THRESHOLD 85

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
    /*0x00*/ u32 ramarray[15];      // The command data.
    /*0x3C*/ u32 pifstatus;         // Set this to PIF_STATUS_EXE to run the commands in ramarray.
} OSPifRam; // size = 0x40

typedef struct {
    /*0x00*/ u8 align;              // For 4-byte alignment. Always CONT_CMD_NOP (0xFF).
    // Command data (3 bytes):
    /*0x01*/ u8 txsize;             // Number of bytes to transmit.
    /*0x02*/ u8 rxsize;             // Number of bytes to receive.
    /*0x03*/ u8 cmd;                // The ID of the command to run.
} OSPifRamChCmd; // size = 0x04

// CONT_CMD_READ_BUTTON
typedef struct
{
    /*0x00*/ u8 align;              // For 4-byte alignment. Always CONT_CMD_NOP (0xFF).
    // Command data (3 bytes):
    /*0x01*/ u8 txsize;             // Number of bytes to transmit.
    /*0x02*/ u8 rxsize;             // Number of bytes to receive.
    /*0x03*/ u8 cmd;                // The ID of the command to run.
    // Received data (4 bytes):
    /*0x04*/ u16 button;            // The received button data.
    /*0x06*/ s8 stick_x;            // The received analog stick X position [-80, 80].
    /*0x07*/ s8 stick_y;            // The received analog stick Y position [-80, 80].
} __OSContReadFormat; // size = 0x08

// CONT_CMD_REQUEST_STATUS
typedef struct
{
    /*0x00*/ u8 align;              // For 4-byte alignment. Always CONT_CMD_NOP (0xFF).
    // Command data (3 bytes):
    /*0x01*/ u8 txsize;             // Number of bytes to transmit.
    /*0x02*/ u8 rxsize;             // Number of bytes to receive.
    /*0x03*/ u8 cmd;                // The ID of the command to run.
    // Received data (4 bytes):
    /*0x04*/ u8 typeh;              // HI byte of device type.
    /*0x05*/ u8 typel;              // LO byte of device type.
    /*0x06*/ u8 status;             // Status byte, depends on device type.
    /*0x07*/ u8 dummy1;             // 
} __OSContRequesFormat; // size = 0x08

// CONT_CMD_REQUEST_STATUS
typedef struct
{
    // Command data (3 bytes):
    /*0x00*/ u8 txsize;             // Number of bytes to transmit.
    /*0x01*/ u8 rxsize;             // Number of bytes to receive.
    /*0x02*/ u8 cmd;                // The ID of the command to run.
    // Received data (3 bytes):
    /*0x03*/ u8 typeh;              // HI byte of device type.
    /*0x04*/ u8 typel;              // LO byte of device type.
    /*0x05*/ u8 status;             // Status byte, depends on device type.
} __OSContRequesFormatShort; // size = 0x06

// CONT_CMD_GCN_SHORT_POLL
typedef struct
{
    /*0x00*/ u8 align;              // For 4-byte alignment. Always CONT_CMD_NOP (0xFF).
    // Command data (3 bytes):
    /*0x01*/ u8 txsize;             // Number of bytes to transmit.
    /*0x02*/ u8 rxsize;             // Number of bytes to receive.
    /*0x03*/ u8 cmd;                // The ID of the command to run.
    // Sent data (2 bytes):
    /*0x04*/ u8 analog_mode;        // Analog mode. //! TODO: documentation
    /*0x05*/ u8 rumble;             // Rumble bit.
    // Received data (8 bytes):
    /*0x06*/ u16 button;            // The received button data.
    /*0x08*/ u8 stick_x;            // The received analog stick X position [-80, 80].
    /*0x09*/ u8 stick_y;            // The received analog stick Y position [-80, 80].
    /*0x0A*/ u8 c_stick_x;          // The received C stick X position [-80, 80].
    /*0x0B*/ u8 c_stick_y;          // The received C stick Y position [-80, 80].
    /*0x0C*/ u8 l_trig;             // The received L trigger position [0, 255].
    /*0x0D*/ u8 r_trig;             // The received R trigger position [0, 255].
} __OSContGCNShortPollFormat; // size = 0x0E

// CONT_CMD_WRITE_MEMPAK
typedef struct
{
    /*0x00*/ u8 align;              // For 4-byte alignment. Always CONT_CMD_NOP (0xFF).
    // Command data (3 bytes):
    /*0x01*/ u8 txsize;             // Number of bytes to transmit.
    /*0x02*/ u8 rxsize;             // Number of bytes to receive.
    /*0x03*/ u8 cmd;                // The ID of the command to run.
    // Received data (35 bytes):
    /*0x04*/ u8 addrh;
    /*0x05*/ u8 addrl;
    /*0x06*/ u8 data[BLOCKSIZE];    // All 0 for no rumble, all 1 for rumble.
    /*0x26*/ u8 datacrc;
} __OSContRamReadFormat; // size = 0x27

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
    /*0x00*/ u8 tx;
    /*0x01*/ u8 rx;
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
    //! No room for 64GB read/write commands 0x13 and 0x14 (https://pastebin.com/06VzdT3w)
    // GBA
    CONT_CMD_READ_GBA,
    CONT_CMD_WRITE_GBA,
    // Game ID https://gitlab.com/pixelfx-public/n64-game-id
    CONT_CMD_WRITE_GAME_ID = 0x1D,
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

#define CONT_CMD_NOP                0xFF // Deos nothing, used for alignment.
#define CONT_CMD_END                0xFE // End command.
#define CONT_CMD_RESET_CHNL         0xFD // Reset channel.
#define CONT_CMD_SKIP_CHNL          0x00 // Skip channel.

// RX Error flags
#define CONT_CMD_RX_SUCCESSFUL      0x00 // No error.
#define CONT_CMD_RX_ERROR_IO        0x40 // IO error.
#define CONT_CMD_RX_ERROR_NO_DEVICE 0x80 // Nothing is plugged into the port.

#define CONT_CMD_RX_ERROR_MASK      0xC0

// PIF status:
#define PIF_STATUS_DONE             0x00 // Command is done.
#define PIF_STATUS_EXE              0x01 // Set pif ram status byte to this to do a command.

////////////////////////
// Gamecube additions //
////////////////////////

typedef struct
{
    /*0x00*/ s8 initialized;
    /*0x01*/ u8 stick_x;
    /*0x02*/ u8 stick_y;
    /*0x03*/ u8 c_stick_x;
    /*0x04*/ u8 c_stick_y;
} OSContCenter; // size = 0x05

typedef struct
{
    /*0x00*/ u16 type;                // Device type.
    /*0x02*/ u16 accessory;           // Accessory type.
    /*0x02*/ u16 pollingInput;        // Input, only used when status polling.
    /*0x04*/ u8 plugged;              // Whether a controller is plugged in.
    /*0x05*/ u8 playerNum;            // 0-4. 0 = not assigned to a player.
    /*0x06*/ OSContCenter contCenter; // Gamecube Controller Center.
    /*0x0B*/ u8 gcRumble;             // GameCube Rumble bit.
} OSPortInfo; // size = 0x0C

/////////////////////
// Buttons structs //
/////////////////////

// -- N64 Controller buttons --

typedef union {
    struct PACKED
    {
        /*0x0*/ u16 A           : 1; // CONT_A
        /*0x0*/ u16 B           : 1; // CONT_B
        /*0x0*/ u16 Z           : 1; // CONT_G
        /*0x0*/ u16 START       : 1; // CONT_START
        /*0x0*/ u16 D_UP        : 1; // CONT_UP
        /*0x0*/ u16 D_DOWN      : 1; // CONT_DOWN
        /*0x0*/ u16 D_LEFT      : 1; // CONT_LEFT
        /*0x0*/ u16 D_RIGHT     : 1; // CONT_RIGHT
        /*0x1*/ union {
                    u16 RESET       : 1; // CONT_RESET
                    u16 X           : 1; // GCN
                };
        /*0x1*/ union {
                    u16 unused      : 1; // CONT_UNUSED
                    u16 Y           : 1; // GCN
                };
        /*0x1*/ u16 L           : 1; // CONT_L
        /*0x1*/ u16 R           : 1; // CONT_R
        /*0x1*/ u16 C_UP        : 1; // CONT_E
        /*0x1*/ u16 C_DOWN      : 1; // CONT_D
        /*0x1*/ u16 C_LEFT      : 1; // CONT_C
        /*0x1*/ u16 C_RIGHT     : 1; // CONT_F
    } buttons; // size = 0x2
    u16 raw;
} N64Buttons; // size = 0x2

// -- GCN Controller buttons --

typedef union {
    struct PACKED
    {
        /*0x0*/ u16 ERRSTAT     : 1; // CONT_GCN_ERRSTAT
        /*0x0*/ u16 ERRLATCH    : 1; // CONT_GCN_ERRLATCH
        /*0x0*/ u16 GET_ORIGIN  : 1; // CONT_GCN_GET_ORIGIN
        /*0x0*/ u16 START       : 1; // CONT_GCN_START
        /*0x0*/ u16 Y           : 1; // CONT_GCN_Y
        /*0x0*/ u16 X           : 1; // CONT_GCN_X
        /*0x0*/ u16 B           : 1; // CONT_GCN_B
        /*0x0*/ u16 A           : 1; // CONT_GCN_A
        /*0x1*/ u16 USE_ORIGIN  : 1; // CONT_GCN_USE_ORIGIN
        /*0x1*/ u16 L           : 1; // CONT_GCN_L
        /*0x1*/ u16 R           : 1; // CONT_GCN_R
        /*0x1*/ u16 Z           : 1; // CONT_GCN_Z
        /*0x1*/ u16 D_UP        : 1; // CONT_GCN_UP
        /*0x1*/ u16 D_DOWN      : 1; // CONT_GCN_DOWN
        /*0x1*/ u16 D_RIGHT     : 1; // CONT_GCN_LEFT
        /*0x1*/ u16 D_LEFT      : 1; // CONT_GCN_RIGHT
    } buttons; // size = 0x2
    u16 raw;
} GCNButtons; // size = 0x2

// -- Mouse buttons --

typedef union {
    struct PACKED
    {
        /*0x0*/ u16 CLICK_LEFT  : 1;
        /*0x0*/ u16 CLICK_RIGHT : 1;
        /*0x0*/ u16 unused      : 14;
    } buttons; // size = 0x2
    u16 raw;
} N64MouseButtons; // size = 0x2

// -- Train Controller buttons --

typedef union {
    struct PACKED
    {
        /*0x0*/ u16 B           : 1;
        /*0x0*/ u16 A           : 1;
        /*0x0*/ u16 ACC1        : 1;
        /*0x0*/ u16 START       : 1;
        /*0x0*/ u16 ACC2        : 1;
        /*0x0*/ u16 EX1         : 1;
        /*0x0*/ u16 EX2         : 1;
        /*0x0*/ u16 ACC3        : 1;
        /*0x1*/ u16 EX3         : 1;
        /*0x1*/ u16 EX4         : 1;
        /*0x1*/ u16 C           : 1;
        /*0x1*/ u16 SELECT      : 1;
        /*0x1*/ u16 BRAKE       : 4;
    } buttons; // size = 0x2
    u16 raw;
} N64TrainButtons; // size = 0x2

/////////////
// externs //
/////////////

extern OSPifRam __osContPifRam;
extern u8 __osMaxControllers;
extern u8 __osContLastCmd;
extern OSPortInfo gPortInfo[MAXCONTROLLERS];

#endif /* CONTROLLER_H */
