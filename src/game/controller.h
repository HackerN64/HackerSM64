#ifndef CONTROLLER_H
#define CONTROLLER_H

#include <PR/os_internal.h>
#include "types.h"

////////////
// Config //
////////////

// How far the player has to move the C-stick for it to register as a C button when converting to N64 controller input.
// [-80, 80]. Default is 38.
#define GCN_C_STICK_THRESHOLD 38
// How far the player has to press the L trigger for it to be considered a Z press.
// [0, 255]. Default is 85.
#define GCN_TRIGGER_THRESHOLD 85

/////////////////////////////////////////////////
// Libultra structs and macros (from ultralib) //
/////////////////////////////////////////////////

#define CHNL_ERR(format) (((format).rxsize & CHNL_ERR_MASK) >> 4)

///////////////////
// Input structs //
///////////////////

typedef union {
    struct PACKED {
        /*0x00*/ u16 x : 8;
        /*0x01*/ u16 y : 8;
    }; /*0x02*/
    /*0x00*/ u16 raw;
} AnalogStick; /*0x02*/

// -- N64 Standard Controller buttons --

typedef struct PACKED {
    /*0x0*/ u16 A           : 1; // CONT_A
    /*0x0*/ u16 B           : 1; // CONT_B
    /*0x0*/ u16 Z           : 1; // CONT_G
    /*0x0*/ u16 START       : 1; // CONT_START
    /*0x0*/ u16 D_UP        : 1; // CONT_UP
    /*0x0*/ u16 D_DOWN      : 1; // CONT_DOWN
    /*0x0*/ u16 D_LEFT      : 1; // CONT_LEFT
    /*0x0*/ u16 D_RIGHT     : 1; // CONT_RIGHT
    /*0x1*/ u16 RESET       : 1; // CONT_RESET
    /*0x1*/ u16 unused      : 1; // CONT_UNUSED
    /*0x1*/ u16 L           : 1; // CONT_L
    /*0x1*/ u16 R           : 1; // CONT_R
    /*0x1*/ u16 C_UP        : 1; // CONT_E
    /*0x1*/ u16 C_DOWN      : 1; // CONT_D
    /*0x1*/ u16 C_LEFT      : 1; // CONT_C
    /*0x1*/ u16 C_RIGHT     : 1; // CONT_F
} N64StandardButtons; /*0x02*/

// -- Mouse buttons --

typedef struct PACKED {
    /*0x0*/ u16 CLICK_LEFT  :  1;
    /*0x0*/ u16 CLICK_RIGHT :  1;
    /*0x0*/ u16             : 14;
} N64MouseButtons; /*0x02*/

// -- Train Controller buttons --

typedef struct PACKED {
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
} N64TrainButtons; /*0x02*/

// -- N64 buttons --

typedef union {
    N64StandardButtons standard;
    N64MouseButtons mouse;
    N64TrainButtons train;
    u16 raw;
} N64Buttons; /*0x02*/

// -- GCN Controller buttons --

typedef struct PACKED {
    /*0x0*/ u16 ERRSTAT     : 1; // CONT_GCN_ERRSTAT    | Error status: Whether there was an error on last transfer.
    /*0x0*/ u16 ERRLATCH    : 1; // CONT_GCN_ERRLATCH   | Error Latched: Check SISR on GCN.
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
} GCNStandardButtons; /*0x02*/

typedef struct PACKED {
    /*0x0*/ u16 ERRSTAT         : 1; // CONT_GCN_ERRSTAT
    /*0x0*/ u16 ERRLATCH        : 1; // CONT_GCN_ERRLATCH
    /*0x0*/ u16 GET_ORIGIN      : 1; // CONT_GCN_GET_ORIGIN
    /*0x0*/ u16 START           : 1; // CONT_GCN_START
    /*0x0*/ u16 LEFT_TOP        : 1;
    /*0x0*/ u16 RIGHT_TOP       : 1;
    /*0x0*/ u16 LEFT_BOTTOM     : 1;
    /*0x0*/ u16 RIGHT_BOTTOM    : 1;
    /*0x1*/ u16 USE_ORIGIN      : 1; // CONT_GCN_USE_ORIGIN
    /*0x1*/ u16                 : 7;
} GCNDKBongosButtons; /*0x02*/

typedef union {
    GCNStandardButtons standard;
    GCNDKBongosButtons dkbongos;
    u16 raw;
} GCNButtons; /*0x02*/

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
typedef struct PACKED {
    /*0x00*/ u32 ramarray[16 - 1];  // The command data.
    /*0x3C*/ u32 pifstatus;         // Set this to PIF_STATUS_EXE to run the commands in ramarray.
} OSPifRam; /*0x40*/

typedef struct PACKED {
    /*0x00*/ u8 txsize;             // Number of bytes to transmit.
    /*0x01*/ u8 rxsize;             // Number of bytes to receive.
} OSContCmdData; /*0x02*/

typedef union {
    struct PACKED {
        /*0x00*/ u8 h;              // HI byte.
        /*0x01*/ u8 l;              // LO byte.
    }; /*0x02*/
    u16 hl;
} HiLo16; /*0x02*/

//////////////////////////////
// Specific command formats //
//////////////////////////////

// -- Standard for all devices --

// 0x00: CONT_CMD_REQUEST_STATUS, 0xFF: CONT_CMD_RESET
typedef struct PACKED {
    /*0x00*/ u8 align0;             // For 4-byte alignment. Always CONT_CMD_NOP (0xFF). //! TODO: verify whether this is necessary.
    /*0x01*/ OSContCmdData cmd;     // The TX/RX sizes.
    /*0x02*/ struct PACKED {
        /*0x02*/ u8 cmdID;              // The ID of the command to run (CONT_CMD_REQUEST_STATUS, CONT_CMD_RESET).
    } send; /*0x01*/
    /*0x04*/ struct PACKED {
        /*0x04*/ HiLo16 type;           // Device type.
        /*0x06*/ u8 status;             // Status byte, depends on device type.
    } recv; /*0x03*/
    /*0x07*/ u8 align1;             // For 4-byte alignment. Always CONT_CMD_NOP (0xFF). //! TODO: verify whether this is necessary.
} __OSContRequestFormat; /*0x08*/

// 0x00: CONT_CMD_REQUEST_STATUS, 0xFF: CONT_CMD_RESET
typedef struct PACKED {
    /*0x00*/ OSContCmdData cmd;     // The TX/RX sizes.
    /*0x02*/ struct PACKED {
        /*0x02*/ u8 cmdID;              // The ID of the command to run (CONT_CMD_REQUEST_STATUS, CONT_CMD_RESET).
    } send; /*0x01*/
    /*0x03*/ struct PACKED {
        /*0x03*/ HiLo16 type;           // Device type.
        /*0x05*/ u8 status;             // Status byte, depends on device type.
    } recv; /*0x03*/
} __OSContRequesFormatShort; /*0x06*/

// -- Standard N64 input poll --

typedef union {
    struct PACKED {
        /*0x00*/ N64Buttons buttons;    // The received button data.
        /*0x02*/ AnalogStick stick;     // The received analog stick position [-80, 80].
    }; /*0x04*/
    u32 raw;
} N64InputData; /*0x04*/

// 0x01: CONT_CMD_READ_BUTTON
typedef struct PACKED {
    /*0x00*/ OSContCmdData cmd;     // The TX/RX sizes.
    /*0x02*/ struct PACKED {
        /*0x02*/ u8 cmdID;              // The ID of the command to run (CONT_CMD_READ_BUTTON).
    } send; /*0x01*/
    /*0x03*/ struct PACKED {
        /*0x03*/ N64InputData input;    // The received input data.
    } recv; /*0x04*/
} __OSContReadFormat; /*0x07*/

// -- Controller Pak Read/Write --

// 0x02: CONT_CMD_READ_MEMPAK, CONT_CMD_READ_64GB, CONT_CMD_READ_GBA
typedef struct PACKED {
    /*0x00*/ u8 align;              // For 4-byte alignment. Always CONT_CMD_NOP (0xFF). //! TODO: verify whether this is necessary.
    /*0x01*/ OSContCmdData cmd;     // The TX/RX sizes.
    /*0x02*/ struct PACKED {
        /*0x02*/ u8 cmdID;              // The ID of the command to run (CONT_CMD_READ_MEMPAK, CONT_CMD_READ_64GB, CONT_CMD_READ_GBA).
        /*0x04*/ HiLo16 addr;           // CRC code for address.
    } send; /*0x03*/
    /*0x06*/ struct PACKED {
        /*0x06*/ u8 data[BLOCKSIZE];    // Address of the data buffer. All 0 for no rumble, all 1 for rumble.
        /*0x26*/ u8 datacrc;            // CRC code for data.
    } recv; /*0x21*/
} __OSContRamReadFormat; /*0x27*/

// 0x03: CONT_CMD_WRITE_MEMPAK, CONT_CMD_WRITE_64GB, CONT_CMD_WRITE_GBA
typedef struct PACKED {
    /*0x00*/ u8 align;              // For 4-byte alignment. Always CONT_CMD_NOP (0xFF). //! TODO: verify whether this is necessary.
    /*0x01*/ OSContCmdData cmd;     // The TX/RX sizes.
    /*0x02*/ struct PACKED {
        /*0x02*/ u8 cmdID;              // The ID of the command to run (CONT_CMD_WRITE_MEMPAK, CONT_CMD_WRITE_64GB, CONT_CMD_WRITE_GBA).
        /*0x04*/ HiLo16 addr;           // CRC code for address.
        /*0x06*/ u8 data[BLOCKSIZE];    // Address of the data buffer. All 0 for no rumble, all 1 for rumble.
    } send; /*0x23*/
    /*0x26*/ struct PACKED {
        /*0x26*/ u8 datacrc;            // CRC code for data.
    } recv; /*0x01*/
} __OSContRamWriteFormat; /*0x27*/

// -- EEPROM Read/Write --

// 0x04: CONT_CMD_READ_EEPROM
typedef struct PACKED {
    /*0x00*/ OSContCmdData cmd;     // The TX/RX sizes.
    /*0x02*/ struct PACKED {
        /*0x02*/ u8 cmdID;              // The ID of the command to run (CONT_CMD_READ_EEPROM).
        /*0x03*/ u8 block;              // Which block of EEPROM to read from.
    } send; /*0x02*/
    /*0x04*/ struct PACKED {
        /*0x04*/ u8 data[EEPROM_BLOCK_SIZE]; // Address of the data buffer.
    } recv; /*0x08*/
} __OSContReadEEPROMFormat; /*0x0C*/

// 0x05: CONT_CMD_WRITE_EEPROM
typedef struct PACKED {
    /*0x00*/ OSContCmdData cmd;     // The TX/RX sizes.
    /*0x02*/ struct PACKED {
        /*0x02*/ u8 cmdID;              // The ID of the command to run (CONT_CMD_WRITE_EEPROM).
        /*0x03*/ u8 block;              // Which block of EEPROM to write to.
        /*0x04*/ u8 data[EEPROM_BLOCK_SIZE]; // Address of the data buffer.
    } send; /*0x0A*/
    /*0x0C*/ struct PACKED {
        /*0x0C*/ u8 busy;               // 0x80 = busy, 0x00 otherwise.
    } recv; /*0x01*/
} __OSContWriteEEPROMFormat; /*0x0D*/

// -- RTC Commands --

#define RTC_BLOCK_SIZE 8

typedef union {
    struct PACKED {
        /*0x00*/ u8 stopped     : 1; // Clock is halted, and it is safe to write to block 2.
        /*0x00*/ u8             : 5; // These bits have never been seen set.
        /*0x00*/ u8 crystalFail : 1; // If this bit is set, the crystal is not working.
        /*0x00*/ u8 batteryFail : 1; // If this bit is set, the supply voltage of the RTC became too low.

    } bits;
    u8 raw;
} RTCStatus; /*0x01*/

// 0x06: CONT_CMD_READ_RTC_STATUS
typedef struct PACKED {
    /*0x00*/ OSContCmdData cmd;     // The TX/RX sizes.
    /*0x02*/ struct PACKED {
        /*0x02*/ u8 cmdID;              // The ID of the command to run (CONT_CMD_READ_RTC_STATUS).
    } send; /*0x01*/
    /*0x03*/ struct PACKED {
        /*0x03*/ HiLo16 identifier;     // 0x0080 = 4 Kibibits (512 bytes), 0x00C0 = 16 Kibibits (2048 bytes)
        /*0x05*/ RTCStatus status;      // RTC clock status.
    } recv; /*0x03*/
} __OSContReadRTCStatusFormat; /*0x06*/

typedef union {
    struct PACKED {
        /*0x00*/ union {
                struct PACKED {
                    u8          : 6;    // Always 0.
                    u8 RTC      : 1;    // Write protects field 2 (RTC).
                    u8 NVRAM    : 1;    // Write protects field 1 (NVRAM).
                };
                u8 raw;
            } writeProtect;
        /*0x01*/ union {
                struct PACKED {
                    u8 unknown  : 1;    // Exists, changeable, no visible function.
                    u8          : 4;    // Always 0.
                    u8 stop     : 2;    // If either bit is set, stops RTC from counting.
                    u8          : 1;    // Always 0.
                };
                u8 raw;
            } control;
        /*0x02*/ u8 unused0[2];
        /*0x04*/ u8 writable[2];    // Can be updated but have no visible function.
        /*0x06*/ u8 unused1[2];
    } block0; // Block 0: Control Registers. Determines the current clock "mode".
    struct PACKED {
        /*0x00*/ u8 unknown[RTC_BLOCK_SIZE];
    } block1; // Block 1: 8 bytes of battery-backed SRAM
    struct PACKED {
        /*0x00*/ u8 seconds;        // [0, 59].
        /*0x01*/ u8 minutes;        // [0, 59].
        /*0x02*/ u8 hours;          // [0, 23] + 0x80.
        /*0x03*/ u8 day_of_month;   // [1, 31].
        /*0x04*/ u8 day_of_week;    // Sunday - Saturday, [0, 6].
        /*0x05*/ u8 month;          // [1, 12].
        /*0x06*/ u8 yearXX;         // Last two digits of year [0, 99].
        /*0x07*/ u8 century;        // Centuries since 1900, [0, 1].
    } block2; // Block 2: The current date and time in binary-coded decimal.
    struct PACKED {
        /*0x00*/ u8 unused[RTC_BLOCK_SIZE];
    } block3; // Block 3: Always 0
    u8 raw[RTC_BLOCK_SIZE];
} RTCBlockData; /*0x08*/

// 0x07: CONT_CMD_READ_RTC_BLOCK
typedef struct PACKED {
    /*0x00*/ OSContCmdData cmd;     // The TX/RX sizes.
    /*0x02*/ struct PACKED {
        /*0x02*/ u8 cmdID;              // The ID of the command to run (CONT_CMD_READ_RTC_BLOCK).
        /*0x03*/ u8 block;              // Which RTC block to read from [0, 3].
    } send; /*0x02*/
    /*0x04*/ struct PACKED {
        /*0x04*/ RTCBlockData data;     // Address of the data buffer.
        /*0x0C*/ RTCStatus status;      // RTC clock status.
    } recv; /*0x09*/
} __OSContReadRTCBlockFormat; /*0x0D*/

// 0x08: CONT_CMD_WRITE_RTC_BLOCK
typedef struct PACKED {
    /*0x00*/ OSContCmdData cmd;     // The TX/RX sizes.
    /*0x02*/ struct PACKED {
        /*0x02*/ u8 cmdID;              // The ID of the command to run (CONT_CMD_WRITE_RTC_BLOCK).
        /*0x03*/ u8 block;              // Which RTC block to write to [0, 3].
        /*0x04*/ RTCBlockData data;     // Address of the data buffer.
    } send; /*0x0A*/
    /*0x0C*/ struct PACKED {
        /*0x0C*/ RTCStatus status;      // RTC clock status.
    } recv; /*0x01*/
} __OSContRWriteRTCBlockFormat; /*0x0D*/

// -- GCN Controller Input poll & calibration --

typedef union {
    struct PACKED {
        /*0x00*/ GCNButtons buttons;    // The received button data.
        /*0x02*/ AnalogStick stick;     // The received analog stick position [-80, 80].
        /*0x04*/ AnalogStick c_stick;   // The received C stick position [-80, 80].
        /*0x06*/ u8 l_trig;             // The received L trigger position [0, 255].
        /*0x07*/ u8 r_trig;             // The received R trigger position [0, 255]. The DK Bongos' clap detector microphone uses this.
    }; /*0x08*/
    struct PACKED {
        /*0x00*/ u32 raw[2];
    }; /*0x08*/
} GCNInputData; /*0x08*/

// 0x40: CONT_CMD_GCN_SHORT_POLL
typedef struct PACKED {
    /*0x00*/ OSContCmdData cmd;     // The TX/RX sizes.
    /*0x02*/ struct PACKED {
        /*0x02*/ u8 cmdID;              // The ID of the command to run (CONT_CMD_GCN_SHORT_POLL).
        /*0x03*/ u8 analog_mode;        // Analog mode. //! TODO: documentation
        /*0x04*/ u8 rumble;             // Rumble bit.
    } send; /*0x03*/
    /*0x05*/ struct PACKED {
        /*0x05*/ GCNInputData input;    // The received input data.
    } recv; /*0x08*/
} __OSContGCNShortPollFormat; /*0x0D*/

////////////////////////////
// Controller accessories //
////////////////////////////

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
#define ACCESSORY_ID_UNKNOWN_82     0x82
#define ACCESSORY_ID_64GB           0x83
#define ACCESSORY_ID_TRANSFER_ON    0x84
#define ACCESSORY_ID_PRINTER        0x85
#define ACCESSORY_ID_TRANSFER_OFF   0xFE

enum ContCmds {
    // N64 Controller
    CONT_CMD_REQUEST_STATUS,            // 0x00: Read Controller type/status.
    CONT_CMD_READ_BUTTON,               // 0x01: Read Input Status.
    // Controller Accessory
    CONT_CMD_READ_MEMPAK,               // 0x02: Read Controller Accessory.
    CONT_CMD_WRITE_MEMPAK,              // 0x03: Write Controller Accessory.
    // EEPROM
    CONT_CMD_READ_EEPROM,               // 0x04: Read EEPROM.
    CONT_CMD_WRITE_EEPROM,              // 0x05: Write EEPROM.
    // RTC
    CONT_CMD_READ_RTC_STATUS,           // 0x06: RTC Info.
    CONT_CMD_READ_RTC_BLOCK,            // 0x07: Read RTC Block.
    CONT_CMD_WRITE_RTC_BLOCK,           // 0x08: Write RTC Block.
    // VRU
    CONT_CMD_READ36_VOICE,              // 0x09: Read from VRx.
    CONT_CMD_WRITE20_VOICE,             // 0x0A: Write to VRx.
    CONT_CMD_READ2_VOICE,               // 0x0B: Read Status VRx.
    CONT_CMD_WRITE4_VOICE,              // 0x0C: Write Config VRx.
    CONT_CMD_SWRITE_VOICE,              // 0x0D: Write Init VRx (Clear Dictionary).
    // Randnet Keyboard
    CONT_CMD_READ_KEYBOARD = 0x13,      // 0x13: Randnet Keyboard Read Keypress.
    // 64GB (https://pastebin.com/06VzdT3w)
    CONT_CMD_READ_64GB  = 0x13,         // 0x13: Read 64GB.
    CONT_CMD_WRITE_64GB = 0x14,         // 0x14: Write 64GB.
    // GBA
    CONT_CMD_READ_GBA  = 0x14,          // 0x14: Read GBA.
    CONT_CMD_WRITE_GBA = 0x15,          // 0x15: Write GBA.
    // Game ID https://gitlab.com/pixelfx-public/n64-game-id
    CONT_CMD_WRITE_GAME_ID = 0x1D,      // 0x1D: The EverDrive sends the game ID on the first controller port on boot using this.
    // GCN Steering Wheel
    CONT_CMD_GCN_WHEEL_FEEDBACK = 0x30, // 0x30: Logitech Speed Force Feedback.
    // GCN Controller
    CONT_CMD_GCN_SHORT_POLL = 0x40,     // 0x40: GameCube Shortpoll (input).
    CONT_CMD_GCN_READ_ORIGIN,           // 0x41: GameCube Read Origin.
    CONT_CMD_GCN_CALIBRATE,             // 0x42: GameCube Recalibrate.
    CONT_CMD_GCN_LONG_POLL,             // 0x43: GameCube Longpoll (input).
    // GCN Keyboard
    CONT_CMD_GCN_READ_KEYBOARD = 0x54,  // 0x54: GameCube Keyboard Poll.

    CONT_CMD_RESET = 0xFF,              // 0xFF: Reset/Info.
};

// Special control bytes used outside of commands.
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
enum PIFStatuses {
    PIF_STATUS_DONE, // Command is done.
    PIF_STATUS_EXE,  // Set pif ram status byte to this to do a command.
};

//////////////////////////
// HackerSM64 additions //
//////////////////////////

typedef struct PACKED {
    /*0x00*/ s8 initialized;            // Whether this controller's centers have been set.
    /*0x01*/ AnalogStick stick;         // The received analog stick position [-80, 80].
    /*0x03*/ AnalogStick c_stick;       // The received C stick X position [-80, 80].
} OSContCenter; /*0x05*/

typedef struct PACKED {
    /*0x00*/ u16 type;                  // Device type.
    /*0x02*/ u16 accessory;             // Accessory type.
    /*0x02*/ u16 pollingInput;          // Input, only used when status polling.
    /*0x04*/ u8 plugged;                // Whether a controller is plugged in.
    /*0x05*/ u8 playerNum;              // 0-4. 0 = not assigned to a player.
    /*0x06*/ OSContCenter contCenter;   // Gamecube Controller Center.
    /*0x0B*/ u8 gcRumble;               // GameCube Rumble status.
} OSPortInfo; /*0x0C*/

/////////////
// externs //
/////////////

extern OSPifRam __osContPifRam;     // A buffer for the PIF RAM.
extern u8       __osMaxControllers; // The last port to read controllers on..
extern u8       __osContLastCmd;    // The ID of the last command that was executed.

extern OSPortInfo gPortInfo[MAXCONTROLLERS];

#endif /* CONTROLLER_H */
