#pragma once

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

#define CHNL_ERR(format) (((format).rx & CHNL_ERR_MASK) >> 4)


////////////////////////////
// PIF RAM format structs //
////////////////////////////

/**
 * PIF RAM format:
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
// PIF RAM format used by vanilla libultra functions, pifstatus uses/overwrites the last 4 bytes.
typedef struct PACKED OSPifRam {
    /*0x00*/ u32 ramarray[(PIF_RAM_SIZE / 4) - 1];  // The command data.
    /*0x3C*/ u32 pifstatus;                         // Set this to PIF_STATUS_EXE to run the commands in ramarray.
} OSPifRam; /*0x40*/

//; PIF RAM format, pifstatus only uses the last byte and frees up the previous 3 bytes for commands that only use this format.
typedef struct PACKED OSPifRamEx {
    /*0x00*/ u8 ramarray[PIF_RAM_SIZE - 1];         // The command data.
    /*0x3F*/ u8 pifstatus;                          // Set this to PIF_STATUS_EXE to run the commands in ramarray.
} OSPifRamEx; /*0x40*/

// Send/receive data that signifies the start of a SI command.
typedef struct PACKED OSContCmdSize {
    /*0x00*/ u8 tx; // The number of bytes of data to transmit.
    /*0x01*/ u8 rx; // The number of bytes of data to receive.
} OSContCmdSize; /*0x02*/

/////////////////////////////////////
// Specific command format structs //
/////////////////////////////////////

// Generic command format for reading fields common to all formats.
typedef struct PACKED __OSContGenericFormat {
    /*0x01*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run.
                }; /*0x01*/
                u8 raw[sizeof(u8)];
            } send; /*0x01*/
    /*0x04*/ union {
                struct PACKED {
                }; /*0x00*/
                u8 raw[0];
            } recv; /*0x00*/
} __OSContGenericFormat; /*0x04*/

// -- Standard for all devices --

// 0x00: CONT_CMD_REQUEST_STATUS, 0xFF: CONT_CMD_RESET
typedef struct PACKED __OSContRequestFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_REQUEST_STATUS, CONT_CMD_RESET).
                }; /*0x01*/
                u8 raw[sizeof(u8)];
            } send; /*0x01*/
    /*0x03*/ union {
                struct PACKED {
                    /*0x00*/ HiLo16 type;           // Device type.
                    /*0x02*/ union {                // Status byte, depends on device type.
                                struct PACKED { // Standard N64 Controller.
                                    /*0x00*/ u8              : 5;
                                    /*0x00*/ u8 crcError     : 1;
                                    /*0x00*/ u8 noPak        : 1;
                                    /*0x00*/ u8 yesPak       : 1;
                                } n64; /*0x01*/
                                struct PACKED { // VRU.
                                    /*0x00*/ u8              : 7;
                                    /*0x00*/ u8 initialized  : 1;
                                } vru; /*0x01*/
                                struct PACKED { // Standard GCN Controller.
                                    /*0x00*/ u8              : 4;
                                    /*0x00*/ u8 rumble       : 1;
                                    /*0x00*/ u8              : 3; // always 0x3?
                                } gcn; /*0x01*/
                                struct PACKED { // ERPROM.
                                    /*0x00*/ u8 busy         : 1;
                                    /*0x00*/ u8              : 7;
                                } eep; /*0x01*/
                                u8 raw;
                            } status; /*0x01*/
                }; /*0x03*/
                u8 raw[sizeof(HiLo16) + sizeof(u8)];
            } recv; /*0x03*/
} __OSContRequestFormat; /*0x06*/

typedef struct PACKED __OSContRequestFormatAligned {
    /*0x00*/ u8 align0;             // For 4-byte alignment. Always PIF_CMD_NOP (0xFF). Only needed for compatibility with vanilla libultra.
    /*0x01*/ __OSContRequestFormat fmt;
    /*0x07*/ u8 align1;             // For 4-byte alignment. Always PIF_CMD_NOP (0xFF). Only needed for compatibility with vanilla libultra.
} __OSContRequestFormatAligned; /*0x08*/

// -- Standard N64 input poll --

// 0x01: CONT_CMD_READ_BUTTON
typedef struct PACKED __OSContReadFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_READ_BUTTON).
                }; /*0x01*/
                u8 raw[sizeof(u8)];
            } send; /*0x01*/
    /*0x03*/ union {
                struct PACKED {
                    /*0x00*/ N64InputData input;    // The received input data.
                }; /*0x04*/
                u8 raw[sizeof(N64InputData)];
            } recv; /*0x04*/
} __OSContReadFormat; /*0x07*/

// -- Controller Pak Read/Write --

// 0x02: CONT_CMD_READ_MEMPAK, CONT_CMD_READ_64GB, CONT_CMD_READ_GBA
typedef struct PACKED __OSContRamReadFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_READ_MEMPAK, CONT_CMD_READ_64GB, CONT_CMD_READ_GBA).
                    /*0x01*/ HiLo16 addr;           // CRC code for address.
                }; /*0x03*/
                u8 raw[sizeof(u8) + sizeof(HiLo16)];
            } send; /*0x03*/
    /*0x05*/ union {
                struct PACKED {
                    /*0x00*/ u8 data[BLOCKSIZE];    // Address of the data buffer. All 0 for no rumble, all 1 for rumble.
                    /*0x20*/ u8 datacrc;            // CRC code for data.
                }; /*0x21*/
                u8 raw[(sizeof(u8) * BLOCKSIZE) + sizeof(u8)];
            } recv; /*0x21*/
} __OSContRamReadFormat; /*0x26*/

typedef struct PACKED __OSContRamReadFormatAligned {
    /*0x00*/ u8 align0;             // For 4-byte alignment. Always PIF_CMD_NOP (0xFF). Only needed for compatibility with vanilla libultra.
    /*0x01*/ __OSContRamReadFormat fmt;
} __OSContRamReadFormatAligned; /*0x27*/

// 0x03: CONT_CMD_WRITE_MEMPAK, CONT_CMD_WRITE_64GB, CONT_CMD_WRITE_GBA
typedef struct PACKED __OSContRamWriteFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_WRITE_MEMPAK, CONT_CMD_WRITE_64GB, CONT_CMD_WRITE_GBA).
                    /*0x01*/ HiLo16 addr;           // CRC code for address.
                    /*0x03*/ u8 data[BLOCKSIZE];    // Address of the data buffer. All 0 for no rumble, all 1 for rumble.
                }; /*0x23*/
                u8 raw[sizeof(u8) + sizeof(HiLo16) + (sizeof(u8) * BLOCKSIZE)];
            } send; /*0x23*/
    /*0x25*/ union {
                struct PACKED {
                    /*0x00*/ u8 datacrc;            // CRC code for data.
                }; /*0x01*/
                u8 raw[sizeof(u8)];
            } recv; /*0x01*/
} __OSContRamWriteFormat; /*0x26*/

typedef struct PACKED __OSContRamWriteFormatAligned {
    /*0x00*/ u8 align0;             // For 4-byte alignment. Always PIF_CMD_NOP (0xFF). Only needed for compatibility with vanilla libultra.
    /*0x01*/ __OSContRamWriteFormat fmt;
} __OSContRamWriteFormatAligned; /*0x27*/

// -- EEPROM Read/Write --

// 0x04: CONT_CMD_READ_EEPROM
typedef struct PACKED __OSContReadEEPROMFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_READ_EEPROM).
                    /*0x01*/ u8 block;              // Which block of EEPROM to read from.
                }; /*0x02*/
                u8 raw[sizeof(u8) + sizeof(u8)];
            } send; /*0x02*/
    /*0x04*/ union {
                struct PACKED {
                    /*0x00*/ u8 data[EEPROM_BLOCK_SIZE]; // Address of the data buffer.
                }; /*0x08*/
                u8 raw[(sizeof(u8) * EEPROM_BLOCK_SIZE)];
            } recv; /*0x08*/
} __OSContReadEEPROMFormat; /*0x0C*/

// 0x05: CONT_CMD_WRITE_EEPROM
typedef struct PACKED __OSContWriteEEPROMFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_WRITE_EEPROM).
                    /*0x01*/ u8 block;              // Which block of EEPROM to write to.
                    /*0x02*/ u8 data[EEPROM_BLOCK_SIZE]; // Address of the data buffer.
                }; /*0x0A*/
                u8 raw[sizeof(u8) + sizeof(u8) + (sizeof(u8) * EEPROM_BLOCK_SIZE)];
            } send; /*0x0A*/
    /*0x0C*/ union {
                struct PACKED {
                    /*0x00*/ u8 busy;               // 0x80 = busy, 0x00 otherwise.
                }; /*0x01*/
                u8 raw[sizeof(u8)];
            } recv; /*0x01*/
} __OSContWriteEEPROMFormat; /*0x0D*/

// -- RTC Commands --

#define RTC_BLOCK_SIZE 8

typedef union {
    struct PACKED {
        /*0x00*/ u8 stopped     : 1;    // Clock is halted, and it is safe to write to block 2.
        /*0x00*/ u8             : 5;    // These bits have never been seen set.
        /*0x00*/ u8 crystalFail : 1;    // If this bit is set, the crystal is not working.
        /*0x00*/ u8 batteryFail : 1;    // If this bit is set, the supply voltage of the RTC became too low.
    }; /*0x01*/
    u8 raw;
} RTCStatus; /*0x01*/

// 0x06: CONT_CMD_READ_RTC_STATUS
typedef struct PACKED __OSContReadRTCStatusFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_READ_RTC_STATUS).
                }; /*0x01*/
                u8 raw[sizeof(u8)];
            } send; /*0x01*/
    /*0x03*/ union {
                struct PACKED {
                    /*0x00*/ HiLo16 identifier;     // 0x0080 = 4 Kibibits (512 bytes), 0x00C0 = 16 Kibibits (2048 bytes)
                    /*0x02*/ RTCStatus status;      // RTC clock status.
                }; /*0x03*/
                u8 raw[sizeof(HiLo16) + sizeof(RTCStatus)];
            } recv; /*0x03*/
} __OSContReadRTCStatusFormat; /*0x06*/

typedef union {
    struct PACKED {
        /*0x00*/ union {
                    struct PACKED {
                        /*0x00*/ u8          : 6;    // Always 0.
                        /*0x00*/ u8 RTC      : 1;    // Write protects field 2 (RTC).
                        /*0x00*/ u8 NVRAM    : 1;    // Write protects field 1 (NVRAM).
                    }; /*0x01*/
                    u8 raw;
                } writeProtect;
        /*0x01*/ union {
                    struct PACKED {
                        /*0x00*/ u8 unknown  : 1;    // Exists, changeable, no visible function.
                        /*0x00*/ u8          : 4;    // Always 0.
                        /*0x00*/ u8 stop     : 2;    // If either bit is set, stops RTC from counting.
                        /*0x00*/ u8          : 1;    // Always 0.
                    }; /*0x01*/
                    u8 raw;
                } control;
        /*0x02*/ u8 unused0[2];
        /*0x04*/ u8 unknown[2];     // These two can be updated but have no visible function.
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
typedef struct PACKED __OSContReadRTCBlockFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_READ_RTC_BLOCK).
                    /*0x01*/ u8 block;              // Which RTC block to read from [0, 3].
                }; /*0x02*/
                u8 raw[sizeof(u8) + sizeof(u8)];
            } send; /*0x02*/
    /*0x04*/ union {
                struct PACKED {
                    /*0x00*/ RTCBlockData data;     // Address of the data buffer.
                    /*0x08*/ RTCStatus status;      // RTC clock status.
                };
                u8 raw[sizeof(RTCBlockData) + sizeof(RTCStatus)];
            } recv; /*0x09*/
} __OSContReadRTCBlockFormat; /*0x0D*/

// 0x08: CONT_CMD_WRITE_RTC_BLOCK
typedef struct PACKED __OSContRWriteRTCBlockFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_WRITE_RTC_BLOCK).
                    /*0x01*/ u8 block;              // Which RTC block to write to [0, 3].
                    /*0x02*/ RTCBlockData data;     // Address of the data buffer.
                }; /*0x0A*/
                u8 raw[sizeof(u8) + sizeof(u8) + sizeof(RTCBlockData)];
            } send; /*0x0A*/
    /*0x0C*/ union {
                struct PACKED {
                    /*0x00*/ RTCStatus status;      // RTC clock status.
                }; /*0x01*/
                u8 raw[sizeof(RTCStatus)];
            } recv; /*0x01*/
} __OSContRWriteRTCBlockFormat; /*0x0D*/

// -- VRU Commands --

typedef union {
    struct PACKED { //! TODO: Check if this is byteswapped.
        /*0x00*/ u16 addr : 11; // Address.
        /*0x01*/ u16 crc  :  5; // CRC.
    }; /*0x02*/
    u16 raw;
} VRUAddrCRC; /*0x02*/

// 0x09: CONT_CMD_READ_VOICE
typedef struct PACKED __OSContRead36VoiceFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_READ_VOICE).
                    /*0x01*/ VRUAddrCRC addrcrc;    // Address and CRC.
                }; /*0x03*/
                u8 raw[sizeof(u8) + sizeof(VRUAddrCRC)];
            } send; /*0x03*/
    /*0x04*/ union {
                struct PACKED {
                    /*0x00*/ u8 data[0x24];         // Address of the 36-byte data buffer. Data is 16 bit byteswapped.
                    /*0x24*/ u8 datacrc;            // CRC code for data.
                }; /*0x25*/
                u8 raw[(sizeof(u8) * 0x24) + sizeof(u8)];
            } recv; /*0x25*/
} __OSContRead36VoiceFormat; /*0x29*/

// 0x0A: CONT_CMD_WRITE_VOICE
typedef struct PACKED __OSContWrite20VoiceFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_WRITE_VOICE).
                    /*0x01*/ VRUAddrCRC addrcrc;    // Address and CRC.
                    /*0x03*/ u8 data[0x14];         // Address of the 20-byte data buffer. Data is 16 bit byteswapped.
                }; /*0x17*/
                u8 raw[sizeof(u8) + sizeof(VRUAddrCRC) + (sizeof(u8) * 0x14)];
            } send; /*0x17*/
    /*0x28*/ union {
                struct PACKED {
                    /*0x00*/ u8 datacrc;            // CRC code for data.
                }; /*0x01*/
                u8 raw[sizeof(u8)];
            } recv; /*0x01*/
} __OSContWrite20VoiceFormat; /*0x29*/

typedef union {
    struct PACKED { //! TODO: Check if this is byteswapped.
        /*0x00*/ u16 unknown : 13; // Unknown status.
        /*0x01*/ u16 mode    :  3; // Mode.
    }; /*0x02*/
    union {
        u8  u8[2];
        u16 u16[1];
    } raw;
} VRUStatus; /*0x02*/

// 0x0B: CONT_CMD_READ_VOICE_STATUS
typedef struct PACKED __OSContRead2VoiceFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_READ_VOICE_STATUS).
                    /*0x01*/ VRUAddrCRC addrcrc;    // Address and CRC.
                }; /*0x03*/
                u8 raw[sizeof(u8) + sizeof(VRUAddrCRC)];
            } send; /*0x03*/
    /*0x05*/ union {
                struct PACKED {
                    /*0x00*/ VRUStatus statusMode;  // Unknown status and VRU mode.
                    /*0x02*/ u8 datacrc;            // CRC code for data.
                }; /*0x03*/
                u8 raw[sizeof(VRUStatus) + sizeof(u8)];
            } recv; /*0x03*/
} __OSContRead2VoiceFormat; /*0x08*/

// 0x0C: CONT_CMD_WRITE_VOICE_STATUS
typedef struct PACKED __OSContWrite4VoiceFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_WRITE_VOICE_STATUS).
                    /*0x01*/ VRUAddrCRC addrcrc;    // Address and CRC.
                    /*0x03*/ VRUStatus statusMode;  // Unknown status and VRU mode.
                    /*0x05*/ u16 arg;               // Unknown argument(s).
                }; /*0x07*/
                u8 raw[sizeof(u8) + sizeof(VRUAddrCRC) + sizeof(VRUStatus) + sizeof(u16)];
            } send; /*0x07*/
    /*0x09*/ union {
                struct PACKED {
                    /*0x00*/ u8 datacrc;            // CRC code for data.
                }; /*0x01*/
                u8 raw[sizeof(u8)];
            } recv; /*0x01*/
} __OSContWrite4VoiceFormat; /*0x0A*/

// 0x0D: CONT_CMD_RESET_VOICE
typedef struct PACKED __OSContSWriteVoiceFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_RESET_VOICE).
                    /*0x01*/ VRUAddrCRC addrcrc;    // Address and CRC.
                }; /*0x03*/
                u8 raw[sizeof(u8) + sizeof(VRUAddrCRC)];
            } send; /*0x03*/
    /*0x05*/ union {
                struct PACKED {
                    /*0x00*/ u8 error;              // Error flags.
                }; /*0x01*/
                u8 raw[sizeof(u8)];
            } recv; /*0x01*/
} __OSContSWriteVoiceFormat; /*0x06*/

// -- Randnet Keyboard input poll --

// 0x13: CONT_CMD_READ_KEYBOARD
typedef struct PACKED __OSContReadKeyboardFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_READ_64GB).
                    /*0x01*/ union {
                                struct PACKED {
                                    /*0x00*/ u8             : 5;
                                    /*0x00*/ u8 power       : 1;    // Power LED on.
                                    /*0x00*/ u8 capLock     : 1;    // Caps Lock LED on.
                                    /*0x00*/ u8 numLock     : 1;    // Num Lock LED on.
                                }; /*0x01*/
                                u8 raw;
                            } LEDs; /*0x01*/
                }; /*0x02*/
                u8 raw[sizeof(u8) + sizeof(u8)];
            } send; /*0x02*/
    /*0x04*/ union {
                struct PACKED {
                    /*0x00*/ u16 keypress[3];       // Which keys are pressed (up to 3 keys can be pressed at a time). //! TODO: Key ID defines/enum.
                    /*0x06*/ union {
                                struct PACKED {
                                    /*0x00*/ u8             : 3;
                                    /*0x00*/ u8 tooManyKeys : 1;    // Too many keys pressed?
                                    /*0x00*/ u8             : 3;
                                    /*0x00*/ u8 homeKey     : 1;    // Home key pressed.
                                }; /*0x01*/
                                u8 raw;
                            } status; /*0x01*/
                }; /*0x07*/
                u8 raw[(sizeof(u16) * 3) + sizeof(u8)];
            } recv; /*0x07*/
} __OSContReadKeyboardFormat; /*0x0B*/

// -- Send Game ID to controller --

// 0x1D: CONT_CMD_WRITE_GAME_ID
typedef struct PACKED __OSContWriteGameIDFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_WRITE_GAME_ID).
                    /*0x01*/ u32 crc_hi;            // CRC HI / CRC1 (bytes [0x10-0x13] of the ROM).
                    /*0x05*/ u32 crc_lo;            // CRC LO / CRC2 (bytes [0x14-0x17] of the ROM).
                    /*0x09*/ u8 mediaformat;        // Media format ROM type (byte 0x3B of the ROM).
                    /*0x0A*/ u8 country_id;         // Country Code (byte 0x3E of the ROM).
                }; /*0x0B*/
                u8 raw[sizeof(u8) + sizeof(u32) + sizeof(u32) + sizeof(u8) + sizeof(u8)];
            } send; /*0x0B*/
    /*0x0D*/ union {
                struct PACKED {
                    /*0x00*/ u8 recv[1];            // Always 0?
                };
                u8 raw[sizeof(u8)];
            } recv; /*0x01*/
} __OSContWriteGameIDFormat; /*0x0E*/

// -- GCN Wheel Feedback --

// 0x30: CONT_CMD_GCN_WHEEL_FEEDBACK
typedef struct PACKED __OSContGCNWheelForceFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_GCN_WHEEL_FEEDBACK).
                    /*0x01*/ union {
                                struct PACKED {
                                    /*0x00*/ u16            : 5;
                                    /*0x00*/ u16 unknown    : 1;  // Unknown. Always 1?
                                    /*0x00*/ u16 state      : 1;  // 0 = motor off, 1 = motor on.
                                    /*0x00*/ u16 strength   : 9;  // Force strength [0, 256]. 0 = left strong, 128 = center, 256 = right strong.
                                }; /*0x02*/
                                u16 raw;
                            } force; /*0x02*/
                }; /*0x03*/
                u8 raw[sizeof(u8) + sizeof(u16)];
            } send; /*0x03*/
    /*0x0D*/ union {
                struct PACKED {
                    /*0x00*/ GCNButtons buttons;    // The received button data.
                    /*0x02*/ union {
                                struct PACKED {
                                    /*0x00*/ u8             : 4;  // Unknown.
                                    /*0x00*/ u8 pedal       : 1;  // Whether the pedals are connected.
                                    /*0x00*/ u8             : 3;  // Unknown.
                                }; /*0x01*/
                                u8 raw;
                            } flags; /*0x01*/
                    /*0x03*/ s8 wheel;              // How far the wheel is turned in a direction [-128, 127].
                    /*0x04*/ Analog_u8 pedal;       // Pedals, if connected [0, 255].
                    /*0x06*/ Analog_u8 paddle;      // Paddles on the back of the wheel [0, 255].
                }; /*0x08*/
                u8 raw[sizeof(GCNButtons) + sizeof(u8) + sizeof(s8) + sizeof(Analog_u8) + sizeof(Analog_u8)];
            } recv; /*0x08*/
} __OSContGCNWheelForceFormat; /*0x0D*/

// -- GCN Controller Input poll & calibration --

// 0x40: CONT_CMD_GCN_SHORT_POLL
typedef struct PACKED __OSContGCNShortPollFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_GCN_SHORT_POLL).
                    /*0x01*/ u8 analog_mode;        // Analog mode (see OSGCNAnalogModes).
                    /*0x02*/ u8 rumble;             // Rumble byte.
                }; /*0x03*/
                u8 raw[sizeof(u8) + sizeof(u8) + sizeof(u8)];
            } send; /*0x03*/
    /*0x05*/ union {
                struct PACKED {
                    /*0x00*/ GCNInputData input;    // The received input data.
                }; /*0x08*/
                u8 raw[sizeof(GCNInputData)];
            } recv; /*0x08*/
} __OSContGCNShortPollFormat; /*0x0D*/

// 0x41: CONT_CMD_GCN_READ_ORIGIN
typedef struct PACKED __OSContGCNReadOriginFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_GCN_READ_ORIGIN).
                }; /*0x01*/
                u8 raw[sizeof(u8)];
            } send; /*0x01*/
    /*0x0D*/ struct PACKED {
                union {
                    struct PACKED {
                        /*0x00*/ GCNInputData origins;  // Only the analog values are used.
                        /*0x08*/ Analog_u8 buttons;     // Analog buttons used by some controllers. Both always 0x02?
                    }; /*0x0A*/
                    u8 raw[sizeof(GCNInputData) + sizeof(Analog_u8)];
                }; /*0x0A*/
            } recv; /*0x0A*/
} __OSContGCNReadOriginFormat; /*0x0D*/

// 0x42: CONT_CMD_GCN_CALIBRATE
typedef struct PACKED __OSContGCNCalibrateFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_GCN_CALIBRATE).
                    /*0x01*/ u8 analog_mode;        // Ignored. Analog mode (see OSGCNAnalogModes).
                    /*0x02*/ u8 rumble;             // Ignored. Rumble byte.
                }; /*0x03*/
                u8 raw[sizeof(u8) + sizeof(u8) + sizeof(u8)];
            } send; /*0x03*/
    /*0x0D*/ union {
                struct PACKED {
                    /*0x00*/ GCNInputData origins;  // Only the analog values are used.
                    /*0x08*/ Analog_u8 buttons;     // Analog buttons used by some controllers. Both always 0x02?
                }; /*0x0A*/
                u8 raw[sizeof(GCNInputData) + sizeof(Analog_u8)];
            } recv; /*0x0A*/
} __OSContGCNCalibrateFormat; /*0x0F*/

// 0x43: CONT_CMD_GCN_LONG_POLL
typedef struct PACKED __OSContGCNLongPollFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_GCN_LONG_POLL).
                    /*0x01*/ u8 analog_mode;        // Ignored. Analog mode (see OSGCNAnalogModes).
                    /*0x02*/ u8 rumble;             // Rumble byte.
                }; /*0x03*/
                u8 raw[sizeof(u8) + sizeof(u8) + sizeof(u8)];
            } send; /*0x03*/
    /*0x0D*/ union {
                struct PACKED {
                    /*0x00*/ GCNInputData input;    // The received input data. Uses mode 3 (2-byte C-stick and triggers) regardless of input command.
                    /*0x08*/ Analog_u8 buttons;     // Analog buttons used by some controllers.
                }; /*0x0A*/
                u8 raw[sizeof(GCNInputData) + sizeof(Analog_u8)];
            } recv; /*0x0A*/
} __OSContGCNLongPollFormat; /*0x0F*/

// -- GCN Keyboard input poll --

// 0x54: CONT_CMD_GCN_READ_KEYBOARD
typedef struct PACKED __OSContGCNReadKeyboardFormat {
    /*0x00*/ OSContCmdSize size;    // The TX/RX sizes.
    /*0x02*/ union {
                struct PACKED {
                    /*0x00*/ u8 cmd;                // The ID of the command to run (CONT_CMD_GCN_READ_KEYBOARD).
                    /*0x01*/ u8 analog_mode;        // Ignored? Analog mode (see OSGCNAnalogModes).
                    /*0x02*/ u8 rumble;             // Ignored? Rumble byte.
                }; /*0x03*/
                u8 raw[sizeof(u8) + sizeof(u8) + sizeof(u8)];
            } send; /*0x03*/
    /*0x0D*/ union {
                struct PACKED {
                    /*0x00*/ GCNInputData input;    // The received input data.
                }; /*0x08*/
                u8 raw[sizeof(GCNInputData)];
            } recv; /*0x08*/
} __OSContGCNReadKeyboardFormat; /*0x0D*/

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

enum OSContCmds {
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
    CONT_CMD_READ_VOICE,                // 0x09: Read from VRx.
    CONT_CMD_WRITE_VOICE,               // 0x0A: Write to VRx.
    CONT_CMD_READ_VOICE_STATUS,         // 0x0B: Read Status VRx.
    CONT_CMD_WRITE_VOICE_STATUS,        // 0x0C: Write Config VRx.
    CONT_CMD_RESET_VOICE,               // 0x0D: Write Init VRx (Clear Dictionary).
    // Randnet Keyboard
    CONT_CMD_READ_KEYBOARD      = 0x13, // 0x13: Randnet Keyboard Read Keypress.
    // 64GB (https://pastebin.com/06VzdT3w)
    CONT_CMD_READ_64GB          = 0x13, // 0x13: Read 64GB.
    CONT_CMD_WRITE_64GB         = 0x14, // 0x14: Write 64GB.
    // GBA
    CONT_CMD_READ_GBA           = 0x14, // 0x14: Read GBA.
    CONT_CMD_WRITE_GBA          = 0x15, // 0x15: Write GBA.
    // Game ID (https://gitlab.com/pixelfx-public/n64-game-id)
    CONT_CMD_WRITE_GAME_ID      = 0x1D, // 0x1D: The EverDrive sends the game ID on the first controller port on boot using this.
    // GCN Steering Wheel
    CONT_CMD_GCN_WHEEL_FEEDBACK = 0x30, // 0x30: Logitech Speed Force Feedback.
    // GCN Controller
    CONT_CMD_GCN_SHORT_POLL     = 0x40, // 0x40: GameCube Short Poll (input).
    CONT_CMD_GCN_READ_ORIGIN,           // 0x41: GameCube Read Origin.
    CONT_CMD_GCN_CALIBRATE,             // 0x42: GameCube Recalibrate.
    CONT_CMD_GCN_LONG_POLL,             // 0x43: GameCube Long Poll (input).
    // GCN Keyboard
    CONT_CMD_GCN_READ_KEYBOARD  = 0x54, // 0x54: GameCube Keyboard Poll.

    CONT_CMD_RESET              = 0xFF, // 0xFF: Reset/Info.
};

// Special control bytes used outside of commands.
enum OSPIFControlBytes {
    PIF_CMD_SKIP_CHNL  = 0x00, // Increment the channel counter without doing anything.
    PIF_CMD_RESET_CHNL = 0xFD, // Reset the channel.
    PIF_CMD_END        = 0xFE, // End the entire command.
    PIF_CMD_NOP        = 0xFF, // Deos nothing, used for alignment.

};

// PIF status:
enum PIFStatuses {
    PIF_STATUS_DONE, // Command is done.
    PIF_STATUS_EXE,  // Set PIF RAM status byte to this to do a command.
};

/////////////
// externs //
/////////////

// From vanilla libultra:
extern OSPifRamEx __osContPifRam;       // A buffer for the PIF RAM.
extern u8         __osMaxControllers;   // The last port to read controllers on.
extern u8         __osContLastCmd;      // The ID of the last command that was executed.
