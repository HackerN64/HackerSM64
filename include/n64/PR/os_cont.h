
/*====================================================================
 * os_cont.h
 *
 * Copyright 1995, Silicon Graphics, Inc.
 * All Rights Reserved.
 *
 * This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics,
 * Inc.; the contents of this file may not be disclosed to third
 * parties, copied or duplicated in any form, in whole or in part,
 * without the prior written permission of Silicon Graphics, Inc.
 *
 * RESTRICTED RIGHTS LEGEND:
 * Use, duplication or disclosure by the Government is subject to
 * restrictions as set forth in subdivision (c)(1)(ii) of the Rights
 * in Technical Data and Computer Software clause at DFARS
 * 252.227-7013, and/or in similar or successor clauses in the FAR,
 * DOD or NASA FAR Supplement. Unpublished - rights reserved under the
 * Copyright Laws of the United States.
 *====================================================================*/

/*---------------------------------------------------------------------*
        Copyright (C) 1998 Nintendo. (Originated by SGI)

        $RCSfile: os_cont.h,v $
        $Revision: 1.1 $
        $Date: 1998/10/09 08:01:05 $
 *---------------------------------------------------------------------*/

#ifndef _OS_CONT_H_
#define	_OS_CONT_H_

#ifdef _LANGUAGE_C_PLUS_PLUS
extern "C" {
#endif /* _LANGUAGE_C_PLUS_PLUS */

#include <PR/ultratypes.h>
#include "os_message.h"
#include "os_pfs.h"

#include "macros.h"


#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)

/**************************************************************************
 *
 * Type definitions
 *
 */

// -- Analog pairs --

typedef union {
    struct PACKED { s8 x:4, y:4; };
    struct PACKED { s8 l:4, r:4; };
    struct PACKED { s8 a:4, b:4; };
    s8 raw;
} Analog_s4; /*0x01*/
typedef union {
    struct PACKED { u8 x:4, y:4; };
    struct PACKED { u8 l:4, r:4; };
    struct PACKED { u8 a:4, b:4; };
    u8 raw;
} Analog_u4; /*0x01*/

typedef union {
    struct PACKED { s8 x:8, y:8; };
    struct PACKED { s8 l:8, r:8; };
    struct PACKED { s8 a:8, b:8; };
    s16 raw;
} Analog_s8; /*0x02*/
typedef union {
    struct PACKED { u8 x:8, y:8; };
    struct PACKED { u8 l:8, r:8; };
    struct PACKED { u8 a:8, b:8; };
    u16 raw;
} Analog_u8; /*0x02*/

#define ANALOG_U4_TO_U8(src) ((Analog_u8){  \
    ((src).a << 4),                         \
    ((src).b << 4),                         \
})

//! TODO: CLAMP_S8 is from math_util.h. Should it be included in this file?
#define ANALOG_S8_CENTER(stick, center) ((Analog_s8){   \
    CLAMP_S8((s32)(stick).x - (center).x),              \
    CLAMP_S8((s32)(stick).y - (center).y),              \
})

//! TODO: CLAMP_U8 is from math_util.h. Should it be included in this file?
#define ANALOG_U8_CENTER(stick, center) ((Analog_u8){   \
    CLAMP_U8((s32)(stick).x - (center).x),              \
    CLAMP_U8((s32)(stick).y - (center).y),              \
})

#define ANALOG_ZERO(type)   (type){ 0x00, 0x00 }
#define ANALOG_S8_ZERO      ANALOG_ZERO(Analog_s8)
#define ANALOG_U8_ZERO      ANALOG_ZERO(Analog_u8)

///////////////////
// Input structs //
///////////////////

// -- N64 Standard Controller buttons --

typedef struct PACKED {
    /*0x0*/ u16       : 4;
    /*0x0*/ u16 UP    : 1; // U_JPAD | CONT_UP
    /*0x0*/ u16 DOWN  : 1; // D_JPAD | CONT_DOWN
    /*0x0*/ u16 LEFT  : 1; // L_JPAD | CONT_LEFT
    /*0x0*/ u16 RIGHT : 1; // R_JPAD | CONT_RIGHT
    /*0x1*/ u16       : 8;
} N64Buttons_D; /*0x02*/

typedef struct PACKED {
    /*0x0*/ u16       : 12;
    /*0x1*/ u16 UP    : 1; // U_CBUTTONS | CONT_E
    /*0x1*/ u16 DOWN  : 1; // D_CBUTTONS | CONT_D
    /*0x1*/ u16 LEFT  : 1; // L_CBUTTONS | CONT_C
    /*0x1*/ u16 RIGHT : 1; // R_CBUTTONS | CONT_F
} N64Buttons_C; /*0x02*/

typedef union {
    struct PACKED {
        /*0x0*/ u16 A               : 1; // CONT_A
        /*0x0*/ u16 B               : 1; // CONT_B
        /*0x0*/ u16 Z               : 1; // CONT_G
        /*0x0*/ u16 START           : 1; // CONT_START
        /*0x0*/ u16                 : 4; // N64Buttons_D
        /*0x1*/ u16 RESET           : 1; // CONT_RESET
        /*0x1*/ u16 unused          : 1; // CONT_UNUSED
        /*0x1*/ u16 L               : 1; // CONT_L
        /*0x1*/ u16 R               : 1; // CONT_R
        /*0x1*/ u16                 : 4; // N64Buttons_C
    }; /*0x02*/
    N64Buttons_D D;
    N64Buttons_C C;
    u16 raw;
} N64StandardButtons; /*0x02*/

// -- Mouse buttons --

typedef union {
    struct PACKED {
        /*0x0*/ u16 CLICK_LEFT      :  1;
        /*0x0*/ u16 CLICK_RIGHT     :  1;
        /*0x0*/ u16                 : 14;
    }; /*0x02*/
    u16 raw;
} N64MouseButtons; /*0x02*/

// -- Train Controller buttons --

typedef union {
    struct PACKED {
        /*0x0*/ u16 B               : 1;
        /*0x0*/ u16 A               : 1;
        /*0x0*/ u16 ACC1            : 1;
        /*0x0*/ u16 START           : 1;
        /*0x0*/ u16 ACC2            : 1;
        /*0x0*/ u16 EX1             : 1;
        /*0x0*/ u16 EX2             : 1;
        /*0x0*/ u16 ACC3            : 1;
        /*0x1*/ u16 EX3             : 1;
        /*0x1*/ u16 EX4             : 1;
        /*0x1*/ u16 C               : 1;
        /*0x1*/ u16 SELECT          : 1;
        /*0x1*/ u16 BRAKE           : 4;
    }; /*0x02*/
    u16 raw;
} N64TrainButtons; /*0x02*/

// -- Fishing Rod buttons --

typedef union {
    struct PACKED {
        /*0x0*/ u16 A               : 1; // CONT_A
        /*0x0*/ u16 B               : 1; // CONT_B
        /*0x0*/ u16 Z               : 1; // CONT_G
        /*0x0*/ u16 START           : 1; // CONT_START
        /*0x0*/ u16 REEL            : 1; // Reel (clockwise); turning counterclockwise trips a slip gear.
        /*0x0*/ u16                 : 1;
        /*0x0*/ u16 TENSION_UP      : 1; // Increase Tension (toward player).
        /*0x0*/ u16 TENSION_DOWN    : 1; // Decrease Tension (away from player).
        /*0x1*/ u16                 : 4;
        /*0x1*/ u16                 : 4; // N64Buttons_C
    };
    N64Buttons_C C;
    u16 raw;
} N64FishingRodButtons; /*0x02*/

// -- N64 buttons union --

typedef union {
    N64StandardButtons standard;
    N64MouseButtons mouse;
    N64TrainButtons train;
    N64FishingRodButtons rod;
    u16 raw;
} N64Buttons; /*0x02*/

// -- N64 Input Data union --

typedef union {
    struct PACKED {
        /*0x00*/ N64Buttons buttons;    // The received button data.
        /*0x02*/ Analog_s8 stick;       // The received analog stick position [-80, 80].
    }; /*0x04*/
    union {
        u8 u8[4];
        u16 u16[2];
        u32 u32[1];
    } raw;
} N64InputData; /*0x04*/

// -- GCN Controller buttons --

typedef struct PACKED {
    /*0x0*/ u16       : 12;
    /*0x1*/ u16 UP    : 1; // CONT_GCN_UP
    /*0x1*/ u16 DOWN  : 1; // CONT_GCN_DOWN
    /*0x1*/ u16 RIGHT : 1; // CONT_GCN_LEFT
    /*0x1*/ u16 LEFT  : 1; // CONT_GCN_RIGHT
} GCNButtons_D; /*0x02*/

typedef union {
    struct PACKED {
        /*0x0*/ u16 ERRSTAT         : 1; // CONT_GCN_ERRSTAT    | Error status: Whether there was an error on last transfer.
        /*0x0*/ u16 ERRLATCH        : 1; // CONT_GCN_ERRLATCH   | Error Latched: Check SISR (GCN console register).
        /*0x0*/ u16 GET_ORIGIN      : 1; // CONT_GCN_GET_ORIGIN | Analog origins changed: Indicates that the controller's analog origins need to be updated console-side after an X+Y+START recalibration.
        /*0x0*/ u16 START           : 1; // CONT_GCN_START
        /*0x0*/ u16 Y               : 1; // CONT_GCN_Y
        /*0x0*/ u16 X               : 1; // CONT_GCN_X
        /*0x0*/ u16 B               : 1; // CONT_GCN_B
        /*0x0*/ u16 A               : 1; // CONT_GCN_A
        /*0x1*/ u16 USE_ORIGIN      : 1; // CONT_GCN_USE_ORIGIN | Use analog origins: 1 = standard controller, 0 = wavebird or bongos (used to detect bongos)?
        /*0x1*/ u16 L               : 1; // CONT_GCN_L
        /*0x1*/ u16 R               : 1; // CONT_GCN_R
        /*0x1*/ u16 Z               : 1; // CONT_GCN_Z
        /*0x1*/ u16                 : 4; // GCNButtons_D
    }; /*0x02*/
    GCNButtons_D D;
    u16 raw;
} GCNStandardButtons; /*0x02*/

// -- GCN DK Bongos buttons --

typedef union {
    struct PACKED {
        /*0x0*/ u16 ERRSTAT         : 1; // CONT_GCN_ERRSTAT    | Error status: Whether there was an error on last transfer.
        /*0x0*/ u16 ERRLATCH        : 1; // CONT_GCN_ERRLATCH   | Error Latched: Check SISR (GCN console register).
        /*0x0*/ u16 GET_ORIGIN      : 1; // CONT_GCN_GET_ORIGIN | Analog origins changed: Indicates that the controller's analog origins need to be updated console-side after an X+Y+START recalibration.
        /*0x0*/ u16 START           : 1; // CONT_GCN_START
        /*0x0*/ u16 LEFT_TOP        : 1;
        /*0x0*/ u16 RIGHT_TOP       : 1;
        /*0x0*/ u16 LEFT_BOTTOM     : 1;
        /*0x0*/ u16 RIGHT_BOTTOM    : 1;
        /*0x1*/ u16 USE_ORIGIN      : 1; // CONT_GCN_USE_ORIGIN | Use analog origins: 1 = standard controller, 0 = wavebird or bongos (used to detect bongos)?
        /*0x1*/ u16                 : 7;
    }; /*0x02*/
    u16 raw;
} GCNDKBongosButtons; /*0x02*/

// -- GCN buttons union --

typedef union {
    GCNStandardButtons standard;
    GCNDKBongosButtons dkbongos;
    u16 raw;
} GCNButtons; /*0x02*/

// -- GCN Analog Modes --

enum OSGCNAnalogModes {
    GCN_MODE_0_211, // 2-byte C-stick, 1-byte triggers, 1-byte buttons.
    GCN_MODE_1_121, // 1-byte C-stick, 2-byte triggers, 1-byte buttons.
    GCN_MODE_2_112, // 1-byte C-stick, 1-byte triggers, 2-byte buttons.
    GCN_MODE_3_220, // 2-byte C-stick, 2-byte triggers, 0-byte buttons.
    GCN_MODE_4_202, // 2-byte C-stick, 0-byte triggers, 2-byte buttons.
    GCN_MODE_5_211, // 2-byte C-stick, 1-byte triggers, 1-byte buttons.
    GCN_MODE_6_211, // 2-byte C-stick, 1-byte triggers, 1-byte buttons.
    GCN_MODE_7_211, // 2-byte C-stick, 1-byte triggers, 1-byte buttons.
};

// -- GCN Input Data union --

typedef union {
    struct PACKED {
        /*0x00*/ GCNButtons buttons;    // The received button data.
        /*0x02*/ Analog_u8 stick;       // The received analog stick position [-80, 80].
        union {                         // Lower bits:
            struct PACKED { // Default, same as mode 3.
                /*0x00*/ Analog_u8 c_stick;  // The received C-stick position [-80, 80].
                /*0x02*/ Analog_u8 trig;     // The received trigger position [0, 255].
            }; /*0x04*/
            struct PACKED { // Mode 0, 5, 6, 7.
                /*0x00*/ Analog_u8 c_stick;  // The received C-stick position [-80, 80].
                /*0x02*/ Analog_u4 trig;     // The received trigger position [0, 255].
                /*0x03*/ Analog_u4 buttons;  // Analog buttons used by some controllers.
            } m0; /*0x04*/
            struct PACKED { // Mode 1.
                /*0x00*/ Analog_u4 c_stick;  // The received C-stick position [-80, 80].
                /*0x01*/ Analog_u8 trig;     // The received trigger position [0, 255].
                /*0x03*/ Analog_u4 buttons;  // Analog buttons used by some controllers.
            } m1; /*0x04*/
            struct PACKED { // Mode 2.
                /*0x00*/ Analog_u4 c_stick;  // The received C-stick position [-80, 80].
                /*0x01*/ Analog_u4 trig;     // The received trigger position [0, 255].
                /*0x02*/ Analog_u8 buttons;  // Analog buttons used by some controllers.
            } m2; /*0x04*/
            struct PACKED { // Mode 3.
                /*0x00*/ Analog_u8 c_stick; // The received C-stick position [-80, 80].
                /*0x02*/ Analog_u8 trig;    // The received trigger position [0, 255].
            } m3; /*0x04*/
            struct PACKED { // Mode 4.
                /*0x00*/ Analog_u8 c_stick; // The received C-stick position [-80, 80].
                /*0x02*/ Analog_u8 buttons; // Analog buttons used by some controllers.
            } m4; /*0x04*/
            struct PACKED { // ASCII Controller.
                /*0x00*/ u8 keypress[3];    // Which keys are pressed (up to 3 keys can be pressed at a time). //! TODO: Key IDs.
                /*0x03*/ u8 status;         // Keyboard status.
            } keyboard; /*0x04*/
            struct PACKED { // DK Bongos.
                /*0x00*/ u8 unused[3];
                /*0x03*/ u8 mic;            // The DK Bongos' clap detector microphone.
            } bongos; /*0x04*/
        };
    }; /*0x08*/
    union {
        u8 u8[8];
        u16 u16[4];
        u32 u32[2];
    } raw; /*0x08*/
} GCNInputData; /*0x08*/

// -- Virtual Controller buttons (used by OSContPadEx) --

typedef union {
    struct PACKED {
        /*0x0*/ u16 A               : 1; // A_BUTTON
        /*0x0*/ u16 B               : 1; // B_BUTTON
        /*0x0*/ u16 Z               : 1; // Z_TRIG
        /*0x0*/ u16 START           : 1; // START_BUTTON
        /*0x0*/ u16                 : 4; // N64Buttons_D
        /*0x1*/ u16 X               : 1; // X_BUTTON
        /*0x1*/ u16 Y               : 1; // Y_BUTTON
        /*0x1*/ u16 L               : 1; // L_TRIG
        /*0x1*/ u16 R               : 1; // R_TRIG
        /*0x1*/ u16                 : 4; // N64Buttons_C
    }; /*0x02*/
    N64Buttons_D D;
    N64Buttons_C C;
    union {
        N64Buttons n64;
        GCNButtons gcn;
    } asPhysical;
    u16 raw;
} OSContButtons; /*0x02*/

/*
 * Structure for controllers
 */

typedef struct { //! TODO: Accessory type/detection.
    /*0x00*/ u16 type;                      /* Controller Type (SI identifier, byteswapped) */
    /*0x02*/ u8  status;                    /* Controller status */
    /*0x03*/ u8	 error;                     /* Error */
} OSContStatus; /*0x04*/

typedef struct {
    /*0x00*/ u16 button;                    /* Button data */
    /*0x02*/ s8  stick_x;                   /* -80 <= stick_x <=  80 */
    /*0x03*/ s8  stick_y;                   /* -80 <= stick_y <=  80 */
    /*0x04*/ u8  error;                     /* Error */
} OSContPad; /*0x05*/

typedef struct PACKED {
    /*0x00*/ u8 pad[1];
    /*0x01*/ _Bool initialized;             /* Whether this controller's origins have been set. */
    /*0x02*/ Analog_u8 stick;               /* -80 <=   stick <=  80 */
    /*0x04*/ Analog_u8 c_stick;             /* -80 <= c_stick <=  80 */
    /*0x06*/ Analog_u8 trig;                /*   0 <= trig    <= 255 */
} OSContOrigins; /*0x08*/

// Custom extended controller pad struct that contains fields for gamecube controllers
typedef struct {
    /*0x00*/ OSContButtons button;          /* Button data */
    /*0x02*/ OSContButtons lockedButton;    /* Button data to ignore */
    /*0x04*/ OSContButtons statPollButton;  /* Previous frame's inputs when status polling. */
    /*0x06*/ OSContButtons physButton;      /* Raw physical button data received from the controller */
    /*0x08*/ Analog_s8 stick;               /* -80 <=   stick <=  80 */
    /*0x0A*/ Analog_s8 c_stick;             /* -80 <= c_stick <=  80 */
    /*0x0C*/ Analog_u8 trig;                /*   0 <= trig    <= 255 */
    /*0x0E*/ OSContOrigins origins;         /* GCN analog origins */
    /*0x16*/ u8 playerNum;                  /* Player number (0 = not assigned) */
    /*0x17*/ u8	errno;                      /* Error number */
} OSContPadEx; /*0x18*/

typedef struct {
    /*0x00*/ void *address;                 /* Ram pad Address: 11 bits */
    /*0x04*/ u8   databuffer[BLOCKSIZE];    /* address of the data buffer */
    /*0x05*/ u8   addressCrc;               /* CRC code for address */
    /*0x06*/ u8   dataCrc;                  /* CRC code for data */
    /*0x07*/ u8   error;                    /* Error */
} OSContRamIo; /*0x08*/


#endif /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

/**************************************************************************
 *
 * Global definitions
 *
 */

/*
 * Number of controllers
 */

#ifndef _HW_VERSION_1
#define MAXCONTROLLERS 4
#else /* !_HW_VERSION_1 */
#define MAXCONTROLLERS 6
#endif /* !_HW_VERSION_1 */

/* Controller errors */
#define CONT_NO_RESPONSE_ERROR              (1 << 3) /* 0x8 */
#define CONT_OVERRUN_ERROR                  (1 << 2) /* 0x4 */
#ifdef _HW_VERSION_1
#define CONT_FRAME_ERROR                    (1 << 1) /* 0x2 */
#define CONT_COLLISION_ERROR                (1 << 0) /* 0x1 */
#endif /* _HW_VERSION_1 */

/* Controller type bits (byteswapped due to __osContGetInitData) */

#define CONT_NONE                           0x0000

// N64:
#define CONT_ABSOLUTE                       (  1 <<  0) // 0x0001
#define CONT_RELATIVE                       (  1 <<  1) // 0x0002
#define CONT_JOYPORT                        (  1 <<  2) // 0x0004

// Console ID:
#define CONT_CONSOLE_MASK                   (0x3 <<  3) // 0x0018 | 0: N64, 1: Dolphin
#define CONT_CONSOLE_N64                    (  0 <<  3) // 0x0000
#define CONT_CONSOLE_GCN                    (  1 <<  3) // 0x0008

// Gamecube:
#define CONT_GCN_WIRELESS                   (  1 <<  7) // 0x0080 | 0: wired, 1: wireless
#define CONT_GCN_MOMOTOR                    (  1 <<  5) // 0x0020 | 0: available, 1: not available
#define CONT_GCN_STANDARD                   (  1 <<  0) // 0x0001 | 0: non standard controller, 1: Dolphin Standard Controller

// Wireless (WaveBird):
#define CONT_GCN_RECEIVED                   (  1 <<  6) // 0x0040 | 0: not wireless, 1: wireless
#define CONT_GCN_WIRELESS_RF                (  1 <<  2) // 0x0004 | 0: IF, 1: RF
#define CONT_GCN_WIRELESS_STATE_FIXED       (  1 <<  1) // 0x0002 | 0: variable, 1: fixed
#define CONT_GCN_WIRELESS_VALID_ORIGIN      (  1 << 13) // 0x2000 | 0: invalid, 1: valid
#define CONT_GCN_WIRELESS_FIX_ID            (  1 << 12) // 0x1000 | 0: not fixed, 1: fixed
#define CONT_GCN_WIRELESS_TYPE_MASK         (0xF <<  8) // 0x0F00 | 0x0: normal, 0x4: lite, 0x8: non-controller
#define CONT_GCN_WIRELESS_LITE_MASK         (0xC <<  8) // 0x0C00
#define CONT_GCN_WIRELESS_LITE              (  1 << 10) // 0x0400 | 0: normal, 1: lite controller
#define CONT_GCN_WIRELESS_NON_CONTROLLER    (  1 << 11) // 0x0800 | 0: normal, 1: non-controller (?)

#define CONT_GCN_WIRELESS_ID                (0xC <<  8) // 0x0C00
#define CONT_GCN_WIRELESS_TYPE_ID           (CONT_GCN_WIRELESS_TYPE_MASK | CONT_GCN_WIRELESS_ID)

/* Controller types (byteswapped due to __osContGetInitData) */

#define CONT_TYPE_NULL          (0xFFFF)
#define CONT_TYPE_UNKNOWN       (0x0000)
// EEP:
#define CONT_EEPROM             (0x8000)
#define CONT_EEP16K             (0x4000)
// N64:
#define	CONT_TYPE_NORMAL        (0x0000 | CONT_CONSOLE_N64 | CONT_ABSOLUTE | CONT_JOYPORT) // 0x0005
#define	CONT_TYPE_MOUSE         (0x0000 | CONT_CONSOLE_N64 | CONT_RELATIVE) // 0x0002
#define	CONT_TYPE_VOICE         (0x0100 | CONT_CONSOLE_N64) // 0x0100
#define CONT_TYPE_KEYBOARD      (0x0200 | CONT_CONSOLE_N64) // 0x0200
#define CONT_TYPE_DANCEPAD      CONT_TYPE_NORMAL
#define CONT_TYPE_DENSHA        CONT_TYPE_NORMAL
#define CONT_TYPE_FISHING       CONT_TYPE_NORMAL
// GBA:
#define CONT_TYPE_64GB          (0x0003) // 0x0003
#define CONT_TYPE_GBA           (0x0400) // 0x0400
// Gamecube:
#define CONT_TYPE_GCN_NORMAL    (0x0000 | CONT_CONSOLE_GCN | CONT_GCN_STANDARD) // 0x0009
#define CONT_TYPE_GCN_RECEIVER  (0x0000 | CONT_CONSOLE_GCN | CONT_GCN_WIRELESS) // 0x0088
#define CONT_TYPE_GCN_WAVEBIRD  (0x0000 | CONT_CONSOLE_GCN | CONT_GCN_WIRELESS | CONT_GCN_STANDARD | CONT_GCN_WIRELESS_STATE_FIXED | CONT_GCN_WIRELESS_FIX_ID) // 0x108B
#define CONT_TYPE_GCN_WHEEL     (0x0000 | CONT_CONSOLE_GCN) // 0x0008
#define CONT_TYPE_GCN_KEYBOARD  (0x2000 | CONT_CONSOLE_GCN) // 0x2008
#define CONT_TYPE_GCN_DANCEPAD  (0x0800 | CONT_CONSOLE_GCN | CONT_GCN_STANDARD) // 0x0808
#define CONT_TYPE_GCN_DKONGAS   CONT_TYPE_GCN_NORMAL

/* Controller status */

#define CONT_CARD_ON            (1 << 0) // 0x01
#define CONT_CARD_PULL          (1 << 1) // 0x02
#define CONT_ADDR_CRC_ER        (1 << 2) // 0x04
#define CONT_EEPROM_BUSY        (1 << 7) // 0x80

/* N64 Buttons */

#define CONT_A                  (1 << 15) // 0x8000 | A
#define CONT_B                  (1 << 14) // 0x4000 | B
#define CONT_G                  (1 << 13) // 0x2000 | Z
#define CONT_START              (1 << 12) // 0x1000 | START
#define CONT_UP                 (1 << 11) // 0x0800 | D-Up
#define CONT_DOWN               (1 << 10) // 0x0400 | D-Down
#define CONT_LEFT               (1 <<  9) // 0x0200 | D-Left
#define CONT_RIGHT              (1 <<  8) // 0x0100 | D-Right
#define CONT_RESET              (1 <<  7) // 0x0080 | Reset analog (L+R+START)
#define CONT_UNUSED             (1 <<  6) // 0x0040 | Unused
#define CONT_L                  (1 <<  5) // 0x0020 | L
#define CONT_R                  (1 <<  4) // 0x0010 | R
#define CONT_E                  (1 <<  3) // 0x0008 | C-Up
#define CONT_D                  (1 <<  2) // 0x0004 | C-Down
#define CONT_C                  (1 <<  1) // 0x0002 | C-Left
#define CONT_F                  (1 <<  0) // 0x0001 | C-Right

/* Gamecube controller buttons */

#define CONT_GCN_ERRSTAT        (1 << 15) // 0x8000 | Error status: Whether there was an error on last transfer.
#define CONT_GCN_ERRLATCH       (1 << 14) // 0x4000 | Error Latched: Check SISR (GCN console register).
#define CONT_GCN_GET_ORIGIN     (1 << 13) // 0x2000 | Analog origins changed: Indicates that the controller's analog origins need to be updated console-side after an X+Y+START recalibration.
#define CONT_GCN_START          (1 << 12) // 0x1000 | START/PAUSE
#define CONT_GCN_Y              (1 << 11) // 0x0800 | Y
#define CONT_GCN_X              (1 << 10) // 0x0400 | X
#define CONT_GCN_B              (1 <<  9) // 0x0200 | B
#define CONT_GCN_A              (1 <<  8) // 0x0100 | A
#define CONT_GCN_USE_ORIGIN     (1 <<  7) // 0x0080 | Use analog origins: 1 = standard controller, 0 = wavebird or bongos (used to detect bongos)?
#define CONT_GCN_L              (1 <<  6) // 0x0040 | L
#define CONT_GCN_R              (1 <<  5) // 0x0020 | R
#define CONT_GCN_Z              (1 <<  4) // 0x0010 | Z
#define CONT_GCN_UP             (1 <<  3) // 0x0008 | D-Up
#define CONT_GCN_DOWN           (1 <<  2) // 0x0004 | D-Down
#define CONT_GCN_RIGHT          (1 <<  1) // 0x0002 | D-Right
#define CONT_GCN_LEFT           (1 <<  0) // 0x0001 | D-Left

/* Nintendo's official button names */

#define A_BUTTON                CONT_A      // 0x8000
#define B_BUTTON                CONT_B      // 0x4000
#define Z_TRIG                  CONT_G      // 0x2000
#define START_BUTTON            CONT_START  // 0x1000
#define U_JPAD                  CONT_UP     // 0x0800
#define D_JPAD                  CONT_DOWN   // 0x0400
#define L_JPAD                  CONT_LEFT   // 0x0200
#define R_JPAD                  CONT_RIGHT  // 0x0100
#define X_BUTTON                CONT_RESET  // 0x0080
#define Y_BUTTON                CONT_UNUSED // 0x0040
#define L_TRIG                  CONT_L      // 0x0020
#define R_TRIG                  CONT_R      // 0x0010
#define U_CBUTTONS              CONT_E      // 0x0008
#define D_CBUTTONS              CONT_D      // 0x0004
#define L_CBUTTONS              CONT_C      // 0x0002
#define R_CBUTTONS              CONT_F      // 0x0001

/* Controller error number */

enum OSContError {
    CONT_ERR_NO_CONTROLLER = PFS_ERR_NOPACK,      /*  1 */
    CONT_ERR_CONTRFAIL     = CONT_OVERRUN_ERROR,  /*  4 */
    CONT_ERR_INVALID       = PFS_ERR_INVALID,     /*  5 */
    CONT_ERR_DEVICE        = PFS_ERR_DEVICE,      /* 11 */
    CONT_ERR_NOT_READY,
    CONT_ERR_VOICE_MEMORY,
    CONT_ERR_VOICE_WORD,
    CONT_ERR_VOICE_NO_RESPONSE,
};


#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)

/**************************************************************************
 *
 * Macro definitions
 *
 */


/**************************************************************************
 *
 * Extern variables
 *
 */


/**************************************************************************
 *
 * Function prototypes
 *
 */

/* Controller interface */

extern s32  osContInit(           OSMesgQueue* mq, u8* bitpattern, OSContStatus* status);
extern s32  osContReset(          OSMesgQueue* mq,                 OSContStatus* status);
extern s32  osContStartQuery(     OSMesgQueue* mq);
extern s32  osContStartReadData(  OSMesgQueue* mq);
extern s32  osStartRead_impl(     OSMesgQueue* mq, u8 cmd);
#ifndef _HW_VERSION_1
extern s32  osContSetCh(u8 ch);
#endif
extern void osContGetQuery(                  OSContStatus* status);
extern void osContGetQueryEx(u8* bitpattern, OSContStatus* status);
extern void osContGetReadData(  OSContPad*   pad);
extern void osContGetReadDataEx(OSContPadEx* pad);


#endif /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif /* _LANGUAGE_C_PLUS_PLUS */

#endif /* !_OS_CONT_H_ */
