
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

#define ANALOG_S8_CENTER(stick, center) ((Analog_s8){   \
    CLAMP_S8((s32)(stick).x - (center).x),              \
    CLAMP_S8((s32)(stick).y - (center).y),              \
})

#define ANALOG_U8_CENTER(stick, center) ((Analog_u8){   \
    CLAMP_U8((s32)(stick).x - (center).x),              \
    CLAMP_U8((s32)(stick).y - (center).y),              \
})

/*
 * Structure for controllers
 */

typedef struct {
    /*0x00*/ u16 type;                      /* Controller Type */
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
    /*0x00*/ s8 initialized;                /* Whether this controller's origins have been set. */
    /*0x01*/ Analog_u8 stick;                /* -80 <=   stick <=  80 */
    /*0x03*/ Analog_u8 c_stick;              /* -80 <= c_stick <=  80 */
    /*0x05*/ Analog_u8 trig;                 /*   0 <= trig    <= 255 */
} OSContOrigins; /*0x07*/

// Custom extended controller pad struct that contains fields for gamecube controllers
typedef struct {
    /*0x00*/ u16 button;                    /* Button data */
    /*0x02*/ u16 lockedButton;              /* Button data to ignore */
    /*0x04*/ Analog_s8 stick;               /* -80 <=   stick <=  80 */
    /*0x06*/ Analog_s8 c_stick;             /* -80 <= c_stick <=  80 */
    /*0x08*/ Analog_u8 trig;                /*   0 <= trig    <= 255 */
    /*0x0A*/ OSContOrigins origins;         /* GCN analog origins */
    /*0x0F*/ u8	errno;                      /* Error number */
} OSContPadEx; /*0x10*/

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

/* Controller type bits (byteswapped due to __osContGetInitData/__osContGetInitDataEx) */

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

/* Controller types (byteswapped due to __osContGetInitData/__osContGetInitDataEx) */

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

#define CONT_A                  (1 << 15) // 0x8000
#define CONT_B                  (1 << 14) // 0x4000
#define CONT_G                  (1 << 13) // 0x2000
#define CONT_START              (1 << 12) // 0x1000
#define CONT_UP                 (1 << 11) // 0x0800
#define CONT_DOWN               (1 << 10) // 0x0400
#define CONT_LEFT               (1 <<  9) // 0x0200
#define CONT_RIGHT              (1 <<  8) // 0x0100
#define CONT_RESET              (1 <<  7) // 0x0080
#define CONT_UNUSED             (1 <<  6) // 0x0040
#define CONT_L                  (1 <<  5) // 0x0020
#define CONT_R                  (1 <<  4) // 0x0010
#define CONT_E                  (1 <<  3) // 0x0008
#define CONT_D                  (1 <<  2) // 0x0004
#define CONT_C                  (1 <<  1) // 0x0002
#define CONT_F                  (1 <<  0) // 0x0001

/* Nintendo's official button names */

#define A_BUTTON                CONT_A      // 0x8000
#define B_BUTTON                CONT_B      // 0x4000
#define X_BUTTON                CONT_RESET  // 0x0080
#define Y_BUTTON                CONT_UNUSED // 0x0040
#define L_TRIG                  CONT_L      // 0x0020
#define R_TRIG                  CONT_R      // 0x0010
#define Z_TRIG                  CONT_G      // 0x2000
#define START_BUTTON            CONT_START  // 0x1000
#define U_JPAD                  CONT_UP     // 0x0800
#define L_JPAD                  CONT_LEFT   // 0x0200
#define R_JPAD                  CONT_RIGHT  // 0x0100
#define D_JPAD                  CONT_DOWN   // 0x0400
#define U_CBUTTONS              CONT_E      // 0x0008
#define L_CBUTTONS              CONT_C      // 0x0002
#define R_CBUTTONS              CONT_F      // 0x0001
#define D_CBUTTONS              CONT_D      // 0x0004

/* Gamecube controller buttons */

#define CONT_GCN_ERRSTAT        (1 << 15) // 0x8000
#define CONT_GCN_ERRLATCH       (1 << 14) // 0x4000
#define CONT_GCN_GET_ORIGIN     (1 << 13) // 0x2000
#define CONT_GCN_START          (1 << 12) // 0x1000
#define CONT_GCN_Y              (1 << 11) // 0x0800
#define CONT_GCN_X              (1 << 10) // 0x0400
#define CONT_GCN_B              (1 <<  9) // 0x0200
#define CONT_GCN_A              (1 <<  8) // 0x0100
#define CONT_GCN_USE_ORIGIN     (1 <<  7) // 0x0080
#define CONT_GCN_L              (1 <<  6) // 0x0040
#define CONT_GCN_R              (1 <<  5) // 0x0020
#define CONT_GCN_Z              (1 <<  4) // 0x0010
#define CONT_GCN_UP             (1 <<  3) // 0x0008
#define CONT_GCN_DOWN           (1 <<  2) // 0x0004
#define CONT_GCN_RIGHT          (1 <<  1) // 0x0002
#define CONT_GCN_LEFT           (1 <<  0) // 0x0001

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

extern s32  osContInit(           OSMesgQueue *mq, u8 *bitpattern, OSContStatus *status);
extern s32  osContReset(          OSMesgQueue *mq,                 OSContStatus *status);
extern s32  osContStartQuery(     OSMesgQueue *mq);
extern s32  osContStartReadData(  OSMesgQueue *mq);
extern s32  osContStartReadDataEx(OSMesgQueue *mq);
#ifndef _HW_VERSION_1
extern s32  osContSetCh(u8 ch);
#endif
extern void osContGetQuery(                  OSContStatus *status);
extern void osContGetQueryEx(u8 *bitpattern, OSContStatus *status);
extern void osContGetReadData(  OSContPad   *pad);
extern void osContGetReadDataEx(OSContPadEx *pad);


#endif /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif /* _LANGUAGE_C_PLUS_PLUS */

#endif /* !_OS_CONT_H_ */
