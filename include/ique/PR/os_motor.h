
/*---------------------------------------------------------------------*
        Copyright (C) 1998 Nintendo.

        $RCSfile: os_motor.h,v $
        $Revision: 1.1.1.1 $
        $Date: 2002/05/02 03:28:25 $
 *---------------------------------------------------------------------*/

#ifndef _OS_MOTOR_H_
#define _OS_MOTOR_H_

#ifdef _LANGUAGE_C_PLUS_PLUS
extern "C" {
#endif /* _LANGUAGE_C_PLUS_PLUS */

#include <PR/ultratypes.h>
#include "os_message.h"
#include "os_pfs.h"


#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)

/**************************************************************************
 *
 * Type definitions
 *
 */


#endif /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

/**************************************************************************
 *
 * Global definitions
 *
 */


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

/* Rumble PAK interface */

extern s32 osMotorInit(  OSMesgQueue *mq, OSPfs *pfs, int controller_no);
extern s32 osMotorInitEx(OSMesgQueue *mq, OSPfs *pfs, int controller_no);
#define MOTOR_MASK_N64  0b01
#define MOTOR_MASK_GCN  0b11
enum OSMotorOP {
    MOTOR_STOP,
    MOTOR_START,
    MOTOR_STOP_HARD, // GCN only.
};
#define osMotorStopHard(x)  __osMotorAccessEx((x), MOTOR_STOP_HARD)
#define osMotorStart(x)     __osMotorAccessEx((x), MOTOR_START)
#define osMotorStop(x)      __osMotorAccessEx((x), MOTOR_STOP)
extern s32 __osMotorAccess(  OSPfs *pfs, s32 flag);
extern s32 __osMotorAccessEx(OSPfs *pfs, s32 flag);


#endif /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif /* _LANGUAGE_C_PLUS_PLUS */

#endif /* !_OS_MOTOR_H_ */
