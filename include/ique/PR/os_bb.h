/*---------------------------------------------------------------------*
        Copyright (C) 2002 BroadOn Communications.
        
        $RCSfile: os_bb.h,v $
        $Revision: 1.13 $
        $Date: 2004/06/28 22:34:42 $
 *---------------------------------------------------------------------*/

#ifndef _OS_BB_H_
#define	_OS_BB_H_

#ifdef _LANGUAGE_C_PLUS_PLUS
extern "C" {
#endif

#include <PR/ultratypes.h>


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

/* BB Player Events */
#define OS_EVENT_FLASH         23   /* flash ctrl interrupt */
#define OS_EVENT_AES           24   /* aes ctrl interrupt */
#define OS_EVENT_IDE           25   /* io interface interrupt */
#define OS_EVENT_PI_ERR        26   /* pi error interrupt */
#define OS_EVENT_USB0          27   /* usb0 interrupt */
#define OS_EVENT_USB1          28   /* usb1 interrupt */
#define OS_EVENT_BUT           29   /* button interrupt */
#define OS_EVENT_MD            30   /* module detect interrupt */
#define OS_EVENT_DBG_WAIT_BRK  31   /* waiting for breakpoint */
#define OS_EVENT_RDB_READ_RESET 32   /* RDB was reset */
#define OS_EVENT_RDB_DATA_RESET 33   /* RDB was reset */
#define OS_NUM_EVENTS           34	/* contradicts os_message.h */

/* BB Player I/O Messages */
#define OS_MESG_TYPE_FLASH	(OS_MESG_TYPE_BASE+7)
#define OS_MESG_TYPE_MD		(OS_MESG_TYPE_BASE+8)
#define OS_MESG_TYPE_RDB_READ_DONE		(OS_MESG_TYPE_BASE+9)
#define OS_MESG_TYPE_RDB_READ_RESET		(OS_MESG_TYPE_BASE+10)
#define OS_MESG_TYPE_RDB_DATA_DONE		(OS_MESG_TYPE_BASE+11)
#define OS_MESG_TYPE_RDB_DATA_RESET		(OS_MESG_TYPE_BASE+12)

/* Stack sizes for managers */
#define OS_CARDM_STACKSIZE	4096

#include "os.h"
#include "os_bbcard.h"
#include "os_bbfs.h"
#include "os_usb.h"
#include "os_bbatb.h"
#include "os_bbexec.h"


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

extern  void    osBbPowerOff(void);
extern  void    osBbPowerOn(void);
extern  void    osBbSetErrorLed(u32 value);
extern  u32     osBbGetErrorLed(void);
extern  u32     osBbGetBoxId(void);
extern  s32	osBbExecFile(s32 fd, const char *name, OSBbLaunchMetaData *md,
	                     u8 buffer[16*1024]);
extern void osBbRtcInit(void);
extern void osBbRtcSet(u8 year, u8 month, u8 day, u8 dow, u8 hour, u8 min,
	               u8 sec);
extern void osBbRtcGet(u8* year, u8* month, u8* day, u8* dow, u8* hour, u8* min,
	               u8* sec);


#endif  /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif

#endif /* !_OS_BB_H_ */
