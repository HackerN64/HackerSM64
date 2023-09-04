/*====================================================================
 * os_usb.h
 *
 * Copyright 2003, BroadOn Communications Corp.
 * All Rights Reserved.
 *
 * This is UNPUBLISHED PROPRIETARY SOURCE CODE of BroadOn Communications
 * Corp.; the contents of this file may not be disclosed to third
 * parties, copied or duplicated in any form, in whole or in part,
 * without the prior written permission of BroadOn Communications.
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
        Copyright (C) 2003 BroadOn Communications Corp
        
        $RCSfile: os_usb.h,v $
        $Revision: 1.12 $
        $Date: 2004/09/03 03:52:47 $
 *---------------------------------------------------------------------*/

#ifndef _OS_USB_H_
#define	_OS_USB_H_

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

/*
 * Structure to describe USB device
 */
typedef struct OSBbUsbInfo_s {
	u8      ua_type;        /* controller mode (host or device) */
	u8      ua_class;       /* device class */
	u8      ua_subclass;    /* subclass */
	u8      ua_protocol;    /* protocol */
	u16     ua_vendor;      /* vendor */
	u16     ua_product;     /* product */
	u8      ua_cfg;         /* configuration */
	u8      ua_ifc;         /* interface */
	u8      ua_ep;          /* endpoint */
	u8      ua_speed;       /* LS/FS/HS */
	u8      ua_mode;        /* R, W or R/W */
	u8      ua_blksize;     /* block size */
	u8      ua_support;     /* have driver */
	u8	ua_state;       /* device state (attached?) */
	u8	*ua_mfr_str;	/* manufacturer string */
	u8	*ua_prod_str;	/* product description string */
	u8	*ua_driver_name; /* driver name */
} OSBbUsbInfo;

/*
 * Handle for USB device
 */
typedef void *OSBbUsbHandle;

#endif /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

/**************************************************************************
 *
 * Global definitions
 *
 */

/*
 * USB Controller Modes
 */
#define OS_USB_TYPE_DISABLED	0
#define OS_USB_TYPE_HOST	1
#define OS_USB_TYPE_DEVICE	2
#define OS_USB_TYPE_EITHER	(OS_USB_TYPE_HOST|OS_USB_HOST_DEVICE)

/*
 * USB Device States
 */
#define OS_USB_STATE_NULL	0
#define OS_USB_STATE_ATTACHED	1

/*
 * USB Device Classes
 */
#define OS_USB_DEV_GENERIC	0
#define OS_USB_DEV_SERIAL	1
#define OS_USB_DEV_MODEM	2
#define OS_USB_DEV_MSD		3

/*
 * USB Device Speeds
 */
#define OS_USB_LOW_SPEED	0	/* LS (1Mb/s) */
#define OS_USB_FULL_SPEED	1	/* FS (12Mb/s) */
#define OS_USB_HIGH_SPEED	2	/* HS (480Mb/s) */

/*
 * USB Device Access Modes
 */
#define OS_USB_MODE_READ	1	/* Read only */
#define OS_USB_MODE_WRITE	2	/* Write only */
#define OS_USB_MODE_RW		3	/* R/W = READ | WRITE */


/*
 * USB subsystem parameters
 */
#define OS_USB_MAX_CONTROLLERS  2
#define OS_USB_QUEUE_SIZE  128

/*
 * Error codes
 */
#define OS_USB_ERROR_BASE	201
#define EUSBRINVAL		OS_USB_ERROR_BASE+0
#define EUSBAGAIN		OS_USB_ERROR_BASE+1
#define EUSBBUSY		OS_USB_ERROR_BASE+2
#define EUSBNXIO		OS_USB_ERROR_BASE+3

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

/* USB interface */
extern s32	osBbUsbInit(void);
extern s32	osBbUsbSetCtlrModes(s32, u32);
extern u32	osBbUsbGetResetCount(s32);
extern s32	osBbUsbDevQuery(    s32, OSBbUsbInfo *, s32);
extern s32	osBbUsbDevGetHandle(s32, OSBbUsbInfo *, OSBbUsbHandle *);
extern s32	osBbUsbDevFreeHandle(OSBbUsbHandle);
extern s32	osBbUsbDevRead(      OSBbUsbHandle, u8 *, s32, u64);
extern s32	osBbUsbDevWrite(     OSBbUsbHandle, u8 *, s32, u64);


#endif  /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif

#endif /* !_OS_USB_H_ */
