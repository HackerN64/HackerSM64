/**************************************************************************
 *									  *
 *		 Copyright (C) 1995, Silicon Graphics, Inc.		  *
 *									  *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *									  *
 **************************************************************************/

/**************************************************************************
 *
 *  $Revision: 1.2 $
 *  $Date: 2004/02/06 02:27:21 $
 *  $Source: /home/routefree/bb/depot/rf/sw/bbplayer/include/rmon.h,v $
 *
 **************************************************************************/

#ifndef _RMON_H_
#define	_RMON_H_

#ifdef _LANGUAGE_C_PLUS_PLUS
extern "C" {
#endif

#include <PR/ultratypes.h>
#define RMON_DBG_BUF_SIZE  2048
#define RMON_STACKSIZE 0x1000

#ifdef BBPLAYER

/* Let's use GDB debugger instead of RMON */
#define rmonMain    gdbMain
#define rmonPrintf  osSyncPrintf

#define GDB_DBG_BUF_SIZE  2048
#define GDB_STACKSIZE     0x1000

extern void gdbMain( void * );
extern void gdbInit();

#else
extern void rmonMain( void * );
extern void rmonPrintf( const char *, ... );
#endif

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif

#endif /* !_OS_H */
