
/*====================================================================
 * os_thread.h
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
        
        $RCSfile: os_thread.h,v $
        $Revision: 1.4 $
        $Date: 2004/02/06 02:16:48 $
 *---------------------------------------------------------------------*/

#ifndef _OS_THREAD_H_
#define	_OS_THREAD_H_

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

typedef s32	OSPri;
typedef s32	OSId;
typedef union	{ struct { f32 f_odd; f32 f_even; } f; f64 d; }	__OSfp;

typedef struct {
	u64	at, v0, v1, a0, a1, a2, a3;
	u64	t0, t1, t2, t3, t4, t5, t6, t7;
	u64	s0, s1, s2, s3, s4, s5, s6, s7;
	u64	t8, t9,         gp, sp, s8, ra;
	u64	lo, hi;
	u32	sr, pc, cause, badvaddr, rcp;
	u32	fpcsr;
	__OSfp	 fp0,  fp2,  fp4,  fp6,  fp8, fp10, fp12, fp14;
	__OSfp	fp16, fp18, fp20, fp22, fp24, fp26, fp28, fp30;
} __OSThreadContext;

typedef struct {
    u32 flag;
    u32 count;
    u64 time;
} __OSThreadprofile_s;

typedef struct OSThread_s {
	struct OSThread_s	*next;		/* run/mesg queue link */
	OSPri			priority;	/* run/mesg queue priority */
	struct OSThread_s	**queue;	/* queue thread is on */
	struct OSThread_s	*tlnext;	/* all threads queue link */
	u16			state;		/* OS_STATE_* */
	u16			flags;		/* flags for rmon */
	OSId			id;		/* id for debugging */
	int			fp;		/* thread has used fp unit */
	__OSThreadprofile_s     *thprof;        /* workarea for thread profiler */
	__OSThreadContext	context;	/* register/interrupt mask */
} OSThread;


#endif /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

/**************************************************************************
 *
 * Global definitions
 *
 */

/* Thread states */

#define OS_STATE_STOPPED	1
#define OS_STATE_RUNNABLE	2
#define OS_STATE_RUNNING	4
#define OS_STATE_WAITING	8

/* Recommended thread priorities for the system threads */

#define OS_PRIORITY_MAX		255
#define OS_PRIORITY_VIMGR	254
#define OS_PRIORITY_RMON	250
#define OS_PRIORITY_RMONSPIN	200
#define OS_PRIORITY_PIMGR	150
#define OS_PRIORITY_SIMGR	140
#define	OS_PRIORITY_APPMAX	127
#define OS_PRIORITY_IDLE	  0	/* Must be 0 */

#ifdef BBPLAYER
/* BB Player thread IDs */
#define OS_TID_RMONMAIN     3201
#define OS_TID_RMONIO       3202
#define OS_TID_PROFILEIO    3251
#define OS_TID_PIMGR        3301
#define OS_TID_RAMROM       3302
#define OS_TID_VIMGR        3401

/* We also have two USB threads (see osint_usb.h) */
#define	OS_TID_USB0		    (OS_USB_TID_BASE)   /* 3141 */
#define	OS_TID_USB1		    (OS_USB_TID_BASE+1) /* 3142 */

/* Also define constants for GDB */
#define OS_PRIORITY_GDB   OS_PRIORITY_RMON
#define OS_TID_GDBMAIN    OS_TID_RMONMAIN
#define OS_TID_GDBIO      OS_TID_RMONIO

#endif

/* for thread profiler */
#define THPROF_IDMAX            64
#define THPROF_STACKSIZE        256

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

/* Thread operations */

extern void		osCreateThread( OSThread *t, OSId id, void (*entry)(void *),
				       void *arg, void *sp, OSPri pri);
extern void		osDestroyThread(OSThread *t);
extern void		osYieldThread(void);
extern void		osStartThread(  OSThread *t);
extern void		osStopThread(   OSThread *t);
extern OSId		osGetThreadId(  OSThread *t);
extern void		osSetThreadPri( OSThread *t, OSPri pri);
extern OSPri	osGetThreadPri( OSThread *t);


#endif  /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif

#endif /* !_OS_THREAD_H_ */
