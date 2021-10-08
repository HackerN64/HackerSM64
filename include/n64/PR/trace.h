#ifndef _TRACE_H_
#define _TRACE_H_


/**************************************************************************
 *									  *
 *		 Copyright (C) 1994, Silicon Graphics, Inc.		  *
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
 *  Module: trace.h
 *
 *  $Revision: 1.1.1.2 $
 *  $Date: 2002/10/29 08:06:34 $
 *  $Author: blythe $
 *  $Source: /home/ryzen/cvsaaaa/depot/rf/sw/n64os20l/include/trace.h,v $
 *
 *  Description:
 *      This file contains debbuging and tracing macros
 *
 **************************************************************************/


/***************************************
 *
 * Macro definitions
 *
 */

/* Debug macros */

#ifdef _TRACE
#undef _TRACE
#endif

#ifdef _DEBUG

#if defined( _EMULATOR ) || ( _INDY_TARGET )
extern unsigned long	Dflags;		/* Global debug flag */

					/* Debug flag settings */
#define	DALL		0xFFFFFFFF	/* Turn on all debug types */
#define	DERROR		0x00000001	/* Turn on error tracing */
#define	DTHREAD		0x00000002	/* Turn on thread tracing */
#define	DTIMER		0x00000004	/* Turn on timer tracing */
#define	DMESSAGE	0x00000008	/* Turn on message tracing */
#define	DMEMORY		0x00000010	/* Turn on memory tracing */
#define	DREGION		0x00000020	/* Turn on region tracing */

#define _TRACE(type, code)	{ if (Dflags & (type)) \
			{ printf("%s, %d: ", __FILE__, __LINE__); code; } }
#else

#define _TRACE(type, code)

#endif  /* _EMULATOR | _INDY_TARGET */

#else

#define _TRACE(type, code)

#endif  /* _DEBUG */


/***************************************
 *
 * Extern function prototypes
 *
 */


#endif  /* _TRACE_H_ */

