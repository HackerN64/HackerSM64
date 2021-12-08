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

/*---------------------------------------------------------------------*
        Copyright (C) 1998 Nintendo. (Originated by SGI)
        
        $RCSfile: os_internal_si.h,v $
        $Revision: 1.1.1.1 $
        $Date: 2002/05/02 03:28:25 $
 *---------------------------------------------------------------------*/

#ifndef _OS_INTERNAL_SI_H_
#define	_OS_INTERNAL_SI_H_

#ifdef _LANGUAGE_C_PLUS_PLUS
extern "C" {
#endif

#include <PR/os.h>

#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)

/* Serial interface (Si) */

extern u32 		__osSiGetStatus(void);
extern s32		__osSiRawWriteIo(u32 devAddr, u32  data);
extern s32		__osSiRawReadIo( u32 devAddr, u32 *data);
extern s32		__osSiRawStartDma(s32 direction, void *dramAddr);


#endif /* _LANGUAGE_C */

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif

#endif /* !_OS_INTERNAL_SI_H */
