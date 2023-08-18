/*---------------------------------------------------------------------*
        Copyright (C) 2002 - 2004 BroadOn Communications.
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
 * Function prototypes
 *
 */
extern void osBbRtcGet(u8* year, u8* month, u8* day, u8* dow, u8* hour, u8* min,
	               u8* sec);

#endif  /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif

#endif /* !_OS_BB_H_ */
