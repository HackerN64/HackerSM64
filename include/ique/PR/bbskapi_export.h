/*---------------------------------------------------------------------*
        Copyright (C) 2002 - 2004 BroadOn Communications.
 *---------------------------------------------------------------------*/

#ifndef __BB_SK_API_HDR__
#define __BB_SK_API_HDR__

#if defined(_LANGUAGE_C)
    
typedef u32  BbId;                 /* 32-bit Player identity */
typedef u32  BbContentId;          /* 32-bit content identity */

/* get the bbid */
int skGetId(BbId *id);
extern int skExit(void);

#endif /* _LANGUAGE_C */

#endif
