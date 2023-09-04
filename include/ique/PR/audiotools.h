/*====================================================================
 * audiotools.h
 *
 * Copyright 1993, Silicon Graphics, Inc.
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

/**************************************************************************
 *
 *  $Revision: 1.1.1.2 $
 *  $Date: 2002/10/29 08:06:34 $
 *  $Source: /home/routefree/bb/depot/rf/sw/bbplayer/include/audiotools.h,v $
 *
 **************************************************************************/

/*-------------------------------------------------------------------
 *
 *  This file contains defines used by the audiotools programs.
 *
 *-------------------------------------------------------------------*/

#ifndef __AUDIOTOOLS__
#define __AUDIOTOOLS__

#define AL_SYM_FILE_VERSION     'SF01'

typedef struct {
    s32 objectId;
    s32 objectClass;
    s32 bankFileOffset;
    s32 stringOffset;
    s32 refCount;
} ALSymFileObj;

typedef struct {
    s32                 revision;
    s32                 fileSize;
    s32                 objectCount;            
} ALSymFile;


#define AL_NO_CLASS        0
#define AL_BANK_CLASS      1
#define AL_INST_CLASS      2
#define AL_SOUND_CLASS     3
#define AL_KEYMAP_CLASS    4
#define AL_WAVETBL_CLASS   5
#define AL_LOOP_CLASS      6
#define AL_BNKFILE_CLASS   7
#define AL_ENVLP_CLASS     8
#define AL_BOOK_CLASS      9
#define AL_SNDFILE_CLASS   10

#define AL_NUM_CLASSES     11

#endif /* !__AUDIOTOOLS__ */
