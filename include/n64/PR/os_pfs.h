
/*====================================================================
 * os_pfs.h
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

        $RCSfile: os_pfs.h,v $
        $Revision: 1.1 $
        $Date: 1998/10/09 08:01:16 $
 *---------------------------------------------------------------------*/

#ifndef _OS_PFS_H_
#define _OS_PFS_H_

#ifdef _LANGUAGE_C_PLUS_PLUS
extern "C" {
#endif /* _LANGUAGE_C_PLUS_PLUS */

#include <PR/ultratypes.h>
#include "os_message.h"


#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)

/**************************************************************************
 *
 * Type definitions
 *
 */

/*
 * Structure for file system
 */
typedef struct {
    /*0x00*/ int status;
    /*0x04*/ OSMesgQueue *queue;
    /*0x08*/ int channel;
    /*0x0C*/ u8  id[32];
    /*0x2C*/ u8  label[32];
    /*0x4C*/ int version;
    /*0x50*/ int dir_size;
    /*0x54*/ int inode_table;       /* block location */
    /*0x58*/ int minode_table;      /* mirrioring inode_table */
    /*0x5C*/ int dir_table;         /* block location */
    /*0x60*/ int inode_start_page;  /* page # */
    /*0x64*/ u8  banks;
    /*0x65*/ u8  activebank;
} OSPfs; /*0x68*/

typedef struct {
    /*0x00*/ u32  file_size; /* bytes */
    /*0x04*/ u32  game_code;
    /*0x08*/ u16  company_code;
    /*0x0A*/ char ext_name[4];
    /*0x0E*/ char game_name[16];
} OSPfsState; /*0x20*/


#endif /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

/**************************************************************************
 *
 * Global definitions
 *
 */

/* File System size */
#define OS_PFS_VERSION      0x0200
#define OS_PFS_VERSION_HI   (OS_PFS_VERSION >> 8)   // 0x0002
#define OS_PFS_VERSION_LO   (OS_PFS_VERSION & 0xFF) // 0x0000

#define PFS_FILE_NAME_LEN   sizeof(((OSPfsState*)0)->game_name) // 16
#define PFS_FILE_EXT_LEN    sizeof(((OSPfsState*)0)->ext_name ) //  4
#define BLOCKSIZE           32 /* bytes  */
#define PFS_ONE_PAGE         8 /* blocks */
#define PFS_MAX_BANKS       62

/* File System flag */
enum OSPfsFlag {
    PFS_READ,
    PFS_WRITE,
    PFS_CREATE,
};

/* File System status */
#define PFS_STATUS_NONE         0x00
#define PFS_INITIALIZED         (1 << 0) // 0x01
#define PFS_CORRUPTED           (1 << 1) // 0x02 /* File system was corrupted */
#define PFS_ID_BROKEN           (1 << 2) // 0x04
#define PFS_MOTOR_INITIALIZED   (1 << 3) // 0x08
#define PFS_GBPAK_INITIALIZED   (1 << 4) // 0x10

/* File System error number */
enum OSPfsError {
    PFS_ERR_SUCCESS,        /* no error                                     */
    PFS_ERR_NOPACK,         /* no memory card is plugged or                 */
    PFS_ERR_NEW_PACK,       /* ram pack has been changed to a different one */
    PFS_ERR_INCONSISTENT,   /* need to run Pfschecker                       */
    PFS_ERR_CONTRFAIL,      /* CONT_OVERRUN_ERROR                           */
    PFS_ERR_INVALID,        /* invalid parameter or file not exist          */
    PFS_ERR_BAD_DATA,       /* the data read from pack are bad              */
    PFS_DATA_FULL,          /* no free pages on ram pack                    */
    PFS_DIR_FULL,           /* no free directories on ram pack              */
    PFS_ERR_EXIST,          /* file exists                                  */
    PFS_ERR_ID_FATAL,       /* dead ram pack                                */
    PFS_ERR_DEVICE,         /* wrong device type                            */
    PFS_ERR_NO_GBCART,      /* no gb cartridge (64GB-PAK)                   */
    PFS_ERR_NEW_GBCART,     /* gb cartridge may be changed                  */
};


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

/* file system interface */

extern s32 osPfsInitPak(OSMesgQueue *mq, OSPfs *pfs, int controller_no);
extern s32 osPfsRepairId(     OSPfs *pfs);
extern s32 osPfsInit(   OSMesgQueue *mq, OSPfs *pfs, int controller_no);
extern s32 osPfsReFormat(     OSPfs *pfs, OSMesgQueue *mq, int);
extern s32 osPfsChecker(      OSPfs *pfs);
extern s32 osPfsAllocateFile( OSPfs *pfs, u16 company_code, u32 game_code, u8 *game_name, u8 *ext_name, int length, s32 *file_no);
extern s32 osPfsFindFile(     OSPfs *pfs, u16 company_code, u32 game_code, u8 *game_name, u8 *ext_name,             s32 *file_no);
extern s32 osPfsDeleteFile(   OSPfs *pfs, u16 company_code, u32 game_code, u8 *game_name, u8 *ext_name);
extern s32 osPfsReadWriteFile(OSPfs *pfs, s32 file_no, u8 flag, int offset, int nbytes, u8 *data_buffer);
extern s32 osPfsFileState(    OSPfs *pfs, s32 file_no, OSPfsState *state);
extern s32 osPfsGetLabel(     OSPfs *pfs, u8 *label, int *length);
extern s32 osPfsSetLabel(     OSPfs *pfs, u8 *label);
extern s32 osPfsIsPlug( OSMesgQueue *mq, u8 *bitpattern);
extern s32 osPfsFreeBlocks(   OSPfs *pfs, s32 *bytes_not_used);
extern s32 osPfsNumFiles(     OSPfs *pfs, s32 *max_files, s32 *files_used);


#endif /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif /* _LANGUAGE_C_PLUS_PLUS */

#endif /* !_OS_PFS_H_ */
