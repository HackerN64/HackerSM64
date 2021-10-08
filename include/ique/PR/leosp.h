
/**************************************************************************
 *
 *  $Revision: 1.1.1.2 $
 *  $Date: 2002/10/29 08:06:34 $
 *  $Source: /home/routefree/bb/depot/rf/sw/bbplayer/include/leosp.h,v $
 *
 **************************************************************************/

#ifndef _LEOSP_H_
#define _LEOSP_H_

#ifdef _LANGUAGE_C_PLUS_PLUS
extern "C" {
#endif

#ifndef _LONGCMD
#define _LONGCMD
#endif

#include <PR/leo.h>

#define LEO_COMMAND_READ_LONG         0x10
#define LEO_COMMAND_WRITE_LONG        0x11
#define LEO_COMMAND_READ_SECTOR       0x12

#define LEO_CONTROL_ND                0x40 /* no defect mode */
#define LEO_CONTROL_LONG              0x01 /* do long read/write */


#define LEO_ERROR_RTC_NOT_SET_CORRECTLY		   48
#define LEO_ERROR_ILLEGAL_SECTOR_SIZE              91

#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)

extern  s32 LeoReadWriteLong(LEOCmd *cmdBlock, s32 direction,
                             s32 LBA, void *vAddr, u32 nLBAs,
                             u32 secSize, OSMesgQueue *mq);
extern  s32 LeoSectorRead(LEOCmd *cmdBlock, s32 LBA, void *vAddr,
                          u32 nLBAs, OSMesgQueue *mq);
extern  s32 LeoSysReadWrite(LEOCmd *cmdBlock, s32 direction,
                            s32 LBA, void *vAddr, u32 nLBAs, OSMesgQueue *mq);
 
#endif  /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif

#endif /* !_LEOSP_H */
