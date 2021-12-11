/* 
 *  N64-HVQM2 library  Sample program
 * 
 *  FILE : system.c (boot program/system utility)
 * 
 *  Copyright (C) 1998,1999 NINTENDO Co.,Ltd.
 * 
 */

/* 1998-12-15 */

#include <ultra64.h>
#include <HVQM2File.h>
#include "hvqm.h"

/***********************************************************************
 *
 * void romcpy(void *dest, void *src, u32 len, s32 pri, OSIoMesg *mb, 
 *             OSMesgQueue *mq)
 *
 * Arguments
 *     dest      DRAM address
 *     src       PI device (ROM) address
 *     len       Transfer length (bytes)
 *     pri       Priority of the transfer request
 *     mb        I/O message block request
 *     mq        Message queue receiving notification of end of DMA
 *
 * Explanation
 *     DMA transfers "len" bytes from ROM address "SRC" to DRAM 
 *  address "dest" and returns after waiting for end of DMA. The
 *  data cache of the transfer destination in DRAM is invalidated 
 *  ahead of time.
 *
 *     The parameters have the same meaning as for osPiStartDma() 
 *
 ***********************************************************************/
void
romcpy(void *dest, void *src, u32 len, s32 pri, OSIoMesg *mb, OSMesgQueue *mq)
{
  osInvalDCache( dest, (s32)len );
  while ( osPiStartDma( mb, pri, OS_READ, (u32)src, dest, len, mq ) == -1 ) {}
  osRecvMesg( mq, (OSMesg *)NULL, OS_MESG_BLOCK );
}

/* end */
