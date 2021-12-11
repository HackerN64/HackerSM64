/* 
 *  N64-HVQM2 library Sample program
 * 
 *  FILE : getrecord.c (module for reading HVQM2 records)
 * 
 *  Copyright (C) 1998,1999 NINTENDO Co.,Ltd.
 * 
 */

/* 1999-02-22 */

#include <ultra64.h>
#include <HVQM2File.h>
#include "hvqm.h"

/*
 * u8 *get_record(HVQM2Record *headerbuf, void *bodybuf, 
 *                u16 type, u8 *stream, OSIoMesg *mb, OSMesgQueue *mq)
 *
 * Arguments
 *     headerbuf Buffer storing the record header (HVQM2Record structure)
 *     bodybuf   Buffer storing the record's data (record body)
 *     type      The requested record type
 *     stream    The current HVQM2 record address
 *     mb        I/O message block request sent to PI manager
 *     mq        Message queue receiving notice of DMA end from PI manager
 *  
 * Explanation:
 *   Finds the next record of the specified "type" from the HVQM2 data
 * address "stream" in ROM, and reads the record header into "headerbuf"
 * and the data body into "bodybuf."  Specify HVQM2_AUDIO for "type" to
 * read an audio record, and specify HVQM2_VIDEO to read a video record.
 *
 *   This function assumes that the reading of audio records and video 
 * records is performed in parallel by separate threads.  Accordingly, 
 * you must prepare in advance a separate mb (I/O message block request
 * sent to PI manager) and separate mq (message queue receiving notice
 * of DMA end from PI manager) for audio records and video records.
 *
 *   The reading of audio records has priority over the reading of 
 * video records.
 *
 *   "stream" must have 2byte alignment.  Please give headerbuf and
 * bodybuf 16byte alignment to conform with the R4300 data cache line
 * size.
 *
 *   Please be sure to reserve sufficient space for bodybuf.
 * 
 *
 * Returned value:
 *     The address of the next record
 */
u8 *
get_record( HVQM2Record *headerbuf, void *bodybuf, u16 type, u8 *stream, OSIoMesg *mb, OSMesgQueue *mq )
{
  u16 record_type;
  u32 record_size;
  s32 pri;

  pri = (type == HVQM2_AUDIO) ? OS_MESG_PRI_HIGH : OS_MESG_PRI_NORMAL;
  for ( ; ; ) {
    romcpy( headerbuf, stream, sizeof(HVQM2Record), pri, mb, mq );
    stream += sizeof(HVQM2Record);
    record_type = load16( headerbuf->type );
    record_size = load32( headerbuf->size );
    if ( record_type == type ) break;
    stream += record_size;
  } 

  if ( record_size > 0 ) {
    romcpy( bodybuf, stream, record_size, pri, mb, mq );
    stream += record_size;
  }
  return stream;
}

/* end */
