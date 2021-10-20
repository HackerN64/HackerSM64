/* 
 *  N64-HVQM2 library  Sample program
 * 
 *  FILE : cfbkeep.c (frame buffer management)
 * 
 *  Copyright (C) 1998,1999 NINTENDO Co.,Ltd.
 * 
 */

/* 1998-12-16 */

#include <ultra64.h>
#include <HVQM2File.h>
#include "hvqm.h"
#include "buffers/framebuffers.h"

/***********************************************************************
 * Array maintaining the state of the frame buffer
 * (cfb_status[N] corresponds to the frame buffer itself cfb[N]
 ***********************************************************************/
u32 cfb_status[NUM_CFBs];

/***********************************************************************
 *
 * void init_cfb(void)
 *
 * Explanation
 *    Clears all frame buffers and initializes state of frame buffer
 *
 ***********************************************************************/
void init_cfb(void) {
  int i, j;

  for ( i = 0; i < NUM_CFBs; i++ ) {
    for ( j = 0; j < SCREEN_WD*SCREEN_HT; j++ ) gFramebuffers[i][j] = 0;
    cfb_status[i] = CFB_FREE;
  }
}

/***********************************************************************
 *
 * void keep_cfb(int cfbno)
 *
 * Explanation
 *     Frame buffer indicated by the index cfbno is protected by
 *  being made unavailable.
 *
 ***********************************************************************/
void
keep_cfb( int cfbno )
{
  cfb_status[cfbno] |= CFB_PRECIOUS;
}

/***********************************************************************
 *
 * void release_cfb(int cfbno)
 *
 * Explanation
 *     Frame buffer indicated by the index cfbno is released
 * from protection.
 *
 ***********************************************************************/
void
release_cfb( int bufno )
{
  if ( bufno >= 0 ) cfb_status[bufno] &= ~CFB_PRECIOUS;
}

/***********************************************************************
 *
 * void release_all_cfb(void)
 *
 * Explanation
 *    All frame buffers are released from protection.
 *
 ***********************************************************************/
void
release_all_cfb(void)
{
  int i;
  for ( i = 0; i < NUM_CFBs; i++ ) cfb_status[i] &= ~CFB_PRECIOUS;
}

/***********************************************************************
 *
 * int get_cfb()
 *
 * Explanation
 *     Search for available frame buffer and return that index.
 *     Thread yields until an available frame buffer is found.
 *
 * Returned value
 *     The index of the available frame buffer.
 *
 ***********************************************************************/
int
get_cfb()
{
  int cfbno;

  for ( ; ; ) {
    for ( cfbno = 0; cfbno < NUM_CFBs; cfbno++ )
      if ( cfb_status[cfbno] == 0 )
	return cfbno;
    osYieldThread();
  }
}

/* end */
