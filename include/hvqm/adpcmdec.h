/* 
 *  adpcmdec.h - N64-HVQM2 library header file
 * 
 *  Copyright (C) 1998,1999 NINTENDO Co.,Ltd.
 *
 */

/*  revision 1.0  1998-12-17  */

#ifndef   __ADPCMDEC_H__
#define   __ADPCMDEC_H__

#include <ultra64.h>

/* ADPCM state information structure */
typedef struct {
  u8 *inPtr;
  s16 *outPtr;
  s16 previous;
  u8 hi_nibble;
  u8 step_index;
} ADPCMstate;

/* ADPCM decoder */
void adpcmDecode(void *instream, u32 format, u32 samples, s16 *outstream, u32 ex_stereo, ADPCMstate *state);

/* adpcmDecode() format argument */
#define  ADPCM_RESET     0
#define  ADPCM_CONTINUE  1

#endif /* __ADPCMDEC_H__ */

/* end */
