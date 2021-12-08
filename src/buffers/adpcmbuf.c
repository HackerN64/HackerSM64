/* 
 *  N64-HVQM2 library   Sample program
 * 
 *  FILE : adpcmbuf.c
 * 
 *  Copyright (C) 1998,1999 NINTENDO Co.,Ltd.
 * 
 */

/* 1999-02-22 */

#include <ultra64.h>
#include <HVQM2File.h>
#include <macros.h>
#include <hvqm/hvqm.h>

/*
 * Buffer for audio records (ADPCM data) read from the HVQM2 data.
 * (Note) Please locate at a 16byte aligned address with the spec file. 
 */
ALIGNED16 u8 adpcmbuf[AUDIO_RECORD_SIZE_MAX];

/* end */
