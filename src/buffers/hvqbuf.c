/* 
 *  N64-HVQM2 library  Sample program
 * 
 *  FILE : hvqbuf.c
 * 
 *  Copyright (C) 1998,1999 NINTENDO Co.,Ltd.
 * 
 */

/* 1999-02-22 */

#include <ultra64.h>
#include <macros.h>
#include <HVQM2File.h>
#include <hvqm/hvqm.h>

/*
 *  Buffer for video records (HVQM2 compressed data) read from 
 * the HVQM2 data.
 * (Note) Please locate at a 16byte aligned address with the spec file.
 */
ALIGNED16 u8 hvqbuf[HVQ_DATASIZE_MAX];

/* end */
