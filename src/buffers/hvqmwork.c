/* 
 *  N64-HVQM2 library  Sample program
 * 
 *  FILE : hvqmwork.c
 * 
 *  Copyright (C) 1998,1999 NINTENDO Co.,Ltd.
 * 
 */

/* 1999-02-12 */

#include <ultra64.h>
#include <hvqm2dec.h>
#include <hvqm/hvqm.h>

/*
 * Work area needed by the HVQM2 library
 *
 * (Note) MAXWIDTH and MAXHEIGHT are set with Makefile
 */
#if 0
/*
 * If the compressed data for decoding is only in 4:1:1 format:
 */
	u16 hvqwork[(MAXWIDTH/8)*(MAXHEIGHT/8)*6];
#else
/*
 * If 4:2:2 data will (also) be decoded:
 */
	u16 hvqwork[(MAXWIDTH/8)*(MAXHEIGHT/4)*4];
#endif

/*
 * Data area for the HVQ microcode
 */
HVQM2Info hvq_spfifo[HVQ_SPFIFO_SIZE];

/*
 * Buffer for RSP task yield
 */
u64 hvq_yieldbuf[HVQM2_YIELD_DATA_SIZE/8];

/* end */
