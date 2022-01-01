/*======================================================================*/
/*		NuSYS							*/
/*		nupisraminit.c						*/
/*									*/
/*		Copyright (C) 1997, NINTENDO Co,Ltd.			*/
/*									*/
/*----------------------------------------------------------------------*/
/* Ver 1.2	98/07/4		Created by Kensaku Ohki(SLANP)		*/
/*----------------------------------------------------------------------*/
/* $Id: nupiinitsram.c,v 1.2 1998/07/11 11:22:46 ohki Exp $	*/
/*======================================================================*/
#include <ultra64.h>

#define SRAM_START_ADDR         0x08000000
#define SRAM_SIZE               0x8000
#define SRAM_LATENCY            0x5
#define SRAM_PULSE              0x0c
#define SRAM_PAGE_SIZE          0xd
#define SRAM_REL_DURATION       0x2

OSPiHandle*		nuPiSramHandle;
static OSPiHandle	SramHandle;

/*----------------------------------------------------------------------*/
/*	nuPiSramInit  - Initialization of handle for SRAM		*/
/*	Initialize handle for SRAM					*/
/*	IN:	None							*/
/*	RET:	If detected, return 1, otherwise 0							*/
/*----------------------------------------------------------------------*/
int nuPiInitSram()
{
    if (SramHandle.baseAddress == PHYS_TO_K1(SRAM_START_ADDR))
	return 0;

    /* Fill basic information */
    SramHandle.type = DEVICE_TYPE_SRAM;
    SramHandle.baseAddress = PHYS_TO_K1(SRAM_START_ADDR);

    /* Get Domain parameters */
    SramHandle.latency = (u8)SRAM_LATENCY;
    SramHandle.pulse = (u8)SRAM_PULSE;
    SramHandle.pageSize = (u8)SRAM_PAGE_SIZE;
    SramHandle.relDuration = (u8)SRAM_REL_DURATION;
    SramHandle.domain = PI_DOMAIN2;

    /* Fill speed and transferInfo to zero */
    SramHandle.speed = 0;
    bzero((void *)&(SramHandle.transferInfo),
	  sizeof(SramHandle.transferInfo));

    /* Put the SramHandle onto PiTable*/
    osEPiLinkHandle(&SramHandle);
    nuPiSramHandle = &SramHandle;
	return 1;
}
