
/*
 * rsp.h
 *
 * Mon Mar 21 21:15:39 PST 1994
 *
 * This file is some common definitions for RSP code.
 * It enforces the programming model for the task lists.
 *
 */

/**************************************************************************
 *
 *  $Revision: 1.1.1.2 $
 *  $Date: 2002/10/29 08:06:34 $
 *  $Source: /home/routefree/bb/depot/rf/sw/bbplayer/include/rsp.h,v $
 *
 **************************************************************************/

#ifndef _RSP_H_
#define _RSP_H_ 1

#define RSP_DMEM_BASE	0x04000000
#define RSP_IMEM_BASE	0x04001000

#define RSP_SCRATCH_BASE	0x20000000	/* for sim use only */

#define DCACHEBASEHI	0x0400
#define DCACHEBASELO	0x0000	

#define RSPBOOTBASEHI	0x0400
#define RSPBOOTBASELO	0x1000

#define TASKBASEHI	0x0400
#define TASKBASELO	0x1080	

#define HOSTSEMADDRHI	0x0000	# semaphore address
#define HOSTSEMADDRLO	0x1ffc	# semaphore address

/* for now, we'll just copy all the output somewhere out-of-the-way... */
#define RDP_BUFF_HI	0x2000
#define RDP_BUFF_LO	0x0000

/* for now, we'll just fix some DRAM stack (for matrices, etc.) */
#define DRAM_STACK_HI	0x001f
#define DRAM_STACK_LO	0xfc00	/* a 1K stack... */

/* 
 * this is a hack; we need a place in DRAM for the RSP to 
 * use as a stack.
 */
#define rsp_DRAM_STACK_HI	0x0020
#define rsp_DRAM_STACK_LO	0x0000

        /* where is the trig ROM? */
#define rsp_TRIG_BASE   0x10010000
#define rsp_TRIG_BASEHI 0x1001
#define rsp_TRIG_BASELO 0x0000

#ifdef _LANGUAGE_ASSEMBLY

    .symbol RSPBOOTBASE,	RSP_IMEM_BASE	# start of rsp bootstrap
    .symbol TASKBASE,	(RSP_IMEM_BASE+0x80)  	# 32 inst. for bootstrap

    .name	DMA_CACHE,		$c0	# these are CP0 registers
    .name	DMA_DRAM,		$c1	#
    .name	DMA_READ_LENGTH,	$c2
    .name	DMA_WRITE_LENGTH,	$c3
    .name	SP_STATUS,		$c4
    .name	DMA_FULL,		$c5
    .name	DMA_BUSY,		$c6
    .name	SP_RESERVED,		$c7
    .name	CMD_START,		$c8
    .name	CMD_END,		$c9
    .name	CMD_CURRENT,		$c10
    .name	CMD_STATUS,		$c11
    .name	CMD_CLOCK,		$c12
    .name	CMD_BUSY,		$c13
    .name	CMD_PIPE_BUSY,		$c14
    .name	CMD_TMEM_BUSY,		$c15

#endif


#endif
