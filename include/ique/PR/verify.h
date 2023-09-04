#ifndef _VERIFY_H_
#define	_VERIFY_H_

/**************************************************************************
 *									  *
 *		 Copyright (C) 1994, Silicon Graphics, Inc.		  *
 *									  *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *									  *
 **************************************************************************/

/**************************************************************************
 *
 *  $Revision: 1.1.1.2 $
 *  $Date: 2002/10/29 08:06:34 $
 *  $Source: /home/routefree/bb/depot/rf/sw/bbplayer/include/verify.h,v $
 *
 **************************************************************************/

#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)

/*
 * This structure must be aligned to 64-bit boundary for the
 * RSP to get at it. Likewise, the important fields that the
 * RSP writes back must also be aligned...
 */
typedef struct {
	unsigned int	rspListAddr;		/* 0x00 */	/* ALIGNED */
	unsigned int	rspListSize;		/* 0x04 */

	unsigned int	rspListType;		/* 0x08 */
	unsigned int	ucodeTextAddr;		/* 0x0c */

	unsigned int	ucodeDataAddr;		/* 0x10 */
	unsigned int	ucodeDataSize;		/* 0x14 */

	unsigned int	dramStackAddr;		/* 0x18 */
	unsigned int	dramStackSize;		/* 0x1c */

	/* this length must be a 64-bit thing so the RSP can DMA it */
	unsigned long long int	rdpListSize;	/* 0x20 */	/* ALIGNED */

	unsigned int	rdpListAddr;		/* 0x28 */	/* ALIGNED */
	unsigned int	pad1;

	unsigned int	bootUcodeAddr;		/* 0x30 */

	unsigned short	width;			/* 0x34 */
	unsigned short	height;			/* 0x36 */

	unsigned int	framebuffer0Addr;	/* 0x38 */
	unsigned int	framebuffer1Addr;	/* 0x3c */
	
	unsigned int	zBufferAddr;		/* 0x40 */

	unsigned long	magicNumber;		/* 0x44 */
	unsigned long	version;		/* 0x48 */

	unsigned int	framebufferFormat;	/* 0x4c */
	unsigned int	framebufferSize;	/* 0x50 */
	
        unsigned int    staticSegAddr;          /* 0x54 */
        unsigned int    staticSegSize;          /* 0x58 */

} VerifyInfo;


#define MAGICNUMBER 	0x102594		/* (Kaelyn's Birthday) */
#define VERSION		1

#define CVGMAGIC 0x00120594

struct CVGHeader {
  unsigned long  magic;
  unsigned char  format;
  unsigned char  size;
  unsigned short width;
  unsigned short height;        
};

#endif /* _LANGUAGE_C */

#define	VERIFY_INFO_PHYSADDR	0x00000800

#define	RSPLIST_ADDR_OFFSET		0x00
#define	RSPLIST_SIZE_OFFSET		0x04
#define	RSPLIST_TYPE_OFFSET		0x08
#define	UCODETEXT_ADDR_OFFSET		0x0c
#define	UCODEDATA_ADDR_OFFSET		0x10
#define	UCODEDATA_SIZE_OFFSET		0x14
#define	DRAMSTACK_ADDR_OFFSET		0x18
#define	DRAMSTACK_SIZE_OFFSET		0x1c
#define	RDPLISTSIZE_OFFSET		0x20
#define	RDPLIST_ADDR_OFFSET		0x28
#define	BOOTUCODE_ADDR_OFFST		0x30
#define	WIDTH_OFFSET			0x34
#define	HEIGHT_OFFSET			0x36
#define	FRAMEBUFFER0_ADDR_OFFSET	0x38
#define	FRAMEBUFFER1_ADDR_OFFSET	0x3c
#define	ZBUFFER_ADDR_OFFSET		0x40


/*
 * Address map for family of verification programs
 */
#define	JEAN_START_ADDR			0x000000
#define	JEAN_END_ADDR			0x010000
#define	APP_START_ADDR			JEAN_END_ADDR
#define	APP_END_ADDR			0x0b0000
#define	FB_START_ADDR			APP_END_ADDR
#define	ZB_START_ADDR			0x100000
#define	ZB_END_ADDR			0x125800
#define	EVAN_START_ADDR			0x130000
#define	EVAN_END_ADDR			0x140000

#endif /* !_VERIFY_H_ */
