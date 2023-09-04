#ifndef _EM_H_
#define	_EM_H_

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
 *  $Source: /home/routefree/bb/depot/rf/sw/bbplayer/include/em.h,v $
 *
 **************************************************************************/

#include <PR/mbi.h>

#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)

typedef struct {
	int		x;
	int		y;
	unsigned int	buttons;
} MouseState;

#define	BUTTON_LEFT	0x4
#define	BUTTON_MIDDLE	0x2
#define	BUTTON_RIGHT	0x1

typedef struct {
	unsigned short button;		/* all of the 14 buttons */
	unsigned char stick_x;
	unsigned char stick_y;
} GamePad;

/* Simulate interrupts with signals */
#define SIGSW1	    (SIGRTMIN + OS_EVENT_SW1)
#define SIGSW2	    (SIGRTMIN + OS_EVENT_SW2)
#define SIGCART	    (SIGRTMIN + OS_EVENT_CART)
#define SIGCOUNTER  (SIGRTMIN + OS_EVENT_COUNTER)	
#define SIGSP	    (SIGRTMIN + OS_EVENT_SP)
#define SIGSI	    (SIGRTMIN + OS_EVENT_SI)
#define SIGAI	    (SIGRTMIN + OS_EVENT_AI)
#define SIGVI	    (SIGRTMIN + OS_EVENT_VI)
#define SIGPI	    (SIGRTMIN + OS_EVENT_PI)
#define SIGDI	    (SIGRTMIN + OS_EVENT_DP)
#define SIGBREAK    (SIGRTMIN + OS_EVENT_CPU_BREAK)
#define SIGSPBREAK  (SIGRTMIN + OS_EVENT_SP_BREAK)


/*
 * Function Prototypes
 */

extern void	emDisplayBuffer(unsigned int);
extern void	emGetMouseState(MouseState *);
extern int	emInitController(int);
extern int	emGetController(int, GamePad *);
extern int	emMemoryDump(const char *);
extern int	emMemoryLoad(const char *);
extern int	emPrintf(const char *, ...);
extern void emGIOInit( void );
extern void emInitSocket( void );
extern void emWriteSocket( char );
extern void emRomToHost( int, int );

/* connect to RSP: */
extern int	emRSPFrame(char *prefix, int fid, int keepMem, int DRAMoutput, int RDPin);
extern int	emRSPAudioFrame(char *prefix, int fid, int keepMem);
extern int	emRSPQuit(void);

/* audio support */

extern void     emAudioInit(char *filename);
extern void     emAudioWrite(short *out, int count);
extern void     emAudioFlush(void);
extern void	emPause(void);

/* DRAM definites */

#define	KSEG0_BASE	0x20000000
#define	DRAM_SIZE	0x200000

/* locations for shared ROM in emulator */

#define	ROM_BASE	(KSEG0_BASE + DRAM_SIZE + 0x2000)
#define GIO_CART_INT_REGISTER	(ROM_BASE + RAMROM_SIZE - 4)
#define SHM_CART_INT_REGISTER	(RAMROM_MSG_SIZE - 4)
#define SHM_MSG_BUFFER_ADDRESS	(RAMROM_MSG_SIZE - 8)


#endif /* _LANGUAGE_C */

#endif /* !_EM_H_ */
