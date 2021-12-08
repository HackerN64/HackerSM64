
/*---------------------------------------------------------------------*
        Copyright (C) 1998 Nintendo.
        
        $RCSfile: os_voice.h,v $
        $Revision: 1.2 $
        $Date: 1999/07/13 08:36:42 $
 *---------------------------------------------------------------------*/

#ifndef _OS_VOICE_H_
#define	_OS_VOICE_H_

#ifdef _LANGUAGE_C_PLUS_PLUS
extern "C" {
#endif

#include <PR/ultratypes.h>

#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)

/**************************************************************************
 *
 * Type definitions
 *
 */

typedef struct {		/* Voice Recognition System */
  OSMesgQueue	*__mq;		/* SI Message Queue */
  int		__channel;	/* Controller Port # */
  s32		__mode;
  u8		cmd_status;	/* Command Status */
} OSVoiceHandle;

typedef struct {		/* Voice Recognition System */
  u16	warning;
  u16	answer_num;		/* 0...5 */
  u16	voice_level;
  u16	voice_sn;
  u16	voice_time;
  u16	answer[5];
  u16	distance[5];
} OSVoiceData;
	

#endif /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

/**************************************************************************
 *
 * Global definitions
 *
 */

/* definition for Voice Recognition System */

#define	VOICE_WARN_TOO_SMALL	0x0400
#define	VOICE_WARN_TOO_LARGE	0x0800
#define	VOICE_WARN_NOT_FIT	0x4000
#define	VOICE_WARN_TOO_NOISY	0x8000

#define	VOICE_STATUS_READY	0
#define	VOICE_STATUS_START	1
#define	VOICE_STATUS_CANCEL	3
#define	VOICE_STATUS_BUSY	5
#define	VOICE_STATUS_END	7


#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)

/**************************************************************************
 *
 * Macro definitions
 *
 */


/**************************************************************************
 *
 * Extern variables
 *
 */


/**************************************************************************
 *
 * Function prototypes
 *
 */

/* Voice Recognition System */
extern s32 osVoiceInit(OSMesgQueue *siMessageQ, OSVoiceHandle *hd, int channel);
extern s32 osVoiceCheckWord(u8 *data);
extern s32 osVoiceClearDictionary(OSVoiceHandle *hd, u8 words);
extern s32 osVoiceControlGain(    OSVoiceHandle *hd, s32 analog, s32 digital);
extern s32 osVoiceSetWord(        OSVoiceHandle *hd, u8 *word);
extern s32 osVoiceStartReadData(  OSVoiceHandle *hd);
extern s32 osVoiceStopReadData(   OSVoiceHandle *hd);
extern s32 osVoiceGetReadData(    OSVoiceHandle *hd, OSVoiceData *result);
extern s32 osVoiceMaskDictionary( OSVoiceHandle *hd, u8 *maskpattern, int size);
extern void osVoiceCountSyllables(u8 *word, u32 *syllable);


#endif  /* defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS) */

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif

#endif /* !_OS_VOICE_H_ */
