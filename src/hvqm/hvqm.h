#ifndef HVQM_H
#define HVQM_H


/*
 * Size of the data area for the HVQ microcode
 */
#define HVQ_SPFIFO_SIZE   30000

/*
 * Size of buffer for video records 
 */
#define HVQ_DATASIZE_MAX  40000

/*
 * Size of buffer for audio records
 */
#define AUDIO_RECORD_SIZE_MAX  5000

#define MAXWIDTH  320
#define MAXHEIGHT 240

/*
 * Size of data area for HVQM2 microcode
 */
#define HVQ_SPFIFO_SIZE   30000

#define	SCREEN_WD	320	/* Screen width [pixel] */
#define	SCREEN_HT	240	/* Screen height [pixel] */

#define	APP_GFX_UCODE_HVQ	6		/* HVQ2 microcode */

#define CFB_FREE     0		/* Available */
#define CFB_PRECIOUS (1<<0)	/* Constrained for decoding of next frame */
#define CFB_SHOWING  (1<<1)	/* Waiting to display or displaying */


#define NUM_CFBs	3
#define STACKSIZE  0x2000
#define  PCM_CHANNELS        2	/* Number of channels */
#define  PCM_CHANNELS_SHIFT  1	/* log2(PCM_CHANNELS) */
#define  PCM_ALIGN           2	/* Alignment of number of samples to send */
#define  PCM_BYTES_PER_SAMPLE  (2 * PCM_CHANNELS) /* Number of bytes in one sample */
#define  PCM_BYTES_PER_SAMPLE_SHIFT  2	/* log2(PCM_BYTES_PER_SAMPLE) */

/*
 * Audio record definitions
 */
#define  AUDIO_SAMPLE_BITS	4
#define  AUDIO_SAMPLES_MAX	(((AUDIO_RECORD_SIZE_MAX-sizeof(HVQM2Audio))*8/AUDIO_SAMPLE_BITS)+1) /* Maximum number of records per sample */

/*
 * PCM buffer specifications
 */
#define  NUM_PCMBUFs	3	/* Number of PCM buffers (2 or more, at least 3 recommended) */
#define  PCMBUF_SPREAD	((PCM_ALIGN-1)+AUDIO_SAMPLES_MAX) /* Minimum required capacity for PCM buffer = Number of samples carried forward from last time + number of samples newly decoded */
#define  PCMBUF_ALIGNED  ((PCMBUF_SPREAD+(PCM_ALIGN-1))&(~(PCM_ALIGN-1))) /* pcmbuf[i] address is aligned */
#define  PCMBUF_SIZE     0xA00

/*
 * Macro for loading multi-byte data from buffer holding data from stream 
 */
#define load32(from) (*(u32*)&(from))
#define load16(from) (*(u16*)&(from))

/*
 * Thread ID and priority
 */
#define IDLE_THREAD_ID         1
#define MAIN_THREAD_ID         3
#define HVQM_THREAD_ID         7
#define TIMEKEEPER_THREAD_ID   8
#define DA_COUNTER_THREAD_ID   9

#define IDLE_PRIORITY         10
#define MAIN_PRIORITY         10
#define HVQM_PRIORITY         11
#define TIMEKEEPER_PRIORITY   12
#define DA_COUNTER_PRIORITY   13

#define PI_COMMAND_QUEUE_SIZE	4

typedef u32 (*tkAudioProc)(void *pcmbuf);
typedef tkAudioProc (*tkRewindProc)(void);

void createTimekeeper();
void tkStart(tkRewindProc rewind, u32 samples_per_sec);
void tkPushVideoframe(void *vaddr, u32 *statP, u64 disptime);
u64 tkGetTime(void);
void tkStop(void);
void createHvqmThread(void);

/*
 * in system.c
 */
void romcpy(void *dest, void *src, u32 len, s32 pri, OSIoMesg *mb, OSMesgQueue *mq);

/*
 * in getrecord.c
 */
u8 *get_record(HVQM2Record *headerbuf, void *bodybuf, u16 type, u8 *stream, OSIoMesg *mb, OSMesgQueue *mq);

/*
 * in cfbkeep.c
 */
extern u32 cfb_status[NUM_CFBs];

void init_cfb(void);
void keep_cfb(int cfbno);
void release_cfb(int cfbno);
void release_all_cfb(void);
int get_cfb();

typedef u32 (*tkAudioProc)(void *pcmbuf);
typedef tkAudioProc (*tkRewindProc)(void);


#endif // HVQM_H