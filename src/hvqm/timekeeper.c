#include <ultra64.h>
#include <HVQM2File.h>
#include "hvqm.h"
#include "audio/data.h"
#include "buffers/framebuffers.h"

/***********************************************************************
 * Timekeeper thread
 ***********************************************************************/
static OSThread  tkThread;
static u64  tkStack[STACKSIZE/sizeof(u64)];

/***********************************************************************
 * Audio DA counter thread
 ***********************************************************************/
static OSThread  daCounterThread;
static u64  daCounterStack[STACKSIZE/sizeof(u64)];

/***********************************************************************
 * Timekeeper's command message queue
 ***********************************************************************/
static OSMesgQueue   tkCmdMesgQ;
static OSMesg        tkCmdMesgBuf;

/***********************************************************************
 * Message queue for responses from timekeeper
 ***********************************************************************/
static OSMesgQueue   tkResMesgQ;
static OSMesg        tkResMesgBuf;

/***********************************************************************
 * Timekeeper's command structure
 ***********************************************************************/
typedef struct {
  tkRewindProc rewind;		/* Pointer to stream rewind function */
  u32 samples_per_sec;		/* Audio sampling rate */
} TKCMD;

/***********************************************************************
 * VI event (retrace) message queue
 ***********************************************************************/
#define  VI_MSG_SIZE  2
static OSMesgQueue    viMessageQ;
static OSMesg         viMessages[VI_MSG_SIZE];

/***********************************************************************
 * AI event (FIFO free) message queue
 ***********************************************************************/
#define  AI_MSG_SIZE  2
static OSMesgQueue    aiMessageQ;
static OSMesg         aiMessages[AI_MSG_SIZE];

/***********************************************************************
 * Ring buffer for reserving display of image
 *
 *     As soon as image (frame buffer) is complete, put into this 
 *     ring buffer, wait for display time to come and set in VI.
 ***********************************************************************/
#define  VIDEO_RING_SIZE  NUM_CFBs

static struct {
  u64 disptime;			/* Display time */
  void *vaddr;			/* Frame buffer address */
  u32 *statP;			/* Pointer to frame buffer state flag */
} videoRing[VIDEO_RING_SIZE];

static int  videoRingCount;	/* Amount of data remaining in ring buffer */
static int  videoRingWrite;	/* Ring buffer write index */
static int  videoRingRead;	/* Ring buffer read index */

/***********************************************************************
 * Ring buffer for reserving playback of audio
 * 
 *     As soon as audio (PCM data) is complete, put into this
 *     ring buffer and push as soon as AI FIFO is free.
 ***********************************************************************/
#define  AUDIO_RING_BUFFER_SIZE  NUM_PCMBUFs

static struct {
  void *buf;			/* PCM data buffer */
  u32 len;			/* PCM data length */
} audioRing[AUDIO_RING_BUFFER_SIZE];

static int  audioRingCount;	/* Amount of data remaining in ring buffer */
static int  audioRingWrite;	/* Ring buffer write index */
static int  audioRingRead;	/* Ring buffer read index */

/***********************************************************************
 * PCM data buffer information
 ***********************************************************************/
static int  pcmBufferCount;	/* Number of PCM data buffers being used */
static u32  aiFIFOsamples;	/* Number of PCM samples waiting in AI FIFO */
static u32  aiDAsamples;	/* Number of samples AI is DMA transferring to DA */

/***********************************************************************
 * Buffer for matching PCM transfer data length to PCM_ALIGN 
 ***********************************************************************/
static s16 pcmModBuf[(PCM_ALIGN-1)*PCM_CHANNELS];

/***********************************************************************
 * Flag enabling measurement of passage of time
 ***********************************************************************/
static int  clock_alive;

/***********************************************************************
 * Audio sampling rate
 ***********************************************************************/
static u32  samples_per_sec;

/***********************************************************************
 * Number of PCM samples that have finished playing
 *
 *    Strictly the number samples that have finished playing.  Does not
 *    include samples in FIFO nor the sample playing in the DA buffer.
 *
 *    From this value you get the amount of time that has passed
 *    since the start of play of the HVQM2 stream.
 ***********************************************************************/
static u64  samples_played;

/***********************************************************************
 * The time samples_played was first updated ( osGetTime() )
 *
 *    Time measured beyond this point is supplemented with osGetTime().
 ***********************************************************************/
static OSTime  last_time;

/***********************************************************************
 *
 * u64 tkGetTime(void)
 *
 * Explanation
 *     The time that has passed since the start of HVQM2 movie playback
 *   is calculated back from the number of samples that have finished
 *   playing (samples_played) and the sampling rate (samples_per_sec).
 *     The time after samples_played was first updated is supplemented 
 *   with osGetTime().
 *
 * Returned value
 *     The time that has passed since playback of the HVQM2 movie started
 *
 ***********************************************************************/
u64 tkGetTime(void){
    u64 retval;

    if (!clock_alive) return 0;

    retval = OS_CYCLES_TO_USEC((u64)osGetTime() - last_time);
    if (samples_per_sec) retval += samples_played * 1000000LL / samples_per_sec;
    return retval;
}

/***********************************************************************
 *
 * void tkClockDisable(void)
 *
 * Explanation
 *     Disable measurement of time from start of HVQM2 movie playback
 *
 ***********************************************************************/
static void tkClockDisable(void)
{
  clock_alive = 0;
}

/***********************************************************************
 *
 * void tkClockStart(void)
 *
 * Explanation
 *     Start measurement of time from start of HVQM2 movie playback
 *
 ***********************************************************************/
static void tkClockStart(void)
{
  samples_played = 0;
  last_time = osGetTime();
  clock_alive = 1;
}

/***********************************************************************
 *
 * daCounterProc - Audio DA counter thread procedure
 *
 *    Counts the number of audio samples that have finished playing.
 *
 ***********************************************************************/
static void   daCounterProc(void UNUSED*argument)
{
  for ( ; ; ) {
    osRecvMesg( &aiMessageQ, NULL, OS_MESG_BLOCK );
    last_time = osGetTime();
    if ( aiDAsamples > 0 ) --pcmBufferCount;
    samples_played += aiDAsamples;
    aiDAsamples = aiFIFOsamples;
    aiFIFOsamples = 0;
  }
}

/***********************************************************************
 *
 * timekeeperProc - Timekeeper thread procedure
 *
 *     Plays the HVQM2 movie's audio records and takes charge of
 *   completed frame buffers, displaying them at the scheduled times.
 *
 ***********************************************************************/
static void timekeeperProc(void UNUSED*argument) {
    TKCMD *cmd;
    void *showing_cfb = NULL;	/* Frame buffer being displayed */
    u32 *showing_cfb_statP = NULL; /* Pointer to state flag of the frame buffer being displayed */
    s32 video_started;		/* Flag indicating start of video display */
    s32 audio_started;		/* Flag indicating start of audio playback */
    s32 video_done;		/* Flag indicating end of video decoding */
    s32 audio_done;		/* Flag indicating end of audio decoding */
    void *pushed_cfb;		/* Frame buffer reserved for swap */
    u32 *pushed_cfb_statP;	/* Pointer to state flag of frame buffer reserved for swap */
    s32 next_pcmbufno;		/* The number of the PCM buffer to use next */
    s32 pcm_mod_samples;	/* The number of PCM samples left over */
    tkAudioProc audioproc;
    OSTime present_vtime;
    
    /* Acquire retrace event */
    osCreateMesgQueue( &viMessageQ, viMessages, VI_MSG_SIZE );
    osViSetEvent( &viMessageQ, 0, 1 );

    /* Acquire AI event*/
    osCreateMesgQueue( &aiMessageQ, aiMessages, AI_MSG_SIZE );
    osSetEventMesg( OS_EVENT_AI, &aiMessageQ, (OSMesg *)1 );

  /*
   * Create and start audio DA counter thread
   */
    osCreateThread(&daCounterThread, DA_COUNTER_THREAD_ID, daCounterProc, 
		 NULL, daCounterStack + (STACKSIZE/sizeof(u64)), 
		 (OSPri)DA_COUNTER_PRIORITY);
    osStartThread(&daCounterThread);
    
    osRecvMesg(&tkCmdMesgQ, (OSMesg *)&cmd, OS_MESG_BLOCK);

    /*
     * Wait for movie playback command
     */
    while ( cmd == NULL ) {
      osSendMesg( &tkResMesgQ, NULL, OS_MESG_BLOCK );
      osRecvMesg( &tkCmdMesgQ, (OSMesg *)&cmd, OS_MESG_BLOCK );
    }
    
    tkClockDisable();
    audioproc = (cmd->rewind)();
    
    samples_per_sec = cmd->samples_per_sec;
    if (samples_per_sec) osAiSetFrequency(samples_per_sec);

    pcmBufferCount = 0;

    aiDAsamples = 0;
    aiFIFOsamples = 0;

    audioRingRead = audioRingWrite = 0;
    audioRingCount = 0;

    videoRingRead = videoRingWrite = 0;
    videoRingCount = 0;

    video_started = 0;
    audio_started = 0;
    video_done = 0;
    audio_done = 0;
    pushed_cfb = NULL;
    pushed_cfb_statP = NULL;
    next_pcmbufno = 0;
    pcm_mod_samples = 0;

    osSendMesg( &tkResMesgQ, NULL, OS_MESG_BLOCK ); /* Notification that playback preparations are finished */

    present_vtime = osGetTime();
    
    while (!video_done || videoRingCount > 0 || pushed_cfb != NULL ||
	    !audio_done || audioRingCount > 0 || aiFIFOsamples > 0) {
        u64 present_time;
        OSTime last_vtime;
        /*
        * Block until retrace comes
        */
        osRecvMesg(&viMessageQ, NULL, OS_MESG_BLOCK);

        last_vtime = present_vtime;
        present_vtime = osGetTime();
        present_time = tkGetTime() + OS_CYCLES_TO_USEC(present_vtime - last_vtime);
        
        if (pushed_cfb != NULL) {
            /* The display has switched from showing_cfb to pushed_cfb (it should have) */
            if (!video_started) {
                /* Display of the first frame has begun (the audio will begin from here) */
                video_started = 1;
                if (!audio_started && audio_done) {
                    audio_started = 1;
                    tkClockStart();
                }
            }

            /* Release showing_cfb  */
            if (showing_cfb_statP != NULL && showing_cfb != pushed_cfb) {
                *showing_cfb_statP &= ~CFB_SHOWING;
            }

            /* Update showing_cfb, pushed_cfb */
            showing_cfb = pushed_cfb;
            showing_cfb_statP = pushed_cfb_statP;
            pushed_cfb = NULL;
            pushed_cfb_statP = NULL;
        }
        
	/*
	 * Push the next frame buffer to the VI
	 *
	 *     Before the start of video display (video_started == 0), 
	 *   wait until audio data is prepared for playback
         *   (audioRingCount > 0).
	 */
	if ( video_started || audioRingCount > 0 || audio_done ) {
	  while ( videoRingCount ) {
	    if ( videoRing[videoRingRead].disptime > present_time ) break;
	    if ( pushed_cfb_statP != NULL ) *pushed_cfb_statP &= ~CFB_SHOWING;
	    pushed_cfb = videoRing[videoRingRead].vaddr;
	    pushed_cfb_statP = videoRing[videoRingRead].statP;
	    *pushed_cfb_statP |= CFB_SHOWING;
	    osViSwapBuffer( pushed_cfb );
	    if ( ++videoRingRead == VIDEO_RING_SIZE ) videoRingRead = 0;
	    --videoRingCount;
	  }
	}
        
	/*
	 * Push the next PCM data to the AI
	 *
	 *      Before the start of video display (video_started == 0) 
	 *    wait until the display of the first image frame 
         *    has begun (video_started == 1).
	 */
	if ( video_started ) {
	  osSetThreadPri( NULL, (OSPri)(DA_COUNTER_PRIORITY + 1) );
	  while ( audioRingCount > 0 && aiFIFOsamples == 0 ) {
	    void *buffer = audioRing[audioRingRead].buf;
	    u32   length = audioRing[audioRingRead].len;
	    if ( osAiGetStatus() & AI_STATUS_FIFO_FULL ) break;
	    if ( osAiSetNextBuffer( buffer, length ) == -1 ) break;
	    aiFIFOsamples = length >> PCM_BYTES_PER_SAMPLE_SHIFT;
	    if ( ! audio_started ) {
	      /*
	       * Audio playback has begun.
	       * Here the time count starts to synchronize audio and video
	       */
	      audio_started = 1;
	      tkClockStart();
	    }
	    if ( ++audioRingRead == AUDIO_RING_BUFFER_SIZE ) audioRingRead = 0;
	    --audioRingCount;
	  }
	  osSetThreadPri( NULL, (OSPri)TIMEKEEPER_PRIORITY );
	}
	/*
	 * Prepare the next PCM data
	 */
	if ( ! audio_done && audioRingCount < AUDIO_RING_BUFFER_SIZE && pcmBufferCount < NUM_PCMBUFs ) {
	  u32 samples;
	  u32 length;
	  void *buffer;
	  s16 *sp, *dp;
	  int i;

	  samples = audioproc( &gAiBuffers[next_pcmbufno][pcm_mod_samples<<PCM_CHANNELS_SHIFT] );

	  if ( samples > 0 ) {
	    ++pcmBufferCount;

	    sp = pcmModBuf;
	    dp = (s16 *)((u8 *)gAiBuffers[next_pcmbufno]);
	    i = pcm_mod_samples << PCM_CHANNELS_SHIFT;
	    while ( i-- ) *dp++ = *sp++;
	    samples += pcm_mod_samples;
	    samples -= (pcm_mod_samples = samples & (PCM_ALIGN-1));
	    length = samples << PCM_BYTES_PER_SAMPLE_SHIFT;
	    buffer = gAiBuffers[next_pcmbufno];
	    osWritebackDCache( buffer, length );
	    audioRing[audioRingWrite].buf = buffer;
	    audioRing[audioRingWrite].len = length;
	    if ( ++audioRingWrite == AUDIO_RING_BUFFER_SIZE ) audioRingWrite = 0;
	    ++audioRingCount;

	    sp = (s16 *)((u8 *)(gAiBuffers[next_pcmbufno]) + length);
	    dp = pcmModBuf;
	    i = pcm_mod_samples << PCM_CHANNELS_SHIFT;
	    while ( i-- ) *dp++ = *sp++;

	    if ( ++next_pcmbufno >= NUM_PCMBUFs ) next_pcmbufno = 0;
	  }
	  else audio_done = 1;
	}
        
	if ( osRecvMesg( &tkCmdMesgQ, (OSMesg *)&cmd, OS_MESG_NOBLOCK ) == 0 )
	  {
	    /*
	     * tkStop() or tkStart() has been executed
	     * 
	     *     Set 1 in video_done and end this playback loop
             *   after playback of the current movie's video and 
	     *   and audio has completely finished.
	     */
	    video_done = 1;
	  }
    }
}

/***********************************************************************
 *
 * void createTimekeeper(void)
 *
 * Explanation
 *     Creates and starts the timekeeper thread
 *
 ***********************************************************************/
void
createTimekeeper(void)
{
  osCreateMesgQueue( &tkCmdMesgQ, &tkCmdMesgBuf, 1 );
  osCreateMesgQueue( &tkResMesgQ, &tkResMesgBuf, 1 );
  osCreateThread( &tkThread, TIMEKEEPER_THREAD_ID, timekeeperProc, 
		 NULL, tkStack + (STACKSIZE/sizeof(u64)), 
		 (OSPri)TIMEKEEPER_PRIORITY );
  osStartThread( &tkThread );
}

/***********************************************************************
 *
 * void tkStart(tkRewindProc rewind, u32 samples_per_sec)
 *
 * Argument
 *     rewind    Pointer to movie rewind callback function
 *
 * Explanation
 *    Directs the timekeeper to play a new movie.  Playback of the
 *  movie starts after callback of "rewind" by the timekeeper.  After
 *  this, the HVQM2 video records are read and decoded and the
 *  completed frame buffers are handed over to the timekeeper by 
 *  tkPushVideoframe() in succession to play the video part of the movie.
 *  As for the audio, the timekeeper itself calls the callback 
 *  function (the value returned by rewind) that gets the PCM data as
 *  needed to playback the audio.
 *
 ***********************************************************************/
void
tkStart( tkRewindProc rewind, u32 samples_per_sec )
{
  TKCMD tkcmd;
  tkcmd.samples_per_sec = samples_per_sec;
  tkcmd.rewind = rewind;
  osSendMesg( &tkCmdMesgQ, (OSMesg *)&tkcmd, OS_MESG_BLOCK );
  osRecvMesg( &tkResMesgQ, (OSMesg *)NULL, OS_MESG_BLOCK );
}

/***********************************************************************
 *
 * void tkStop(void)
 *
 * Explanation
 *    Stops the timekeeper.  However, the timekeeper continues to
 *  run until playback of the audio of the playing movie is complete.
 *  To stop the stream in mid-stream, make 0 the value returned by
 *  the callback function that reads audio and is called from the
 *  timekeeper, and then execute tkStop().
 *
 *    To immediately start the playback of a new movie, it is OK to
 *  to execute tkStart() without executing tkStop().
 *
 ***********************************************************************/
void
tkStop( void )
{
  osSendMesg( &tkCmdMesgQ, (OSMesg *)NULL, OS_MESG_BLOCK );
  osRecvMesg( &tkResMesgQ, (OSMesg *)NULL, OS_MESG_BLOCK );
}

/***********************************************************************
 *
 * void tkPushVideoframe(void *vaddr, u32 *statP, u64 disptime)
 * 
 * Argument
 *     vaddr     Frame buffer address
 *     statP     Pointer to state flag data regarding vaddr
 *     disptime  Display time
 *
 * Explanation
 *    Entrusts the completed frame buffer vaddr to the timekeeper.
 *  The timekeeper displays the frame buffer after waiting for the
 *  "disptime" amount of time to pass from the start of the movie.
 *
 *    To wait for the display of the vaddr frame buffer or to 
 *  indicate that something is being displayed, the CFB_SHOWING 
 *  flag is set in the u32 type data pointed to by the statP pointer.
 *  At the time when display of vaddr is finished, (once its display
 *  is over and display has switched to another frame buffer) this
 *  flag is cleared by the timekeeper.
 *
 ***********************************************************************/
void
tkPushVideoframe( void *vaddr, u32 *statP, u64 disptime )
{
  *statP |= CFB_SHOWING;
  while ( videoRingCount >= VIDEO_RING_SIZE ) osYieldThread();
  videoRing[videoRingWrite].disptime = disptime;
  videoRing[videoRingWrite].vaddr = vaddr;
  videoRing[videoRingWrite].statP = statP;
  if ( ++videoRingWrite == VIDEO_RING_SIZE ) videoRingWrite = 0;
  ++videoRingCount;
}
