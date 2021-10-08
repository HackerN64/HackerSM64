/* 
 *  HVQM2File.h: HVQM2(YUV 4:2:2/4:1:1) file format header file.
 * 
 *  Copyright (C) 1998,1999 NINTENDO Co., Ltd.
 * 
 */

/*  revision 1.1  1999-02-12  */

#ifndef __HVQM2FILE_H__
#define __HVQM2FILE_H__

#include <HVQmd.h>

/*
 * HVQM2Header : HVQM2 file header
 */
typedef struct {
  CARD8  file_version[16];	/* File version  "HVQM2 1.0" */
  CARD32 file_size;		/* File size [byte] */

  CARD16 width;			/* Number of pixels in horizontal direction of image */
  CARD16 height;		/* Number of pixels in vertical direction of image */
  CARD8  h_sampling_rate;	/* Video UV component's sampling step in horizontal direction */
  CARD8  v_sampling_rate;	/* Video UV component's sampling step in vertical direction */
  CARD8  y_shiftnum;		/* Image base read start y-coordinate LSB */
  CARD8  video_quantize_shift;	/* Video quantized step */

  CARD32 total_frames;		/* Total number of video records  */
  CARD32 usec_per_frame;	/* Video frame interval [usec.] */
  CARD32 max_frame_size;	/* Maximum size of video record [bytes]
				   (Excluding record header) */
  CARD32 max_sp_packets;	/* Maximum number of packets needed for SP FIFO */

  CARD8  audio_format;		/* Audio data format  */
  CARD8  channels;		/* Number of audio channels  */
  CARD8  sample_bits;		/* Number of bits in 1 sample (channel) [bit] */
  CARD8  audio_quantize_step;	/* Audio quantized step */

  CARD32 total_audio_records;	/* Total number of audio records  */
  CARD32 samples_per_sec;	/* Number of audio samples per second */
  CARD32 max_audio_record_size;	/* Maximum size of audio record [byte]
				   (Excluding record header) */
} HVQM2Header;

/*
 *  Audio data format  (HVQM2Header.audio_format)
 */
#define HVQM2_AUDIO_PCM		0 /* PCM format  */
#define HVQM2_AUDIO_IMA_ADPCM	1 /* IMA ADPCM format */

/*
 * HVQM2Record : Record header (Located directly after HVQM2Header)
 */
typedef struct {
  CARD16 type;			/* Record type  */
  CARD16 format;		/* Data format  */
  CARD32 size;			/* Record size (excluding the header) [byte] */
} HVQM2Record;

/*
 * Record type  (HVQM2Record.type)
 */
#define HVQM2_AUDIO		0 /* Audio record */
#define HVQM2_VIDEO		1 /* Video record */

/*
 * Audio record's (HVQM2Record.type == HVQM2_AUDIO)
 * data type  (HVQM2Record.format)
 */
#define HVQM2_AUDIO_KEYFRAME	0 /* Key frame */
#define HVQM2_AUDIO_PREDICT	1 /* Predict frame */

/*
 * Video record's (HVQM2Record.type == HVQM2_VIDEO)
 * data format (HVQM2Record.format)
 */
#define HVQM2_VIDEO_KEYFRAME	0 /* Key frame */
#define HVQM2_VIDEO_PREDICT	1 /* Predict frame */
#define HVQM2_VIDEO_HOLD	2 /* Hold frame */

/*
 * HVQM2Audio : Audio header (Follows record header)
 */
typedef struct {
  CARD32 samples;		/* Number of samples (/channels)  */
} HVQM2Audio;

/*
 * HVQM2Frame :  Video header  (Follows record header)
 */
typedef struct {
  CARD32 basisnum_offset[2];	/* Basis number block (0: brightness, 1: color difference) */
  CARD32 basnumrn_offset[2];	/* Basis number cold run (0: brightness, 1: color difference)   */
  CARD32 scale_offset[3];	/* Basis coefficient (0:Y, 1:U, 2:V) */
  CARD32 fixvl_offset[3];	/* Fixed length code (0:Y, 1:U, 2:V) */
  CARD32 dcval_offset[3];	/* Block DC (0:Y, 1:U, 2:V) */
} HVQM2Frame;

/*
 * HVQM2KeyFrame : Key frame header (Follows the video header)
 */
typedef struct {
  CARD32 dcrun_offset[3];	/* DC value cold run (0:Y, 1:U, 2:V) */
  CARD16 nest_start_x;		/* Base start position (x coordinate) */
  CARD16 nest_start_y;		/* Base start position (y coordinate) */
} HVQM2KeyFrame;

/*
 * HVQM2PredictFrame : Predict frame header (Follows video header)
 */
typedef struct {
  CARD32 movevector_offset;	/* Movement vector */
  CARD32 macroblock_offset;	/* Macro block state flag */
} HVQM2PredictFrame;

/* Nest size  */
#define HVQM2_NESTSIZE_L 70	/* Number of elements on long side */
#define HVQM2_NESTSIZE_S 38	/* Number of elements on shot side */
#define HVQM2_NESTSIZE   (HVQM2_NESTSIZE_L * HVQM2_NESTSIZE_S)

#endif /* __HVQM2FILE_H__ */
