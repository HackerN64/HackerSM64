/* 
 *  hvqm2dec.h - N64-HVQM2 library header file
 * 
 *  Copyright (C) 1998,1999 NINTENDO Co.,Ltd.
 */

/*  revision 1.2  1999-03-26  */

#ifndef __HVQM2DEC_H__
#define __HVQM2DEC_H__

#include <ultra64.h>
#include <HVQM2File.h>

typedef struct {
  u8 nbase;
  u8 dc;
  u8 dc_l;
  u8 dc_r;
  u8 dc_u;
  u8 dc_d;
} HVQM2Block;

typedef struct {
  u8 sx;
  u8 sy;
  s16 scale;
  u16 offset;
  u16 lineskip;
} HVQM2Basis;

typedef union {
  HVQM2Block blockinfo;
  HVQM2Basis basisinfo;
  long long int	force_structure_alignment;
} HVQM2Info;

typedef struct {
  u64 *info;
  u64 *buf;
  u16 buf_width;
  u8  chroma_step_h;
  u8  chroma_step_v;
  u16 hmcus;
  u16 vmcus;
  u8 alpha;
  u8 nest[HVQM2_NESTSIZE];
} HVQM2Arg_t;

typedef union {
  HVQM2Arg_t t;
  long long int	force_structure_alignment;
} HVQM2Arg;

/*
 *  Functions supporting 16-bit color (CPU version)
 */
void hvqm2Init1(u8 alpha);
u32  hvqm2Setup1(HVQM2Header *header, u32 outbufWidth);
void hvqm2Decode1(void *code, u32 format, u16 *outbuf, u16 *previm, u16 *workbuf);

/*
 *  Functions supporting 32-bit color (CPU version)
 */
void hvqm2Init2(u8 alpha);
u32  hvqm2Setup2(HVQM2Header *header, u32 outbufWidth);
void hvqm2Decode2(void *code, u32 format, u32 *outbuf, u32 *previm, u16 *workbuf);

/*
 *  Functions supporting 16-bit color (RSP version)
 */
void hvqm2InitSP1(u8 alpha);
u32  hvqm2SetupSP1(HVQM2Header *header, u32 outbufWidth);
u32  hvqm2DecodeSP1(void *code, u32 format, u16 *outbuf, u16 *previm, u16 *workbuf, HVQM2Arg *arg, HVQM2Info *infobuf);

/*
 *  Functions supporting 32-bit color (RSP version)
 */
void hvqm2InitSP2(u8 alpha);
u32  hvqm2SetupSP2(HVQM2Header *header, u32 outbufWidth);
u32  hvqm2DecodeSP2(void *code, u32 format, u32 *outbuf, u32 *previm, u16 *workbuf, HVQM2Arg *arg, HVQM2Info *infobuf);

/*
 *  RSP microcode-related definitions
 */
#define  M_HVQM2TASK  7			/* task type  */
#define  HVQM2_UCODE_DATA_SIZE  0x70	/* microcode data size */
#define  HVQM2_YIELD_DATA_SIZE  0xc10	/* yield buffer size */

/*
 *  Definitions for RSP microcode supporting 16-bit color
 */
extern  u8  hvqm2sp1TextStart[], hvqm2sp1TextEnd[];
extern  u8  hvqm2sp1DataStart[], hvqm2sp1DataEnd[];

/*
 *  Definitions for RSP microcode supporting 32-bit color
 */
extern  u8  hvqm2sp2TextStart[], hvqm2sp2TextEnd[];
extern  u8  hvqm2sp2DataStart[], hvqm2sp2DataEnd[];

#endif /* __HVQM2DEC_H__ */

/* end */
