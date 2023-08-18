/*---------------------------------------------------------------------*
        Copyright (C) 1997-1998, Nintendo.
        
        File            gzsort.h
        Coded    by     Yoshitaka Yasumoto.     Jun 16, 1997.
        
        $Id: gzsort.h,v 1.1.1.1 2002/05/02 03:28:24 blythe Exp $
 *---------------------------------------------------------------------*/
#ifndef _GZSORT_H_
#define _GZSORT_H_

#ifdef _LANGUAGE_C_PLUS_PLUS
extern "C" {
#endif

#include <PR/ultratypes.h>

/*===========================================================================*
 *      GBI Commands for ZSort microcode
 *===========================================================================*/
/* GBI Header */
#define	G_ZS_ZOBJ		0x80
#define	G_ZS_RDPCMD		0x81
#define	G_ZS_MOVEMEM		0xdc
#define	G_ZS_SENDSIGNAL		0xda
#define	G_ZS_WAITSIGNAL		0xd9
#define	G_ZS_SETSUBDL		0xd8
#define	G_ZS_LINKSUBDL		0xd7
#define	G_ZS_MULT_MPMTX		0xd6
#define	G_ZS_MTXCAT		0xd5
#define	G_ZS_MTXTRNSP		0xd4
#define	G_ZS_LIGHTING_L		0xd3	/* Not use */
#define	G_ZS_LIGHTING		0xd2
#define	G_ZS_XFMLIGHT		0xd1
#define	G_ZS_INTERPOLATE	0xd0

/*------------------------------------------------------------------------*
 *	case_G_ZOBJ
 *	+--------+--------+--------+--------+
 *	|       Pointr for zobj0:32         |
 *	+--------+--------+--------+--------+
 *	|       Pointr for zobj1:32         |
 *	+--------+--------+--------+--------+
 *	Draw zobject links
 */
#define	gSPZObject(pkt, objA, objB)			\
{							\
	Gfx *_g = (Gfx *)(pkt);				\
	_g->words.w0 = (unsigned int)(objA);		\
	_g->words.w1 = (unsigned int)(objB);      	\
}

#define	gsSPZObject(objA, objB)				\
{							\
	(unsigned int)(objA), (unsigned int)(objB)	\
}

/*------------------------------------------------------------------------*
 *	case_G_RDPCMD
 *	+--------+--------+--------+--------+
 *	| RDPCMD |             0            |
 *	+--------+--------+--------+--------+
 *	|      Pointr for rdpcmd:32         |
 *	+--------+--------+--------+--------+
 *	Submit rdp cmd
 */
#define	gSPZRdpCmd(pkt, cmd)	gImmp1((pkt), G_ZS_RDPCMD, (cmd))
#define	gsSPZRdpCmd(cmd)	gsImmp1(G_ZS_RDPCMD, (cmd))

/*------------------------------------------------------------------------*
 *	case_G_MOVEMEM
 *	+--------+--------++-------+-+----+-+
 *	|MOVEMEM |  Len:9  |  Ofs:9  |ID:5|F|
 *	+--------+--------++-------+-+----+-+
 *	|       Pointr for RDRAM:32         |
 *	+--------+--------+--------+--------+
 *	Read/Write DMEM
 */
#define	GZM_USER0		0
#define	GZM_USER1		2
#define	GZM_MMTX		4
#define	GZM_PMTX		6
#define	GZM_MPMTX		8
#define	GZM_OTHERMODE		10
#define	GZM_VIEWPORT		12
#define	GZF_LOAD		0
#define	GZF_SAVE		1

#define	gSPZMoveMem(pkt, id, ofs, len, buffer, flag)		\
{								\
  Gfx *_g = (Gfx *)(pkt);					\
  _g->words.w0 = _SHIFTL(G_ZS_MOVEMEM, 24, 8)|			\
                 _SHIFTL(((len)-1)>>3, 15, 9)|			\
                 _SHIFTL((ofs)>>3,      6, 9)|(id)|(flag);	\
	_g->words.w1 = (unsigned int)(buffer);			\
}

#define	gsSPZMoveMem(id, ofs, len, buffer, flag)		\
{								\
	_SHIFTL(G_ZS_MOVEMEM, 24, 8)|				\
	_SHIFTL(((len)-1)>>3, 15, 9)|				\
	_SHIFTL((ofs)>>3,      6, 9)|(id)|(flag),		\
	(unsigned int)(buffer)					\
}

/*------------------------------------------------------------------------*/
#define	gSPZSetUMem(pkt, umem, size, adrs)	\
	gSPZMoveMem((pkt),GZM_USER0,(umem),(size),(adrs),GZF_LOAD)
#define	gsSPZSetUMem(umem, size, adrs)		\
	gsSPZMoveMem(GZM_USER0,(umem),(size),(adrs),GZF_LOAD)

#define	gSPZGetUMem(pkt, umem, size, adrs)	\
	gSPZMoveMem((pkt),GZM_USER0,(umem),(size),(adrs),GZF_SAVE)
#define	gsSPZGetUMem(umem, size, adrs)		\
	gsSPZMoveMem(GZM_USER0,(umem),(size),(adrs),GZF_SAVE)

/*------------------------------------------------------------------------*/
#define	gSPZSetMtx(pkt, mid, mp)	\
        gSPZMoveMem((pkt), (mid), 0, sizeof(Mtx), (mp), GZF_LOAD)
#define	gsSPZSetMtx(mid, mp)	\
        gsSPZMoveMem((mid), 0, sizeof(Mtx), (mp), GZF_LOAD)

/*------------------------------------------------------------------------*/
#define	gSPZGetMtx(pkt, mid, mp)	\
        gSPZMoveMem((pkt), (mid), 0, sizeof(Mtx), (mp), GZF_SAVE)
#define	gsSPZGetMtx(mid, mp)	\
        gsSPZMoveMem((mid), 0, sizeof(Mtx), (mp), GZF_SAVE)

/*------------------------------------------------------------------------*/
#define	gSPZSetAmbient(pkt, umem, lights)				\
	gSPZSetUMem((pkt),(umem),ZSIZE_AMBIENT,(lights))
#define	gsSPZSetAmbient(umem, lights)					\
	gsSPZSetUMem((umem),ZSIZE_AMBIENT,(lights))

/*------------------------------------------------------------------------*/
#define	gSPZSetDefuse(pkt, umem, lid, lights)				\
	gSPZSetUMem((pkt),(umem)+ZSIZE_AMBIENT+(lid)*ZSIZE_LIGHT,	\
		    ZSIZE_DEFUSE, (lights))
#define	gsSPZSetDefuse(umem, lid, lights)				\
	gsSPZSetUMem((umem)+ZSIZE_AMBIENT+(lid)*ZSIZE_LIGHT,		\
		     ZSIZE_DEFUSE, (lights))

/*------------------------------------------------------------------------*/
#define	gSPZSetLookAt(pkt, umem, lid, lookat)				\
{	gSPZSetUMem((pkt), (umem)+ZSIZE_AMBIENT+((lid)+0)*ZSIZE_LIGHT,	\
		    ZSIZE_DEFUSE,					\
		    ((u32)(lookat))+sizeof(Light)*0);			\
	gSPZSetUMem((pkt), (umem)+ZSIZE_AMBIENT+((lid)+1)*ZSIZE_LIGHT,	\
		    ZSIZE_DEFUSE,					\
		    ((u32)(lookat))+sizeof(Light)*1);			}
#define	gsSPZSetLookAt(umem, lid, lookat)				\
	gsSPZSetUMem((umem)+ZSIZE_AMBIENT+((lid)+0)*ZSIZE_LIGHT,	\
		    ZSIZE_DEFUSE,					\
		    ((u32)(lookat))+sizeof(Light)*0),			\
	gsSPZSetUMem((umem)+ZSIZE_AMBIENT+((lid)+1)*ZSIZE_LIGHT,	\
		    ZSIZE_DEFUSE,					\
		    ((u32)(lookat))+sizeof(Light)*1)

/*------------------------------------------------------------------------*/
#define	gSPZViewport(pkt, v)	\
	gSPZMoveMem((pkt), GZM_VIEWPORT, 0, 16, (v), GZF_LOAD)
#define	gsSPZViewport(v)	\
	gsSPZMoveMem(GZM_VIEWPORT, 0, 16, (v), GZF_LOAD)

#define	GZ_VIEWPORT_FOG_S(in, out)	(0x400000/((out)-(in)))
#define	GZ_VIEWPORT_FOG_T(in, out)	((0x200*(out))/((in)-(out)))

/*------------------------------------------------------------------------*
 *	case_G_SENDSIGNAL
 *	+--------+--------+--------+--------+
 *	|Send SIG|  Parameter1:24           |
 *	+--------+--------+--------+--------+
 *	|           Parameter2:32           |
 *	+--------+--------+--------+--------+
 *	Send signal to CPU
 */
#define	GZ_INTR_SPBRK1	SP_STATUS_RSPSIGNAL
#define	GZ_INTR_SPBRK2							\
	(SP_SET_RSPSIGNAL|SP_CLR_TASKDONE|SP_CLR_YIELDED|SP_SET_INTR)

#define	gSPZSendMessage(pkt)						\
	gDma0p((pkt), G_ZS_SENDSIGNAL, GZ_INTR_SPBRK2, GZ_INTR_SPBRK1)
#define	gsSPZSendMessage()						\
	gsDma0p(G_ZS_SENDSIGNAL, GZ_INTR_SPBRK2, GZ_INTR_SPBRK1)

/*------------------------------------------------------------------------*
 *	case_G_WAITSIGNAL
 *	+--------+--------+--------+--------+
 *	|Wait SIG| Phy Adrs of signal buf:24|
 *	+--------+--------+--------+--------+
 *	|           Parameter:32            |
 *	+--------+--------+--------+--------+
 *	Wait signal from CPU
 */
#define	gSPZWaitSignal(pkt, adrs, param)			\
	gDma0p((pkt), G_ZS_WAITSIGNAL, (param), (adrs))

#define	gsSPZWaitSignal(adrs, param)				\
{								\
  (unsigned int)((char *)(adrs)+((G_ZS_WAITSIGNAL-0x80)<<24)),	\
  (unsigned int)(param)						\
}

#define	GZ_SENDSIGNAL(adrs,val)	{				\
  IO_WRITE(&(((zSignal *)(adrs))->t.signal), (val));		\
  IO_WRITE(SP_STATUS_REG, SP_SET_CPUSIGNAL);			\
}

/*------------------------------------------------------------------------*
 *	case_G_SETSUBDL
 *	+--------+--------+--------+--------+
 *	|SETSUBDL|            0             |
 *	+--------+--------+--------+--------+
 *	|               DL:32               |
 *	+--------+--------+--------+--------+
 */
#define	gSPZSetSubDL(pkt, dl)	gImmp1((pkt), G_ZS_SETSUBDL, (u32)(dl))
#define	gsSPZSetSubDL(dl)	gsImmp1(G_ZS_SETSUBDL, (u32)(dl))

/*------------------------------------------------------------------------*
 *	case_G_LINKSUBDL
 *	+--------+--------+--------+--------+
 *	|LINKSUBD|            0             |
 *	+--------+--------+--------+--------+
 *	|                 0                 |
 *	+--------+--------+--------+--------+
 */
#define	gSPZLinkSubDL(pkt)	gImmp0((pkt), G_ZS_LINKSUBDL)
#define	gsSPZLinkSubDL()	gsImmp0(G_ZS_LINKSUBDL)

/*------------------------------------------------------------------------*
 *	case_G_MULT_MPMTX
 *	+--------+--------+--------+--------+
 *	|MULT_MPM|        0        | Mid:8  |
 *	+--------+--------+----+---+--------+
 *	| Num:8  |   Src:12    |   Dest:12  |
 *	+--------+-------------+------------+
 */
#define	gSPZMultMPMtx(pkt, mid, src, num, dest)				\
{									\
  Gfx *_g = (Gfx *)(pkt);						\
  _g->words.w0 = _SHIFTL(G_ZS_MULT_MPMTX,24,8)|_SHIFTL((mid),0,8);	\
  _g->words.w1 = _SHIFTL(((num)-1), 24, 8)|				\
                 _SHIFTL((src)+1024,12,12)|_SHIFTL((dest)+1024, 0,12);	\
}

#define	gsSPZMultMPMtx(mid, src, num, dest)				\
{									\
	_SHIFTL(G_ZS_MULT_MPMTX,24,8)|_SHIFTL((mid),0, 8),		\
	_SHIFTL(((num)-1), 24, 8)|					\
	_SHIFTL((src)+1024,12,12)|_SHIFTL((dest)+1024,0,12)		\
}

/*------------------------------------------------------------------------*
 *	case_G_MTXCAT
 *	+--------+--------+--------+--------+
 *	| MTXCAT |    0   |    0   | MidS:8 |
 *	+--------+--------+--------+--------+
 *	|    0   | MidT:8 |    0   | MidD:8 |
 *	+--------+--------+--------+--------+
 */
#define	gSPZMtxCat(pkt, midS, midT, midD)				\
{									\
	Gfx *_g = (Gfx *)(pkt);						\
	_g->words.w0 = _SHIFTL(G_ZS_MTXCAT,24,8)|_SHIFTL((midS), 0,8);	\
	_g->words.w1 = _SHIFTL((midT),     16,8)|_SHIFTL((midD), 0,8);	\
}

#define	gsSPZMtxCat(midS, midT, midD)					\
{									\
	_SHIFTL(G_ZS_MTXCAT,24,8)|_SHIFTL((midS), 0,8),			\
	_SHIFTL((midT),     16,8)|_SHIFTL((midD), 0,8)			\
}

/*------------------------------------------------------------------------*
 *	case_G_MTXTRNSP
 *	+--------+--------+--------+--------+
 *	|MTXTRNSP|    0   |    0   |    0   |
 *	+--------+--------+--------+--------+
 *	|    0   |    0   |    0   | Mid:8  |
 *	+--------+--------+--------+--------+
 */
#define	gSPZMtxTrnsp3x3(pkt, mid)	gImmp1((pkt), G_ZS_MTXTRNSP, (mid))
#define	gsSPZMtxTrnsp3x3(mid)		gsImmp1(G_ZS_MTXTRNSP, (mid))

/*------------------------------------------------------------------------*
 *	case_G_XFMLIGHT
 *	+--------+--------+--------+--------+
 *	|XFMLIGHT|        0        | Mid:8  |
 *	+--------+----+---+----+---+--------+
 * 	|      0      | Lnum:8 |   Lptr:12  |
 *	+--------+----+---+----+---+--------+
 */
#define	gSPZXfmLights(pkt, mid, umem, lnum)				\
{									\
  Gfx *_g = (Gfx *)(pkt);						\
  _g->words.w0 = _SHIFTL(G_ZS_XFMLIGHT,24,8)|(mid);			\
  _g->words.w1 = _SHIFTL((lnum)-1,12,8)|_SHIFTL((umem)+1024, 0,12);	\
}

#define	gsSPZXfmLights(mid, umem, lnum)					\
{									\
	_SHIFTL(G_ZS_XFMLIGHT,24,8)|(mid),				\
	_SHIFTL((lnum)-1,12,8)|_SHIFTL((umem)+1024, 0,12)		\
}

/*------------------------------------------------------------------------*
 *	case_G_LIGHTING
 *	+--------+-------------+------------+
 *	|LIGHTING|  Color:12   |  Norm:12   |
 *	+--------+-------------+------------+
 *	| Num:8  | DestColor:12| DestTxtr:12|
 *	+--------+-------------+------------+
 *	gSPZLight/gSPZLightMaterial
 */
#define	gSPZLightMaterial(pkt, csrc, nsrc, num, cdest, tdest)		\
{									\
  Gfx *_g = (Gfx *)(pkt);						\
  _g->words.w0 = _SHIFTL(G_ZS_LIGHTING,24,8)|				\
                 _SHIFTL((csrc)+1024, 12,12)|_SHIFTL((nsrc) +1024,0,12);\
  _g->words.w1 = _SHIFTL((num)-1,     24, 8)|				\
		 _SHIFTL((cdest)+1024,12,12)|_SHIFTL((tdest)+1024,0,12);\
}

#define	gsSPZLightMaterial(csrc, nsrc, num, cdest, tdest)		\
{									\
	_SHIFTL(G_ZS_LIGHTING,24,8)|					\
	_SHIFTL((csrc)+1024, 12,12)|_SHIFTL((nsrc) +1024,0,12),		\
	_SHIFTL((num)-1,     24, 8)|					\
	_SHIFTL((cdest)+1024,12,12)|_SHIFTL((tdest)+1024,0,12)		\
}

#define	gSPZLight(pkt, nsrc, num, cdest, tdest)				\
	gSPZLightMaterial((pkt),-16,(nsrc),(num),(cdest),(tdest))
#define	gsSPZLight(nsrc, num, cdest, tdest)				\
	gsSPZLightMaterial(-16,(nsrc),(num),(cdest),(tdest))

/*------------------------------------------------------------------------*
 *	case_G_INTERPOLATE
 *	+--------+--------+-----------------+
 *	|INTERPLT| Type:8 |    Factor:16    |
 *	+--------+--------+----+------------+
 *	| Num:8  | Src1/Dest:12|   Src2:12  |
 *	+--------+-------------+------------+
 */
#define	GZ_INTRP_S16		0
#define	GZ_INTRP_S8		2
#define	GZ_INTRP_U8		4

#define	gSPZMix(pkt, src1, src2, num, factor, type)			\
{									\
	Gfx *_g = (Gfx *)(pkt);						\
	_g->words.w0 = _SHIFTL(G_ZS_INTERPOLATE,24,8)|			\
		       _SHIFTL((type),16, 8)|_SHIFTL((factor),0,16);	\
	_g->words.w1 = _SHIFTL((num)/8-1,24,8)|				\
		       _SHIFTL((src1),12,12)|_SHIFTL((src2),  0,12);	\
}

#define	gsSPZMix(src1, src2, num, factor, type)				\
{									\
	_SHIFTL(G_ZS_INTERPOLATE,24,8)|					\
	_SHIFTL((type),16, 8)|_SHIFTL((factor),0,16),			\
	_SHIFTL(((num)-1)/8,24,8)|					\
	_SHIFTL((src1),12,12)|_SHIFTL((src2),  0,12)			\
}

#define	gSPZMixS16(pkt, src1, src2, num, factor)			\
	gSPZMix((pkt), (src1), (src2), (num), (factor), GZ_INTRP_S16)
#define	gSPZMixS8(pkt, src1, src2, num, factor)				\
	gSPZMix((pkt), (src1), (src2), (num), (factor), GZ_INTRP_S8)
#define	gSPZMixU8(pkt, src1, src2, num, factor)				\
	gSPZMix((pkt), (src1), (src2), (num), (factor), GZ_INTRP_U8)

#define	gsSPZMixS16(src1, src2, num, factor)				\
	gsSPZMix((src1), (src2), (num), (factor), GZ_INTRP_S16)
#define	gsSPZMixS8(src1, src2, num, factor)				\
	gsSPZMix((src1), (src2), (num), (factor), GZ_INTRP_S8)
#define	gsSPZMixU8(src1, src2, num, factor)				\
	gsSPZMix((src1), (src2), (num), (factor), GZ_INTRP_U8)

/*------------------------------------------------------------------------*
 *  Compatible GBIs
 *------------------------------------------------------------------------*/
#define	gSPZSegment(pkt, segment, base)	gSPSegment((pkt), (segment), (base))
#define	gsSPZSegment(segment, base)	gsSPSegment((segment), (base))
#define	gSPZPerspNormalize(pkt, s)	gSPPerspNormalize((pkt), (s))
#define	gsSPZPerspNormalize(s)		gsSPPerspNormalize(s)
#define	gSPZDisplayList(pkt, dl)	gSPDisplayList((pkt), (dl))
#define	gsSPZDisplayList(dl)		gsSPDisplayList(dl)
#define	gSPZEndDisplayList(pkt)		gSPEndDisplayList(pkt)
#define	gsSPZEndDisplayList()		gsSPEndDisplayList()

/*===========================================================================*
 *      Data structures for ZSort microcode
 *===========================================================================*/

#define	G_ZOBJ_NONE	0x80000000

/* ZObject IDs */ 
#define	ZH_NULL		0
#define	ZH_SHTRI	1
#define	ZH_TXTRI	2
#define	ZH_SHQUAD	3
#define	ZH_TXQUAD	4
#define	ZHDR(pointer, type)	((type)+((u32)(pointer)))

/*===========================================================================*
 *      Useful MACRO
 *===========================================================================*/
#define	guZFixLookAt(lp)	\
        { (lp)->l[1].l.col[1] = (lp)->l[1].l.colc[1] = 0x00; }

#if	(defined(_LANGUAGE_C)||defined(_LANGUAGE_C_PLUS_PLUS))
/*------------------------------------------------------------------------*/
/* ZMath: Vertex structure format */

/*-----------------------------------------------------*/
#define	ZSIZE_VSRC	6

typedef	struct	{  s16	x,y,z;		} zVtxSrc;

/*-----------------------------------------------------*/
#define	ZSIZE_VDEST	16

typedef	struct	{
  s16	sx, sy;
  s32	invw;
  s16	xi, yi;
  u8	cc;
  u8	fog;
  s16	wi;
} zVtxDest;

#define	GZ_CC_RIGHT	0x01
#define	GZ_CC_TOP	0x02
#define	GZ_CC_NEAR	0x04
#define	GZ_CC_LEFT	0x10
#define	GZ_CC_BOTTOM	0x20
#define	GZ_CC_FAR	0x40

/*-----------------------------------------------------*/
#define	ZSIZE_NSRC	 3

typedef struct  {  s8	nx,ny,nz;	} zNorm;

/*-----------------------------------------------------*/
#define	ZSIZE_CSRC	 4
#define	ZSIZE_CDEST	 4

typedef struct  {  u8	r, g, b, a;	} zColor_t;
typedef union   {
  zColor_t	n;
  u32		w;
} zColor;

/*-----------------------------------------------------*/
#define	ZSIZE_TDEST	 4

typedef struct  {  s16	s, t;		} zTxtr_t;
typedef union   {
  zTxtr_t       n;
  u32           w;
} zTxtr;

/*-----------------------------------------------------*/
#define	ZSIZE_LIGHT		24
#define	ZSIZE_DEFUSE		16
#define	ZSIZE_AMBIENT		 8

/*------------------------------------------------------------------------*/
/* ZObject: Common header */
typedef	struct	{
  u32	header;
  Gfx	*rdpcmd1;
} zHeader_t;

typedef	union	{
  zHeader_t	t;
  u64		force_structure_alignment;
} zHeader;

/*------------------------------------------------------------------------*/
/* ZObject: Null Node */
typedef	struct	{
  zHeader	*header;
  Gfx		*rdpcmd1;
  Gfx		*rdpcmd2;
  Gfx		*rdpcmd3;
} zNull_t;

typedef	union	{
  zNull_t	t;
  u64		force_structure_alignment;
} zNull;

/*------------------------------------------------------------------------*/
/* ZObject: Smooth Shaded Triangle */
typedef	struct	{
  s16	x, y;
  u8	r, g, b, a;
} zShVtx;

/*------------------------------------------------------------------------*/
typedef	struct	{
  zHeader	*header;
  Gfx		*rdpcmd1;
  zShVtx	v[3];
} zShTri_t;

typedef	struct	{
  zHeader	*header;
  Gfx		*rdpcmd1;
  u32		xy0, clr0;
  u32		xy1, clr1;
  u32		xy2, clr2;  
} zShTri_w;

typedef	union	{
  zShTri_t	t;
  zShTri_w	w;
  u64		force_structure_alignment;
} zShTri;

/*------------------------------------------------------------------------*/
/* ZObject: Smooth Shaded Quadrangle */
typedef	struct	{
  zHeader	*header;
  Gfx		*rdpcmd1;
  zShVtx	v[4];
} zShQuad_t;

typedef	struct	{
  zHeader	*header;
  Gfx		*rdpcmd1;
  u32		xy0, clr0;
  u32		xy1, clr1;
  u32		xy2, clr2;  
  u32		xy3, clr3;
} zShQuad_w;

typedef	union	{
  zShQuad_t	t;
  zShQuad_w	w;
  u64		force_structure_alignment;
} zShQuad;

/*------------------------------------------------------------------------*/
/* ZObject: Smooth Shaded and Texture Mapped Triangle */
typedef	struct	{
  s16	x, y;
  u8	r, g, b, a;
  s16	s, t;
  s32	invw;
} zTxVtx;

/*------------------------------------------------------------------------*/
typedef	struct	{
  zHeader	*header;
  Gfx		*rdpcmd1;
  Gfx		*rdpcmd2;
  Gfx		*rdpcmd3;
  zTxVtx	v[3];
} zTxTri_t;

typedef	struct	{
  zHeader	*header;
  Gfx		*rdpcmd1;
  Gfx		*rdpcmd2;
  Gfx		*rdpcmd3;
  u32		xy0, clr0, st0, invw0;
  u32		xy1, clr1, st1, invw1;
  u32		xy2, clr2, st2, invw2;
} zTxTri_w;

typedef	union	{
  zTxTri_t	t;
  zTxTri_w	w;
  u64		force_structure_alignment;
} zTxTri;

/*------------------------------------------------------------------------*/
/* ZObject: Smooth Shaded and Texture Mapped Quadrangle */
typedef	struct	{
  zHeader	*header;
  Gfx		*rdpcmd1;
  Gfx		*rdpcmd2;
  Gfx		*rdpcmd3;
  zTxVtx	v[4];
} zTxQuad_t;

typedef	struct	{
  zHeader	*header;
  Gfx		*rdpcmd1;
  Gfx		*rdpcmd2;
  Gfx		*rdpcmd3;
  u32		xy0, clr0, st0, invw0;
  u32		xy1, clr1, st1, invw1;
  u32		xy2, clr2, st2, invw2;
  u32		xy3, clr3, st3, invw3;
} zTxQuad_w;

typedef	union	{
  zTxQuad_t	t;
  zTxQuad_w	w;
  u64		force_structure_alignment;
} zTxQuad;

/*------------------------------------------------------------------------*/
typedef	struct	{
  u32	signal;
  u32	padding;
} zSignal_t;

typedef	union	{
  zSignal_t	t;
  u64		force_structure_alignment;
} zSignal;

/*===========================================================================*
 *      External functions 
 *===========================================================================*/
extern  u64     gspZSort_fifoTextStart[], gspZSort_fifoTextEnd[];
extern  u64     gspZSort_fifoDataStart[], gspZSort_fifoDataEnd[];
extern  u64     gspZSort_pl_fifoTextStart[], gspZSort_pl_fifoTextEnd[];
extern  u64     gspZSort_pl_fifoDataStart[], gspZSort_pl_fifoDataEnd[];
#endif

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif
#endif /* _GZSORT_H_ */

/*======== End of gzsort.h ========*/
