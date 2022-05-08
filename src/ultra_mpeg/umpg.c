/******************************************************************************
 *        ultra_mpeg - An MPEG-1/2 decoder library for the Nintendo 64        *
 *                       Copyright (C) 2020  devwizard                        *
 *     This project is licensed under the terms of the MIT license.  See      *
 *     LICENSE for more information.                                          *
 ******************************************************************************/

#if !defined(_UMPG_PL_MPEG) && !defined(_UMPG_LIBMPEG2)
#error must define _UMPG_PL_MPEG or _UMPG_LIBMPEG2
#endif

#include <stdlib.h>

#include <ultra64.h>
#include <types.h>

#ifdef _UMPG_PL_MPEG
#define PL_MPEG_IMPLEMENTATION
#include <pl_mpeg.h>
#endif

#ifdef _UMPG_LIBMPEG2
#include <mpeg2.h>
#include <mpeg2convert.h>
#endif

#define UMPG_BILERP     0
#define UMPG_TW         64
#define UMPG_TH         32
#define UMPG_DMA_SIZE   0x4000

struct umpg_t
{
#ifdef _UMPG_PL_MPEG
    /* 0x00      */ plm_t          *plm;
    /* 0x04      */ plm_buffer_t   *buffer;
    /* 0x08      */ Gfx            *video_gfx;
#endif
#ifdef _UMPG_LIBMPEG2
    /*      0x00 */ mpeg2dec_t     *mpeg2dec;
#endif
    /* 0x0C 0x04 */ s16             x;
    /* 0x0E 0x06 */ s16             y;
    /* 0x10 0x08 */ u16             w;
    /* 0x12 0x0A */ u16             h;
    /* 0x14 0x0C */ const u8       *start;
    /* 0x18 0x10 */ const u8       *end;
    /* 0x1C 0x14 */ const u8       *src;
    /* 0x20 0x18 */ u8             *dma_data;
    /* 0x24 0x1C */ size_t          dma_size;
    /* 0x28 0x20 */ Gfx            *gfx;
    /* 0x2C 0x24 */ OSIoMesg        iomesg;
    /* 0x44 0x3C */ OSMesgQueue     mq_pi;
#ifdef _UMPG_PL_MPEG
    /* 0x5C      */ OSMesgQueue     mq_ai;
#endif
#ifdef _UMPG_LIBMPEG2
    /*      0x54 */ OSMesgQueue     mq_timer;
#endif
    /* 0x74 0x6C */ OSMesg          msg[1];
#ifdef _UMPG_PL_MPEG
    /* 0x78      */ OSTime          time;
    /* 0x80      */ f64             frame_time;
    /* 0x88      */ u8             *video_table[2];
    /* 0x90      */ s16            *audio_table[2];
    /* 0x98      */ u8              video_index;
    /* 0x99      */ u8              audio_index;
#endif
#ifdef _UMPG_LIBMPEG2
    /*      0x70 */ OSTimer         timer;
#endif
};  /* 0x9A 0x90 */

static const Gfx gfx_umpg_start[] =
{
    gsDPPipeSync(),
#if UMPG_BILERP
    gsDPSetCombineMode(G_CC_DECALRGBA, G_CC_YUV2RGB),
#else
    gsDPSetCombineMode(G_CC_1CYUV2RGB, G_CC_1CYUV2RGB),
#endif
    gsDPSetTile(
        G_IM_FMT_YUV, G_IM_SIZ_16b, 8, 0, G_TX_RENDERTILE, 0,
        G_TX_CLAMP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD,
        G_TX_CLAMP | G_TX_NOMIRROR, G_TX_NOMASK, G_TX_NOLOD
    ),
    gsDPSetOtherMode(
        G_PM_NPRIMITIVE |
        (UMPG_BILERP ? G_CYC_2CYCLE : G_CYC_1CYCLE) |
        G_TP_NONE |
        G_TD_CLAMP |
        G_TL_TILE |
        G_TT_NONE |
        (UMPG_BILERP ? G_TF_AVERAGE : G_TF_POINT) |
        (UMPG_BILERP ? G_TC_FILTCONV : G_TC_CONV) |
        G_CK_NONE |
        G_CD_MAGICSQ |
        G_AD_DISABLE |
        15,
        (UMPG_BILERP ? G_RM_PASS : G_RM_OPA_SURF) | G_RM_OPA_SURF2 |
        G_ZS_PIXEL |
        G_AC_NONE
    ),
    gsDPSetConvert(G_CV_K0, G_CV_K1, G_CV_K2, G_CV_K3, G_CV_K4, G_CV_K5),
    gsSPEndDisplayList(),
};

static void umpg_dma_sync(struct umpg_t *umpg)
{
    size_t size = umpg->dma_size;
    umpg->dma_size = 0;
    if (size > 0)
    {
        osRecvMesg(&umpg->mq_pi, NULL, OS_MESG_BLOCK);
    #ifdef _UMPG_PL_MPEG
        plm_buffer_write(umpg->buffer, umpg->dma_data, size);
    #endif
    #ifdef _UMPG_LIBMPEG2
        mpeg2_buffer(umpg->mpeg2dec, umpg->dma_data, umpg->dma_data+size);
    #endif
    }
}

static void umpg_dma_read(struct umpg_t *umpg, size_t size)
{
    size_t max;
    if (umpg->src == umpg->end)
    {
        umpg->src = umpg->start;
    }
    size = (size+1) & ~0x01;
    max = umpg->end - umpg->src;
    if (size > max)
    {
        size = max;
    }
    if (size > 0)
    {
        uintptr_t src = (uintptr_t)umpg->src;
        umpg->src += size;
        umpg->dma_size = size;
        osInvalDCache(umpg->dma_data, size);
        osPiStartDma(
            &umpg->iomesg, OS_MESG_PRI_NORMAL, OS_READ, src, umpg->dma_data,
            size, &umpg->mq_pi
        );
    }
}

static void umpg_gfx_init(struct umpg_t *umpg, uint w, uint h)
{
    if (umpg->gfx == NULL)
    {
        Gfx *gfx;
        uint y;
        uint x;
        int  xl;
        int  yl;
        int  xh;
        int  yh;
        uint dsdx;
        uint dtdy;
    #ifdef _UMPG_PL_MPEG
        if (umpg->video_table[0] == NULL)
        {
            size_t size = 2*w*h;
            umpg->video_table[0] = malloc(2*size);
            umpg->video_table[1] = umpg->video_table[0] + size;
        }
    #endif
        x = (w+UMPG_TW-1) / UMPG_TW;
        y = (h+UMPG_TH-1) / UMPG_TH;
        umpg->gfx = gfx = malloc(sizeof(*gfx) * (2 + 7*x*y));
        xl = umpg->x > 0 ? umpg->x : 0;
        yl = umpg->y > 0 ? umpg->y : 0;
        xh = umpg->x + umpg->w;
        yh = umpg->y + umpg->h;
        gDPSetScissorFrac(gfx++, G_SC_NON_INTERLACE, xl, yl, xh, yh);
        dsdx = 0x1000*w/umpg->w;
        dtdy = 0x1000*h/umpg->h;
        for (y = 0; y < h; y += UMPG_TH)
        {
            for (x = 0; x < w; x += UMPG_TW)
            {
                int  s;
                int  t;
            #if UMPG_BILERP
                int  ul;
                int  vl;
            #else
                uint ul;
                uint vl;
            #endif
                uint uh;
                uint vh;
                uint uls;
                uint ult;
                uint lrs;
                uint lrt;
                s = 32*x - UMPG_BILERP*16;
                t = 32*y - UMPG_BILERP*16;
                xl = umpg->x + umpg->w*(x+      0)/w;
                yl = umpg->y + umpg->h*(y+      0)/h;
                xh = umpg->x + umpg->w*(x+UMPG_TW)/w;
                yh = umpg->y + umpg->h*(y+UMPG_TH)/h;
                ul = (x+      0) - UMPG_BILERP*2;
                vl = (y+      0) - UMPG_BILERP*1;
                uh = (x+UMPG_TW) + UMPG_BILERP*2;
                vh = (y+UMPG_TH) + UMPG_BILERP*1;
            #if UMPG_BILERP
                if (ul < 0)
                {
                    ul = 0;
                }
                if (vl < 0)
                {
                    vl = 0;
                }
            #endif
                if (uh > w)
                {
                    uh = w;
                }
                if (vh > h)
                {
                    vh = h;
                }
                uls = (ul  ) << G_TEXTURE_IMAGE_FRAC;
                ult = (vl  ) << G_TEXTURE_IMAGE_FRAC;
                lrs = (uh-1) << G_TEXTURE_IMAGE_FRAC;
                lrt = (vh-1) << G_TEXTURE_IMAGE_FRAC;
                gDPPipeSync(gfx++);
                gDPLoadSync(gfx++);
                gDPLoadTile(gfx++, G_TX_RENDERTILE, uls, ult, lrs, lrt);
                gDPSetTileSize(gfx++, G_TX_RENDERTILE, uls, ult, lrs, lrt);
                gSPTextureRectangle(
                    gfx++, xl, yl, xh, yh, G_TX_RENDERTILE, s, t, dsdx, dtdy
                );
            }
        }
        gSPEndDisplayList(gfx);
    }
}

static void umpg_gfx_draw(struct umpg_t *umpg, Gfx **gfx, u8 *dst, uint w)
{
    gDPSetTextureImage((*gfx)++, G_IM_FMT_YUV, G_IM_SIZ_16b, w, dst);
    gSPDisplayList((*gfx)++, gfx_umpg_start);
    gSPDisplayList((*gfx)++, umpg->gfx);
}

#ifdef _UMPG_PL_MPEG
static void umpg_uyvy(
    u8 *dst, uint w, uint h, const u8 *y, const u8 *u, const u8 *v
)
{
    do
    {
        const u8 *s = u;
        const u8 *t = v;
        uint x;
        x = w;
        do
        {
        #define UYVY(n)                                                        \
        {                                                                      \
            dst[4*(n)+0] = s[n]; dst[4*(n)+1] = y[2*(n)+0];                    \
            dst[4*(n)+2] = t[n]; dst[4*(n)+3] = y[2*(n)+1];                    \
        }
            UYVY(0);
            UYVY(1);
            UYVY(2);
            UYVY(3);
            UYVY(4);
            UYVY(5);
            UYVY(6);
            UYVY(7);
        #undef UYVY
            dst   += 8*4;
            y     += 8*2;
            s     += 8*1;
            t     += 8*1;
            x     -= 8*2;
        }
        while (x > 0);
        x = w;
        do
        {
        #define UYVY(n)                                                        \
        {                                                                      \
            dst[4*(n)+0] = (u[n]+s[n])/2; dst[4*(n)+1] = y[2*(n)+0];           \
            dst[4*(n)+2] = (v[n]+t[n])/2; dst[4*(n)+3] = y[2*(n)+1];           \
        }
            UYVY(0);
            UYVY(1);
            UYVY(2);
            UYVY(3);
            UYVY(4);
            UYVY(5);
            UYVY(6);
            UYVY(7);
        #undef UYVY
            dst   += 8*4;
            y     += 8*2;
            u     += 8*1;
            v     += 8*1;
            s     += 8*1;
            t     += 8*1;
            x     -= 8*2;
        }
        while (x > 0);
        h -= 2;
    }
    while (h > 0);
}

static void umpg_video_decode_callback(
    unused plm_t *plm, plm_frame_t *frame, void *user
)
{
    struct umpg_t *umpg = user;
    uint w = frame->width;
    uint h = frame->height;
    u8 *dst;
    umpg_gfx_init(umpg, w, h);
    dst = umpg->video_table[umpg->video_index];
    umpg->video_index ^= 1;
    umpg_gfx_draw(umpg, &umpg->video_gfx, dst, w);
    umpg_uyvy(dst, w, h, frame->y.data, frame->cb.data, frame->cr.data);
}

static void umpg_audio_decode_callback(
    unused plm_t *plm, plm_samples_t *samples, void *user
)
{
    struct umpg_t *umpg = user;
    s16 *buf;
    s16 *dst;
    const f32 *src;
    uint len;
    buf = dst = umpg->audio_table[umpg->audio_index];
    umpg->audio_index ^= 1;
    src = samples->interleaved;
    len = PLM_AUDIO_SAMPLES_PER_FRAME;
    do
    {
        dst[0] = 0x10000*src[0];
        dst[1] = 0x10000*src[1];
        dst += 2;
        src += 2;
        len -= 1;
    }
    while (len > 0);
    osRecvMesg(&umpg->mq_ai, NULL, OS_MESG_BLOCK);
    osWritebackDCacheAll();
    osAiSetNextBuffer(buf, sizeof(s16)*2*PLM_AUDIO_SAMPLES_PER_FRAME);
}
#endif

#ifdef _UMPG_LIBMPEG2
static void umpg_gfx_sync(struct umpg_t *umpg, uint delay)
{
    if (delay != umpg->timer.interval)
    {
        osSetTimer(&umpg->timer, 0, delay, &umpg->mq_timer, (OSMesg)0);
    }
    osRecvMesg(&umpg->mq_timer, NULL, OS_MESG_BLOCK);
}
#endif

struct umpg_t *umpg_init(
    int x, int y, uint w, uint h, const void *start, const void *end
)
{
    struct umpg_t *umpg = malloc(sizeof(*umpg));
    umpg->x              = x;
    umpg->y              = y;
    umpg->w              = w;
    umpg->h              = h;
    umpg->start          = start;
    umpg->end            = end;
    umpg->src            = start;
    umpg->dma_size       = 0;
    umpg->gfx            = NULL;
#ifdef _UMPG_PL_MPEG
    umpg->time           = 0;
    umpg->video_table[0] = NULL;
    umpg->audio_table[0] = NULL;
    umpg->video_index    = 0;
    umpg->audio_index    = 0;
#endif
    umpg->dma_data = malloc(UMPG_DMA_SIZE);
    osCreateMesgQueue(&umpg->mq_pi, umpg->msg, lenof(umpg->msg));
#ifdef _UMPG_PL_MPEG
    umpg->buffer = plm_buffer_create_with_capacity(PLM_BUFFER_DEFAULT_SIZE);
    umpg_dma_read(umpg, UMPG_DMA_SIZE);
    umpg_dma_sync(umpg);
    umpg->plm = plm_create_with_buffer(umpg->buffer, true);
    plm_set_video_decode_callback(umpg->plm, umpg_video_decode_callback, umpg);
    plm_video_set_no_delay(umpg->plm->video_decoder, true);
    umpg->frame_time = 2.0 / plm_get_framerate(umpg->plm);
    if (plm_get_num_audio_streams(umpg->plm) > 0)
    {
        int freq;
        f64 time;
        umpg->audio_table[0] =
            malloc(sizeof(s16)*2 * 2*PLM_AUDIO_SAMPLES_PER_FRAME);
        umpg->audio_table[1] =
            umpg->audio_table[0] + 2*PLM_AUDIO_SAMPLES_PER_FRAME;
        osCreateMesgQueue(&umpg->mq_ai, umpg->msg, lenof(umpg->msg));
        osSetEventMesg(OS_EVENT_AI, &umpg->mq_ai, (OSMesg)0);
        osSendMesg(&umpg->mq_ai, (OSMesg)0, OS_MESG_NOBLOCK);
        plm_set_audio_decode_callback(
            umpg->plm, umpg_audio_decode_callback, umpg
        );
        plm_set_audio_enabled(umpg->plm, true);
        plm_set_audio_stream(umpg->plm, 0);
        freq = osAiSetFrequency(plm_get_samplerate(umpg->plm));
        time = umpg->frame_time + (f64)PLM_AUDIO_SAMPLES_PER_FRAME/freq;
        plm_set_audio_lead_time(umpg->plm, time);
    }
#endif
#ifdef _UMPG_LIBMPEG2
    umpg->mpeg2dec = mpeg2_init();
    osCreateMesgQueue(&umpg->mq_timer, umpg->msg, lenof(umpg->msg));
    umpg->timer.interval = 0;
#endif
    return umpg;
}

void umpg_free(struct umpg_t *umpg)
{
    free(umpg->dma_data);
    if (umpg->gfx != NULL)
    {
        free(umpg->gfx);
    }
#ifdef _UMPG_PL_MPEG
    if (umpg->video_table[0] != NULL)
    {
        free(umpg->video_table[0]);
    }
    if (umpg->audio_table[0] != NULL)
    {
        free(umpg->audio_table[0]);
    }
    plm_destroy(umpg->plm);
#endif
#ifdef _UMPG_LIBMPEG2
    mpeg2_close(umpg->mpeg2dec);
#endif
    free(umpg);
}

uint umpg_update(struct umpg_t *umpg, Gfx **gfx)
{
#ifdef _UMPG_PL_MPEG
    uint end = false;
    umpg->video_gfx = *gfx;
    do
    {
        OSTime time = osGetTime();
        f64 seconds = (int)(time-umpg->time) / 46875000.0;
        if (seconds > umpg->frame_time)
        {
            seconds = umpg->frame_time;
        }
        umpg->time = time;
        umpg_dma_sync(umpg);
        umpg_dma_read(
            umpg, UMPG_DMA_SIZE-plm_buffer_get_remaining(umpg->buffer)
        );
        plm_decode(umpg->plm, seconds);
        end = plm_has_ended(umpg->plm);
    }
    while (!end && *gfx == umpg->video_gfx);
    *gfx = umpg->video_gfx;
    return end;
#endif
#ifdef _UMPG_LIBMPEG2
    const mpeg2_info_t *info = mpeg2_info(umpg->mpeg2dec);
    while (umpg->src != umpg->end)
    {
        switch (mpeg2_parse(umpg->mpeg2dec))
        {
            case STATE_BUFFER:
                umpg_dma_sync(umpg);
                umpg_dma_read(umpg, UMPG_DMA_SIZE);
                break;
            case STATE_SEQUENCE:
                mpeg2_convert(umpg->mpeg2dec, mpeg2convert_uyvy, NULL);
                break;
            case STATE_SLICE:
            case STATE_END:
            case STATE_INVALID_END:
                if (info->display_fbuf != NULL)
                {
                    const mpeg2_sequence_t *seq = info->sequence;
                    umpg_gfx_init(umpg, seq->width, seq->height);
                    umpg_gfx_draw(
                        umpg, gfx, info->display_fbuf->buf[0], seq->width
                    );
                    /* frame_period * 46875000/27000000 */
                    umpg_gfx_sync(umpg, seq->frame_period * 125/72);
                    return false;
                }
                break;
            case STATE_INVALID:
                return true;
                break;
            default:
                break;
        }
    }
    return true;
#endif
}

void umpg_resize(struct umpg_t *umpg, int x, int y, uint w, uint h)
{
    umpg->x = x;
    umpg->y = y;
    umpg->w = w;
    umpg->h = h;
    if (umpg->gfx != NULL)
    {
        free(umpg->gfx);
        umpg->gfx = NULL;
    }
}
