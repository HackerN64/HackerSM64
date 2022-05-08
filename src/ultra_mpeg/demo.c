/******************************************************************************
 *        ultra_mpeg - An MPEG-1/2 decoder library for the Nintendo 64        *
 *                       Copyright (C) 2020  devwizard                        *
 *     This project is licensed under the terms of the MIT license.  See      *
 *     LICENSE for more information.                                          *
 ******************************************************************************/

#include <ultra64.h>
#include <types.h>
#include <sm64.h>

#include "mem.h"
#include "umpg.h"

extern u8 heap_start[];

extern u8 mpg_demo_start[];
extern u8 mpg_demo_end[];

struct umpg_t *demo_init(void)
{
    mem_init();
    mem_link(heap_start,         (void *)0x801C1000);
    mem_link((void *)0x80367460, (void *)0x80378800);
    mem_link((void *)0x80400000, (void *)0x80800000);
    return umpg_init(4*0, 4*8, 4*320, 4*224, mpg_demo_start, mpg_demo_end);
}

void demo_update(struct umpg_t *umpg)
{
    video_gfx_cimg();
    umpg_update(umpg, &video_gfx);
    video_gfx_end();
}
