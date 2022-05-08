/******************************************************************************
 *        ultra_mpeg - An MPEG-1/2 decoder library for the Nintendo 64        *
 *                       Copyright (C) 2020  devwizard                        *
 *     This project is licensed under the terms of the MIT license.  See      *
 *     LICENSE for more information.                                          *
 ******************************************************************************/

#ifndef _UMPG_H_
#define _UMPG_H_

#include <ultra64.h>
#include <types.h>

extern struct umpg_t *umpg_init(
    int x, int y, uint w, uint h, const void *start, const void *end
);
extern void umpg_free(struct umpg_t *umpg);
extern uint umpg_update(struct umpg_t *umpg, Gfx **gfx);
extern void umpg_resize(struct umpg_t *umpg, int x, int y, uint w, uint h);

#endif
