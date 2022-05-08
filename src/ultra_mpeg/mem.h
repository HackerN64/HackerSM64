/******************************************************************************
 *        ultra_mpeg - An MPEG-1/2 decoder library for the Nintendo 64        *
 *                       Copyright (C) 2020  devwizard                        *
 *     This project is licensed under the terms of the MIT license.  See      *
 *     LICENSE for more information.                                          *
 ******************************************************************************/

#ifndef _MEM_H_
#define _MEM_H_

#include <ultra64.h>
#include <types.h>

extern void mem_init(void);
extern void mem_link(void *start, void *end);

#endif /* _MEM_H_ */
