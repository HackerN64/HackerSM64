/******************************************************************************
 *        ultra_mpeg - An MPEG-1/2 decoder library for the Nintendo 64        *
 *                       Copyright (C) 2020  devwizard                        *
 *     This project is licensed under the terms of the MIT license.  See      *
 *     LICENSE for more information.                                          *
 ******************************************************************************/

#include <ultra64.h>
#include <types.h>

#include "mem.h"

#define HEAP_SIG 0x4D4B

struct heap_t
{
    /* 0x00 */  u16     sig;
    /* 0x02 */  u16     free;
    /* 0x04 */  struct heap_t *prev;
    /* 0x08 */  struct heap_t *next;
    /* 0x0C */  size_t  size;
};  /* 0x10 */

static struct heap_t *mem_heap;

void *malloc(size_t size)
{
    size_t h_size;
    struct heap_t *heap;
    size = (size+0x0F) & ~0x0F;
    h_size = size + sizeof(struct heap_t);
    heap = mem_heap;
    while (heap != NULL)
    {
        if (heap->sig == HEAP_SIG && heap->free && heap->size >= h_size)
        {
            void *ptr = heap+1;
            struct heap_t *next = (struct heap_t *)((u8 *)ptr + size);
            next->prev = heap;
            next->next = heap->next;
            next->size = heap->size - h_size;
            next->sig  = HEAP_SIG;
            next->free = true;
            heap->next = next;
            heap->size = size;
            heap->free = false;
            return heap+1;
        }
        heap = heap->next;
    }
    return NULL;
}

void free(void *ptr)
{
    struct heap_t *heap = ((struct heap_t *)ptr) - 1;
    if (heap->sig == HEAP_SIG)
    {
        struct heap_t *next;
        struct heap_t *prev;
        heap->free = true;
        next = (struct heap_t *)((u8 *)(heap+1) + heap->size);
        if (heap->next == next && next->free)
        {
            heap->next = next->next;
            heap->size += sizeof(*next) + next->size;
        }
        prev = heap->prev;
        if (prev != NULL)
        {
            next = (struct heap_t *)((u8 *)(prev+1) + prev->size);
            if (heap == next && prev->free)
            {
                prev->next = heap->next;
                prev->size += sizeof(*heap) + heap->size;
            }
        }
    }
}

void mem_init(void)
{
    mem_heap = NULL;
}

void mem_link(void *start, void *end)
{
    struct heap_t **next = &mem_heap;
    struct heap_t *prev;
    struct heap_t *heap;
    while (true)
    {
        prev = *next;
        if (prev == NULL)
        {
            break;
        }
        next = &prev->next;
    }
    *next = heap = start;
    heap->prev = prev;
    heap->next = NULL;
    heap->size = (u8 *)end-(u8 *)start - sizeof(*heap);
    heap->sig  = HEAP_SIG;
    heap->free = true;
}
