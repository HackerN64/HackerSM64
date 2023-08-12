#pragma once

#include <ultra64.h>

#include "types.h"


// Virtual RAM boundary defines.
#define VIRTUAL_RAM_START (Address)RAM_START
#define VIRTUAL_RAM_END   (Address)0xFFFFFFFF
#define VIRTUAL_RAM_SIZE  (size_t)(VIRTUAL_RAM_END - VIRTUAL_RAM_START)


void headless_dma(Address devAddr, void* dramAddr, size_t size);
_Bool try_read_data(Word* dest, Address addr);
_Bool try_read_byte(Byte* dest, Address addr);
_Bool try_read_halfword(Halfword* dest, Address addr);
_Bool try_read_word(Word* dest, Address addr);
_Bool try_read_doubleword(Doubleword* dest, Address addr);
