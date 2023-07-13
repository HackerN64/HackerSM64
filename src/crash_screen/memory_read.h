#pragma once

#include <ultra64.h>

#include "types.h"

void headless_dma(Address devAddr, void* dramAddr, size_t size);
_Bool try_read_data(Word* dest, Address addr);
_Bool try_read_byte(Byte* dest, Address addr);
_Bool try_read_halfword(Halfword* dest, Address addr);
_Bool try_read_word(Word* dest, Address addr);
_Bool try_read_doubleword(Doubleword* dest, Address addr);
