#pragma once

#include <ultra64.h>

#include "types.h"

void headless_dma(Address devAddr, void* dramAddr, size_t size);
_Bool read_data(Word* dest, Address addr);
_Bool read_unaligned_byte(Byte* dest, Address addr);
