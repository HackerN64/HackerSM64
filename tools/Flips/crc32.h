#pragma once
//Module name: crc32
//Author: Alcaro
//Date: June 3, 2015
//Licence: GPL v3.0 or higher

#include <stdint.h>
#include <stdlib.h>

uint32_t crc32_update(const uint8_t* data, size_t len, uint32_t crc);
static inline uint32_t crc32(const uint8_t* data, size_t len) { return crc32_update(data, len, 0); }
