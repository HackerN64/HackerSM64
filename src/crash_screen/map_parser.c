#include <ultra64.h>

#include "memory_read.h"

#include "map_parser.h"

#include "segments.h"


//! TODO: Use new map symbol data to get this info?

ALIGNED8 static const MemoryRegion sTextRegions[] = {
TEXT_REGION_SEGMENT(boot)
TEXT_REGION_SEGMENT(main)
TEXT_REGION_SEGMENT(engine)
TEXT_REGION_SEGMENT(behavior)
TEXT_REGION_SEGMENT(goddard)
#ifdef KEEP_MARIO_HEAD
TEXT_REGION_SEGMENT(libgoddard)
#endif
TEXT_REGION_SEGMENT(intro)

TEXT_REGION_GROUP(group0)
TEXT_REGION_GROUP(group1)
TEXT_REGION_GROUP(group2)
TEXT_REGION_GROUP(group3)
TEXT_REGION_GROUP(group4)
TEXT_REGION_GROUP(group5)
TEXT_REGION_GROUP(group6)
TEXT_REGION_GROUP(group7)
TEXT_REGION_GROUP(group8)
TEXT_REGION_GROUP(group9)
TEXT_REGION_GROUP(group10)
TEXT_REGION_GROUP(group11)
TEXT_REGION_GROUP(group12)
TEXT_REGION_GROUP(group13)
TEXT_REGION_GROUP(group14)
TEXT_REGION_GROUP(group15)
TEXT_REGION_GROUP(group16)
TEXT_REGION_GROUP(group17)
TEXT_REGION_GROUP(common0)
TEXT_REGION_GROUP(common1)

#define STUB_LEVEL(_0, _1, _2, _3, _4, _5, _6, _7, _8)
#define DEFINE_LEVEL(_0, _1, _2, folder, _4, _5, _6, _7, _8, _9, _10) TEXT_REGION_LEVEL(folder)
#include "levels/level_defines.h"
#undef STUB_LEVEL
#undef DEFINE_LEVEL
};


size_t gNumMapSymbols = 0;


void map_data_init(void) {
    gNumMapSymbols = (gMapSymbolsEnd - gMapSymbols);

    Address start = (Address)_mapDataSegmentRomStart;
    Address end   = (Address)_mapDataSegmentRomEnd;

    headless_dma(start, (size_t*)(RAM_END - RAM_1MB), (end - start));
}

// Check whether the address is in a .text segment.
_Bool is_in_code_segment(Address addr) {
    for (int i = 0; i < ARRAY_COUNT(sTextRegions); i++) {
        if (addr >= sTextRegions[i].start && addr < sTextRegions[i].end) {
            return TRUE;
        }
    }

    return FALSE;
}

const char* get_map_symbol_name(const struct MapSymbol* symbol) {
#ifndef INCLUDE_DEBUG_MAP
    return NULL;
#endif
    if (symbol == NULL) {
        return NULL;
    }

    return (const char*)((u32)gMapStrings + symbol->name_offset);
}

static _Bool check_addr_in_symbol(Address addr, const struct MapSymbol* symbol) {
    return ((symbol != NULL) && (addr >= symbol->addr) && (addr < (symbol->addr + symbol->size)));
}

s32 get_symbol_index_from_addr_forward(Address addr) {
#ifndef INCLUDE_DEBUG_MAP
    return -1;
#endif
    const struct MapSymbol* symbol = &gMapSymbols[0];

    for (size_t i = 0; i < gNumMapSymbols; i++) {
        if (check_addr_in_symbol(addr, symbol)) {
            return i;
        }

        symbol++;
    }

    return -1;
}

s32 get_symbol_index_from_addr_backward(Address addr) {
#ifndef INCLUDE_DEBUG_MAP
    return -1;
#endif
    const struct MapSymbol* symbol = &gMapSymbols[gNumMapSymbols - 1];

    for (size_t i = gNumMapSymbols; i-- > 0;) {
        if (check_addr_in_symbol(addr, symbol)) {
            return i;
        }

        symbol--;
    }

    return -1;
}

// Changes 'addr' to the starting address of the function it's in and returns a pointer to the function name.
const struct MapSymbol* get_map_symbol(Address addr, enum SymbolSearchDirections searchDirection) {
#ifndef INCLUDE_DEBUG_MAP
    return NULL;
#endif
    Word data = 0;
    if (!try_read_data(&data, addr)) {
        return NULL;
    }

    s32 index = -1;
    switch (searchDirection) {
        case SYMBOL_SEARCH_FORWARD:  index = get_symbol_index_from_addr_forward(addr);  break;
        case SYMBOL_SEARCH_BACKWARD: index = get_symbol_index_from_addr_backward(addr); break;
    }

    if (index != -1) {
        return &gMapSymbols[index];
    }

    return NULL;
}
