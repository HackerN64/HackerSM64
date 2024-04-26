#pragma once

#include <ultra64.h>

#include "types.h"


#ifdef INCLUDE_DEBUG_MAP
 #define IS_DEBUG_MAP_ENABLED() (gNumMapSymbols > 0)
#else // !INCLUDE_DEBUG_MAP
 #define IS_DEBUG_MAP_ENABLED() FALSE
#endif // !INCLUDE_DEBUG_MAP


typedef s32 MapSymbolIndex;


enum SymbolSearchDirections {
    SYMBOL_SEARCH_FORWARD,  // Use this to get the earlier symbol on an overlap.
    SYMBOL_SEARCH_BACKWARD, // Use this to get the later symbol on an overlap.
    SYMBOL_SEARCH_BINARY,   // Use this when speed matters more than getting a specific symbol on an overlap.
};

// See mapPacker.py.
typedef struct MapSymbol {
    /*0x00*/ Address addr;          // Symbol address.
    /*0x04*/ size_t size;           // Symbol size.
    /*0x08*/ size_t name_offset;    // Offset of symbol name in gMapStrings.
    /*0x0C*/ u16 name_len;          // Symbol name length.
    /*0x0D*/ uchar type;            // Symbol type.
    /*0x0E*/ uchar errc;            // Error char: 'S' = unknown size.
} MapSymbol; /*0x10*/

typedef struct AddressPair {
    /*0x00*/ const Address start;
    /*0x04*/ const Address end;
} AddressPair; /*0x08*/

#define EXTERN_TEXT_SYMBOL(_name, _side) \
extern const Byte _##_name##SegmentText##_side[];

#define EXTERN_TEXT_REGION(_name) \
EXTERN_TEXT_SYMBOL(_name, Start) \
EXTERN_TEXT_SYMBOL(_name, End)


#define EXTERN_SEGMENT_TEXT(_name) \
EXTERN_TEXT_REGION(_name)

#define EXTERN_GROUP_TEXT(_name) \
EXTERN_TEXT_REGION(_name##_geo)

#define EXTERN_LEVEL_TEXT(_name) \
EXTERN_TEXT_REGION(_name##script) \
EXTERN_TEXT_REGION(_name##geo)

EXTERN_SEGMENT_TEXT(boot)
EXTERN_SEGMENT_TEXT(main)
EXTERN_SEGMENT_TEXT(engine)
EXTERN_SEGMENT_TEXT(crashscreen)
EXTERN_SEGMENT_TEXT(behavior)
EXTERN_SEGMENT_TEXT(goddard)
#ifdef KEEP_MARIO_HEAD
EXTERN_SEGMENT_TEXT(libgoddard)
#endif // KEEP_MARIO_HEAD
EXTERN_SEGMENT_TEXT(intro)

EXTERN_GROUP_TEXT(group0)
EXTERN_GROUP_TEXT(group1)
EXTERN_GROUP_TEXT(group2)
EXTERN_GROUP_TEXT(group3)
EXTERN_GROUP_TEXT(group4)
EXTERN_GROUP_TEXT(group5)
EXTERN_GROUP_TEXT(group6)
EXTERN_GROUP_TEXT(group7)
EXTERN_GROUP_TEXT(group8)
EXTERN_GROUP_TEXT(group9)
EXTERN_GROUP_TEXT(group10)
EXTERN_GROUP_TEXT(group11)
EXTERN_GROUP_TEXT(group12)
EXTERN_GROUP_TEXT(group13)
EXTERN_GROUP_TEXT(group14)
EXTERN_GROUP_TEXT(group15)
EXTERN_GROUP_TEXT(group16)
EXTERN_GROUP_TEXT(group17)
EXTERN_GROUP_TEXT(common0)
EXTERN_GROUP_TEXT(common1)

#define STUB_LEVEL(_0, _1, _2, _3, _4, _5, _6, _7, _8)
#define DEFINE_LEVEL(_0, _1, _2, folder, _4, _5, _6, _7, _8, _9, _10) EXTERN_LEVEL_TEXT(folder)
#include "levels/level_defines.h"
#undef STUB_LEVEL
#undef DEFINE_LEVEL

#define MEMORY_REGION(_start, _end) \
    { .start = (const Address)(_start), .end = (const Address)(_end) },

#define TEXT_REGION(_name) \
    MEMORY_REGION(_##_name##SegmentTextStart, _##_name##SegmentTextEnd)

#define TEXT_REGION_SEGMENT(_name) \
    TEXT_REGION(_name)

#define TEXT_REGION_GROUP(_name) \
    TEXT_REGION(_name##_geo)

#define TEXT_REGION_LEVEL(_name) \
    TEXT_REGION(_name##script) \
    TEXT_REGION(_name##geo)


extern const MapSymbol gMapSymbols[];
extern const MapSymbol gMapSymbolsEnd[];
extern const Byte gMapStrings[];
extern const Byte gMapStringEnd[];


extern MapSymbolIndex gNumMapSymbols;


void map_data_init(void);
_Bool addr_is_in_text_segment(Address addr);
_Bool symbol_is_function(const MapSymbol* symbol);
const char* get_map_symbol_name(const MapSymbol* symbol);
_Bool addr_is_in_symbol(Address addr, const MapSymbol* symbol);
MapSymbolIndex get_symbol_index_from_addr_forward(Address addr);
MapSymbolIndex get_symbol_index_from_addr_backward(Address addr);
MapSymbolIndex get_symbol_index_from_addr_binary(Address addr);
const MapSymbol* get_map_symbol(Address addr, enum SymbolSearchDirections searchDirection);
