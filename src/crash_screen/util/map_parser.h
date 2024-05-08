#pragma once

#include <ultra64.h>

#include "types.h"


#ifdef INCLUDE_DEBUG_MAP
 #define IS_DEBUG_MAP_ENABLED() (gNumMapSymbols > 0)
#else // !INCLUDE_DEBUG_MAP
 #define IS_DEBUG_MAP_ENABLED() FALSE
#endif // !INCLUDE_DEBUG_MAP


typedef s32 MapSymbolIndex;


typedef enum SymbolSearchDirections {
    SYMBOL_SEARCH_FORWARD,  // Use this to get the earlier symbol on an overlap.
    SYMBOL_SEARCH_BACKWARD, // Use this to get the later symbol on an overlap.
    SYMBOL_SEARCH_BINARY,   // Use this when speed matters more than getting a specific symbol on an overlap.
} SymbolSearchDirections;

// See mapPacker.py.
typedef struct MapSymbol {
    /*0x00*/ union {
                Address addr;          // Symbol address.
                struct PACKED {
                    /*0x00*/ Address segment :  8; // 0x80 or higher = global symbol.
                    /*0x01*/ Address address : 24;
                }; // Segmented address
            };
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

#define MEMORY_REGION(_start, _end) {   \
    .start = (const Address)(_start),   \
    .end   = (const Address)(_end),     \
},

#define TEXT_REGION(_name) \
    MEMORY_REGION(_##_name##SegmentTextStart, _##_name##SegmentTextEnd)

#define TEXT_REGION_SEGMENT(_name) \
    TEXT_REGION(_name)

#define TEXT_REGION_GROUP(_name) \
    TEXT_REGION(_name##_geo)

#define TEXT_REGION_LEVEL(_name) \
    TEXT_REGION(_name) \
    TEXT_REGION(_name##_geo)


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
const MapSymbol* get_map_symbol(Address addr, SymbolSearchDirections searchDirection);
