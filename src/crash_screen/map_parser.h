#pragma once

#include <ultra64.h>

#include "types.h"


struct MapEntry {
    /*0x00*/ uintptr_t addr;
    /*0x04*/ uintptr_t name_offset;
    /*0x08*/ size_t name_len;
    /*0x0C*/ size_t pad;
}; /*0x10*/

typedef struct {
    /*0x00*/ const uintptr_t start;
    /*0x04*/ const uintptr_t end;
} TextRegion; /*0x08*/

#define EXTERN_TEXT_SYMBOL(name, side) \
extern const u8 _##name##SegmentText##side[];

#define EXTERN_TEXT_REGION(name) \
EXTERN_TEXT_SYMBOL(name, Start) \
EXTERN_TEXT_SYMBOL(name, End)


#define EXTERN_SEGMENT_TEXT(name) \
EXTERN_TEXT_REGION(name)

#define EXTERN_GROUP_TEXT(name) \
EXTERN_TEXT_REGION(name##_geo)

#define EXTERN_LEVEL_TEXT(name) \
EXTERN_TEXT_REGION(name##script) \
EXTERN_TEXT_REGION(name##geo)

EXTERN_SEGMENT_TEXT(boot)
EXTERN_SEGMENT_TEXT(main)
EXTERN_SEGMENT_TEXT(engine)
EXTERN_SEGMENT_TEXT(behavior)
EXTERN_SEGMENT_TEXT(goddard)
#ifdef KEEP_MARIO_HEAD
EXTERN_SEGMENT_TEXT(libgoddard)
#endif
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

#define TEXT_REGION(name) \
    { (const uintptr_t)_##name##SegmentTextStart, (const uintptr_t)_##name##SegmentTextEnd },

#define TEXT_REGION_SEGMENT(name) \
    TEXT_REGION(name)

#define TEXT_REGION_GROUP(name) \
    TEXT_REGION(name##_geo)

#define TEXT_REGION_LEVEL(name) \
    TEXT_REGION(name##script) \
    TEXT_REGION(name##geo)


#define IS_IN_RDRAM(addr) (((addr) >= VALID_RAM_START) && ((addr) < VALID_RAM_END))


extern const struct MapEntry gMapEntries[];
extern const struct MapEntry gMapEntryEnd[];
extern const u8 gMapStrings[];
extern const u8 gMapStringEnd[];
extern const u8 _mapDataSegmentRomStart[];


void map_data_init(void);
_Bool is_in_code_segment(uintptr_t addr);
const char* parse_map(uintptr_t* addr);
_Bool is_in_same_function(uintptr_t oldPos, uintptr_t newPos);
