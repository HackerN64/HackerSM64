#pragma once

#include <ultra64.h>

#include "types.h"


#define STACK_TRAVERSAL_LIMIT 100


struct MapEntry {
    /*0x00*/ uintptr_t addr;
    /*0x04*/ uintptr_t name_offset;
    /*0x08*/ size_t name_len;
    /*0x0C*/ size_t pad;
}; /*0x10*/

typedef struct {
    /*0x00*/ uintptr_t start;
    /*0x04*/ uintptr_t end;
} TextRegion; /*0x08*/


#define EXTERN_SEGMENT_TEXT(name) \
extern u8 _##name##SegmentTextStart[]; \
extern u8 _##name##SegmentTextEnd[];

#define EXTERN_GROUP_TEXT(name) \
extern u8 _##name##_geoSegmentTextStart[]; \
extern u8 _##name##_geoSegmentTextEnd[];

#define EXTERN_LEVEL_TEXT(name) \
extern u8 _##name##scriptSegmentTextStart[]; \
extern u8 _##name##scriptSegmentTextEnd[]; \
extern u8 _##name##geoSegmentTextStart[]; \
extern u8 _##name##geoSegmentTextEnd[];

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

#define TEXT_REGION_SEGMENT(name) \
    { (uintptr_t)_##name##SegmentTextStart, (uintptr_t)_##name##SegmentTextEnd },

#define TEXT_REGION_GROUP(name) \
    { (uintptr_t)_##name##_geoSegmentTextStart, (uintptr_t)_##name##_geoSegmentTextEnd },

#define TEXT_REGION_LEVEL(name) \
    { (uintptr_t)_##name##scriptSegmentTextStart, (uintptr_t)_##name##scriptSegmentTextEnd }, \
    { (uintptr_t)_##name##geoSegmentTextStart,    (uintptr_t)_##name##geoSegmentTextEnd    },


#define IS_IN_RDRAM(addr)   (((addr) >= RAM_START) && ((addr) < RAM_END))


void map_data_init(void);
_Bool is_in_code_segment(uintptr_t addr);
const char* parse_map(uintptr_t* addr);
const char* parse_map_exact(uintptr_t addr);
const char* find_function_in_stack(uintptr_t* sp);
_Bool is_in_same_function(uintptr_t oldPos, uintptr_t newPos);
