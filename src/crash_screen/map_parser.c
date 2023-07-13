#include <ultra64.h>
#include "segments.h"
#include "crash_screen.h"
#include "map_parser.h"


//! TODO: replace this functionality with nm -S mappings


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


size_t gNumMapEntries = 0;


void map_data_init(void) {
    gNumMapEntries = (gMapEntryEnd - gMapEntries);

    Address start = (Address)_mapDataSegmentRomStart;
    Address end   = (Address)_mapDataSegmentRomEnd;

    headless_dma(start, (size_t*)(RAM_END - RAM_1MB), (end - start));
}

// Check whether the address is in a .text segment.
//! TODO: Only use entry_is_text.
_Bool is_in_code_segment(Address addr) {
    for (int i = 0; i < ARRAY_COUNT(sTextRegions); i++) {
        if (addr >= sTextRegions[i].start && addr < sTextRegions[i].end) {
            return TRUE;
        }
    }

    return FALSE;
}

// Check whether a map entry is marked as .text.
_Bool entry_is_text(const struct MapEntry* entry) {
    return (entry->type == 'T' || entry->type == 't');
}

const char* get_map_entry_name(const struct MapEntry* entry) {
    return (const char*)((u32)gMapStrings + entry->name_offset);
}

s32 get_map_entry_index(Address addr) {
    const struct MapEntry* entry = &gMapEntries[0];

    for (size_t i = 0; i < gNumMapEntries; i++) {
        if ((addr >= entry->addr) && (addr < (entry->addr + entry->size))) {
            return i;
        }

        entry++;
    }

    return -1;
}

// Changes 'addr' to the starting address of the function it's in and returns a pointer to the function name.
const char* parse_map(Address* addr) {
#ifndef INCLUDE_DEBUG_MAP
    return NULL;
#endif
    *addr = ALIGNFLOOR(*addr, sizeof(Word));

    Word data = 0;
    if (!try_read_data(&data, *addr)) {
        return NULL;
    }

    s32 index = get_map_entry_index(*addr);

    if (index != -1) {
        const struct MapEntry* entry = &gMapEntries[index];
        *addr = entry->addr;
        return get_map_entry_name(entry);
    }

    return NULL;
}
