#include <ultra64.h>
#include "segments.h"
#include "crash_screen.h"
#include "map_parser.h"


//! TODO: replace this with nm -S mappings
static const TextRegion sTextRegions[] = {
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

static size_t sNumMapEntries = 0;


static void headless_dma(uintptr_t devAddr, void* dramAddr, size_t size) {
    u32 stat = IO_READ(PI_STATUS_REG);

    while (stat & (PI_STATUS_IO_BUSY | PI_STATUS_DMA_BUSY)) {
        stat = IO_READ(PI_STATUS_REG);
    }

    IO_WRITE(PI_DRAM_ADDR_REG, K0_TO_PHYS(dramAddr));
    IO_WRITE(PI_CART_ADDR_REG, K1_TO_PHYS((uintptr_t)osRomBase | devAddr));
    IO_WRITE(PI_WR_LEN_REG, (size - 1));
}

static u32 headless_pi_status(void) {
    return IO_READ(PI_STATUS_REG);
}

void map_data_init(void) {
    sNumMapEntries = (gMapEntryEnd - gMapEntries);

    headless_dma((uintptr_t)_mapDataSegmentRomStart, (size_t*)(RAM_END - RAM_1MB), RAM_1MB);

    while (headless_pi_status() & (PI_STATUS_DMA_BUSY | PI_STATUS_ERROR));
}

// Check whether the address is in a .text segment.
//! TODO: do INCLUDE_DEBUG_MAP inside this instead of on this whole file.
_Bool is_in_code_segment(uintptr_t addr) {
    //! TODO: Allow reading .text memory outside 0x80000000-0x80800000.
    if (!IS_IN_RDRAM(addr)) {
        return FALSE;
    }

    for (int i = 0; i < ARRAY_COUNT(sTextRegions); i++) {
        if (addr >= sTextRegions[i].start && addr <= sTextRegions[i].end) {
            return TRUE;
        }
    }

    return FALSE;
}

#ifdef INCLUDE_DEBUG_MAP
// Changes 'addr' to the starting address of the function it's in and returns a pointer to the function name.
const char* parse_map(uintptr_t* addr) {
    if (!is_in_code_segment(*addr)) {
        return NULL;
    }

    // The pointer starts at the end of the map entry data.
    const struct MapEntry* entry = &gMapEntries[sNumMapEntries];

    // Loop backwards through the map entries until a map entry address is earlier than the given address.
    while (entry > gMapEntries) {
        entry--;
        if (*addr >= entry->addr) {
            *addr = entry->addr;
            return (char*)((uintptr_t)gMapStrings + entry->name_offset);
        }
    }

    return NULL;
}

// Check whether two addresses share the same function.
_Bool is_in_same_function(uintptr_t oldPos, uintptr_t newPos) {
    if (oldPos == newPos) {
        return TRUE;
    }

    oldPos &= ~(sizeof(uintptr_t) - 1); // ALIGN4
    newPos &= ~(sizeof(uintptr_t) - 1); // ALIGN4

    if (oldPos == newPos) {
        return TRUE;
    }

    parse_map(&oldPos);
    parse_map(&newPos);

    return (oldPos == newPos);
}
#else
const char* parse_map(UNUSED uintptr_t* addr) {
    return NULL;
}

_Bool is_in_same_function(UNUSED uintptr_t oldPos, UNUSED uintptr_t newPos) {
    return FALSE;
}
#endif
