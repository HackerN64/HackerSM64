#include <ultra64.h>
#include "segments.h"
#include "crash_screen.h"
#include "map_parser.h"


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
    headless_dma((uintptr_t)_mapDataSegmentRomStart, (size_t*)(RAM_END - RAM_1MB), RAM_1MB);

    while (headless_pi_status() & (PI_STATUS_DMA_BUSY | PI_STATUS_ERROR));
}

static ALWAYS_INLINE const char* map_entry_to_name(struct MapEntry* entry) {
    return (char*)((uintptr_t)gMapStrings + entry->name_offset);
}

_Bool is_in_code_segment(uintptr_t addr) {
    //! TODO: Allow reading .text memory outside RDRAM
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

    for (u32 i = 0; i < gMapEntrySize; i++) {
        if (gMapEntries[i].addr >= *addr) {
            if (gMapEntries[i].addr > *addr) {
                i--;
            }

            *addr = gMapEntries[i].addr;

            return map_entry_to_name(&gMapEntries[i]);
        }
    }

    return NULL;
}

// If 'addr' is the starting address of the function it's, returns a pointer to the function name.
const char* parse_map_exact(uintptr_t addr) {
    if (!is_in_code_segment(addr)) {
        return NULL;
    }

    for (u32 i = 0; i < gMapEntrySize; i++) {
        if (gMapEntries[i].addr == addr) {
            return map_entry_to_name(&gMapEntries[i]);
        }
    }

    return NULL;
}

//
const char* find_function_in_stack(uintptr_t* sp) {
    const char* fname = NULL;

    for (s32 i = 0; i < STACK_TRAVERSAL_LIMIT; i++) {
        uintptr_t val = *(uintptr_t*)*sp;
        *sp += sizeof(uintptr_t);

        fname = parse_map(&val);

        if (fname != NULL) {
            return fname;
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
const char* parse_map_exact(UNUSED uintptr_t addr) {
    return NULL;
}
const char* find_function_in_stack(UNUSED uintptr_t* sp) {
    return NULL;
}

_Bool is_in_same_function(UNUSED uintptr_t oldPos, UNUSED uintptr_t newPos) {
    return FALSE;
}
#endif
