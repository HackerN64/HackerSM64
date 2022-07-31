#include <ultra64.h>
#include <PR/os_internal_error.h>
#include <stdarg.h>
#include <string.h>
#include "segments.h"

#define STACK_TRAVERSAL_LIMIT 100

struct MapEntry {
    uintptr_t addr;
    size_t nm_offset;
    size_t nm_len;
    size_t pad;
};

extern u8 gMapStrings[];
extern struct MapEntry gMapEntries[];
extern size_t gMapEntrySize;
extern u8 _mapDataSegmentRomStart[];

static void headless_dma(uintptr_t devAddr, void *dramAddr, size_t size) {
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

extern u8 _mainSegmentStart[];
extern u8 _mainSegmentTextEnd[];
extern u8 _engineSegmentStart[];
extern u8 _engineSegmentTextEnd[];
extern u8 _goddardSegmentStart[];
extern u8 _goddardSegmentTextEnd[];

#define IS_IN_SEGMENT(addr, segment) ((addr >= (uintptr_t)_##segment##SegmentStart) && (addr <= (uintptr_t)_##segment##SegmentTextEnd))

s32 is_in_code_segment(uintptr_t addr) {
    return (IS_IN_SEGMENT(addr, main)
         || IS_IN_SEGMENT(addr, engine)
         || IS_IN_SEGMENT(addr, goddard));
}

char *parse_map(uintptr_t addr) {
    if (is_in_code_segment(addr)) {
        for (u32 i = 0; i < gMapEntrySize; i++) {
            if (gMapEntries[i].addr >= addr) {
                return (char*) ((uintptr_t)gMapStrings + gMapEntries[(gMapEntries[i].addr == addr) ? i : (i - 1)].nm_offset);
            }
        }
    }

    return NULL;
}

char *parse_map_exact(uintptr_t addr) {
    if (is_in_code_segment(addr)) {
        for (u32 i = 0; i < gMapEntrySize; i++) {
            if (gMapEntries[i].addr == addr) {
                return (char*) ((uintptr_t)gMapStrings + gMapEntries[i].nm_offset);
            }
        }
    }

    return NULL;
}

char *find_function_in_stack(uintptr_t **sp) {
    for (s32 i = 0; i < STACK_TRAVERSAL_LIMIT; i++) {
        uintptr_t val = *(*sp)++;

        if (is_in_code_segment(val)) {
            return parse_map(val);
        }
    }

    return NULL;
}
