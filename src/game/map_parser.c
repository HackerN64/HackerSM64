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
    IO_WRITE(PI_WR_LEN_REG, size - 1);
}

static u32 headless_pi_status(void) {
    return IO_READ(PI_STATUS_REG);
}

void map_data_init(void) {
    headless_dma((uintptr_t)_mapDataSegmentRomStart, (size_t*)(RAM_END - 0x100000), 0x100000);
    while (headless_pi_status() & (PI_STATUS_DMA_BUSY | PI_STATUS_ERROR));
}

char *parse_map(uintptr_t pc) {
    u32 i;

    for (i = 0; i < gMapEntrySize; i++) {
        if (gMapEntries[i].addr >= pc) {
            break;
        }
    }

    if (i == gMapEntrySize - 1) {
        return NULL;
    } else {
        return (char*) ((uintptr_t)gMapStrings + gMapEntries[i - 1].nm_offset);
    }
}

extern u8 _mainSegmentStart[];
extern u8 _mainSegmentTextEnd[];
extern u8 _engineSegmentStart[];
extern u8 _engineSegmentTextEnd[];
extern u8 _goddardSegmentStart[];
extern u8 _goddardSegmentTextEnd[];

char *find_function_in_stack(uintptr_t **sp) {
    for (int i = 0; i < STACK_TRAVERSAL_LIMIT; i++) {
        uintptr_t val = *(*sp)++;

        if (((val >= (uintptr_t)_mainSegmentStart   ) && (val <= (uintptr_t)_mainSegmentTextEnd   ))
         || ((val >= (uintptr_t)_engineSegmentStart ) && (val <= (uintptr_t)_engineSegmentTextEnd ))
         || ((val >= (uintptr_t)_goddardSegmentStart) && (val <= (uintptr_t)_goddardSegmentTextEnd))) {
            return parse_map(val);
        }
    }

    return NULL;
}
