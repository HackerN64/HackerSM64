#include <ultra64.h>
#include <PR/os_internal_error.h>
#include <stdarg.h>
#include <string.h>
#include "segments.h"

#define STACK_TRAVERSAL_LIMIT 100

struct MapEntry {
	u32 addr;
	u32 nm_offset;
	u32 nm_len;
	u32 pad;
};
extern u8 gMapStrings[];
extern struct MapEntry gMapEntries[];
extern u32 gMapEntrySize;
extern u8 _mapDataSegmentRomStart[];


// code provided by Wiseguy
static void headless_dma(u32 devAddr, void *dramAddr, u32 size)
{
    register u32 stat = IO_READ(PI_STATUS_REG);
	while (stat & (PI_STATUS_IO_BUSY | PI_STATUS_DMA_BUSY)) {
	    stat = IO_READ(PI_STATUS_REG);
	}
    IO_WRITE(PI_DRAM_ADDR_REG, K0_TO_PHYS(dramAddr));
    IO_WRITE(PI_CART_ADDR_REG, K1_TO_PHYS((u32)osRomBase | devAddr));
    IO_WRITE(PI_WR_LEN_REG, size - 1);
}
static u32 headless_pi_status(void)
{
    return IO_READ(PI_STATUS_REG);
}
// end of code provided by Wiseguy


void map_data_init(void) {
	headless_dma((u32)_mapDataSegmentRomStart, (u32*)(RAM_END - 0x100000), 0x100000);
	while (headless_pi_status() & (PI_STATUS_DMA_BUSY | PI_STATUS_ERROR));
}

char *parse_map(u32 pc) {
	u32 i;

	for (i = 0; i < gMapEntrySize; i++) {
		if (gMapEntries[i].addr >= pc) break;
	}

	if (i == gMapEntrySize - 1) {
		return NULL;
	} else {
		return (char*) ((u32)gMapStrings + gMapEntries[i - 1].nm_offset);
	}
}

extern u8 _mainSegmentStart[];
extern u8 _mainSegmentTextEnd[];
extern u8 _engineSegmentStart[];
extern u8 _engineSegmentTextEnd[];
extern u8 _goddardSegmentStart[];
extern u8 _goddardSegmentTextEnd[];

char *find_function_in_stack(u32 *sp) {
	for (int i = 0; i < STACK_TRAVERSAL_LIMIT; i++) {
		u32 val = *sp;
		val = *(u32 *)val;
		*sp += 4;

		if ((val >= (u32)_mainSegmentStart) && (val <= (u32)_mainSegmentTextEnd)) {
			return parse_map(val);
		}
		else if ((val >= (u32)_engineSegmentStart) && (val <= (u32)_engineSegmentTextEnd)) {
			return parse_map(val);
		}
		else if ((val >= (u32)_goddardSegmentStart) && (val <= (u32)_goddardSegmentTextEnd)) {
			return parse_map(val);
		}


	}
	return NULL;
}


