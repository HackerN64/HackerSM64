#include <ultra64.h>

#include "memory_read.h"

#include "segments.h"


#define MEMORY_REGION_MAPPED(_addr, _name, _ro, _wo, _align) {  \
    .addr = _addr,                                              \
    .name = _name,                                              \
    .flags = {                                                  \
        .ro = FALSE,                                            \
        .wo = FALSE,                                            \
        .align = FALSE,                                         \
        .mapped = TRUE,                                         \
    },                                                          \
}
#define MEMORY_REGION_UNMAPPED(_addr) { \
    .addr = _addr,                      \
    .name = "",                         \
    .flags = {                          \
        .ro = FALSE,                    \
        .wo = FALSE,                    \
        .align = FALSE,                 \
        .mapped = FALSE,                \
    },                                  \
}
#define MEMORY_REGION_END() MEMORY_REGION_UNMAPPED(0xFFFFFFFF)

// https://n64brew.dev/wiki/Memory_map
const MemoryRegion sKernelSegments[K_END + 1] = {
    [KUSEG                          ] = MEMORY_REGION_MAPPED(0x00000000,                    "KUSEG",                    FALSE, FALSE, FALSE), // [0x00000000 - 0x7FFFFFFF] User segment, TLB mapped
    [KSEG0                          ] = MEMORY_REGION_MAPPED(VIRTUAL_RAM_START,             "KSEG0",                    FALSE, FALSE, FALSE), // [0x80000000 - 0x9FFFFFFF] Kernel segment 0, directly mapped, cached
    [KSEG1                          ] = MEMORY_REGION_MAPPED(0xA0000000,                    "KSEG1",                    FALSE, FALSE, FALSE), // [0xA0000000 - 0xBFFFFFFF] Kernel segment 1, directly mapped, uncached
    [KSSEG                          ] = MEMORY_REGION_MAPPED(0xC0000000,                    "KSSEG",                    FALSE, FALSE, FALSE), // [0xC0000000 - 0xDFFFFFFF] Kernel supervisor segment, TLB mapped
    [KSEG3                          ] = MEMORY_REGION_MAPPED(0xE0000000,                    "KSEG3",                    FALSE, FALSE, FALSE), // [0xE0000000 - 0xFFFFFFFF] Kernel segment 3, TLB mapped
    [K_END                          ] = MEMORY_REGION_END(),
};
const MemoryRegion sBusDevices[BUS_END + 1] = {
    [BUS_RDRAM                      ] = MEMORY_REGION_MAPPED(0x00000000,                    "RDRAM",                    FALSE, FALSE, FALSE), // [0x00000000 - 0x03FFFFFF]
    [BUS_RCP                        ] = MEMORY_REGION_MAPPED(SP_DMEM_START,                 "RCP",                      FALSE, FALSE, FALSE), // [0x04000000 - 0x04FFFFFF]
    [BUS_PI_EXT_1                   ] = MEMORY_REGION_MAPPED(PI_DOM2_ADDR1,                 "PI",                       FALSE, FALSE, FALSE), // [0x05000000 - 0x1FBFFFFF]
    [BUS_SI_EXT                     ] = MEMORY_REGION_MAPPED(PIF_ROM_START,                 "SI",                       FALSE, FALSE, FALSE), // [0x1FC00000 - 0x1FCFFFFF]
    [BUS_PI_EXT_2                   ] = MEMORY_REGION_MAPPED(PI_DOM1_ADDR3,                 "PI",                       FALSE, FALSE, FALSE), // [0x1FD00000 - 0x7FFFFFFF]
    [BUS_UNMAPPED                   ] = MEMORY_REGION_UNMAPPED(VIRTUAL_RAM_START),                                                            // [0x80000000 - 0xFFFFFFFF]
    [BUS_END                        ] = MEMORY_REGION_END(),
};
const MemoryRegion sMemoryRegions[MEM_MEMORY_REGIONS_END + 1] = {
    // RDRAM:
    [MEM_RDRAM_MEMORY               ] = MEMORY_REGION_MAPPED(0x00000000,                    "Memory",                   FALSE, FALSE, FALSE), // [0x00000000 - 0x03EFFFFF] (RDRAM)
    [MEM_RDRAM_REGISTERS            ] = MEMORY_REGION_MAPPED(RDRAM_BASE_REG,                "Registers",                FALSE, FALSE, FALSE), // [0x03F00000 - 0x03F7FFFF] (RDRAM)
    [MEM_RDRAM_REGISTERS_BROADCAST  ] = MEMORY_REGION_MAPPED((RDRAM_BASE_REG + 0x80000),    "Registers (broadcast)",    FALSE, TRUE,  FALSE), // [0x03F80000 - 0x03FFFFFF] (RDRAM)
    // RCP:
    [MEM_RCP_RSP_DMEM               ] = MEMORY_REGION_MAPPED(SP_DMEM_START,                 "DMEM",                     FALSE, FALSE, FALSE), // [0x04000000 - 0x04000FFF] (RCP)
    [MEM_RCP_RSP_IMEM               ] = MEMORY_REGION_MAPPED(SP_IMEM_START,                 "IMEM",                     FALSE, FALSE, FALSE), // [0x04001000 - 0x04001FFF] (RCP)
    [MEM_RCP_RSP_DMEM_IMEM_MIRRORS  ] = MEMORY_REGION_MAPPED((SP_IMEM_END + 1),             "DMEM IMEM Mirrors",        FALSE, FALSE, FALSE), // [0x04002000 - 0x0403FFFF] (RCP)
    [MEM_RCP_RSP_REGISTERS          ] = MEMORY_REGION_MAPPED(SP_BASE_REG,                   "Registers",                FALSE, FALSE, FALSE), // [0x04040000 - 0x040BFFFF] (RCP)
    [MEM_RCP_UNMAPPED_1             ] = MEMORY_REGION_UNMAPPED(SP_BASE_REG + 0x80000),                                                        // [0x040C0000 - 0x040FFFFF] (RCP)
    [MEM_RCP_RDP_COMMAND_REGISTERS  ] = MEMORY_REGION_MAPPED(DPC_BASE_REG,                  "Command Registers",        FALSE, FALSE, FALSE), // [0x04100000 - 0x041FFFFF] (RCP)
    [MEM_RCP_RDP_SPAN_REGISTERS     ] = MEMORY_REGION_MAPPED(DPS_BASE_REG,                  "Span Registers",           FALSE, FALSE, FALSE), // [0x04200000 - 0x042FFFFF] (RCP)
    [MEM_RCP_MI                     ] = MEMORY_REGION_MAPPED(MI_BASE_REG,                   "MIPS Interface",           FALSE, FALSE, FALSE), // [0x04300000 - 0x043FFFFF] (RCP)
    [MEM_RCP_VI                     ] = MEMORY_REGION_MAPPED(VI_BASE_REG,                   "Video Interface",          FALSE, FALSE, FALSE), // [0x04400000 - 0x044FFFFF] (RCP)
    [MEM_RCP_AI                     ] = MEMORY_REGION_MAPPED(AI_BASE_REG,                   "Audio Interface",          FALSE, FALSE, FALSE), // [0x04500000 - 0x045FFFFF] (RCP)
    [MEM_RCP_PI                     ] = MEMORY_REGION_MAPPED(PI_BASE_REG,                   "Peripheral Interface",     FALSE, FALSE, FALSE), // [0x04600000 - 0x046FFFFF] (RCP)
    [MEM_RCP_RI                     ] = MEMORY_REGION_MAPPED(RI_BASE_REG,                   "RDRAM Interface",          FALSE, FALSE, FALSE), // [0x04700000 - 0x047FFFFF] (RCP)
    [MEM_RCP_SI                     ] = MEMORY_REGION_MAPPED(SI_BASE_REG,                   "Serial Interface",         FALSE, FALSE, FALSE), // [0x04800000 - 0x048FFFFF] (RCP)
    [MEM_RCP_UNMAPPED_2             ] = MEMORY_REGION_UNMAPPED(SI_BASE_REG + 0x100000),                                                       // [0x04900000 - 0x04FFFFFF] (RCP)
    // PI EXT 1:
    [MEM_PI_EXT_N64DD_REGISTERS     ] = MEMORY_REGION_MAPPED(PI_DOM2_ADDR1,                 "N64DD Registers",          FALSE, FALSE, FALSE), // [0x05000000 - 0x05FFFFFF] (PI)
    [MEM_PI_EXT_N64DD_IPL_ROM       ] = MEMORY_REGION_MAPPED(PI_DOM1_ADDR1,                 "N64DD IPL Rom",            FALSE, FALSE, FALSE), // [0x06000000 - 0x07FFFFFF] (PI)
    [MEM_PI_EXT_CARTRIDGE_SRAM      ] = MEMORY_REGION_MAPPED(PI_DOM2_ADDR2,                 "Cartridge SRAM",           FALSE, FALSE, FALSE), // [0x08000000 - 0x0FFFFFFF] (PI)
    [MEM_PI_EXT_CARTRIDGE_ROM       ] = MEMORY_REGION_MAPPED(PI_DOM1_ADDR2,                 "Cartridge ROM",            FALSE, FALSE, FALSE), // [0x10000000 - 0x1FBFFFFF] (PI)
    // SI EXT:
    [MEM_SI_EXT_PIF_ROM             ] = MEMORY_REGION_MAPPED(PIF_ROM_START,                 "PIF Boot ROM",             FALSE, FALSE, FALSE), // [0x1FC00000 - 0x1FC007BF] (SI)
    [MEM_SI_EXT_PIF_RAM             ] = MEMORY_REGION_MAPPED(PIF_RAM_START,                 "PIF RAM",                  FALSE, FALSE, FALSE), // [0x1FC001C0 - 0x1FC007FF] (SI)
    [MEM_SI_EXT_RESERVED            ] = MEMORY_REGION_MAPPED((PIF_RAM_END + 1),             "Reserved",                 FALSE, FALSE, FALSE), // [0x1FC00800 - 0x1FCFFFFF] (SI)
    // PI EXT 2:
    [MEM_PI_EXT_UNUSED1             ] = MEMORY_REGION_MAPPED(PI_DOM1_ADDR3,                 "Unused 1",                 FALSE, FALSE, FALSE), // [0x1FD00000 - 0x1FFFFFFF] (PI)
    [MEM_PI_EXT_UNUSED2             ] = MEMORY_REGION_MAPPED(0x20000000,                    "Unused 2",                 FALSE, FALSE, FALSE), // [0x20000000 - 0x7FFFFFFF] (PI)
    // UNMAPPED:
    [MEM_UNMAPPED                   ] = MEMORY_REGION_UNMAPPED(VIRTUAL_RAM_START),                                                            // [0x80000000 - 0xFFFFFFFF]
    [MEM_MEMORY_REGIONS_END         ] = MEMORY_REGION_END(),
};


// Whether a PI DMA is in progress.
ALWAYS_INLINE static _Bool pi_is_busy(void) {
    return ((IO_READ(PI_STATUS_REG) & (PI_STATUS_DMA_BUSY | PI_STATUS_IO_BUSY)) != 0);
}

// Whether a PI DMA has finished.
ALWAYS_INLINE static _Bool pi_dma_is_unfinished(void) {
    return ((IO_READ(PI_STATUS_REG) & (PI_STATUS_DMA_BUSY | PI_STATUS_ERROR)) != 0);
}

//! TODO: Description.
void headless_dma(Address devAddr, void* dramAddr, size_t size) {
    // Wait until DMA is finished and no IO is currently in progress.
    while (pi_is_busy());

    IO_WRITE(PI_DRAM_ADDR_REG, K0_TO_PHYS(dramAddr));
    IO_WRITE(PI_CART_ADDR_REG, K1_TO_PHYS((Address)osRomBase | devAddr));
    IO_WRITE(PI_WR_LEN_REG, (size - 1));

    // Wait until DMA is finished and no IO has occured.
    while (pi_dma_is_unfinished());
}

ALWAYS_INLINE static _Bool is_in_region(const MemoryRegion* regionList, Address addr, int regionId) {
    return ((addr >= regionList[regionId].addr) && (addr < regionList[regionId + 1].addr));
}

// Whether the address is in a specific memory region (see 'enum MemoryRegions' and 'sMemoryBounds').
ALWAYS_INLINE static _Bool is_in_memory_region(Address addr, enum MemoryRegions regionId) {
    return is_in_region(sMemoryRegions, addr, regionId);
    // return ((addr >= sMemoryBounds[region]) && (addr < sMemoryBounds[region + 1]));
}

ALWAYS_INLINE static int get_region_id(const MemoryRegion* regionList, Address addr, size_t listSize) {
    for (u32 i = 0; i < listSize; i++) {
        if (is_in_region(regionList, addr, i)) {
            return i;
        }
    }

    return -1;
}

const char* get_region_str_from_addr_impl(Address addr, const MemoryRegion* regionList, size_t listSize) {
    int index = get_region_id(regionList, addr, listSize);
    return ((index != -1) ? regionList[index].name : "");
}

#define get_region_str_from_addr(_addr, _regionList) get_region_str_from_addr_impl(_addr, _regionList, (ARRAY_COUNT(_regionList) - 1))

char memory_string_buf[256] = "";

const char* get_memory_string_from_addr(Address addr) {
    bzero(memory_string_buf, sizeof(memory_string_buf));
    char* p = &memory_string_buf[0];

    addr = ALIGNFLOOR(addr, sizeof(Word));
    Address physAddr = osVirtualToPhysical((void*)addr);


    // int kernelSegmentId = get_region_id(sKernelSegments, physAddr, K_END);
    // if (kernelSegmentId != -1) {
    //     p += sprintf(p, "%s - ", sKernelSegments[kernelSegmentId].name);
    // }

    int busDeviceId = get_region_id(sBusDevices, physAddr, BUS_END);
    if (busDeviceId != -1) {
        p += sprintf(p, "%s: ", sBusDevices[busDeviceId].name);
    }

    int memoryRegionId = get_region_id(sMemoryRegions, physAddr, MEM_MEMORY_REGIONS_END);
    if (memoryRegionId != -1) {
        p += sprintf(p, "%s", sMemoryRegions[memoryRegionId].name);
    }

    return memory_string_buf;
}

// Whether the address is in RDRAM region.
UNUSED static _Bool is_in_rdram(Address addr) {
    return (
        is_in_memory_region(addr, MEM_RDRAM_MEMORY             ) ||
        is_in_memory_region(addr, MEM_RDRAM_REGISTERS          ) ||
        is_in_memory_region(addr, MEM_RDRAM_REGISTERS_BROADCAST)
    );
}

// Whether the address is in RCP region.
UNUSED static _Bool is_in_rcp(Address addr) {
    return (
        is_in_memory_region(addr, MEM_RCP_RSP_DMEM             ) ||
        is_in_memory_region(addr, MEM_RCP_RSP_IMEM             ) ||
        is_in_memory_region(addr, MEM_RCP_RSP_DMEM_IMEM_MIRRORS) ||
        is_in_memory_region(addr, MEM_RCP_RSP_REGISTERS        ) ||
        is_in_memory_region(addr, MEM_RCP_UNMAPPED_1           ) ||
        is_in_memory_region(addr, MEM_RCP_RDP_COMMAND_REGISTERS) ||
        is_in_memory_region(addr, MEM_RCP_RDP_SPAN_REGISTERS   ) ||
        is_in_memory_region(addr, MEM_RCP_MI                   ) ||
        is_in_memory_region(addr, MEM_RCP_VI                   ) ||
        is_in_memory_region(addr, MEM_RCP_AI                   ) ||
        is_in_memory_region(addr, MEM_RCP_PI                   ) ||
        is_in_memory_region(addr, MEM_RCP_RI                   ) ||
        is_in_memory_region(addr, MEM_RCP_SI                   ) ||
        is_in_memory_region(addr, MEM_RCP_UNMAPPED_2           )
    );
}

// Whether the address is in an unmapped region.
static _Bool is_unmapped(Address addr) {
    return (
        is_in_memory_region(addr, MEM_RCP_UNMAPPED_1) ||
        is_in_memory_region(addr, MEM_RCP_UNMAPPED_2) ||
        is_in_memory_region(addr, MEM_UNMAPPED      )
    );
}

// Whether the address is in SP region.
static _Bool is_in_sp(Address addr) {
    return (
        is_in_memory_region(addr, MEM_RCP_RSP_DMEM             ) ||
        is_in_memory_region(addr, MEM_RCP_RSP_IMEM             ) ||
        is_in_memory_region(addr, MEM_RCP_RSP_DMEM_IMEM_MIRRORS) ||
        is_in_memory_region(addr, MEM_RCP_RSP_REGISTERS        )
    );
}

// Whether the address is in PIF region.
static _Bool is_in_pif(Address addr) {
    return (
        is_in_memory_region(addr, MEM_SI_EXT_PIF_ROM) ||
        is_in_memory_region(addr, MEM_SI_EXT_PIF_RAM)
    );
}

// Whether the address is in PI region.
static _Bool is_in_pi(Address addr) {
    return (
        is_in_memory_region(addr, MEM_PI_EXT_N64DD_REGISTERS) ||
        is_in_memory_region(addr, MEM_PI_EXT_N64DD_IPL_ROM  ) ||
        is_in_memory_region(addr, MEM_PI_EXT_CARTRIDGE_SRAM ) ||
        is_in_memory_region(addr, MEM_PI_EXT_CARTRIDGE_ROM  ) ||
        is_in_memory_region(addr, MEM_PI_EXT_UNUSED1        ) ||
        is_in_memory_region(addr, MEM_PI_EXT_UNUSED2        )
    );
}

extern s32 __osSiDeviceBusy(void);
extern u32 __osSpDeviceBusy(void);

/**
 * @brief Try reading a 4-byte aligned word at 'addr' to 'dest'.
 *
 * @param[out] dest 4-byte pointer to the location to write to.
 * @param[in ] addr The 4-byte aligned address
 * @return _Bool Whether the read was successful.
 */
_Bool try_read_word_aligned(Word* dest, Address addr) {
    addr = ALIGNFLOOR(addr, sizeof(Word));

    Address physAddr = osVirtualToPhysical((void*)addr);

    // Check whether the address is virtually mapped (would throw a TLB exception):
    if (physAddr == ((Address)-1)) {
        return FALSE;
    }

    // Do a normal read if the address is in RDRAM:
    if (physAddr < TOTAL_RAM_SIZE) {
        *dest = *(Word*)addr;

        return TRUE;
    }

    // Check whether the address is physically mapped (would lock up the system):
    if (is_unmapped(physAddr)) {
        return FALSE;
    }

    // Make sure the RSP is halted and there is no SP DMA:
    if (is_in_sp(physAddr) && __osSpDeviceBusy()) {
        return FALSE;
    }

    // Make sure there is no SI DMA:
    if (is_in_pif(physAddr) && __osSiDeviceBusy()) {
        return FALSE;
    }

    // Make sure that there is no PI DMA:
    if (is_in_pi(physAddr) && pi_is_busy()) {
        return FALSE;
    }

    // Reading from this will automatically set it and will essentially block SP DMA:
    if (physAddr == SP_SEMAPHORE_REG) {
        return FALSE;
    }

    *dest = IO_READ(physAddr);

    return TRUE;
}

/**
 * @brief Try reading an 8-byte aligned word at 'addr' to 'dest'.
 *
 * @param[out] dest 8-byte pointer to the location to write to.
 * @param[in ] addr The 8-byte aligned address
 * @return _Bool Whether the read was successful.
 */
_Bool try_read_doubleword_aligned(Doubleword* dest, Address addr) {
    addr = ALIGNFLOOR(addr, sizeof(Doubleword));

    Word hi = 0x00;
    Word lo = 0x00;

    if (
        try_read_word_aligned(&hi, (addr + (0 * sizeof(Word)))) &&
        try_read_word_aligned(&lo, (addr + (1 * sizeof(Word))))
    ) {
        *dest = (HiLo64){
            .hi = hi,
            .lo = lo,
        }.raw;

        return TRUE;
    }

    return FALSE;
}

/**
 * @brief Try reading an unaligned byte.
 *
 * @param[out] dest Pointer to the location to write the data to.
 * @param[in ] addr Address of the location to read the data from.
 * @return _Bool Whether the read was successful.
 */
_Bool try_read_byte(Byte* dest, Address addr) {
    Address alignedAddr = ALIGNFLOOR(addr, sizeof(Word));
    size_t offset = (addr - alignedAddr); // 0-3.
    Word_4Bytes data = {
        .word = 0x00000000,
    };

    if (try_read_word_aligned(&data.word, alignedAddr)) {
        *dest = data.byte[offset];

        return TRUE;
    }

    return FALSE;
}

/**
 * @brief Try reading an unaligned halfword.
 *
 * @param[out] dest Pointer to the location to write the data to.
 * @param[in ] addr Address of the location to read the data from.
 * @return _Bool Whether the read was successful.
 */
_Bool try_read_halfword(Halfword* dest, Address addr) {
    Byte hi = 0x00;
    Byte lo = 0x00;

    if (
        try_read_byte(&hi, (addr + (0 * sizeof(Byte)))) &&
        try_read_byte(&lo, (addr + (1 * sizeof(Byte))))
    ) {
        *dest = (HiLo16){
            .hi = hi,
            .lo = lo,
        }.raw;

        return TRUE;
    }

    return FALSE;
}

/**
 * @brief Try reading an unaligned word.
 *
 * @param[out] dest Pointer to the location to write the data to.
 * @param[in ] addr Address of the location to read the data from.
 * @return _Bool Whether the read was successful.
 */
_Bool try_read_word(Word* dest, Address addr) {
    Halfword hi = 0x00;
    Halfword lo = 0x00;

    if (
        try_read_halfword(&hi, (addr + (0 * sizeof(Halfword)))) &&
        try_read_halfword(&lo, (addr + (1 * sizeof(Halfword))))
    ) {
        *dest = (HiLo32){
            .hi = hi,
            .lo = lo,
        }.raw;

        return TRUE;
    }

    return FALSE;
}

/**
 * @brief Try reading an unaligned doubleword.
 *
 * @param[out] dest Pointer to the location to write the data to.
 * @param[in ] addr Address of the location to read the data from.
 * @return _Bool Whether the read was successful.
 */
_Bool try_read_doubleword(Doubleword* dest, Address addr) {
    Word hi = 0x00;
    Word lo = 0x00;

    if (
        try_read_word(&hi, (addr + (0 * sizeof(Word)))) &&
        try_read_word(&lo, (addr + (1 * sizeof(Word))))
    ) {
        *dest = (HiLo64){
            .hi = hi,
            .lo = lo,
        }.raw;

        return TRUE;
    }

    return FALSE;
}

_Bool is_valid_ram_addr(Address addr) {
    Word data = 0x00000000;
    return ((addr >= RAM_START) && (addr < RAM_END) && try_read_word_aligned(&data, addr));
}

/**
 * @brief Return TRUE if the specified address is unmapped in 64-bit kernel space.
 */
_Bool is_unmapped_kx64(uint64_t vaddr) {
    if (vaddr <= 0x000000ffffffffffull) return FALSE;
    if (vaddr <= 0x3fffffffffffffffull) return TRUE;
    if (vaddr <= 0x400000ffffffffffull) return FALSE;
    if (vaddr <= 0x7fffffffffffffffull) return TRUE;
    if (vaddr <= 0x80000000ffffffffull) return FALSE;
    if (vaddr <= 0x87ffffffffffffffull) return TRUE;
    if (vaddr <= 0x88000000ffffffffull) return FALSE;
    if (vaddr <= 0x8fffffffffffffffull) return TRUE;
    if (vaddr <= 0x90000000ffffffffull) return FALSE;
    if (vaddr <= 0x97ffffffffffffffull) return TRUE;
    if (vaddr <= 0x98000000ffffffffull) return FALSE;
    if (vaddr <= 0x9fffffffffffffffull) return TRUE;
    if (vaddr <= 0xa0000000ffffffffull) return FALSE;
    if (vaddr <= 0xa7ffffffffffffffull) return TRUE;
    if (vaddr <= 0xa8000000ffffffffull) return FALSE;
    if (vaddr <= 0xafffffffffffffffull) return TRUE;
    if (vaddr <= 0xb0000000ffffffffull) return FALSE;
    if (vaddr <= 0xb7ffffffffffffffull) return TRUE;
    if (vaddr <= 0xb8000000ffffffffull) return FALSE;
    if (vaddr <= 0xbfffffffffffffffull) return TRUE;
    if (vaddr <= 0xc00000ff7fffffffull) return FALSE;
    if (vaddr <= 0xffffffff7fffffffull) return TRUE;
    if (vaddr <= 0xffffffff9fffffffull) return FALSE;
    if (vaddr <= 0xffffffffbfffffffull) return FALSE;
    if (vaddr <= 0xffffffffdfffffffull) return FALSE;
    if (vaddr <= 0xffffffffffffffffull) return FALSE;
    __builtin_unreachable();
}
