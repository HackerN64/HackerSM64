#include <ultra64.h>
#include "segments.h"
#include "crash_main.h"
#include "memory_read.h"


enum BusDevices {
    BUS_RDRAM,
    BUS_RCP,
    BUS_PI_EXT_1,
    BUS_SI_EXT,
    BUS_PI_EXT_2,
    BUS_UNMAPPED,
    BUS_END,
};

UNUSED ALIGNED8 static const Address sBusDeviceBounds[] = {
    [BUS_RDRAM   ] = 0x00000000,
    [BUS_RCP     ] = SP_DMEM_START,
    [BUS_PI_EXT_1] = PI_DOM2_ADDR1,
    [BUS_SI_EXT  ] = PIF_ROM_START,
    [BUS_PI_EXT_2] = PI_DOM1_ADDR3,
    [BUS_UNMAPPED] = VIRTUAL_RAM_START,
    [BUS_END     ] = 0xFFFFFFFF,
};

enum KernelSegments {
    KUSEG,
    KSEG0,
    KSEG1,
    KSSEG,
    KSEG3,
    K_END,
};

UNUSED ALIGNED8 static const Address sKernelSegmentBounds[] = {
    [KUSEG] = 0x00000000,
    [KSEG0] = VIRTUAL_RAM_START,
    [KSEG1] = 0xA0000000,
    [KSSEG] = 0xC0000000,
    [KSEG3] = 0xE0000000,
    [K_END] = 0xFFFFFFFF,
};

enum MemoryRegions {
    MEM_RDRAM_MEMORY,
    MEM_RDRAM_REGISTERS,
    MEM_RDRAM_REGISTERS_BROADCAST,
    MEM_RCP_RSP_DMEM,
    MEM_RCP_RSP_IMEM,
    MEM_RCP_RSP_DMEM_IMEM_MIRRORS,
    MEM_RCP_RSP_REGISTERS,
    MEM_RCP_UNMAPPED_1,
    MEM_RCP_RDP_COMMAND_REGISTERS,
    MEM_RCP_RDP_SPAN_REGISTERS,
    MEM_RCP_MI,
    MEM_RCP_VI,
    MEM_RCP_AI,
    MEM_RCP_PI,
    MEM_RCP_RI,
    MEM_RCP_SI,
    MEM_RCP_UNMAPPED_2,
    MEM_PI_EXT_N64DD_REGISTERS,
    MEM_PI_EXT_N64DD_IPL_ROM,
    MEM_PI_EXT_CARTRIDGE_SRAM,
    MEM_PI_EXT_CARTRIDGE_ROM,
    MEM_SI_EXT_PIF_ROM,
    MEM_SI_EXT_PIF_RAM,
    MEM_SI_EXT_RESERVED,
    MEM_PI_EXT_UNUSED1,
    MEM_PI_EXT_UNUSED2,
    MEM_UNMAPPED,
    MEM_MEMORY_REGIONS_END,
};

ALIGNED4 static const Address sMemoryBounds[] = {
    [MEM_RDRAM_MEMORY             ] = 0x00000000,
    [MEM_RDRAM_REGISTERS          ] = RDRAM_BASE_REG,
    [MEM_RDRAM_REGISTERS_BROADCAST] = (RDRAM_BASE_REG + 0x80000),
    [MEM_RCP_RSP_DMEM             ] = SP_DMEM_START,
    [MEM_RCP_RSP_IMEM             ] = SP_IMEM_START,
    [MEM_RCP_RSP_DMEM_IMEM_MIRRORS] = (SP_IMEM_END + 1),
    [MEM_RCP_RSP_REGISTERS        ] = SP_BASE_REG,
    [MEM_RCP_UNMAPPED_1           ] = (SP_BASE_REG + 0x80000),
    [MEM_RCP_RDP_COMMAND_REGISTERS] = DPC_BASE_REG,
    [MEM_RCP_RDP_SPAN_REGISTERS   ] = DPS_BASE_REG,
    [MEM_RCP_MI                   ] = MI_BASE_REG,
    [MEM_RCP_VI                   ] = VI_BASE_REG,
    [MEM_RCP_AI                   ] = AI_BASE_REG,
    [MEM_RCP_PI                   ] = PI_BASE_REG,
    [MEM_RCP_RI                   ] = RI_BASE_REG,
    [MEM_RCP_SI                   ] = SI_BASE_REG,
    [MEM_RCP_UNMAPPED_2           ] = (SI_BASE_REG + 0x100000),
    [MEM_PI_EXT_N64DD_REGISTERS   ] = PI_DOM2_ADDR1,
    [MEM_PI_EXT_N64DD_IPL_ROM     ] = PI_DOM1_ADDR1,
    [MEM_PI_EXT_CARTRIDGE_SRAM    ] = PI_DOM2_ADDR2,
    [MEM_PI_EXT_CARTRIDGE_ROM     ] = PI_DOM1_ADDR2,
    [MEM_SI_EXT_PIF_ROM           ] = PIF_ROM_START,
    [MEM_SI_EXT_PIF_RAM           ] = PIF_RAM_START,
    [MEM_SI_EXT_RESERVED          ] = (PIF_RAM_END + 1),
    [MEM_PI_EXT_UNUSED1           ] = PI_DOM1_ADDR3,
    [MEM_PI_EXT_UNUSED2           ] = 0x20000000,
    [MEM_UNMAPPED                 ] = VIRTUAL_RAM_START,
    [MEM_MEMORY_REGIONS_END       ] = 0xFFFFFFFF,
};

// Whether a PI DMA is in progress.
static _Bool pi_is_busy(void) {
    return ((IO_READ(PI_STATUS_REG) & (PI_STATUS_DMA_BUSY | PI_STATUS_IO_BUSY)) != 0);
}

// Whether a PI DMA has finished.
static _Bool pi_dma_is_unfinished(void) {
    return ((IO_READ(PI_STATUS_REG) & (PI_STATUS_DMA_BUSY | PI_STATUS_ERROR)) != 0);
}

//! TODO: Description
void headless_dma(Address devAddr, void* dramAddr, size_t size) {
    // Wait until DMA is finished and no IO is currently in progress.
    while (pi_is_busy());

    IO_WRITE(PI_DRAM_ADDR_REG, K0_TO_PHYS(dramAddr));
    IO_WRITE(PI_CART_ADDR_REG, K1_TO_PHYS((Address)osRomBase | devAddr));
    IO_WRITE(PI_WR_LEN_REG, (size - 1));

    // Wait until DMA is finished and no IO has occured.
    while (pi_dma_is_unfinished());
}

//! TODO: Description
ALWAYS_INLINE static _Bool is_in_memory_region(Address addr, enum MemoryRegions region) {
    return ((addr >= sMemoryBounds[region]) && (addr < sMemoryBounds[region + 1]));
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

// Try reading a 4-byte aligned word at 'addr' to 'dest'. Returns whether the read was successful.
_Bool try_read_data(Word* dest, Address addr) {
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

// Try read unaligned byte.
_Bool try_read_byte(Byte* dest, Address addr) {
    Address alignedAddr = ALIGNFLOOR(addr, sizeof(Word));
    size_t offset = (addr - alignedAddr); // 0-3
    Word data = 0;

    if (try_read_data(&data, alignedAddr)) {
        *dest = (Byte)(data >> (((sizeof(Word) - 1) - offset) * SIZEOF_BITS(Byte))); // (data >> (((4 - 1) - offset) * 8))

        return TRUE;
    }

    return FALSE;
}

// Try read unaligned halfword.
_Bool try_read_halfword(Halfword* dest, Address addr) {
    Byte hi = 0x00;
    Byte lo = 0x00;

    if (
        try_read_byte(&hi, (addr + (0 * sizeof(Byte)))) &&
        try_read_byte(&lo, (addr + (1 * sizeof(Byte))))
    ) {
        *dest = (((Halfword)hi << SIZEOF_BITS(Byte)) | (Halfword)lo);

        return TRUE;
    }

    return FALSE;
}

// Try read unaligned word.
_Bool try_read_word(Word* dest, Address addr) {
    Halfword hi = 0x00;
    Halfword lo = 0x00;

    if (
        try_read_halfword(&hi, (addr + (0 * sizeof(Halfword)))) &&
        try_read_halfword(&lo, (addr + (1 * sizeof(Halfword))))
    ) {
        *dest = (((Word)hi << SIZEOF_BITS(Halfword)) | (Word)lo);

        return TRUE;
    }

    return FALSE;
}

// Try read unaligned doubleword.
_Bool try_read_doubleword(Doubleword* dest, Address addr) {
    Word hi = 0x00;
    Word lo = 0x00;

    if (
        try_read_word(&hi, (addr + (0 * sizeof(Word)))) &&
        try_read_word(&lo, (addr + (1 * sizeof(Word))))
    ) {
        *dest = (((Doubleword)hi << SIZEOF_BITS(Word)) | (Doubleword)lo);

        return TRUE;
    }

    return FALSE;
}
