#include <ultra64.h>
#include "segments.h"
#include "crash_screen.h"
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
    [BUS_RCP     ] = 0x04000000,
    [BUS_PI_EXT_1] = 0x05000000,
    [BUS_SI_EXT  ] = 0x1FC00000,
    [BUS_PI_EXT_2] = 0x1FD00000,
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

static _Bool pi_is_busy(void) {
    return ((IO_READ(PI_STATUS_REG) & (PI_STATUS_DMA_BUSY | PI_STATUS_IO_BUSY)) != 0);
}

static _Bool pi_dma_is_unfinished(void) {
    return ((IO_READ(PI_STATUS_REG) & (PI_STATUS_DMA_BUSY | PI_STATUS_ERROR)) != 0);
}

void headless_dma(Address devAddr, void* dramAddr, size_t size) {
    // Wait until DMA is finished and no IO is currently in progress.
    while (pi_is_busy());

    IO_WRITE(PI_DRAM_ADDR_REG, K0_TO_PHYS(dramAddr));
    IO_WRITE(PI_CART_ADDR_REG, K1_TO_PHYS((Address)osRomBase | devAddr));
    IO_WRITE(PI_WR_LEN_REG, (size - 1));

    // Wait until DMA is finished and no IO has occured.
    while (pi_dma_is_unfinished());
}

ALWAYS_INLINE static _Bool is_in_memory_region(Address addr, enum MemoryRegions region) {
    return ((addr >= sMemoryBounds[region]) && (addr < sMemoryBounds[region + 1]));
}

UNUSED static _Bool is_in_rdram(Address addr) {
    return (
        is_in_memory_region(addr, MEM_RDRAM_MEMORY             ) ||
        is_in_memory_region(addr, MEM_RDRAM_REGISTERS          ) ||
        is_in_memory_region(addr, MEM_RDRAM_REGISTERS_BROADCAST)
    );
}

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

static _Bool is_unmapped(Address addr) {
    return (
        is_in_memory_region(addr, MEM_RCP_UNMAPPED_1) ||
        is_in_memory_region(addr, MEM_RCP_UNMAPPED_2) ||
        is_in_memory_region(addr, MEM_UNMAPPED      )
    );
}

static _Bool is_in_sp(Address addr) {
    return (
        is_in_memory_region(addr, MEM_RCP_RSP_DMEM             ) ||
        is_in_memory_region(addr, MEM_RCP_RSP_IMEM             ) ||
        is_in_memory_region(addr, MEM_RCP_RSP_DMEM_IMEM_MIRRORS) ||
        is_in_memory_region(addr, MEM_RCP_RSP_REGISTERS        )
    );
}

static _Bool is_in_pif(Address addr) {
    return (
        is_in_memory_region(addr, MEM_SI_EXT_PIF_ROM) ||
        is_in_memory_region(addr, MEM_SI_EXT_PIF_RAM)
    );
}

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

_Bool read_data(Word* dest, Address addr) {
    addr = ALIGNFLOOR(addr, sizeof(Word));

    Address physAddr = osVirtualToPhysical((void*)addr);

    // Check whether the address is virtually mapped:
    if (physAddr == ((Address)-1)) {
        return FALSE;
    }

    // Do a normal read if the address is in RDRAM:
    if (physAddr < TOTAL_RAM_SIZE) {
        *dest = *(Word*)addr;

        return TRUE;
    }

    // Check whether the address is physically mapped:
    if (is_unmapped(physAddr)) {
        return FALSE;
    }

    if (is_in_sp(physAddr) && __osSpDeviceBusy()) {
        return FALSE;
    }

    if (is_in_pif(physAddr) && __osSiDeviceBusy()) {
        return FALSE;
    }

    if (is_in_pi(physAddr) && pi_is_busy()) {
        return FALSE;
    }

    if (physAddr == SP_SEMAPHORE_REG) {
        return FALSE;
    }

    *dest = IO_READ(physAddr);

    return TRUE;
}

_Bool read_unaligned_byte(Byte* dest, Address addr) {
    Address alignedAddr = ALIGNFLOOR(addr, sizeof(Word));
    size_t offset = (addr - alignedAddr); // 0-3
    Word data = 0;

    if (read_data(&data, alignedAddr)) {
        *dest = (Byte)(data >> ((3 - offset) * 8));

        return TRUE;
    }

    return FALSE;
}
