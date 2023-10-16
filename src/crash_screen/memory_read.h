#pragma once

#include <ultra64.h>

#include "types.h"


// Virtual RAM boundary defines.
#define VIRTUAL_RAM_START (Address)RAM_START
#define VIRTUAL_RAM_END   (Address)0xFFFFFFFF
#define VIRTUAL_RAM_SIZE  (size_t)(VIRTUAL_RAM_END - VIRTUAL_RAM_START)


enum BusDevices {
    BUS_RDRAM,
    BUS_RCP,
    BUS_PI_EXT_1,
    BUS_SI_EXT,
    BUS_PI_EXT_2,
    BUS_UNMAPPED,
    BUS_END,
};

enum KernelSegments {
    KUSEG,
    KSEG0,
    KSEG1,
    KSSEG,
    KSEG3,
    K_END,
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


void headless_dma(Address devAddr, void* dramAddr, size_t size);
_Bool try_read_data(Word* dest, Address addr);
_Bool try_read_byte(Byte* dest, Address addr);
_Bool try_read_halfword(Halfword* dest, Address addr);
_Bool try_read_word(Word* dest, Address addr);
_Bool try_read_doubleword(Doubleword* dest, Address addr);
