#pragma once

#include <ultra64.h>

#include "types.h"


// Physical addresses:
#define PHYS_MEM_START  0x00000000
#define PHYS_MEM_END    (K0BASE - 1)
#define PHYS_MEM_SIZE   (size_t)(PHYS_MEM_END - PHYS_MEM_START)
// Virtual addresses:
#define VIRT_MEM_START  K0BASE
#define VIRT_MEM_END    0xFFFFFFFF
#define VIRT_MEM_SIZE   (size_t)(VIRT_MEM_END - VIRT_MEM_START)
// Total memory size:
#define TOTAL_MEM_START PHYS_MEM_START
#define TOTAL_MEM_END   VIRT_MEM_END
#define TOTAL_MEM_SIZE  (size_t)(TOTAL_MEM_END - TOTAL_MEM_START)
// Viewable/scrollable memory size:
#define VIEW_MEM_START TOTAL_MEM_START
#define VIEW_MEM_END   TOTAL_MEM_END
#define VIEW_MEM_SIZE  TOTAL_MEM_SIZE


typedef enum KernelSegments {
    KUSEG, // 0x00000000-0x7FFFFFFF
    KSEG0, // 0x80000000-0x9FFFFFFF
    KSEG1, // 0xA0000000-0xBFFFFFFF
    KSSEG, // 0xC0000000-0xDFFFFFFF
    KSEG3, // 0xE0000000-0xFFFFFFFF
    K_END,
    NUM_KSEGS,
} KernelSegments;

typedef enum BusDevices {
    BUS_RDRAM,
    BUS_RCP,
    BUS_PI_EXT_1,
    BUS_SI_EXT,
    BUS_PI_EXT_2,
    BUS_UNMAPPED,
    BUS_END,
    NUM_BUS_DEVICES,
} BusDevices;

typedef enum MemoryRegions {
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
    NUM_MEM_REGIONS,
} MemoryRegions;


typedef struct MemoryRegion {
    /*0x00*/ const Address addr; // The starting address of this region.
    /*0x04*/ const char* name;
    /*0x08*/ union {
                struct PACKED {
                    //! TODO: implement these:
                    /*0x08*/ const _Bool ro;     // Read only.
                    /*0x09*/ const _Bool wo;     // Write only.
                    /*0x0B*/ const _Bool align;  // Reads must be aligned.
                    /*0x0A*/ const _Bool mapped; // Region is mapped.
                };
                u8 raw;
            } flags;
} MemoryRegion; /*0x0C*/


void headless_dma(Address devAddr, void* dramAddr, size_t size);
_Bool virtual_to_physical(Address* pAddr, Address vAddr);
_Bool try_read_word_aligned(Word* dest, Address addr);
_Bool try_read_doubleword_aligned(Doubleword* dest, Address addr);
_Bool try_read_byte(Byte* dest, Address addr);
_Bool try_read_halfword(Halfword* dest, Address addr);
_Bool try_read_word(Word* dest, Address addr);
_Bool try_read_doubleword(Doubleword* dest, Address addr);
_Bool is_valid_ram_addr(Address addr);
_Bool is_unmapped_kx64(uint64_t vaddr);
const char* get_memory_string_from_addr(Address addr);
