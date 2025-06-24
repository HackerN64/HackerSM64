#include <ultra64.h>
#include <macros.h>
#include <PR/os_internal_error.h>
#include <stdarg.h>
#include <string.h>
#include "segments.h"
#include "game/memory.h"

#include "symtable.h"

#ifdef DEBUG_EXPORT_SYMBOLS

u32 SYMT_ROM = 0xFFFFFFFF;
extern u8 _mapDataSegmentRomStart[];
// The start and end of the exception vector
extern u8 __osExceptionPreamble[];
extern u8 send_mesg[];

#define is_in_exception(addr) ((addr) >= (u32)__osExceptionPreamble && (addr) < (u32)send_mesg)

// code provided by Wiseguy
static void headless_dma(u32 devAddr, void *dramAddr, u32 size) {
    register u32 stat = IO_READ(PI_STATUS_REG);
    while (stat & (PI_STATUS_IO_BUSY | PI_STATUS_DMA_BUSY)) {
        stat = IO_READ(PI_STATUS_REG);
    }
    IO_WRITE(PI_DRAM_ADDR_REG, K0_TO_PHYS(dramAddr));
    IO_WRITE(PI_CART_ADDR_REG, K1_TO_PHYS((u32)osRomBase | devAddr));
    IO_WRITE(PI_WR_LEN_REG, size - 1);
}
static u32 headless_pi_status(void) {
    return IO_READ(PI_STATUS_REG);
}
// end of code provided by Wiseguy

void map_parser_dma(void *dst, void *src, size_t size) {
    headless_dma((u32)src, dst, size);
    while (headless_pi_status() & PI_STATUS_IO_BUSY);
}

/** 
 * @brief Open the SYMT symbol table in the rompak.
 * 
 * If not found, return a null header.
 */
symtable_header_t symt_open(void) {
    SYMT_ROM = (u32)_mapDataSegmentRomStart;

    symtable_header_t ALIGNED8 symt_header;
    
    if (SYMT_ROM == 0) {
        return (symtable_header_t){0};
    }

    osWritebackDCache(&symt_header, sizeof(symt_header));
    map_parser_dma(
        &symt_header,
        (uintptr_t *)SYMT_ROM,
        sizeof(symtable_header_t)
    );

    if (symt_header.head[0] != 'S' || symt_header.head[1] != 'Y' || symt_header.head[2] != 'M' || symt_header.head[3] != 'T') {
        osSyncPrintf("symt_open: invalid symbol table found at 0x%08lx\n", SYMT_ROM);
        SYMT_ROM = 0;
        return (symtable_header_t){0};
    }
    if (symt_header.version != 2) {
        osSyncPrintf("symt_open: unsupported symbol table version %ld -- please update your n64sym tool\n", symt_header.version);
        SYMT_ROM = 0;
        return (symtable_header_t){0};
    }

    return symt_header;
}

/**
 * @brief Return an entry in the address table by index
 * 
 * @param symt      SYMT file header
 * @param idx       Index of the entry to return
 * @return addrtable_entry_t  Entry of the address table
 */
addrtable_entry_t symt_addrtab_entry(symtable_header_t *symt, int idx) {
    return IO_READ(0xB0000000 | (SYMT_ROM + symt->addrtab_off + idx * 4));
}

/**
 * @brief Search the SYMT address table for the given address.
 * 
 * Run a binary search to find the entry in the table. If there is a single exact match,
 * the entry is returned. If there are multiple entries with the same address, the first
 * entry is returned (this is the case for inlined functions: so some entries following
 * the current one will have the same address). If there is no exact match, the entry
 * with the biggest address just before the given address is returned.
 *
 * @param symt      SYMT file header
 * @param addr      Address to search for
 * @param idx       If not null, will be set to the index of the entry found (or the index just before)
 * @return          The found entry (or the entry just before)
 */
addrtable_entry_t symt_addrtab_search(symtable_header_t *symt, u32 addr, int *idx) {
    int min = 0;
    int max = symt->addrtab_size - 1;
    while (min < max) {
        int mid = (min + max) / 2;
        addrtable_entry_t entry = symt_addrtab_entry(symt, mid);
        if (addr <= ADDRENTRY_ADDR(entry)) {
            max = mid;
        } else {
            min = mid + 1;
        }
    }
    addrtable_entry_t entry = symt_addrtab_entry(symt, min);
    if (min > 0 && ADDRENTRY_ADDR(entry) > addr) {
        entry = symt_addrtab_entry(symt, --min);
    }
    if (idx) {
        *idx = min;
    }
    return entry;
}

/**
 * @brief Fetch a string from the string table
 * 
 * @param symt  SYMT file
 * @param sidx  Index of the first character of the string in the string table
 * @param slen  Length of the string
 * @param buf   Destination buffer
 * @param size  Size of the destination buffer
 * @return char*  Fetched string within the destination buffer (might not be at offset 0 for alignment reasons)
 */
char *symt_string(symtable_header_t *symt, int sidx, int slen, char *buf, int size) {
    // Align 2-byte phase of the RAM buffer with the ROM address. This is required
    // for map_parser_dma.
    int tweak = (sidx ^ (u32)buf) & 1;
    char *func = buf + tweak; size -= tweak;
    int nbytes = MIN(slen, size);

    osWritebackDCache(buf, size);
    map_parser_dma(
        func, 
        (uintptr_t *)(SYMT_ROM + symt->strtab_off + sidx),
        nbytes
    );
    func[nbytes] = 0;

    if (tweak) {
        buf[0] = ' ';
    }
    return func;
}

/**
 * @brief Fetch a symbol table entry from the SYMT file.
 * 
 * @param symt    SYMT file
 * @param entry   Output entry pointer
 * @param idx     Index of the entry to fetch
 */
void symt_entry_fetch(symtable_header_t *symt, symtable_entry_t *entry, int idx) {
    osWritebackDCache(entry, sizeof(symtable_entry_t));

    // char dbg[100];
    // sprintf(dbg,"symt_entry_fetch %d\n", idx);
    // osSyncPrintf(dbg);
    map_parser_dma(
        entry,
        (uintptr_t *)(SYMT_ROM + symt->symtab_off + idx * sizeof(symtable_entry_t)),
        sizeof(symtable_entry_t)
    );
}

// Fetch the function name of an entry
char *symt_entry_func(symtable_header_t *symt, symtable_entry_t *entry, u32 addr, char *buf, int size) {
    if (is_in_exception(addr)) {
        // Special case exception handlers. This is just to show something slightly
        // more readable instead of "notcart+0x0" or similar assembly symbols
        sprintf(buf, "<EXCEPTION HANDLER>");
        return buf;
    } else {
        return symt_string(symt, entry->func_sidx, entry->func_len, buf, size);
    }
}

// Fetch the file name of an entry
char *symt_entry_file(symtable_header_t *symt, symtable_entry_t *entry, char *buf, int size) {
    return symt_string(symt, entry->file_sidx, entry->file_len, buf, size);
}

#endif // DEBUG_EXPORT_SYMBOLS
