#include <ultra64.h>
#include <macros.h>
#include <PR/os_internal_error.h>
#include <stdarg.h>
#include <string.h>
#include "segments.h"
#include "memory.h"
#include "backtrace.h"
#include "cop0.h"

/**
 * @brief Exception codes
 */
typedef enum {
    EXCEPTION_CODE_INTERRUPT = 0,
    EXCEPTION_CODE_TLB_MODIFICATION = 1,
    EXCEPTION_CODE_TLB_LOAD_I_MISS = 2,
    EXCEPTION_CODE_TLB_STORE_MISS = 3,
    EXCEPTION_CODE_LOAD_I_ADDRESS_ERROR = 4,
    EXCEPTION_CODE_STORE_ADDRESS_ERROR = 5,
    EXCEPTION_CODE_I_BUS_ERROR = 6,
    EXCEPTION_CODE_D_BUS_ERROR = 7,
    EXCEPTION_CODE_SYS_CALL = 8,
    EXCEPTION_CODE_BREAKPOINT = 9,
    EXCEPTION_CODE_RESERVED_INSTRUCTION = 10,
    EXCEPTION_CODE_COPROCESSOR_UNUSABLE = 11,
    EXCEPTION_CODE_ARITHMETIC_OVERFLOW = 12,
    EXCEPTION_CODE_TRAP = 13,
    EXCEPTION_CODE_FLOATING_POINT = 15,
    EXCEPTION_CODE_WATCH = 23,
} exception_code_t;

/** @brief Check if addr is a valid PC address */
static u8 is_valid_address(u32 addr)
{
    // TODO: for now we only handle RAM (cached access). This should be extended to handle
    // TLB-mapped addresses for instance.
    return addr >= 0x80000400 && addr < 0x80800000 && (addr & 3) == 0;
}

#define offsetof(type, member)  __builtin_offsetof (type, member)

// The start and end of the exception vector
extern u8 __osExceptionPreamble[];
extern u8 send_mesg[];
#define is_in_exception(addr) ((addr) >= (u32)__osExceptionPreamble && (addr) < (u32)send_mesg)

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

/** @brief Address of the SYMT symbol table in the rompak. */
static u32 SYMT_ROM = 0xFFFFFFFF;

/** @brief Placeholder used in frames where symbols are not available */
static const char *UNKNOWN_SYMBOL = "???";

extern void dma_read(void *dest, void *srcStart, void *srcEnd);

void map_parser_dma(void *dst, void *src, size_t size) {
    // char dm[500];
    // sprintf(dm, "DMA [%08X] <-- [%08X %08X] (%08X)", dst, src, size, SYMT_ROM);
    // osSyncPrintf(dm);
    // dma_read(dst, src, src+ALIGN8(size));
    headless_dma((u32)src, dst, size);
    while (headless_pi_status() & PI_STATUS_IO_BUSY);
}

#define STACK_TRAVERSAL_LIMIT 100

/** @brief Function alignment enfored by the compiler (-falign-functions). 
 * 
 * @note This must be kept in sync with n64.mk.
 */
#define FUNCTION_ALIGNMENT      32

extern u8 _mapDataSegmentRomStart[];

extern u8 _mainSegmentStart[];
extern u8 _mainSegmentTextEnd[];
extern u8 _engineSegmentStart[];
extern u8 _engineSegmentTextEnd[];
extern u8 _goddardSegmentStart[];
extern u8 _goddardSegmentTextEnd[];

char* __symbolize(void *vaddr, char *buf, int size) {
    symtable_header_t symt = symt_open(vaddr);
    if (symt.head[0]) {
        u32 addr = (u32)vaddr;
        int idx = 0;
        addrtable_entry_t a = symt_addrtab_search(&symt, addr, &idx);
        while (!ADDRENTRY_IS_FUNC(a)) {
            a = symt_addrtab_entry(&symt, --idx);
        }

        symtable_entry_t ALIGNED16 entry;
        // Read the symbol name
        symt_entry_fetch(&symt, &entry, idx);
        char *func = symt_entry_func(&symt, &entry, addr, buf, size-12);
        char lbuf[12];
        sprintf(lbuf, "+0x%lx", addr - ADDRENTRY_ADDR(a));
        return strcat(func, lbuf);
    }
    sprintf(buf, "%s", UNKNOWN_SYMBOL);
    return buf;
}

char *parse_map(u32 addr) {
    static char map_name[64] ALIGNED16;
    __symbolize((u32*)addr, map_name, sizeof(map_name));

    return map_name;
}

char *get_filename(u32 addr) {
    static char file_name[64] ALIGNED16;

    return file_name;
}

char *find_function_in_stack(u32 *sp, int *line) {
    char *ret = NULL;
    u32 val = *sp;

    #define CALLSITE_OFFSET 8
    for (int i = 0; i < STACK_TRAVERSAL_LIMIT; i++) {
        val = *sp;
        val = *(u32 *)val;
        *sp += 4;


        if ((val >= (u32)_mainSegmentStart) && (val <= (u32)_mainSegmentTextEnd)) {
            ret = parse_map(val + CALLSITE_OFFSET);
            break;
        }
        else if ((val >= (u32)_engineSegmentStart) && (val <= (u32)_engineSegmentTextEnd)) {
            ret = parse_map(val + CALLSITE_OFFSET);
            break;
        }
        else if ((val >= (u32)_goddardSegmentStart) && (val <= (u32)_goddardSegmentTextEnd)) {
            ret = parse_map(val + CALLSITE_OFFSET);
            break;
        }
    }

    if (ret) {
        search_symbol((void*)(val + CALLSITE_OFFSET), line);
    }

    return ret;
}

void crash_screen_print(s32 x, s32 y, const char *fmt, ...);

char *search_symbol(void *vaddr, int *line) {
    static char filebuf[100];

    symtable_header_t symt = symt_open(vaddr);
    if (symt.head[0]) {
        u32 addr = (u32)vaddr;
        int idx = 0;
        addrtable_entry_t a = symt_addrtab_search(&symt, addr, &idx);

        symtable_entry_t ALIGNED16 entry;

        // Read the symbol name
        filebuf[0] = 0;
        symt_entry_fetch(&symt, &entry, idx);
        *line = entry.line;
        char *file = symt_entry_file(&symt, &entry, addr, filebuf, sizeof(filebuf));
        return file;
    }
    return UNKNOWN_SYMBOL;
}
