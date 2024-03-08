#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

#include "crash_draw.h"
#include "crash_main.h"
#include "crash_settings.h"
#include "crash_pages.h"
#include "crash_print.h"
#include "map_parser.h"
#include "memory_read.h"

#include "registers.h"


//! TODO: Addresses for K0 and K1
static const RegisterInfo sRegisters_CPU[32] = {
    [REG_CPU_R0] = DEF_REG(0x00000000, sizeof(u64), FALSE, "R0", "R0"),
    [REG_CPU_AT] = DEF_CPU_REG(at, "AT"),
    [REG_CPU_V0] = DEF_CPU_REG(v0, "V0"), [REG_CPU_V1] = DEF_CPU_REG(v1, "V1"),
    [REG_CPU_A0] = DEF_CPU_REG(a0, "A0"), [REG_CPU_A1] = DEF_CPU_REG(a1, "A1"), [REG_CPU_A2] = DEF_CPU_REG(a2, "A2"), [REG_CPU_A3] = DEF_CPU_REG(a3, "A3"),
    [REG_CPU_T0] = DEF_CPU_REG(t0, "T0"), [REG_CPU_T1] = DEF_CPU_REG(t1, "T1"), [REG_CPU_T2] = DEF_CPU_REG(t0, "T2"), [REG_CPU_T3] = DEF_CPU_REG(t3, "T3"), [REG_CPU_T4] = DEF_CPU_REG(t4, "T4"), [REG_CPU_T5] = DEF_CPU_REG(t5, "T5"), [REG_CPU_T6] = DEF_CPU_REG(t6, "T6"), [REG_CPU_T7] = DEF_CPU_REG(t7, "T7"), 
    [REG_CPU_S0] = DEF_CPU_REG(s0, "S0"), [REG_CPU_S1] = DEF_CPU_REG(s1, "S1"), [REG_CPU_S2] = DEF_CPU_REG(s0, "S2"), [REG_CPU_S3] = DEF_CPU_REG(s3, "S3"), [REG_CPU_S4] = DEF_CPU_REG(s4, "S4"), [REG_CPU_S5] = DEF_CPU_REG(s5, "S5"), [REG_CPU_S6] = DEF_CPU_REG(s6, "S6"), [REG_CPU_S7] = DEF_CPU_REG(s7, "S7"), 
    [REG_CPU_T8] = DEF_CPU_REG(t8, "T8"), [REG_CPU_T9] = DEF_CPU_REG(t9, "T9"),
    [REG_CPU_K0] = DEF_REG(0x00000000, sizeof(u64), FALSE, "K0", "K0"), [REG_CPU_K1] = DEF_REG(0x00000000, sizeof(u64), FALSE, "K1", "K1"),
    [REG_CPU_GP] = DEF_CPU_REG(gp, "GP"),
    [REG_CPU_SP] = DEF_CPU_REG(sp, "SP"),
    [REG_CPU_FP] = DEF_CPU_REG(s8, "FP"),
    [REG_CPU_RA] = DEF_CPU_REG(ra, "RA"),
};

//! TODO: Addresses
static const RegisterInfo sRegisters_COP0[32] = {
    [REG_COP0_INX      ] = DEF_REG(0x00000000, sizeof(u32), FALSE, "Index",     "IX"),
    [REG_COP0_RAND     ] = DEF_REG(0x00000000, sizeof(u32), FALSE, "Random",    "RN"),
    [REG_COP0_ENTRYLO0 ] = DEF_REG(0x00000000, sizeof(u64), FALSE, "EntryLo0",  "L0"),
    [REG_COP0_ENTRYLO1 ] = DEF_REG(0x00000000, sizeof(u64), FALSE, "EntryLo1",  "L1"),
    [REG_COP0_CONTEXT  ] = DEF_REG(0x00000000, sizeof(u64), FALSE, "Context",   "CX"),
    [REG_COP0_PAGEMASK ] = DEF_REG(0x00000000, sizeof(u32), FALSE, "PageMask",  "PM"),
    [REG_COP0_WIRED    ] = DEF_REG(0x00000000, sizeof(u32), FALSE, "Wired",     "WR"),
    [REG_COP0_7        ] = DEF_REG(0x00000000, sizeof(int), FALSE, "07",        "07"),
    [REG_COP0_BADVADDR ] = DEF_THREAD_REG(badvaddr,                "BadVAddr",  "VA"), // DEF_REG(0x00000000, sizeof(u64), FALSE, "BadVAddr",  "VA"),
    [REG_COP0_COUNT    ] = DEF_REG(0x00000000, sizeof(u32), FALSE, "Count",     "CT"),
    [REG_COP0_ENTRYHI  ] = DEF_REG(0x00000000, sizeof(u64), FALSE, "EntryHi",   "EH"),
    [REG_COP0_COMPARE  ] = DEF_REG(0x00000000, sizeof(u32), FALSE, "Compare",   "CP"),
    [REG_COP0_SR       ] = DEF_THREAD_REG(sr,                      "Status",    "SR"), // DEF_REG(0x00000000, sizeof(u32), FALSE, "Status",    "SR"),
    [REG_COP0_CAUSE    ] = DEF_THREAD_REG(cause,                   "Cause",     "CS"), // DEF_REG(0x00000000, sizeof(u32), FALSE, "Cause",     "CS"),
    [REG_COP0_EPC      ] = DEF_THREAD_REG(pc,                      "EPC",       "PC"), // DEF_REG(0x00000000, sizeof(u64), FALSE, "EPC",       "PC"),
    [REG_COP0_PRID     ] = DEF_REG(0x00000000, sizeof(u32), FALSE, "PRId",      "PR"),
    [REG_COP0_CONFIG   ] = DEF_REG(0x00000000, sizeof(u32), FALSE, "Config",    "CF"),
    [REG_COP0_LLADDR   ] = DEF_REG(0x00000000, sizeof(u32), FALSE, "LLAddr",    "LL"),
    [REG_COP0_WATCHLO  ] = DEF_REG(0x00000000, sizeof(u32), FALSE, "WatchLo",   "WL"),
    [REG_COP0_WATCHHI  ] = DEF_REG(0x00000000, sizeof(u32), FALSE, "WatchHi",   "WH"),
    [REG_COP0_XCONTEXT ] = DEF_REG(0x00000000, sizeof(u64), FALSE, "XContext",  "XX"),
    [REG_COP0_21       ] = DEF_REG(0x00000000, sizeof(int), FALSE, "21",        "21"),
    [REG_COP0_22       ] = DEF_REG(0x00000000, sizeof(int), FALSE, "22",        "22"),
    [REG_COP0_23       ] = DEF_REG(0x00000000, sizeof(int), FALSE, "23",        "23"),
    [REG_COP0_24       ] = DEF_REG(0x00000000, sizeof(int), FALSE, "24",        "24"),
    [REG_COP0_25       ] = DEF_REG(0x00000000, sizeof(int), FALSE, "25",        "25"),
    [REG_COP0_ECC      ] = DEF_REG(0x00000000, sizeof(u32), FALSE, "ParityErr", "PE"),
    [REG_COP0_CACHE_ERR] = DEF_REG(0x00000000, sizeof(u32), FALSE, "CacheErr",  "CE"),
    [REG_COP0_TAGLO    ] = DEF_REG(0x00000000, sizeof(u32), FALSE, "TagLo",     "TL"),
    [REG_COP0_TAGHI    ] = DEF_REG(0x00000000, sizeof(u32), FALSE, "TagHi",     "TH"),
    [REG_COP0_ERROR_EPC] = DEF_REG(0x00000000, sizeof(u64), FALSE, "ErrorEPC",  "EE"),
    [REG_COP0_31       ] = DEF_REG(0x00000000, sizeof(int), FALSE, "31",        "31"),
};

//! TODO: Better define format
static const RegisterInfo sRegisters_COP1[32] = {
    [REG_COP1_F00] = DEF_FLT_REG(0,  "F00", "00"), [REG_COP1_F02] = DEF_FLT_REG(2,  "F02", "02"),
    [REG_COP1_F04] = DEF_FLT_REG(4,  "F04", "04"), [REG_COP1_F06] = DEF_FLT_REG(6,  "F06", "06"), [REG_COP1_F08] = DEF_FLT_REG(8,  "F08", "08"), [REG_COP1_F10] = DEF_FLT_REG(10, "F10", "10"),
    [REG_COP1_F12] = DEF_FLT_REG(12, "F12", "12"), [REG_COP1_F14] = DEF_FLT_REG(14, "F14", "14"),
    [REG_COP1_F16] = DEF_FLT_REG(16, "F16", "16"), [REG_COP1_F18] = DEF_FLT_REG(18, "F18", "18"),
    [REG_COP1_F20] = DEF_FLT_REG(20, "F20", "20"), [REG_COP1_F22] = DEF_FLT_REG(22, "F22", "22"), [REG_COP1_F24] = DEF_FLT_REG(24, "F24", "24"), [REG_COP1_F26] = DEF_FLT_REG(26, "F26", "26"), [REG_COP1_F28] = DEF_FLT_REG(28, "F28", "28"), [REG_COP1_F30] = DEF_FLT_REG(30, "F30", "30"),
};

static const RegisterInfo* sRegisters[] = {
    [CPU  + 1] = sRegisters_CPU,
    [COP0 + 1] = sRegisters_COP0,
    [COP1 + 1] = sRegisters_COP1,
    // [COP2 + 1] = sRegisters_COP2,
    // [COP3 + 1] = sRegisters_COP3,
};


const RegisterInfo* gSavedRegBuf[REG_BUFFER_SIZE];
int gSavedRegBufSize = 0;


const RegisterInfo* get_reg_info(enum Coprocessors cop, int idx) {
    return &sRegisters[cop + 1][idx];
}

u64 get_reg_val_from_info(const RegisterInfo* info) {
    if (info == NULL) {
        return 0;
    }

    Address addr = 0x00000000;
    if (info->isThread) {
        __OSThreadContext* tc = &gCrashedThread->context;
        addr = ((Address)tc + info->addr);
    } else {
        addr = info->addr;
    }

    if (info->size == sizeof(u64)) {
        Doubleword data = 0;
        if (try_read_doubleword(&data, addr)) {
            return data;
        }
    } else {
        Word data = 0;
        if (try_read_word(&data, addr)) {
            return data;
        }
    }

    return 0;
    // return (info->size == sizeof(u64)) ? *(u64*)addr : *(u32*)addr;
}

void clear_saved_reg_buffer(void) {
    bzero(gSavedRegBuf, sizeof(gSavedRegBuf));
    gSavedRegBufSize = 0;
}

void append_reg_to_buffer(const RegisterInfo* reg) {
    if (gSavedRegBufSize < REG_BUFFER_SIZE) {
        gSavedRegBuf[gSavedRegBufSize++] = reg;
    }
}
