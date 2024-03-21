#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/cs_main.h"
#include "map_parser.h"
#include "memory_read.h"

#include "registers.h"


// https://n64.readthedocs.io/
// https://hack64.net/docs/VR43XX.pdf


// -- CPU Register info --

const char* sRegDesc[] = {
    [REG_DESC_ZERO  ] = "zero",
    [REG_DESC_AT    ] = "assembler temporary value",
    [REG_DESC_SUBRET] = "subroutine return value",
    [REG_DESC_SUBARG] = "subroutine argument",
    [REG_DESC_TEMP  ] = "temporary value",
    [REG_DESC_SAVED ] = "saved value",
    [REG_DESC_KERNEL] = "kernel value",
    [REG_DESC_GP    ] = "global pointer",
    [REG_DESC_SP    ] = "stack pointer",
    [REG_DESC_FP    ] = "saved value or frame pointer", //! TODO: determine which one this register is.
    [REG_DESC_RA    ] = "return address",
};
ALIGNED32 static const RegisterInfo sRegisters_CPU[CPU_NUM_REGISTERS] = {
    [REG_CPU_R0] = DEF_CPU_SREG(zero, "R0", REG_DESC_ZERO),
    [REG_CPU_AT] = DEF_CPU_TREG(at, "AT", REG_DESC_AT    ),
    [REG_CPU_V0] = DEF_CPU_TREG(v0, "V0", REG_DESC_SUBRET), [REG_CPU_V1] = DEF_CPU_TREG(v1, "V1", REG_DESC_SUBRET),
    [REG_CPU_A0] = DEF_CPU_TREG(a0, "A0", REG_DESC_SUBARG), [REG_CPU_A1] = DEF_CPU_TREG(a1, "A1", REG_DESC_SUBARG), [REG_CPU_A2] = DEF_CPU_TREG(a2, "A2", REG_DESC_SUBARG), [REG_CPU_A3] = DEF_CPU_TREG(a3, "A3", REG_DESC_SUBARG),
    [REG_CPU_T0] = DEF_CPU_TREG(t0, "T0", REG_DESC_TEMP  ), [REG_CPU_T1] = DEF_CPU_TREG(t1, "T1", REG_DESC_TEMP  ), [REG_CPU_T2] = DEF_CPU_TREG(t0, "T2", REG_DESC_TEMP  ), [REG_CPU_T3] = DEF_CPU_TREG(t3, "T3", REG_DESC_TEMP  ), [REG_CPU_T4] = DEF_CPU_TREG(t4, "T4", REG_DESC_TEMP  ), [REG_CPU_T5] = DEF_CPU_TREG(t5, "T5", REG_DESC_TEMP  ), [REG_CPU_T6] = DEF_CPU_TREG(t6, "T6", REG_DESC_TEMP  ), [REG_CPU_T7] = DEF_CPU_TREG(t7, "T7", REG_DESC_TEMP  ),
    [REG_CPU_S0] = DEF_CPU_TREG(s0, "S0", REG_DESC_SAVED ), [REG_CPU_S1] = DEF_CPU_TREG(s1, "S1", REG_DESC_SAVED ), [REG_CPU_S2] = DEF_CPU_TREG(s0, "S2", REG_DESC_SAVED ), [REG_CPU_S3] = DEF_CPU_TREG(s3, "S3", REG_DESC_SAVED ), [REG_CPU_S4] = DEF_CPU_TREG(s4, "S4", REG_DESC_SAVED ), [REG_CPU_S5] = DEF_CPU_TREG(s5, "S5", REG_DESC_SAVED ), [REG_CPU_S6] = DEF_CPU_TREG(s6, "S6", REG_DESC_SAVED ), [REG_CPU_S7] = DEF_CPU_TREG(s7, "S7", REG_DESC_SAVED ),
    [REG_CPU_T8] = DEF_CPU_TREG(t8, "T8", REG_DESC_TEMP  ), [REG_CPU_T9] = DEF_CPU_TREG(t9, "T9", REG_DESC_TEMP  ),
    [REG_CPU_K0] = DEF_CPU_SREG(k0, "K0", REG_DESC_KERNEL), [REG_CPU_K1] = DEF_CPU_SREG(k1, "K1", REG_DESC_KERNEL),
    [REG_CPU_GP] = DEF_CPU_TREG(gp, "GP", REG_DESC_GP    ),
    [REG_CPU_SP] = DEF_CPU_TREG(sp, "SP", REG_DESC_SP    ),
    [REG_CPU_FP] = DEF_CPU_TREG(s8, "FP", REG_DESC_FP    ),
    [REG_CPU_RA] = DEF_CPU_TREG(ra, "RA", REG_DESC_RA    ),
};

#define CASE_CPU_REG(_idx, _reg) CASE_REG(CPU, _idx, _reg)
uint64_t get_cpu_reg_val(enum CPURegisters idx) {
    int val = 0;
    switch (idx) {
        CASE_CPU_REG(REG_CPU_R0, zero);
        //! TODO: Directly accessing $at requires ".set noat".
        // CASE_CPU_REG(REG_CPU_AT, at);
        case REG_CPU_AT: return __osRunningThread->context.at;
        CASE_CPU_REG(REG_CPU_V0, v0); CASE_CPU_REG(REG_CPU_V1, v1);
        CASE_CPU_REG(REG_CPU_A0, a0); CASE_CPU_REG(REG_CPU_A1, a1); CASE_CPU_REG(REG_CPU_A2, a2); CASE_CPU_REG(REG_CPU_A3, a3);
        CASE_CPU_REG(REG_CPU_T0, t0); CASE_CPU_REG(REG_CPU_T1, t1); CASE_CPU_REG(REG_CPU_T2, t2); CASE_CPU_REG(REG_CPU_T3, t3); CASE_CPU_REG(REG_CPU_T4, t4); CASE_CPU_REG(REG_CPU_T5, t5); CASE_CPU_REG(REG_CPU_T6, t6); CASE_CPU_REG(REG_CPU_T7, t7);
        CASE_CPU_REG(REG_CPU_S0, s0); CASE_CPU_REG(REG_CPU_S1, s1); CASE_CPU_REG(REG_CPU_S2, s2); CASE_CPU_REG(REG_CPU_S3, s3); CASE_CPU_REG(REG_CPU_S4, 24); CASE_CPU_REG(REG_CPU_S5, s5); CASE_CPU_REG(REG_CPU_S6, s6); CASE_CPU_REG(REG_CPU_S7, s7);
        CASE_CPU_REG(REG_CPU_T8, t8); CASE_CPU_REG(REG_CPU_T9, t9);
        CASE_CPU_REG(REG_CPU_K0, k0); CASE_CPU_REG(REG_CPU_K1, k1);
        CASE_CPU_REG(REG_CPU_GP, gp);
        CASE_CPU_REG(REG_CPU_SP, sp);
        CASE_CPU_REG(REG_CPU_FP, fp);
        CASE_CPU_REG(REG_CPU_RA, ra);
        default: break;
    }
    return val;
}


// -- COP0 Register info --

const char* sRegDesc_CP0[] = {
    [REG_DESC_CP0_INX      ] = "index of TLB entry",
    [REG_DESC_CP0_RAND     ] = "random TLB index between $Wired and 0x1F",
    [REG_DESC_CP0_ENTRYLO0 ] = "TLB entry low buts (even)",
    [REG_DESC_CP0_ENTRYLO1 ] = "TLB entry low buts (odd)",
    [REG_DESC_CP0_CONTEXT  ] = "pointer to PIE entry",
    [REG_DESC_CP0_PAGEMASK ] = "page size spec.",
    [REG_DESC_CP0_WIRED    ] = "num wired TLB entries",
    [REG_DESC_CP0_BADVADDR ] = "virt. addr of failed TLB translation",
    [REG_DESC_CP0_COUNT    ] = "timer count",
    [REG_DESC_CP0_ENTRYHI  ] = "TLB entry high bits",
    [REG_DESC_CP0_COMPARE  ] = "timer compare value",
    [REG_DESC_CP0_SR       ] = "operation status setting",
    [REG_DESC_CP0_CAUSE    ] = "cause of last exception",
    [REG_DESC_CP0_EPC      ] = "exception program counter",
    [REG_DESC_CP0_PRID     ] = "processor revision id",
    [REG_DESC_CP0_CONFIG   ] = "memory system mode setting",
    [REG_DESC_CP0_LLADDR   ] = "load linked insn addr",
    [REG_DESC_CP0_WATCHLO  ] = "watch exception low bits",
    [REG_DESC_CP0_WATCHHI  ] = "watch exception high bits",
    [REG_DESC_CP0_XCONTEXT ] = "indicates PIE entry",
    [REG_DESC_CP0_ECC      ] = "cache parity",
    [REG_DESC_CP0_CACHE_ERR] = "cache error/status",
    [REG_DESC_CP0_TAGLO    ] = "cache tag low bits",
    [REG_DESC_CP0_TAGHI    ] = "cache tag high bits",
    [REG_DESC_CP0_ERROR_EPC] = "error exception program counter",
    [REG_DESC_CP0_RESERVED ] = "reserved",
};
ALIGNED32 static const RegisterInfo sRegisters_COP0[CP0_NUM_REGISTERS] = {
    [REG_CP0_INX      ] = DEF_COP0_SREG(C0_INX,       u32,           "Index",    "IX", REG_DESC_CP0_INX      ),
    [REG_CP0_RAND     ] = DEF_COP0_SREG(C0_RAND,      u32,           "Random",   "RN", REG_DESC_CP0_RAND     ),
    [REG_CP0_ENTRYLO0 ] = DEF_COP0_SREG(C0_ENTRYLO0,  u64,           "EntryLo0", "L0", REG_DESC_CP0_ENTRYLO0 ),
    [REG_CP0_ENTRYLO1 ] = DEF_COP0_SREG(C0_ENTRYLO1,  u64,           "EntryLo1", "L1", REG_DESC_CP0_ENTRYLO1 ),
    [REG_CP0_CONTEXT  ] = DEF_COP0_SREG(C0_CONTEXT,   u64,           "Context",  "CX", REG_DESC_CP0_CONTEXT  ),
    [REG_CP0_PAGEMASK ] = DEF_COP0_SREG(C0_PAGEMASK,  u32,           "PageMask", "PM", REG_DESC_CP0_PAGEMASK ),
    [REG_CP0_WIRED    ] = DEF_COP0_SREG(C0_WIRED,     u32,           "Wired",    "WR", REG_DESC_CP0_WIRED    ),
    [REG_CP0_7        ] = DEF_COP0_SREG(7,            int,           "07",       "07", REG_DESC_CP0_RESERVED ),
    [REG_CP0_BADVADDR ] = DEF_COP0_TREG(C0_BADVADDR,  u64, badvaddr, "BadVAddr", "VA", REG_DESC_CP0_BADVADDR ),
    [REG_CP0_COUNT    ] = DEF_COP0_SREG(C0_COUNT,     u32,           "Count",    "CT", REG_DESC_CP0_COUNT    ),
    [REG_CP0_ENTRYHI  ] = DEF_COP0_SREG(C0_ENTRYHI,   u64,           "EntryHi",  "EH", REG_DESC_CP0_ENTRYHI  ),
    [REG_CP0_COMPARE  ] = DEF_COP0_SREG(C0_COMPARE,   u32,           "Compare",  "CP", REG_DESC_CP0_COMPARE  ),
    [REG_CP0_SR       ] = DEF_COP0_TREG(C0_SR,        u32, sr,       "Status",   "SR", REG_DESC_CP0_SR       ),
    [REG_CP0_CAUSE    ] = DEF_COP0_TREG(C0_CAUSE,     u32, cause,    "Cause",    "CR", REG_DESC_CP0_CAUSE    ),
    [REG_CP0_EPC      ] = DEF_COP0_TREG(C0_EPC,       u64, pc,       "EPC",      "PC", REG_DESC_CP0_EPC      ), //! TODO: Are pc and epc the same? They seem to be different sizes but they match on a crash.
    [REG_CP0_PRID     ] = DEF_COP0_SREG(C0_PRID,      u32,           "PRId",     "PR", REG_DESC_CP0_PRID     ),
    [REG_CP0_CONFIG   ] = DEF_COP0_SREG(C0_CONFIG,    u32,           "Config",   "CF", REG_DESC_CP0_CONFIG   ),
    [REG_CP0_LLADDR   ] = DEF_COP0_SREG(C0_LLADDR,    u32,           "LLAddr",   "LL", REG_DESC_CP0_LLADDR   ),
    [REG_CP0_WATCHLO  ] = DEF_COP0_SREG(C0_WATCHLO,   u32,           "WatchLo",  "WL", REG_DESC_CP0_WATCHLO  ),
    [REG_CP0_WATCHHI  ] = DEF_COP0_SREG(C0_WATCHHI,   u32,           "WatchHi",  "WH", REG_DESC_CP0_WATCHHI  ),
    [REG_CP0_XCONTEXT ] = DEF_COP0_SREG(20,           u64,           "XContext", "XX", REG_DESC_CP0_XCONTEXT ),
    [REG_CP0_21       ] = DEF_COP0_SREG(21,           int,           "21",       "21", REG_DESC_CP0_RESERVED ),
    [REG_CP0_22       ] = DEF_COP0_SREG(22,           int,           "22",       "22", REG_DESC_CP0_RESERVED ),
    [REG_CP0_23       ] = DEF_COP0_SREG(23,           int,           "23",       "23", REG_DESC_CP0_RESERVED ),
    [REG_CP0_24       ] = DEF_COP0_SREG(24,           int,           "24",       "24", REG_DESC_CP0_RESERVED ),
    [REG_CP0_25       ] = DEF_COP0_SREG(25,           int,           "25",       "25", REG_DESC_CP0_RESERVED ),
    [REG_CP0_ECC      ] = DEF_COP0_SREG(C0_ECC,       u32,           "PErr",     "PE", REG_DESC_CP0_ECC      ),
    [REG_CP0_CACHE_ERR] = DEF_COP0_SREG(C0_CACHE_ERR, u32,           "CacheErr", "CE", REG_DESC_CP0_CACHE_ERR),
    [REG_CP0_TAGLO    ] = DEF_COP0_SREG(C0_TAGLO,     u32,           "TagLo",    "TL", REG_DESC_CP0_TAGLO    ),
    [REG_CP0_TAGHI    ] = DEF_COP0_SREG(C0_TAGHI,     u32,           "TagHi",    "TH", REG_DESC_CP0_TAGHI    ),
    [REG_CP0_ERROR_EPC] = DEF_COP0_SREG(C0_ERROR_EPC, u64,           "ErrorEPC", "EE", REG_DESC_CP0_ERROR_EPC),
    [REG_CP0_31       ] = DEF_COP0_SREG(31,           int,           "31",       "31", REG_DESC_CP0_RESERVED ),
};

#define CASE_COP0_REG(_idx, _reg) CASE_REG(COP0, _idx, _reg)
uint64_t get_cop0_reg_val(enum COP0Registers idx) {
    int val = 0;
    switch (idx) {
        CASE_COP0_REG(REG_CP0_INX,       C0_INX      );
        CASE_COP0_REG(REG_CP0_RAND,      C0_RAND     );
        CASE_COP0_REG(REG_CP0_ENTRYLO0,  C0_ENTRYLO0 );
        CASE_COP0_REG(REG_CP0_ENTRYLO1,  C0_ENTRYLO1 );
        CASE_COP0_REG(REG_CP0_CONTEXT,   C0_CONTEXT  );
        CASE_COP0_REG(REG_CP0_PAGEMASK,  C0_PAGEMASK );
        CASE_COP0_REG(REG_CP0_WIRED,     C0_WIRED    );
        CASE_COP0_REG(REG_CP0_7,         7           );
        CASE_COP0_REG(REG_CP0_BADVADDR,  C0_BADVADDR );
        CASE_COP0_REG(REG_CP0_COUNT,     C0_COUNT    );
        CASE_COP0_REG(REG_CP0_ENTRYHI,   C0_ENTRYHI  );
        CASE_COP0_REG(REG_CP0_COMPARE,   C0_COMPARE  );
        CASE_COP0_REG(REG_CP0_SR,        C0_SR       );
        CASE_COP0_REG(REG_CP0_CAUSE,     C0_CAUSE    );
        CASE_COP0_REG(REG_CP0_EPC,       C0_EPC      );
        CASE_COP0_REG(REG_CP0_PRID,      C0_PRID     );
        CASE_COP0_REG(REG_CP0_CONFIG,    C0_CONFIG   );
        CASE_COP0_REG(REG_CP0_LLADDR,    C0_LLADDR   );
        CASE_COP0_REG(REG_CP0_WATCHLO,   C0_WATCHLO  );
        CASE_COP0_REG(REG_CP0_WATCHHI,   C0_WATCHHI  );
        CASE_COP0_REG(REG_CP0_XCONTEXT,  20          );
        CASE_COP0_REG(REG_CP0_21,        21          );
        CASE_COP0_REG(REG_CP0_22,        22          );
        CASE_COP0_REG(REG_CP0_23,        23          );
        CASE_COP0_REG(REG_CP0_24,        24          );
        CASE_COP0_REG(REG_CP0_25,        25          );
        CASE_COP0_REG(REG_CP0_ECC,       C0_ECC      );
        CASE_COP0_REG(REG_CP0_CACHE_ERR, C0_CACHE_ERR);
        CASE_COP0_REG(REG_CP0_TAGLO,     C0_TAGLO    );
        CASE_COP0_REG(REG_CP0_TAGHI,     C0_TAGHI    );
        CASE_COP0_REG(REG_CP0_ERROR_EPC, C0_ERROR_EPC);
        CASE_COP0_REG(REG_CP0_31,        31          );
        default: break;
    }
    return val;
}


// -- COP1 Register info --

//! TODO: Better name format
ALIGNED32 static const RegisterInfo sRegisters_COP1[CP1_NUM_REGISTERS] = { // even = low bits, odd = high bits.
    // Subroutine return values:
    [REG_CP1_F00] = DEF_COP1_TREG_EVEN(0,  "00", REG_DESC_SUBRET), [REG_CP1_F01] = DEF_COP1_TREG_ODD(0,  "01", REG_DESC_SUBRET),
    [REG_CP1_F02] = DEF_COP1_TREG_EVEN(2,  "02", REG_DESC_SUBRET), [REG_CP1_F03] = DEF_COP1_TREG_ODD(2,  "03", REG_DESC_SUBRET),
    // Temporary values:
    [REG_CP1_F04] = DEF_COP1_TREG_EVEN(4,  "04", REG_DESC_TEMP  ), [REG_CP1_F05] = DEF_COP1_TREG_ODD(4,  "05", REG_DESC_TEMP  ),
    [REG_CP1_F06] = DEF_COP1_TREG_EVEN(6,  "06", REG_DESC_TEMP  ), [REG_CP1_F07] = DEF_COP1_TREG_ODD(6,  "07", REG_DESC_TEMP  ),
    [REG_CP1_F08] = DEF_COP1_TREG_EVEN(8,  "08", REG_DESC_TEMP  ), [REG_CP1_F09] = DEF_COP1_TREG_ODD(8,  "09", REG_DESC_TEMP  ),
    [REG_CP1_F10] = DEF_COP1_TREG_EVEN(10, "10", REG_DESC_TEMP  ), [REG_CP1_F11] = DEF_COP1_TREG_ODD(10, "11", REG_DESC_TEMP  ),
    // Subroutine arguments:
    [REG_CP1_F12] = DEF_COP1_TREG_EVEN(12, "12", REG_DESC_SUBARG), [REG_CP1_F13] = DEF_COP1_TREG_ODD(12, "13", REG_DESC_SUBARG),
    [REG_CP1_F14] = DEF_COP1_TREG_EVEN(14, "14", REG_DESC_SUBARG), [REG_CP1_F15] = DEF_COP1_TREG_ODD(14, "15", REG_DESC_SUBARG),
    // Temporary values:
    [REG_CP1_F16] = DEF_COP1_TREG_EVEN(16, "16", REG_DESC_TEMP  ), [REG_CP1_F17] = DEF_COP1_TREG_ODD(16, "17", REG_DESC_TEMP  ),
    [REG_CP1_F18] = DEF_COP1_TREG_EVEN(18, "18", REG_DESC_TEMP  ), [REG_CP1_F19] = DEF_COP1_TREG_ODD(18, "19", REG_DESC_TEMP  ),
    // Saved values:
    [REG_CP1_F20] = DEF_COP1_TREG_EVEN(20, "20", REG_DESC_SAVED ), [REG_CP1_F21] = DEF_COP1_TREG_ODD(20, "21", REG_DESC_SAVED ),
    [REG_CP1_F22] = DEF_COP1_TREG_EVEN(22, "22", REG_DESC_SAVED ), [REG_CP1_F23] = DEF_COP1_TREG_ODD(22, "23", REG_DESC_SAVED ),
    [REG_CP1_F24] = DEF_COP1_TREG_EVEN(24, "24", REG_DESC_SAVED ), [REG_CP1_F25] = DEF_COP1_TREG_ODD(24, "25", REG_DESC_SAVED ),
    [REG_CP1_F26] = DEF_COP1_TREG_EVEN(26, "26", REG_DESC_SAVED ), [REG_CP1_F27] = DEF_COP1_TREG_ODD(26, "27", REG_DESC_SAVED ),
    [REG_CP1_F28] = DEF_COP1_TREG_EVEN(28, "28", REG_DESC_SAVED ), [REG_CP1_F29] = DEF_COP1_TREG_ODD(28, "29", REG_DESC_SAVED ),
    [REG_CP1_F30] = DEF_COP1_TREG_EVEN(30, "30", REG_DESC_SAVED ), [REG_CP1_F31] = DEF_COP1_TREG_ODD(30, "31", REG_DESC_SAVED ),
};

#define CASE_COP1_REG(_idx, _reg) CASE_REG(COP1, _idx, _reg)
uint64_t get_cop1_reg_val(enum COP1Registers idx) {
    int val = 0;
    switch (idx) {
        // Subroutine return values:
        CASE_COP1_REG(REG_CP1_F00, f0 ); CASE_COP1_REG(REG_CP1_F01, f1 );
        CASE_COP1_REG(REG_CP1_F02, f2 ); CASE_COP1_REG(REG_CP1_F03, f3 );
        // Temporary values:
        CASE_COP1_REG(REG_CP1_F04, f4 ); CASE_COP1_REG(REG_CP1_F05, f5 );
        CASE_COP1_REG(REG_CP1_F06, f6 ); CASE_COP1_REG(REG_CP1_F07, f7 );
        CASE_COP1_REG(REG_CP1_F08, f8 ); CASE_COP1_REG(REG_CP1_F09, f9 );
        CASE_COP1_REG(REG_CP1_F10, f10); CASE_COP1_REG(REG_CP1_F11, f11);
        // Subroutine arguments:
        CASE_COP1_REG(REG_CP1_F12, f12); CASE_COP1_REG(REG_CP1_F13, f13);
        CASE_COP1_REG(REG_CP1_F14, f14); CASE_COP1_REG(REG_CP1_F15, f15);
        // Temporary values:
        CASE_COP1_REG(REG_CP1_F16, f16); CASE_COP1_REG(REG_CP1_F17, f17);
        CASE_COP1_REG(REG_CP1_F18, f18); CASE_COP1_REG(REG_CP1_F19, f19);
        // Saved values:
        CASE_COP1_REG(REG_CP1_F20, f20); CASE_COP1_REG(REG_CP1_F21, f21);
        CASE_COP1_REG(REG_CP1_F22, f22); CASE_COP1_REG(REG_CP1_F23, f23);
        CASE_COP1_REG(REG_CP1_F24, f24); CASE_COP1_REG(REG_CP1_F25, f25);
        CASE_COP1_REG(REG_CP1_F26, f26); CASE_COP1_REG(REG_CP1_F27, f27);
        CASE_COP1_REG(REG_CP1_F28, f28); CASE_COP1_REG(REG_CP1_F29, f29);
        CASE_COP1_REG(REG_CP1_F30, f30); CASE_COP1_REG(REG_CP1_F31, f31);
        default: break;
    }
    return val;
}

ALIGNED32 static const RegisterInfo sRegisters_FCR[2] = {
    [REG_DESC_FCR_COP_IMPL_REV  ] = DEF_SREG(       sizeof(u32), "FCR0",  "FC", REG_DESC_FCR_COP_IMPL_REV  ),
    [REG_DESC_FCR_CONTROL_STATUS] = DEF_TREG(fpcsr, sizeof(u32), "FPCSR", "FS", REG_DESC_FCR_CONTROL_STATUS),
};
const char* sRegDesc_FPR[] = {
    [REG_DESC_FCR_COP_IMPL_REV  ] = "coprocessor implementation/revision",
    [REG_DESC_FCR_CONTROL_STATUS] = "floating point ontrol/status",
};
uint64_t get_fcr_reg_val(int idx) {
    int val = 0;
    switch (idx) {
        case REG_FCR_IMPL_REV:       ASM_GET_REG_FCR(val,  "$0"); break;
        case REG_FCR_CONTROL_STATUS: ASM_GET_REG_FCR(val, "$31"); break;
        default: break;
        
    }
    return val;
}


static const RegisterInfo* sRegisters[] = {
    [CPU  + 1] = sRegisters_CPU,
    [COP0 + 1] = sRegisters_COP0,
    [COP1 + 1] = sRegisters_COP1,
    // [COP2 + 1] = sRegisters_COP2,
    // [COP3 + 1] = sRegisters_COP3,
};


// A buffer to save registers to. Used by cs_insn_to_string().
RegisterId gSavedRegBuf[REG_BUFFER_SIZE];
int gSavedRegBufSize = 0;


const RegisterInfo* get_fcr_reg_info(int idx) {
    if (idx == REG_FCR_CONTROL_STATUS) {
        idx = REG_DESC_FCR_CONTROL_STATUS;
    }

    return &sRegisters_FCR[idx];
}
const RegisterInfo* get_reg_info(enum Coprocessors cop, int idx) {
    return ((cop == FCR) ? get_fcr_reg_info(idx) : &sRegisters[cop + 1][idx]);
}

uint64_t get_thread_reg_val(enum Coprocessors cop, int idx, OSThread* thread) {
    const RegisterInfo* info = get_reg_info(cop, idx);

    if (info == NULL) {
        return 0;
    }

    __OSThreadContext* tc = &thread->context;
    Address addr = ((Address)tc + info->offset);

    if (info->size == sizeof(uint64_t)) {
        Doubleword data = 0;
        if (try_read_doubleword_aligned(&data, addr)) {
            return data;
        }
    } else {
        Word data = 0;
        if (try_read_word_aligned(&data, addr)) {
            return data;
        }
    }

    return 0;
}

uint64_t get_direct_reg_val(enum Coprocessors cop, int idx) {
    switch (cop) {
        case CPU:  return get_cpu_reg_val(idx);  break;
        case COP0: return get_cop0_reg_val(idx); break;
        case COP1: return get_cop1_reg_val(idx); break;
        case FCR:  return get_fcr_reg_val(idx);  break;
        default: return 0;
    }
}

uint64_t get_reg_val(enum Coprocessors cop, int idx) {
    const RegisterInfo* info = get_reg_info(cop, idx);

    if ((info != NULL) && (info->offset != OSTHREAD_NULL_OFFSET)) { // If register exists in __OSThreadContext, use the data from the crashed thread.
        return get_thread_reg_val(cop, idx, gInspectThread);
    } else {
        return get_direct_reg_val(cop, idx);
    }

    return 0;
}

const char* get_reg_desc(enum Coprocessors cop, int idx) {
    const RegisterInfo* info = get_reg_info(cop, idx);

    if (info == NULL) {
        return 0;
    }

    switch (cop) {
        case COP0: return sRegDesc_CP0[info->descId];
        case FCR:  return sRegDesc_FPR[info->descId];
        default:   return sRegDesc[info->descId];
    }
}

void clear_saved_reg_buffer(void) {
    bzero(gSavedRegBuf, sizeof(gSavedRegBuf));
    gSavedRegBufSize = 0;
}

void append_reg_to_buffer(enum Coprocessors cop, int idx, _Bool isFlt, _Bool isOutput) {
    if (gSavedRegBufSize < ARRAY_COUNT(gSavedRegBuf)) {
        gSavedRegBuf[gSavedRegBufSize++] = (RegisterId){
            .cop = cop,
            .idx = idx,
            .flt = isFlt,
            .out = isOutput,
        };
    }
}

enum FloatErrorType validate_f32(IEEE754_f32 val) {
    if (val.mantissa != 0) {
        if (val.exponent == 0x00) {
            return FLT_ERR_DENORM; // Denormalized value.
        } else if (val.exponent == 0xFF) {
            return FLT_ERR_NAN; // NaN.
        }
    }

    return FLT_ERR_NONE;
}

enum FloatErrorType validate_f64(IEEE754_f64 val) {
    if (val.mantissa != 0) {
        if (val.exponent == 0x00) {
            return FLT_ERR_DENORM; // Denormalized value.
        } else if (val.exponent == 0xFF) {
            return FLT_ERR_NAN; // NaN.
        }
    }

    return FLT_ERR_NONE;
}
