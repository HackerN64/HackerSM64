#pragma once

#include "crash_screen/util/registers.h"


// -- COP0 Registers --


static const char* sRegDesc_CP0[] = {
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
ALIGNED32 static const RegisterInfo sRegInfo_CP0[CP0_NUM_REGISTERS] = {
    [REG_CP0_INX      ] = DEF_CP0_SREG(C0_INX,       u32,           "Index",    "IX", REG_DESC_CP0_INX      ),
    [REG_CP0_RAND     ] = DEF_CP0_SREG(C0_RAND,      u32,           "Random",   "RN", REG_DESC_CP0_RAND     ),
    [REG_CP0_ENTRYLO0 ] = DEF_CP0_SREG(C0_ENTRYLO0,  u64,           "EntryLo0", "L0", REG_DESC_CP0_ENTRYLO0 ),
    [REG_CP0_ENTRYLO1 ] = DEF_CP0_SREG(C0_ENTRYLO1,  u64,           "EntryLo1", "L1", REG_DESC_CP0_ENTRYLO1 ),
    [REG_CP0_CONTEXT  ] = DEF_CP0_SREG(C0_CONTEXT,   u64,           "Context",  "CX", REG_DESC_CP0_CONTEXT  ),
    [REG_CP0_PAGEMASK ] = DEF_CP0_SREG(C0_PAGEMASK,  u32,           "PageMask", "PM", REG_DESC_CP0_PAGEMASK ),
    [REG_CP0_WIRED    ] = DEF_CP0_SREG(C0_WIRED,     u32,           "Wired",    "WR", REG_DESC_CP0_WIRED    ),
    [REG_CP0_7        ] = DEF_CP0_SREG(7,            int,           "07",       "07", REG_DESC_CP0_RESERVED ),
    [REG_CP0_BADVADDR ] = DEF_CP0_TREG(C0_BADVADDR,  u64, badvaddr, "BadVAddr", "VA", REG_DESC_CP0_BADVADDR ),
    [REG_CP0_COUNT    ] = DEF_CP0_SREG(C0_COUNT,     u32,           "Count",    "CT", REG_DESC_CP0_COUNT    ),
    [REG_CP0_ENTRYHI  ] = DEF_CP0_SREG(C0_ENTRYHI,   u64,           "EntryHi",  "EH", REG_DESC_CP0_ENTRYHI  ),
    [REG_CP0_COMPARE  ] = DEF_CP0_SREG(C0_COMPARE,   u32,           "Compare",  "CP", REG_DESC_CP0_COMPARE  ),
    [REG_CP0_SR       ] = DEF_CP0_TREG(C0_SR,        u32, sr,       "Status",   "SR", REG_DESC_CP0_SR       ),
    [REG_CP0_CAUSE    ] = DEF_CP0_TREG(C0_CAUSE,     u32, cause,    "Cause",    "CR", REG_DESC_CP0_CAUSE    ),
    [REG_CP0_EPC      ] = DEF_CP0_TREG(C0_EPC,       u64, pc,       "EPC",      "PC", REG_DESC_CP0_EPC      ),
    [REG_CP0_PRID     ] = DEF_CP0_SREG(C0_PRID,      u32,           "PRId",     "PR", REG_DESC_CP0_PRID     ),
    [REG_CP0_CONFIG   ] = DEF_CP0_SREG(C0_CONFIG,    u32,           "Config",   "CF", REG_DESC_CP0_CONFIG   ),
    [REG_CP0_LLADDR   ] = DEF_CP0_SREG(C0_LLADDR,    u32,           "LLAddr",   "LL", REG_DESC_CP0_LLADDR   ),
    [REG_CP0_WATCHLO  ] = DEF_CP0_SREG(C0_WATCHLO,   u32,           "WatchLo",  "WL", REG_DESC_CP0_WATCHLO  ),
    [REG_CP0_WATCHHI  ] = DEF_CP0_SREG(C0_WATCHHI,   u32,           "WatchHi",  "WH", REG_DESC_CP0_WATCHHI  ),
    [REG_CP0_XCONTEXT ] = DEF_CP0_SREG(20,           u64,           "XContext", "XX", REG_DESC_CP0_XCONTEXT ),
    [REG_CP0_21       ] = DEF_CP0_SREG(21,           int,           "21",       "21", REG_DESC_CP0_RESERVED ),
    [REG_CP0_22       ] = DEF_CP0_SREG(22,           int,           "22",       "22", REG_DESC_CP0_RESERVED ),
    [REG_CP0_23       ] = DEF_CP0_SREG(23,           int,           "23",       "23", REG_DESC_CP0_RESERVED ),
    [REG_CP0_24       ] = DEF_CP0_SREG(24,           int,           "24",       "24", REG_DESC_CP0_RESERVED ),
    [REG_CP0_25       ] = DEF_CP0_SREG(25,           int,           "25",       "25", REG_DESC_CP0_RESERVED ),
    [REG_CP0_ECC      ] = DEF_CP0_SREG(C0_ECC,       u32,           "PErr",     "PE", REG_DESC_CP0_ECC      ),
    [REG_CP0_CACHE_ERR] = DEF_CP0_SREG(C0_CACHE_ERR, u32,           "CacheErr", "CE", REG_DESC_CP0_CACHE_ERR),
    [REG_CP0_TAGLO    ] = DEF_CP0_SREG(C0_TAGLO,     u32,           "TagLo",    "TL", REG_DESC_CP0_TAGLO    ),
    [REG_CP0_TAGHI    ] = DEF_CP0_SREG(C0_TAGHI,     u32,           "TagHi",    "TH", REG_DESC_CP0_TAGHI    ),
    [REG_CP0_ERROR_EPC] = DEF_CP0_SREG(C0_ERROR_EPC, u64,           "ErrorEPC", "EE", REG_DESC_CP0_ERROR_EPC),
    [REG_CP0_31       ] = DEF_CP0_SREG(31,           int,           "31",       "31", REG_DESC_CP0_RESERVED ),
};
#define CASE_CP0_REG(_idx, _reg) CASE_REG(COP0, _idx, _reg)
Doubleword get_cp0_reg_val(int idx) {
    Word val = 0;
    switch (idx) {
        CASE_CP0_REG(REG_CP0_INX,       C0_INX      );
        CASE_CP0_REG(REG_CP0_RAND,      C0_RAND     );
        CASE_CP0_REG(REG_CP0_ENTRYLO0,  C0_ENTRYLO0 );
        CASE_CP0_REG(REG_CP0_ENTRYLO1,  C0_ENTRYLO1 );
        CASE_CP0_REG(REG_CP0_CONTEXT,   C0_CONTEXT  );
        CASE_CP0_REG(REG_CP0_PAGEMASK,  C0_PAGEMASK );
        CASE_CP0_REG(REG_CP0_WIRED,     C0_WIRED    );
        CASE_CP0_REG(REG_CP0_7,         7           );
        CASE_CP0_REG(REG_CP0_BADVADDR,  C0_BADVADDR );
        CASE_CP0_REG(REG_CP0_COUNT,     C0_COUNT    );
        CASE_CP0_REG(REG_CP0_ENTRYHI,   C0_ENTRYHI  );
        CASE_CP0_REG(REG_CP0_COMPARE,   C0_COMPARE  );
        CASE_CP0_REG(REG_CP0_SR,        C0_SR       );
        CASE_CP0_REG(REG_CP0_CAUSE,     C0_CAUSE    );
        CASE_CP0_REG(REG_CP0_EPC,       C0_EPC      );
        CASE_CP0_REG(REG_CP0_PRID,      C0_PRID     );
        CASE_CP0_REG(REG_CP0_CONFIG,    C0_CONFIG   );
        CASE_CP0_REG(REG_CP0_LLADDR,    C0_LLADDR   );
        CASE_CP0_REG(REG_CP0_WATCHLO,   C0_WATCHLO  );
        CASE_CP0_REG(REG_CP0_WATCHHI,   C0_WATCHHI  );
        CASE_CP0_REG(REG_CP0_XCONTEXT,  20          );
        CASE_CP0_REG(REG_CP0_21,        21          );
        CASE_CP0_REG(REG_CP0_22,        22          );
        CASE_CP0_REG(REG_CP0_23,        23          );
        CASE_CP0_REG(REG_CP0_24,        24          );
        CASE_CP0_REG(REG_CP0_25,        25          );
        CASE_CP0_REG(REG_CP0_ECC,       C0_ECC      );
        CASE_CP0_REG(REG_CP0_CACHE_ERR, C0_CACHE_ERR);
        CASE_CP0_REG(REG_CP0_TAGLO,     C0_TAGLO    );
        CASE_CP0_REG(REG_CP0_TAGHI,     C0_TAGHI    );
        CASE_CP0_REG(REG_CP0_ERROR_EPC, C0_ERROR_EPC);
        CASE_CP0_REG(REG_CP0_31,        31          );
        default: break;
    }
    return val;
}
static const RegisterSource sRegisters_CP0 = DEF_REG_LIST_PROCESSOR(
    "CP0",
    "System Control Processor",
    get_cp0_reg_val,
    sRegDesc_CP0,
    sRegInfo_CP0
);
