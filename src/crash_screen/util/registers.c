#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/crash_main.h"
#include "map_parser.h"
#include "memory_read.h"

#include "registers.h"


#define CASE_REG(_cop, _idx, _reg) case _idx: ASM_GET_REG_##_cop(val, STR_REG_PREFIX TO_STRING2(_reg)); break;


// -- CPU Register info --

ALIGNED32 static const RegisterInfo sRegisters_CPU[CPU_NUM_REGISTERS] = {
    [REG_CPU_R0] = DEF_CPU_SREG(zero, "R0"),
    [REG_CPU_AT] = DEF_CPU_TREG(at, "AT"),
    [REG_CPU_V0] = DEF_CPU_TREG(v0, "V0"), [REG_CPU_V1] = DEF_CPU_TREG(v1, "V1"),
    [REG_CPU_A0] = DEF_CPU_TREG(a0, "A0"), [REG_CPU_A1] = DEF_CPU_TREG(a1, "A1"), [REG_CPU_A2] = DEF_CPU_TREG(a2, "A2"), [REG_CPU_A3] = DEF_CPU_TREG(a3, "A3"),
    [REG_CPU_T0] = DEF_CPU_TREG(t0, "T0"), [REG_CPU_T1] = DEF_CPU_TREG(t1, "T1"), [REG_CPU_T2] = DEF_CPU_TREG(t0, "T2"), [REG_CPU_T3] = DEF_CPU_TREG(t3, "T3"), [REG_CPU_T4] = DEF_CPU_TREG(t4, "T4"), [REG_CPU_T5] = DEF_CPU_TREG(t5, "T5"), [REG_CPU_T6] = DEF_CPU_TREG(t6, "T6"), [REG_CPU_T7] = DEF_CPU_TREG(t7, "T7"),
    [REG_CPU_S0] = DEF_CPU_TREG(s0, "S0"), [REG_CPU_S1] = DEF_CPU_TREG(s1, "S1"), [REG_CPU_S2] = DEF_CPU_TREG(s0, "S2"), [REG_CPU_S3] = DEF_CPU_TREG(s3, "S3"), [REG_CPU_S4] = DEF_CPU_TREG(s4, "S4"), [REG_CPU_S5] = DEF_CPU_TREG(s5, "S5"), [REG_CPU_S6] = DEF_CPU_TREG(s6, "S6"), [REG_CPU_S7] = DEF_CPU_TREG(s7, "S7"),
    [REG_CPU_T8] = DEF_CPU_TREG(t8, "T8"), [REG_CPU_T9] = DEF_CPU_TREG(t9, "T9"),
    [REG_CPU_K0] = DEF_CPU_SREG(k0, "K0"), [REG_CPU_K1] = DEF_CPU_SREG(k1, "K1"),
    [REG_CPU_GP] = DEF_CPU_TREG(gp, "GP"),
    [REG_CPU_SP] = DEF_CPU_TREG(sp, "SP"),
    [REG_CPU_FP] = DEF_CPU_TREG(s8, "FP"),
    [REG_CPU_RA] = DEF_CPU_TREG(ra, "RA"),
};

#define CASE_CPU_REG(_idx, _reg) CASE_REG(CPU, _idx, _reg)
uint64_t get_cpu_reg_val(enum CPURegisters idx) {
    uint64_t val = 0;
    switch (idx) {
        CASE_CPU_REG(REG_CPU_R0, zero);
        //! TODO: Directly accessing $at requires ".set noat".
        // CASE_CPU_REG(REG_CPU_AT, at);
        case REG_CPU_AT: return gCrashedThread->context.at;
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

ALIGNED32 static const RegisterInfo sRegisters_COP0[COP0_NUM_REGISTERS] = {
    [REG_COP0_INX      ] = DEF_COP0_SREG(C0_INX,       sizeof(u32),           "Index",     "IX"),
    [REG_COP0_RAND     ] = DEF_COP0_SREG(C0_RAND,      sizeof(u32),           "Random",    "RN"),
    [REG_COP0_ENTRYLO0 ] = DEF_COP0_SREG(C0_ENTRYLO0,  sizeof(u64),           "EntryLo0",  "L0"),
    [REG_COP0_ENTRYLO1 ] = DEF_COP0_SREG(C0_ENTRYLO1,  sizeof(u64),           "EntryLo1",  "L1"),
    [REG_COP0_CONTEXT  ] = DEF_COP0_SREG(C0_CONTEXT,   sizeof(u64),           "Context",   "CX"),
    [REG_COP0_PAGEMASK ] = DEF_COP0_SREG(C0_PAGEMASK,  sizeof(u32),           "PageMask",  "PM"),
    [REG_COP0_WIRED    ] = DEF_COP0_SREG(C0_WIRED,     sizeof(u32),           "Wired",     "WR"),
    [REG_COP0_7        ] = DEF_COP0_SREG(7,            sizeof(int),           "07",        "07"),
    [REG_COP0_BADVADDR ] = DEF_COP0_TREG(C0_BADVADDR,  sizeof(u64), badvaddr, "BadVAddr",  "VA"),
    [REG_COP0_COUNT    ] = DEF_COP0_SREG(C0_COUNT,     sizeof(u32),           "Count",     "CT"),
    [REG_COP0_ENTRYHI  ] = DEF_COP0_SREG(C0_ENTRYHI,   sizeof(u64),           "EntryHi",   "EH"),
    [REG_COP0_COMPARE  ] = DEF_COP0_SREG(C0_COMPARE,   sizeof(u32),           "Compare",   "CP"),
    [REG_COP0_SR       ] = DEF_COP0_TREG(C0_SR,        sizeof(u32), sr,       "Status",    "SR"),
    [REG_COP0_CAUSE    ] = DEF_COP0_TREG(C0_CAUSE,     sizeof(u32), cause,    "Cause",     "CS"), //! TODO: Is this the same as tc->cause?
    [REG_COP0_EPC      ] = DEF_COP0_TREG(C0_EPC,       sizeof(u64), pc,       "EPC",       "PC"), //! TODO: Are pc and epc the same? They seem to be different sizes.
    [REG_COP0_PRID     ] = DEF_COP0_SREG(C0_PRID,      sizeof(u32),           "PRId",      "PR"),
    [REG_COP0_CONFIG   ] = DEF_COP0_SREG(C0_CONFIG,    sizeof(u32),           "Config",    "CF"),
    [REG_COP0_LLADDR   ] = DEF_COP0_SREG(C0_LLADDR,    sizeof(u32),           "LLAddr",    "LL"),
    [REG_COP0_WATCHLO  ] = DEF_COP0_SREG(C0_WATCHLO,   sizeof(u32),           "WatchLo",   "WL"),
    [REG_COP0_WATCHHI  ] = DEF_COP0_SREG(C0_WATCHHI,   sizeof(u32),           "WatchHi",   "WH"),
    [REG_COP0_XCONTEXT ] = DEF_COP0_SREG(20,           sizeof(u64),           "XContext",  "XX"),
    [REG_COP0_21       ] = DEF_COP0_SREG(21,           sizeof(int),           "21",        "21"),
    [REG_COP0_22       ] = DEF_COP0_SREG(22,           sizeof(int),           "22",        "22"),
    [REG_COP0_23       ] = DEF_COP0_SREG(23,           sizeof(int),           "23",        "23"),
    [REG_COP0_24       ] = DEF_COP0_SREG(24,           sizeof(int),           "24",        "24"),
    [REG_COP0_25       ] = DEF_COP0_SREG(25,           sizeof(int),           "25",        "25"),
    [REG_COP0_ECC      ] = DEF_COP0_SREG(C0_ECC,       sizeof(u32),           "ParityErr", "PE"),
    [REG_COP0_CACHE_ERR] = DEF_COP0_SREG(C0_CACHE_ERR, sizeof(u32),           "CacheErr",  "CE"),
    [REG_COP0_TAGLO    ] = DEF_COP0_SREG(C0_TAGLO,     sizeof(u32),           "TagLo",     "TL"),
    [REG_COP0_TAGHI    ] = DEF_COP0_SREG(C0_TAGHI,     sizeof(u32),           "TagHi",     "TH"),
    [REG_COP0_ERROR_EPC] = DEF_COP0_SREG(C0_ERROR_EPC, sizeof(u64),           "ErrorEPC",  "EE"),
    [REG_COP0_31       ] = DEF_COP0_SREG(31,           sizeof(int),           "31",        "31"),
};

#define CASE_COP0_REG(_idx, _reg) CASE_REG(COP0, _idx, _reg)
uint64_t get_cop0_reg_val(enum COP0Registers idx) {
    uint64_t val = 0;
    switch (idx) {
        CASE_COP0_REG(REG_COP0_INX,       C0_INX      );
        CASE_COP0_REG(REG_COP0_RAND,      C0_RAND     );
        CASE_COP0_REG(REG_COP0_ENTRYLO0,  C0_ENTRYLO0 );
        CASE_COP0_REG(REG_COP0_ENTRYLO1,  C0_ENTRYLO1 );
        CASE_COP0_REG(REG_COP0_CONTEXT,   C0_CONTEXT  );
        CASE_COP0_REG(REG_COP0_PAGEMASK,  C0_PAGEMASK );
        CASE_COP0_REG(REG_COP0_WIRED,     C0_WIRED    );
        CASE_COP0_REG(REG_COP0_7,         7           );
        CASE_COP0_REG(REG_COP0_BADVADDR,  C0_BADVADDR );
        CASE_COP0_REG(REG_COP0_COUNT,     C0_COUNT    );
        CASE_COP0_REG(REG_COP0_ENTRYHI,   C0_ENTRYHI  );
        CASE_COP0_REG(REG_COP0_COMPARE,   C0_COMPARE  );
        CASE_COP0_REG(REG_COP0_SR,        C0_SR       );
        CASE_COP0_REG(REG_COP0_CAUSE,     C0_CAUSE    );
        CASE_COP0_REG(REG_COP0_EPC,       C0_EPC      );
        CASE_COP0_REG(REG_COP0_PRID,      C0_PRID     );
        CASE_COP0_REG(REG_COP0_CONFIG,    C0_CONFIG   );
        CASE_COP0_REG(REG_COP0_LLADDR,    C0_LLADDR   );
        CASE_COP0_REG(REG_COP0_WATCHLO,   C0_WATCHLO  );
        CASE_COP0_REG(REG_COP0_WATCHHI,   C0_WATCHHI  );
        CASE_COP0_REG(REG_COP0_XCONTEXT,  20          );
        CASE_COP0_REG(REG_COP0_21,        21          );
        CASE_COP0_REG(REG_COP0_22,        22          );
        CASE_COP0_REG(REG_COP0_23,        23          );
        CASE_COP0_REG(REG_COP0_24,        24          );
        CASE_COP0_REG(REG_COP0_25,        25          );
        CASE_COP0_REG(REG_COP0_ECC,       C0_ECC      );
        CASE_COP0_REG(REG_COP0_CACHE_ERR, C0_CACHE_ERR);
        CASE_COP0_REG(REG_COP0_TAGLO,     C0_TAGLO    );
        CASE_COP0_REG(REG_COP0_TAGHI,     C0_TAGHI    );
        CASE_COP0_REG(REG_COP0_ERROR_EPC, C0_ERROR_EPC);
        CASE_COP0_REG(REG_COP0_31,        31          );
        default: break;
    }
    return val;
}


// -- COP1 Register info --

//! TODO: Better name format
ALIGNED32 static const RegisterInfo sRegisters_COP1[COP1_NUM_REGISTERS] = {
    [REG_COP1_F00] = DEF_COP1_TREG_EVEN(0,  "00"), [REG_COP1_F02] = DEF_COP1_TREG_EVEN(2,  "02"),
    [REG_COP1_F04] = DEF_COP1_TREG_EVEN(4,  "04"), [REG_COP1_F06] = DEF_COP1_TREG_EVEN(6,  "06"), [REG_COP1_F08] = DEF_COP1_TREG_EVEN(8,  "08"), [REG_COP1_F10] = DEF_COP1_TREG_EVEN(10, "10"),
    [REG_COP1_F12] = DEF_COP1_TREG_EVEN(12, "12"), [REG_COP1_F14] = DEF_COP1_TREG_EVEN(14, "14"),
    [REG_COP1_F16] = DEF_COP1_TREG_EVEN(16, "16"), [REG_COP1_F18] = DEF_COP1_TREG_EVEN(18, "18"),
    [REG_COP1_F20] = DEF_COP1_TREG_EVEN(20, "20"), [REG_COP1_F22] = DEF_COP1_TREG_EVEN(22, "22"), [REG_COP1_F24] = DEF_COP1_TREG_EVEN(24, "24"), [REG_COP1_F26] = DEF_COP1_TREG_EVEN(26, "26"), [REG_COP1_F28] = DEF_COP1_TREG_EVEN(28, "28"), [REG_COP1_F30] = DEF_COP1_TREG_EVEN(30, "30"),
};

#define CASE_COP1_REG(_idx, _reg) CASE_REG(COP1, _idx, _reg)
uint64_t get_cop1_reg_val(enum COP1Registers idx) {
    uint64_t val = 0;
    switch (idx) {
        CASE_COP1_REG(REG_COP1_F00, f0 ); CASE_COP1_REG(REG_COP1_F02, f2 );
        CASE_COP1_REG(REG_COP1_F04, f4 ); CASE_COP1_REG(REG_COP1_F06, f6 ); CASE_COP1_REG(REG_COP1_F08, f8 ); CASE_COP1_REG(REG_COP1_F10, f10);
        CASE_COP1_REG(REG_COP1_F12, f12); CASE_COP1_REG(REG_COP1_F14, f14);
        CASE_COP1_REG(REG_COP1_F16, f16); CASE_COP1_REG(REG_COP1_F18, f18);
        CASE_COP1_REG(REG_COP1_F20, f20); CASE_COP1_REG(REG_COP1_F22, f22); CASE_COP1_REG(REG_COP1_F24, f24); CASE_COP1_REG(REG_COP1_F26, f26); CASE_COP1_REG(REG_COP1_F28, f28); CASE_COP1_REG(REG_COP1_F30, f30);
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


const RegisterInfo* get_reg_info(enum Coprocessors cop, int idx) {
    return &sRegisters[cop + 1][idx];
}

uint64_t get_direct_reg_val(enum Coprocessors cop, int idx) {
    switch (cop) {
        case CPU:  return get_cpu_reg_val(idx);  break;
        case COP0: return get_cop0_reg_val(idx); break;
        case COP1: return get_cop1_reg_val(idx); break;
        default: return 0;
    }
}

uint64_t get_reg_val(enum Coprocessors cop, int idx) {
    const RegisterInfo* info = get_reg_info(cop, idx);

    if (info == NULL) {
        return 0;
    }

    if (info->offset != OSTHREAD_NULL_OFFSET) { // If register exists in __OSThreadContext, use the data from the crashed thread.
        __OSThreadContext* tc = &gCrashedThread->context;
        Address addr = ((Address)tc + info->offset);

        if (info->size == sizeof(uint64_t)) {
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
    } else {
        return get_direct_reg_val(cop, idx);
    }

    return 0;
}

void clear_saved_reg_buffer(void) {
    bzero(gSavedRegBuf, sizeof(gSavedRegBuf));
    gSavedRegBufSize = 0;
}

void append_reg_to_buffer(s16 cop, s16 idx) {
    if (gSavedRegBufSize < ARRAY_COUNT(gSavedRegBuf)) {
        gSavedRegBuf[gSavedRegBufSize++] = (RegisterId){ .cop = cop, .idx = idx, };
    }
}

enum FloatErrorType validate_float(IEEE754_f32 val) {
    if (val.mantissa != 0) {
        if (val.exponent == 0x00) {
            return FLT_ERR_DENORM; // Denormalized value.
        } else if (val.exponent == 0xFF) {
            return FLT_ERR_NAN; // NaN.
        }
    }

    return FLT_ERR_NONE;
}
