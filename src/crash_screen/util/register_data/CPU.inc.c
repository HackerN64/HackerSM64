#pragma once

#include "crash_screen/util/registers.h"


// -- CPU Registers --


#define DEF_CPU_SREG(_reg, _name, _sureAddr, _descId) DEF_SREG(      sizeof(u64), _name, _name, _sureAddr, _descId)
#define DEF_CPU_TREG(_reg, _name, _sureAddr, _descId) DEF_TREG(_reg, sizeof(u64), _name, _name, _sureAddr, _descId)
ALIGNED32 static const RegisterInfo sRegInfo_CPU[CPU_NUM_REGISTERS] = {
    [REG_CPU_R0] = DEF_CPU_SREG(zero, "R0", FALSE, REG_DESC_ZERO),
    [REG_CPU_AT] = DEF_CPU_TREG(at, "AT", FALSE, REG_DESC_AT    ),
    [REG_CPU_V0] = DEF_CPU_TREG(v0, "V0", FALSE, REG_DESC_SUBRET), [REG_CPU_V1] = DEF_CPU_TREG(v1, "V1", FALSE, REG_DESC_SUBRET),
    [REG_CPU_A0] = DEF_CPU_TREG(a0, "A0", FALSE, REG_DESC_SUBARG), [REG_CPU_A1] = DEF_CPU_TREG(a1, "A1", FALSE, REG_DESC_SUBARG), [REG_CPU_A2] = DEF_CPU_TREG(a2, "A2", FALSE, REG_DESC_SUBARG), [REG_CPU_A3] = DEF_CPU_TREG(a3, "A3", FALSE, REG_DESC_SUBARG),
    [REG_CPU_T0] = DEF_CPU_TREG(t0, "T0", FALSE, REG_DESC_TEMP  ), [REG_CPU_T1] = DEF_CPU_TREG(t1, "T1", FALSE, REG_DESC_TEMP  ), [REG_CPU_T2] = DEF_CPU_TREG(t0, "T2", FALSE, REG_DESC_TEMP  ), [REG_CPU_T3] = DEF_CPU_TREG(t3, "T3", FALSE, REG_DESC_TEMP  ), [REG_CPU_T4] = DEF_CPU_TREG(t4, "T4", FALSE, REG_DESC_TEMP  ), [REG_CPU_T5] = DEF_CPU_TREG(t5, "T5", FALSE, REG_DESC_TEMP  ), [REG_CPU_T6] = DEF_CPU_TREG(t6, "T6", FALSE, REG_DESC_TEMP  ), [REG_CPU_T7] = DEF_CPU_TREG(t7, "T7", FALSE, REG_DESC_TEMP  ),
    [REG_CPU_S0] = DEF_CPU_TREG(s0, "S0", FALSE, REG_DESC_SAVED ), [REG_CPU_S1] = DEF_CPU_TREG(s1, "S1", FALSE, REG_DESC_SAVED ), [REG_CPU_S2] = DEF_CPU_TREG(s0, "S2", FALSE, REG_DESC_SAVED ), [REG_CPU_S3] = DEF_CPU_TREG(s3, "S3", FALSE, REG_DESC_SAVED ), [REG_CPU_S4] = DEF_CPU_TREG(s4, "S4", FALSE, REG_DESC_SAVED ), [REG_CPU_S5] = DEF_CPU_TREG(s5, "S5", FALSE, REG_DESC_SAVED ), [REG_CPU_S6] = DEF_CPU_TREG(s6, "S6", FALSE, REG_DESC_SAVED ), [REG_CPU_S7] = DEF_CPU_TREG(s7, "S7", FALSE, REG_DESC_SAVED ),
    [REG_CPU_T8] = DEF_CPU_TREG(t8, "T8", FALSE, REG_DESC_TEMP  ), [REG_CPU_T9] = DEF_CPU_TREG(t9, "T9", FALSE, REG_DESC_TEMP  ),
    [REG_CPU_K0] = DEF_CPU_SREG(k0, "K0", FALSE, REG_DESC_KERNEL), [REG_CPU_K1] = DEF_CPU_SREG(k1, "K1", FALSE, REG_DESC_KERNEL),
    [REG_CPU_GP] = DEF_CPU_TREG(gp, "GP", TRUE,  REG_DESC_GP    ),
    [REG_CPU_SP] = DEF_CPU_TREG(sp, "SP", TRUE,  REG_DESC_SP    ),
    [REG_CPU_FP] = DEF_CPU_TREG(s8, "FP", FALSE, REG_DESC_FP    ), //! TODO: "FP" or "S8"?
    [REG_CPU_RA] = DEF_CPU_TREG(ra, "RA", TRUE,  REG_DESC_RA    ),
};
#define CASE_CPU_REG(_idx, _reg) CASE_REG(CPU, _idx, _reg)
Doubleword get_cpu_reg_val(int idx) {
    Word val = 0;
    switch (idx) {
        CASE_CPU_REG(REG_CPU_R0, zero);
        //! TODO: Directly accessing $at requires ".set noat".
        // CASE_CPU_REG(REG_CPU_AT, at);
        case REG_CPU_AT: val = __osRunningThread->context.at; break;
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
static const RegisterSource sRegisters_CPU = DEF_REG_LIST_PROCESSOR(
    "CPU",
    "Central Processung Unit",
    get_cpu_reg_val,
    sRegDesc_Default,
    sRegInfo_CPU
);
