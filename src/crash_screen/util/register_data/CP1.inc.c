#pragma once

#include "crash_screen/util/registers.h"


// -- COP1 Registers --


//! TODO: Better name format
ALIGNED32 static const RegisterInfo sRegInfo_CP1[CP1_NUM_REGISTERS] = { // even = low bits, odd = high bits.
    // Subroutine return values:
    [REG_CP1_F00] = DEF_CP1_TREG_EVEN(0,  "00", REG_DESC_SUBRET), [REG_CP1_F01] = DEF_CP1_TREG_ODD(0,  "01", REG_DESC_SUBRET),
    [REG_CP1_F02] = DEF_CP1_TREG_EVEN(2,  "02", REG_DESC_SUBRET), [REG_CP1_F03] = DEF_CP1_TREG_ODD(2,  "03", REG_DESC_SUBRET),
    // Temporary values:
    [REG_CP1_F04] = DEF_CP1_TREG_EVEN(4,  "04", REG_DESC_TEMP  ), [REG_CP1_F05] = DEF_CP1_TREG_ODD(4,  "05", REG_DESC_TEMP  ),
    [REG_CP1_F06] = DEF_CP1_TREG_EVEN(6,  "06", REG_DESC_TEMP  ), [REG_CP1_F07] = DEF_CP1_TREG_ODD(6,  "07", REG_DESC_TEMP  ),
    [REG_CP1_F08] = DEF_CP1_TREG_EVEN(8,  "08", REG_DESC_TEMP  ), [REG_CP1_F09] = DEF_CP1_TREG_ODD(8,  "09", REG_DESC_TEMP  ),
    [REG_CP1_F10] = DEF_CP1_TREG_EVEN(10, "10", REG_DESC_TEMP  ), [REG_CP1_F11] = DEF_CP1_TREG_ODD(10, "11", REG_DESC_TEMP  ),
    // Subroutine arguments:
    [REG_CP1_F12] = DEF_CP1_TREG_EVEN(12, "12", REG_DESC_SUBARG), [REG_CP1_F13] = DEF_CP1_TREG_ODD(12, "13", REG_DESC_SUBARG),
    [REG_CP1_F14] = DEF_CP1_TREG_EVEN(14, "14", REG_DESC_SUBARG), [REG_CP1_F15] = DEF_CP1_TREG_ODD(14, "15", REG_DESC_SUBARG),
    // Temporary values:
    [REG_CP1_F16] = DEF_CP1_TREG_EVEN(16, "16", REG_DESC_TEMP  ), [REG_CP1_F17] = DEF_CP1_TREG_ODD(16, "17", REG_DESC_TEMP  ),
    [REG_CP1_F18] = DEF_CP1_TREG_EVEN(18, "18", REG_DESC_TEMP  ), [REG_CP1_F19] = DEF_CP1_TREG_ODD(18, "19", REG_DESC_TEMP  ),
    // Saved values:
    [REG_CP1_F20] = DEF_CP1_TREG_EVEN(20, "20", REG_DESC_SAVED ), [REG_CP1_F21] = DEF_CP1_TREG_ODD(20, "21", REG_DESC_SAVED ),
    [REG_CP1_F22] = DEF_CP1_TREG_EVEN(22, "22", REG_DESC_SAVED ), [REG_CP1_F23] = DEF_CP1_TREG_ODD(22, "23", REG_DESC_SAVED ),
    [REG_CP1_F24] = DEF_CP1_TREG_EVEN(24, "24", REG_DESC_SAVED ), [REG_CP1_F25] = DEF_CP1_TREG_ODD(24, "25", REG_DESC_SAVED ),
    [REG_CP1_F26] = DEF_CP1_TREG_EVEN(26, "26", REG_DESC_SAVED ), [REG_CP1_F27] = DEF_CP1_TREG_ODD(26, "27", REG_DESC_SAVED ),
    [REG_CP1_F28] = DEF_CP1_TREG_EVEN(28, "28", REG_DESC_SAVED ), [REG_CP1_F29] = DEF_CP1_TREG_ODD(28, "29", REG_DESC_SAVED ),
    [REG_CP1_F30] = DEF_CP1_TREG_EVEN(30, "30", REG_DESC_SAVED ), [REG_CP1_F31] = DEF_CP1_TREG_ODD(30, "31", REG_DESC_SAVED ),
};
#define CASE_CP1_REG(_idx, _reg) CASE_REG(COP1, _idx, _reg)
Doubleword get_cp1_reg_val(int idx) {
    Word val = 0;
    switch (idx) {
        // Subroutine return values:
        CASE_CP1_REG(REG_CP1_F00, f0 ); CASE_CP1_REG(REG_CP1_F01, f1 );
        CASE_CP1_REG(REG_CP1_F02, f2 ); CASE_CP1_REG(REG_CP1_F03, f3 );
        // Temporary values:
        CASE_CP1_REG(REG_CP1_F04, f4 ); CASE_CP1_REG(REG_CP1_F05, f5 );
        CASE_CP1_REG(REG_CP1_F06, f6 ); CASE_CP1_REG(REG_CP1_F07, f7 );
        CASE_CP1_REG(REG_CP1_F08, f8 ); CASE_CP1_REG(REG_CP1_F09, f9 );
        CASE_CP1_REG(REG_CP1_F10, f10); CASE_CP1_REG(REG_CP1_F11, f11);
        // Subroutine arguments:
        CASE_CP1_REG(REG_CP1_F12, f12); CASE_CP1_REG(REG_CP1_F13, f13);
        CASE_CP1_REG(REG_CP1_F14, f14); CASE_CP1_REG(REG_CP1_F15, f15);
        // Temporary values:
        CASE_CP1_REG(REG_CP1_F16, f16); CASE_CP1_REG(REG_CP1_F17, f17);
        CASE_CP1_REG(REG_CP1_F18, f18); CASE_CP1_REG(REG_CP1_F19, f19);
        // Saved values:
        CASE_CP1_REG(REG_CP1_F20, f20); CASE_CP1_REG(REG_CP1_F21, f21);
        CASE_CP1_REG(REG_CP1_F22, f22); CASE_CP1_REG(REG_CP1_F23, f23);
        CASE_CP1_REG(REG_CP1_F24, f24); CASE_CP1_REG(REG_CP1_F25, f25);
        CASE_CP1_REG(REG_CP1_F26, f26); CASE_CP1_REG(REG_CP1_F27, f27);
        CASE_CP1_REG(REG_CP1_F28, f28); CASE_CP1_REG(REG_CP1_F29, f29);
        CASE_CP1_REG(REG_CP1_F30, f30); CASE_CP1_REG(REG_CP1_F31, f31);
        default: break;
    }
    return val;
}
static const RegisterSource sRegisters_CP1 = DEF_REG_LIST_PROCESSOR(
    "CP1",
    "Floating-Point Unit (FPU)",
    get_cp1_reg_val,
    sRegDesc_Default,
    sRegInfo_CP1
);


// -- FCR Registers --

static const char* sRegDesc_FCR[] = {
    [REG_DESC_FCR_COP_IMPL_REV  ] = "coprocessor implementation/revision",
    [REG_DESC_FCR_CONTROL_STATUS] = "floating point control/status",
};
ALIGNED32 static const RegisterInfo sRegInfo_FCR[2] = {
    [REG_DESC_FCR_COP_IMPL_REV  ] = DEF_SREG(       sizeof(u32), "FCR0",  "FC", REG_DESC_FCR_COP_IMPL_REV  ),
    [REG_DESC_FCR_CONTROL_STATUS] = DEF_TREG(fpcsr, sizeof(u32), "FPCSR", "FS", REG_DESC_FCR_CONTROL_STATUS),
};
const RegisterInfo* get_fcr_reg_info(int idx) {
    if (idx == REG_FCR_CONTROL_STATUS) {
        idx = REG_DESC_FCR_CONTROL_STATUS;
    }

    return &sRegInfo_FCR[idx];
}
Doubleword get_fcr_reg_val(int idx) {
    Word val = 0;
    switch (idx) {
        case REG_FCR_IMPL_REV:       ASM_GET_REG_FCR(val,  "$0"); break;
        case REG_FCR_CONTROL_STATUS: ASM_GET_REG_FCR(val, "$31"); break;
        default: break;
    }
    return val;
}
static const RegisterSource sRegisters_FCR = DEF_REG_LIST_PROCESSOR_FUNC(
    "FCR",
    "CP1 Control/Status",
    get_fcr_reg_val,
    sRegDesc_FCR,
    sRegInfo_FCR,
    get_fcr_reg_info
);
