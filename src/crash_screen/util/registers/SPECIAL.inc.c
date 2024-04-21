// -- SPECIAL Registers --


static const char* sRegDesc_SPECIAL[] = {
    // [REG_SPC_PC   ] = "program counter",
    [REG_SPC_LO   ] = "special register LO",
    [REG_SPC_HI   ] = "special register HI",
    // [REG_SPC_LLBIT] = "load linked bit",
    [REG_SPC_RCP  ] = "thread rcp",
};
ALIGNED32 static const RegisterInfo sRegInfo_SPECIAL[] = {
    [REG_SPC_LO   ] = DEF_TREG(lo,  sizeof(u64), "LO",    "LO", REG_SPC_LO   ),
    [REG_SPC_HI   ] = DEF_TREG(hi,  sizeof(u64), "HI",    "HI", REG_SPC_HI   ),
    // [REG_SPC_LLBIT] = DEF_SREG(     sizeof(u32), "LLBit", "LL", REG_SPC_LLBIT),
    [REG_SPC_RCP  ] = DEF_TREG(rcp, sizeof(u32), "RCP",   "RC", REG_SPC_RCP  ),
};
Doubleword get_special_reg_val(int idx) {
    Word val = 0;
    switch (idx) {
        case REG_SPC_LO:    asm volatile("mflo %0":"=r"(val):); break;
        case REG_SPC_HI:    asm volatile("mfhi %0":"=r"(val):); break;
        // case REG_SPC_LLBIT: asm volatile("sc %0,()":"=r"(val):); break;
        case REG_SPC_RCP:   return __osRunningThread->context.rcp;
        default: break;
    }
    return val;
}
static const RegisterSource sRegisters_SPECIAL = DEF_REG_LIST_PROCESSOR(
    "special",
    "special registers",
    get_special_reg_val,
    sRegDesc_SPECIAL,
    sRegInfo_SPECIAL
);
