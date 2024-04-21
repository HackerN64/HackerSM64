// -- MIPS Interface (MI) --
// https://n64brew.dev/wiki/MIPS_Interface


static const char* sRegDesc_MI[] = {
    [REGID_MI_INIT_MODE] = "Init mode",
    [REGID_MI_VERSION  ] = "Version",
    [REGID_MI_INTR     ] = "Interrupt",
    [REGID_MI_INTR_MASK] = "Interrupt mask",
};
ALIGNED32 static const RegisterInfo sRegInfo_MI[] = {
    [REGID_MI_INIT_MODE] = DEF_IREG(MI_INIT_MODE_REG, "INIT_MODE/MODE", REGID_MI_INIT_MODE),
    [REGID_MI_VERSION  ] = DEF_IREG(MI_VERSION_REG,   "VERSION/NOOP",   REGID_MI_VERSION  ),
    [REGID_MI_INTR     ] = DEF_IREG(MI_INTR_REG,      "INTERRUPT",      REGID_MI_INTR     ),
    [REGID_MI_INTR_MASK] = DEF_IREG(MI_INTR_MASK_REG, "INTERRUPT MASK", REGID_MI_INTR_MASK),
};
static const RegisterSource sRegisters_MI = DEF_REG_LIST_INTERFACE(
    "MI",
    "MIPS Interface",
    sRegDesc_MI,
    sRegInfo_MI
);
