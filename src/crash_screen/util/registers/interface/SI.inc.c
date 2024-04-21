// -- Serial Interface (SI) --
// https://n64brew.dev/wiki/Serial_Interface


static const char* sRegDesc_SI[] = {
    [REGID_SI_DRAM_ADDR     ] = "DRAM address for DMAs",
    [REGID_SI_PIF_ADDR_RD64B] = "Offset in PIF to read data",
    [REGID_SI_PIF_ADDR_WR64B] = "Data to write to PIF RAM",
    [REGID_SI_STATUS        ] = "SI status",
};
ALIGNED32 static const RegisterInfo sRegInfo_SI[] = {
    [REGID_SI_DRAM_ADDR     ] = DEF_IREG(SI_DRAM_ADDR_REG,      "DRAM_ADDR",         REGID_SI_DRAM_ADDR     ),
    [REGID_SI_PIF_ADDR_RD64B] = DEF_IREG(SI_PIF_ADDR_RD64B_REG, "PIF_ADDR_READ64B",  REGID_SI_PIF_ADDR_RD64B),
    [REGID_SI_PIF_ADDR_WR64B] = DEF_IREG(SI_PIF_ADDR_WR64B_REG, "PIF_ADDR_WRITE64B", REGID_SI_PIF_ADDR_WR64B),
    [REGID_SI_STATUS        ] = DEF_IREG(SI_STATUS_REG,         "STATUS",            REGID_SI_STATUS        ),
};
static const RegisterSource sRegisters_SI = DEF_REG_LIST_INTERFACE(
    "SI",
    "Serial Interface",
    sRegDesc_SI,
    sRegInfo_SI
);
