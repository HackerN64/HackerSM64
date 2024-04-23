// -- RDRAM Interface (RI) --
// https://n64brew.dev/wiki/RDRAM_Interface


static const char* sRegDesc_RI[] = {
    [REGID_RI_MODE        ] = "Mode",
    [REGID_RI_CONFIG      ] = "Config",
    [REGID_RI_CURRENT_LOAD] = "Current load",
    [REGID_RI_SELECT      ] = "Select",
    [REGID_RI_REFRESH     ] = "Refresh",
    [REGID_RI_LATENCY     ] = "Latency",
    [REGID_RI_RERROR      ] = "Read error",
    [REGID_RI_WERROR      ] = "Write error",
};
ALIGNED32 static const RegisterInfo sRegInfo_RI[] = {
    [REGID_RI_MODE        ] = DEF_IREG(RI_MODE_REG,         "MODE",          REGID_RI_MODE        ),
    [REGID_RI_CONFIG      ] = DEF_IREG(RI_CONFIG_REG,       "CONFIG",        REGID_RI_CONFIG      ),
    [REGID_RI_CURRENT_LOAD] = DEF_IREG(RI_CURRENT_LOAD_REG, "CURRENT_LOAD",  REGID_RI_CURRENT_LOAD), //! Write-only?
    [REGID_RI_SELECT      ] = DEF_IREG(RI_SELECT_REG,       "SELECT",        REGID_RI_SELECT      ),
    [REGID_RI_REFRESH     ] = DEF_IREG(RI_REFRESH_REG,      "REFRESH/COUNT", REGID_RI_REFRESH     ),
    [REGID_RI_LATENCY     ] = DEF_IREG(RI_LATENCY_REG,      "LATENCY",       REGID_RI_LATENCY     ),
    [REGID_RI_RERROR      ] = DEF_IREG(RI_RERROR_REG,       "READ_ERROR",    REGID_RI_RERROR      ),
    [REGID_RI_WERROR      ] = DEF_IREG(RI_WERROR_REG,       "WRITE_ERROR",   REGID_RI_WERROR      ),
};
static const RegisterSource sRegisters_RI = DEF_REG_LIST_INTERFACE(
    "RI",
    "Rambus DRAM (RDRAM) Interface",
    sRegDesc_RI,
    sRegInfo_RI
);
