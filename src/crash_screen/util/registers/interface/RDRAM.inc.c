// -- RAMbus DRAM (RDRAM) --
// https://n64brew.dev/wiki/RDRAM


static const char* sRegDesc_RDRAM[] = {
    [REGID_RDRAM_CONFIG      ] = "RDRAM configuration", // Read-only register which describes RDRAM configuration.
    [REGID_RDRAM_DEVICE_ID   ] = "RDRAM base address", // Specifies base address of RDRAM.
    [REGID_RDRAM_DELAY       ] = "CAS timing", // Specifies CAS timing parameters.
    [REGID_RDRAM_MODE        ] = "Operationg mode", // Control operating mode and IOL output current.
    [REGID_RDRAM_REF_INTERVAL] = "Refresh interval", // Specifies refresh interval for devices that require refresh.
    [REGID_RDRAM_REF_ROW     ] = "Next row & bank to be refreshed", // 	Next row and bank to be refreshed.
    [REGID_RDRAM_RAS_INTERVAL] = "RAS access interval", // Specifies RAS access interval.
    [REGID_RDRAM_MIN_INTERVAL] = "Minimum delay information", // Provides minimum delay information and some special control.
    [REGID_RDRAM_ADDR_SELECT ] = "Address select", // Specifies Adr field subufield swapping to maximize hit rate.
    [REGID_RDRAM_DEVICE_MANUF] = "Device manufacturer", // Read-only register providing manufacturer and device information.
    [REGID_RDRAM_ROW         ] = "Address of currently sensed row in each bank", // Address of currently sensed row in each bank.
};
ALIGNED32 static const RegisterInfo sRegInfo_RDRAM[] = {
    [REGID_RDRAM_CONFIG      ] = DEF_IREG(RDRAM_CONFIG_REG,       "CONFIG/DEVICE_TYPE", REGID_RDRAM_CONFIG      ),
    [REGID_RDRAM_DEVICE_ID   ] = DEF_IREG(RDRAM_DEVICE_ID_REG,    "DEVICE_ID",          REGID_RDRAM_DEVICE_ID   ),
    [REGID_RDRAM_DELAY       ] = DEF_IREG(RDRAM_DELAY_REG,        "DELAY",              REGID_RDRAM_DELAY       ),
    [REGID_RDRAM_MODE        ] = DEF_IREG(RDRAM_MODE_REG,         "MODE",               REGID_RDRAM_MODE        ),
    [REGID_RDRAM_REF_INTERVAL] = DEF_IREG(RDRAM_REF_INTERVAL_REG, "REF_INTERVAL",       REGID_RDRAM_REF_INTERVAL),
    [REGID_RDRAM_REF_ROW     ] = DEF_IREG(RDRAM_REF_ROW_REG,      "REF_ROW",            REGID_RDRAM_REF_ROW     ),
    [REGID_RDRAM_RAS_INTERVAL] = DEF_IREG(RDRAM_RAS_INTERVAL_REG, "RAS_INTERVAL",       REGID_RDRAM_RAS_INTERVAL),
    [REGID_RDRAM_MIN_INTERVAL] = DEF_IREG(RDRAM_MIN_INTERVAL_REG, "MIN_INTERVAL",       REGID_RDRAM_MIN_INTERVAL),
    [REGID_RDRAM_ADDR_SELECT ] = DEF_IREG(RDRAM_ADDR_SELECT_REG,  "ADDR_SELECT",        REGID_RDRAM_ADDR_SELECT ),
    [REGID_RDRAM_DEVICE_MANUF] = DEF_IREG(RDRAM_DEVICE_MANUF_REG, "DEVICE_MANUF",       REGID_RDRAM_DEVICE_MANUF),
    [REGID_RDRAM_ROW         ] = DEF_IREG(0x03F00200,             "ROW",                REGID_RDRAM_ROW         ),
};
// Word get_rdram_reg_val(int idx) {
//     Word data = 0x00000000;
//     if (try_read_word_aligned(&data, sRegInfo_RDRAM[idx].addr)) {
//         return data;
//     }
//     return -1;
// }
static const RegisterSource sRegisters_RDRAM = DEF_REG_LIST_INTERFACE(
    "RDRAM",
    "RAMbus DRAM",
    sRegDesc_RDRAM,
    sRegInfo_RDRAM
);
