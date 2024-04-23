#pragma once

#include "crash_screen/util/registers.h"


// -- Peripheral Interface (PI) --
// https://n64brew.dev/wiki/Peripheral_Interface


static const char* sRegDesc_PI[] = {
    [REGID_PI_DRAM_ADDR   ] = "DRAM address",
    [REGID_PI_CART_ADDR   ] = "Cartridge address",
    [REGID_PI_RD_LEN      ] = "Read length",
    [REGID_PI_WR_LEN      ] = "Write length",
    [REGID_PI_STATUS      ] = "PI status",
    [REGID_PI_BSD_DOM1_LAT] = "DOM1 latency",
    [REGID_PI_BSD_DOM1_PWD] = "DOM1 pulse width",
    [REGID_PI_BSD_DOM1_PGS] = "DOM1 page size",
    [REGID_PI_BSD_DOM1_RLS] = "DOM1 release",
    [REGID_PI_BSD_DOM2_LAT] = "DOM2 latency",
    [REGID_PI_BSD_DOM2_PWD] = "DOM2 pulse width",
    [REGID_PI_BSD_DOM2_PGS] = "DOM2 page size",
    [REGID_PI_BSD_DOM2_RLS] = "DOM2 release",
};
ALIGNED32 static const RegisterInfo sRegInfo_PI[] = {
    [REGID_PI_DRAM_ADDR   ] = DEF_IREG(PI_DRAM_ADDR_REG,    "DRAM_ADDR",            REGID_PI_DRAM_ADDR   ),
    [REGID_PI_CART_ADDR   ] = DEF_IREG(PI_CART_ADDR_REG,    "CART_ADDR",            REGID_PI_CART_ADDR   ),
    [REGID_PI_RD_LEN      ] = DEF_IREG(PI_RD_LEN_REG,       "READ_LENGTH",          REGID_PI_RD_LEN      ),
    [REGID_PI_WR_LEN      ] = DEF_IREG(PI_WR_LEN_REG,       "WRITE_LENGTH",         REGID_PI_WR_LEN      ),
    [REGID_PI_STATUS      ] = DEF_IREG(PI_STATUS_REG,       "STATUS",               REGID_PI_STATUS      ),
    [REGID_PI_BSD_DOM1_LAT] = DEF_IREG(PI_BSD_DOM1_LAT_REG, "BSD_DOM1_LAT/DOMAIN1", REGID_PI_BSD_DOM1_LAT),
    [REGID_PI_BSD_DOM1_PWD] = DEF_IREG(PI_BSD_DOM1_PWD_REG, "BSD_DOM1_PWD",         REGID_PI_BSD_DOM1_PWD),
    [REGID_PI_BSD_DOM1_PGS] = DEF_IREG(PI_BSD_DOM1_PGS_REG, "BSD_DOM1_PGS",         REGID_PI_BSD_DOM1_PGS),
    [REGID_PI_BSD_DOM1_RLS] = DEF_IREG(PI_BSD_DOM1_RLS_REG, "BSD_DOM1_RLS",         REGID_PI_BSD_DOM1_RLS),
    [REGID_PI_BSD_DOM2_LAT] = DEF_IREG(PI_BSD_DOM2_LAT_REG, "BSD_DOM2_LAT/DOMAIN2", REGID_PI_BSD_DOM2_LAT),
    [REGID_PI_BSD_DOM2_PWD] = DEF_IREG(PI_BSD_DOM2_PWD_REG, "BSD_DOM2_PWD",         REGID_PI_BSD_DOM2_PWD),
    [REGID_PI_BSD_DOM2_PGS] = DEF_IREG(PI_BSD_DOM2_PGS_REG, "BSD_DOM2_PGS",         REGID_PI_BSD_DOM2_PGS),
    [REGID_PI_BSD_DOM2_RLS] = DEF_IREG(PI_BSD_DOM2_RLS_REG, "BSD_DOM2_RLS",         REGID_PI_BSD_DOM2_RLS),
};
static const RegisterSource sRegisters_PI = DEF_REG_LIST_INTERFACE(
    "PI",
    "Peripheral Interface",
    sRegDesc_PI,
    sRegInfo_PI
);
