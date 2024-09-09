#pragma once

#include "crash_screen/util/registers.h"


// -- RDP Span (DPS) --
// https://n64brew.dev/wiki/Reality_Display_Processor/Interface


static const char* sRegDesc_DPS[] = {
    [REGID_DPS_TBIST       ] = "BIST status",
    [REGID_DPS_TEST_MODE   ] = "Test mode",
    [REGID_DPS_BUFTEST_ADDR] = "Buffer test address",
    [REGID_DPS_BUFTEST_DATA] = "Buffer test data",
};
ALIGNED32 static const RegisterInfo sRegInfo_DPS[] = {
    [REGID_DPS_TBIST       ] = DEF_IREG(DPS_TBIST_REG,        "TBIST",        REGID_DPS_TBIST       ),
    [REGID_DPS_TEST_MODE   ] = DEF_IREG(DPS_TEST_MODE_REG,    "TEST_MODE",    REGID_DPS_TEST_MODE   ),
    [REGID_DPS_BUFTEST_ADDR] = DEF_IREG(DPS_BUFTEST_ADDR_REG, "BUFTEST_ADDR", REGID_DPS_BUFTEST_ADDR),
    [REGID_DPS_BUFTEST_DATA] = DEF_IREG(DPS_BUFTEST_DATA_REG, "BUFTEST_DATA", REGID_DPS_BUFTEST_DATA),
};
static const RegisterSource sRegisters_DPS = DEF_REG_LIST_INTERFACE(
    "DPS",
    "Reality Display Processor (RDP) Span",
    sRegDesc_DPS,
    sRegInfo_DPS
);
