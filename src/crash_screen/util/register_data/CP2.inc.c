#pragma once

#include "crash_screen/util/registers.h"


// -- COP2 Registers --


static const RegisterSource sRegisters_CP2 = {
    .name     = "CP2 (RCP vector unit)",
    .desc     = "Reality Co-Processor Vector Unit",
    .valFunc  = NULL,
    .infoFunc = NULL,
    .descList = NULL,
    .numRegs  = 0,
};
