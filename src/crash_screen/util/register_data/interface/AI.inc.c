#pragma once

#include "crash_screen/util/registers.h"


// -- Audio Interface (AI) --
// https://n64brew.dev/wiki/Audio_Interface


static const char* sRegDesc_AI[] = {
    [REGID_AI_DRAM_ADDR] = "DRAM address",
    [REGID_AI_LEN      ] = "Length",
    [REGID_AI_CONTROL  ] = "DMA control",
    [REGID_AI_STATUS   ] = "AI status",
    [REGID_AI_DACRATE  ] = "DA counter rate",
    [REGID_AI_BITRATE  ] = "Bitrate",
};
ALIGNED32 static const RegisterInfo sRegInfo_AI[] = {
    [REGID_AI_DRAM_ADDR] = DEF_IREG(AI_DRAM_ADDR_REG, "DRAM_ADDR", REGID_AI_DRAM_ADDR), //! TODO: Write-only on console?
    [REGID_AI_LEN      ] = DEF_IREG(AI_LEN_REG,       "LENGTH",    REGID_AI_LEN      ),
    [REGID_AI_CONTROL  ] = DEF_IREG(AI_CONTROL_REG,   "CONTROL",   REGID_AI_CONTROL  ), //! TODO: Write-only on console?
    [REGID_AI_STATUS   ] = DEF_IREG(AI_STATUS_REG,    "STATUS",    REGID_AI_STATUS   ),
    [REGID_AI_DACRATE  ] = DEF_IREG(AI_DACRATE_REG,   "DAC_RATE",  REGID_AI_DACRATE  ), //! TODO: Write-only on console?
    [REGID_AI_BITRATE  ] = DEF_IREG(AI_BITRATE_REG,   "BIT_RATE",  REGID_AI_BITRATE  ), //! TODO: Write-only on console?
};
static const RegisterSource sRegisters_AI = DEF_REG_LIST_INTERFACE(
    "AI",
    "Audio Interface",
    sRegDesc_AI,
    sRegInfo_AI
);
