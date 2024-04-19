#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

#include "PR/rdb.h"

#include "crash_screen/cs_main.h"
#include "map_parser.h"
#include "memory_read.h"
#include "registers.h"

#include "interface.h"



// -- RDRAM --

InterfaceReg iregs_RDRAM[] = {
    [REGID_RDRAM_CONFIG      ] = DEF_IREG(RDRAM_CONFIG_REG,       "CONFIG/DEVICE_TYPE"),
    [REGID_RDRAM_DEVICE_ID   ] = DEF_IREG(RDRAM_DEVICE_ID_REG,    "DEVICE_ID"         ),
    [REGID_RDRAM_DELAY       ] = DEF_IREG(RDRAM_DELAY_REG,        "DELAY"             ),
    [REGID_RDRAM_MODE        ] = DEF_IREG(RDRAM_MODE_REG,         "MODE"              ),
    [REGID_RDRAM_REF_INTERVAL] = DEF_IREG(RDRAM_REF_INTERVAL_REG, "REF_INTERVAL"      ),
    [REGID_RDRAM_REF_ROW     ] = DEF_IREG(RDRAM_REF_ROW_REG,      "REF_ROW"           ),
    [REGID_RDRAM_RAS_INTERVAL] = DEF_IREG(RDRAM_RAS_INTERVAL_REG, "RAS_INTERVAL"      ),
    [REGID_RDRAM_MIN_INTERVAL] = DEF_IREG(RDRAM_MIN_INTERVAL_REG, "MIN_INTERVAL"      ),
    [REGID_RDRAM_ADDR_SELECT ] = DEF_IREG(RDRAM_ADDR_SELECT_REG,  "ADDR_SELECT"       ),
    [REGID_RDRAM_DEVICE_MANUF] = DEF_IREG(RDRAM_DEVICE_MANUF_REG, "DEVICE_MANUF"      ),
    DEF_IREG_END(),
};
InterfaceReg iregs_SP[] = {
    [REGID_SP_MEM_ADDR ] = DEF_IREG(SP_MEM_ADDR_REG,  "MEM_ADDR"    ),
    [REGID_SP_DRAM_ADDR] = DEF_IREG(SP_DRAM_ADDR_REG, "DRAM_ADDR"   ),
    [REGID_SP_RD_LEN   ] = DEF_IREG(SP_RD_LEN_REG,    "READ_LENGTH" ),
    [REGID_SP_WR_LEN   ] = DEF_IREG(SP_WR_LEN_REG,    "WRITE_LENGTH"),
    [REGID_SP_STATUS   ] = DEF_IREG(SP_STATUS_REG,    "STATUS"      ),
    [REGID_SP_DMA_FULL ] = DEF_IREG(SP_DMA_FULL_REG,  "DMA_FULL"    ),
    [REGID_SP_DMA_BUSY ] = DEF_IREG(SP_DMA_BUSY_REG,  "DMA_BUSY"    ),
    [REGID_SP_SEMAPHORE] = DEF_IREG(SP_SEMAPHORE_REG, "SEMAPHORE"   ),
    [REGID_SP_PC       ] = DEF_IREG(SP_PC_REG,        "PC"          ),
    [REGID_SP_IBIST    ] = DEF_IREG(SP_IBIST_REG,     "IBIST"       ),
    DEF_IREG_END(),
};
InterfaceReg iregs_DPC[] = {
    [REGID_DPC_START   ] = DEF_IREG(DPC_START_REG,    "START"   ),
    [REGID_DPC_END     ] = DEF_IREG(DPC_END_REG,      "END"     ),
    [REGID_DPC_CURRENT ] = DEF_IREG(DPC_CURRENT_REG,  "CURRENT" ),
    [REGID_DPC_STATUS  ] = DEF_IREG(DPC_STATUS_REG,   "STATUS"  ),
    [REGID_DPC_CLOCK   ] = DEF_IREG(DPC_CLOCK_REG,    "CLOCK"   ),
    [REGID_DPC_BUFBUSY ] = DEF_IREG(DPC_BUFBUSY_REG,  "BUFBUSY" ),
    [REGID_DPC_PIPEBUSY] = DEF_IREG(DPC_PIPEBUSY_REG, "PIPEBUSY"),
    [REGID_DPC_IMEM    ] = DEF_IREG(DPC_TMEM_REG,     "TMEM"    ),
    DEF_IREG_END(),
};
InterfaceReg iregs_DPS[] = {
    [REGID_DPS_TBIST       ] = DEF_IREG(DPS_TBIST_REG,        "TBIST"       ),
    [REGID_DPS_TEST_MODE   ] = DEF_IREG(DPS_TEST_MODE_REG,    "TEST_MODE"   ),
    [REGID_DPS_BUFTEST_ADDR] = DEF_IREG(DPS_BUFTEST_ADDR_REG, "BUFTEST_ADDR"),
    [REGID_DPS_BUFTEST_DATA] = DEF_IREG(DPS_BUFTEST_DATA_REG, "BUFTEST_DATA"),
    DEF_IREG_END(),
};
InterfaceReg iregs_MI[] = {
    [REGID_MI_INIT_MODE] = DEF_IREG(MI_INIT_MODE_REG, "INIT_MODE/MODE"),
    [REGID_MI_VERSION  ] = DEF_IREG(MI_VERSION_REG,   "VERSION/NOOP"  ),
    [REGID_MI_INTR     ] = DEF_IREG(MI_INTR_REG,      "INTERRUPT"     ),
    [REGID_MI_INTR_MASK] = DEF_IREG(MI_INTR_MASK_REG, "INTERRUPT MASK"),
    DEF_IREG_END(),
};
InterfaceReg iregs_VI[] = {
    [REGID_VI_STATUS ] = DEF_IREG(VI_STATUS_REG,  "STATUS/CONTROL"        ),
    [REGID_VI_ORIGIN ] = DEF_IREG(VI_ORIGIN_REG,  "ORIGIN/DRAM_ADDR"      ),
    [REGID_VI_WIDTH  ] = DEF_IREG(VI_WIDTH_REG,   "WIDTH/H_WIDTH"         ),
    [REGID_VI_INTR   ] = DEF_IREG(VI_INTR_REG,    "INTERRUPT/V_INTERRUPT" ),
    [REGID_VI_CURRENT] = DEF_IREG(VI_CURRENT_REG, "CURRENT/V_CURRENT_LINE"),
    [REGID_VI_BURST  ] = DEF_IREG(VI_BURST_REG,   "BURST/TIMING"          ),
    [REGID_VI_V_SYNC ] = DEF_IREG(VI_V_SYNC_REG,  "V_SYNC"                ),
    [REGID_VI_H_SYNC ] = DEF_IREG(VI_H_SYNC_REG,  "H_SYNC"                ),
    [REGID_VI_LEAP   ] = DEF_IREG(VI_LEAP_REG,    "LEAP/H_SYNC_LEAP"      ),
    [REGID_VI_H_START] = DEF_IREG(VI_H_START_REG, "H_START/H_VIDEO"       ),
    [REGID_VI_V_START] = DEF_IREG(VI_V_START_REG, "V_START/V_VIDEO"       ),
    [REGID_VI_V_BURST] = DEF_IREG(VI_V_BURST_REG, "V_BURST"               ),
    [REGID_VI_X_SCALE] = DEF_IREG(VI_X_SCALE_REG, "X_SCALE"               ),
    [REGID_VI_Y_SCALE] = DEF_IREG(VI_Y_SCALE_REG, "Y_SCALE"               ),
    DEF_IREG_END(),
};
InterfaceReg iregs_AI[] = {
    [REGID_AI_DRAM_ADDR] = DEF_IREG(AI_DRAM_ADDR_REG, "DRAM_ADDR"),
    [REGID_AI_LEN      ] = DEF_IREG(AI_LEN_REG,       "LENGTH"   ),
    [REGID_AI_CONTROL  ] = DEF_IREG(AI_CONTROL_REG,   "CONTROL"  ),
    [REGID_AI_STATUS   ] = DEF_IREG(AI_STATUS_REG,    "STATUS"   ),
    [REGID_AI_DACRATE  ] = DEF_IREG(AI_DACRATE_REG,   "DAC_RATE" ),
    [REGID_AI_BITRATE  ] = DEF_IREG(AI_BITRATE_REG,   "BIT_RATE" ),
    DEF_IREG_END(),
};
InterfaceReg iregs_PI[] = {
    [REGID_PI_DRAM_ADDR   ] = DEF_IREG(PI_DRAM_ADDR_REG,    "DRAM_ADDR"           ),
    [REGID_PI_CART_ADDR   ] = DEF_IREG(PI_CART_ADDR_REG,    "CART_ADDR"           ),
    [REGID_PI_RD_LEN      ] = DEF_IREG(PI_RD_LEN_REG,       "READ_LENGTH"         ),
    [REGID_PI_WR_LEN      ] = DEF_IREG(PI_WR_LEN_REG,       "WRITE_LENGTH"        ),
    [REGID_PI_STATUS      ] = DEF_IREG(PI_STATUS_REG,       "STATUS"              ),
    [REGID_PI_BSD_DOM1_LAT] = DEF_IREG(PI_BSD_DOM1_LAT_REG, "BSD_DOM1_LAT/DOMAIN1"),
    [REGID_PI_BSD_DOM1_PWD] = DEF_IREG(PI_BSD_DOM1_PWD_REG, "BSD_DOM1_PWD"        ),
    [REGID_PI_BSD_DOM1_PGS] = DEF_IREG(PI_BSD_DOM1_PGS_REG, "BSD_DOM1_PGS"        ),
    [REGID_PI_BSD_DOM1_RLS] = DEF_IREG(PI_BSD_DOM1_RLS_REG, "BSD_DOM1_RLS"        ),
    [REGID_PI_BSD_DOM2_LAT] = DEF_IREG(PI_BSD_DOM2_LAT_REG, "BSD_DOM2_LAT/DOMAIN2"),
    [REGID_PI_BSD_DOM2_PWD] = DEF_IREG(PI_BSD_DOM2_PWD_REG, "BSD_DOM2_PWD"        ),
    [REGID_PI_BSD_DOM2_PGS] = DEF_IREG(PI_BSD_DOM2_PGS_REG, "BSD_DOM2_PGS"        ),
    [REGID_PI_BSD_DOM2_RLS] = DEF_IREG(PI_BSD_DOM2_RLS_REG, "BSD_DOM2_RLS"        ),
    DEF_IREG_END(),
};
InterfaceReg iregs_RI[] = {
    [REGID_RI_MODE        ] = DEF_IREG(RI_MODE_REG,         "MODE"         ),
    [REGID_RI_CONFIG      ] = DEF_IREG(RI_CONFIG_REG,       "CONFIG"       ),
    [REGID_RI_CURRENT_LOAD] = DEF_IREG(RI_CURRENT_LOAD_REG, "CURRENT_LOAD" ),
    [REGID_RI_SELECT      ] = DEF_IREG(RI_SELECT_REG,       "SELECT"       ),
    [REGID_RI_REFRESH     ] = DEF_IREG(RI_REFRESH_REG,      "REFRESH/COUNT"),
    [REGID_RI_LATENCY     ] = DEF_IREG(RI_LATENCY_REG,      "LATENCY"      ),
    [REGID_RI_RERROR      ] = DEF_IREG(RI_RERROR_REG,       "READ_ERROR"   ),
    [REGID_RI_WERROR      ] = DEF_IREG(RI_WERROR_REG,       "WRITE_ERROR"  ),
    DEF_IREG_END(),
};
InterfaceReg iregs_SI[] = {
    [REGID_SI_DRAM_ADDR     ] = DEF_IREG(SI_DRAM_ADDR_REG,      "DRAM_ADDR"        ),
    [REGID_SI_PIF_ADDR_RD64B] = DEF_IREG(SI_PIF_ADDR_RD64B_REG, "PIF_ADDR_READ64B" ),
    [REGID_SI_PIF_ADDR_WR64B] = DEF_IREG(SI_PIF_ADDR_WR64B_REG, "PIF_ADDR_WRITE64B"),
    [REGID_SI_STATUS        ] = DEF_IREG(SI_STATUS_REG,         "STATUS"           ),
    DEF_IREG_END(),
};
InterfaceReg iregs_GIO[] = {
    [REGID_GIO_BASE     ] = DEF_IREG(GIO_BASE_REG,      "BASE"          ),
    [REGID_GIO_GIO_INTR ] = DEF_IREG(GIO_GIO_INTR_REG,  "INTERRUPT"     ),
    [REGID_GIO_GIO_SYNC ] = DEF_IREG(GIO_GIO_SYNC_REG,  "SYNC"          ),
    [REGID_GIO_CART_INTR] = DEF_IREG(GIO_CART_INTR_REG, "CART_INTERRUPT"),
    DEF_IREG_END(),
};
InterfaceReg iregs_RDB[] = {
    [REGID_RDB_BASE             ] = DEF_IREG(RDB_BASE_REG,          "BASE"             ),
    [REGID_RDB_WRITE_INTR       ] = DEF_IREG(RDB_WRITE_INTR_REG,    "WRITE_INTERRUPT"  ),
    [REGID_RDB_READ_INTR        ] = DEF_IREG(RDB_READ_INTR_REG,     "READ_INTERRUPT"   ),
    DEF_IREG_END(),
};
InterfaceReg iregs_GIO_RDB[] = {
    [REGID_GIO_RDB_BASE      ] = DEF_IREG(GIO_RDB_BASE_REG,       "BASE"           ),
    [REGID_GIO_RDB_WRITE_INTR] = DEF_IREG(GIO_RDB_WRITE_INTR_REG, "WRITE_INTERRUPT"),
    [REGID_GIO_RDB_READ_INTR ] = DEF_IREG(GIO_RDB_READ_INTR_REG,  "READ_INTERRUPT" ),
    DEF_IREG_END(),
};

InterfaceInfo gInterfaceInfos[] = {
    [REGS_RDRAM  ] = { .list = iregs_RDRAM,   .name = "RDRAM",   .desc = "RAMBus DRAM", },
    [REGS_SP     ] = { .list = iregs_SP,      .name = "SP",      .desc = "Reality Signal Processor", },
    [REGS_DPC    ] = { .list = iregs_DPC,     .name = "DPC",     .desc = "Reality Display Processor (RDP) Command", },
    [REGS_DPS    ] = { .list = iregs_DPS,     .name = "DPS",     .desc = "Reality Display Processor (RDP) Span", },
    [REGS_MI     ] = { .list = iregs_MI,      .name = "MI",      .desc = "MIPS Interface", },
    [REGS_VI     ] = { .list = iregs_VI,      .name = "VI",      .desc = "Video Interface", },
    [REGS_AI     ] = { .list = iregs_AI,      .name = "AI",      .desc = "Audio Interface", },
    [REGS_PI     ] = { .list = iregs_PI,      .name = "PI",      .desc = "Peripheral Interface", },
    [REGS_RI     ] = { .list = iregs_RI,      .name = "RI",      .desc = "Rambus DRAM (RDRAM) Interface", },
    [REGS_SI     ] = { .list = iregs_SI,      .name = "SI",      .desc = "Serial Interface", },
    [REGS_GIO    ] = { .list = iregs_GIO,     .name = "GIO",     .desc = "Development Board", },
    [REGS_RDB    ] = { .list = iregs_RDB,     .name = "RDB",     .desc = "Debug port", },
    [REGS_GIO_RDB] = { .list = iregs_GIO_RDB, .name = "GIO_RDB", .desc = "Development Board & Debug Port", },
};

InterfaceInfo* get_interface_info(enum Interfaces interfaceId) {
    return &gInterfaceInfos[interfaceId];
}

InterfaceReg* get_interface_reg_info(enum Interfaces interfaceId, u8 regId) {
    InterfaceInfo* interface = get_interface_info(interfaceId);
    return &interface->list[regId];
}

const char* get_interface_reg_name(enum Interfaces interfaceId, u8 regId) {
    InterfaceReg* reg = get_interface_reg_info(interfaceId, regId);
    return reg->name;
}

Word get_interface_reg_val(enum Interfaces interfaceId, u8 regId) {
    InterfaceReg* reg = get_interface_reg_info(interfaceId, regId);
    Word data = 0x00000000;

    if (try_read_word_aligned(&data, reg->addr)) {
        return data;
    }

    return 0;
}
