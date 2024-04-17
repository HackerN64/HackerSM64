#pragma once

#include <ultra64.h>

#include "types.h"

#include "reg_bits.h"


enum PACKED Interfaces {
    REGS_RDRAM,     // RDRAM_BASE_REG
    REGS_SP,        // SP_BASE_REG
    REGS_DPC,       // DPC_BASE_REG
    REGS_DPS,       // DPS_BASE_REG // DP Span Registers
    REGS_MI,        // MI_BASE_REG
    REGS_VI,        // VI_BASE_REG
    REGS_AI,        // AI_BASE_REG
    REGS_PI,        // PI_BASE_REG
    REGS_RI,        // RI_BASE_REG
    REGS_SI,        // SI_BASE_REG

    REGS_GIO,       // GIO_BASE_REG
    REGS_RDB,       // RGB_BASE_REG
    REGS_GIO_RDB,   // GIO_RDB_BASE_REG

    NUM_INTERFACES,
};
enum PACKED REGIDS_RDRAM { // RDRAM_BASE_REG
    REGID_RDRAM_CONFIG, REGID_RDRAM_DEVICE_TYPE = REGID_RDRAM_CONFIG, // RDRAM_CONFIG_REG/RDRAM_DEVICE_TYPE_REG
    REGID_RDRAM_DEVICE_ID,    // RDRAM_DEVICE_ID_REG
    REGID_RDRAM_DELAY,        // RDRAM_DELAY_REG
    REGID_RDRAM_MODE,         // RDRAM_MODE_REG
    REGID_RDRAM_REF_INTERVAL, // RDRAM_REF_INTERVAL_REG
    REGID_RDRAM_REF_ROW,      // RDRAM_REF_ROW_REG
    REGID_RDRAM_RAS_INTERVAL, // RDRAM_RAS_INTERVAL_REG
    REGID_RDRAM_MIN_INTERVAL, // RDRAM_MIN_INTERVAL_REG
    REGID_RDRAM_ADDR_SELECT,  // RDRAM_ADDR_SELECT_REG
    REGID_RDRAM_DEVICE_MANUF, // RDRAM_DEVICE_MANUF_REG

    NUM_REGS_RDRAM,
};
enum PACKED REGIDS_SP { // SP_BASE_REG
    REGID_SP_MEM_ADDR,  // SP_MEM_ADDR_REG  // SP memory address (R/W): [11:0] DMEM/IMEM address; [12] 0=DMEM,1=IMEM
    REGID_SP_DRAM_ADDR, // SP_DRAM_ADDR_REG // SP DRAM DMA address (R/W): [23:0] RDRAM address
    REGID_SP_RD_LEN,    // SP_RD_LEN_REG    // SP read DMA length (R/W): [11:0] length, [19:12] count, [31:20] skip direction: I/DMEM <- RDRAM
    REGID_SP_WR_LEN,    // SP_WR_LEN_REG    // SP write DMA length (R/W): [11:0] length, [19:12] count, [31:20] skip direction: I/DMEM -> RDRAM
    REGID_SP_STATUS,    // SP_STATUS_REG    // SP status (R/W): [14:0] valid bits; see below for write/read mode
    REGID_SP_DMA_FULL,  // SP_DMA_FULL_REG  // SP DMA full (R): [0] valid bit; dma full
    REGID_SP_DMA_BUSY,  // SP_DMA_BUSY_REG  // SP DMA busy (R): [0] valid bit; dma busy
    REGID_SP_SEMAPHORE, // SP_SEMAPHORE_REG // SP semaphore (R/W): Read: [0] semaphore flag (set on read) | Write: [] clear semaphore flag
    REGID_SP_PC,        // SP_PC_REG        // SP PC (R/W): [11:0] program counte
    REGID_SP_IBIST,     // SP_IBIST_REG     // SP IMEM BIST REG (R/W): [6:0] BIST status bits; see below for detail

    NUM_REGS_SP,
};
enum PACKED REGIDS_DPC { // DPC_BASE_REG
    REGID_DPC_START,    // DPC_START_REG    // DP CMD DMA start (R/W): [23:0] DMEM/RDRAM start address
    REGID_DPC_END,      // DPC_END_REG      // DP CMD DMA end (R/W): [23:0] DMEM/RDRAM end address
    REGID_DPC_CURRENT,  // DPC_CURRENT_REG  // DP CMD DMA end (R): [23:0] DMEM/RDRAM current address
    REGID_DPC_STATUS,   // DPC_STATUS_REG   // DP CMD status (R/W): [9:0] valid bits - see below for definitions
    REGID_DPC_CLOCK,    // DPC_CLOCK_REG    // DP clock counter (R): [23:0] clock counter
    REGID_DPC_BUFBUSY,  // DPC_BUFBUSY_REG  // DP buffer busy counter (R): [23:0] clock counter
    REGID_DPC_PIPEBUSY, // DPC_PIPEBUSY_REG // DP pipe busy counter (R): [23:0] clock counter
    REGID_DPC_IMEM,     // DPC_TMEM_REG     // DP TMEM load counter (R): [23:0] clock counter

    NUM_REGS_DPC,
};
enum PACKED REGIDS_DPS { // DPS_BASE_REG
    REGID_DPS_TBIST,        // DP tmem bist (R/W): [10:0] BIST status bits; see below for detail
    REGID_DPS_TEST_MODE,    // DP span test mode (R/W): [0] Span buffer test access enable
    REGID_DPS_BUFTEST_ADDR, // DP span buffer test address (R/W): [6:0] bits; see below for detail
    REGID_DPS_BUFTEST_DATA, // DP span buffer test data (R/W): [31:0] span buffer data

    NUM_REGS_DPS,
};
enum PACKED REGIDS_MI { // MI_BASE_REG
    REGID_MI_INIT_MODE, REGID_MI_MODE = REGID_MI_INIT_MODE, // MI_INIT_MODE_REG/MI_MODE_REG // MI init mode (W): [6:0] init length, [7] clear init mode, [8] set init mode [9/10] clear/set ebus test mode, [11] clear DP interrupt (R): [6:0] init length, [7] init mode, [8] ebus test mode
    REGID_MI_VERSION,   REGID_MI_NOOP = REGID_MI_VERSION,   // MI_VERSION_REG/MI_NOOP_REG   // MI version (R): [7:0] io, [15:8] rac, [23:16] rdp, [31:24] rsp
    REGID_MI_INTR,                                          // MI_INTR_REG                  // MI interrupt (R): [5:0] valid bits - see below for bit patterns
    REGID_MI_INTR_MASK,                                     // MI_INTR_MASK_REG             // MI interrupt mask (W): [11:0] valid bits - see below for bit patterns (R): [5:0] valid bits - see below for bit patterns 

    NUM_REGS_MI,
};
enum PACKED REGIDS_VI { // VI_BASE_REG
    REGID_VI_STATUS,  REGID_VI_CONTROL        = REGID_VI_STATUS,    // VI_STATUS_REG/VI_CONTROL_REG
    REGID_VI_ORIGIN,  REGID_VI_DRAM_ADDR      = REGID_VI_ORIGIN,    // VI_ORIGIN_REG/VI_DRAM_ADDR_REG
    REGID_VI_WIDTH,   REGID_VI_H_WIDTH        = REGID_VI_WIDTH,     // VI_WIDTH_REG/VI_H_WIDTH_REG
    REGID_VI_INTR,    REGID_VI_V_INTR         = REGID_VI_INTR,      // VI_INTR_REG/VI_V_INTR_REG
    REGID_VI_CURRENT, REGID_VI_V_CURRENT_LINE = REGID_VI_CURRENT,   // VI_CURRENT_REG/VI_V_CURRENT_LINE_REG
    REGID_VI_BURST,   REGID_VI_TIMING         = REGID_VI_BURST,     // VI_BURST_REG/VI_TIMING_REG
    REGID_VI_V_SYNC,                                                // VI_V_SYNC_REG
    REGID_VI_H_SYNC,                                                // VI_H_SYNC_REG
    REGID_VI_LEAP,    REGID_VI_H_SYNC_LEAP    = REGID_VI_LEAP,      // VI_LEAP_REG/VI_H_SYNC_LEAP_REG
    REGID_VI_H_START, REGID_VI_H_VIDEO        = REGID_VI_H_START,   // VI_H_START_REG/VI_H_VIDEO_REG
    REGID_VI_V_START, REGID_VI_V_VIDEO        = REGID_VI_V_START,   // VI_V_START_REG/VI_V_VIDEO_REG
    REGID_VI_V_BURST,                                               // VI_V_BURST_REG
    REGID_VI_X_SCALE,                                               // VI_X_SCALE_REG
    REGID_VI_Y_SCALE,                                               // VI_Y_SCALE_REG

    NUM_REGS_VI,
};
enum PACKED REGIDS_AI { // AI_BASE_REG
    REGID_AI_DRAM_ADDR, // AI_DRAM_ADDR_REG // AI DRAM address (W): [23:0] starting RDRAM address (8B-aligned)
    REGID_AI_LEN,       // AI_LEN_REG       // AI length (R/W): [14:0] transfer length (v1.0) - Bottom 3 bits are ignored [17:0] transfer length (v2.0) - Bottom 3 bits are ignored
    REGID_AI_CONTROL,   // AI_CONTROL_REG   // AI control (W): [0] DMA enable - if LSB == 1, DMA is enabled
    REGID_AI_STATUS,    // AI_STATUS_REG    //! TODO:
    REGID_AI_DACRATE,   // AI_DACRATE_REG   //! TODO:
    REGID_AI_BITRATE,   // AI_BITRATE_REG   //! TODO:

    NUM_REGS_AI,
};
enum PACKED REGIDS_PI { // PI_BASE_REG
    REGID_PI_DRAM_ADDR,                                                 // PI_DRAM_ADDR_REG
    REGID_PI_CART_ADDR,                                                 // PI_CART_ADDR_REG
    REGID_PI_RD_LEN,                                                    // PI_RD_LEN_REG
    REGID_PI_WR_LEN,                                                    // PI_WR_LEN_REG
    REGID_PI_STATUS,                                                    // PI_STATUS_REG
    REGID_PI_BSD_DOM1_LAT, REGID_PI_DOMAIN1 = REGID_PI_BSD_DOM1_LAT,    // PI_BSD_DOM1_LAT_REG/PI_DOMAIN1_REG
    REGID_PI_BSD_DOM1_PWD,                                              // PI_BSD_DOM1_PWD_REG
    REGID_PI_BSD_DOM1_PGS,                                              // PI_BSD_DOM1_PGS_REG
    REGID_PI_BSD_DOM1_RLS,                                              // PI_BSD_DOM1_RLS_REG
    REGID_PI_BSD_DOM2_LAT, REGID_PI_DOMAIN2 = REGID_PI_BSD_DOM2_LAT,    // PI_BSD_DOM2_LAT_REG/PI_DOMAIN2_REG
    REGID_PI_BSD_DOM2_PWD,                                              // PI_BSD_DOM2_PWD_REG
    REGID_PI_BSD_DOM2_PGS,                                              // PI_BSD_DOM2_PGS_REG
    REGID_PI_BSD_DOM2_RLS,                                              // PI_BSD_DOM2_RLS_REG

    NUM_REGS_PI,
};
enum PACKED REGIDS_RI { // RI_BASE_REG
    REGID_RI_MODE,                                          // RI_MODE_REG
    REGID_RI_CONFIG,                                        // RI_CONFIG_REG
    REGID_RI_CURRENT_LOAD,                                  // RI_CURRENT_LOAD_REG
    REGID_RI_SELECT,                                        // RI_SELECT_REG
    REGID_RI_REFRESH, REGID_RI_COUNT = REGID_RI_REFRESH,    // RI_REFRESH_REG/RI_COUNT_REG
    REGID_RI_LATENCY,                                       // RI_LATENCY_REG
    REGID_RI_RERROR,                                        // RI_RERROR_REG
    REGID_RI_WERROR,                                        // RI_WERROR_REG

    NUM_REGS_RI
};
enum PACKED REGIDS_SI { // SI_BASE_REG
    REGID_SI_DRAM_ADDR,         // SI_DRAM_ADDR_REG
    REGID_SI_PIF_ADDR_RD64B,    // SI_PIF_ADDR_RD64B_REG
    REGID_SI_PIF_ADDR_WR64B,    // SI_PIF_ADDR_WR64B_REG
    REGID_SI_STATUS,            // SI_STATUS_REG

    NUM_REGS_SI,
};
enum PACKED REGIDS_GIO { // GIO_BASE_REG
    REGID_GIO_BASE,         // GIO_BASE_REG
    REGID_GIO_GIO_INTR,     // GIO_GIO_INTR_REG
    REGID_GIO_GIO_SYNC,     // GIO_GIO_SYNC_REG
    REGID_GIO_CART_INTR,    // GIO_CART_INTR_REG

    NUM_REGS_GIO,
};
enum PACKED REGIDS_RDB { // RDB_BASE_REG
    REGID_RDB_BASE,                 // RDB_BASE_REG
    REGID_RDB_WRITE_INTR,           // RDB_WRITE_INTR_REG
    REGID_RDB_READ_INTR,            // RDB_READ_INTR_REG

    NUM_REGS_RDB,
};
enum PACKED REGIDS_GIO_RDB { // GIO_RDB_BASE_REG
    REGID_GIO_RDB_BASE,             // GIO_RDB_BASE_REG
    REGID_GIO_RDB_WRITE_INTR,       // GIO_RDB_WRITE_INTR_REG
    REGID_GIO_RDB_READ_INTR,        // GIO_RDB_READ_INTR_REG

    NUM_REGS_GIO_RDB,
};

typedef struct InterfaceReg {
    /*0x00*/ const char* name;
    /*0x04*/ Address addr;
} InterfaceReg; /*0x08*/

#define DEF_IREG(_reg, _name) { \
    .name = _name,              \
    .addr = _reg,               \
}
#define DEF_IREG_END() DEF_IREG(0, NULL)


typedef struct InterfaceInfo {
    /*0x00*/ const char* name;
    /*0x04*/ const char* desc;
    /*0x08*/ InterfaceReg* list;
} InterfaceInfo; /*0x0C*/


extern InterfaceInfo gInterfaceInfos[];

InterfaceInfo* get_interface_info(enum Interfaces interfaceId);
InterfaceReg* get_interface_reg_info(enum Interfaces interfaceId, u8 regId);
const char* get_interface_reg_name(enum Interfaces interfaceId, u8 regId);
Word get_interface_reg_val(enum Interfaces interfaceId, u8 regId);
