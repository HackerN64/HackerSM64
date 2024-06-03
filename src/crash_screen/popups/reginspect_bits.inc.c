#pragma once

#include "crash_screen/popups/popup_reginspect.h"
#include "crash_screen/util/registers.h"


//! TODO: Move these to their respective files in /register_data/


const RegBitsInfo regBits_C0_INDEX[] = {
    REG_BITS_CMD_SETX(STRLEN("tlb probe: ")),
    REG_BITS_CMD_BSTR("tlb probe", C0_INX_TLB_FAIL,  REG_BITS_INFO_STR_SUCCESS),
    REG_BITS_CMD_DEC( "tlb index", C0_INX_TLB_INDEX, 2),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_C0_SR[] = {
    REG_BITS_CMD_SETX(STRLEN("xxxxxxxxxx: ")),
    REG_BITS_CMD_SETW(STRLEN("xxxxxxxx")),
    REG_BITS_CMD_STR( "cop1",       SR_CU1,      REG_BITS_INFO_STR_ENABLED),
    REG_BITS_CMD_STR( "low power",  SR_RP,       REG_BITS_INFO_STR_ON_OFF),
    REG_BITS_CMD_STR( "extra fpr",  SR_FR,       REG_BITS_INFO_STR_ENABLED),
    REG_BITS_CMD_BSTR("endian",     SR_RE,       REG_BITS_INFO_STR_ENDIAN),
    REG_BITS_CMD_HEX( "intr mask",  SR_IMASK,    2),
    REG_BITS_CMD_STR( "kernel",     SR_KX,       REG_BITS_INFO_STR_BIT_MODE),
    REG_BITS_CMD_STR( "supervisor", SR_SX,       REG_BITS_INFO_STR_BIT_MODE),
    REG_BITS_CMD_STR( "user",       SR_UX,       REG_BITS_INFO_STR_BIT_MODE),
    REG_BITS_CMD_STR( "exec mode",  SR_KSU_MASK, REG_BITS_INFO_STR_C0_SR_EXEC_MODE),
    REG_BITS_CMD_STR( "error",      SR_ERL,      REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR( "exception",  SR_EXL,      REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR( "glbl intr",  SR_IE,       REG_BITS_INFO_STR_ENABLED),
    REG_BITS_CMD_WRAP(),
    REG_BITS_CMD_STR( "insn trace", SR_ITS,      REG_BITS_INFO_STR_ENABLED),
    REG_BITS_CMD_STR( "bs exc vec", SR_BEV,      REG_BITS_INFO_STR_C0_SR_BEV),
    REG_BITS_CMD_STR( "tlb down",   SR_TS,       REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR( "soft reset", SR_SR,       REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR( "cp0 cond",   SR_CH,       REG_BITS_INFO_STR_TRUTH),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_C0_CAUSE[] = {
    REG_BITS_CMD_SETX(STRLEN("is branch delay slot: ")),
    REG_BITS_CMD_STR("is branch delay slot", CAUSE_BD, REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_GAP(),
    REG_BITS_CMD_NONE("interrupts pending"),
    REG_BITS_CMD_DEC( "  Timer",       CAUSE_IP8, 1),
    REG_BITS_CMD_FUNC("  Indy RDB",    (CAUSE_IP7 | CAUSE_IP6), REG_BITS_INFO_FUNC_READWRITE),
    REG_BITS_CMD_DEC( "  Reset (NMI)", CAUSE_IP5, 1),
    REG_BITS_CMD_DEC( "  Cartridge",   CAUSE_IP4, 1),
    REG_BITS_CMD_DEC( "  MIPS",        CAUSE_IP3, 1),
    REG_BITS_CMD_DEC( "  Software",    (CAUSE_SW2 | CAUSE_SW1), 1),
    REG_BITS_CMD_GAP(),
    REG_BITS_CMD_DEC("exc code", CAUSE_EXCMASK, 2),
    REG_BITS_CMD_SETX(2),
    REG_BITS_CMD_FUNC(NULL, BITMASK(32), REG_BITS_INFO_FUNC_C0_CAUSE),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_C0_CONFIG[] = {
    REG_BITS_CMD_SETX(STRLEN("dirty shr coh:")),
    REG_BITS_CMD_SETW(STRLEN("xxxxxxxx")),
    REG_BITS_CMD_STR( "master check",  CONFIG_CM, REG_BITS_INFO_STR_ENABLED),
    REG_BITS_CMD_ISTR("clock ratio",   CONFIG_EC, REG_BITS_INFO_LIST_C0_CONFIG_SYS_CLOCK_RATIO),
    REG_BITS_CMD_ISTR("data pattern",  CONFIG_EP, REG_BITS_INFO_LIST_C0_CONFIG_TRANS_DATA_PATTERN),
    REG_BITS_CMD_HEX( "2cache blksz",  CONFIG_SB, 2),

    REG_BITS_CMD_STR( "split cache",   CONFIG_SS, REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_BSTR("scache port",   CONFIG_SW, REG_BITS_INFO_STR_64_128),
    REG_BITS_CMD_BSTR("port width",    CONFIG_EW, REG_BITS_INFO_STR_32_64),
    REG_BITS_CMD_BSTR("2cache",        CONFIG_SC, REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_BSTR("dirty shr coh", CONFIG_SM, REG_BITS_INFO_STR_ENABLED),
    REG_BITS_CMD_STR( "endian",        CONFIG_BE, REG_BITS_INFO_STR_ENDIAN),
    REG_BITS_CMD_STR( "ecc mode",      CONFIG_EM, REG_BITS_INFO_STR_C0_CONFIG_EM),
    REG_BITS_CMD_STR( "blk order",     CONFIG_EB, REG_BITS_INFO_STR_C0_CONFIG_EB),
    REG_BITS_CMD_ISTR("k0seg coh",     CONFIG_K0, REG_BITS_INFO_LIST_C0_CONFIG_K0),
    REG_BITS_CMD_WRAP(),
    REG_BITS_CMD_SETX(STRLEN("icache blksz:")),
    REG_BITS_CMD_HEX( "1icache sz",    CONFIG_IC, 1),
    REG_BITS_CMD_HEX( "1dcache sz",    CONFIG_DC, 1),
    REG_BITS_CMD_HEX( "icache blksz",  CONFIG_IB, 1),
    REG_BITS_CMD_HEX( "dcache blksz",  CONFIG_DB, 1),
    REG_BITS_CMD_STR( "upd on SC",     CONFIG_CU, REG_BITS_INFO_STR_YES_NO),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_FPR_CSR[] = {
    REG_BITS_CMD_SETX(STRLEN("flush denorms to zero: ")),
    REG_BITS_CMD_STR("flush denorms to zero", FPCSR_FS,      REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("condition bit",         FPCSR_C,       REG_BITS_INFO_STR_TRUTH),
    REG_BITS_CMD_STR("rounding mode",         FPCSR_RM_MASK, REG_BITS_INFO_STR_FCR31_ROUNDING_MODE),
    REG_BITS_CMD_NONE("\nexception bits:\n  E:unimpl   | V:invalid   | Z:div0\n  O:overflow | U:underflow | I:inexact\n\n                 E V Z O U I"),
    REG_BITS_CMD_SETX(17),
    REG_BITS_CMD_BIN("          cause",  (FPCSR_CE | FPCSR_CV | FPCSR_CZ | FPCSR_CO | FPCSR_CU | FPCSR_CI), 2),
    REG_BITS_CMD_SETX(19),
    REG_BITS_CMD_BIN("          enable",            (FPCSR_EV | FPCSR_EZ | FPCSR_EO | FPCSR_EU | FPCSR_EI), 2),
    REG_BITS_CMD_BIN("          flags",             (FPCSR_FV | FPCSR_FZ | FPCSR_FO | FPCSR_FU | FPCSR_FI), 2),
    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_SPC_RCP[] = { //! TODO: Individual interrupts.
    REG_BITS_CMD_SETX(STRLEN("interrupt mask: ")),
    REG_BITS_CMD_HEX("interrupt mask", (RCP_IMASK >> RCP_IMASKSHIFT), 2),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_RDRAM_CONFIG[] = {
    REG_BITS_CMD_STR("9 bits per byte", RDRAM_CONFIG_BN,          REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("Low latency",     RDRAM_CONFIG_EN,          REG_BITS_INFO_STR_ENABLED),
    REG_BITS_CMD_DEC("column bits",     RDRAM_CONFIG_COLUMN_BITS, 2),
    REG_BITS_CMD_DEC("bank bits",       RDRAM_CONFIG_BANK_BITS,   2),
    REG_BITS_CMD_DEC("row bits",        RDRAM_CONFIG_ROW_BITS,    2),
    REG_BITS_CMD_STR("version",         RDRAM_CONFIG_VERSION,     REG_BITS_INFO_STR_RDRAM_VERSION),
    REG_BITS_CMD_STR("device type",     RDRAM_CONFIG_TYPE,        REG_BITS_INFO_STR_RDRAM_TYPE),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_RDRAM_MODE[] = {
    REG_BITS_CMD_SETX(STRLEN("Select PowerDown latency: ")),
    REG_BITS_CMD_STR("CCEnable",                 RDRAM_MODE_CE_MASK, REG_BITS_INFO_STR_AUTO),
    REG_BITS_CMD_DEC("CCMult",                   RDRAM_MODE_X2_MASK, 1),
    REG_BITS_CMD_DEC("Select PowerDown Latency", RDRAM_MODE_PL_MASK, 1),
    REG_BITS_CMD_STR("RDRAM device",             RDRAM_MODE_DE_MASK, REG_BITS_INFO_STR_ENABLED),
    REG_BITS_CMD_STR("PowerDown",                RDRAM_MODE_LE_MASK, REG_BITS_INFO_STR_ENABLED),
    REG_BITS_CMD_DEC("AckDis",                   RDRAM_MODE_AD_MASK, 1),
    REG_BITS_CMD_FUNC("CCValue",                 BITMASK(32),        REG_BITS_INFO_FUNC_RDRAM_MODE_CCVALUE),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_SP_STATUS[] = {
    REG_BITS_CMD_SETX(STRLEN("intr. on break: ")),
    REG_BITS_CMD_SETW(STRLEN("yes")),
    REG_BITS_CMD_STR("halt",           SP_STATUS_HALT,       REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("broke",          SP_STATUS_BROKE,      REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("dma busy",       SP_STATUS_DMA_BUSY,   REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("dma full",       SP_STATUS_DMA_FULL,   REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("io full",        SP_STATUS_IO_FULL,    REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("single step",    SP_STATUS_SSTEP,      REG_BITS_INFO_STR_ON_OFF),
    REG_BITS_CMD_STR("intr. on break", SP_STATUS_INTR_BREAK, REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_WRAP(),
    REG_BITS_CMD_SETX(STRLEN("sigN (xxx signal): ")),
    REG_BITS_CMD_DEC("sig0 (yield)",      SP_STATUS_YIELD,     1),
    REG_BITS_CMD_DEC("sig1 (yielded)",    SP_STATUS_YIELDED,   1),
    REG_BITS_CMD_DEC("sig2 (task done)",  SP_STATUS_TASKDONE,  1),
    REG_BITS_CMD_DEC("sig3 (rsp signal)", SP_STATUS_RSPSIGNAL, 1),
    REG_BITS_CMD_DEC("sig4 (cpu signal)", SP_STATUS_CPUSIGNAL, 1),
    REG_BITS_CMD_DEC("sig5",              SP_STATUS_SIG5,      1),
    REG_BITS_CMD_DEC("sig6",              SP_STATUS_SIG6,      1),
    REG_BITS_CMD_DEC("sig7",              SP_STATUS_SIG7,      1),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_DPC_STATUS[] = {
    REG_BITS_CMD_SETX(STRLEN("transfer src: ")),
    REG_BITS_CMD_STR("start valid",  DPC_STATUS_START_VALID,   REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("end valid",    DPC_STATUS_END_VALID,     REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("dma busy",     DPC_STATUS_DMA_BUSY,      REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("cbuf ready",   DPC_STATUS_CBUF_READY,    REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("cmd busy",     DPC_STATUS_CMD_BUSY,      REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("pipe busy",    DPC_STATUS_PIPE_BUSY,     REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("tmem busy",    DPC_STATUS_TMEM_BUSY,     REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("start gclk",   DPC_STATUS_START_GCLK,    REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("flush",        DPC_STATUS_FLUSH,         REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("freeze",       DPC_STATUS_FREEZE,        REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("transfer src", DPC_STATUS_XBUS_DMEM_DMA, REG_BITS_INFO_STR_XBUS_DMEM),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_VI_CONTROL[] = {
    REG_BITS_CMD_SETX(STRLEN("dither filter: ")),
    REG_BITS_CMD_STR("dither filter", VI_CTRL_DITHER_FILTER_ON,   REG_BITS_INFO_STR_ON_OFF),
    REG_BITS_CMD_BIN("pixel advance", VI_CTRL_PIXEL_ADVANCE_MASK, 1),
    REG_BITS_CMD_STR("kill we",       VI_CTRL_KILL_WE,            REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("aa mode",       VI_CTRL_ANTIALIAS_MASK,     REG_BITS_INFO_STR_VI_AA_MODE),
    REG_BITS_CMD_STR("test mode",     VI_CTRL_TEST_MODE,          REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("serrate",       VI_CTRL_SERRATE_ON,         REG_BITS_INFO_STR_ON_OFF),
    REG_BITS_CMD_STR("vbus clock",    VI_CTRL_VBUS_CLOCK_ENABLE,  REG_BITS_INFO_STR_ENABLED), //! TODO: Warning to never set this bit.
    REG_BITS_CMD_STR("divot",         VI_CTRL_DIVOT_ON,           REG_BITS_INFO_STR_ON_OFF),
    REG_BITS_CMD_STR("gamma",         VI_CTRL_GAMMA_ON,           REG_BITS_INFO_STR_ON_OFF),
    REG_BITS_CMD_STR("gamma dither",  VI_CTRL_GAMMA_DITHER_ON,    REG_BITS_INFO_STR_ON_OFF),
    REG_BITS_CMD_STR("type",          VI_CTRL_TYPE_MASK,          REG_BITS_INFO_STR_VI_TYPE),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_VI_BURST[] = {
    REG_BITS_CMD_SETX(STRLEN("burst start: ")),
    REG_BITS_CMD_DEC("vsync width", VI_BURST_VSYNC_WIDTH, 1),
    REG_BITS_CMD_DEC("hsync width", VI_BURST_HSYNC_WIDTH, 1),
    REG_BITS_CMD_DEC("burst start", VI_BURST_START,       1),
    REG_BITS_CMD_DEC("burst width", VI_BURST_WIDTH,       1),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_VI_H_SYNC[] = {
    REG_BITS_CMD_SETX(STRLEN("h sync: ")),
    REG_BITS_CMD_BIN("leap",   VI_H_SYNC_LEAP,   1),
    REG_BITS_CMD_DEC("h sync", VI_H_SYNC_H_SYNC, 1),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_AI_CONTROL[] = { //! TODO: Should single print be used for this?
    REG_BITS_CMD_SETX(STRLEN("dma: ")),
    REG_BITS_CMD_STR("dma", AI_CONTROL_DMA_ON, REG_BITS_INFO_STR_ENABLED),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_AI_STATUS[] = {
    REG_BITS_CMD_SETX(STRLEN("word clock: ")),
    REG_BITS_CMD_STR("fifo full",  AI_STATUS_FIFO_FULL,         REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("dma busy",   AI_STATUS_DMA_BUSY,          REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("enabled",    BIT(CTZ(AI_STATUS_ENABLED)), REG_BITS_INFO_STR_ENABLED),
    REG_BITS_CMD_DEC("word clock", AI_STATUS_WC,    1),
    REG_BITS_CMD_DEC("bit clock",  AI_STATUS_BC,    1),
    REG_BITS_CMD_DEC("count",      AI_STATUS_COUNT, 5),
    //! TODO: AI_STATUS_FULL2?

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_PI_STATUS[] = {
    REG_BITS_CMD_SETX(STRLEN("intr. (dma completed): ")),
    REG_BITS_CMD_STR("intr. (dma completed)", PI_STATUS_INTR,     REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("dma error",             PI_STATUS_ERROR,    REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("io busy",               PI_STATUS_IO_BUSY,  REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("dma busy",              PI_STATUS_DMA_BUSY, REG_BITS_INFO_STR_YES_NO),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_RI_MODE[] = {
    REG_BITS_CMD_SETX(STRLEN("op mode: ")),
    REG_BITS_CMD_STR("stop r",  RI_MODE_STOP_R,  REG_BITS_INFO_STR_ENABLED),
    REG_BITS_CMD_STR("stop t",  RI_MODE_STOP_T,  REG_BITS_INFO_STR_ENABLED),
    REG_BITS_CMD_STR("op mode", RI_MODE_OP_MODE, REG_BITS_INFO_STR_RI_MODE_OP),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_RI_CONFIG[] = {
    REG_BITS_CMD_SETX(STRLEN("auto cc: ")),
    REG_BITS_CMD_STR("auto cc", RI_CONFIG_AUTO, REG_BITS_INFO_STR_ENABLED),
    REG_BITS_CMD_DEC("cc",      RI_CONFIG_CC,   2),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_RI_SELECT[] = {
    REG_BITS_CMD_SETX(STRLEN("transmit: ")),
    REG_BITS_CMD_HEX("transmit", RI_SELECT_TSEL, 1),
    REG_BITS_CMD_HEX("receive",  RI_SELECT_RSEL, 1),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_RI_REFRESH[] = {
    REG_BITS_CMD_SETX(STRLEN("dirty refresh delay: ")),
    REG_BITS_CMD_BIN("multibank",           RI_REFRESH_MULTIBANK, 2),
    REG_BITS_CMD_STR("optimize",            RI_REFRESH_OPT,       REG_BITS_INFO_STR_ENABLED),
    REG_BITS_CMD_STR("automatic refresh",   RI_REFRESH_EN,        REG_BITS_INFO_STR_ENABLED),
    REG_BITS_CMD_DEC("bank",                RI_REFRESH_BANK,      1), //! TODO: imem/dmem?
    REG_BITS_CMD_DEC("dirty refresh delay", RI_REFRESH_DIRTY,     3),
    REG_BITS_CMD_DEC("clean refresh delay", RI_REFRESH_CLEAN,     3),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_RI_RERROR[] = {
    REG_BITS_CMD_SETX(STRLEN("over range error: ")),
    REG_BITS_CMD_STR("over range error", RI_ERROR_OVER, REG_BITS_INFO_STR_SET),
    REG_BITS_CMD_STR("unexpected nack",  RI_ERROR_NACK, REG_BITS_INFO_STR_SET),
    REG_BITS_CMD_STR("missing ack",      RI_ERROR_ACK,  REG_BITS_INFO_STR_SET),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_RI_WERROR[] = {
    REG_BITS_CMD_SETX(STRLEN("dirty bits: ")),
    REG_BITS_CMD_BIN("dirty bits", RI_BANK_DIRTY, 1),
    REG_BITS_CMD_BIN("valid bits", RI_BANK_VALID, 1),

    REG_BITS_CMD_END(),
};

const RegBitsInfo regBits_SI_STATUS[] = {
    REG_BITS_CMD_SETX(STRLEN("pif channel state: ")),
    REG_BITS_CMD_STR("interrupt",         SI_STATUS_INTERRUPT,      REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_BIN("dma state",         SI_STATUS_DMA_STATE_MASK, 2),
    REG_BITS_CMD_BIN("pif channel state", SI_STATUS_PCH_STATE_MASK, 2),
    REG_BITS_CMD_STR("dma error",         SI_STATUS_DMA_ERROR,      REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("read pending",      SI_STATUS_READ_PENDING,   REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("io busy",           SI_STATUS_RD_BUSY,        REG_BITS_INFO_STR_YES_NO),
    REG_BITS_CMD_STR("dma busy",          SI_STATUS_DMA_BUSY,       REG_BITS_INFO_STR_YES_NO),

    REG_BITS_CMD_END(),
};

typedef struct RegInspectExtraInfo {
    /*0x00*/ const enum RegisterSources src;
    /*0x01*/ const s8 idx;
    /*0x02*/ const RegBitsType singlePrint; // If != 0, then ignore 'list' and print the whole value in a different format.
    /*0x03*/ const u8 singlePrintArg;
    /*0x04*/ const RegBitsInfo* list;
} RegInspectExtraInfo; /*0x08*/
const RegInspectExtraInfo sRegInspectExtraInfoFuncs[] = {
    { .src = REGS_CP0,   .idx = REG_CP0_INX,            .list = regBits_C0_INDEX,  }, // $Index
    // $Random
    // $EntryLo0
    // $EntryLo1
    // $Context
    // $PageMask
    // $Wired
    // $BadVAddr (not a bitfield)
    // $Count
    // $EntryHi
    // $Compare
    { .src = REGS_CP0,   .idx = REG_CP0_SR,             .list = regBits_C0_SR,     }, // $Status
    { .src = REGS_CP0,   .idx = REG_CP0_CAUSE,          .list = regBits_C0_CAUSE,  }, // $Cause
    // $EPC (not a bitfield)
    // $PRId
    { .src = REGS_CP0,   .idx = REG_CP0_CONFIG,         .list = regBits_C0_CONFIG, }, // $Config
    // $LLAddr (not a bitfield?)
    // $WatchLo
    // $WatchHi
    // $XContext
    // $PErr
    // $CacheErr
    // $TagLo
    // $TagHi
    // $ErrorEPC (not a bitfield)

    // FCR $0 (imp/rev)
    { .src = REGS_FCR,   .idx = REG_FCR_CONTROL_STATUS, .list = regBits_FPR_CSR,  }, // $31

    { .src = REGS_SPC,   .idx = REG_SPC_RCP,            .list = regBits_SPC_RCP,  }, // __OSThreadContext.rcp

    { .src = REGS_RDRAM, .idx = REGID_RDRAM_CONFIG,     .list = regBits_RDRAM_CONFIG, }, // RDRAM_CONFIG_REG/RDRAM_DEVICE_TYPE_REG
    // RDRAM_DEVICE_ID_REG
    // RDRAM_DELAY_REG
    { .src = REGS_RDRAM, .idx = REGID_RDRAM_MODE,       .list = regBits_RDRAM_MODE,   },
    // RDRAM_REF_INTERVAL_REG
    // RDRAM_REF_ROW_REG
    // RDRAM_RAS_INTERVAL_REG
    // RDRAM_MIN_INTERVAL_REG
    // RDRAM_ADDR_SELECT_REG
    // RDRAM_DEVICE_MANUF_REG
    // RDRAM_ROW_REG

    // SP_MEM_ADDR_REG
    { .src = REGS_SP,    .idx = REGID_SP_DRAM_ADDR,     .singlePrint = REG_BITS_TYPE_ADDR, .singlePrintArg = 1, }, // SP_DRAM_ADDR_REG
    // SP_READ_LENGTH_REG
    // SP_WRITE_LENGTH_REG
    { .src = REGS_SP,    .idx = REGID_SP_STATUS,        .list = regBits_SP_STATUS,    }, // SP_STATUS_REG
    // SP_DMA_FULL_REG
    // SP_DMA_BUSY_REG
    // SP_SEMAPHORE_REG
    // SP_PC_REG
    // SP_IBIST_REG

    { .src = REGS_DPC,   .idx = REGID_DPC_START,        .singlePrint = REG_BITS_TYPE_ADDR, .singlePrintArg = 1, }, // DPC_START_REG
    { .src = REGS_DPC,   .idx = REGID_DPC_END,          .singlePrint = REG_BITS_TYPE_ADDR, .singlePrintArg = 1, }, // DPC_END_REG
    { .src = REGS_DPC,   .idx = REGID_DPC_CURRENT,      .singlePrint = REG_BITS_TYPE_ADDR, .singlePrintArg = 1, }, // DPC_CURRENT_REG
    { .src = REGS_DPC,   .idx = REGID_DPC_STATUS,       .list = regBits_DPC_STATUS,   }, // DPC_STATUS_REG
    // DPC_CLOCK_REG
    // DPC_BUFBUSY_REG
    // DPC_PIPEBUSY_REG
    // DPC_TMEM_REG

    // DPS_TBIST_REG
    // DPS_TEST_MODE_REG
    // DPS_BUFTEST_ADDR_REG
    // DPS_BUFTEST_DATA_REG

    // MI_INIT_MODE_REG/MI_MODE_REG
    // MI_VERSION_REG/MI_NOOP_REG
    // MI_INTERRUPT_REG
    // MI_INTERRUPT_MASK_REG

    { .src = REGS_VI,    .idx = REGID_VI_CONTROL,       .list = regBits_VI_CONTROL,   }, // VI_STATUS_REG/VI_CONTROL_REG
    { .src = REGS_VI,    .idx = REGID_VI_ORIGIN,        .singlePrint = REG_BITS_TYPE_ADDR, .singlePrintArg = 1, }, // VI_ORIGIN_REG/VI_DRAM_ADDR_REG
    { .src = REGS_VI,    .idx = REGID_VI_WIDTH,         .singlePrint = REG_BITS_TYPE_DEC,  .singlePrintArg = 1, }, // VI_WIDTH_REG/VI_H_WIDTH_REG
    // VI_INTERRUPT_REG/VI_V_INTERRUPT_REG
    // VI_CURRENT_REG/VI_V_CURRENT_LINE_REG
    { .src = REGS_VI,    .idx = REGID_VI_BURST,         .list = regBits_VI_BURST,     }, // VI_BURST_REG/VI_TIMING_REG
    { .src = REGS_VI,    .idx = REGID_VI_V_SYNC,        .singlePrint = REG_BITS_TYPE_DEC,  .singlePrintArg = 1, }, // VI_V_SYNC_REG
    { .src = REGS_VI,    .idx = REGID_VI_H_SYNC,        .list = regBits_VI_H_SYNC,    }, // VI_H_SYNC_REG
    // VI_H_SYNC_LEAP_REG
    // VI_H_START_REG/VI_VIDEO_REG
    // VI_V_START_REG/VI_VIDEO_REG
    // VI_V_BURST_REG
    // VI_X_SCALE_REG
    // VI_Y_SCALE_REG
    // VI_TEST_ADDR_REG
    // VI_STAGED_DATA_REG

    { .src = REGS_AI,    .idx = REGID_AI_DRAM_ADDR,     .singlePrint = REG_BITS_TYPE_ADDR,  .singlePrintArg = 1, }, // AI_DRAM_ADDR_REG
    { .src = REGS_AI,    .idx = REGID_AI_LEN,           .singlePrint = REG_BITS_TYPE_HEX,   .singlePrintArg = 1, }, // AI_LENGTH_REG
    { .src = REGS_AI,    .idx = REGID_AI_CONTROL,       .list = regBits_AI_CONTROL,   }, // AI_CONTROL_REG
    { .src = REGS_AI,    .idx = REGID_AI_STATUS,        .list = regBits_AI_STATUS,    }, // AI_STATUS_REG
    { .src = REGS_AI,    .idx = REGID_AI_DACRATE,       .singlePrint = REG_BITS_TYPE_DEC,   .singlePrintArg = 1, }, // AI_DAC_RATE_REG
    { .src = REGS_AI,    .idx = REGID_AI_BITRATE,       .singlePrint = REG_BITS_TYPE_DEC,   .singlePrintArg = 1, }, // AI_BIT_RATE_REG

    { .src = REGS_PI,    .idx = REGID_PI_DRAM_ADDR,     .singlePrint = REG_BITS_TYPE_ADDR,  .singlePrintArg = 1, }, // PI_DRAM_ADDR_REG
    // PI_CART_ADDR_REG
    // PI_READ_LENGTH_REG
    // PI_WRITE_LENGTH_REG
    { .src = REGS_PI,    .idx = REGID_PI_STATUS,        .list = regBits_PI_STATUS,    }, // PI_STATUS_REG
    // .../PI_DOMAIN1_REG
    // ...
    // .../PI_DOMAIN2_REG
    // ...

    { .src = REGS_RI,    .idx = REGID_RI_MODE,          .list = regBits_RI_MODE,      }, // RI_MODE_REG
    { .src = REGS_RI,    .idx = REGID_RI_CONFIG,        .list = regBits_RI_CONFIG,    }, // RI_CONFIG_REG
    // RI_CURRENT_LOAD_REG
    { .src = REGS_RI,    .idx = REGID_RI_SELECT,        .list = regBits_RI_SELECT,    }, // RI_SELECT_REG
    { .src = REGS_RI,    .idx = REGID_RI_REFRESH,       .list = regBits_RI_REFRESH,   }, // RI_REFRESH_REG/RI_COUNT_REG
    // RI_LATENCY_REG
    { .src = REGS_RI,    .idx = REGID_RI_RERROR,        .list = regBits_RI_RERROR,    }, // RI_READ_ERROR_REG
    { .src = REGS_RI,    .idx = REGID_RI_WERROR,        .list = regBits_RI_WERROR,    }, // RI_WRITE_ERROR_REG

    { .src = REGS_SI,    .idx = REGID_SI_DRAM_ADDR,     .singlePrint = REG_BITS_TYPE_ADDR,  .singlePrintArg = 1, }, // SI_DRAM_ADDR_REG
    // SI_PIF_ADDR_READ64B_REG
    // SI_PIF_ADDR_WRITE64B_REG
    { .src = REGS_SI,    .idx = REGID_SI_STATUS,        .list = regBits_SI_STATUS,    }, // SI_STATUS_REG
};
