// -- Reality Signal Processor (RSP) (SP) --
// https://n64brew.dev/wiki/Reality_Signal_Processor/Interface


static const char* sRegDesc_SP[] = {
    [REGID_SP_MEM_ADDR ] = "DMA address in IMEM/DMEM", // Address in IMEM/DMEM for a DMA transfer.
    [REGID_SP_DRAM_ADDR] = "DMA address in RDRAM", // Address in RDRAM for a DMA transfer.
    [REGID_SP_RD_LEN   ] = "DMA read length", // Length of a DMA transfer. Writing this register triggers a DMA transfer from RDRAM to IMEM/DMEM.
    [REGID_SP_WR_LEN   ] = "DMA write length ", // Length of a DMA transfer. Writing this register triggers a DMA transfer from IMEM/DMEM to RDRAM.
    [REGID_SP_STATUS   ] = "RSP status", // RSP status register.
    [REGID_SP_DMA_FULL ] = "DMA pending", // Report whether there is a pending DMA transfer (mirror of DMA_FULL bit of SP_STATUS).
    [REGID_SP_DMA_BUSY ] = "DMA in progress", // 	Report whether there is a DMA transfer in progress (mirror of DMA_BUSY bit of SP_STATUS).
    [REGID_SP_SEMAPHORE] = "CPU <-> RSP mutex", // Register to assist implementing a simple mutex between VR4300 and RSP.
    [REGID_SP_PC       ] = "Program counter",
    [REGID_SP_IBIST    ] = "BIST Status",
};
ALIGNED32 static const RegisterInfo sRegInfo_SP[] = {
    [REGID_SP_MEM_ADDR ] = DEF_IREG(SP_MEM_ADDR_REG,  "MEM_ADDR",     REGID_SP_MEM_ADDR ),
    [REGID_SP_DRAM_ADDR] = DEF_IREG(SP_DRAM_ADDR_REG, "DRAM_ADDR",    REGID_SP_DRAM_ADDR),
    [REGID_SP_RD_LEN   ] = DEF_IREG(SP_RD_LEN_REG,    "READ_LENGTH",  REGID_SP_RD_LEN   ),
    [REGID_SP_WR_LEN   ] = DEF_IREG(SP_WR_LEN_REG,    "WRITE_LENGTH", REGID_SP_WR_LEN   ),
    [REGID_SP_STATUS   ] = DEF_IREG(SP_STATUS_REG,    "STATUS",       REGID_SP_STATUS   ),
    [REGID_SP_DMA_FULL ] = DEF_IREG(SP_DMA_FULL_REG,  "DMA_FULL",     REGID_SP_DMA_FULL ),
    [REGID_SP_DMA_BUSY ] = DEF_IREG(SP_DMA_BUSY_REG,  "DMA_BUSY",     REGID_SP_DMA_BUSY ),
    [REGID_SP_SEMAPHORE] = DEF_IREG(SP_SEMAPHORE_REG, "SEMAPHORE",    REGID_SP_SEMAPHORE),
    [REGID_SP_PC       ] = DEF_IREG(SP_PC_REG,        "PC",           REGID_SP_PC       ),
    [REGID_SP_IBIST    ] = DEF_IREG(SP_IBIST_REG,     "IBIST",        REGID_SP_IBIST    ),
};
static const RegisterSource sRegisters_SP = DEF_REG_LIST_INTERFACE(
    "SP",
    "Reality Signal Processor (RSP)",
    sRegDesc_SP,
    sRegInfo_SP
);
