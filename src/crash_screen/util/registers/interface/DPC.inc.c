// -- RDP Command (DPC) --
// https://n64brew.dev/wiki/Reality_Display_Processor/Interface


static const char* sRegDesc_DPC[] = {
    [REGID_DPC_START   ] = "DMA start", // Start address in RDRAM / DMEM for a DMA transfer of RDP commands.
    [REGID_DPC_END     ] = "DMA end",  // End address in RDRAM / DMEM for a DMA transfer of RDP commands (exclusive bound).
    [REGID_DPC_CURRENT ] = "DMA current", // Current address in RDRAM / DMEM being transferred by the DMA engine.
    [REGID_DPC_STATUS  ] = "Status", // Status register.
    [REGID_DPC_CLOCK   ] = "Clock counter",
    [REGID_DPC_BUFBUSY ] = "Buffer busy counter",
    [REGID_DPC_PIPEBUSY] = "Pipe busy counter",
    [REGID_DPC_IMEM    ] = "TMEM load counter",
};
ALIGNED32 static const RegisterInfo sRegInfo_DPC[] = {
    [REGID_DPC_START   ] = DEF_IREG(DPC_START_REG,    "START",    REGID_DPC_START   ),
    [REGID_DPC_END     ] = DEF_IREG(DPC_END_REG,      "END",      REGID_DPC_END     ),
    [REGID_DPC_CURRENT ] = DEF_IREG(DPC_CURRENT_REG,  "CURRENT",  REGID_DPC_CURRENT ),
    [REGID_DPC_STATUS  ] = DEF_IREG(DPC_STATUS_REG,   "STATUS",   REGID_DPC_STATUS  ),
    [REGID_DPC_CLOCK   ] = DEF_IREG(DPC_CLOCK_REG,    "CLOCK",    REGID_DPC_CLOCK   ),
    [REGID_DPC_BUFBUSY ] = DEF_IREG(DPC_BUFBUSY_REG,  "BUFBUSY",  REGID_DPC_BUFBUSY ),
    [REGID_DPC_PIPEBUSY] = DEF_IREG(DPC_PIPEBUSY_REG, "PIPEBUSY", REGID_DPC_PIPEBUSY),
    [REGID_DPC_IMEM    ] = DEF_IREG(DPC_TMEM_REG,     "TMEM",     REGID_DPC_IMEM    ),
};
static const RegisterSource sRegisters_DPC = DEF_REG_LIST_INTERFACE(
    "DPC",
    "Reality Display Processor (RDP) Command",
    sRegDesc_DPC,
    sRegInfo_DPC
);
