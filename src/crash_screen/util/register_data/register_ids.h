
typedef enum REGIDS_CPU {
    REG_CPU_R0,                                                                                     // $zero. Hardware enforced.
    REG_CPU_AT,                                                                                     // Assembler temporary value. Don't use unless you know it's safe.
    REG_CPU_V0, REG_CPU_V1,                                                                         // Subroutine return value. V1 is used for 64 bit returns.
    REG_CPU_A0, REG_CPU_A1, REG_CPU_A2, REG_CPU_A3,                                                 // Subroutine arguments. If more are needed, they are stored to sp+0x10 in the calling stack space.
    REG_CPU_T0, REG_CPU_T1, REG_CPU_T2, REG_CPU_T3, REG_CPU_T4, REG_CPU_T5, REG_CPU_T6, REG_CPU_T7, // Temporary values. Not saved between functions.
    REG_CPU_S0, REG_CPU_S1, REG_CPU_S2, REG_CPU_S3, REG_CPU_S4, REG_CPU_S5, REG_CPU_S6, REG_CPU_S7, // Saved values. Saved to stack before modifying and then restored before returning.
    REG_CPU_T8, REG_CPU_T9,                                                                         // Temporary values.
    REG_CPU_K0, REG_CPU_K1,                                                                         // Reserved by kernel. Do not modify when using interrupts.
    REG_CPU_GP,                                                                                     // Global pointer. This can pointto 64kb of small misc variables.
    REG_CPU_SP,                                                                                     // Stack pointer. Subtract to allocate stack space and add back to deallocate.
    REG_CPU_FP,                                                                                     // Saved value 8 ("S8") or frame pointer ("FP"), depending on compiler.
    REG_CPU_RA,                                                                                     // Return address. Jump to this to return from a function. Hardware enforced.
    CPU_NUM_REGISTERS,
} REGIDS_CPU;

typedef enum RegDesc_default {
    REG_DESC_ZERO,
    REG_DESC_AT,
    REG_DESC_SUBRET,
    REG_DESC_SUBARG,
    REG_DESC_TEMP,
    REG_DESC_SAVED,
    REG_DESC_KERNEL,
    REG_DESC_GP,
    REG_DESC_SP,
    REG_DESC_FP,
    REG_DESC_RA,
} RegDesc_default;

typedef enum REGIDS_CP0 {
    REG_CP0_INX       = C0_INX,         // Programmable pointer into TLB array.
    REG_CP0_RAND      = C0_RAND,        // Pseudorandom pointer into TLB array (read only).
    REG_CP0_ENTRYLO0  = C0_ENTRYLO0,    // Low half of TLB entry for even virtual addresses (VPN).
    REG_CP0_ENTRYLO1  = C0_ENTRYLO1,    // Low half of TLB entry for odd virtual addresses (VPN).
    REG_CP0_CONTEXT   = C0_CONTEXT,     // Pointer to kernel virtual page table entry (PTE) in 32-bit mode.
    REG_CP0_PAGEMASK  = C0_PAGEMASK,    // Page size specification.
    REG_CP0_WIRED     = C0_WIRED,       // Number of wired TLB entries.
    REG_CP0_7         = 7,              // Reserved for future use.
    REG_CP0_BADVADDR  = C0_BADVADDR,    // Display of virtual address that occurred an error last.
    REG_CP0_COUNT     = C0_COUNT,       // Timer Count.
    REG_CP0_ENTRYHI   = C0_ENTRYHI,     // High half of TLB entry (including ASID).
    REG_CP0_COMPARE   = C0_COMPARE,     // Timer Compare Value.
    REG_CP0_SR        = C0_SR,          // Operation status setting.
    REG_CP0_CAUSE     = C0_CAUSE,       // Display of cause of last exception.
    REG_CP0_EPC       = C0_EPC,         // Exception Program Counter.
    REG_CP0_PRID      = C0_PRID,        // Processor Revision Identifier.
    REG_CP0_CONFIG    = C0_CONFIG,      // Memory system mode setting.
    REG_CP0_LLADDR    = C0_LLADDR,      // Load Linked instruction address display.
    REG_CP0_WATCHLO   = C0_WATCHLO,     // Memory reference trap address low bits.
    REG_CP0_WATCHHI   = C0_WATCHHI,     // Memory reference trap address high bits.
    REG_CP0_XCONTEXT  = 20,             // Pointer to Kernel virtual PTE table in 64-bit mode.
    REG_CP0_21        = 21,             // Reserved for future use.
    REG_CP0_22        = 22,             // Reserved for future use.
    REG_CP0_23        = 23,             // Reserved for future use.
    REG_CP0_24        = 24,             // Reserved for future use.
    REG_CP0_25        = 25,             // Reserved for future use.
    REG_CP0_ECC       = C0_ECC,         // Cache parity bits.
    REG_CP0_CACHE_ERR = C0_CACHE_ERR,   // Cache Error and Status register.
    REG_CP0_TAGLO     = C0_TAGLO,       // Cache Tag register low.
    REG_CP0_TAGHI     = C0_TAGHI,       // Cache Tag register high.
    REG_CP0_ERROR_EPC = C0_ERROR_EPC,   // Error Exception Program Counter.
    REG_CP0_31        = 31,             // Reserved for future use.
    CP0_NUM_REGISTERS,
} REGIDS_CP0;

typedef enum RegDesc_CP0 {
    REG_DESC_CP0_INX,
    REG_DESC_CP0_RAND,
    REG_DESC_CP0_ENTRYLO0,
    REG_DESC_CP0_ENTRYLO1,
    REG_DESC_CP0_CONTEXT,
    REG_DESC_CP0_PAGEMASK,
    REG_DESC_CP0_WIRED,
    REG_DESC_CP0_BADVADDR,
    REG_DESC_CP0_COUNT,
    REG_DESC_CP0_ENTRYHI,
    REG_DESC_CP0_COMPARE,
    REG_DESC_CP0_SR,
    REG_DESC_CP0_CAUSE,
    REG_DESC_CP0_EPC,
    REG_DESC_CP0_PRID,
    REG_DESC_CP0_CONFIG,
    REG_DESC_CP0_LLADDR,
    REG_DESC_CP0_WATCHLO,
    REG_DESC_CP0_WATCHHI,
    REG_DESC_CP0_XCONTEXT,
    REG_DESC_CP0_ECC,
    REG_DESC_CP0_CACHE_ERR,
    REG_DESC_CP0_TAGLO,
    REG_DESC_CP0_TAGHI,
    REG_DESC_CP0_ERROR_EPC,
    REG_DESC_CP0_RESERVED,
} RegDesc_CP0;

typedef enum REGIDS_CP1 {
    REG_CP1_F00, REG_CP1_F01, REG_CP1_F02, REG_CP1_F03,                                                                                                                 // Subroutine return value.
    REG_CP1_F04, REG_CP1_F05, REG_CP1_F06, REG_CP1_F07, REG_CP1_F08, REG_CP1_F09, REG_CP1_F10,  REG_CP1_F11,                                                        // Temporary values.
    REG_CP1_F12, REG_CP1_F13, REG_CP1_F14, REG_CP1_F15,                                                                                                                 // Subroutine arguments.
    REG_CP1_F16, REG_CP1_F17, REG_CP1_F18, REG_CP1_F19,                                                                                                                 // Temporary values.
    REG_CP1_F20, REG_CP1_F21, REG_CP1_F22, REG_CP1_F23, REG_CP1_F24, REG_CP1_F25, REG_CP1_F26, REG_CP1_F27, REG_CP1_F28, REG_CP1_F29, REG_CP1_F30, REG_CP1_F31, // Saved Values.
    CP1_NUM_REGISTERS,
} REGIDS_CP1;

typedef enum REGIDS_FCR {
    REG_FCR_IMPL_REV       =  0,
    REG_FCR_CONTROL_STATUS = 31,
    FCR_NUM_REGISTERS,
} REGIDS_FCR;

typedef enum RegDesc_FCR {
    REG_DESC_FCR_COP_IMPL_REV,
    REG_DESC_FCR_CONTROL_STATUS,
} RegDesc_FCR;

typedef enum REGIDS_SPC {
    // REG_SPC_PC,    // Program Counter
    REG_SPC_LO,    // mflo/mtlo
    REG_SPC_HI,    // mfhi/mthi
    // REG_SPC_LLBIT, // Load Linked: set by LL, cleared by ERET, tested by SC
    REG_SPC_RCP,   // From OSThread, not a vr4300 register.
} REGIDS_SPC;
