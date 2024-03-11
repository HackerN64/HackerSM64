#pragma once

#include <ultra64.h>

#include "types.h"


#define STR_REG_PREFIX "$"


// Coprocessors.
enum Coprocessors {
    CPU = -1,
    COP0, // Coprocessor-0 (System Control Coprocessor).
    COP1, // Coprocessor-1 (Floating-Point Unit).
    COP2, // Coprocessor-2 (Reality Co-Processor Vector Unit).
    COP3, // Coprocessor-3 (CP3).
};

enum CPURegisters {
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
};

enum COP0Registers {
    REG_COP0_INX       = C0_INX,         // Programmable pointer into TLB array.
    REG_COP0_RAND      = C0_RAND,        // Pseudorandom pointer into TLB array (read only).
    REG_COP0_ENTRYLO0  = C0_ENTRYLO0,    // Low half of TLB entry for even virtual addresses (VPN).
    REG_COP0_ENTRYLO1  = C0_ENTRYLO1,    // Low half of TLB entry for odd virtual addresses (VPN).
    REG_COP0_CONTEXT   = C0_CONTEXT,     // Pointer to kernel virtual page table entry (PTE) in 32-bit mode.
    REG_COP0_PAGEMASK  = C0_PAGEMASK,    // Page size specification.
    REG_COP0_WIRED     = C0_WIRED,       // Number of wired TLB entries.
    REG_COP0_7         = 7,              // Reserved for future use.
    REG_COP0_BADVADDR  = C0_BADVADDR,    // Display of virtual address that occurred an error last.
    REG_COP0_COUNT     = C0_COUNT,       // Timer Count.
    REG_COP0_ENTRYHI   = C0_ENTRYHI,     // High half of TLB entry (including ASID).
    REG_COP0_COMPARE   = C0_COMPARE,     // Timer Compare Value.
    REG_COP0_SR        = C0_SR,          // Operation status setting.
    REG_COP0_CAUSE     = C0_CAUSE,       // Display of cause of last exception.
    REG_COP0_EPC       = C0_EPC,         // Exception Program Counter.
    REG_COP0_PRID      = C0_PRID,        // Processor Revision Identifier.
    REG_COP0_CONFIG    = C0_CONFIG,      // Memory system mode setting.
    REG_COP0_LLADDR    = C0_LLADDR,      // Load Linked instruction address display.
    REG_COP0_WATCHLO   = C0_WATCHLO,     // Memory reference trap address low bits.
    REG_COP0_WATCHHI   = C0_WATCHHI,     // Memory reference trap address high bits.
    REG_COP0_XCONTEXT  = 20,             // Pointer to Kernel virtual PTE table in 64-bit mode.
    REG_COP0_21        = 21,             // Reserved for future use.
    REG_COP0_22        = 22,             // Reserved for future use.
    REG_COP0_23        = 23,             // Reserved for future use.
    REG_COP0_24        = 24,             // Reserved for future use.
    REG_COP0_25        = 25,             // Reserved for future use.
    REG_COP0_ECC       = C0_ECC,         // Cache parity bits.
    REG_COP0_CACHE_ERR = C0_CACHE_ERR,   // Cache Error and Status register.
    REG_COP0_TAGLO     = C0_TAGLO,       // Cache Tag register low.
    REG_COP0_TAGHI     = C0_TAGHI,       // Cache Tag register high.
    REG_COP0_ERROR_EPC = C0_ERROR_EPC,   // Error Exception Program Counter.
    REG_COP0_31        = 31,             // Reserved for future use.
    COP0_NUM_REGISTERS,
};


typedef union {
    struct PACKED {
        u32             :  2; //  0- 1: Unused            - Always zero.
        u32 exception   :  4; //  2- 6: Exception code    - Which exception/interrupt occurred?
        u32             :  1; //  7   : Unused            - Always zero.
        u32 interrupt   :  8; //  8-15: Interrupt Pending - Which interrupts are waiting to be serviced? Used with Interrupt Mask on $Status.
        u32             : 12; // 16-27: Unused            - Always zero.
        u32 coprocessor :  2; // 28-29: Coprocessor error - Which coprocessor threw the exception, often not used.
        u32             :  1; // 30   : Unused            - Always zero.
        u32 delay       :  1; // 31   : Branch delay      - Did the exception/interrupt occur in a branch delay slot?
    };
    u32 raw;
} Reg_Cause;

typedef union {
    struct PACKED {
        u32 ie  : 1; //  0   : Global interrupt enable         - Should interrupts be handled?
        u32 exl : 1; //  1   : Exception level                 - Are we currently handling an exception?
        u32 erl : 1; //  2   : Error level                     - Are we currently handling an error?
        u32 ksu : 2; //  3- 4: Execution mode                  - (00 = kernel, 01 = supervisor, 10 = user)
        u32 ux  : 1; //  5   : 64 bit addressing enabled in user mode
        u32 sx  : 1; //  6   : 64 bit addressing enabled in supervisor mode
        u32 kx  : 1; //  7   : 64 bit addressing enabled in kernel mode
        u32 im  : 8; //  8-15: Interrupt mask                  - &’d against interrupt pending in $Cause
        u32 ds  : 9; // 16-24: Diagnostic status               - TODO: description
        u32 re  : 1; // 25   : Reverse endianness              - (0 = big endian, 1 = little endian)
        u32 fr  : 1; // 26   : Enables additional fp registers - (0 = 16 regs, 1 = 32 regs)
        u32 rp  : 1; // 27   : Enable low power mode           - Run the CPU at 1/4th clock speed)
        u32 cu0 : 1; // 28   : Coprocessor 0 enabled           - This bit is ignored by the N64, COP0 is always enabled!
        u32 cu1 : 1; // 29   : Coprocessor 1 enabled           - If this bit is 0, all COP1 instructions throw exceptions
        u32 cu2 : 1; // 30   : Coprocessor 2 enabled           - This bit is ignored by the N64, there is no COP2!
        u32 cu3 : 1; // 31   : Coprocessor 3 enabled           - This bit is ignored by the N64, there is no COP3!
    };
    u32 raw;
} Reg_Status;


enum COP1Registers {
    REG_COP1_F00, REG_COP1_F02,                                                         // Subroutine return value.
    REG_COP1_F04, REG_COP1_F06, REG_COP1_F08, REG_COP1_F10,                             // Temporary values.
    REG_COP1_F12, REG_COP1_F14,                                                         // Subroutine arguments.
    REG_COP1_F16, REG_COP1_F18,                                                         // Temporary values.
    REG_COP1_F20, REG_COP1_F22, REG_COP1_F24, REG_COP1_F26, REG_COP1_F28, REG_COP1_F30, // Saved Values.
    COP1_NUM_REGISTERS,
};


typedef union {
    struct {
        /*0x00*/ s16 cop;
        /*0x02*/ s16 idx;
    }; /*0x04*/
    /*0x04*/ u32 raw;
} RegisterId; /*0x04*/

typedef struct {
    /*0x00*/ const char* asmReg; /*0x04*/
    /*0x04*/ const u16 offset; /*0x01*/
    /*0x06*/ const u8 size;
    /*0x07*/ const char shortName[3]; /*0x03*/
    /*0x0A*/ const char name[10]; /*0x0A*/
} RegisterInfo; /*0x14*/

#define DEF_REG(_asmReg, _size, _offset, _name, _shortName) { \
    .asmReg    = _asmReg,       \
    .offset    = _offset,       \
    .size      = _size,         \
    .name      = _name,         \
    .shortName = _shortName,    \
}

#define DEF_SREG(_reg, _size, _name, _shortName) {  \
    .asmReg    = TO_STRING2(_reg),                  \
    .offset    = (u16)-1,                           \
    .size      = _size,                             \
    .name      = _name,                             \
    .shortName = _shortName,                        \
}

#define DEF_TREG(_reg, _field, _size, _name, _shortName) {      \
    .asmReg    = TO_STRING2(_reg),                              \
    .offset    = __builtin_offsetof(__OSThreadContext, _field), \
    .size      = sizeof_member(__OSThreadContext, _field),      \
    .name      = _name,                                         \
    .shortName = _shortName,                                    \
}

#define DEF_CPU_SREG(_reg, _name) DEF_SREG(_reg, sizeof(u64), _name, _name)
#define DEF_CPU_TREG(_reg, _name) DEF_TREG(_reg, _reg, sizeof(u64), _name, _name)

#define DEF_COP0_SREG(_reg, _size,         _name, _shortName) DEF_SREG(_reg,         _size, _name, _shortName)
#define DEF_COP0_TREG(_reg, _size, _field, _name, _shortName) DEF_TREG(_reg, _field, _size, _name, _shortName)

#define DEF_COP1_SREG(_reg, _name) DEF_SREG(_reg,                    sizeof(f32), "F"_name, _name)
#define DEF_COP1_TREG(_reg, _name) DEF_TREG(_reg, fp##_reg.f.f_even, sizeof(f32), "F"_name, _name)

#define REG_BUFFER_SIZE 3

extern RegisterId gSavedRegBuf[REG_BUFFER_SIZE];
extern int gSavedRegBufSize;

const RegisterInfo* get_reg_info(enum Coprocessors cop, int idx);
u64 get_reg_val(enum Coprocessors cop, int idx);

void clear_saved_reg_buffer(void);
void append_reg_to_buffer(s16 cop, s16 idx);
