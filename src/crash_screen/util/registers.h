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


// $Status register.
typedef union {
    struct PACKED {
        u32 ie  : 1; //  0   : Global interrupt enable         - Should interrupts be handled?
        u32 exl : 1; //  1   : Exception level                 - Are we currently handling an exception?
        u32 erl : 1; //  2   : Error level                     - Are we currently handling an error?
        u32 ksu : 2; //  3- 4: Execution mode                  - (00 = kernel, 01 = supervisor, 10 = user)
        u32 ux  : 1; //  5   : 64 bit addressing enabled in user mode.
        u32 sx  : 1; //  6   : 64 bit addressing enabled in supervisor mode.
        u32 kx  : 1; //  7   : 64 bit addressing enabled in kernel mode.
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

// $Cause register.
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

// tc->cause register.
typedef union {
    struct PACKED {
        u32 delaySlot :  1; // Exception triggered in delay slot.
        u32 cop_num   :  2; // Coprocessor_exception.
        u32           : 24;
        u32 exc_code  :  5; // Exception Code.
    };
    u32 raw;
} Reg_CauseT;

// tc->fpcsr register.
typedef union {
    struct PACKED {
        u32                   :  7;
        u32 flushDenormToZero :  1; // Flush denorm to zero.
        u32 cond              :  1; // Condition bit.
        u32                   :  5;
        u32 cause_bits        :  6; // See Reg_FPCSR.cause.
        u32 enable_bits       :  5; // See Reg_FPCSR.enable.
        u32 flag_bits         :  5; // See Reg_FPCSR.flag.
        u32 rounding_mode     :  2; // Round to: (1:zero, 2:+inf, 3:-inf).
    };
    struct PACKED {
        u32               : 14;
        u32 unimplemented :  1; // cause: unimplemented operation
        u32 invalid       :  1; // cause: invalid operation
        u32 div0          :  1; // cause: division by zero
        u32 overflow      :  1; // cause: overflow
        u32 underflow     :  1; // cause: underflow
        u32 inexact       :  1; // cause: inexact operation
        u32               : 12;
    } cause;
    struct PACKED {
        u32               : 20;
        u32 invalid       :  1; // enable: invalid operation
        u32 div0          :  1; // enable: division by zero
        u32 overflow      :  1; // enable: overflow
        u32 underflow     :  1; // enable: underflow
        u32 inexact       :  1; // enable: inexact operation
        u32               :  7;
    } enable;
    struct PACKED {
        u32               : 25;
        u32 invalid       :  1; // flag: invalid operation
        u32 div0          :  1; // flag: division by zero
        u32 overflow      :  1; // flag: overflow
        u32 underflow     :  1; // flag: underflow
        u32 inexact       :  1; // flag: inexact operation
        u32               :  2;
    } flag;
    u32 raw;
} Reg_FPCSR;


enum COP1Registers {
    REG_COP1_F00, REG_COP1_F01, REG_COP1_F02, REG_COP1_F03,                                                                                                                 // Subroutine return value.
    REG_COP1_F04, REG_COP1_F05, REG_COP1_F06, REG_COP1_F07, REG_COP1_F08, REG_COP1_F09, REG_COP1_F10,  REG_COP1_F11,                                                        // Temporary values.
    REG_COP1_F12, REG_COP1_F13, REG_COP1_F14, REG_COP1_F15,                                                                                                                 // Subroutine arguments.
    REG_COP1_F16, REG_COP1_F17, REG_COP1_F18, REG_COP1_F19,                                                                                                                 // Temporary values.
    REG_COP1_F20, REG_COP1_F21, REG_COP1_F22, REG_COP1_F23, REG_COP1_F24, REG_COP1_F25, REG_COP1_F26, REG_COP1_F27, REG_COP1_F28, REG_COP1_F29, REG_COP1_F30, REG_COP1_F31, // Saved Values.
    COP1_NUM_REGISTERS,
};

enum FloatErrorType {
    FLT_ERR_NONE,
    FLT_ERR_DENORM,
    FLT_ERR_NAN,
    NUM_FLT_ERR,
};


typedef struct {
    /*0x00*/ const u16 offset;
    /*0x02*/ const u8 size;
    /*0x03*/ const char shortName[3];
    /*0x06*/ const char name[10];
} RegisterInfo; /*0x10*/

typedef union {
    struct {
        /*0x00*/ s8 cop;
        /*0x01*/ s8 idx;
        /*0x02*/ _Bool flt;
        /*0x03*/ _Bool out;
    }; /*0x04*/
    /*0x04*/ u32 raw;
} RegisterId; /*0x04*/


#define OSTHREAD_NULL_OFFSET (u16)-1

#define DEF_SREG(_size, _name, _shortName) {    \
    .offset    = OSTHREAD_NULL_OFFSET,          \
    .size      = _size,                         \
    .name      = _name,                         \
    .shortName = _shortName,                    \
}

#define DEF_TREG(_field, _size, _name, _shortName) {            \
    .offset    = __builtin_offsetof(__OSThreadContext, _field), \
    .size      = sizeof_member(__OSThreadContext, _field),      \
    .name      = _name,                                         \
    .shortName = _shortName,                                    \
}

#define DEF_CPU_SREG(_reg, _name) DEF_SREG(      sizeof(u64), _name, _name)
#define DEF_CPU_TREG(_reg, _name) DEF_TREG(_reg, sizeof(u64), _name, _name)

#define DEF_COP0_SREG(_reg, _size,         _name, _shortName) DEF_SREG(        _size, _name, _shortName)
#define DEF_COP0_TREG(_reg, _size, _field, _name, _shortName) DEF_TREG(_field, _size, _name, _shortName)

#define DEF_COP1_SREG(_reg, _name)      DEF_SREG(                   sizeof(f32), "F"_name, _name)
#define DEF_COP1_TREG_EVEN(_reg, _name) DEF_TREG(fp##_reg.f.f_even, sizeof(f32), "F"_name, _name)
#define DEF_COP1_TREG_ODD(_reg, _name)  DEF_TREG(fp##_reg.f.f_odd,  sizeof(f32), "F"_name, _name)

#define REG_BUFFER_SIZE 3

extern RegisterId gSavedRegBuf[REG_BUFFER_SIZE];
extern int gSavedRegBufSize;

const RegisterInfo* get_reg_info(enum Coprocessors cop, int idx);
uint64_t get_direct_reg_val(enum Coprocessors cop, int idx);
uint64_t get_reg_val(enum Coprocessors cop, int idx);

void clear_saved_reg_buffer(void);
void append_reg_to_buffer(enum Coprocessors cop, int idx, _Bool isFlt, _Bool isOutput);

enum FloatErrorType validate_float(IEEE754_f32 val);
