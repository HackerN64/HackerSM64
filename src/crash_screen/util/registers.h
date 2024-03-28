#pragma once

#include <ultra64.h>

#include "types.h"

#include "reg_bits.h"


#define STR_REG_PREFIX "$"


// Coprocessors.
enum Coprocessors {
    CPU = -1,
    COP0, // Coprocessor-0 (System Control Coprocessor).
    COP1, // Coprocessor-1 (Floating-Point Unit).
    COP2, // Coprocessor-2 (Reality Co-Processor Vector Unit).
    COP3, // Coprocessor-3 (CP3).
    FCR,  // Coprocessor-1 Control/Status registers
    SPC,  // Special registers
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

enum RegDesc {
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
};

enum CP0Registers {
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
};

enum RegDesc_CP0 {
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
};

enum CP1Registers {
    REG_CP1_F00, REG_CP1_F01, REG_CP1_F02, REG_CP1_F03,                                                                                                                 // Subroutine return value.
    REG_CP1_F04, REG_CP1_F05, REG_CP1_F06, REG_CP1_F07, REG_CP1_F08, REG_CP1_F09, REG_CP1_F10,  REG_CP1_F11,                                                        // Temporary values.
    REG_CP1_F12, REG_CP1_F13, REG_CP1_F14, REG_CP1_F15,                                                                                                                 // Subroutine arguments.
    REG_CP1_F16, REG_CP1_F17, REG_CP1_F18, REG_CP1_F19,                                                                                                                 // Temporary values.
    REG_CP1_F20, REG_CP1_F21, REG_CP1_F22, REG_CP1_F23, REG_CP1_F24, REG_CP1_F25, REG_CP1_F26, REG_CP1_F27, REG_CP1_F28, REG_CP1_F29, REG_CP1_F30, REG_CP1_F31, // Saved Values.
    CP1_NUM_REGISTERS,
};

enum FCRRegisters {
    REG_FCR_IMPL_REV       =  0,
    REG_FCR_CONTROL_STATUS = 31,
    FCR_NUM_REGISTERS,
};

enum RegDesc_FCR {
    REG_DESC_FCR_COP_IMPL_REV,
    REG_DESC_FCR_CONTROL_STATUS,
};

enum SpecialRegisters {
    // REG_SPC_PC,    // Program Counter
    REG_SPC_LO,    // mflo/mtlo
    REG_SPC_HI,    // mfhi/mthi
    // REG_SPC_LLBIT, // Load Linked: set by LL, cleared by ERET, tested by SC
    REG_SPC_RCP,   // From OSThread, not a vr4300 register.
};


enum FloatErrorType {
    FLT_ERR_NONE,
    FLT_ERR_DENORM,
    FLT_ERR_NAN,
    NUM_FLT_ERR,
};


typedef struct RegisterInfo {
    /*0x00*/ const u16 offset;
    /*0x02*/ const u8 size;
    /*0x03*/ const u8 descId;
    /*0x04*/ const char* name;
    /*0x08*/ const char shortName[4];
} RegisterInfo; /*0x0C*/

typedef struct CoprocessorInfo {
    const char* name;
    uint64_t (*getRegValFunc)(int idx);
    const RegisterInfo* (*getRegInfoFunc)(int idx);
    const char** regDescList;
} CoprocessorInfo;

enum RegisterValueTypes {
    REG_VAL_TYPE_INT,
    REG_VAL_TYPE_FLOAT,
    REG_VAL_TYPE_ADDR,
};

typedef union RegisterId {
    struct {
        /*0x00*/ s8 cop;
        /*0x01*/ s8 idx;
        /*0x02*/ union {
            /*0x02*/ struct {
                /*0x02*/ u8 type; // enum RegisterValueTypes
                /*0x03*/ struct PACKED {
                            u8     : 6;
                            u8 dbl : 1; // is 64-bit?
                            u8 out : 1; // is output?
                        };
                    };
            u16 raw;
        } valInfo;
    }; /*0x04*/
    /*0x04*/ u32 raw;
} RegisterId; /*0x04*/


#define OSTHREAD_NULL_OFFSET (u16)-1

#define DEF_SREG(_size, _name, _shortName, _descId) {   \
    .offset    = OSTHREAD_NULL_OFFSET,                  \
    .size      = _size,                                 \
    .name      = _name,                                 \
    .shortName = _shortName,                            \
    .descId    = _descId,                               \
}

#define DEF_TREG(_field, _size, _name, _shortName, _descId) {   \
    .offset    = __builtin_offsetof(__OSThreadContext, _field), \
    .size      = sizeof_member(__OSThreadContext, _field),      \
    .name      = _name,                                         \
    .shortName = _shortName,                                    \
    .descId    = _descId,                                       \
}

#define DEF_CPU_SREG(_reg, _name, _descId) DEF_SREG(      sizeof(u64), _name, _name, _descId)
#define DEF_CPU_TREG(_reg, _name, _descId) DEF_TREG(_reg, sizeof(u64), _name, _name, _descId)

#define DEF_CP0_SREG(_reg, _type,         _name, _shortName, _descId) DEF_SREG(        sizeof(_type), _name, _shortName, _descId)
#define DEF_CP0_TREG(_reg, _type, _field, _name, _shortName, _descId) DEF_TREG(_field, sizeof(_type), _name, _shortName, _descId)

#define DEF_CP1_SREG(_reg, _name, _descId)      DEF_SREG(                   sizeof(f32), "F"_name, _name, _descId)
#define DEF_CP1_TREG_EVEN(_reg, _name, _descId) DEF_TREG(fp##_reg.f.f_even, sizeof(f32), "F"_name, _name, _descId)
#define DEF_CP1_TREG_ODD(_reg, _name, _descId)  DEF_TREG(fp##_reg.f.f_odd,  sizeof(f32), "F"_name, _name, _descId)

#define CASE_REG(_cop, _idx, _reg) case _idx: ASM_GET_REG_##_cop(val, STR_REG_PREFIX TO_STRING2(_reg)); break;


extern OSThread* __osRunningThread;


#define REG_BUFFER_SIZE 3
extern RegisterId gSavedRegBuf[REG_BUFFER_SIZE];
extern int gSavedRegBufSize;

const char* get_coprocessor_name(enum Coprocessors cop);

const RegisterInfo* get_reg_info(enum Coprocessors cop, int idx);
uint64_t get_thread_reg_val(enum Coprocessors cop, int idx, OSThread* thread);
uint64_t get_direct_reg_val(enum Coprocessors cop, int idx);
uint64_t get_reg_val(enum Coprocessors cop, int idx);
const char* get_reg_desc(enum Coprocessors cop, int idx);

void clear_saved_reg_buffer(void);
void append_reg_to_buffer(enum Coprocessors cop, int idx, enum RegisterValueTypes type, _Bool isOutput);

enum FloatErrorType validate_f32(IEEE754_f32 val);
enum FloatErrorType validate_f64(IEEE754_f64 val);

ALWAYS_INLINE static f32 f32_from_word(u32 x) {
    return ((IEEE754_f32){ .asU32 = x, }).asF32;
}

ALWAYS_INLINE static f64 f64_from_doubleword(u64 x) {
    return ((IEEE754_f64){ .asU64 = x, }).asF64;
}
