#pragma once

#include <ultra64.h>

#include "types.h"

#include "reg_bits.h"


#define STR_REG_PREFIX "$"


enum PACKED Coprocessors {
    COP0, // Coprocessor-0 (System Control Coprocessor).
    COP1, // Coprocessor-1 (Floating-Point Unit).
    COP2, // Coprocessor-2 (Reality Co-Processor Vector Unit).
    COP3, // Coprocessor-3 (CP3).
};

enum PACKED Interfaces {
    INTERFACE_RDRAM,     // RDRAM_BASE_REG
    INTERFACE_SP,        // SP_BASE_REG
    INTERFACE_DPC,       // DPC_BASE_REG
    INTERFACE_DPS,       // DPS_BASE_REG // DP Span Registers
    INTERFACE_MI,        // MI_BASE_REG
    INTERFACE_VI,        // VI_BASE_REG
    INTERFACE_AI,        // AI_BASE_REG
    INTERFACE_PI,        // PI_BASE_REG
    INTERFACE_RI,        // RI_BASE_REG
    INTERFACE_SI,        // SI_BASE_REG

    INTERFACE_GIO,       // GIO_BASE_REG
    INTERFACE_RDB,       // RGB_BASE_REG
    INTERFACE_GIO_RDB,   // GIO_RDB_BASE_REG

    NUM_INTERFACES,
};

// Coprocessors.
enum PACKED RegisterSources {
    REGS_CPU,

    REGS_COPROCESSORS_START,
#define COPROCESSOR_TO_SRC(_cop) (REGS_COPROCESSORS_START + _cop)
    REGS_CP0 = COPROCESSOR_TO_SRC(COP0),
    REGS_CP1 = COPROCESSOR_TO_SRC(COP1),
    REGS_CP2 = COPROCESSOR_TO_SRC(COP2),
    REGS_CP3 = COPROCESSOR_TO_SRC(COP3),

    REGS_FCR,  // Coprocessor-1 Control/Status registers
    REGS_SPC,  // Special registers

    REGS_INTERFACES_START,
#define INTERFACE_TO_SRC(_interface) (REGS_INTERFACES_START + _interface)
    REGS_RDRAM   = INTERFACE_TO_SRC(INTERFACE_RDRAM  ),
    REGS_SP      = INTERFACE_TO_SRC(INTERFACE_SP     ),
    REGS_DPC     = INTERFACE_TO_SRC(INTERFACE_DPC    ),
    REGS_DPS     = INTERFACE_TO_SRC(INTERFACE_DPS    ),
    REGS_MI      = INTERFACE_TO_SRC(INTERFACE_MI     ),
    REGS_VI      = INTERFACE_TO_SRC(INTERFACE_VI     ),
    REGS_AI      = INTERFACE_TO_SRC(INTERFACE_AI     ),
    REGS_PI      = INTERFACE_TO_SRC(INTERFACE_PI     ),
    REGS_RI      = INTERFACE_TO_SRC(INTERFACE_RI     ),
    REGS_SI      = INTERFACE_TO_SRC(INTERFACE_SI     ),
    REGS_GIO     = INTERFACE_TO_SRC(INTERFACE_GIO    ),
    REGS_RDB     = INTERFACE_TO_SRC(INTERFACE_RDB    ),
    REGS_GIO_RDB = INTERFACE_TO_SRC(INTERFACE_GIO_RDB),
    REGS_INTERFACES_END = REGS_GIO_RDB,

    NUM_REG_SOURCES,
};


#include "registers/register_ids.h"
#include "registers/interface/register_ids_interfaces.h"


enum PACKED RegisterValueTypes {
    REG_VAL_TYPE_INT,
    REG_VAL_TYPE_FLOAT,
    REG_VAL_TYPE_ADDR,
    REG_VAL_TYPE_BITS, //! TODO: Is this needed?
};

typedef union RegisterId {
    struct {
        /*0x00*/ enum RegisterSources src;
        /*0x01*/ s8 idx;
        /*0x02*/ union {
                    struct {
                        enum RegisterValueTypes type;
                        struct PACKED {
                            u8     : 5;
                            u8 thr : 1; // Is on thread (unimplemented).
                            u8 dbl : 1; // [0=32-bit,1=64-bit] (unimplemented).
                            u8 out : 1; // [0=default,1=output] (don't print value on summary if output).
                        };
                    };
                    u16 raw;
                } valInfo;
    }; /*0x04*/
    /*0x04*/ u32 raw;
} RegisterId; /*0x04*/

//! TODO: Can this be shrunk? Ideally it fits into 8 bytes, but that may not be possible.
typedef struct RegisterInfo {
    /*0x00*/ const char* name;
    /*0x04*/ union {
                const Address addr; // Interface register address. //! TODO: Can this be changed to offset from base address to save 3 bytes? Potential issues: [RDRAM_ROW,SP_PC,SP_IBIST,GIO_SYNC,GIO_CART_INTERRUPT]
                struct {
                    struct PACKED {
                        const u8 is64bit : 1; // [0=32bit,1=64bit]. This is ignored for interface registers which are always 32 bit.
                        const u8 offset  : 7; // (byte offset in __OSThreadContext) / sizeof(u32)
                    };
                    const char shortName[3]; //! TODO: Can the null terminator be excluded here somehow to make room for other data?
                }; // Thread register.
            };
    /*0x08*/ const u8 descId; // : 5
    /*0x09*/ const u8 bitsId; // : 5 //! TODO:
    /*0x10*/ const u8 pad[2];
} RegisterInfo; /*0x0C*/

#define REGINFO_NULL_OFFSET BITMASK(7)//(u16)-1

#define DEF_SREG(_size, _name, _shortName, _descId) {   \
    .offset    = REGINFO_NULL_OFFSET,                   \
    .is64bit   = (_size == sizeof(u64)),                \
    .name      = _name,                                 \
    .shortName = _shortName,                            \
    .descId    = _descId,                               \
}

#define DEF_TREG(_field, _size, _name, _shortName, _descId) {   \
    .offset    = ((OFFSETOF(__OSThreadContext, _field)) / sizeof(u32)), \
    .is64bit   = (sizeof_member(__OSThreadContext, _field) == sizeof(u64)),      \
    .name      = _name,                                         \
    .shortName = _shortName,                                    \
    .descId    = _descId,                                       \
}

//! TODO: Use offset from base addr?
#define DEF_IREG(_reg, _name, _descId) {    \
    .name   = _name,                        \
    .addr   = _reg,                         \
    .descId = _descId,                      \
}

#define DEF_CPU_SREG(_reg, _name, _descId) DEF_SREG(      sizeof(u64), _name, _name, _descId)
#define DEF_CPU_TREG(_reg, _name, _descId) DEF_TREG(_reg, sizeof(u64), _name, _name, _descId)

#define DEF_CP0_SREG(_reg, _type,         _name, _shortName, _descId) DEF_SREG(        sizeof(_type), _name, _shortName, _descId)
#define DEF_CP0_TREG(_reg, _type, _field, _name, _shortName, _descId) DEF_TREG(_field, sizeof(_type), _name, _shortName, _descId)

#define DEF_CP1_SREG(_reg, _name, _descId)      DEF_SREG(                   sizeof(f32), "F"_name, _name, _descId)
#define DEF_CP1_TREG_EVEN(_reg, _name, _descId) DEF_TREG(fp##_reg.f.f_even, sizeof(f32), "F"_name, _name, _descId)
#define DEF_CP1_TREG_ODD(_reg, _name, _descId)  DEF_TREG(fp##_reg.f.f_odd,  sizeof(f32), "F"_name, _name, _descId)

#define CASE_REG(_cop, _idx, _reg) case _idx: ASM_GET_REG_##_cop(val, STR_REG_PREFIX EXPAND_AND_STRINGIFY(_reg)); break;

#define DEF_IREG_END() DEF_IREG(0, NULL, 0)


typedef struct RegisterSource {
    /*0x00*/ const char* name;
    /*0x04*/ const char* desc;
    /*0x08*/ union {
                Doubleword (*valFunc)(int idx); // If NULL, get by index and use 'addr'.
            };
    /*0x0C*/ union {
                const RegisterInfo* (*infoFunc)(int idx); // If NULL, get by index.
                const RegisterInfo* infoList;
            };
    /*0x10*/ const char** descList;
    /*0x14*/ 
    /*0x14*/ u8 numRegs;
    /*0x15*/ _Bool hasInfoFunc;
    /*0x16*/ u8 pad[2];
} RegisterSource; /*0x20*/

#define DEF_REG_LIST_PROCESSOR_FUNC(_name, _desc, _valFunc, _descList, _infoList, _infoFunc) { \
    .name        = _name,       \
    .desc        = _desc,       \
    .valFunc     = _valFunc,    \
    .descList    = _descList,   \
    .hasInfoFunc = TRUE,        \
    .infoFunc    = _infoFunc,   \
    .numRegs     = ARRAY_COUNT(_infoList), \
}
#define DEF_REG_LIST_PROCESSOR(_name, _desc, _valFunc, _descList, _infoList) { \
    .name        = _name,       \
    .desc        = _desc,       \
    .valFunc     = _valFunc,    \
    .descList    = _descList,   \
    .hasInfoFunc = FALSE,       \
    .infoList    = _infoList,   \
    .numRegs     = ARRAY_COUNT(_infoList), \
}
#define DEF_REG_LIST_INTERFACE(_name, _desc, _descList, _infoList) { \
    .name        = _name,       \
    .desc        = _desc,       \
    .valFunc     = NULL,        \
    .descList    = _descList,   \
    .hasInfoFunc = FALSE,       \
    .infoList    = _infoList,   \
    .numRegs     = ARRAY_COUNT(_infoList), \
}


extern OSThread* __osRunningThread;


#define REG_BUFFER_SIZE 3
extern RegisterId gSavedRegBuf[REG_BUFFER_SIZE];
extern int gSavedRegBufSize;


const RegisterSource* get_reg_src(enum RegisterSources src);
ALWAYS_INLINE static const RegisterSource* get_coprocessor_src(enum Coprocessors copID) {
    return get_reg_src(COPROCESSOR_TO_SRC(copID));
}
ALWAYS_INLINE static const RegisterSource* get_interface_src(enum Interfaces interfaceID) {
    return get_reg_src(INTERFACE_TO_SRC(interfaceID));
}
const RegisterInfo* get_reg_info_from_src(const RegisterSource* regSrc, int idx);
const RegisterInfo* get_reg_info(enum RegisterSources src, int idx);
Doubleword get_thread_reg_val(enum RegisterSources src, int idx, OSThread* thread);
Doubleword get_direct_reg_val(enum RegisterSources src, int idx);
Doubleword get_reg_val(enum RegisterSources src, int idx);
const char* get_reg_desc(enum RegisterSources src, int idx);

void clear_saved_reg_buffer(void);
void append_reg_to_buffer(enum RegisterSources src, int idx, enum RegisterValueTypes type, _Bool isOutput);
