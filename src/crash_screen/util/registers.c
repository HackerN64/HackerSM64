#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

#include "PR/rdb.h"

#include "crash_screen/cs_main.h"
#include "map_parser.h"
#include "memory_read.h"

#include "registers.h"


// https://n64.readthedocs.io/
// https://hack64.net/docs/VR43XX.pdf


const char* sRegDesc_Default[] = {
    [REG_DESC_ZERO  ] = "zero",
    [REG_DESC_AT    ] = "assembler temporary value",
    [REG_DESC_SUBRET] = "subroutine return value",
    [REG_DESC_SUBARG] = "subroutine argument",
    [REG_DESC_TEMP  ] = "temporary value",
    [REG_DESC_SAVED ] = "saved value",
    [REG_DESC_KERNEL] = "kernel value",
    [REG_DESC_GP    ] = "global pointer",
    [REG_DESC_SP    ] = "stack pointer",
    [REG_DESC_FP    ] = "saved value or frame pointer", //! TODO: determine which one this register is.
    [REG_DESC_RA    ] = "return address",
};

#include "register_data/CPU.inc.c"
#include "register_data/CP0.inc.c"
#include "register_data/CP1.inc.c"
#include "register_data/CP2.inc.c"
#include "register_data/CP3.inc.c"
#include "register_data/SPECIAL.inc.c"

#include "register_data/interface/RDRAM.inc.c"
#include "register_data/interface/SP.inc.c"
#include "register_data/interface/DPC.inc.c"
#include "register_data/interface/DPS.inc.c"
#include "register_data/interface/MI.inc.c"
#include "register_data/interface/VI.inc.c"
#include "register_data/interface/AI.inc.c"
#include "register_data/interface/PI.inc.c"
#include "register_data/interface/RI.inc.c"
#include "register_data/interface/SI.inc.c"
// GIO
// RDB
#include "register_data/interface/GIO_RDB.inc.c"




// -- COPROCESSORS --

static const RegisterSource* sRegisterLists[NUM_REG_SOURCES] = {
    // Processors:
    [REGS_CPU    ] = &sRegisters_CPU,
    [REGS_CP0    ] = &sRegisters_CP0,
    [REGS_CP1    ] = &sRegisters_CP1,
    [REGS_CP2    ] = &sRegisters_CP2,
    [REGS_CP3    ] = &sRegisters_CP3,
    [REGS_FCR    ] = &sRegisters_FCR,
    [REGS_SPC    ] = &sRegisters_SPECIAL,
    // Interfaces:
    [REGS_RDRAM  ] = &sRegisters_RDRAM,
    [REGS_SP     ] = &sRegisters_SP,
    [REGS_DPC    ] = &sRegisters_DPC,
    [REGS_DPS    ] = &sRegisters_DPS,
    [REGS_MI     ] = &sRegisters_MI,
    [REGS_VI     ] = &sRegisters_VI,
    [REGS_AI     ] = &sRegisters_AI,
    [REGS_PI     ] = &sRegisters_PI,
    [REGS_RI     ] = &sRegisters_RI,
    [REGS_SI     ] = &sRegisters_SI,
    // Debug:
    [REGS_GIO    ] = &sRegisters_GIO,
    [REGS_RDB    ] = &sRegisters_RDB,
    [REGS_GIO_RDB] = &sRegisters_GIO_RDB,
};


const RegisterSource* get_reg_src(enum RegisterSources src) {
    return sRegisterLists[src];
}

const RegisterInfo* get_reg_info_from_src(const RegisterSource* regSrc, int idx) {
    return (regSrc->hasInfoFunc) ? regSrc->infoFunc(idx) : &regSrc->infoList[idx];
}

const RegisterInfo* get_reg_info(enum RegisterSources src, int idx) {
    const RegisterSource* regSrc = sRegisterLists[src];
    return get_reg_info_from_src(regSrc, idx);
}

Doubleword get_thread_reg_val(enum RegisterSources src, int idx, OSThread* thread) {
    const RegisterInfo* info = get_reg_info(src, idx);

    if (info == NULL) {
        return 0;
    }

    __OSThreadContext* tc = &thread->context;
    Address addr = ((Address)tc + (info->offset * sizeof(u32)));

    if (info->is64bit) {
        Doubleword data = 0;
        if (try_read_doubleword_aligned(&data, addr)) {
            return data;
        }
    } else {
        Word data = 0;
        if (try_read_word_aligned(&data, addr)) {
            return data;
        }
    }

    return 0;
}

Doubleword get_direct_reg_val(enum RegisterSources src, int idx) {
    const RegisterSource* regSrc = sRegisterLists[src];
    return (regSrc->valFunc != NULL) ? regSrc->valFunc(idx) : 0;
}

Doubleword get_reg_val(enum RegisterSources src, int idx, _Bool checkThread) {
    const RegisterInfo* regInfo = get_reg_info(src, idx);

    if (regInfo == NULL) {
        return 0;
    }

    if (src >= REGS_INTERFACES_START) {
        Word data = 0x00000000;
        if (try_read_word_aligned(&data, regInfo->addr)) {
            return data;
        }
    } else if (checkThread && (regInfo->offset != REGINFO_NULL_OFFSET)) {
        // If register exists in __OSThreadContext, use the data from the inspected thread.
        return get_thread_reg_val(src, idx, gInspectThread);
    } else {
        return get_direct_reg_val(src, idx);
    }

    UNREACHABLE();
}

const char* get_reg_desc(enum RegisterSources src, int idx) {
    const RegisterSource* regSrc = sRegisterLists[src];
    const RegisterInfo* info = get_reg_info_from_src(regSrc, idx);

    if (info == NULL) {
        return 0;
    }

    return regSrc->descList[info->descId];
}

// A buffer to save registers to. Used by cs_insn_to_string().
RegisterId gSavedRegBuf[REG_BUFFER_SIZE];
int gSavedRegBufSize = 0;

void clear_saved_reg_buffer(void) {
    bzero(gSavedRegBuf, sizeof(gSavedRegBuf));
    gSavedRegBufSize = 0;
}

void append_reg_to_buffer(enum RegisterSources src, int idx, enum RegisterValueTypes type, _Bool isOutput) {
    if (gSavedRegBufSize < ARRAY_COUNT(gSavedRegBuf)) {
        gSavedRegBuf[gSavedRegBufSize++] = (RegisterId){
            .src = src,
            .idx = idx,
            .valInfo = {
                .type = type,
                .thr  = TRUE,
                .dbl  = FALSE, //! TODO: implement this.
                .out  = isOutput,
            },
        };
    }
}
