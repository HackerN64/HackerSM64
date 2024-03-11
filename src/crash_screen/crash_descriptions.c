#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/crash_main.h"
#include "util/map_parser.h"
#include "util/memory_read.h"
#include "util/registers.h"

#include "crash_descriptions.h"


static const ThreadIDName sThreadIDNames[] = {
    { .threadID = THREAD_0,                   .name = "0",              },
    { .threadID = THREAD_1_IDLE,              .name = "idle",           },
    { .threadID = THREAD_2,                   .name = "2",              },
    { .threadID = THREAD_3_MAIN,              .name = "main",           },
    { .threadID = THREAD_4_SOUND,             .name = "sound",          },
    { .threadID = THREAD_5_GAME_LOOP,         .name = "game loop",      },
    { .threadID = THREAD_6_RUMBLE,            .name = "rumble",         },
    { .threadID = THREAD_7_HVQM,              .name = "HVQM",           },
    { .threadID = THREAD_8_TIMEKEEPER,        .name = "timekeeper",     },
    { .threadID = THREAD_9_DA_COUNTER,        .name = "DA counter",     },
    { .threadID = THREAD_1000_CRASH_SCREEN_0, .name = "Crash Screen 0", },
    { .threadID = THREAD_1001_CRASH_SCREEN_1, .name = "Crash Screen 1", },
    { .threadID = THREAD_1002_CRASH_SCREEN_2, .name = "Crash Screen 2", },
};

static const char* sCauseDesc[18] = {
    [CAUSE_DESC_INT    ] = "Interrupt",
    [CAUSE_DESC_MOD    ] = "TLB modification",
    [CAUSE_DESC_RMISS  ] = "TLB exception on load or inst.",
    [CAUSE_DESC_WMISS  ] = "TLB exception on store",
    [CAUSE_DESC_RADE   ] = "Address error on load or inst.",
    [CAUSE_DESC_WADE   ] = "Address error on store",
    [CAUSE_DESC_IBE    ] = "Bus error on inst.",
    [CAUSE_DESC_DBE    ] = "Bus error on data",
    [CAUSE_DESC_SYSCALL] = "Failed Assert: See below",
    [CAUSE_DESC_BREAK  ] = "Breakpoint exception",
    [CAUSE_DESC_II     ] = "Reserved instruction",
    [CAUSE_DESC_CPU    ] = "Coprocessor unusable",
    [CAUSE_DESC_OV     ] = "Arithmetic overflow",
    [CAUSE_DESC_TRAP   ] = "Trap exception",
    [CAUSE_DESC_VCEI   ] = "Virtual coherency on inst.",
    [CAUSE_DESC_FPE    ] = "Floating point exception",
    [CAUSE_DESC_WATCH  ] = "Watchpoint exception",
    [CAUSE_DESC_VCED   ] = "Virtual coherency on data",
};

static const char* sFpcsrDesc[6] = {
    [FPCSR_DESC_CE] = "Unimplemented operation",
    [FPCSR_DESC_CV] = "Invalid operation",
    [FPCSR_DESC_CZ] = "Division by zero",
    [FPCSR_DESC_CO] = "Overflow",
    [FPCSR_DESC_CU] = "Underflow",
    [FPCSR_DESC_CI] = "Inexact operation",
};


// Returns a CAUSE description from 'sCauseDesc'.
const char* get_cause_desc(__OSThreadContext* tc) {
    uint64_t badvaddr = tc->badvaddr;
    uint32_t epc = tc->pc + ((tc->cause & 0x80000000) ? 4 : 0); //! TODO: Is this correct?
    u32 cause = (tc->cause & CAUSE_EXCMASK);

    switch (cause) {
        // Heuristics from libdragon.
        case EXC_RMISS:
            if (epc == (u32)badvaddr) {
                return "Invalid program counter address";
            } else if (badvaddr < 128) {
                // This is probably a NULL pointer dereference, though it can go through a structure or an array,
                // so leave some margin to the actual faulting address.
                return "NULL pointer dereference (read)";
            } else {
                return "Read from invalid memory address";
            }
            break;
        case EXC_WMISS:
            if (badvaddr < 128) {
                return "NULL pointer dereference (write)";
            } else {
                return "Write to invalid memory address";
            }
        case EXC_MOD:
            return "Write to read-only memory";
        case EXC_RADE:
            if (epc == (uint32_t)badvaddr) {
                if (is_unmapped_kx64(badvaddr)) {
                    return "Program counter in invalid 64-bit address";
                } else {
                    return "Misaligned program counter address";
                }
            } else {
                if (is_unmapped_kx64(badvaddr)) {
                    return "Read from invalid 64-bit address";
                } else {
                    return "Misaligned read from memory";
                }
            }
            break;
        case EXC_WADE:
            return "Misaligned write to memory";

        // Make the last two "cause" case indexes sequential for array access.
        case EXC_WATCH: cause = EXC_CODE(16); break; // 23 -> 16
        case EXC_VCED:  cause = EXC_CODE(17); break; // 31 -> 17
    }

    cause >>= CAUSE_EXCSHIFT;

    if (cause < ARRAY_COUNT(sCauseDesc)) {
        return sCauseDesc[cause];
    }

    return NULL;
}

_Bool check_for_denorms(void) {
    for (int j = 0; j < gSavedRegBufSize; j++) {
        RegisterId reg = gSavedRegBuf[j];

        if (reg.cop == COP1) {
            IEEE754_f32 val = {
                .asU32 = get_reg_val(reg.cop, reg.idx)
            };

            if (validate_float(val) == FLT_ERR_DENORM) {
                return TRUE;
            }
        }
    }

    return FALSE;
}

// Returns a FPCSR description from 'sFpcsrDesc'.
const char* get_fpcsr_desc(u32 fpcsr) {
    u32 bit = BIT(FPCSR_SHIFT);

    for (u32 i = 0; i < ARRAY_COUNT(sFpcsrDesc); i++) {
        if (fpcsr & bit) {
            if ((i == FPCSR_DESC_CE) && check_for_denorms()) {
                return "Denormalized float";
            }

            return sFpcsrDesc[i];
        }

        bit >>= 1;
    }

    return NULL;
}

// Returns a thread name from 'sThreadIDNames'.
const char* get_thread_name_from_id(enum ThreadID threadID) {
    const ThreadIDName* threadIDName = &sThreadIDNames[0];

    for (int i = 0; i < ARRAY_COUNT(sThreadIDNames); i++) {
        if (threadIDName->threadID == threadID) {
            return threadIDName->name;
        }

        threadIDName++;
    }

    return NULL;
}
