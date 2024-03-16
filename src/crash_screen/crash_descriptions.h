#pragma once

#include <ultra64.h>

#include "types.h"

#include "game/main.h"

#include "game/emutest.h"



enum CauseDescriptions {
    CAUSE_DESC_INT     = (EXC_INT     >> CAUSE_EXCSHIFT),
    CAUSE_DESC_MOD     = (EXC_MOD     >> CAUSE_EXCSHIFT),
    CAUSE_DESC_RMISS   = (EXC_RMISS   >> CAUSE_EXCSHIFT),
    CAUSE_DESC_WMISS   = (EXC_WMISS   >> CAUSE_EXCSHIFT),
    CAUSE_DESC_RADE    = (EXC_RADE    >> CAUSE_EXCSHIFT),
    CAUSE_DESC_WADE    = (EXC_WADE    >> CAUSE_EXCSHIFT),
    CAUSE_DESC_IBE     = (EXC_IBE     >> CAUSE_EXCSHIFT),
    CAUSE_DESC_DBE     = (EXC_DBE     >> CAUSE_EXCSHIFT),
    CAUSE_DESC_SYSCALL = (EXC_SYSCALL >> CAUSE_EXCSHIFT),
    CAUSE_DESC_BREAK   = (EXC_BREAK   >> CAUSE_EXCSHIFT),
    CAUSE_DESC_II      = (EXC_II      >> CAUSE_EXCSHIFT),
    CAUSE_DESC_CPU     = (EXC_CPU     >> CAUSE_EXCSHIFT),
    CAUSE_DESC_OV      = (EXC_OV      >> CAUSE_EXCSHIFT),
    CAUSE_DESC_TRAP    = (EXC_TRAP    >> CAUSE_EXCSHIFT),
    CAUSE_DESC_VCEI    = (EXC_VCEI    >> CAUSE_EXCSHIFT),
    CAUSE_DESC_FPE     = (EXC_FPE     >> CAUSE_EXCSHIFT),
    CAUSE_DESC_WATCH   = 16, // (EXC_WATCH   >> CAUSE_EXCSHIFT),
    CAUSE_DESC_VCED    = 17, // (EXC_VCED    >> CAUSE_EXCSHIFT),
    NUM_CAUSE_DESC,
};

#define FPCSR_SHIFT 17
#define FPCSR_CAUSES (6 - 1)
#define FPCSR_SHIFT_2 (FPCSR_SHIFT - FPCSR_CAUSES)

enum FPCSRDescriptions {
    FPCSR_DESC_CE = (FPCSR_CAUSES - __builtin_ctz(FPCSR_CE >> FPCSR_SHIFT_2)), // Unimplemented operation.
    FPCSR_DESC_CV = (FPCSR_CAUSES - __builtin_ctz(FPCSR_CV >> FPCSR_SHIFT_2)), // Invalid operation.
    FPCSR_DESC_CZ = (FPCSR_CAUSES - __builtin_ctz(FPCSR_CZ >> FPCSR_SHIFT_2)), // Division by zero.
    FPCSR_DESC_CO = (FPCSR_CAUSES - __builtin_ctz(FPCSR_CO >> FPCSR_SHIFT_2)), // Overflow.
    FPCSR_DESC_CU = (FPCSR_CAUSES - __builtin_ctz(FPCSR_CU >> FPCSR_SHIFT_2)), // Underflow.
    FPCSR_DESC_CI = (FPCSR_CAUSES - __builtin_ctz(FPCSR_CI >> FPCSR_SHIFT_2)), // Inexact operation.
    NUM_FPCSR_DESC,
};


typedef struct ThreadName {
    /*0x00*/ union {
                /*0x00*/ OSId id;
                /*0x00*/ OSPri pri;
            };
    /*0x04*/ const char* name;
} ThreadName; /*0x08*/


typedef struct EmulatorName {
    /*0x00*/ const enum Emulator bits;
    /*0x04*/ const char* name;
} EmulatorName; /*0x08*/


extern char HackerSM64_version_txt[];
extern const char crash_screen_version[];

extern const char* gRegionName;
extern const char* osTvTypeStrings[];
extern const char* gUcodeName;
extern const char* gSaveTypeName;
extern const char* gCompressionName;


const char* get_thread_name(OSThread* thread);
const char* get_thread_state_str(OSThread* thread);
const char* get_thread_flags_str(OSThread* thread);

const char* get_cause_desc(__OSThreadContext* tc);
const char* get_fpcsr_desc(u32 fpcsr, _Bool checkSpecial);
const char* get_emulator_name(enum Emulator emu);
