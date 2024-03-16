#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/crash_main.h"
#include "util/map_parser.h"
#include "util/memory_read.h"
#include "util/registers.h"

#include "crash_descriptions.h"

#include "game/emutest.h"
#include "game/version.h"


// Include the version number from VERSION.txt. Includes a newline at the end.
INCBIN(char, HackerSM64_version_txt, "VERSION.txt", 4);

// Crash screen version. Includes a newline at the end.
const char crash_screen_version[] = "v2.0\n"; //! TODO: Change this on release.


// -- STRINGS --

// Region string:
#define DEF_REGION_NAME(_name) const char* gRegionName = TO_STRING2(_name);
#ifdef VERSION_JP
DEF_REGION_NAME(jp);
#elif VERSION_US
DEF_REGION_NAME(us);
#elif VERSION_EU
DEF_REGION_NAME(eu);
#elif VERSION_SH
DEF_REGION_NAME(sh);
#elif BBPLAYER
DEF_REGION_NAME(bb);
#else
DEF_REGION_NAME(xx);
#endif

// osTvType strings:
const char* osTvTypeStrings[] = {
    [OS_TV_PAL ] = "pal",
    [OS_TV_NTSC] = "ntsc",
    [OS_TV_MPAL] = "mpal",
};

// Microcode string:
#define DEF_UCODE_NAME(_name) const char* gUcodeName = TO_STRING2(_name);
#ifdef L3DEX2_ALONE
DEF_UCODE_NAME(L3DEX2_alone);
#elif F3DZEX_GBI_2
DEF_UCODE_NAME(f3dzex2_PosLight);
#elif F3DZEX_NON_GBI_2
DEF_UCODE_NAME(f3dzex2_Non_PosLight);
#elif F3DEX2PL_GBI
DEF_UCODE_NAME(F3DEX2_PosLight);
#elif F3DEX_GBI_2
DEF_UCODE_NAME(F3DEX2);
#elif F3DEX_GBI
DEF_UCODE_NAME(F3DEX);
#elif SUPER3D_GBI
DEF_UCODE_NAME(Super3D);
#else
DEF_UCODE_NAME(Fast3D);
#endif

// Save type string:
#define DEF_SAVETYPE_NAME(_name) const char* gSaveTypeName = TO_STRING2(_name);
#ifdef EEP4K
DEF_SAVETYPE_NAME(eep4k); // 4kbit
#elif EEP16K
DEF_SAVETYPE_NAME(eep16k); // 16kbit
#elif SRAM
DEF_SAVETYPE_NAME(sram); // 256kbit
#else
DEF_SAVETYPE_NAME(unknown);
#endif

// Compression type string:
#define DEF_COMPRESSION_NAME(_name) const char* gCompressionName = TO_STRING2(_name);
#ifdef GZIP
DEF_COMPRESSION_NAME(gzip);
#elif RNC1
DEF_COMPRESSION_NAME(rnc1);
#elif RNC2
DEF_COMPRESSION_NAME(rnc2);
#elif YAYO
DEF_COMPRESSION_NAME(yay0);
#elif MIO0
DEF_COMPRESSION_NAME(mio0);
#elif UNCOMPRESSED
DEF_COMPRESSION_NAME(none);
#else
DEF_COMPRESSION_NAME(unk);
#endif


// -- THREAD --

static const ThreadName sThreadIDNames[] = {
    { .id = THREAD_0_MANAGER,           .name = "manager",        }, // Uses sThreadPriNames.
    { .id = THREAD_1_IDLE,              .name = "idle",           },
    { .id = THREAD_2,                   .name = "unused",         },
    { .id = THREAD_3_MAIN,              .name = "main",           },
    { .id = THREAD_4_SOUND,             .name = "sound",          }, //! TODO: libultra scheduler also uses ID 4.
    { .id = THREAD_5_GAME_LOOP,         .name = "game loop",      },
    { .id = THREAD_6_RUMBLE,            .name = "rumble",         },
    { .id = THREAD_7_HVQM,              .name = "HVQM",           },
    { .id = THREAD_8_TIMEKEEPER,        .name = "timekeeper",     },
    { .id = THREAD_9_DA_COUNTER,        .name = "DA counter",     },
    { .id = THREAD_13_FAULT,            .name = "UNF Fault",      },
    { .id = THREAD_14_USB,              .name = "UNF USB",        },
    { .id = THREAD_1000_CRASH_SCREEN_0, .name = "Crash Screen 0", },
    { .id = THREAD_1001_CRASH_SCREEN_1, .name = "Crash Screen 1", },
    { .id = THREAD_1002_CRASH_SCREEN_2, .name = "Crash Screen 2", },
};
static const ThreadName sThreadPriNames[] = {
    { .pri = OS_PRIORITY_SIMGR,    .name = "SI manager", },
    { .pri = OS_PRIORITY_PIMGR,    .name = "PI manager", },
    { .pri = OS_PRIORITY_VIMGR,    .name = "VI manager", },
    { .pri = OS_PRIORITY_RMON,     .name = "rmon",       },
    { .pri = OS_PRIORITY_RMONSPIN, .name = "rmonspin",   },
};
static const char* get_thread_name_from_list(int id, const ThreadName* list, size_t listSize) {
    const ThreadName* threadName = &list[0];
    for (size_t i = 0; i < listSize; i++) {
        if (threadName->id == id) {
            return threadName->name;
        }
        threadName++;
    }

    return NULL;
}

// Returns a thread name from 'sThreadIDNames'.
const char* get_thread_name(OSThread* thread) {
    OSId id = osGetThreadId(thread);
    const char* name = NULL;

    // Libultra threads on thread ID 0:
    if (id == THREAD_0_MANAGER) {
        name = get_thread_name_from_list(osGetThreadPri(thread), sThreadPriNames, ARRAY_COUNT(sThreadPriNames));
        if (name != NULL) return name;
    }

    name = get_thread_name_from_list(id, sThreadIDNames, ARRAY_COUNT(sThreadIDNames));
    if (name != NULL) return name;

    return NULL;
}


// -- CAUSE --

static const char* sCauseDesc[NUM_CAUSE_DESC] = {
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
// Returns a CAUSE description from 'sCauseDesc'.
const char* get_cause_desc(__OSThreadContext* tc) {
    uint64_t badvaddr = tc->badvaddr;
    uint32_t epc = GET_EPC(tc);
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
        case EXC_WATCH: cause = EXC_CODE(CAUSE_DESC_WATCH); break; // 23 -> 16
        case EXC_VCED:  cause = EXC_CODE(CAUSE_DESC_VCED ); break; // 31 -> 17
    }

    cause >>= CAUSE_EXCSHIFT;

    if (cause < ARRAY_COUNT(sCauseDesc)) {
        return sCauseDesc[cause];
    }

    return NULL;
}


// -- FPCSR/FPE --

static const char* sFpcsrDesc[NUM_FPCSR_DESC] = {
    [FPCSR_DESC_CE] = "Unimplemented operation",
    [FPCSR_DESC_CV] = "Invalid operation",
    [FPCSR_DESC_CZ] = "Division by zero",
    [FPCSR_DESC_CO] = "Overflow",
    [FPCSR_DESC_CU] = "Underflow",
    [FPCSR_DESC_CI] = "Inexact operation",
};
static const char* sFltErrDesc[NUM_FLT_ERR] = {
    [FLT_ERR_NONE  ] = "",
    [FLT_ERR_DENORM] = "Denormalized float",
    [FLT_ERR_NAN   ] = "NaN float",
};

//! TODO: NaN floats aren't detected here even though validate_float does, and this works with denorms.
enum FloatErrorType validate_floats_in_reg_buffer(void) {
    enum FloatErrorType fltErrType = FLT_ERR_NONE;

    for (int i = 0; i < gSavedRegBufSize; i++) {
        RegisterId reg = gSavedRegBuf[i];

        if (reg.flt) {
            IEEE754_f32 val = {
                .asU32 = get_reg_val(reg.cop, reg.idx)
            };
            fltErrType = validate_float(val);

            if (fltErrType != FLT_ERR_NONE) {
                break;
            }
        }
    }

    return fltErrType;
}

// Returns a FPCSR description from 'sFpcsrDesc'.
// Only use 'checkSpecial' if disasm has just been run.
const char* get_fpcsr_desc(u32 fpcsr, _Bool checkSpecial) {
    u32 bit = BIT(FPCSR_SHIFT);

    for (u32 i = 0; i < NUM_FPCSR_DESC; i++) {
        if (fpcsr & bit) {
            if (checkSpecial && (i == FPCSR_DESC_CE)) {
                enum FloatErrorType fltErrType = validate_floats_in_reg_buffer();

                if (fltErrType != FLT_ERR_NONE) {
                    return sFltErrDesc[fltErrType];
                }
            }

            return sFpcsrDesc[i];
        }

        bit >>= 1;
    }

    return NULL;
}

// -- EMULATOR --

#define EMULATOR_STRING(_bits, _name)  { .bits = _bits, .name = _name, }
static const EmulatorName sEmulatorStrings[] = {
    { .bits = EMU_WIIVC,            .name = "Wii VC",           },
    { .bits = EMU_PROJECT64_1_OR_2, .name = "pj64 1 or 2",      },
    { .bits = EMU_PROJECT64_3,      .name = "pj64 3",           },
    { .bits = EMU_PROJECT64_4,      .name = "pj64 4",           },
    { .bits = EMU_MUPEN_OLD,        .name = "mupen (old)",      },
    { .bits = EMU_MUPEN64PLUS_NEXT, .name = "mupen64plus-next", },
    { .bits = EMU_CEN64,            .name = "cen64",            },
    { .bits = EMU_SIMPLE64,         .name = "simple64",         },
    { .bits = EMU_PARALLELN64,      .name = "ParaLLEl N64",     },
    { .bits = EMU_ARES,             .name = "ares",             },
    { .bits = EMU_CONSOLE,          .name = "CONSOLE",          },
};
const char* get_emulator_name(enum Emulator emu) {
    for (int i = 0; i < ARRAY_COUNT(sEmulatorStrings); i++) {
        if (emu == sEmulatorStrings[i].bits) {
            return sEmulatorStrings[i].name;
        }
    }

    return NULL;
}
