#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/cs_main.h"
#include "util/insn_disasm.h"
#include "util/map_parser.h"
#include "util/memory_read.h"
#include "util/registers.h"

#include "cs_descriptions.h"

#include "game/emutest.h"
#include "game/version.h"


// Include the version number from VERSION.txt. Includes a newline at the end.
INCBIN(char, HackerSM64_version_txt, "VERSION.txt", 4);

// Crash screen version. Includes a newline at the end.
INCBIN(char, CrashScreen_version_txt, "src/crash_screen/VERSION.txt", 4);


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
#elif VERSION_CN
DEF_REGION_NAME(cn);
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

typedef struct ThreadName {
    /*0x00*/ union {
                /*0x00*/ OSId id;
                /*0x00*/ OSPri pri;
            };
    /*0x04*/ const char* name;
} ThreadName; /*0x08*/
static const ThreadName sThreadIDNames[] = {
    { .id = THREAD_0,                   .name = "libultra?",        }, // Uses sThreadPriNames.
    { .id = THREAD_1_IDLE,              .name = "idle",             },
    { .id = THREAD_2,                   .name = "unused",           },
    { .id = THREAD_3_MAIN,              .name = "main",             },
    { .id = THREAD_4_SOUND,             .name = "sound",            }, //! TODO: libultra scheduler also uses ID 4 and has no set priority. Is there a way to differentiate the two?
    { .id = THREAD_5_GAME_LOOP,         .name = "game loop",        },
    { .id = THREAD_6_RUMBLE,            .name = "rumble",           },
    { .id = THREAD_7_HVQM,              .name = "hvqm",             },
    { .id = THREAD_8_TIMEKEEPER,        .name = "timekeeper",       },
    { .id = THREAD_9_DA_COUNTER,        .name = "da counter",       },
    { .id = THREAD_13_FAULT,            .name = "unf fault",        },
    { .id = THREAD_14_USB,              .name = "unf usb",          },
    { .id = THREAD_1000_CRASH_SCREEN_0, .name = "crash screen 0",   },
    { .id = THREAD_1001_CRASH_SCREEN_1, .name = "crash screen 1",   },
    { .id = THREAD_1002_CRASH_SCREEN_2, .name = "crash screen 2",   },
};
static const ThreadName sThreadPriNames[] = {
    { .pri = OS_PRIORITY_SIMGR,         .name = "si manager", },
    { .pri = 149,                       .name = "debug/usb?", }, //! TODO: Find out what this thread is. It only exists when running UNF.
    { .pri = OS_PRIORITY_PIMGR,         .name = "pi manager", },
    { .pri = OS_PRIORITY_VIMGR,         .name = "vi manager", },
    { .pri = OS_PRIORITY_RMON,          .name = "rmon",       },
    { .pri = OS_PRIORITY_RMONSPIN,      .name = "rmonspin",   },
};
static const char* get_thread_name_from_list(int id, const ThreadName* list, size_t listSize) {
    const ThreadName* threadName = &list[0];
    for (size_t i = 0; i < listSize; i++) {
        if (id == threadName->id) {
            return threadName->name;
        }
        threadName++;
    }

    return NULL;
}
// Returns a thread name from 'sThreadIDNames'.
const char* get_thread_name(OSThread* thread) {
    if (thread == NULL) {
        return NULL;
    }
    OSId id = osGetThreadId(thread);
    OSPri pri = osGetThreadPri(thread);
    const char* name = NULL;

    // Determine libultra threads on thread ID 0 by priority instead of ID:
    if ((id == THREAD_0) && (pri > OS_PRIORITY_APPMAX)) {
        name = get_thread_name_from_list(pri, sThreadPriNames, ARRAY_COUNT(sThreadPriNames));
        if (name != NULL) {
            return name;
        }
    }

    name = get_thread_name_from_list(id, sThreadIDNames, ARRAY_COUNT(sThreadIDNames));
    if (name != NULL) {
        return name;
    }

#ifdef INCLUDE_DEBUG_MAP
    if (name == NULL) {
        const MapSymbol* symbol = get_map_symbol((Address)thread, SYMBOL_SEARCH_BACKWARD);
        if (symbol != NULL) {
            return get_map_symbol_name(symbol);
        }
    }
#endif // INCLUDE_DEBUG_MAP

    return NULL;
}

static const char* sThreadStateStrings[] = {
    [__builtin_ctz(OS_STATE_STOPPED )] = "stopped",
    [__builtin_ctz(OS_STATE_RUNNABLE)] = "runnable",
    [__builtin_ctz(OS_STATE_RUNNING )] = "running",
    [__builtin_ctz(OS_STATE_WAITING )] = "waiting",
};
const char* get_thread_state_str(OSThread* thread) {
    u16 state = thread->state;
    if (state == 0x0000) return NULL;
    return sThreadStateStrings[__builtin_ctz(state)];
}

static const char* sThreadFlagStrings[] = {
    [__builtin_ctz(OS_FLAG_CPU_BREAK)] = "break", // CPU break
    [__builtin_ctz(OS_FLAG_FAULT    )] = "fault",
};
const char* get_thread_flags_str(OSThread* thread) {
    u16 flags = thread->flags;
    if (flags == 0x0000) return NULL;
    return sThreadFlagStrings[__builtin_ctz(flags)];
}


// -- PROCESSOR --

typedef struct PRId_name {
    /*0x00*/ const u8 Imp;
    /*0x01*/ const char name[7];
} PRId_name; /*0x08*/
const PRId_name sPRId_names[] = {
    { .Imp = 0x0B, .name = "vr4300", },
};
const char* get_processor_name(u8 imp) {
    for (int i = 0; i < ARRAY_COUNT(sPRId_names); i++) {
        if (imp == sPRId_names[i].Imp) {
            return sPRId_names[i].name;
        }
    }

    return "unknown";
}


// -- COPROCESSOR --

static const char* sCOPNames[] = {
    [CPU  + 1] = "CPU",
    [COP0 + 1] = "CP0 (system control)",
    [COP1 + 1] = "CP1 (FPU)",
    [COP2 + 1] = "CP2 (RCP vector unit)",
    [COP3 + 1] = "CP3",
    [FCR  + 1] = "CP1 (FCR)",
    [SPC  + 1] = "special registers",
};
const char* get_coprocessor_name(enum Coprocessors cop) {
    return sCOPNames[cop + 1];
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
    [CAUSE_DESC_SYSCALL] = "Syscall exception",
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
const char* get_cause_desc(__OSThreadContext* tc, _Bool specific) {
    u32 cause = (tc->cause & CAUSE_EXCMASK);

    if (specific) {
        uint64_t badvaddr = tc->badvaddr;
        uint32_t epc = GET_EPC(tc);

        // Heuristics from libdragon (plus a few extras):
        switch (cause) {
            case EXC_INT: // Non-crash interrupts (can be shown after changing the inspected thread).
                //! TODO: Can the instruction location here potentially change?
                if (tc->pc == ADDR_INSN_WAITING_FOR_MESG) {
                    return "Waiting for mesg";
                }
                //! TODO: Fix these unsafe data reads and clean this up:
                InsnData insn = { .raw = *(u32*)tc->pc };
                InsnData prev = { .raw = *(u32*)(tc->pc - sizeof(Word)) };
                #define INSN_IS_B_0(_insn) (((_insn).opcode == OPC_BEQ) && ((_insn).rs == (_insn).rt) && ((_insn).offset == (u16)-1))
                if (INSN_IS_B_0(insn) || (((Reg_CP0_Cause)tc->cause).BD && (insn.raw == 0x00000000) && INSN_IS_B_0(prev))) {
                    return "Empty infinite loop";
                }
                #undef INSN_IS_B_0
                break;
            case EXC_MOD:
                return "Write to read-only memory";
            case EXC_RMISS:
                if (epc == (u32)badvaddr) {
                    return "Invalid program counter address";
                } else if (badvaddr < 128) {
                    // This is probably a NULL pointer dereference, though it can go through a structure or an array,
                    // so leave some margin to the actual faulting address.
                    if (tc->pc == INSN_OFFSET_FROM_ADDR(strlen, 0)) { // 1st instruction of strlen
                        return "NULL string dereference (read)";
                    } else {
                        return "NULL pointer dereference (read)";
                    }
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
            case EXC_SYSCALL:
                if (tc->pc == ADDR_INSN_ASSERT) {
                    return "Failed Assert (see below)";
                }
        }
    }

    switch (cause) {
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

enum FloatErrorType validate_floats_in_reg_buffer(void) {
    enum FloatErrorType fltErrType = FLT_ERR_NONE;

    for (int i = 0; i < gSavedRegBufSize; i++) {
        RegisterId reg = gSavedRegBuf[i];

        if (reg.flt) {
            IEEE754_f32 val = {
                .asU32 = get_reg_val(reg.cop, reg.idx)
            };
            fltErrType = validate_f32(val);

            if (fltErrType != FLT_ERR_NONE) {
                break;
            }
        }
    }

    return fltErrType;
}

// Returns a FPCSR description from 'sFpcsrDesc'.
// Only use 'specific' if disasm has just been run, because it checks saved registers with validate_floats_in_reg_buffer().
const char* get_fpcsr_desc(u32 fpcsr, _Bool specific) {
    u32 bit = BIT(FPCSR_SHIFT);

    for (u32 i = 0; i < NUM_FPCSR_DESC; i++) {
        if (fpcsr & bit) {
            if (specific && (i == FPCSR_DESC_CE)) {
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

typedef struct EmulatorName {
    /*0x00*/ const enum Emulator bits;
    /*0x04*/ const char* name;
} EmulatorName; /*0x08*/
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


// -- MAP SYMBOL --

typedef struct MapSymbolType {
    /*0x00*/ char c;
    /*0x01*/ u8 pad[3];
    /*0x04*/ const char* desc;
} MapSymbolType; /*0x08*/
static const MapSymbolType sMapSymbolTypes[] = {
    { .c = 'a', .desc = "absolute (static)", },
    { .c = 'A', .desc = "absolute",          },
    { .c = 'b', .desc = ".bss (static)",     },
    { .c = 'B', .desc = ".bss",              },
    { .c = 'd', .desc = ".data (static)",    },
    { .c = 'D', .desc = ".data",             },
    { .c = 't', .desc = ".text (static)",    },
    { .c = 'T', .desc = ".text",             },
    { .c = 'W', .desc = "weak (untagged)",   },
};
const char* get_map_symbol_type_desc(char c) {
    for (int i = 0; i < ARRAY_COUNT(sMapSymbolTypes); i++) {
        if (c == sMapSymbolTypes[i].c) {
            return sMapSymbolTypes[i].desc;
        }
    }

    return NULL;
}
