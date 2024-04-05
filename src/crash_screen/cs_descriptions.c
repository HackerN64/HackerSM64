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
#include "game/level_update.h"


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


typedef struct IdNamePair {
    /*0x00*/ const int id;
    /*0x04*/ const char* name;
} IdNamePair; /*0x08*/
static const char* get_name_from_id_list_impl(int id, const IdNamePair* list, size_t count) {
    for (size_t i = 0; i < count; i++) {
        if (id == list[i].id) {
            return list[i].name;
        }
    }

    return NULL;
}
#define get_name_from_id_list(_id, _list) get_name_from_id_list_impl((_id), (_list), ARRAY_COUNT(_list))

typedef struct RangeNamePair {
    /*0x00*/ const u32 start;
    /*0x04*/ const u32 end;
    /*0x08*/ const char* name;
} RangeNamePair; /*0x0C*/
static const char* get_name_from_range_list_impl(u32 id, const RangeNamePair* list, size_t count) {
    for (size_t i = 0; i < count; i++) {
        if ((id >= list[i].start) && (id < list[i].end)) {
            return list[i].name;
        }
    }

    return NULL;
}
#define get_name_from_range_list(_id, _list) get_name_from_range_list_impl((_id), (_list), ARRAY_COUNT(_list))


// -- THREAD --

static const IdNamePair sThreadIDNames[] = {
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
static const IdNamePair sThreadPriNames[] = {
    { .id = OS_PRIORITY_SIMGR,          .name = "si manager", },
    { .id = 149,                        .name = "debug/usb?", }, //! TODO: Find out what this thread is. It only exists when running UNF.
    { .id = OS_PRIORITY_PIMGR,          .name = "pi manager", },
    { .id = OS_PRIORITY_VIMGR,          .name = "vi manager", },
    { .id = OS_PRIORITY_RMON,           .name = "rmon",       },
    { .id = OS_PRIORITY_RMONSPIN,       .name = "rmonspin",   },
};
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
        name = get_name_from_id_list(pri, sThreadPriNames);
        if (name != NULL) {
            return name;
        }
    }

    name = get_name_from_id_list(id, sThreadIDNames);
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


// -- WARP NODE PRESETS --

static IdNamePair sWarpNodeSpecialIds[] = {
    { .id = WARP_NODE_MAIN_ENTRY,    .name = "main entry",    },
    { .id = WARP_NODE_DEFAULT,       .name = "default",       },
    { .id = WARP_NODE_DEATH,         .name = "death",         },
    { .id = WARP_NODE_LOOK_UP,       .name = "look up",       },
    { .id = WARP_NODE_WARP_FLOOR,    .name = "warp floor",    },
    { .id = WARP_NODE_CREDITS_MIN,   .name = "credits min",   },
    { .id = WARP_NODE_CREDITS_START, .name = "credits start", },
    { .id = WARP_NODE_CREDITS_NEXT,  .name = "credits next",  },
    { .id = WARP_NODE_CREDITS_END,   .name = "credits end",   },
};
const char* get_warp_node_name(const enum WarpNodes id) {
    const char* ret = get_name_from_id_list(id, sWarpNodeSpecialIds);
    return ((ret != NULL) ? ret : "");
}


// -- SEGMENTS --

static const char* sSegmentNames[32] = {
    [SEGMENT_MAIN               ] = "main",
    [SEGMENT_RENDER             ] = "render",
    [SEGMENT_SEGMENT2           ] = "segment2",
    [SEGMENT_COMMON1_YAY0       ] = "common1 gfx",
    [SEGMENT_GROUP0_YAY0        ] = "group0 gfx",
    [SEGMENT_GROUPA_YAY0        ] = "groupA gfx",
    [SEGMENT_GROUPB_YAY0        ] = "groupB gfx",
    [SEGMENT_LEVEL_DATA         ] = "level data",
    [SEGMENT_COMMON0_YAY0       ] = "common0 gfx",
    [SEGMENT_TEXTURE            ] = "textures",
    [SEGMENT_SKYBOX             ] = "skybox",
    [SEGMENT_EFFECT_YAY0        ] = "effects",
    [SEGMENT_GROUPA_GEO         ] = "groupA geo",
    [SEGMENT_GROUPB_GEO         ] = "groupB geo",
    [SEGMENT_LEVEL_SCRIPT       ] = "level script",
    [SEGMENT_COMMON0_GEO        ] = "common0 geo",
    [SEGMENT_LEVEL_ENTRY        ] = "level entry",
    [SEGMENT_MARIO_ANIMS        ] = "mario anims",
    [SEGMENT_UNKNOWN_18         ] = "18",
    [SEGMENT_BEHAVIOR_DATA      ] = "bhv data",
    [SEGMENT_MENU_INTRO         ] = "menu",
    [SEGMENT_GLOBAL_LEVEL_SCRIPT] = "global level script",
    [SEGMENT_COMMON1_GEO        ] = "common1 geo",
    [SEGMENT_GROUP0_GEO         ] = "group0 geo",
    [SEGMENT_DEMO_INPUTS        ] = "demo inputs",
    [SEGMENT_EU_TRANSLATION     ] = "translations",
    [SEGMENT_UNKNOWN_26         ] = "26",
    [SEGMENT_UNKNOWN_27         ] = "27",
    [SEGMENT_UNKNOWN_28         ] = "28",
    [SEGMENT_UNKNOWN_29         ] = "29",
    [SEGMENT_UNKNOWN_30         ] = "30",
    [SEGMENT_UNKNOWN_31         ] = "31",
};
const char* get_segment_name(u8 segmentId) {
    return ((segmentId < ARRAY_COUNT(sSegmentNames)) ? sSegmentNames[segmentId] : NULL);
}


// -- RAM POOLS --

#include "game/game_init.h"
#include "engine/surface_load.h"

extern u32 sPoolFreeSpace;
extern u8 *sPoolStart;
extern u8 *sPoolEnd;


static const RangeNamePair sHardcodedSegmentRanges[] = {
    { .start = (Address)_mainSegmentStart,            .end = (Address)_mainSegmentEnd,            .name = "MAIN SEG",     },
    { .start = (Address)_engineSegmentStart,          .end = (Address)_engineSegmentEnd,          .name = "ENGINE SEG",   },
    { .start = (Address)_goddardSegmentStart,         .end = (Address)_goddardSegmentEnd,         .name = "GODDARD",      },
    { .start = (Address)_framebuffersSegmentBssStart, .end = (Address)_framebuffersSegmentBssEnd, .name = "FRAMEBUFFERS", },
    { .start = (Address)_zbufferSegmentBssStart,      .end = (Address)_zbufferSegmentBssEnd,      .name = "ZBUFFER",      },
    { .start = (Address)_buffersSegmentBssStart,      .end = (Address)_buffersSegmentBssEnd,      .name = "BUFFERS",      },
};

// For stuff that doesn't have a map symbol name.
const char* get_hardcoded_memory_str(Address addr) {
    // Is addr in static surface pool?
    if (
        (addr >= (Address)gCurrStaticSurfacePool) &&
        (addr < (Address)gCurrStaticSurfacePoolEnd)
    ) {
        return "STATIC SURF POOL";
    }

    // Is addr in dynamic surface pool?
    if (addr >= (Address)gDynamicSurfacePool) {
        if (addr < (Address)gDynamicSurfacePoolEnd) {
            return "DYNAMIC SURF POOL (ACTIVE)";
        } else if (addr < (Address)((Byte*)gCurrStaticSurfacePool + DYNAMIC_SURFACE_POOL_SIZE)) {
            return "DYNAMIC SURF POOL (UNUSED)";
        }
    }

#ifdef INCLUDE_DEBUG_MAP
    // Is addr in map symbol data?
    if ((gNumMapSymbols != 0) /* Check whether the map data was loaded. */ && (addr >= (Address)_mapDataSegmentStart)) {
        if (addr < (Address)((Byte*)_mapDataSegmentStart + ((gMapSymbolsEnd - gMapSymbols) * sizeof(MapSymbol)))) {
            return "MAP SYMBOLS (DATA)";
        } else if (addr < (Address)_mapDataSegmentEnd) {
            return "MAP SYMBOLS (STRINGS)";
        }
    }
#else // !INCLUDE_DEBUG_MAP
    // Inportant sections that had a map symbol:

    // Is addr in GFX pool?
    if (addr >= (Address)gGfxPool->buffer) {
        if (addr < (Address)gDisplayListHead) {
            return "GFX POOL (USED)";
        } else if (addr < (Address)gGfxPoolEnd) {
            return "GFX POOL (ALLOCATED)";
        } else if (addr < (Address)((Byte*)gGfxPool->buffer + GFX_POOL_SIZE)) {
            return "GFX POOL (UNUSED)";
        }
    }

    //! TODO: audio heap, thread stacks, SPTasks, save buffer, both gGfxPools.
#endif // !INCLUDE_DEBUG_MAP

    const char* hardcodedSegmentStr = get_name_from_range_list(addr, sHardcodedSegmentRanges);
    if (hardcodedSegmentStr != NULL) {
        return hardcodedSegmentStr;
    }

    if (addr >= (Address)sPoolStart) {
        if (addr < ((Address)sPoolEnd - sPoolFreeSpace)) {
            return "MAIN POOL (USED)";
        }
        if (addr < (Address)sPoolEnd) {
            return "MAIN POOL (UNUSED)";
        }
    }

    return NULL;
}


// -- PROCESSOR --

// https://en.wikichip.org/wiki/mips/prid_register
const IdNamePair sPRId_names[] = {
    { .id = 0x0B, .name = "vr4300", },
};
const char* get_processor_name(u8 imp) {
    const char* ret = get_name_from_id_list(imp, sPRId_names);
    return ((ret != NULL) ? ret : "unknown");
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
_Bool check_for_empty_infinite_loop(Address pc, _Bool inBranchDelaySlot) {
    InsnData insn = { .raw = 0x00000000, };
    InsnData prev = { .raw = 0x00000000, };
    _Bool insnValid = try_read_word_aligned(&insn.raw, pc);
    #define INSN_IS_B_0(_insn) (((_insn).opcode == OPC_BEQ) && ((_insn).rs == (_insn).rt) && ((_insn).offset == (u16)-1))
    return (
        insnValid && (
            INSN_IS_B_0(insn) || (
                inBranchDelaySlot &&
                try_read_word_aligned(&prev.raw, (pc - sizeof(Word))) &&
                (insn.raw == 0x00000000) &&
                INSN_IS_B_0(prev)
            )
        )
    );
    #undef INSN_IS_B_0
}
// Returns a CAUSE description from 'sCauseDesc'.
const char* get_cause_desc(__OSThreadContext* tc, _Bool specific) {
    u32 cause = (tc->cause & CAUSE_EXCMASK);

    if (specific) {
        Address badvaddr = tc->badvaddr;
        Address pc = tc->pc;
        _Bool inBranchDelaySlot = ((Reg_CP0_Cause)tc->cause).BD;
        Address epc = (pc + (inBranchDelaySlot ? sizeof(Word) : 0)); // GET_EPC(pc);

        // Special crash heuristics, mostly from libdragon:
        switch (cause) {
            case EXC_INT: // Non-crash interrupts (can be shown after changing the inspected thread).
                if (pc == ADDR_INSN_WAITING_FOR_MESG) {
                    return "Waiting for mesg";
                }
                if (check_for_empty_infinite_loop(pc, inBranchDelaySlot)) {
                    return "Empty infinite loop";
                }
                break;
            case EXC_MOD:
                return "Write to read-only memory";
            case EXC_RMISS:
                if (epc == badvaddr) {
                    return "Invalid program counter address";
                } else if (badvaddr < 128) {
                    // This is probably a NULL pointer dereference, though it can go through a structure or an array,
                    // so leave some margin to the actual faulting address.
                    if (pc == INSN_OFFSET_FROM_ADDR(strlen, 0)) { // 1st instruction of strlen
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
                if (epc == badvaddr) {
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
                if (pc == ADDR_INSN_ASSERT) {
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

        if (reg.valInfo.type == REG_VAL_TYPE_FLOAT) {
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

static const IdNamePair sEmulatorStrings[] = {
    { .id = EMU_WIIVC,            .name = "Wii VC",           },
    { .id = EMU_PROJECT64_1_OR_2, .name = "pj64 1 or 2",      },
    { .id = EMU_PROJECT64_3,      .name = "pj64 3",           },
    { .id = EMU_PROJECT64_4,      .name = "pj64 4",           },
    { .id = EMU_MUPEN_OLD,        .name = "mupen (old)",      },
    { .id = EMU_MUPEN64PLUS_NEXT, .name = "mupen64plus-next", },
    { .id = EMU_CEN64,            .name = "cen64",            },
    { .id = EMU_SIMPLE64,         .name = "simple64",         },
    { .id = EMU_PARALLELN64,      .name = "ParaLLEl N64",     },
    { .id = EMU_ARES,             .name = "ares",             },
    { .id = EMU_CONSOLE,          .name = "CONSOLE",          },
};
const char* get_emulator_name(enum Emulator emu) {
    return get_name_from_id_list(emu, sEmulatorStrings);
}


// -- MAP SYMBOL --

// https://sourceware.org/binutils/docs/binutils/nm.html
static const IdNamePair sMapSymbolTypes[] = {
    { .id = 'a', .name = "absolute (static)", }, // Local absolute symbol.
    { .id = 'A', .name = "absolute",          }, // Global absolute symbol.
    { .id = 'b', .name = ".bss (static)",     }, // Local bss symbol.
    { .id = 'B', .name = ".bss",              }, // Global bss symbol.
    { .id = 'd', .name = ".data (static)",    }, // Local data symbol.
    { .id = 'D', .name = ".data",             }, // Global data symbol.
    { .id = 'r', .name = ".rodata (static)",  }, // Local read only symbol.
    { .id = 'R', .name = ".rodata",           }, // Global read only symbol.
    { .id = 't', .name = ".text (static)",    }, // Local text symbol.
    { .id = 'T', .name = ".text",             }, // Global text symbol.
    { .id = 'W', .name = "weak (untagged)",   },
    { .id = 'U', .name = "undefined",         }, // Undefined symbol.
};
const char* get_map_symbol_type_desc(char c) {
    return get_name_from_id_list(c, sMapSymbolTypes);
}
