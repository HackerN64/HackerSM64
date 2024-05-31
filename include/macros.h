#ifndef MACROS_H
#define MACROS_H

#include "platform_info.h"

#ifndef __sgi
#define GLOBAL_ASM(...)
#endif

#include "attributes.h"
#include "builtins.h"

#define member(_T, _m) ((_T*)0)->_m
#define sizeof_member(_T, _m) sizeof(member(_T, _m))
#define typeof_member(_T, _m) typeof(member(_T, _m))

#define ARRAY_COUNT(_arr) (ssize_t)(sizeof(_arr) / sizeof(_arr[0])) //! TODO: Use size_t here to match sizeof.
#define FITS_IN_ARRAY(_x, _arr) (((_x) >= 0) && ((_x) < ARRAY_COUNT(_arr)))

#define BITS_PER_BYTE   __CHAR_BIT__
#define BITS_PER_HEX    4
#define SIZEOF_BITS(_t) (sizeof(_t) * BITS_PER_BYTE) // NOTE: Does not work with sizes that are not multiples of 8 (eg. packed struct bitfields).
#define SIZEOF_HEX(_t)  (SIZEOF_BITS(_t) / BITS_PER_HEX)

#define GLUE(_a, _b) _a ## _b
#define GLUE2(_a, _b) GLUE(_a, _b)

#define STRINGIFY(_s)    #_s
#define EXPAND_AND_STRINGIFY(_s)   STRINGIFY(_s)

#define STRLEN(_s)          (sizeof(_s) - 1)
#define STR_LAST_CHAR(_s)   (_s)[STRLEN(_s) - 1]

// Includes the raw data of the file at 'path' into the rom, aligned by 'align' number of bytes, and creates a pointer to it with the given type and name.
//! TODO: Non-.rodata versions.
#define INCBIN(_type, _name, _path, _align) \
    __asm__( \
        ".section \".rodata\", \"a\", @progbits\n" \
        ".balign "EXPAND_AND_STRINGIFY(_align)"\n" \
        ".global "EXPAND_AND_STRINGIFY(_name)"\n" \
        EXPAND_AND_STRINGIFY(_name)":\n" \
        ".incbin \""_path"\"\n" \
        ".previous\n" \
    ); \
    extern _type _name[];

// Align to previous boundry.
#ifndef ALIGNFLOOR
#define ALIGNFLOOR(VAL_, ALIGNMENT_) ((VAL_) & ~(ALIGNMENT_ - 1))
#endif

// Align to next boundary.
#ifndef ALIGN
#define ALIGN(VAL_, ALIGNMENT_) (((VAL_) + ((ALIGNMENT_) - 1)) & ~((ALIGNMENT_) - 1))
#endif

// Round up to the next multiple.
#define ALIGN4(val)  ALIGN((val),  4)
#define ALIGN8(val)  ALIGN((val),  8)
#define ALIGN16(val) ALIGN((val), 16)
#define ALIGN32(val) ALIGN((val), 32)
#define ALIGN64(val) ALIGN((val), 64)

#ifndef NO_SEGMENTED_MEMORY
// Convert a physical address to virtual.
#define PHYSICAL_TO_VIRTUAL(addr)   ((uintptr_t)(addr) | 0x80000000)

// Convert a virtual address to physical.
#define VIRTUAL_TO_PHYSICAL(addr)   ((uintptr_t)(addr) & 0x1FFFFFFF)

// Another way of converting virtual to physical.
#define VIRTUAL_TO_PHYSICAL2(addr)  ((u8*)(addr) - 0x80000000U)
#else // NO_SEGMENTED_MEMORY
// No conversion needed other than cast.
#define PHYSICAL_TO_VIRTUAL(addr)   ((uintptr_t)(addr))
#define VIRTUAL_TO_PHYSICAL(addr)   ((uintptr_t)(addr))
#define VIRTUAL_TO_PHYSICAL2(addr)  ((void*)(addr))
#endif // NO_SEGMENTED_MEMORY


//! TODO: Move everything below here to assert.h?:

// Static (compile-time) assertions.
#ifdef __GNUC__
#define STATIC_ASSERT(cond, msg) _Static_assert(cond, msg)
#else
#define STATIC_ASSERT(cond, msg) typedef char GLUE2(static_assertion_failed, __LINE__)[(cond) ? 1 : -1]
#endif

#include "game/assert.h"

#include "game/asm.h"

//! TODO: Ideally this should use EXCEPTION_TRAP() to save 8 bytes, but that freezes most emulators instead of doing a proper crash.
#define FORCE_CRASH() do {  \
    EXCEPTION_RMISS();      \
    UNREACHABLE();          \
} while (0);

// Set where the program counter will be on the next crash.
#define SET_CRASH_PTR(ptr) \
do { \
    extern uintptr_t gSetCrashAddress; \
    gSetCrashAddress = (uintptr_t)&(ptr); \
} while (0)

// Cause a crash with the program counter at a specific location.
#define FORCE_CRASH_AT_PTR(ptr) \
do { \
    SET_CRASH_PTR(ptr); \
    FORCE_CRASH(); \
} while (0)

#endif // MACROS_H
