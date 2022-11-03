#ifndef MACROS_H
#define MACROS_H

#include "platform_info.h"

#ifndef __sgi
#define GLOBAL_ASM(...)
#endif

#define ARRAY_COUNT(arr) (s32)(sizeof(arr) / sizeof(arr[0]))

#define GLUE(a, b) a ## b
#define GLUE2(a, b) GLUE(a, b)

// Avoid compiler warnings for unused variables
#ifdef __GNUC__
#define UNUSED __attribute__((unused))
#else
#define UNUSED
#endif

// Avoid undefined behaviour for non-returning functions
#ifdef __GNUC__
#define NORETURN __attribute__((noreturn))
#else
#define NORETURN
#endif

// Always inline a function
#ifdef __GNUC__
#define ALWAYS_INLINE inline __attribute__((always_inline))
#else
#define ALWAYS_INLINE inline
#endif

// Fall through a switch case
#ifdef __GNUC__
#define FALL_THROUGH __attribute__((fallthrough))
#else
#define FALL_THROUGH
#endif

// Align to 4-byte boundary
#ifdef __GNUC__
#define ALIGNED4 __attribute__((aligned(4)))
#else
#define ALIGNED4
#endif

// Align to 8-byte boundary (for DMA requirements)
#ifdef __GNUC__
#define ALIGNED8 __attribute__((aligned(8)))
#else
#define ALIGNED8
#endif

// Align to 16-byte boundary (for audio lib requirements)
#ifdef __GNUC__
#define ALIGNED16 __attribute__((aligned(16)))
#else
#define ALIGNED16
#endif

// Align to 32-byte boundary
#ifdef __GNUC__
#define ALIGNED32 __attribute__((aligned(32)))
#else
#define ALIGNED32
#endif

// Align to 64-byte boundary
#ifdef __GNUC__
#define ALIGNED64 __attribute__((aligned(64)))
#else
#define ALIGNED64
#endif

#ifndef NO_SEGMENTED_MEMORY
// convert a virtual address to physical.
#define VIRTUAL_TO_PHYSICAL(addr)   ((uintptr_t)(addr) & 0x1FFFFFFF)

// convert a physical address to virtual.
#define PHYSICAL_TO_VIRTUAL(addr)   ((uintptr_t)(addr) | 0x80000000)

// another way of converting virtual to physical
#define VIRTUAL_TO_PHYSICAL2(addr)  ((u8 *)(addr) - 0x80000000U)
#else
// no conversion needed other than cast.
#define VIRTUAL_TO_PHYSICAL(addr)   ((uintptr_t)(addr))
#define PHYSICAL_TO_VIRTUAL(addr)   ((uintptr_t)(addr))
#define VIRTUAL_TO_PHYSICAL2(addr)  ((void *)(addr))
#endif

// Static (compile-time) assertions.
#ifdef __GNUC__
#define STATIC_ASSERT(cond, msg) _Static_assert(cond, msg)
#else
#define STATIC_ASSERT(cond, msg) typedef char GLUE2(static_assertion_failed, __LINE__)[(cond) ? 1 : -1]
#endif

#define FORCE_CRASH do { *(vs8*)0 = 0; } while (0)

#define SET_CRASH_ADDR(ptr) \
do { \
    extern uintptr_t gCrashAddress; \
    gCrashAddress = (uintptr_t)&(ptr); \
} while (0)

#define FORCE_CRASH_AT_ADDR(ptr) \
do { \
    SET_CRASH_ADDR(ptr); \
    FORCE_CRASH; \
} while (0)

#endif // MACROS_H
