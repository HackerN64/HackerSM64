#ifndef MACROS_H
#define MACROS_H

#include "platform_info.h"

#ifndef __sgi
#define GLOBAL_ASM(...)
#endif

#define ARRAY_COUNT(arr) (s32)(sizeof(arr) / sizeof(arr[0]))

#define GLUE(a, b) a ## b
#define GLUE2(a, b) GLUE(a, b)

// Avoid compiler warnings for unused variables.
#ifdef __GNUC__
#define UNUSED __attribute__((unused))
#else
#define UNUSED
#endif

// Avoid undefined behaviour for non-returning functions.
#ifdef __GNUC__
#define NORETURN __attribute__((noreturn))
#else
#define NORETURN
#endif

// Always inline a function.
#ifdef __GNUC__
#define ALWAYS_INLINE inline __attribute__((always_inline))
#else
#define ALWAYS_INLINE inline
#endif

// Fall through a switch case.
#ifdef __GNUC__
#define FALL_THROUGH __attribute__((fallthrough))
#else
#define FALL_THROUGH
#endif

// Use Og when compiling the function.
#ifdef __GNUC__
#define OPTIMIZE_OG __attribute__((optimize("Og")))
#else
#define OPTIMIZE_OG 
#endif

// Use Os when compiling the function.
#ifdef __GNUC__
#define OPTIMIZE_OS __attribute__((optimize("Os")))
#else
#define OPTIMIZE_OS 
#endif

// Use Ofast when compiling the function.
#ifdef __GNUC__
#define OPTIMIZE_OFAST __attribute__((optimize("Ofast")))
#else
#define OPTIMIZE_OFAST 
#endif

// Ignore 4-byte alignment in structs.
#ifdef __GNUC__
#define PACKED __attribute__((packed))
#else
#define PACKED 
#endif

// Align to 4-byte boundary.
#ifdef __GNUC__
#define ALIGNED4 __attribute__((aligned(4)))
#else
#define ALIGNED4
#endif

// Align to 8-byte boundary (for DMA requirements).
#ifdef __GNUC__
#define ALIGNED8 __attribute__((aligned(8)))
#else
#define ALIGNED8
#endif

// Align to 16-byte boundary (for audio lib requirements).
#ifdef __GNUC__
#define ALIGNED16 __attribute__((aligned(16)))
#else
#define ALIGNED16
#endif

// Align to 32-byte boundary.
#ifdef __GNUC__
#define ALIGNED32 __attribute__((aligned(32)))
#else
#define ALIGNED32
#endif

// Align to 64-byte boundary.
#ifdef __GNUC__
#define ALIGNED64 __attribute__((aligned(64)))
#else
#define ALIGNED64
#endif

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
// Convert a virtual address to physical.
#define VIRTUAL_TO_PHYSICAL(addr)   ((uintptr_t)(addr) & 0x1FFFFFFF)

// Convert a physical address to virtual.
#define PHYSICAL_TO_VIRTUAL(addr)   ((uintptr_t)(addr) | 0x80000000)

// Another way of converting virtual to physical.
#define VIRTUAL_TO_PHYSICAL2(addr)  ((u8 *)(addr) - 0x80000000U)
#else // NO_SEGMENTED_MEMORY
// No conversion needed other than cast.
#define VIRTUAL_TO_PHYSICAL(addr)   ((uintptr_t)(addr))
#define PHYSICAL_TO_VIRTUAL(addr)   ((uintptr_t)(addr))
#define VIRTUAL_TO_PHYSICAL2(addr)  ((void *)(addr))
#endif // NO_SEGMENTED_MEMORY

// Static (compile-time) assertions.
#ifdef __GNUC__
#define STATIC_ASSERT(cond, msg) _Static_assert(cond, msg)
#else
#define STATIC_ASSERT(cond, msg) typedef char GLUE2(static_assertion_failed, __LINE__)[(cond) ? 1 : -1]
#endif

#define FORCE_CRASH { *(vs8*)0 = 0; }

// Taken from Linux Kernel
#define __GET_UNALIGNED_T(type, ptr)                                                                   \
    ({                                                                                                 \
        const struct {                                                                                 \
            type x;                                                                                    \
        } PACKED *__pptr = (typeof(__pptr)) (ptr);                                                     \
        __pptr->x;                                                                                     \
    })

#define __PUT_UNALIGNED_T(type, val, ptr)                                                              \
    do {                                                                                               \
        struct {                                                                                       \
            type x;                                                                                    \
        } PACKED *__pptr = (typeof(__pptr)) (ptr);                                                     \
        __pptr->x = (val);                                                                             \
    } while (0)

#define GET_UNALIGNED8(ptr) __GET_UNALIGNED_T(uint64_t, (ptr))
#define PUT_UNALIGNED8(val, ptr) __PUT_UNALIGNED_T(uint64_t, (val), (ptr))
#define GET_UNALIGNED4(ptr) __GET_UNALIGNED_T(uint32_t, (ptr))
#define GET_UNALIGNED4S(ptr) __GET_UNALIGNED_T(int32_t, (ptr))
#define PUT_UNALIGNED4(val, ptr) __PUT_UNALIGNED_T(uint32_t, (val), (ptr))

#endif // MACROS_H
