#pragma once



// Avoid compiler warnings for unused variables or functions.
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

// Never inline a function.
#ifdef __GNUC__
#define NEVER_INLINE __attribute__((noinline))
#else
#define NEVER_INLINE
#endif

// Fall through a switch case.
#ifdef __GNUC__
#define FALL_THROUGH __attribute__((fallthrough))
#else
#define FALL_THROUGH
#endif

// Hot (fequently used) code paths.
#ifdef __GNUC__
#define HOT __attribute__((hot))
#else
#define HOT
#endif

// Cold (infrequently used) code paths.
#ifdef __GNUC__
#define COLD __attribute__((cold))
#else
#define COLD
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

// Align to arbitrary byte boundary (must be a power of 2).
#ifdef __GNUC__
#define ALIGNED(__align__) __attribute__((aligned(__align__)))
#else
#define ALIGNED(__align__)
#endif
