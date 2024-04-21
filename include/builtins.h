#pragma once

// These can be used by the preprocessor.
// https://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html

//! TODO: Should this include all builtins?


// unreachable
#define UNREACHABLE()       __builtin_unreachable()

// trap
#define TRAP()              __builtin_trap()

// offsetof
#define OFFSETOF(_T, _m)    __builtin_offsetof(_T, _m)


// Suffixes:
//    : unsigned int
// l  : unsigned long
// ll : unsigned long long
// g  : generic

// clz
#define CLZ(_x)             __builtin_clz(_x)
#define CLZL(_x)            __builtin_clzl(_x)
#define CLZLL(_x)           __builtin_clzll(_x)
#define CLZG(_x)            __builtin_clzg(_x)

// ctz
#define CTZ(_x)             __builtin_ctz(_x)
#define CTZL(_x)            __builtin_ctzl(_x)
#define CTZLL(_x)           __builtin_ctzll(_x)
#define CTZG(_x)            __builtin_ctzg(_x)

// popcount
#define POPCOUNT(_x)        __builtin_popcount(_x)
#define POPCOUNTL(_x)       __builtin_popcountl(_x)
#define POPCOUNTLL(_x)      __builtin_popcountll(_x)
#define POPCOUNTG(_x)       __builtin_popcountg(_x)

// // bswap
// #define BSWAP16(_x)         __builtin_bswap16(_x)
// #define BSWAP32(_x)         __builtin_bswap32(_x)
// #define BSWAP64(_x)         __builtin_bswap64(_x)
// // #define BSWAP128(_x)        __builtin_bswap128(_x)
