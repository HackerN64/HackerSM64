#pragma once

// These can be used by the preprocessor.
// https://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html


// Suffixes:
//    : unsigned int
// l  : unsigned long
// ll : unsigned long long
// g  : generic

// clz
#define CLZ(_bits)          __builtin_clz(_bits)
#define CLZL(_bits)         __builtin_clzl(_bits)
#define CLZLL(_bits)        __builtin_clzll(_bits)
#define CLZG(_bits)         __builtin_clzg(_bits)

// ctz
#define CTZ(_bits)          __builtin_ctz(_bits)
#define CTZL(_bits)         __builtin_ctzl(_bits)
#define CTZLL(_bits)        __builtin_ctzll(_bits)
#define CTZG(_bits)         __builtin_ctzg(_bits)

// popcount
#define POPCOUNT(_bits)     __builtin_popcount(_bits)
#define POPCOUNTL(_bits)    __builtin_popcountl(_bits)
#define POPCOUNTLL(_bits)   __builtin_popcountll(_bits)
#define POPCOUNTG(_bits)    __builtin_popcountg(_bits)
