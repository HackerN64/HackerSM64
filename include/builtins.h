#pragma once

// These can be used by the preprocessor.
//! TODO: Should this include every valid builtin?


// offsetof (offset of member in struct, in bytes)
// https://gcc.gnu.org/onlinedocs/gcc/Offsetof.html
#define OFFSETOF(_T, _m)    __builtin_offsetof(/*type*/ _T, /*member*/ _m)


// Other GCC builtins:
// https://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html

// alloca (allocates memory on the stack)
#define ALLOCA(_size)                                   __builtin_alloca(_size)
#define ALLOCA_WITH_ALIGN(_size, _align)                __builtin_alloca_with_align((_size), (_align))
#define ALLOCA_WITH_ALIGN_AND_MAX(_size, _align, _max)  __builtin_alloca_with_align_and_max((_size), (_align), (_max))

// has_attribute (returns whether type or symbol '_T' has attribute '_a')
#define HAS_ATTRIBUTE(_T, _a)   __builtin_has_attribute(_T, _a)

// speculation_safe_value 
//! TODO:
#define SPECULATION_SAFE_VALUE(_val, _failval)  __builtin_speculation_safe_value(_val, _failval)

// types_compatible_p (returns whether two types are compatible)
#define TYPES_COMPATIBLE_P(_T1, _T2)    __builtin_types_compatible_p(_T1, _T2)

// call_with_static_chain
//! TODO:
#define CALL_WITH_STATIC_CHAIN(_call_exp, _pointer_exp) __builtin_call_with_static_chain(_call_exp, _pointer_exp)

// choose_expr
//! TODO:
#define CHOOSE_EXPR(_const_exp, _exp1, _exp2)   __builtin_choose_expr(_const_exp, _exp1, _exp2)

// tgmath
//! TODO:
#define TGMATH(_functions, _arguments)  __builtin_tgmath(_functions, _arguments)

// constant_p (_returns whether the expression '_p' is constant at compile time)
#define CONSTANT_P(_exp)    __builtin_constant_p(_exp)

// clear_padding (clears padding bits of object at '_ptr')
#define CLEAR_PADDING(_ptr) __builtin_clear_padding(_ptr)

// expect ('_exp' likely == '_c')
#define EXPECT(_exp, _c)                            __builtin_expect(_exp, _c)
#define EXPECT_LIKELY(_exp)                         EXPECT(_exp, TRUE)
#define EXPECT_UNLIKELY(_exp)                       EXPECT(_exp, FALSE)
#define EXPECT_WITH_PROBABILITY(_exp, _c, _prob)    __builtin_expect_with_probability(_exp, _c, _prob)

// trap (exit program abnormally)
//! TODO: Why does this freeze most emulators instead of triggering a crash?
#define TRAP()              __builtin_trap()

// unreachable (tells the compiler that this location is unreachable in the code)
#define UNREACHABLE()       __builtin_unreachable()

// assoc_barrier
//! TODO:
#define ASSOC_BARRIER(_expr)    __builtin_assoc_barrier(_expr)

// assume_aligned
//! TODO:
#define ASSUME_ALIGNED(_exp, align, ...)    __builtin_assume_aligned(_exp, _align, __VA_ARGS__)

// hardcoded strings describing the current location
#define LINE()              __builtin_LINE()        // Current line number as integer (equivalent to __LINE__).
#define FUNCTION()          __builtin_FUNCTION()    // Current function name as string pointer (equivalent to __FUNCTION__).
#define FILE()              __builtin_FILE()        // Current file name as string pointer (equivalent to __FILE__).

// clear_cache (flush icache between '_begin' and '_end')
#define CLEAR_CACHE(_begin, _end)   __builtin___clear_cache(_begin, _end)

// prefetch
//! TODO:
#define PREFETCH(_addr, ...)    __builtin_prefetch(_addr, __VA_ARGS__)

// object_size
// https://gcc.gnu.org/onlinedocs/gcc/Object-Size-Checking.html
#define OBJECT_SIZE(_ptr, _type)            __builtin_object_size(_ptr, _type)
#define DYNAMIC_OBJECT_SIZE(_ptr, _type)    __builtin_dynamic_object_size(_ptr, _type)

// classify_type
typedef enum __ClassifyTypeEnum { // should be equivalent to "enum type_class" in "typeclass.h"
    TYPE_NONE      = -1, // undefined type (eg. blank)
    TYPE_VOID      =  0, // Does not generate with current settings.
    TYPE_INTEGER   =  1, // eg. char, short, float, double
    TYPE_CHAR      =  2, // Does not generate with current settings.
    TYPE_ENUMERAL  =  3, // Does not generate with current settings.
    TYPE_BOOL      =  4, // Does not generate with current settings.
    TYPE_POINTER   =  5, // pointer
    TYPE_REFERENCE =  6, // Does not generate with current settings.
    TYPE_OFFSET    =  7, // Does not generate with current settings.
    TYPE_REAL      =  8, // AKA floating-point
    TYPE_COMPLEX   =  9, // complex/imaginary (eg. 2i)
    TYPE_FUNCTION  = 10, // Does not generate with current settings.
    TYPE_METHOD    = 11, // Does not generate with current settings.
    TYPE_RECORD    = 12, // AKA struct
    TYPE_UNION     = 13, // union
    TYPE_ARRAY     = 14, // Does not generate with current settings.
    TYPE_STRING    = 15, // Does not generate with current settings.
    TYPE_LANG      = 16, // Does not generate with current settings.
} __ClassifyTypeEnum;
#define CLASSIFY_TYPE(_x)   __builtin_classify_type(_x)
#define IS_INTEGER(_x)      (CLASSIFY_TYPE(_x) == TYPE_INTEGER)
#define IS_POINTER(_x)      (CLASSIFY_TYPE(_x) == TYPE_POINTER)
#define IS_FLOAT(_x)        (CLASSIFY_TYPE(_x) == TYPE_REAL)
#define IS_COMPLEX(_x)      (CLASSIFY_TYPE(_x) == TYPE_COMPLEX)
#define IS_STRUCT(_x)       (CLASSIFY_TYPE(_x) == TYPE_RECORD)
#define IS_UNION(_x)        (CLASSIFY_TYPE(_x) == TYPE_UNION)

// huge_val (floating-point positive infinity, or DBL_MAX if infinity is unsupported)
#define HUGE_VAL()      __builtin_huge_val()    // generic
#define HUGE_VALF()     __builtin_huge_valf()   // float
#define HUGE_VALL()     __builtin_huge_vall()   // long double
#define HUGE_VALFN()    __builtin_huge_valfn()  // _Floatn
#define HUGE_VALFNX()   __builtin_huge_valfnx() // _Floatnx

// fpclassify
typedef enum __FPClassifyEnum {
    FP_NAN,         // NaN
    FP_INFINITE,    // ±Infinity
    FP_NORMAL,      // Normalized float
    FP_SUBNORMAL,   // Denormalized float
    FP_ZERO,        // ±Dero
} __FPClassifyEnum;
#define FPCLASSIFY(_x)      __builtin_fpclassify(FP_NAN, FP_INFINITE, FP_NORMAL, FP_SUBNORMAL, FP_ZERO, (_x))
#define FLT_IS_NAN(_x)      (FPCLASSIFY(_x) == FP_NAN)
#define FLT_IS_INF(_x)      (FPCLASSIFY(_x) == FP_INFINITE)
#define FLT_IS_NORM(_x)     (FPCLASSIFY(_x) == FP_NORMAL)
#define FLT_IS_SUBNORM(_x)  (FPCLASSIFY(_x) == FP_SUBNORMAL)
#define FLT_IS_ZERO(_x)     (FPCLASSIFY(_x) == FP_ZERO)

// inf (floating-point infinity)
#define F_INF()       __builtin_inf()     // generic
#define F_INFD32()    __builtin_infd32()  // _Decimal32
#define F_INFD64()    __builtin_infd64()  // _Decimal64
#define F_INFD128()   __builtin_infd128() // _Decimal128
#define F_INFF()      __builtin_inff()    // float
#define F_INFL()      __builtin_infl()    // long double
#define F_INFFN()     __builtin_inffn()   // _Floatn
#define F_INFFNX()    __builtin_inffnx()  // _Floatnx

// isinf_sign (returns: [0=!inf, 1=+inf, -1=-inf])
#define F_ISINF_SIGN(_f)  __builtin_isinf_sign(_f)

// NaN
#define F_NAN(_str)       __builtin_nan(_str)         // generic
#define F_NAND32(_str)    __builtin_nand32(_str)      // _Decimal32
#define F_NAND64(_str)    __builtin_nand64(_str)      // _Decimal64
#define F_NAND128(_str)   __builtin_nand128(_str)     // _Decimal128
#define F_NANF(_str)      __builtin_nanf(_str)        // float
#define F_NANL(_str)      __builtin_nanl(_str)        // long double
#define F_NANFN(_str)     __builtin_nanfn(_str)       // _Floatn
#define F_NANFNX(_str)    __builtin_nanfnx(_str)      // _Floatnx
// forced signaling NaN:
#define F_NANS(_str)      __builtin_nans(_str)        // generic
#define F_NANSD32(_str)   __builtin_nansd32(_str)     // _Decimal32
#define F_NANSD64(_str)   __builtin_nansd64(_str)     // _Decimal64
#define F_NANSD128(_str)  __builtin_nansd128(_str)    // _Decimal128
#define F_NANSF(_str)     __builtin_nansf(_str)       // float
#define F_NANSL(_str)     __builtin_nansl(_str)       // long double
#define F_NANSFN(_str)    __builtin_nansfn(_str)      // _Floatn
#define F_NANSFNX(_str)   __builtin_nansfnx(_str)     // _Floatnx

// is signaling (returns whether '_f' is signaling NaN)
#define F_ISSIGNALING(_f) __builtin_issignaling(_f)

// Suffixes:
//    : unsigned int
// l  : unsigned long
// ll : unsigned long long
// g  : generic

// ffs (returns 1 + the least significant 1-bit of '_x')
#define FFS(_x)             __builtin_ffs(_x)
#define FFSL(_x)            __builtin_ffsl(_x)
#define FFSLL(_x)           __builtin_ffsll(_x)
#define FFSG(...)           __builtin_ffsg(__VA_ARGS__)

// clz (returns the number of leading 0-bits in '_x')
#define CLZ(_x)             __builtin_clz(_x)
#define CLZL(_x)            __builtin_clzl(_x)
#define CLZLL(_x)           __builtin_clzll(_x)
#define CLZG(...)           __builtin_clzg(__VA_ARGS__)

// ctz (returns the number of trailing 0-bits in '_x')
#define CTZ(_x)             __builtin_ctz(_x)
#define CTZL(_x)            __builtin_ctzl(_x)
#define CTZLL(_x)           __builtin_ctzll(_x)
#define CTZG(...)           __builtin_ctzg(__VA_ARGS__)

// popcount (returns the number of 1-bits in '_x')
#define POPCOUNT(_x)        __builtin_popcount(_x)
#define POPCOUNTL(_x)       __builtin_popcountl(_x)
#define POPCOUNTLL(_x)      __builtin_popcountll(_x)
#define POPCOUNTG(...)      __builtin_popcountg(__VA_ARGS__)

// parity (returns the number of 1-bits in '_x' modulo 2)
#define PARITY(_x)          __builtin_parity(_x)
#define PARITYL(_x)         __builtin_parityl(_x)
#define PARITYLL(_x)        __builtin_parityll(_x)
#define PARITYG(...)        __builtin_parityg(__VA_ARGS__)


#ifdef __STDC__
// stdc
#define BIT_CEIL(_x)            __builtin_stdc_bit_ceil(_x)
#define BIT_FLOOR(_x)           __builtin_stdc_bit_floor(_x)
#define BIT_WIDTH(_x)           __builtin_stdc_bit_width(_x)
#define COUNT_ONES(_x)          __builtin_stdc_count_ones(_x)
#define COUNT_ZEROS(_x)         __builtin_stdc_count_zeros(_x)
#define FIRST_LEADING_ONE(_x)   __builtin_stdc_first_leading_one(_x)
#define FIRST_LEADING_ZERO(_x)  __builtin_stdc_first_leading_zero(_x)
#define FIRST_TRAILING_ONE(_x)  __builtin_stdc_first_trailing_one(_x)
#define FIRST_TRAILING_ZERO(_x) __builtin_stdc_first_trailing_zero(_x)
#define HAS_SINGLE_BIT(_x)      __builtin_stdc_has_single_bit(_x)
#define LEADING_ONES(_x)        __builtin_stdc_leading_ones(_x)
#define LEADING_ZEROS(_x)       __builtin_stdc_leading_zeros(_x)
#define TRAILING_ONES(_x)       __builtin_stdc_trailing_ones(_x)
#define TRAILING_ZEROS(_x)      __builtin_stdc_trailing_zeros(_x)
#endif // __STDC__

// powi
#define POWI(_x, _i)        __builtin_powi((_x), (_i))  // double
#define POWIF(_x, _i)       __builtin_powif((_x), (_i)) // float
#define POWIL(_x, _i)       __builtin_powil((_x), (_i)) // long double

//! TODO: BSWAP16 is overwritten in "effects.h"
// // bswap
// #define BSWAP16(_x)         __builtin_bswap16(_x)
// #define BSWAP32(_x)         __builtin_bswap32(_x)
// #define BSWAP64(_x)         __builtin_bswap64(_x)
// // #define BSWAP128(_x)        __builtin_bswap128(_x)

// MIPS exclusive:
// https://gcc.gnu.org/onlinedocs/gcc/Other-MIPS-Built-in-Functions.html

#if __GCC_HAVE_BUILTIN_MIPS_CACHE
// mips_cache (see pages 402-405 of vr4300 manual)
typedef enum __MipsCache_cache { // .
    CACHE_ICACHE, // Instruction cache.
    CACHE_DCACHE, // Data cache.
    CACHE_RESV_2,
    CACHE_RESV_3,
} __MIPSCache_cache;
typedef enum __MipsCache_ICacheOp {
    MIPS_ICACHE_Index_Invalidate            = 0,
    MIPS_ICACHE_Index_Load_Tag              = 1,
    MIPS_ICACHE_Index_Store_Tag             = 2,
    MIPS_ICACHE_Resv_3                      = 3,
    MIPS_ICACHE_Hit_Invalidate              = 4,
    MIPS_ICACHE_Fill                        = 5,
    MIPS_ICACHE_Hit_Write_Back              = 6,
    MIPS_ICACHE_Resv_7                      = 7,
} __MipsCache_ICacheOp;
typedef enum __MipsCache_DCacheOp {
    MIPS_DCACHE_Index_Write_Back_Invalidate = 0,
    MIPS_DCACHE_Index_Load_Tag              = 1,
    MIPS_DCACHE_Index_Store_Tag             = 2,
    MIPS_DCACHE_Resv_3                      = 3,
    MIPS_DCACHE_Hit_Invalidate              = 4,
    MIPS_DCACHE_Hit_Write_Back_Invalidate   = 5,
    MIPS_DCACHE_Hit_Write_Back              = 6,
    MIPS_DCACHE_Resv_7                      = 7,
} __MipsCache_DCacheOp;
#define CACHE(_op, _addr)                               __builtin_mips_cache((_op), (_addr))
#define CACHE_OP(_cache, _op, _addr)                    CACHE((((_op) << 2) | _cache), (_addr))
#define ICACHE_OP_Index_Invalidate(_addr)               CACHE_OP(CACHE_ICACHE, MIPS_ICACHE_Index_Invalidate,            (_addr))
#define ICACHE_OP_Index_Load_Tag(_addr)                 CACHE_OP(CACHE_ICACHE, MIPS_ICACHE_Index_Load_Tag,              (_addr))
#define ICACHE_OP_Index_Store_Tag(_addr)                CACHE_OP(CACHE_ICACHE, MIPS_ICACHE_Index_Store_Tag,             (_addr))
#define ICACHE_OP_Hit_Invalidate(_addr)                 CACHE_OP(CACHE_ICACHE, MIPS_ICACHE_Hit_Invalidate,              (_addr))
#define ICACHE_OP_Fill(_addr)                           CACHE_OP(CACHE_ICACHE, MIPS_ICACHE_Fill,                        (_addr))
#define ICACHE_OP_Hit_Write_Back(_addr)                 CACHE_OP(CACHE_ICACHE, MIPS_ICACHE_Hit_Write_Back,              (_addr))
#define DCACHE_OP_Index_Write_Back_Invalidate(_addr)    CACHE_OP(CACHE_ICACHE, MIPS_DCACHE_Index_Write_Back_Invalidate, (_addr))
#define DCACHE_OP_Index_Load_Tag(_addr)                 CACHE_OP(CACHE_ICACHE, MIPS_DCACHE_Index_Load_Tag,              (_addr))
#define DCACHE_OP_Index_Store_Tag(_addr)                CACHE_OP(CACHE_ICACHE, MIPS_DCACHE_Index_Store_Tag,             (_addr))
#define DCACHE_OP_Hit_Invalidate(_addr)                 CACHE_OP(CACHE_ICACHE, MIPS_DCACHE_Hit_Invalidate,              (_addr))
#define DCACHE_OP_Hit_Write_Back_Invalidate(_addr)      CACHE_OP(CACHE_ICACHE, MIPS_DCACHE_Hit_Write_Back_Invalidate,   (_addr))
#define DCACHE_OP_Hit_Write_Back(_addr)                 CACHE_OP(CACHE_ICACHE, MIPS_DCACHE_Hit_Write_Back,              (_addr))
#endif // __GCC_HAVE_BUILTIN_MIPS_CACHE

// mips fcsr
#define MIPS_GET_FCSR()         __builtin_mips_get_fcsr()
#define MIPS_SET_FCSR(_x)       __builtin_mips_set_fcsr(_x)
