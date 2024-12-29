/*
 * common_defs.h
 *
 * Copyright 2016 Eric Biggers
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef COMMON_DEFS_H
#define COMMON_DEFS_H

#include "libdeflate.h"

// HackerSM64 patch
#include <PR/ultratypes.h>
#define bool int
#define true 1
#define false 0
#define UINT32_MAX 0xFFFFFFFFU

/* ========================================================================== */
/*                             Target architecture                            */
/* ========================================================================== */

/* If possible, define a compiler-independent ARCH_* macro. */
#undef ARCH_X86_64
#undef ARCH_X86_32
#undef ARCH_ARM64
#undef ARCH_ARM32
#undef ARCH_RISCV

/* ========================================================================== */
/*                              Type definitions                              */
/* ========================================================================== */

/*
 * Word type of the target architecture.  Use 'size_t' instead of
 * 'unsigned long' to account for platforms such as Windows that use 32-bit
 * 'unsigned long' on 64-bit architectures.
 */
typedef size_t machine_word_t;

/* Number of bytes in a word */
#define WORDBYTES	((int)sizeof(machine_word_t))

/* Number of bits in a word */
#define WORDBITS	(8 * WORDBYTES)

/* ========================================================================== */
/*                         Optional compiler features                         */
/* ========================================================================== */

/* Compiler version checks.  Only use when absolutely necessary. */
#if defined(__GNUC__) && !defined(__clang__) && !defined(__INTEL_COMPILER)
#  define GCC_PREREQ(major, minor)		\
	(__GNUC__ > (major) ||			\
	 (__GNUC__ == (major) && __GNUC_MINOR__ >= (minor)))
#  if !GCC_PREREQ(4, 9)
#    error "gcc versions older than 4.9 are no longer supported"
#  endif
#else
#  define GCC_PREREQ(major, minor)	0
#endif
#ifdef __clang__
#  ifdef __apple_build_version__
#    define CLANG_PREREQ(major, minor, apple_version)	\
	(__apple_build_version__ >= (apple_version))
#  else
#    define CLANG_PREREQ(major, minor, apple_version)	\
	(__clang_major__ > (major) ||			\
	 (__clang_major__ == (major) && __clang_minor__ >= (minor)))
#  endif
#  if !CLANG_PREREQ(3, 9, 8000000)
#    error "clang versions older than 3.9 are no longer supported"
#  endif
#else
#  define CLANG_PREREQ(major, minor, apple_version)	0
#endif
#ifdef _MSC_VER
#  define MSVC_PREREQ(version)	(_MSC_VER >= (version))
#  if !MSVC_PREREQ(1900)
#    error "MSVC versions older than Visual Studio 2015 are no longer supported"
#  endif
#else
#  define MSVC_PREREQ(version)	0
#endif

/*
 * __has_attribute(attribute) - check whether the compiler supports the given
 * attribute (and also supports doing the check in the first place).  Mostly
 * useful just for clang, since gcc didn't add this macro until gcc 5.
 */
#ifndef __has_attribute
#  define __has_attribute(attribute)	0
#endif

/*
 * __has_builtin(builtin) - check whether the compiler supports the given
 * builtin (and also supports doing the check in the first place).  Mostly
 * useful just for clang, since gcc didn't add this macro until gcc 10.
 */
#ifndef __has_builtin
#  define __has_builtin(builtin)	0
#endif

/* inline - suggest that a function be inlined */
#ifdef _MSC_VER
#  define inline		__inline
#endif /* else assume 'inline' is usable as-is */

/* forceinline - force a function to be inlined, if possible */
#if defined(__GNUC__) || __has_attribute(always_inline)
#  define forceinline		inline __attribute__((always_inline))
#elif defined(_MSC_VER)
#  define forceinline		__forceinline
#else
#  define forceinline		inline
#endif

/* MAYBE_UNUSED - mark a function or variable as maybe unused */
#if defined(__GNUC__) || __has_attribute(unused)
#  define MAYBE_UNUSED		__attribute__((unused))
#else
#  define MAYBE_UNUSED
#endif

/* NORETURN - mark a function as never returning, e.g. due to calling abort() */
#if defined(__GNUC__) || __has_attribute(noreturn)
#  define NORETURN		__attribute__((noreturn))
#else
#  define NORETURN
#endif

/*
 * restrict - hint that writes only occur through the given pointer.
 *
 * Don't use MSVC's __restrict, since it has nonstandard behavior.
 * Standard restrict is okay, if it is supported.
 */
#if !defined(__STDC_VERSION__) || (__STDC_VERSION__ < 201112L)
#  if defined(__GNUC__) || defined(__clang__)
#    define restrict		__restrict__
#  else
#    define restrict
#  endif
#endif /* else assume 'restrict' is usable as-is */

/* likely(expr) - hint that an expression is usually true */
#if defined(__GNUC__) || __has_builtin(__builtin_expect)
#  define likely(expr)		__builtin_expect(!!(expr), 1)
#else
#  define likely(expr)		(expr)
#endif

/* unlikely(expr) - hint that an expression is usually false */
#if defined(__GNUC__) || __has_builtin(__builtin_expect)
#  define unlikely(expr)	__builtin_expect(!!(expr), 0)
#else
#  define unlikely(expr)	(expr)
#endif

/* prefetchr(addr) - prefetch into L1 cache for read */
#undef prefetchr
#if defined(__GNUC__) || __has_builtin(__builtin_prefetch)
#  define prefetchr(addr)	__builtin_prefetch((addr), 0)
#elif defined(_MSC_VER)
#  if defined(ARCH_X86_32) || defined(ARCH_X86_64)
#    define prefetchr(addr)	_mm_prefetch((addr), _MM_HINT_T0)
#  elif defined(ARCH_ARM64)
#    define prefetchr(addr)	__prefetch2((addr), 0x00 /* prfop=PLDL1KEEP */)
#  elif defined(ARCH_ARM32)
#    define prefetchr(addr)	__prefetch(addr)
#  endif
#endif
#ifndef prefetchr
#  define prefetchr(addr)
#endif

/* prefetchw(addr) - prefetch into L1 cache for write */
#undef prefetchw
#if defined(__GNUC__) || __has_builtin(__builtin_prefetch)
#  define prefetchw(addr)	__builtin_prefetch((addr), 1)
#elif defined(_MSC_VER)
#  if defined(ARCH_X86_32) || defined(ARCH_X86_64)
#    define prefetchw(addr)	_m_prefetchw(addr)
#  elif defined(ARCH_ARM64)
#    define prefetchw(addr)	__prefetch2((addr), 0x10 /* prfop=PSTL1KEEP */)
#  elif defined(ARCH_ARM32)
#    define prefetchw(addr)	__prefetchw(addr)
#  endif
#endif
#ifndef prefetchw
#  define prefetchw(addr)
#endif

/*
 * _aligned_attribute(n) - declare that the annotated variable, or variables of
 * the annotated type, must be aligned on n-byte boundaries.
 */
#undef _aligned_attribute
#if defined(__GNUC__) || __has_attribute(aligned)
#  define _aligned_attribute(n)	__attribute__((aligned(n)))
#elif defined(_MSC_VER)
#  define _aligned_attribute(n)	__declspec(align(n))
#endif

/*
 * _target_attribute(attrs) - override the compilation target for a function.
 *
 * This accepts one or more comma-separated suffixes to the -m prefix jointly
 * forming the name of a machine-dependent option.  On gcc-like compilers, this
 * enables codegen for the given targets, including arbitrary compiler-generated
 * code as well as the corresponding intrinsics.  On other compilers this macro
 * expands to nothing, though MSVC allows intrinsics to be used anywhere anyway.
 */
#if defined(__GNUC__) || __has_attribute(target)
#  define _target_attribute(attrs)	__attribute__((target(attrs)))
#else
#  define _target_attribute(attrs)
#endif

/* ========================================================================== */
/*                          Miscellaneous macros                              */
/* ========================================================================== */

#define ARRAY_LEN(A)		(sizeof(A) / sizeof((A)[0]))
#define MIN(a, b)		((a) <= (b) ? (a) : (b))
#define MAX(a, b)		((a) >= (b) ? (a) : (b))
#define DIV_ROUND_UP(n, d)	(((n) + (d) - 1) / (d))
#define STATIC_ASSERT(expr)	((void)sizeof(char[1 - 2 * !(expr)]))
#define ALIGN(n, a)		(((n) + (a) - 1) & ~((a) - 1))
#define ROUND_UP(n, d)		((d) * DIV_ROUND_UP((n), (d)))

/* ========================================================================== */
/*                           Endianness handling                              */
/* ========================================================================== */

/*
 * CPU_IS_LITTLE_ENDIAN() - 1 if the CPU is little endian, or 0 if it is big
 * endian.  When possible this is a compile-time macro that can be used in
 * preprocessor conditionals.  As a fallback, a generic method is used that
 * can't be used in preprocessor conditionals but should still be optimized out.
 */
// HackerSM64 patch - ensure that the endianness is big endian for MIPS
#define CPU_IS_LITTLE_ENDIAN() 0

/* bswap16(v) - swap the bytes of a 16-bit integer */
static forceinline u16 bswap16(u16 v)
{
#if defined(__GNUC__) || __has_builtin(__builtin_bswap16)
	return __builtin_bswap16(v);
#elif defined(_MSC_VER)
	return _byteswap_ushort(v);
#else
	return (v << 8) | (v >> 8);
#endif
}

/* bswap32(v) - swap the bytes of a 32-bit integer */
static forceinline u32 bswap32(u32 v)
{
#if defined(__GNUC__) || __has_builtin(__builtin_bswap32)
	return __builtin_bswap32(v);
#elif defined(_MSC_VER)
	return _byteswap_ulong(v);
#else
	return ((v & 0x000000FF) << 24) |
	       ((v & 0x0000FF00) << 8) |
	       ((v & 0x00FF0000) >> 8) |
	       ((v & 0xFF000000) >> 24);
#endif
}

/* bswap64(v) - swap the bytes of a 64-bit integer */
static forceinline u64 bswap64(u64 v)
{
#if defined(__GNUC__) || __has_builtin(__builtin_bswap64)
	return __builtin_bswap64(v);
#elif defined(_MSC_VER)
	return _byteswap_uint64(v);
#else
	return ((v & 0x00000000000000FF) << 56) |
	       ((v & 0x000000000000FF00) << 40) |
	       ((v & 0x0000000000FF0000) << 24) |
	       ((v & 0x00000000FF000000) << 8) |
	       ((v & 0x000000FF00000000) >> 8) |
	       ((v & 0x0000FF0000000000) >> 24) |
	       ((v & 0x00FF000000000000) >> 40) |
	       ((v & 0xFF00000000000000) >> 56);
#endif
}

#define le16_bswap(v) (CPU_IS_LITTLE_ENDIAN() ? (v) : bswap16(v))
#define le32_bswap(v) (CPU_IS_LITTLE_ENDIAN() ? (v) : bswap32(v))
#define le64_bswap(v) (CPU_IS_LITTLE_ENDIAN() ? (v) : bswap64(v))
#define be16_bswap(v) (CPU_IS_LITTLE_ENDIAN() ? bswap16(v) : (v))
#define be32_bswap(v) (CPU_IS_LITTLE_ENDIAN() ? bswap32(v) : (v))
#define be64_bswap(v) (CPU_IS_LITTLE_ENDIAN() ? bswap64(v) : (v))

/* ========================================================================== */
/*                          Unaligned memory accesses                         */
/* ========================================================================== */

// Taken from Linux Kernel, HackerSM64 patch
#define PACKED __attribute__((packed))
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

#define UNALIGNED_ACCESS_IS_FAST 1

/* Unaligned loads and stores without endianness conversion */
// HackerSM64 patch to ensure correct compilation for N64

#define DEFINE_UNALIGNED_TYPE(type)				\
static forceinline type						\
load_##type##_unaligned(const void *p)				\
{								\
	return __GET_UNALIGNED_T(type, p);			\
}								\
								\
static forceinline void						\
store_##type##_unaligned(type v, void *p)			\
{								\
	__PUT_UNALIGNED_T(type, v, p); \
}

DEFINE_UNALIGNED_TYPE(u16)
DEFINE_UNALIGNED_TYPE(u32)
DEFINE_UNALIGNED_TYPE(u64)
DEFINE_UNALIGNED_TYPE(machine_word_t)

#define load_word_unaligned	load_machine_word_t_unaligned
#define store_word_unaligned	store_machine_word_t_unaligned

/* Unaligned loads with endianness conversion */

static forceinline u16
get_unaligned_le16(const u8 *p)
{
	if (UNALIGNED_ACCESS_IS_FAST)
		return le16_bswap(load_u16_unaligned(p));
	else
		return ((u16)p[1] << 8) | p[0];
}

static forceinline u16
get_unaligned_be16(const u8 *p)
{
	if (UNALIGNED_ACCESS_IS_FAST)
		return be16_bswap(load_u16_unaligned(p));
	else
		return ((u16)p[0] << 8) | p[1];
}

static forceinline u32
get_unaligned_le32(const u8 *p)
{
	if (UNALIGNED_ACCESS_IS_FAST)
		return le32_bswap(load_u32_unaligned(p));
	else
		return ((u32)p[3] << 24) | ((u32)p[2] << 16) |
			((u32)p[1] << 8) | p[0];
}

static forceinline u32
get_unaligned_be32(const u8 *p)
{
	if (UNALIGNED_ACCESS_IS_FAST)
		return be32_bswap(load_u32_unaligned(p));
	else
		return ((u32)p[0] << 24) | ((u32)p[1] << 16) |
			((u32)p[2] << 8) | p[3];
}

static forceinline u64
get_unaligned_le64(const u8 *p)
{
	if (UNALIGNED_ACCESS_IS_FAST)
		return le64_bswap(load_u64_unaligned(p));
	else
		return ((u64)p[7] << 56) | ((u64)p[6] << 48) |
			((u64)p[5] << 40) | ((u64)p[4] << 32) |
			((u64)p[3] << 24) | ((u64)p[2] << 16) |
			((u64)p[1] << 8) | p[0];
}

static forceinline machine_word_t
get_unaligned_leword(const u8 *p)
{
	STATIC_ASSERT(WORDBITS == 32 || WORDBITS == 64);
	if (WORDBITS == 32)
		return get_unaligned_le32(p);
	else
		return get_unaligned_le64(p);
}

/* Unaligned stores with endianness conversion */

static forceinline void
put_unaligned_le16(u16 v, u8 *p)
{
	if (UNALIGNED_ACCESS_IS_FAST) {
		store_u16_unaligned(le16_bswap(v), p);
	} else {
		p[0] = (u8)(v >> 0);
		p[1] = (u8)(v >> 8);
	}
}

static forceinline void
put_unaligned_be16(u16 v, u8 *p)
{
	if (UNALIGNED_ACCESS_IS_FAST) {
		store_u16_unaligned(be16_bswap(v), p);
	} else {
		p[0] = (u8)(v >> 8);
		p[1] = (u8)(v >> 0);
	}
}

static forceinline void
put_unaligned_le32(u32 v, u8 *p)
{
	if (UNALIGNED_ACCESS_IS_FAST) {
		store_u32_unaligned(le32_bswap(v), p);
	} else {
		p[0] = (u8)(v >> 0);
		p[1] = (u8)(v >> 8);
		p[2] = (u8)(v >> 16);
		p[3] = (u8)(v >> 24);
	}
}

static forceinline void
put_unaligned_be32(u32 v, u8 *p)
{
	if (UNALIGNED_ACCESS_IS_FAST) {
		store_u32_unaligned(be32_bswap(v), p);
	} else {
		p[0] = (u8)(v >> 24);
		p[1] = (u8)(v >> 16);
		p[2] = (u8)(v >> 8);
		p[3] = (u8)(v >> 0);
	}
}

static forceinline void
put_unaligned_le64(u64 v, u8 *p)
{
	if (UNALIGNED_ACCESS_IS_FAST) {
		store_u64_unaligned(le64_bswap(v), p);
	} else {
		p[0] = (u8)(v >> 0);
		p[1] = (u8)(v >> 8);
		p[2] = (u8)(v >> 16);
		p[3] = (u8)(v >> 24);
		p[4] = (u8)(v >> 32);
		p[5] = (u8)(v >> 40);
		p[6] = (u8)(v >> 48);
		p[7] = (u8)(v >> 56);
	}
}

static forceinline void
put_unaligned_leword(machine_word_t v, u8 *p)
{
	STATIC_ASSERT(WORDBITS == 32 || WORDBITS == 64);
	if (WORDBITS == 32)
		put_unaligned_le32(v, p);
	else
		put_unaligned_le64(v, p);
}

/* ========================================================================== */
/*                         Bit manipulation functions                         */
/* ========================================================================== */

/*
 * Bit Scan Reverse (BSR) - find the 0-based index (relative to the least
 * significant end) of the *most* significant 1 bit in the input value.  The
 * input value must be nonzero!
 */

static forceinline unsigned
bsr32(u32 v)
{
#if defined(__GNUC__) || __has_builtin(__builtin_clz)
	return 31 - __builtin_clz(v);
#elif defined(_MSC_VER)
	unsigned long i;

	_BitScanReverse(&i, v);
	return i;
#else
	unsigned i = 0;

	while ((v >>= 1) != 0)
		i++;
	return i;
#endif
}

static forceinline unsigned
bsr64(u64 v)
{
#if defined(__GNUC__) || __has_builtin(__builtin_clzll)
	return 63 - __builtin_clzll(v);
#elif defined(_MSC_VER) && defined(_WIN64)
	unsigned long i;

	_BitScanReverse64(&i, v);
	return i;
#else
	unsigned i = 0;

	while ((v >>= 1) != 0)
		i++;
	return i;
#endif
}

static forceinline unsigned
bsrw(machine_word_t v)
{
	STATIC_ASSERT(WORDBITS == 32 || WORDBITS == 64);
	if (WORDBITS == 32)
		return bsr32(v);
	else
		return bsr64(v);
}

/*
 * Bit Scan Forward (BSF) - find the 0-based index (relative to the least
 * significant end) of the *least* significant 1 bit in the input value.  The
 * input value must be nonzero!
 */

static forceinline unsigned
bsf32(u32 v)
{
#if defined(__GNUC__) || __has_builtin(__builtin_ctz)
	return __builtin_ctz(v);
#elif defined(_MSC_VER)
	unsigned long i;

	_BitScanForward(&i, v);
	return i;
#else
	unsigned i = 0;

	for (; (v & 1) == 0; v >>= 1)
		i++;
	return i;
#endif
}

static forceinline unsigned
bsf64(u64 v)
{
#if defined(__GNUC__) || __has_builtin(__builtin_ctzll)
	return __builtin_ctzll(v);
#elif defined(_MSC_VER) && defined(_WIN64)
	unsigned long i;

	_BitScanForward64(&i, v);
	return i;
#else
	unsigned i = 0;

	for (; (v & 1) == 0; v >>= 1)
		i++;
	return i;
#endif
}

static forceinline unsigned
bsfw(machine_word_t v)
{
	STATIC_ASSERT(WORDBITS == 32 || WORDBITS == 64);
	if (WORDBITS == 32)
		return bsf32(v);
	else
		return bsf64(v);
}

/*
 * rbit32(v): reverse the bits in a 32-bit integer.  This doesn't have a
 * fallback implementation; use '#ifdef rbit32' to check if this is available.
 */
#undef rbit32
#if (defined(__GNUC__) || defined(__clang__)) && defined(ARCH_ARM32) && \
	(__ARM_ARCH >= 7 || (__ARM_ARCH == 6 && defined(__ARM_ARCH_6T2__)))
static forceinline u32
rbit32(u32 v)
{
	__asm__("rbit %0, %1" : "=r" (v) : "r" (v));
	return v;
}
#define rbit32 rbit32
#elif (defined(__GNUC__) || defined(__clang__)) && defined(ARCH_ARM64)
static forceinline u32
rbit32(u32 v)
{
	__asm__("rbit %w0, %w1" : "=r" (v) : "r" (v));
	return v;
}
#define rbit32 rbit32
#endif

#endif /* COMMON_DEFS_H */
