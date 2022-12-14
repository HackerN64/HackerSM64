#ifndef MATH_UTIL_H
#define MATH_UTIL_H

#include <PR/ultratypes.h>

#include "types.h"

#define NEAR_ZERO   __FLT_EPSILON__
#define NEAR_ONE    (1.0f - __FLT_EPSILON__)

#define FLOAT_ONE   0x3F800000

extern Vec3f gVec3fX;
extern Vec3f gVec3fY;
extern Vec3f gVec3fZ;
extern Vec3f gVec3fNX;
extern Vec3f gVec3fNY;
extern Vec3f gVec3fNZ;
extern Vec3f gVec3fZero;
extern Vec3s gVec3sZero;
extern Vec3i gVec3iZero;
extern Vec3f gVec3fOne;
extern Vec3s gVec3sOne;

/**
 * Converts an angle in degrees to sm64's s16 angle units. For example, DEGREES(90) == 0x4000
 * This should be used mainly to make camera code clearer at first glance.
 */
// #define DEGREES(x) ((x) * 0x10000 / 360)
#define DEGREES(x) ((x) * 0x2000 / 45)
// #define DEGREES(x) (((x) << 13) / 45)

/*
 * The sine and cosine tables overlap, but "#define gCosineTable (gSineTable +
 * 0x400)" doesn't give expected codegen; gSineTable and gCosineTable need to
 * be different symbols for code to match. Most likely the tables were placed
 * adjacent to each other, and gSineTable cut short, such that reads overflow
 * into gCosineTable.
 *
 * These kinds of out of bounds reads are undefined behavior, and break on
 * e.g. GCC (which doesn't place the tables next to each other, and probably
 * exploits array sizes for range analysis-based optimizations as well).
 * Thus, for non-IDO compilers we use the standard-compliant version.
 */
extern f32 gSineTable[];
#define gCosineTable (gSineTable + 0x400)

#define sins(x) gSineTable[  (u16) (x) >> 4]
#define coss(x) gCosineTable[(u16) (x) >> 4]
#define tans(x) (sins(x) / coss(x))
#define cots(x) (coss(x) / sins(x))
#define atans(x) gArctanTable[(s32)((((x) * 1024) + 0.5f))] // is this correct? used for atan2_lookup

#define RAD_PER_DEG (M_PI / 180.0f)
#define DEG_PER_RAD (180.0f / M_PI)

#define angle_to_degrees(  x) (f32)(((Angle)(x) / 65536.0f) * 360.0f)
#define degrees_to_angle(  x) (Angle)(((f32)(x) * 0x10000 ) / 360   )
#define angle_to_radians(  x) (f32)(((Angle)(x) * M_PI    ) / 0x8000)
#define radians_to_angle(  x) (Angle)(((f32)(x) / M_PI    ) * 0x8000)
#define degrees_to_radians(x) (f32)(   (f32)(x) * RAD_PER_DEG       )
#define radians_to_degrees(x) (f32)(   (f32)(x) * DEG_PER_RAD       )

#define signum_positive(x) ((x < 0) ? -1 : 1)

// #define min(a, b) MIN((a), (b)) // ((a) < (b) ? (a) : (b))
// #define max(a, b) MAX((a), (b)) // ((a) > (b) ? (a) : (b))
#define CLAMP(x, low, high)  (((x) > (high)) ? (high) : (((x) < (low)) ? (low) : (x)))

// from limits.h
#define S8_MAX __SCHAR_MAX__
#define S8_MIN (-S8_MAX - 1)
#define U8_MAX (S8_MAX * 2 + 1)
#define S16_MAX __SHRT_MAX__
#define S16_MIN (-S16_MAX - 1)
#define U16_MAX (S16_MAX * 2 + 1)
#define S32_MAX __INT_MAX__
#define S32_MIN (-S32_MAX - 1)
#define U32_MAX (S32_MAX * 2U + 1U)
#define S64_MAX __LONG_LONG_MAX__
#define S64_MIN (-S64_MAX - 1LL)
#define U64_MAX (S64_MAX * 2ULL + 1ULL)
#define F32_MAX __FLT_MAX__
#define F32_MIN __FLT_MIN__
#define F64_MAX __DBL_MAX__
#define F64_MIN __DBL_MIN__

#define CLAMP_U8( x)        CLAMP((x),     0x0,  U8_MAX)
#define CLAMP_S8( x)        CLAMP((x),  S8_MIN,  S8_MAX)
#define CLAMP_U16(x)        CLAMP((x),     0x0, U16_MAX)
#define CLAMP_S16(x)        CLAMP((x), S16_MIN, S16_MAX)
#define CLAMP_U32(x)        CLAMP((x),     0x0, U32_MAX)
#define CLAMP_S32(x)        CLAMP((x), S32_MIN, S32_MAX)
#define CLAMP_U64(x)        CLAMP((x),     0x0, U64_MAX)
#define CLAMP_S64(x)        CLAMP((x), S64_MIN, S64_MAX)
#define CLAMP_F32(x)        CLAMP((x), F32_MIN, F32_MAX)
#define CLAMP_F64(x)        CLAMP((x), F64_MIN, F64_MAX)

#define SWAP(a, b)          { ((a) ^= (b)); ((b) ^= (a)); ((a) ^= (b)); }

#define sqr(x)              (    (x) * (x))
#define cube(x)             ( sqr(x) * (x))
#define quad(x)             (cube(x) * (x))

#define average_2(a, b      )   (((a) + (b)            ) / 2.0f)
#define average_3(a, b, c   )   (((a) + (b) + (c)      ) / 3.0f)
#define average_4(a, b, c, d)   (((a) + (b) + (c) + (d)) / 4.0f)

#define vec2_same(v, s)     (((v)[0]) = ((v)[1])                       = (s))
#define vec3_same(v, s)     (((v)[0]) = ((v)[1]) = ((v)[2])            = (s))
#define vec4_same(v, s)     (((v)[0]) = ((v)[1]) = ((v)[2]) = ((v)[3]) = (s))

#define vec2_zero(v)        (vec2_same((v), 0))
#define vec3_zero(v)        (vec3_same((v), 0))
#define vec4_zero(v)        (vec4_same((v), 0))

#define vec2_c(v)           (   (v)[0] + (v)[1])
#define vec3_c(v)           (vec2_c(v) + (v)[2])
#define vec4_c(v)           (vec3_c(v) + (v)[3])

#define vec2_average(v)     (vec2_c(v) / 2.0f)
#define vec3_average(v)     (vec3_c(v) / 3.0f)
#define vec4_average(v)     (vec4_c(v) / 4.0f)

#define vec2_sumsq(v)       (  sqr((v)[0]) + sqr((v)[1]))
#define vec3_sumsq(v)       (vec2_sumsq(v) + sqr((v)[2]))
#define vec4_sumsq(v)       (vec3_sumsq(v) + sqr((v)[3]))

#define vec2_mag(v)         (sqrtf(vec2_sumsq(v)))
#define vec3_mag(v)         (sqrtf(vec3_sumsq(v)))
#define vec4_mag(v)         (sqrtf(vec4_sumsq(v)))

#define vec3_yaw(from, to)  (atan2s(((to)[2] - (from)[2]), ((to)[0] - (from)[0])))

#define vec2_dot(a, b)       (((a)[0] * (b)[0]) + ((a)[1] * (b)[1]))
#define vec3_dot(a, b)      (vec2_dot((a), (b)) + ((a)[2] * (b)[2]))
#define vec4_dot(a, b)      (vec3_dot((a), (b)) + ((a)[3] * (b)[3]))

/// Make vector 'dest' the cross product of vectors a and b.
#define vec3_cross(dst, a, b) {                         \
    (dst)[0] = (((a)[1] * (b)[2]) - ((a)[2] * (b)[1])); \
    (dst)[1] = (((a)[2] * (b)[0]) - ((a)[0] * (b)[2])); \
    (dst)[2] = (((a)[0] * (b)[1]) - ((a)[1] * (b)[0])); \
}

/**
 * Set 'dest' the normal vector of a triangle with vertices a, b and c.
 * Equivalent to cross((c-b), (c-a)).
 */
#define find_vector_perpendicular_to_plane(dest, a, b, c) {                                     \
    (dest)[0] = ((b)[1] - (a)[1]) * ((c)[2] - (b)[2]) - ((c)[1] - (b)[1]) * ((b)[2] - (a)[2]);  \
    (dest)[1] = ((b)[2] - (a)[2]) * ((c)[0] - (b)[0]) - ((c)[2] - (b)[2]) * ((b)[0] - (a)[0]);  \
    (dest)[2] = ((b)[0] - (a)[0]) * ((c)[1] - (b)[1]) - ((c)[0] - (b)[0]) * ((b)[1] - (a)[1]);  \
}

/**
 * | ? ? ? 0 |
 * | ? ? ? 0 |
 * | ? ? ? 0 |
 * | 0 0 0 1 |
 * i.e. a matrix representing a linear transformation over 3 space.
 */
// Multiply a vector by a matrix of the form
#define linear_mtxf_mul_vec3(mtx, dstV, srcV) {                                                     \
    (dstV)[0] = (((mtx)[0][0] * (srcV)[0]) + ((mtx)[1][0] * (srcV)[1]) + ((mtx)[2][0] * (srcV)[2]));\
    (dstV)[1] = (((mtx)[0][1] * (srcV)[0]) + ((mtx)[1][1] * (srcV)[1]) + ((mtx)[2][1] * (srcV)[2]));\
    (dstV)[2] = (((mtx)[0][2] * (srcV)[0]) + ((mtx)[1][2] * (srcV)[1]) + ((mtx)[2][2] * (srcV)[2]));\
}

#define linear_mtxf_mul_vec3_and_translate(mtx, dstV, srcV) {   \
    linear_mtxf_mul_vec3((mtx), (dstV), (srcV));                \
    vec3_add((dstV), (mtx)[3]);                                 \
}

// Multiply a vector by the transpose of a matrix of the form
#define linear_mtxf_transpose_mul_vec3(mtx, dstV, srcV) {   \
    (dstV)[0] = vec3_dot((mtx)[0], (srcV));                 \
    (dstV)[1] = vec3_dot((mtx)[1], (srcV));                 \
    (dstV)[2] = vec3_dot((mtx)[2], (srcV));                 \
}

#define vec2_set(dst, x, y) {           \
    (dst)[0] = (x);                     \
    (dst)[1] = (y);                     \
}
#define vec3_set(dst, x, y, z) {        \
    vec2_set((dst), (x), (y));          \
    (dst)[2] = (z);                     \
}
#define vec4_set(dst, x, y, z, w) {     \
    vec3_set((dst), (x), (y), (z));     \
    (dst)[3] = (w);                     \
}

#define vec2_copy(dst, src) {           \
    (dst)[0] = (src)[0];                \
    (dst)[1] = (src)[1];                \
}
#define vec3_copy(dst, src) {           \
    vec2_copy((dst), (src));            \
    (dst)[2] = (src)[2];                \
}
#define vec4_copy(dst, src) {           \
    vec3_copy((dst), (src));            \
    (dst)[3] = (src)[3];                \
}

#define vec3_copy_y_off(dst, src, y) {  \
    (dst)[0] =  (src)[0];               \
    (dst)[1] = ((src)[1] + (y));        \
    (dst)[2] =  (src)[2];               \
}

#define vec2_copy_roundf(dst, src) {    \
    (dst)[0] = roundf((src)[0]);        \
    (dst)[1] = roundf((src)[1]);        \
}
#define vec3_copy_roundf(dst, src) {    \
    vec2_copy_roundf((dst), (src));     \
    (dst)[2] = roundf((src)[2]);        \
}
#define vec4_copy_roundf(dst, src) {    \
    vec3_copy_roundf((dst), (src));     \
    (dst)[3] = roundf((src)[3]);        \
}

#define vec2_copy_inverse(dst, src) {   \
    (dst)[0] = (src)[1];                \
    (dst)[1] = (src)[0];                \
}
#define vec3_copy_inverse(dst, src) {   \
    (dst)[0] = (src)[2];                \
    (dst)[1] = (src)[1];                \
    (dst)[2] = (src)[0];                \
}
#define vec4_copy_inverse(dst, src) {   \
    (dst)[0] = (src)[3];                \
    (dst)[1] = (src)[2];                \
    (dst)[2] = (src)[1];                \
    (dst)[3] = (src)[0];                \
}

#define vec3_copy_offset_m1(dst, src) { \
    (dst)[0] = (src)[1];                \
    (dst)[1] = (src)[2];                \
    (dst)[2] = (src)[0];                \
}

#define vec2_copy_negative(dst, src) {  \
    (dst)[0] = -(src)[0];               \
    (dst)[1] = -(src)[1];               \
}
#define vec3_copy_negative(dst, src) {  \
    vec2_copy_negative((dst), (src));   \
    (dst)[2] = -(src)[2];               \
}
#define vec4_copy_negative(dst, src) {  \
    vec3_copy_negative((dst), (src));   \
    (dst)[3] = -(src)[3];               \
}

#define vec2_sum(dst, src1, src2) {     \
    (dst)[0] = ((src1)[0] + (src2)[0]); \
    (dst)[1] = ((src1)[1] + (src2)[1]); \
}
#define vec3_sum(dst, src1, src2) {     \
    vec2_sum((dst), (src1), (src2));    \
    (dst)[2] = ((src1)[2] + (src2)[2]); \
}
#define vec4_sum(dst, src1, src2) {     \
    vec3_sum((dst), (src1), (src2));    \
    (dst)[3] = ((src1)[3] + (src2)[3]); \
}

#define vec2_add(dst, src) vec2_sum((dst), (dst), (src))
#define vec3_add(dst, src) vec3_sum((dst), (dst), (src))
#define vec4_add(dst, src) vec4_sum((dst), (dst), (src))

#define vec2_sum_val(dst, src, x) {     \
    (dst)[0] = ((src)[0] + (x));        \
    (dst)[1] = ((src)[1] + (x));        \
}
#define vec3_sum_val(dst, src, x) {     \
    vec2_sum_val((dst), (src), (x));    \
    (dst)[2] = ((src)[2] + (x));        \
}
#define vec4_sum_val(dst, src, x) {     \
    vec3_sum_val((dst), (src), (x));    \
    (dst)[3] = ((src)[2] + (x));        \
}

#define vec2_add_val(dst, x) vec2_sum_val((dst), (dst), (x))
#define vec3_add_val(dst, x) vec3_sum_val((dst), (dst), (x))
#define vec4_add_val(dst, x) vec4_sum_val((dst), (dst), (x))

#define vec2_diff(dst, src1, src2) {    \
    (dst)[0] = ((src1)[0] - (src2)[0]); \
    (dst)[1] = ((src1)[1] - (src2)[1]); \
}
#define vec3_diff(dst, src1, src2) {    \
    vec2_diff((dst), (src1), (src2));   \
    (dst)[2] = ((src1)[2] - (src2)[2]); \
}
#define vec4_diff(dst, src1, src2) {    \
    vec3_diff((dst), (src1), (src2));   \
    (dst)[3] = ((src1)[3] - (src2)[3]); \
}

#define vec2_sub(dst, src) vec2_diff((dst), (dst), (src))
#define vec3_sub(dst, src) vec3_diff((dst), (dst), (src))
#define vec4_sub(dst, src) vec4_diff((dst), (dst), (src))

#define vec2_diff_val(dst, src, x) {    \
    (dst)[0] = ((src)[0] - (x));        \
    (dst)[1] = ((src)[1] - (x));        \
}
#define vec3_diff_val(dst, src, x) {    \
    vec2_diff_val((dst), (src), (x));   \
    (dst)[2] = ((src)[2] - (x));        \
}
#define vec4_diff_val(dst, src, x) {    \
    vec3_diff_val((dst), (src), (x));   \
    (dst)[3] = ((src)[3] - (x));        \
}

#define vec2_sub_val(dst, x) vec2_diff_val((dst), (dst), (x))
#define vec3_sub_val(dst, x) vec3_diff_val((dst), (dst), (x))
#define vec4_sub_val(dst, x) vec4_diff_val((dst), (dst), (x))

#define vec2_prod(dst, src1, src2) {    \
    (dst)[0] = ((src1)[0] * (src2)[0]); \
    (dst)[1] = ((src1)[1] * (src2)[1]); \
}
#define vec3_prod(dst, src1, src2) {    \
    vec2_prod((dst), (src1), (src2));   \
    (dst)[2] = ((src1)[2] * (src2)[2]); \
}
#define vec4_prod(dst, src1, src2) {    \
    vec3_prod((dst), (src1), (src2));   \
    (dst)[3] = ((src1)[3] * (src2)[3]); \
}

#define vec2_mul(dst, src) vec2_prod((dst), (dst), (src))
#define vec3_mul(dst, src) vec3_prod((dst), (dst), (src))
#define vec4_mul(dst, src) vec4_prod((dst), (dst), (src))

#define vec2_prod_val(dst, src, x) {    \
    (dst)[0] = ((src)[0] * (x));        \
    (dst)[1] = ((src)[1] * (x));        \
}
#define vec3_prod_val(dst, src, x) {    \
    vec2_prod_val((dst), (src), (x));   \
    (dst)[2] = ((src)[2] * (x));        \
}
#define vec4_prod_val(dst, src, x) {    \
    vec3_prod_val((dst), (src), (x));   \
    (dst)[3] = ((src)[3] * (x));        \
}

#define vec2_mul_val(dst, x) vec2_prod_val(dst, dst, x)
#define vec3_mul_val(dst, x) vec3_prod_val(dst, dst, x)
#define vec4_mul_val(dst, x) vec4_prod_val(dst, dst, x)

#define vec2_quot(dst, src1, src2) {    \
    (dst)[0] = ((src1)[0] / (src2)[0]); \
    (dst)[1] = ((src1)[1] / (src2)[1]); \
}
#define vec3_quot(dst, src1, src2) {    \
    vec2_quot((dst), (src1), (src2));   \
    (dst)[2] = ((src1)[2] / (src2)[2]); \
}
#define vec4_quot(dst, src1, src2) {    \
    vec3_quot((dst), (src1), (src2));   \
    (dst)[3] = ((src1)[3] / (src2)[3]); \
}

#define vec2_div(dst, src) vec2_quot((dst), (dst), (src))
#define vec3_div(dst, src) vec3_quot((dst), (dst), (src))
#define vec4_div(dst, src) vec4_quot((dst), (dst), (src))

#define vec2_quot_val(dst, src, x) {    \
    (dst)[0] = ((src)[0] / (x));        \
    (dst)[1] = ((src)[1] / (x));        \
}
#define vec3_quot_val(dst, src, x) {    \
    vec2_quot_val((dst), (src), (x));   \
    (dst)[2] = ((src)[2] / (x));        \
}
#define vec4_quot_val(dst, src, x) {    \
    vec3_quot_val((dst), (src), (x));   \
    (dst)[3] = ((src)[3] / (x));        \
}

#define vec2_div_val(dst, x) vec2_quot_val((dst), (dst), (x))
#define vec3_div_val(dst, x) vec3_quot_val((dst), (dst), (x))
#define vec4_div_val(dst, x) vec4_quot_val((dst), (dst), (x))

#define MAT4_VEC_DOT_PROD(R, A, B, row, col) {              \
    (R)[(row)][(col)]  = ((A)[(row)][0] * (B)[0][(col)]);   \
    (R)[(row)][(col)] += ((A)[(row)][1] * (B)[1][(col)]);   \
    (R)[(row)][(col)] += ((A)[(row)][2] * (B)[2][(col)]);   \
}
#define MAT4_DOT_PROD(R, A, B, row, col) {                  \
    (R)[(row)][(col)]  = ((A)[(row)][0] * (B)[0][(col)]);   \
    (R)[(row)][(col)] += ((A)[(row)][1] * (B)[1][(col)]);   \
    (R)[(row)][(col)] += ((A)[(row)][2] * (B)[2][(col)]);   \
    (R)[(row)][(col)] += ((A)[(row)][3] * (B)[3][(col)]);   \
}

#define MAT4_MULTIPLY(R, A, B) {        \
    MAT4_DOT_PROD((R), (A), (B), 0, 0); \
    MAT4_DOT_PROD((R), (A), (B), 0, 1); \
    MAT4_DOT_PROD((R), (A), (B), 0, 2); \
    MAT4_DOT_PROD((R), (A), (B), 0, 3); \
    MAT4_DOT_PROD((R), (A), (B), 1, 0); \
    MAT4_DOT_PROD((R), (A), (B), 1, 1); \
    MAT4_DOT_PROD((R), (A), (B), 1, 2); \
    MAT4_DOT_PROD((R), (A), (B), 1, 3); \
    MAT4_DOT_PROD((R), (A), (B), 2, 0); \
    MAT4_DOT_PROD((R), (A), (B), 2, 1); \
    MAT4_DOT_PROD((R), (A), (B), 2, 2); \
    MAT4_DOT_PROD((R), (A), (B), 2, 3); \
    MAT4_DOT_PROD((R), (A), (B), 3, 0); \
    MAT4_DOT_PROD((R), (A), (B), 3, 1); \
    MAT4_DOT_PROD((R), (A), (B), 3, 2); \
    MAT4_DOT_PROD((R), (A), (B), 3, 3); \
}

#define MTXF_END(mtx) {                         \
    (mtx)[0][3] = (mtx)[1][3] = (mtx)[2][3] = 0;\
    ((u32 *)(mtx))[15] = FLOAT_ONE;             \
}

#define NAME_INVMAG(v) v##_invmag

/// Scale vector 'v' so it has length 1
#define vec3_normalize(v) {                                     \
    register f32 NAME_INVMAG(v) = vec3_mag((v));                \
    NAME_INVMAG(v) = (1.0f / MAX(NAME_INVMAG(v), NEAR_ZERO));   \
    vec3_mul_val((v), NAME_INVMAG(v));                          \
}

#define vec3_normalize_max(v, max) {    \
    register f32 v##_mag = vec3_mag(v); \
    v##_mag = MAX(v##_mag, NEAR_ZERO);  \
    if (v##_mag > max) {                \
        v##_mag = (max / v##_mag);      \
        vec3_mul_val(v, v##_mag);       \
    }                                   \
}

#define ABS(x)  (((x) > 0) ? (x) : -(x))

/// From Wiseguy
ALWAYS_INLINE s32 roundf(f32 in) {
    f32 tmp;
    s32 out;
    __asm__("round.w.s %0,%1" : "=f" (tmp) : "f" (in ));
    __asm__("mfc1      %0,%1" : "=r" (out) : "f" (tmp));
    return out;
}
// backwards compatibility
#define round_float(in) roundf(in)

/// Absolute value
ALWAYS_INLINE f32 absf(f32 in) {
    f32 out;
    __asm__("abs.s %0,%1" : "=f" (out) : "f" (in));
    return out;
}
ALWAYS_INLINE s32 absi(s32 in) {
    return ABS(in);
}
#define abss absi

#define FLT_IS_NONZERO(x) (absf(x) > NEAR_ZERO)

u32 random_u16(void);
f32 random_float(void);
s32 random_sign(void);

f32  min_3f(   f32 a, f32 b, f32 c);
s32  min_3i(   s32 a, s32 b, s32 c);
s32  min_3s(   s16 a, s16 b, s16 c);
f32  max_3f(   f32 a, f32 b, f32 c);
s32  max_3i(   s32 a, s32 b, s32 c);
s32  max_3s(   s16 a, s16 b, s16 c);
void min_max_3f(f32 a, f32 b, f32 c, f32 *min, f32 *max);
void min_max_3i(s32 a, s32 b, s32 c, s32 *min, s32 *max);
void min_max_3s(s16 a, s16 b, s16 c, s16 *min, s16 *max);

void vec3f_copy    (Vec3f dest, const Vec3f src);
void vec3i_copy    (Vec3i dest, const Vec3i src);
void vec3s_copy    (Vec3s dest, const Vec3s src);
void vec3s_to_vec3i(Vec3i dest, const Vec3s src);
void vec3s_to_vec3f(Vec3f dest, const Vec3s src);
void vec3i_to_vec3s(Vec3s dest, const Vec3i src);
void vec3i_to_vec3f(Vec3f dest, const Vec3i src);
void vec3f_to_vec3s(Vec3s dest, const Vec3f src);
void vec3f_to_vec3i(Vec3i dest, const Vec3f src);

void vec3f_copy_y_off(Vec3f dest, Vec3f src, f32 yOff);

void surface_normal_to_vec3f(Vec3f dest, struct Surface *surf);

void vec3f_set(Vec3f dest, const f32 x, const f32 y, const f32 z);
void vec3i_set(Vec3i dest, const s32 x, const s32 y, const s32 z);
void vec3s_set(Vec3s dest, const s16 x, const s16 y, const s16 z);

void vec3f_add (Vec3f dest, const Vec3f a               );
void vec3i_add (Vec3i dest, const Vec3i a               );
void vec3s_add (Vec3s dest, const Vec3s a               );
void vec3f_sum (Vec3f dest, const Vec3f a, const Vec3f b);
void vec3i_sum (Vec3i dest, const Vec3i a, const Vec3i b);
void vec3s_sum (Vec3s dest, const Vec3s a, const Vec3s b);
void vec3f_sub (Vec3f dest, const Vec3f a               );
void vec3i_sub (Vec3i dest, const Vec3i a               );
void vec3s_sub (Vec3s dest, const Vec3s a               );
void vec3f_diff(Vec3f dest, const Vec3f a, const Vec3f b);
void vec3i_diff(Vec3i dest, const Vec3i a, const Vec3i b);
void vec3s_diff(Vec3s dest, const Vec3s a, const Vec3s b);
void vec3f_mul (Vec3f dest, const Vec3f a               );
void vec3i_mul (Vec3i dest, const Vec3i a               );
void vec3s_mul (Vec3s dest, const Vec3s a               );
void vec3f_prod(Vec3f dest, const Vec3f a, const Vec3f b);
void vec3i_prod(Vec3i dest, const Vec3i a, const Vec3i b);
void vec3s_prod(Vec3s dest, const Vec3s a, const Vec3s b);
void vec3f_div (Vec3f dest, const Vec3f a               );
void vec3i_div (Vec3i dest, const Vec3i a               );
void vec3s_div (Vec3s dest, const Vec3s a               );
void vec3f_quot(Vec3f dest, const Vec3f a, const Vec3f b);
void vec3i_quot(Vec3i dest, const Vec3i a, const Vec3i b);
void vec3s_quot(Vec3s dest, const Vec3s a, const Vec3s b);

f32  vec3f_dot(              const Vec3f a, const Vec3f b);
void vec3f_cross(Vec3f dest, const Vec3f a, const Vec3f b);
void vec3f_normalize(Vec3f dest);
void mtxf_copy(Mat4 dest, Mat4 src);
void mtxf_identity(Mat4 mtx);
void mtxf_translate(Mat4 dest, Vec3f b);
void mtxf_lookat(Mat4 mtx, Vec3f from, Vec3f to, s32 roll);
void mtxf_rotate_zxy_and_translate(Mat4 dest, Vec3f trans, Vec3s rot);
void mtxf_rotate_xyz_and_translate(Mat4 dest, Vec3f trans, Vec3s rot);
void mtxf_rotate_zxy_and_translate_and_mul(Vec3s rot, Vec3f trans, Mat4 dest, Mat4 src);
void mtxf_rotate_xyz_and_translate_and_mul(Vec3s rot, Vec3f trans, Mat4 dest, Mat4 src);
void mtxf_billboard(Mat4 dest, Mat4 mtx, Vec3f position, Vec3f scale, s32 angle);
void mtxf_shadow(Mat4 dest, Mat4 src, Vec3f upDir, Vec3f pos, Vec3f scale, s32 yaw);
void mtxf_align_terrain_normal(Mat4 dest, Vec3f upDir, Vec3f pos, s32 yaw);
void mtxf_align_terrain_triangle(Mat4 mtx, Vec3f pos, s32 yaw, f32 radius);
void mtxf_mul(Mat4 dest, Mat4 a, Mat4 b);
void mtxf_scale_vec3f(Mat4 dest, Mat4 mtx, Vec3f s);
void mtxf_mul_vec3s(Mat4 mtx, Vec3s b);

extern void mtxf_to_mtx_fast(register s16 *dest, register float *src);
ALWAYS_INLINE void mtxf_to_mtx(register void *dest, register void *src) {
    mtxf_to_mtx_fast((s16*)dest, (float*)src);
    // guMtxF2L(src, dest);
}

void mtxf_rotate_xy(Mtx *mtx, s32 angle);
void linear_mtxf_mul_vec3f(Mat4 m, Vec3f dst, Vec3f v);
void linear_mtxf_mul_vec3f_and_translate(Mat4 m, Vec3f dst, Vec3f v);
void linear_mtxf_transpose_mul_vec3f(Mat4 m, Vec3f dst, Vec3f v);
void get_pos_from_transform_mtx(Vec3f dest, Mat4 objMtx, Mat4 camMtx);

void vec2f_get_lateral_dist(                   Vec2f from, Vec2f to,            f32 *lateralDist                            );
void vec3f_get_lateral_dist(                   Vec3f from, Vec3f to,            f32 *lateralDist                            );
void vec3f_get_lateral_dist_squared(           Vec3f from, Vec3f to,            f32 *lateralDist                            );
void vec3f_get_dist(                           Vec3f from, Vec3f to, f32 *dist                                              );
void vec3f_get_dist_squared(                   Vec3f from, Vec3f to, f32 *dist                                              );
void vec3f_get_dist_and_yaw(                   Vec3f from, Vec3f to, f32 *dist,                                  Angle  *yaw);
void vec3f_get_pitch(                          Vec3f from, Vec3f to,                              Angle  *pitch             );
void vec3f_get_yaw(                            Vec3f from, Vec3f to,                                             Angle  *yaw);
void vec3f_get_angle(                          Vec3f from, Vec3f to,                              Angle  *pitch, Angle  *yaw);
void vec3f_get_lateral_dist_and_pitch(         Vec3f from, Vec3f to,            f32 *lateralDist, Angle  *pitch             );
void vec3f_get_lateral_dist_and_yaw(           Vec3f from, Vec3f to,            f32 *lateralDist,                Angle  *yaw);
void vec3f_get_lateral_dist_and_angle(         Vec3f from, Vec3f to,            f32 *lateralDist, Angle  *pitch, Angle  *yaw);
void vec3f_get_dist_and_lateral_dist_and_angle(Vec3f from, Vec3f to, f32 *dist, f32 *lateralDist, Angle  *pitch, Angle  *yaw);
void vec3f_get_dist_and_angle(                 Vec3f from, Vec3f to, f32 *dist,                   Angle  *pitch, Angle  *yaw);
void vec3s_get_dist_and_angle(                 Vec3s from, Vec3s to, s16 *dist,                   Angle  *pitch, Angle  *yaw);
void vec3f_to_vec3s_get_dist_and_angle(        Vec3f from, Vec3s to, f32 *dist,                    Angle *pitch, Angle  *yaw);
void vec3s_set_dist_and_angle(                 Vec3s from, Vec3s to, s16  dist,                   Angle32 pitch, Angle32 yaw);
void vec3f_set_dist_and_angle(                 Vec3f from, Vec3f to, f32  dist,                   Angle32 pitch, Angle32 yaw);

s32 approach_angle(s32 current, s32 target, s32 inc);
s32 approach_s16(s32 current, s32 target, s32 inc, s32 dec);
s32 approach_s32(s32 current, s32 target, s32 inc, s32 dec);
f32 approach_f32(f32 current, f32 target, f32 inc, f32 dec);
Bool32 approach_angle_bool(s16 *current, s32 target, s32 inc);
Bool32 approach_s16_bool(s16 *current, s32 target, s32 inc, s32 dec);
Bool32 approach_s32_bool(s32 *current, s32 target, s32 inc, s32 dec);
Bool32 approach_f32_bool(f32 *current, f32 target, f32 inc, f32 dec);
#define approach_s16_symmetric(current, target, inc) approach_s16((current), (target), (inc), (inc))
#define approach_s32_symmetric(current, target, inc) approach_s32((current), (target), (inc), (inc))
#define approach_f32_symmetric(current, target, inc) approach_f32((current), (target), (inc), (inc))
#define approach_s16_symmetric_bool(current, target, inc) approach_s16_bool((current), (target), (inc), (inc))
#define approach_s32_symmetric_bool(current, target, inc) approach_s32_bool((current), (target), (inc), (inc))
#define approach_f32_symmetric_bool(current, target, inc) approach_f32_bool((current), (target), (inc), (inc))
s32 approach_f32_signed(f32 *current, f32 target, f32 inc);
s32 approach_f32_asymptotic_bool(f32 *current, f32 target, f32 multiplier);
f32 approach_f32_asymptotic(f32 current, f32 target, f32 multiplier);
s32 approach_s16_asymptotic_bool(s16 *current, s16 target, s16 divisor);
s32 approach_s16_asymptotic(s16 current, s16 target, s16 divisor);
s32 abs_angle_diff(s16 a0, s16 a1);
s32 atan2s(f32 y, f32 x);
f32 atan2f(f32 a, f32 b);
void spline_get_weights(Vec4f result, f32 t, UNUSED s32 c);
void anim_spline_init(Vec4s *keyFrames);
s32  anim_spline_poll(Vec3f result);
void find_surface_on_ray(Vec3f orig, Vec3f dir, struct Surface **hit_surface, Vec3f hit_pos, s32 flags);

#endif // MATH_UTIL_H
