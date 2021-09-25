#ifndef MATH_UTIL_H
#define MATH_UTIL_H

#include <PR/ultratypes.h>

#include "types.h"


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

#define sins(x) gSineTable[(u16) (x) >> 4]
#define coss(x) gCosineTable[(u16) (x) >> 4]
#define atans(x) gArctanTable[(s32)((((x) * 1024) + 0.5f))] // is this correct? used for atan2_lookup

#define DEG_PER_RAD 57.29577950560105
#define RAD_PER_DEG (1.0 / DEG_PER_RAD)

#define angle_to_degrees(  x) (f32)(((Angle)(x) / 65536.0f) * 360.0f)
#define degrees_to_angle(  x) (Angle)(((f32)(x) * 0x10000 ) / 360   )
#define angle_to_radians(  x) (f32)(((Angle)(x) * M_PI    ) / 0x8000)
#define radians_to_angle(  x) (Angle)(((f32)(x) / M_PI    ) * 0x8000)
#define degrees_to_radians(x) (f32)(   (f32)(x) * RAD_PER_DEG       )
#define radians_to_degrees(x) (f32)(   (f32)(x) * DEG_PER_RAD       )

#define ABSF(x) ((x) > 0.0f ? (x) : -(x))
#define ABSI(x) ((x) > 0    ? (x) : -(x))
#define ABS(x)  ABSF((x))

#define min(a, b) MIN((a), (b)) // ((a) < (b) ? (a) : (b))
#define max(a, b) MAX((a), (b)) // ((a) > (b) ? (a) : (b))
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

#define RAYCAST_FIND_FLOOR  (0x1)
#define RAYCAST_FIND_WALL   (0x2)
#define RAYCAST_FIND_CEIL   (0x4)
#define RAYCAST_FIND_WATER  (0x8)
#define RAYCAST_FIND_ALL    (0xFFFFFFFF)

void vec3f_copy(Vec3f dest, Vec3f src);
void vec3f_set(Vec3f dest, f32 x, f32 y, f32 z);
void vec3f_add(Vec3f dest, Vec3f a);
void vec3f_sum(Vec3f dest, Vec3f a, Vec3f b);
void vec3s_copy(Vec3s dest, Vec3s src);
void vec3s_set(Vec3s dest, s16 x, s16 y, s16 z);
void vec3s_add(Vec3s dest, Vec3s a);
void vec3s_sum(Vec3s dest, Vec3s a, Vec3s b);
void vec3s_sub(Vec3s dest, Vec3s a);
void vec3f_to_vec3s(Vec3s dest, Vec3f a);
void find_vector_perpendicular_to_plane(Vec3f dest, Vec3f a, Vec3f b, Vec3f c);
void vec3f_cross(Vec3f dest, Vec3f a, Vec3f b);
void vec3f_normalize(Vec3f dest);
void mtxf_copy(Mat4 dest, Mat4 src);
void mtxf_identity(Mat4 mtx);
void mtxf_translate(Mat4 dest, Vec3f b);
void mtxf_lookat(Mat4 mtx, Vec3f from, Vec3f to, s32 roll);
void mtxf_rotate_zxy_and_translate(Mat4 dest, Vec3f translate, Vec3s rotate);
void mtxf_rotate_xyz_and_translate(Mat4 dest, Vec3f b, Vec3s c);
void mtxf_billboard(Mat4 dest, Mat4 mtx, Vec3f position, s32 angle);
void mtxf_align_terrain_normal(Mat4 dest, Vec3f upDir, Vec3f pos, s32 yaw);
void mtxf_align_terrain_triangle(Mat4 mtx, Vec3f pos, s32 yaw, f32 radius);
void mtxf_mul(Mat4 dest, Mat4 a, Mat4 b);
void mtxf_scale_vec3f(Mat4 dest, Mat4 mtx, Vec3f s);
void mtxf_mul_vec3s(Mat4 mtx, Vec3s b);
void mtxf_to_mtx(void *dest, void *src);
void mtxf_to_mtx_constant(register s16 *dest, register f32 *src);
void mtxf_to_mtx_scale(Mtx *dest, Mat4 src);
void mtxf_rotate_xy(Mtx *mtx, s32 angle);
void get_pos_from_transform_mtx(Vec3f dest, Mat4 objMtx, Mat4 camMtx);
void vec3f_get_dist_and_angle(Vec3f from, Vec3f to, f32 *dist, s16 *pitch, s16 *yaw);
void vec3f_set_dist_and_angle(Vec3f from, Vec3f to, f32  dist, s32  pitch, s32  yaw);
s32 approach_s32(s32 current, s32 target, s32 inc, s32 dec);
f32 approach_f32(f32 current, f32 target, f32 inc, f32 dec);
s32 approach_angle(s32 current, s32 target, s32 inc);
s16 atan2s(f32 y, f32 x);
f32 atan2f(f32 a, f32 b);
void spline_get_weights(Vec4f result, f32 t, UNUSED s32 c);
void anim_spline_init(Vec4s *keyFrames);
s32 anim_spline_poll(Vec3f result);
extern void find_surface_on_ray(Vec3f orig, Vec3f dir, struct Surface **hit_surface, Vec3f hit_pos, s32 flags);

#endif // MATH_UTIL_H
