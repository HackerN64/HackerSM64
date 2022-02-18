#include <ultra64.h>

#include "sm64.h"
#include "engine/graph_node.h"
#include "math_util.h"
#include "surface_collision.h"
#include "trig_tables.inc.c"
#include "surface_load.h"
#include "game/puppyprint.h"
#include "game/rendering_graph_node.h"

#include "config.h"
#include "config/config_world.h"


Mat4 identityMtx = {
    { 1.0f, 0.0f, 0.0f, 0.0f },
    { 0.0f, 1.0f, 0.0f, 0.0f },
    { 0.0f, 0.0f, 1.0f, 0.0f },
    { 0.0f, 0.0f, 0.0f, 1.0f },
};

Vec3f gVec3fZero = {  0.0f,  0.0f,  0.0f };
Vec3f gVec3fOne  = {  1.0f,  1.0f,  1.0f };
Vec3s gVec3sZero = {     0,     0,     0 };
Vec3i gVec3iZero = {     0,     0,     0 };
Vec3s gVec3sOne  = {     1,     1,     1 };

static u16 gRandomSeed16;

// Generate a pseudorandom integer from 0 to 65535 from the random seed, and update the seed.
u32 random_u16(void) {
    if (gRandomSeed16 == 22026) {
        gRandomSeed16 = 0;
    }

    u16 temp1 = (((gRandomSeed16 & 0x00FF) << 8) ^ gRandomSeed16);

    gRandomSeed16 = ((temp1 & 0x00FF) << 8) + ((temp1 & 0xFF00) >> 8);

    temp1 = (((temp1 & 0x00FF) << 1) ^ gRandomSeed16);
    u16 temp2 = ((temp1 >> 1) ^ 0xFF80);

    if ((temp1 & 0x1) == 0) {
        if (temp2 == 43605) {
            gRandomSeed16 = 0;
        } else {
            gRandomSeed16 = (temp2 ^ 0x1FF4);
        }
    } else {
        gRandomSeed16 = (temp2 ^ 0x8180);
    }

    return gRandomSeed16;
}

// Generate a pseudorandom float in the range [0, 1).
f32 random_float(void) {
    return ((f32) random_u16() / (f32) 0x10000);
}

// Return either -1 or 1 with a 50:50 chance.
s32 random_sign(void) {
    return ((random_u16() >= 0x7FFF) ? 1 : -1);
}

/// Returns the lowest of three values.
#define min_3_func(a0, a1, a2) {\
    if (a1 < a0) a0 = a1;       \
    if (a2 < a0) a0 = a2;       \
    return a0;                  \
}
f32 min_3f(f32 a, f32 b, f32 c) { min_3_func(a, b, c); }
s32 min_3i(s32 a, s32 b, s32 c) { min_3_func(a, b, c); }
s32 min_3s(s16 a, s16 b, s16 c) { min_3_func(a, b, c); }

/// Returns the highest of three values.
#define max_3_func(a0, a1, a2) {\
    if (a1 > a0) a0 = a1;       \
    if (a2 > a0) a0 = a2;       \
    return a0;                  \
}
f32 max_3f(f32 a, f32 b, f32 c) { max_3_func(a, b, c); }
s32 max_3i(s32 a, s32 b, s32 c) { max_3_func(a, b, c); }
s32 max_3s(s16 a, s16 b, s16 c) { max_3_func(a, b, c); }

/// A combination of the above.
#define min_max_3_func(a, b, c, min, max) { \
    if (b < a) {                            \
        *max = a;                           \
        *min = b;                           \
    } else {                                \
        *min = a;                           \
        *max = b;                           \
    }                                       \
    if (c < *min) *min = c;                 \
    if (c > *max) *max = c;                 \
}
void min_max_3f(f32 a, f32 b, f32 c, f32 *min, f32 *max) { min_max_3_func(a, b, c, min, max); }
void min_max_3i(s32 a, s32 b, s32 c, s32 *min, s32 *max) { min_max_3_func(a, b, c, min, max); }
void min_max_3s(s16 a, s16 b, s16 c, s16 *min, s16 *max) { min_max_3_func(a, b, c, min, max); }

/// Compares a vector to three floats, return TRUE if they match.
Bool32 vec3f_compare_f32(Vec3f vec, f32 x, f32 y, f32 z) {
    f32 vecX = vec[0];
    f32 vecY = vec[1];
    f32 vecZ = vec[2];
    return (x == vecX && y == vecY && z == vecZ);
}

/// Compares two vectors, return TRUE if they match.
Bool32 vec3f_compare(Vec3f a, Vec3f b) {
    f32 aX = a[0];
    f32 aY = a[1];
    f32 aZ = a[2];
    f32 bX = b[0];
    f32 bY = b[1];
    f32 bZ = b[2];
    return (aX == bX && aY == bY && aZ == bZ);
}

/// Perform a bitwise copy from vector 'src' to 'dest'.
#define vec3_copy_bits(destFmt, dest, srcFmt, src) {\
    destFmt x = ((srcFmt *) src)[0];                \
    destFmt y = ((srcFmt *) src)[1];                \
    destFmt z = ((srcFmt *) src)[2];                \
    ((destFmt *) dest)[0] = x;                      \
    ((destFmt *) dest)[1] = y;                      \
    ((destFmt *) dest)[2] = z;                      \
}
void vec3f_copy    (Vec3f dest, const Vec3f src) { vec3_copy_bits(f32, dest, f32, src); } // 32 -> 32
void vec3i_copy    (Vec3i dest, const Vec3i src) { vec3_copy_bits(s32, dest, s32, src); } // 32 -> 32
void vec3s_copy    (Vec3s dest, const Vec3s src) { vec3_copy_bits(s16, dest, s16, src); } // 16 -> 16
void vec3s_to_vec3i(Vec3i dest, const Vec3s src) { vec3_copy_bits(s32, dest, s16, src); } // 16 -> 32
void vec3s_to_vec3f(Vec3f dest, const Vec3s src) { vec3_copy_bits(f32, dest, s16, src); } // 16 -> 32
void vec3i_to_vec3s(Vec3s dest, const Vec3i src) { vec3_copy_bits(s16, dest, s32, src); } // 32 -> 16
void vec3i_to_vec3f(Vec3f dest, const Vec3i src) { vec3_copy_bits(f32, dest, s32, src); } // 32 -> 32

void surface_normal_to_vec3f(Vec3f dest, struct Surface *surf) {
    f32 x = surf->normal.x;
    f32 y = surf->normal.y;
    f32 z = surf->normal.z;
    ((f32 *) dest)[0] = x;
    ((f32 *) dest)[1] = y;
    ((f32 *) dest)[2] = z;
}

/// Convert float vector a to a short vector 'dest' by rounding the components to the nearest integer.
#define vec3_copy_bits_roundf(fmt, dest, src) { \
    fmt x = roundf(src[0]);                     \
    fmt y = roundf(src[1]);                     \
    fmt z = roundf(src[2]);                     \
    ((fmt *) dest)[0] = x;                      \
    ((fmt *) dest)[1] = y;                      \
    ((fmt *) dest)[2] = z;                      \
}
void vec3f_to_vec3s(Vec3s dest, const Vec3f src) { vec3_copy_bits_roundf(s16, dest, src); } // 32 -> 16
void vec3f_to_vec3i(Vec3i dest, const Vec3f src) { vec3_copy_bits_roundf(s32, dest, src); } // 32 -> 32
#undef vec3_copy_bits_roundf

#define vec3_copy_y_off_func(destFmt, dest, srcFmt, src, yOff) {\
    destFmt x = ((srcFmt *) src)[0];                            \
    destFmt y = ((srcFmt *) src)[1] + yOff;                     \
    destFmt z = ((srcFmt *) src)[2];                            \
    ((destFmt *) dest)[0] = x;                                  \
    ((destFmt *) dest)[1] = y;                                  \
    ((destFmt *) dest)[2] = z;                                  \
}
void vec3f_copy_y_off(Vec3f dest, Vec3f src, f32 yOff) { vec3_copy_y_off_func(f32, dest, f32, src, yOff); }
#undef vec3_copy_y_off_func

/// Set vector 'dest' to (x, y, z)
inline void vec3f_set(Vec3f dest, const f32 x, const f32 y, const f32 z) { vec3_set(dest, x, y, z); }
inline void vec3i_set(Vec3i dest, const s32 x, const s32 y, const s32 z) { vec3_set(dest, x, y, z); }
inline void vec3s_set(Vec3s dest, const s16 x, const s16 y, const s16 z) { vec3_set(dest, x, y, z); }

/// Add vector 'a' to 'dest'
#define vec3_add_func(fmt, dest, a) {   \
    fmt *dstp = (fmt *)(dest);          \
    fmt sum, sum2;                      \
    s32 i;                              \
    for (i = 0; i < 3; i++) {           \
        sum = *(a)++;                   \
        sum2 = *dstp;                   \
        *dstp++ = (sum + sum2);         \
    }                                   \
}
void vec3f_add(Vec3f dest, const Vec3f a) { vec3_add_func(f32, dest, a); }
void vec3i_add(Vec3i dest, const Vec3i a) { vec3_add_func(s32, dest, a); }
void vec3s_add(Vec3s dest, const Vec3s a) { vec3_add_func(s16, dest, a); }
#undef vec3_add_func

/// Make 'dest' the sum of vectors a and b.
#define vec3_sum_func(fmt, dest, a, b) {\
    fmt *dstp = (fmt *)(dest);          \
    fmt sum, sum2;                      \
    s32 i;                              \
    for (i = 0; i < 3; i++) {           \
        sum = *(a)++;                   \
        sum2 = *(b)++;                  \
        *dstp++ = (sum + sum2);         \
    }                                   \
}
void vec3f_sum(Vec3f dest, const Vec3f a, const Vec3f b) { vec3_sum_func(f32, dest, a, b); }
void vec3i_sum(Vec3i dest, const Vec3i a, const Vec3i b) { vec3_sum_func(s32, dest, a, b); }
void vec3s_sum(Vec3s dest, const Vec3s a, const Vec3s b) { vec3_sum_func(s16, dest, a, b); }
#undef vec3_sum_func

/// Subtract vector a from 'dest'
#define vec3_sub_func(fmt, dest, a) {   \
    fmt x = ((fmt *) a)[0];             \
    fmt y = ((fmt *) a)[1];             \
    fmt z = ((fmt *) a)[2];             \
    ((fmt *) dest)[0] -= x;             \
    ((fmt *) dest)[1] -= y;             \
    ((fmt *) dest)[2] -= z;             \
}
void vec3f_sub(Vec3f dest, const Vec3f a) { vec3_sub_func(f32, dest, a); }
void vec3i_sub(Vec3i dest, const Vec3i a) { vec3_sub_func(s32, dest, a); }
void vec3s_sub(Vec3s dest, const Vec3s a) { vec3_sub_func(s16, dest, a); }
#undef vec3_sub_func

/// Make 'dest' the difference of vectors a and b.
#define vec3_diff_func(fmt, dest, a, b) {   \
    fmt x1 = ((fmt *) a)[0];                \
    fmt y1 = ((fmt *) a)[1];                \
    fmt z1 = ((fmt *) a)[2];                \
    fmt x2 = ((fmt *) b)[0];                \
    fmt y2 = ((fmt *) b)[1];                \
    fmt z2 = ((fmt *) b)[2];                \
    ((fmt *) dest)[0] = (x1 - x2);          \
    ((fmt *) dest)[1] = (y1 - y2);          \
    ((fmt *) dest)[2] = (z1 - z2);          \
}
void vec3f_diff(Vec3f dest, const Vec3f a, const Vec3f b) { vec3_diff_func(f32, dest, a, b); }
void vec3i_diff(Vec3i dest, const Vec3i a, const Vec3i b) { vec3_diff_func(s32, dest, a, b); }
void vec3s_diff(Vec3s dest, const Vec3s a, const Vec3s b) { vec3_diff_func(s16, dest, a, b); }
#undef vec3_diff_func

/// Multiply vector 'a' into 'dest'
#define vec3_mul_func(fmt, dest, a) {   \
    fmt x = ((fmt *) a)[0];             \
    fmt y = ((fmt *) a)[1];             \
    fmt z = ((fmt *) a)[2];             \
    ((fmt *) dest)[0] *= x;             \
    ((fmt *) dest)[1] *= y;             \
    ((fmt *) dest)[2] *= z;             \
}
void vec3f_mul(Vec3f dest, const Vec3f a) { vec3_mul_func(f32, dest, a); }
void vec3i_mul(Vec3i dest, const Vec3i a) { vec3_mul_func(s32, dest, a); }
void vec3s_mul(Vec3s dest, const Vec3s a) { vec3_mul_func(s16, dest, a); }
#undef vec3_mul_func

/// Make 'dest' the product of vectors a and b.
#define vec3_prod_func(fmt, dest, a, b) {   \
    fmt x1 = ((fmt *) a)[0];                \
    fmt y1 = ((fmt *) a)[1];                \
    fmt z1 = ((fmt *) a)[2];                \
    fmt x2 = ((fmt *) b)[0];                \
    fmt y2 = ((fmt *) b)[1];                \
    fmt z2 = ((fmt *) b)[2];                \
    ((fmt *) dest)[0] = (x1 * x2);          \
    ((fmt *) dest)[1] = (y1 * y2);          \
    ((fmt *) dest)[2] = (z1 * z2);          \
}
void vec3f_prod(Vec3f dest, const Vec3f a, const Vec3f b) { vec3_prod_func(f32, dest, a, b); }
void vec3i_prod(Vec3i dest, const Vec3i a, const Vec3i b) { vec3_prod_func(s32, dest, a, b); }
void vec3s_prod(Vec3s dest, const Vec3s a, const Vec3s b) { vec3_prod_func(s16, dest, a, b); }
#undef vec3_prod_func

/// Performs element-wise division of two 3-vectors.
#define vec3_div_func(fmt, dest, a) {   \
    fmt x = ((fmt *) a)[0];             \
    fmt y = ((fmt *) a)[1];             \
    fmt z = ((fmt *) a)[2];             \
    ((fmt *) dest)[0] /= x;             \
    ((fmt *) dest)[1] /= y;             \
    ((fmt *) dest)[2] /= z;             \
}
void vec3f_div(Vec3f dest, const Vec3f a) { vec3_div_func(f32, dest, a); }
void vec3i_div(Vec3i dest, const Vec3i a) { vec3_div_func(s32, dest, a); }
void vec3s_div(Vec3s dest, const Vec3s a) { vec3_div_func(s16, dest, a); }
#undef vec3_div_func

/// Make 'dest' the quotient of vectors a and b.
#define vec3_quot_func(fmt, dest, a, b) {   \
    fmt x1 = ((fmt *) a)[0];                \
    fmt y1 = ((fmt *) a)[1];                \
    fmt z1 = ((fmt *) a)[2];                \
    fmt x2 = ((fmt *) b)[0];                \
    fmt y2 = ((fmt *) b)[1];                \
    fmt z2 = ((fmt *) b)[2];                \
    ((fmt *) dest)[0] = (x1 / x2);          \
    ((fmt *) dest)[1] = (y1 / y2);          \
    ((fmt *) dest)[2] = (z1 / z2);          \
}
void vec3f_quot(Vec3f dest, const Vec3f a, const Vec3f b) { vec3_quot_func(f32, dest, a, b); }
void vec3i_quot(Vec3i dest, const Vec3i a, const Vec3i b) { vec3_quot_func(s32, dest, a, b); }
void vec3s_quot(Vec3s dest, const Vec3s a, const Vec3s b) { vec3_quot_func(s16, dest, a, b); }
#undef vec3_quot_func

/// Return the dot product of vectors a and b.
f32 vec3f_dot(const Vec3f a, const Vec3f b) {
    return vec3_dot(a, b);
}

/// Make vector 'dest' the cross product of vectors a and b.
void vec3f_cross(Vec3f dest, const Vec3f a, const Vec3f b) {
    f32 x1 = a[0];
    f32 y1 = a[1];
    f32 z1 = a[2];
    f32 x2 = b[0];
    f32 y2 = b[1];
    f32 z2 = b[2];
    dest[0] = (y1 * z2) - (z1 * y2);
    dest[1] = (z1 * x2) - (x1 * z2);
    dest[2] = (x1 * y2) - (y1 * x2);
}

/// Scale vector 'dest' so it has length 1.
void vec3f_normalize(Vec3f dest) {
    f32 mag = vec3_sumsq(dest);
    if (mag > construct_float(NEAR_ZERO)) {
        f32 invsqrt = (construct_float(1.0f) / sqrtf(mag));
        vec3_mul_val(dest, invsqrt);
    } else {
        // Default to up vector.
        dest[0] = 0;
        ((u32 *) dest)[1] = FLOAT_ONE;
        dest[2] = 0;
    }
}

/// Scale vector 'dest' so it has length at most 'max'.
void vec3f_normalize_max(Vec3f dest, f32 max) {
    f32 mag = vec3_sumsq(dest);
    if (mag > construct_float(NEAR_ZERO)) {
        if (mag > sqr(max)) {
            f32 invsqrt = max / sqrtf(mag);
            vec3_mul_val(dest, invsqrt);
        }
    } else {
        // Default to up vector.
        dest[0] = 0;
        ((u32 *) dest)[1] = FLOAT_ONE;
        dest[2] = 0;
    }
}

/// Struct the same data size as a Mat4
struct CopyMat4 {
    f32 a[16];
};

/// Copy matrix 'src' to 'dest' by casting to a struct CopyMat4 pointer.
UNUSED void mtxf_copy(Mat4 dest, Mat4 src) {
    *((struct CopyMat4 *) dest) = *((struct CopyMat4 *) src);
}

/// Set mtx to the identity matrix.
void mtxf_identity(Mat4 mtx) {
    s32 i;
    f32 *dest;
    for (dest = ((f32 *) mtx + 1), i = 0; i < 14; dest++, i++) {
        *dest = 0;
    }
    for (dest = (f32 *) mtx, i = 0; i < 4; dest += 5, i++) {
        *((u32 *) dest) = FLOAT_ONE;
    }
}

/// Set dest to a translation matrix of vector b.
UNUSED void mtxf_translate(Mat4 dest, Vec3f b) {
    s32 i;
    f32 *pen;
    for (pen = ((f32 *) dest + 1), i = 0; i < 12; pen++, i++) {
        *pen = 0;
    }
    for (pen = (f32 *) dest, i = 0; i < 4; pen += 5, i++) {
        *((u32 *) pen) = FLOAT_ONE;
    }
    vec3f_copy(dest[3], b);
}

/**
 * Set matrix 'dest' to 'src' scaled by vector s
 */
void mtxf_scale_vec3f(Mat4 dest, Mat4 src, Vec3f s) {
    f32 x = s[0];
    f32 y = s[1];
    f32 z = s[2];
    f32 *dstp = (f32 *)dest;
    f32 *srcp = (f32 *)src;
    s32 i;
    for (i = 0; i < 4; i++) {
        dstp[ 0] = srcp[ 0] * x;
        dstp[ 4] = srcp[ 4] * y;
        dstp[ 8] = srcp[ 8] * z;
        dstp[12] = srcp[12];
        dstp++;
        srcp++;
    }
}

/**
 * Multiply a vector with a transformation matrix, which applies the transformation
 * to the point. Note that the bottom row is assumed to be [0, 0, 0, 1], which is
 * true for transformation matrices if the translation has a w component of 1.
 */
UNUSED void mtxf_mul_vec3s(Mat4 mtx, Vec3s dest) {
    f32 x = dest[0];
    f32 y = dest[1];
    f32 z = dest[2];
    f32 *mtxp = (f32 *)mtx;
    s16 *dstp = (s16 *)dest;
    s32 i;
    for (i = 0; i < 3; i++) {
        *dstp++ = ((mtxp[ 0] * x)
                 + (mtxp[ 4] * y)
                 + (mtxp[ 8] * z)
                 +  mtxp[12]);
        mtxp++;
    }
}

void mtxf_translate_local_vec3f(Mat4 mtx, Vec3f dest, Vec3f src) {
    f32 x = src[0];
    f32 y = src[1];
    f32 z = src[2];
    f32 *mtxp = (f32 *)mtx;
    f32 *dstp = (f32 *)dest;
    s32 i;
    for (i = 0; i < 3; i++) {
        *dstp++ += ((mtxp[0] * x)
                  + (mtxp[4] * y)
                  + (mtxp[8] * z));
        mtxp++;
    }
}

/**
 * Multiply a vector by a matrix of the form
 * | ? ? ? 0 |
 * | ? ? ? 0 |
 * | ? ? ? 0 |
 * | 0 0 0 1 |
 * i.e. a matrix representing a linear transformation over 3 space.
 */
void linear_mtxf_mul_vec3f(Mat4 mtx, Vec3f dest, Vec3f src) {
    f32 x = src[0];
    f32 y = src[1];
    f32 z = src[2];
    f32 *mtxp = (f32 *)mtx;
    f32 *dstp = (f32 *)dest;
    s32 i;
    for (i = 0; i < 3; i++) {
        *dstp++ = ((mtxp[0] * x)
                 + (mtxp[4] * y)
                 + (mtxp[8] * z));
        mtxp++;
    }
}

// Transform 'dst' by 'mtx' from 'src' with translation.
void linear_mtxf_mul_vec3f_and_translate(Mat4 mtx, Vec3f dest, Vec3f src) {
    f32 x = src[0];
    f32 y = src[1];
    f32 z = src[2];
    f32 *mtxp = (f32 *)mtx;
    f32 *dstp = (f32 *)dest;
    s32 i;
    for (i = 0; i < 3; i++) {
        *dstp++ = ((mtxp[ 0] * x)
                 + (mtxp[ 4] * y)
                 + (mtxp[ 8] * z)
                 +  mtxp[12]);
        mtxp++;
    }
}

/**
 * Multiply a vector by the transpose of a matrix of the form
 * | ? ? ? 0 |
 * | ? ? ? 0 |
 * | ? ? ? 0 |
 * | 0 0 0 1 |
 * i.e. a matrix representing a linear transformation over 3 space.
 */
void linear_mtxf_transpose_mul_vec3f(Mat4 mtx, Vec3f dest, Vec3f src) {
    f32 x = src[0];
    f32 y = src[1];
    f32 z = src[2];
    f32 *dstp = (f32 *)dest;
    s32 i;
    for (i = 0; i < 3; i++) {
        *dstp++ = ((mtx[i][0] * x)
                 + (mtx[i][1] * y)
                 + (mtx[i][2] * z)); // dot(mtx[i], src);
    }
}

/// Build a matrix that rotates around the z axis, then the x axis, then the y axis, and then translates.
void mtxf_rotate_zxy_and_translate(Mat4 dest, Vec3f trans, Vec3s rot) {
    f32 sx = sins(rot[0]);
    f32 cx = coss(rot[0]);
    f32 sy = sins(rot[1]);
    f32 cy = coss(rot[1]);
    f32 sz = sins(rot[2]);
    f32 cz = coss(rot[2]);
    f32 sysz = (sy * sz);
    f32 cycz = (cy * cz);
    dest[0][0] = (sysz * sx) + cycz;
    f32 cysz = (cy * sz);
    f32 sycz = (sy * cz);
    dest[1][0] = (sycz * sx) - cysz;
    dest[2][0] = (cx * sy);
    dest[0][1] = (cx * sz);
    dest[1][1] = (cx * cz);
    dest[2][1] = -sx;
    dest[0][2] = (cysz * sx) - sycz;
    dest[1][2] = (cycz * sx) + sysz;
    dest[2][2] = (cx * cy);
    vec3f_copy(dest[3], trans);
    MTXF_END(dest);
}

/// Build a matrix that rotates around the x axis, then the y axis, then the z axis, and then translates.
UNUSED void mtxf_rotate_xyz_and_translate(Mat4 dest, Vec3f trans, Vec3s rot) {
    f32 sx = sins(rot[0]);
    f32 cx = coss(rot[0]);
    f32 sy = sins(rot[1]);
    f32 cy = coss(rot[1]);
    f32 sz = sins(rot[2]);
    f32 cz = coss(rot[2]);
    dest[0][0] = (cy * cz);
    dest[0][1] = (cy * sz);
    dest[0][2] = -sy;
    f32 sxcz = (sx * cz);
    f32 cxsz = (cx * sz);
    dest[1][0] = (sxcz * sy) - cxsz;
    f32 sxsz = (sx * sz);
    f32 cxcz = (cx * cz);
    dest[1][1] = (sxsz * sy) + cxcz;
    dest[1][2] = (sx * cy);
    dest[2][0] = (cxcz * sy) + sxsz;
    dest[2][1] = (cxsz * sy) - sxcz;
    dest[2][2] = (cx * cy);
    vec3f_copy(dest[3], trans);
    MTXF_END(dest);
}

/// Build a matrix that rotates around the z axis, then the x axis, then the y axis, and then translates and multiplies.
void mtxf_rotate_zxy_and_translate_and_mul(Vec3s rot, Vec3f trans, Mat4 dest, Mat4 src) {
    f32 sx = sins(rot[0]);
    f32 cx = coss(rot[0]);
    f32 sy = sins(rot[1]);
    f32 cy = coss(rot[1]);
    f32 sz = sins(rot[2]);
    f32 cz = coss(rot[2]);
    Vec3f entry;
    f32 sysz = (sy * sz);
    f32 cycz = (cy * cz);
    entry[0] = (sysz * sx) + cycz;
    entry[1] = (sz * cx);
    f32 cysz = (cy * sz);
    f32 sycz = (sy * cz);
    entry[2] = (cysz * sx) - sycz;
    linear_mtxf_mul_vec3f(src, dest[0], entry);
    entry[0] = (sycz * sx) - cysz;
    entry[1] = (cz * cx);
    entry[2] = (cycz * sx) + sysz;
    linear_mtxf_mul_vec3f(src, dest[1], entry);
    entry[0] = (cx * sy);
    entry[1] = -sx;
    entry[2] = (cx * cy);
    linear_mtxf_mul_vec3f(src, dest[2], entry);
    linear_mtxf_mul_vec3f(src, dest[3], trans);
    vec3f_add(dest[3], src[3]);
    MTXF_END(dest);
}

/// Build a matrix that rotates around the x axis, then the y axis, then the z axis, and then translates and multiplies.
void mtxf_rotate_xyz_and_translate_and_mul(Vec3s rot, Vec3f trans, Mat4 dest, Mat4 src) {
    f32 sx = sins(rot[0]);
    f32 cx = coss(rot[0]);
    f32 sy = sins(rot[1]);
    f32 cy = coss(rot[1]);
    f32 sz = sins(rot[2]);
    f32 cz = coss(rot[2]);
    Vec3f entry;
    entry[0] = (cy * cz);
    entry[1] = (cy * sz);
    entry[2] = -sy;
    linear_mtxf_mul_vec3f(src, dest[0], entry);
    f32 sxcz = (sx * cz);
    f32 cxsz = (cx * sz);
    entry[0] = (sxcz * sy) - cxsz;
    f32 sxsz = (sx * sz);
    f32 cxcz = (cx * cz);
    entry[1] = (sxsz * sy) + cxcz;
    entry[2] = (sx * cy);
    linear_mtxf_mul_vec3f(src, dest[1], entry);
    entry[0] = (cxcz * sy) + sxsz;
    entry[1] = (cxsz * sy) - sxcz;
    entry[2] = (cx * cy);
    linear_mtxf_mul_vec3f(src, dest[2], entry);
    linear_mtxf_mul_vec3f(src, dest[3], trans);
    vec3f_add(dest[3], src[3]);
    MTXF_END(dest);
}

/**
 * Set mtx to a look-at matrix for the camera. The resulting transformation
 * transforms the world as if there exists a camera at position 'from' pointed
 * at the position 'to'. The up-vector is assumed to be (0, 1, 0), but the 'roll'
 * angle allows a bank rotation of the camera.
 */
void mtxf_lookat(Mat4 mtx, Vec3f from, Vec3f to, s32 roll) {
    Vec3f colX, colY, colZ;
    f32 dx = to[0] - from[0];
    f32 dz = to[2] - from[2];
    f32 invLength = sqrtf(sqr(dx) + sqr(dz));
    invLength = (construct_float(-1.0f) / MAX(invLength, construct_float(NEAR_ZERO)));
    dx *= invLength;
    dz *= invLength;
    f32 sr  = sins(roll);
    colY[1] = coss(roll);
    colY[0] =  sr * dz;
    colY[2] = -sr * dx;
    vec3f_diff(colZ, from, to); // to & from are swapped
    vec3f_normalize(colZ);
    vec3f_cross(colX, colY, colZ);
    vec3f_normalize(colX);
    vec3f_cross(colY, colZ, colX);
    vec3f_normalize(colY);
    mtx[0][0] = colX[0];
    mtx[1][0] = colX[1];
    mtx[2][0] = colX[2];
    mtx[0][1] = colY[0];
    mtx[1][1] = colY[1];
    mtx[2][1] = colY[2];
    mtx[0][2] = colZ[0];
    mtx[1][2] = colZ[1];
    mtx[2][2] = colZ[2];
    mtx[3][0] = -vec3f_dot(from, colX);
    mtx[3][1] = -vec3f_dot(from, colY);
    mtx[3][2] = -vec3f_dot(from, colZ);
    MTXF_END(mtx);
}

/**
 * Set 'dest' to a transformation matrix that turns an object to face the camera.
 * 'mtx' is the look-at matrix from the camera.
 * 'position' is the position of the object in the world.
 * 'scale' is the scale of the object.
 * 'roll' rotates the object while still facing the camera.
 */
void mtxf_billboard(Mat4 dest, Mat4 mtx, Vec3f position, Vec3f scale, s32 roll) {
    s32 i, j;
    f32 sx = scale[0];
    f32 sy = scale[1];
    f32 sz = scale[2];
    Mat4 *cameraMat = &gCameraTransform;
    for (i = 0; i < 3; i++) {
        for (j = 0; j < 3; j++) {
            dest[i][j] = (*cameraMat)[j][i];
        }
        dest[i][3] = 0.0f;
    }
    if (roll != 0x0) {
        f32 m00 = dest[0][0];
        f32 m01 = dest[0][1];
        f32 m02 = dest[0][2];
        f32 m10 = dest[1][0];
        f32 m11 = dest[1][1];
        f32 m12 = dest[1][2];
        f32 cosa = coss(roll);
        f32 sina = sins(roll);
        dest[0][0] =  (cosa * m00) + (sina * m10); 
        dest[0][1] =  (cosa * m01) + (sina * m11); 
        dest[0][2] =  (cosa * m02) + (sina * m12);
        dest[1][0] = -(sina * m00) + (cosa * m10);
        dest[1][1] = -(sina * m01) + (cosa * m11);
        dest[1][2] = -(sina * m02) + (cosa * m12);
    }
    for (i = 0; i < 3; i++) {
        dest[0][i] *= sx;
        dest[1][i] *= sy;
        dest[2][i] *= sz;
    }

    // Translation = input translation + position
    vec3f_copy(dest[3], position);
    vec3f_add(dest[3], mtx[3]);
    ((u32 *) dest)[15] = FLOAT_ONE;
}

void mtxf_held_object(Mat4 dest, Mat4 src, Mat4 throwMatrix, Vec3f translation, Vec3f scale) {
    f32 x = translation[0];
    f32 y = translation[1];
    f32 z = translation[2];

    mtxf_scale_vec3f(dest, throwMatrix, scale);

    f32 *dstp = (f32 *)dest[3];
    f32 *srcp = (f32 *)src[3];
    f32 *thp = (f32 *)throwMatrix;
    s32 i;

    for (i = 0; i < 3; i++) {
        *dstp++ = ((thp[0] * x)
                 + (thp[4] * y)
                 + (thp[8] * z)
                 + srcp[0]);
        srcp++;
        thp++;
    }

    ((u32 *) dest)[15] = FLOAT_ONE;
}

/**
 * Creates 'colX' and 'colZ' perpendicular to 'colY' input;
 */
static void vec3f_create_axis_normals_from_up_dir(Vec3f colX, Vec3f colY, Vec3f colZ) {
    vec3f_normalize(colY);
    vec3f_cross(colX, colY, colZ);
    vec3f_normalize(colX);
    vec3f_cross(colZ, colX, colY);
    vec3f_normalize(colZ);
}

/**
 * Mostly the same as 'mtxf_align_terrain_normal', but also applies a scale and multiplication.
 * 'upDir' is the terrain normal
 * 'pos' is the object's position in the world
 * 'scale' is the scale of the shadow
 * 'yaw' is the angle which it should face
 */
void mtxf_shadow(Mat4 dest, Vec3f upDir, Vec3f pos, Vec3f scale, s32 yaw) {
    Vec3f leftDir;
    Vec3f forwardDir;
    vec3f_set(forwardDir, sins(yaw), 0.0f, coss(yaw));
    vec3f_create_axis_normals_from_up_dir(leftDir, upDir, forwardDir);
    vec3f_prod(dest[0], leftDir, scale);
    vec3f_prod(dest[1], upDir, scale);
    vec3f_prod(dest[2], forwardDir, scale);
    vec3f_copy(dest[3], pos);
    MTXF_END(dest);
}

/**
 * Set 'dest' to a transformation matrix that aligns an object with the terrain
 * based on the normal. Used for enemies.
 * 'normal' is the terrain normal
 * 'yaw' is the angle which it should face
 * 'pos' is the object's position in the world
 */
void mtxf_align_terrain_normal(Mat4 dest, Vec3f normal, Vec3f pos, s32 yaw) {
    vec3f_copy(dest[1], normal); // up
    vec3f_set(dest[2], sins(yaw), 0.0f, coss(yaw)); // forward
    vec3f_create_axis_normals_from_up_dir(dest[0], dest[1], dest[2]);
    vec3f_copy(dest[3], pos);
    MTXF_END(dest);
}

static void find_floor_at_relative_angle(Vec3f point, Vec3f pos, s32 yaw, s32 angle, f32 radius, f32 height, f32 minY) {
    Angle dir = yaw + angle;
    point[0] = pos[0] + (radius * sins(dir));
    point[2] = pos[2] + (radius * coss(dir));
    point[1] = find_floor_height(point[0], height, point[2]);
    if (point[1] - pos[1] < minY) {
        point[1] = pos[1];
    }
}

/**
 * Set 'mtx' to a transformation matrix that aligns an object with the terrain
 * based on 3 height samples in an equilateral triangle around the object.
 * Used for Mario when crawling or sliding.
 * 'yaw' is the angle which it should face
 * 'pos' is the object's position in the world
 * 'radius' is the distance from each triangle vertex to the center
 */
void mtxf_align_terrain_triangle(Mat4 mtx, Vec3f pos, s32 yaw, f32 radius) {
    Vec3f point0, point1, point2;
    Vec3f xColumn, yColumn, zColumn;
    f32 minY = (-radius * 3);
    f32 height = (pos[1] + 150);

    find_floor_at_relative_angle(point0, pos, yaw, DEGREES( 60), radius, height, minY);
    find_floor_at_relative_angle(point1, pos, yaw, DEGREES(180), radius, height, minY);
    find_floor_at_relative_angle(point2, pos, yaw, DEGREES(-60), radius, height, minY);

    f32 avgY = average_3(point0[1], point1[1], point2[1]);

    vec3f_set(zColumn, sins(yaw), 0.0f, coss(yaw));
    find_vector_perpendicular_to_plane(yColumn, point0, point1, point2);
    vec3f_create_axis_normals_from_up_dir(xColumn, yColumn, zColumn);
    vec3f_copy(mtx[0], xColumn);
    vec3f_copy(mtx[1], yColumn);
    vec3f_copy(mtx[2], zColumn);

    mtx[3][0] = pos[0];
    mtx[3][1] = MAX(pos[1], avgY);
    mtx[3][2] = pos[2];

    MTXF_END(mtx);
}

/**
 * Sets matrix 'dest' to the matrix product b * a assuming they are both
 * transformation matrices with a w-component of 1. Since the bottom row
 * is assumed to equal [0, 0, 0, 1], it saves some multiplications and
 * addition.
 * The resulting matrix represents first applying transformation b and
 * then a.
 */
void mtxf_mul(Mat4 dest, Mat4 a, Mat4 b) {
    Vec3f entry;
    f32 *ap = (f32 *)a;
    f32 *destp = (f32 *)dest;
    f32 *bp;
    s32 i;
    for (i = 0; i < 16; i++) {
        vec3_copy(entry, ap);
        for (bp = (f32 *)b; (i & 3) != 3; i++) {
            *destp++ = ((entry[0] * bp[0])
                      + (entry[1] * bp[4])
                      + (entry[2] * bp[8]));
            bp++;
        }
        *destp = 0;
        ap += 4;
        destp++;
    }
    vec3f_add(dest[3], b[3]);
    ((u32 *) dest)[15] = FLOAT_ONE;
}

/**
 * Returns a float value from an index in a fixed point matrix.
 */
f32 mtx_get_float(Mtx *mtx, u32 xIndex, u32 yIndex) {
    f32 ret = 0.0f;
    u32 index = (yIndex * 4) + xIndex;
    if (index < 16) {
        s16 *src = (s16 *)mtx;
        s32 fixed_val = (src[index +  0] << 16)
                      | (src[index + 16] & 0xFFFF);
        f32 scale = construct_float(1.0f / (float)0x00010000);
        ret = mul_without_nop((f32)fixed_val, scale);
    }
    return ret;
}

/**
 * Writes a float value to a fixed point matrix.
 */
void mtx_set_float(Mtx *mtx, f32 val, u32 xIndex, u32 yIndex) {
    u32 index = (yIndex * 4) + xIndex;
    if (index < 16) {
        f32 scale = construct_float((float)0x00010000);
        s32 fixed_val = mul_without_nop(val, scale);
        s16 *dst = (s16 *)mtx;
        dst[index +  0] = (fixed_val >> 16);
        dst[index + 16] = (fixed_val & 0xFFFF);
    }
}

/**
 * Set 'mtx' to a transformation matrix that rotates around the z axis.
 */
#define MATENTRY(a, b)                          \
    ((s16 *) mtx)[a     ] = (((s32) b) >> 16);  \
    ((s16 *) mtx)[a + 16] = (((s32) b) & 0xFFFF);
void mtx_rotate_xy(Mtx *mtx, s32 angle) {
    s32 c = (coss(angle) * 0x10000);
    s32 s = (sins(angle) * 0x10000);
    f32 *mtxp = (f32 *)mtx;
    s32 i;
    for (i = 0; i < 16; i++) {
        *mtxp++ = 0;
    }
    MATENTRY(0,  c)
    MATENTRY(1,  s)
    MATENTRY(4, -s)
    MATENTRY(5,  c)
    ((s16 *) mtx)[10] = 1;
    ((s16 *) mtx)[15] = 1;
}

void create_transformation_from_matrices(Mat4 dst, Mat4 a1, Mat4 a2) {
    f32 *dstp = (f32 *)dst;
    f32 tx = a2[3][0];
    f32 ty = a2[3][1];
    f32 tz = a2[3][2];
    f32 rx, ry, rz;
    s32 i;
    for (i = 0; i < 3; i++) {
        rx = a2[i][0];
        ry = a2[i][1];
        rz = a2[i][2];
        dstp[ 0] = (a1[0][0] * rx + a1[0][1] * ry + a1[0][2] * rz); //   dot(a1[0], a2[i])
        dstp[ 4] = (a1[1][0] * rx + a1[1][1] * ry + a1[1][2] * rz); //   dot(a1[1], a2[i])
        dstp[ 8] = (a1[2][0] * rx + a1[2][1] * ry + a1[2][2] * rz); //   dot(a1[2], a2[i])
        dstp[12] = (a1[3][0] * rx + a1[3][1] * ry + a1[3][2] * rz)  //   dot(a1[3], a2[i])
                 - (      tx * rx +       ty * ry +       tz * rz); // - dot(a2[3], a2[i])
        dstp++;
    }
    MTXF_END(dst);
}

/**
 * Makes 'destWorldPos' the world space equivalent of 'srcLocalPos'.
 */
void vec3f_local_pos_to_world_pos(Vec3f destWorldPos, Vec3f srcLocalPos, Vec3f originPos, Vec3s rotation) {
    Mat4 mtx;

    mtxf_rotate_zxy_and_translate(mtx, srcLocalPos, rotation);
    linear_mtxf_mul_vec3f(mtx, destWorldPos, srcLocalPos);
    vec3f_add(destWorldPos, originPos);
}

/**
 * Makes 'destLocalPos' the local space equivalent of 'srcWorldPos'.
 */
void vec3f_world_pos_to_local_pos(Vec3f destLocalPos, Vec3f srcWorldPos, Vec3f originPos, Vec3s rotation) {
    Mat4 mtx;
    Vec3f relativePos;

    vec3f_diff(relativePos, srcWorldPos, originPos);
    mtxf_rotate_zxy_and_translate(mtx, originPos, rotation);
    linear_mtxf_transpose_mul_vec3f(mtx, destLocalPos, relativePos);
}

/**
 * Take the vector starting at 'from' pointed at 'to' an retrieve the length
 * of that vector, as well as the yaw and pitch angles.
 * Basically it converts the direction to spherical coordinates.
 */

/// Finds the horizontal distance between two vectors.
void vec3f_get_lateral_dist(Vec3f from, Vec3f to, f32 *lateralDist) {
    f32 dx = to[0] - from[0];
    f32 dz = to[2] - from[2];
    *lateralDist = sqrtf(sqr(dx) + sqr(dz));
}

/// Finds the squared horizontal distance between two vectors. Avoids a sqrtf call.
void vec3f_get_lateral_dist_squared(Vec3f from, Vec3f to, f32 *lateralDist) {
    f32 dx = to[0] - from[0];
    f32 dz = to[2] - from[2];
    *lateralDist = (sqr(dx) + sqr(dz));
}

/// Finds the distance between two vectors.
void vec3f_get_dist(Vec3f from, Vec3f to, f32 *dist) {
    Vec3f d;
    vec3_diff(d, to, from);
    *dist = vec3_mag(d);
}

/// Finds the squared distance between two vectors. Avoids a sqrtf call.
void vec3f_get_dist_squared(Vec3f from, Vec3f to, f32 *dist) {
    Vec3f d;
    vec3_diff(d, to, from);
    *dist = vec3_sumsq(d);
}

/// Finds the distance and yaw etween two vectors.
void vec3f_get_dist_and_yaw(Vec3f from, Vec3f to, f32 *dist, s16 *yaw) {
    Vec3f d;
    vec3_diff(d, to, from);
    *dist = vec3_mag(d);
    *yaw = atan2s(d[2], d[0]);
}

/// Finds the pitch between two vectors.
void vec3f_get_pitch(Vec3f from, Vec3f to, s16 *pitch) {
    Vec3f d;
    vec3_diff(d, to, from);
    *pitch = atan2s(sqrtf(sqr(d[0]) + sqr(d[2])), d[1]);
}

/// Finds the yaw between two vectors.
void vec3f_get_yaw(Vec3f from, Vec3f to, s16 *yaw) {
    f32 dx = to[0] - from[0];
    f32 dz = to[2] - from[2];
    *yaw = atan2s(dz, dx);
}

/// Finds the pitch and yaw between two vectors.
void vec3f_get_angle(Vec3f from, Vec3f to, s16 *pitch, s16 *yaw) {
    Vec3f d;
    vec3_diff(d, to, from);
    *pitch = atan2s(sqrtf(sqr(d[0]) + sqr(d[2])), d[1]);
    *yaw   = atan2s(d[2], d[0]);
}

/// Finds the horizontal distance and pitch between two vectors.
void vec3f_get_lateral_dist_and_pitch(Vec3f from, Vec3f to, f32 *lateralDist, Angle *pitch) {
    Vec3f d;
    vec3_diff(d, to, from);
    *lateralDist = sqrtf(sqr(d[0]) + sqr(d[2]));
    *pitch       = atan2s(*lateralDist, d[1]);
}

/// Finds the horizontal distance and yaw between two vectors.
void vec3f_get_lateral_dist_and_yaw(Vec3f from, Vec3f to, f32 *lateralDist, Angle *yaw) {
    f32 dx = (to[0] - from[0]);
    f32 dz = (to[2] - from[2]);
    *lateralDist = sqrtf(sqr(dx) + sqr(dz));
    *yaw         = atan2s(dz, dx);
}

/// Finds the horizontal distance and angles between two vectors.
void vec3f_get_lateral_dist_and_angle(Vec3f from, Vec3f to, f32 *lateralDist, Angle *pitch, Angle *yaw) {
    Vec3f d;
    vec3_diff(d, to, from);
    *lateralDist = sqrtf(sqr(d[0]) + sqr(d[2]));
    *pitch       = atan2s(*lateralDist, d[1]);
    *yaw         = atan2s(d[2], d[0]);
}

/// Finds the distance and angles between two vectors.
void vec3f_get_dist_and_angle(Vec3f from, Vec3f to, f32 *dist, Angle *pitch, Angle *yaw) {
    Vec3f d;
    vec3_diff(d, to, from);
    f32 xz = sqr(d[0]) + sqr(d[2]);
    *dist  = sqrtf(xz + sqr(d[1]));
    *pitch = atan2s(sqrtf(xz), d[1]);
    *yaw   = atan2s(d[2], d[0]);
}
void vec3s_get_dist_and_angle(Vec3s from, Vec3s to, s16 *dist, Angle *pitch, Angle *yaw) {
    Vec3s d;
    vec3_diff(d, to, from);
    f32 xz = sqr(d[0]) + sqr(d[2]);
    *dist  = sqrtf(xz + sqr(d[1]));
    *pitch = atan2s(sqrtf(xz), d[1]);
    *yaw   = atan2s(d[2], d[0]);
}
void vec3f_to_vec3s_get_dist_and_angle(Vec3f from, Vec3s to, f32 *dist, Angle *pitch, Angle *yaw) {
    Vec3f d;
    vec3_diff(d, to, from);
    f32 xz = sqr(d[0]) + sqr(d[2]);
    *dist  = sqrtf(xz + sqr(d[1]));
    *pitch = atan2s(sqrtf(xz), d[1]);
    *yaw   = atan2s(d[2], d[0]);
}

/// Finds the distance, horizontal distance, and angles between two vectors.
void vec3f_get_dist_and_lateral_dist_and_angle(Vec3f from, Vec3f to, f32 *dist, f32 *lateralDist, Angle *pitch, Angle *yaw) {
    Vec3f d;
    vec3_diff(d, to, from);
    f32 xz = sqr(d[0]) + sqr(d[2]);
    *dist        = sqrtf(xz + sqr(d[1]));
    *lateralDist = sqrtf(xz);
    *pitch       = atan2s(*lateralDist, d[1]);
    *yaw         = atan2s(d[2], d[0]);
}

/**
 * Construct the 'to' point which is distance 'dist' away from the 'from' position,
 * and has the angles pitch and yaw.
 */
#define vec3_set_dist_and_angle(from, to, dist, pitch, yaw) { \
    f32 x = from[0];                                          \
    f32 y = from[1];                                          \
    f32 z = from[2];                                          \
    f32 dcos = (dist * coss(pitch));                          \
    to[0] = (x + (dcos * sins(yaw  )));                       \
    to[1] = (y + (dist * sins(pitch)));                       \
    to[2] = (z + (dcos * coss(yaw  )));                       \
}
void vec3f_set_dist_and_angle(Vec3f from, Vec3f to, f32 dist, Angle32 pitch, Angle32 yaw) {
    vec3_set_dist_and_angle(from, to, dist, pitch, yaw);
}
void vec3s_set_dist_and_angle(Vec3s from, Vec3s to, s16 dist, Angle32 pitch, Angle32 yaw) {
    vec3_set_dist_and_angle(from, to, dist, pitch, yaw);
}
#undef vec3_set_dist_and_angle

/**
 * Similar to approach_s32, but converts to s16 and allows for overflow between 32767 and -32768
 */
s32 approach_angle(s32 current, s32 target, s32 inc) {
    s32 dist = (s16)(target - current);
    if (dist < 0) {
        dist += inc;
        if (dist > 0) dist = 0;
    } else if (dist > 0) {
        dist -= inc;
        if (dist < 0) dist = 0;
    }
    return (target - dist);
}
Bool32 approach_angle_bool(s16 *current, s32 target, s32 inc) {
    *current = approach_angle(*current, target, inc);
    return (*current != target);
}

s32 approach_s16(s32 current, s32 target, s32 inc, s32 dec) {
    s16 dist = (target - current);
    if (dist >= 0) { // target >= current
        current = ((dist >  inc) ? (current + inc) : target);
    } else { // target < current
        current = ((dist < -dec) ? (current - dec) : target);
    }
    return (s16)current;
}
Bool32 approach_s16_bool(s16 *current, s32 target, s32 inc, s32 dec) {
    *current = approach_s16(*current, target, inc, dec);
    return (*current != target);
}

/**
 * Return the value 'current' after it tries to approach target, going up at
 * most 'inc' and going down at most 'dec'.
 */
s32 approach_s32(s32 current, s32 target, s32 inc, s32 dec) {
    s32 dist = (target - current);
    if (dist > 0) { // current < target
        current = ((dist >  inc) ? (current + inc) : target);
    } else if (dist < 0) { // current > target
        current = ((dist < -dec) ? (current - dec) : target);
    }
    return current;
}
Bool32 approach_s32_bool(s32 *current, s32 target, s32 inc, s32 dec) {
    *current = approach_s32(*current, target, inc, dec);
    return (*current != target);
}

/**
 * Return the value 'current' after it tries to approach target, going up at
 * most 'inc' and going down at most 'dec'.
 */
f32 approach_f32(f32 current, f32 target, f32 inc, f32 dec) {
    f32 dist = (target - current);
    if (dist >= 0.0f) { // target >= current
        current = ((dist >  inc) ? (current + inc) : target);
    } else { // target < current
        current = ((dist < -dec) ? (current - dec) : target);
    }
    return current;
}
Bool32 approach_f32_bool(f32 *current, f32 target, f32 inc, f32 dec) {
    *current = approach_f32(*current, target, inc, dec);
    return !(*current == target);
}

Bool32 approach_f32_signed(f32 *current, f32 target, f32 inc) {
    *current += inc;
    if (inc >= 0.0f) {
        if (*current > target) {
            *current = target;
            return TRUE;
        }
    } else {
        if (*current < target) {
            *current = target;
            return TRUE;
        }
    }
    return FALSE;
}

/**
 * Approaches an f32 value by taking the difference between the target and current value
 * and adding a fraction of that to the current value.
 * Edits the current value directly, returns TRUE if the target has been reached, FALSE otherwise.
 */
Bool32 approach_f32_asymptotic_bool(f32 *current, f32 target, f32 multiplier) {
    if (multiplier > construct_float(1.0f)) {
        multiplier = construct_float(1.0f);
    }

    *current = (*current + ((target - *current) * multiplier));
    return (*current != target);
}

/**
 * Nearly the same as the above function, returns new value instead.
 */
f32 approach_f32_asymptotic(f32 current, f32 target, f32 multiplier) {
    return (current + ((target - current) * multiplier));
}

/**
 * Approaches an s16 value in the same fashion as approach_f32_asymptotic_bool, returns TRUE if target
 * is reached. Note: Since this function takes integers as parameters, the last argument is the
 * reciprocal of what it would be in the previous two functions.
 */
Bool32 approach_s16_asymptotic_bool(s16 *current, s16 target, s16 divisor) {
    s16 temp = *current;
    if (divisor == 0) {
        *current = target;
    } else {
        temp -= target;
        temp -= (temp / divisor);
        temp += target;
        *current = temp;
    }
    return (*current != target);
}

/**
 * Approaches an s16 value in the same fashion as approach_f32_asymptotic, returns the new value.
 * Note: last parameter is the reciprocal of what it would be in the f32 functions
 */
s32 approach_s16_asymptotic(s16 current, s16 target, s16 divisor) {
    s16 temp = current;
    if (divisor == 0) {
        current = target;
    } else {
        temp -= target;
        temp -= (temp / divisor);
        temp += target;
        current = temp;
    }
    return current;
}

s32 abs_angle_diff(s16 a0, s16 a1) {
    s16 diff = (a1 - a0);
    if (diff == -0x8000) {
        return 0x7FFF;
    }
    return abss(diff);
}

/**
 * Helper function for atan2s. Does a look up of the arctangent of y/x assuming
 * the resulting angle is in range [0, 0x2000] (1/8 of a circle).
 */
static u32 atan2_lookup(f32 y, f32 x) {
    return (x == 0)
        ? 0x0
        : atans(y / x);
}

/**
 * Compute the angle from (0, 0) to (x, y) as a s16. Given that terrain is in
 * the xz-plane, this is commonly called with (z, x) to get a yaw angle.
 */
s32 atan2s(f32 y, f32 x) {
    u16 ret;
    if (x >= 0) {
        if (y >= 0) {
            if (y >= x) {
                ret = atan2_lookup(x, y);
            } else {
                ret = 0x4000 - atan2_lookup(y, x);
            }
        } else {
            y = -y;
            if (y < x) {
                ret = 0x4000 + atan2_lookup(y, x);
            } else {
                ret = 0x8000 - atan2_lookup(x, y);
            }
        }
    } else {
        x = -x;
        if (y < 0) {
            y = -y;
            if (y >= x) {
                ret = 0x8000 + atan2_lookup(x, y);
            } else {
                ret = 0xC000 - atan2_lookup(y, x);
            }
        } else {
            if (y < x) {
                ret = 0xC000 + atan2_lookup(y, x);
            } else {
                ret = -atan2_lookup(x, y);
            }
        }
    }
    return ret;
}

/**
 * Compute the atan2 in radians by calling atan2s and converting the result.
 */
f32 atan2f(f32 y, f32 x) {
    return angle_to_radians(atan2s(y, x));
}

// Variables for a spline curve animation (used for the flight path in the grand star cutscene)
Vec4s *gSplineKeyframe;
float gSplineKeyframeFraction;
int gSplineState;

enum gSplineStates {
    CURVE_NONE,
    CURVE_BEGIN_1,
    CURVE_BEGIN_2,
    CURVE_MIDDLE,
    CURVE_END_1,
    CURVE_END_2
};

/**
 * Set 'result' to a 4-vector with weights corresponding to interpolation
 * value t in [0, 1] and gSplineState. Given the current control point P, these
 * weights are for P[0], P[1], P[2] and P[3] to obtain an interpolated point.
 * The weights naturally sum to 1, and they are also always in range [0, 1] so
 * the interpolated point will never overshoot. The curve is guaranteed to go
 * through the first and last point, but not through intermediate points.
 *
 * gSplineState ensures that the curve is clamped: the first two points
 * and last two points have different weight formulas. These are the weights
 * just before gSplineState transitions:
 * 1:    [1,      0,      0,      0]
 * 1->2: [0, (3/12), (7/12), (2/12)]
 * 2->3: [0, (1/ 6), (4/ 6), (1/ 6)]
 * 3->3: [0, (1/ 6), (4/ 6), (1/ 6)] (repeats)
 * 3->4: [0, (1/ 6), (4/ 6), (1/ 6)]
 * 4->5: [0, (2/12), (7/12), (3/12)]
 * 5:    [0,      0,      0,      1]
 *
 * I suspect that the weight formulas will give a 3rd degree B-spline with the
 * common uniform clamped knot vector, e.g. for n points:
 * [0, 0, 0, 0, 1, 2, ... n-1, n, n, n, n]
 * TODO: verify the classification of the spline / figure out how polynomials were computed
 */
void spline_get_weights(Vec4f result, f32 t, UNUSED s32 c) {
    f32 tinv  = 1 - t;
    f32 tinv2 = tinv * tinv;
    f32 tinv3 = tinv2 * tinv;
    f32 t2 = t * t;
    f32 t3 = t2 * t;
    const f32 half    = construct_float(0.5f);
    const f32 quarter = construct_float(0.25f);
    const f32 sixth   = construct_float(1.0f / 6.0f);

    switch (gSplineState) {
        case CURVE_BEGIN_1:
            result[0] = tinv3;
            result[1] = ( t3 * construct_float(1.75f)) - (t2 * construct_float(4.5f)) + (t * construct_float(3.0f));
            result[2] = (-t3 * construct_float(11 / 12.0f)) + (t2 * construct_float(1.5f));
            result[3] = t3 * sixth;
            break;
        case CURVE_BEGIN_2:
            result[0] = tinv3 * quarter;
            result[1] = (t3 * construct_float(7 / 12.0f)) - (t2 * construct_float(1.25f)) + (t * quarter) + construct_float(7 / 12.0f);
            result[2] = (-t3 * half) + (t2 * half) + (t * half) + sixth;
            result[3] = t3 * sixth;
            break;
        case CURVE_MIDDLE:
            result[0] = tinv3 * sixth;
            result[1] = (t3 * half) - t2 + construct_float(4.0f / 6.0f);
            result[2] = (-t3 * half) + (t2 * half) + (t * half) + sixth;
            result[3] = t3 * sixth;
            break;
        case CURVE_END_1:
            result[0] = tinv3 * sixth;
            result[1] = (-tinv3 * half) + (tinv2 * half) + (tinv * half) + sixth;
            result[2] = (tinv3 * construct_float(7.0f / 12.0f)) - (tinv2 * construct_float(1.25f)) + (tinv * quarter) + construct_float(7.0f / 12.0f);
            result[3] = t3 * quarter;
            break;
        case CURVE_END_2:
            result[0] = tinv3 * sixth;
            result[1] = (-tinv3 * construct_float(11.0f / 12.0f)) + (tinv2 * construct_float(1.5f));
            result[2] = (tinv3 * construct_float(1.75f)) - (tinv2 * construct_float(4.5f)) + (tinv * construct_float(3.0f));
            result[3] = t3;
            break;
    }
}

/**
 * Initialize a spline animation.
 * 'keyFrames' should be an array of (s, x, y, z) vectors
 *  s: the speed of the keyframe in 1000/frames, e.g. s=100 means the keyframe lasts 10 frames
 *  (x, y, z): point in 3D space on the curve
 * The array should end with three entries with s=0 (infinite keyframe duration).
 * That's because the spline has a 3rd degree polynomial, so it looks 3 points ahead.
 */
void anim_spline_init(Vec4s *keyFrames) {
    gSplineKeyframe = keyFrames;
    gSplineKeyframeFraction = 0;
    gSplineState = 1;
}

/**
 * Poll the next point from a spline animation.
 * anim_spline_init should be called before polling for vectors.
 * Returns TRUE when the last point is reached, FALSE otherwise.
 */
s32 anim_spline_poll(Vec3f result) {
    Vec4f weights;
    s32 i;
    s32 hasEnded = FALSE;

    vec3_zero(result);
    spline_get_weights(weights, gSplineKeyframeFraction, gSplineState);
    for (i = 0; i < 4; i++) {
        result[0] += weights[i] * gSplineKeyframe[i][1];
        result[1] += weights[i] * gSplineKeyframe[i][2];
        result[2] += weights[i] * gSplineKeyframe[i][3];
    }

    gSplineKeyframeFraction += (gSplineKeyframe[0][0] * construct_float(1.0f / 1000.0f));
    if (gSplineKeyframeFraction >= 1) {
        gSplineKeyframe++;
        gSplineKeyframeFraction--;
        switch (gSplineState) {
            case CURVE_END_2:
                hasEnded = TRUE;
                break;
            case CURVE_MIDDLE:
                if (gSplineKeyframe[2][0] == 0) {
                    gSplineState = CURVE_END_1;
                }
                break;
            default:
                gSplineState++;
                break;
        }
    }

    return hasEnded;
}

/**************************************************
 *                    RAYCASTING                  *
 **************************************************/

#define RAY_OFFSET 30.0f /* How many units to extrapolate surfaces when testing for a raycast */
#define RAY_STEPS      4 /* How many steps to do when casting rays, default to quartersteps.  */

/**
 * @brief Checks if a ray intersects a surface using Möller–Trumbore intersection algorithm.
 *
 * @param orig is the starting point of the ray.
 * @param dir is the normalized ray direction.
 * @param dir_length is the length of the ray.
 * @param surface is the surface to check.
 * @param hit_pos returns the position on the surface where the ray intersects it.
 * @param length returns the distance from the starting point to the hit position.
 * @return s32 TRUE if the ray intersects a surface.
 */
s32 ray_surface_intersect(Vec3f orig, Vec3f dir, f32 dir_length, struct Surface *surface, Vec3f hit_pos, f32 *length) {
    // Ignore certain surface types.
    if ((surface->type == SURFACE_INTANGIBLE) || (surface->flags & SURFACE_FLAG_NO_CAM_COLLISION)) return FALSE;
    // Convert the vertices to Vec3f.
    Vec3f v0, v1, v2;
    vec3s_to_vec3f(v0, surface->vertex1);
    vec3s_to_vec3f(v1, surface->vertex2);
    vec3s_to_vec3f(v2, surface->vertex3);
    // Get surface normal and extend it by RAY_OFFSET.
    Vec3f norm;
    surface_normal_to_vec3f(norm, surface);
    vec3_mul_val(norm, RAY_OFFSET);
    // Move the face forward by RAY_OFFSET.
    vec3f_add(v0, norm);
    vec3f_add(v1, norm);
    vec3f_add(v2, norm);
    // Make 'e1' (edge 1) the vector from vertex 0 to vertex 1.
    Vec3f e1;
    vec3f_diff(e1, v1, v0);
    // Make 'e2' (edge 2) the vector from vertex 0 to vertex 2.
    Vec3f e2;
    vec3f_diff(e2, v2, v0);
    // Make 'h' the cross product of 'dir' and edge 2.
    Vec3f h;
    vec3f_cross(h, dir, e2);
    // Determine the cos(angle) difference between ray and surface normals.
    f32 det = vec3f_dot(e1, h);
    // Check if we're perpendicular from the surface.
    if ((det > construct_float(-NEAR_ZERO)) && (det < construct_float(NEAR_ZERO))) return FALSE;
    // Check if we're making contact with the surface.
    // Make f the inverse of the cos(angle) between ray and surface normals.
    f32 f = 1.0f / det; // invDet
    // Make 's' the vector from vertex 0 to 'orig'.
    Vec3f s;
    vec3f_diff(s, orig, v0);
    // Make 'u' the cos(angle) between vectors 's' and normals, divided by 'det'.
    f32 u = f * vec3f_dot(s, h);
    // Check if 'u' is within bounds.
    if ((u < 0.0f) || (u > 1.0f)) return FALSE;
    // Make 'q' the cross product of 's' and edge 1.
    Vec3f q;
    vec3f_cross(q, s, e1);
    // Make 'v' the cos(angle) between the ray and 'q', divided by 'det'.
    f32 v = f * vec3f_dot(dir, q);
    // Check if 'v' is within bounds.
    if ((v < 0.0f) || ((u + v) > 1.0f)) return FALSE;
    // Get the length between our origin and the surface contact point.
    // Make '*length' the cos(angle) betqwwn edge 2 and 'q', divided by 'det'.
    *length = f * vec3f_dot(e2, q);
    // Check if the length to the hit point is shorter than the ray length.
    if ((*length <= construct_float(NEAR_ZERO)) || (*length > dir_length)) return FALSE;
    // Successful contact.
    // Make 'add_dir' into 'dir' scaled by 'length'.
    Vec3f add_dir;
    vec3_prod_val(add_dir, dir, *length);
    // Make 'hit_pos' into the sum of 'orig' and 'add_dir'.
    vec3f_sum(hit_pos, orig, add_dir);
    return TRUE;
}

void find_surface_on_ray_list(struct SurfaceNode *list, Vec3f orig, Vec3f dir, f32 dir_length, struct Surface **hit_surface, Vec3f hit_pos, f32 *max_length) {
    s32 hit;
    f32 length;
    Vec3f chk_hit_pos;
    f32 top, bottom;
#if PUPPYPRINT_DEBUG
    OSTime first = osGetTime();
#endif
    // Get upper and lower bounds of ray
    if (dir[1] >= 0.0f) {
        // Ray is upwards.
        top    = orig[1] + (dir[1] * dir_length);
        bottom = orig[1];
    } else {
        // Ray is downwards.
        top    = orig[1];
        bottom = orig[1] + (dir[1] * dir_length);
    }

    // Iterate through every surface of the list
    for (; list != NULL; list = list->next) {
        // Reject surface if out of vertical bounds
        if ((list->surface->lowerY > top) || (list->surface->upperY < bottom)) continue;
        // Check intersection between the ray and this surface
        hit = ray_surface_intersect(orig, dir, dir_length, list->surface, chk_hit_pos, &length);
        if (hit && (length <= *max_length)) {
            *hit_surface = list->surface;
            vec3f_copy(hit_pos, chk_hit_pos);
            *max_length = length;
        }
    }
#if PUPPYPRINT_DEBUG
    collisionTime[perfIteration] += (osGetTime() - first);
#endif
}

void find_surface_on_ray_cell(s32 cellX, s32 cellZ, Vec3f orig, Vec3f normalized_dir, f32 dir_length, struct Surface **hit_surface, Vec3f hit_pos, f32 *max_length, s32 flags) {
    // Skip if OOB
    if ((cellX >= 0) && (cellX <= (NUM_CELLS - 1)) && (cellZ >= 0) && (cellZ <= (NUM_CELLS - 1))) {
        f32 ny = normalized_dir[1];
        s32 rayUp   = (ny >  construct_float(NEAR_ONE));
        s32 rayDown = (ny < -construct_float(NEAR_ONE));
        
        for (s32 i = 0; i < NUM_SPATIAL_PARTITIONS; i++) {
            if ((rayDown && (i == SPATIAL_PARTITION_CEILS ))
             || (rayUp   && (i == SPATIAL_PARTITION_FLOORS))) {
                continue;
            }

            if (flags & BIT(i)) {
                // Iterate through each surface in this partition
                find_surface_on_ray_list( gStaticSurfacePartition[cellZ][cellX][i].next, orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
                find_surface_on_ray_list(gDynamicSurfacePartition[cellZ][cellX][i].next, orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
            }
        }
    }
}

void find_surface_on_ray(Vec3f orig, Vec3f dir, struct Surface **hit_surface, Vec3f hit_pos, s32 flags) {
    Vec3f normalized_dir;
    f32 step;
    s32 i;
    const f32 invcell = construct_float(1.0f / CELL_SIZE);

    // Set that no surface has been hit
    *hit_surface = NULL;
    vec3f_sum(hit_pos, orig, dir);

    // Get normalized direction
    f32 dir_length = vec3_mag(dir);
    f32 max_length = dir_length;
    vec3f_copy(normalized_dir, dir);
    vec3f_normalize(normalized_dir);

    // Get our cell coordinate
    f32 fCellX    = (orig[0] + LEVEL_BOUNDARY_MAX) * invcell;
    f32 fCellZ    = (orig[2] + LEVEL_BOUNDARY_MAX) * invcell;
    s32 cellX     = fCellX;
    s32 cellZ     = fCellZ;
    s32 cellPrevX = cellX;
    s32 cellPrevZ = cellZ;

    // Don't do DDA if straight down
    if ((normalized_dir[1] >= construct_float(NEAR_ONE)) || (normalized_dir[1] <= construct_float(-NEAR_ONE))) {
        find_surface_on_ray_cell(cellX, cellZ, orig, normalized_dir, dir_length, hit_surface, hit_pos, &max_length, flags);
        return;
    }

    // Get cells we cross using DDA
    f32 absDir0 = absf(dir[0]);
    f32 absDir2 = absf(dir[2]);
    if (absDir0 >= absDir2) {
        step = (RAY_STEPS * absDir0) * invcell;
    } else {
        step = (RAY_STEPS * absDir2) * invcell;
    }

    f32 dx = (dir[0] / step) * invcell;
    f32 dz = (dir[2] / step) * invcell;

    for (i = 0; i < step && *hit_surface == NULL; i++) {
        find_surface_on_ray_cell(cellX, cellZ, orig, normalized_dir, dir_length, hit_surface, hit_pos, &max_length, flags);

        // Move cell coordinate
        fCellX   += dx;
        fCellZ   += dz;
        cellPrevX = cellX;
        cellPrevZ = cellZ;
        cellX     = fCellX;
        cellZ     = fCellZ;

        if ((cellPrevX != cellX) && (cellPrevZ != cellZ)) {
            find_surface_on_ray_cell(cellX, cellPrevZ, orig, normalized_dir, dir_length, hit_surface, hit_pos, &max_length, flags);
            find_surface_on_ray_cell(cellPrevX, cellZ, orig, normalized_dir, dir_length, hit_surface, hit_pos, &max_length, flags);
        }
    }
}

void find_surface_on_ray_between_points(Vec3f pos1, Vec3f pos2, struct Surface **hit_surface, Vec3f hit_pos, s32 flags) {
    Vec3f dir;
    vec3f_diff(dir, pos2, pos1);
    find_surface_on_ray(pos1, dir, hit_surface, hit_pos, flags);
}

// Converts a floating point matrix to a fixed point matrix
// Makes some assumptions about certain fields in the matrix, which will always be true for valid matrices.
__attribute__((optimize("Os"))) __attribute__((aligned(32)))
void mtxf_to_mtx_fast(s16* dst, float* src) {
    int i;
    float scale = construct_float((float)0x00010000 / WORLD_SCALE);
    // Iterate over rows of values in the input matrix
    for (i = 0; i < 4; i++) {
        // Read the three input in the current row (assume the fourth is zero)
        float a = src[(4 * i) + 0];
        float b = src[(4 * i) + 1];
        float c = src[(4 * i) + 2];
        float a_scaled = mul_without_nop(a, scale);
        float b_scaled = mul_without_nop(b, scale);
        float c_scaled = mul_without_nop(c, scale);

        // Convert the three inputs to fixed
        s32 a_int = (s32)a_scaled;
        s32 b_int = (s32)b_scaled;
        s32 c_int = (s32)c_scaled;

        s32 c_high = (c_int & 0xFFFF0000);
        s32 c_low = (c_int << 16);

        // Write the integer part of a, as well as garbage into the next two bytes.
        // Those two bytes will get overwritten by the integer part of b.
        // This prevents needing to shift or mask the integer value of a.
        *(s32*)(&dst[(4 * i) +  0]) = a_int;
        // Write the fractional part of a
        dst[(4 * i) + 16] = (s16)a_int;

        // Write the integer part of b using swl to avoid needing to shift.
        swl(dst + (4 * i), b_int, 2);
        // Write the fractional part of b.
        dst[(4 * i) + 16 + 1] = (s16)b_int;

        // Write the integer part of c and two zeroes for the 4th column.
        *(s32*)(&dst[(4 * i) + 2]) = c_high;
        // Write the fractional part of c and two zeroes for the 4th column
        *(s32*)(&dst[(4 * i) + 16 + 2]) = c_low;
    }
    // Write 1.0 to the bottom right entry in the output matrix
    // The low half was already set to zero in the loop, so we only need
    //  to set the top half.
    dst[15] = 1;
}
