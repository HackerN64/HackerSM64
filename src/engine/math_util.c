#include <ultra64.h>

#include "sm64.h"
#include "engine/graph_node.h"
#include "math_util.h"
#include "surface_collision.h"
#include "extended_bounds.h"
#include "trig_tables.inc.c"
#include "surface_load.h"
#include "game/puppyprint.h"
#include "game/rendering_graph_node.h"

#include "config.h"

// unused Mtx(s)
s16 identityMtx[4][4] = { { 1, 0, 0, 0 }, { 0, 1, 0, 0 }, { 0, 0, 1, 0 }, { 0, 0, 0, 1 } };
s16 zeroMtx[4][4]     = { { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 } };

Vec3f gVec3fX    = {  1.0f,  0.0f,  0.0f };
Vec3f gVec3fY    = {  0.0f,  1.0f,  0.0f };
Vec3f gVec3fZ    = {  0.0f,  0.0f,  1.0f };
Vec3f gVec3fNX   = { -1.0f,  0.0f,  0.0f };
Vec3f gVec3fNY   = {  0.0f, -1.0f,  0.0f };
Vec3f gVec3fNZ   = {  0.0f,  0.0f, -1.0f };
Vec3f gVec3fZero = {  0.0f,  0.0f,  0.0f };
Vec3f gVec3fOne  = {  1.0f,  1.0f,  1.0f };
Vec3s gVec3sZero = {     0,     0,     0 };
Vec3i gVec3iZero = {     0,     0,     0 };
Vec3s gVec3sOne  = {     1,     1,     1 };

/// From Wiseguy
static inline s32 asm_roundf(f32 in) {
    f32 tmp;
    s32 out;
    __asm__("round.w.s %0,%1" : "=f" (tmp) : "f" (in));
    __asm__("mfc1 %0,%1" : "=r" (out) : "f" (tmp));
    return out;
}

f32 roundf(f32 x) {
    return asm_roundf(x);
}

static inline float asm_absf(float in) {
    f32 out;
    __asm__("abs.s %0,%1" : "=f" (out) : "f" (in));
    return out;
}

f32 absf(f32 x) {
    return asm_absf(x);
}

/// Returns the lowest of three values.
s32 min_3i(s32 a0, s32 a1, s32 a2) { if (a1 < a0) a0 = a1; if (a2 < a0) a0 = a2; return a0; }
f32 min_3f(f32 a0, f32 a1, f32 a2) { if (a1 < a0) a0 = a1; if (a2 < a0) a0 = a2; return a0; }

/// Returns the highest of three values.
s32 max_3i(s32 a0, s32 a1, s32 a2) { if (a1 > a0) a0 = a1; if (a2 > a0) a0 = a2; return a0; }
f32 max_3f(f32 a0, f32 a1, f32 a2) { if (a1 > a0) a0 = a1; if (a2 > a0) a0 = a2; return a0; }

/// Copy vector 'src' to 'dest'
void vec3f_copy(Vec3f dest, Vec3f src) {
    register u32 x = ((u32 *) src)[0];
    register u32 y = ((u32 *) src)[1];
    register u32 z = ((u32 *) src)[2];
    ((u32 *) dest)[0] = x;
    ((u32 *) dest)[1] = y;
    ((u32 *) dest)[2] = z;
}

/// Set vector 'dest' to (x, y, z)
inline void vec3f_set(Vec3f dest, f32 x, f32 y, f32 z) {
    vec3_set(dest, x, y, z);
}

/// Add vector 'a' to 'dest'
void vec3f_add(Vec3f dest, Vec3f a) {
    register f32 *temp = (f32 *)dest;
    register s32 j;
    register f32 sum, sum2;
    for (j = 0; j < 3; j++) {
        sum = *a;
        a++;
        sum2 = *temp;
        temp++;
        temp[-1] = (sum + sum2);
    }
}

/// Make 'dest' the sum of vectors a and b.
void vec3f_sum(Vec3f dest, Vec3f a, Vec3f b) {
    register f32 *temp = (f32 *)dest;
    register s32 j;
    register f32 x, y;
    for (j = 0; j < 3; j++) {
        x = *a;
        a++;
        y = *b;
        b++;
        *temp = x + y;
        temp++;
    }
}

/// Make 'dest' the difference of vectors a and b.
void vec3f_diff(Vec3f dest, Vec3f a, Vec3f b) {
    vec3_diff(dest, a, b);
}

/// Copy vector src to dest
void vec3s_copy(Vec3s dest, Vec3s src) {
    register s16 x = src[0];
    register s16 y = src[1];
    register s16 z = src[2];
    dest[0] = x;
    dest[1] = y;
    dest[2] = z;
}

/// Set vector 'dest' to (x, y, z)
inline void vec3s_set(Vec3s dest, s16 x, s16 y, s16 z) {
    vec3_set(dest, x, y, z);
}

/// Add vector a to 'dest'
void vec3s_add(Vec3s dest, Vec3s a) {
    vec3_add(dest, a);
}

/// Make 'dest' the sum of vectors a and b.
void vec3s_sum(Vec3s dest, Vec3s a, Vec3s b) {
    vec3_sum(dest, a, b);
}

/// Subtract vector a from 'dest'
void vec3s_sub(Vec3s dest, Vec3s a) {
    vec3_sub(dest, a);
}

void vec3f_sub(Vec3f dest, Vec3f src) {
    vec3_sub(dest, src);
}

f32 vec3f_dot(Vec3f a, Vec3f b) {
    return vec3_dot(a, b);
}

/**
 * Convert float vector a to a short vector 'dest' by rounding the components
 * to the nearest integer.
 */
void vec3f_to_vec3s(Vec3s dest, Vec3f a) {
    // add/subtract 0.5 in order to round to the nearest s32 instead of truncating
    dest[0] = a[0] + ((a[0] > 0) ? 0.5f : -0.5f);
    dest[1] = a[1] + ((a[1] > 0) ? 0.5f : -0.5f);
    dest[2] = a[2] + ((a[2] > 0) ? 0.5f : -0.5f);
}

/**
 * Set 'dest' the normal vector of a triangle with vertices a, b and c.
 * It is similar to vec3f_cross, but it calculates the vectors (c-b) and (b-a)
 * at the same time.
 */
void find_vector_perpendicular_to_plane(Vec3f dest, Vec3f a, Vec3f b, Vec3f c) {
    dest[0] = (b[1] - a[1]) * (c[2] - b[2]) - (c[1] - b[1]) * (b[2] - a[2]);
    dest[1] = (b[2] - a[2]) * (c[0] - b[0]) - (c[2] - b[2]) * (b[0] - a[0]);
    dest[2] = (b[0] - a[0]) * (c[1] - b[1]) - (c[0] - b[0]) * (b[1] - a[1]);
}

/// Make vector 'dest' the cross product of vectors a and b.
void vec3f_cross(Vec3f dest, Vec3f a, Vec3f b) {
    vec3_cross(dest, a, b);
}

/// Scale vector 'dest' so it has length 1
void vec3f_normalize(Vec3f dest) {
    f32 mag = sqrtf(sqr(dest[0]) + sqr(dest[1]) + sqr(dest[2]));
    if (mag > NEAR_ZERO) {
        register f32 invsqrt = 1.0f / mag;
        vec3_mul_val(dest, invsqrt);
    } else {
        dest[0] = 0;
        dest[1] = 1;
        dest[2] = 0;
    }
}

/// Scale vector 'dest' so it has length -1
void vec3f_normalize_negative(Vec3f dest) {
    f32 mag = sqrtf(sqr(dest[0]) + sqr(dest[1]) + sqr(dest[2]));
    if (mag > NEAR_ZERO) {
        register f32 invsqrt = -1.0f / mag;
        vec3_mul_val(dest, invsqrt);
    } else {
        dest[0] = 0;
        dest[1] = 1;
        dest[2] = 0;
    }
}

struct CopyMe {
    f32 a[4 * 4];
};

/// Copy matrix 'src' to 'dest'
void mtxf_copy(register Mat4 dest, register Mat4 src) {
    *((struct CopyMe *) dest) = *((struct CopyMe *) src);
}

/**
 * Set mtx to the identity matrix
 */
void mtxf_identity(register Mat4 mtx) {
    s32 i;
    f32 *dest;
    for (dest = (f32 *) mtx + 1, i = 0; i < 14; dest++, i++) {
        *dest = 0;
    }
    for (dest = (f32 *) mtx, i = 0; i < 4; dest += 5, i++) {
        *((u32 *) dest) = 0x3F800000;
    }
}

/**
 * Set dest to a translation matrix of vector b
 */
void mtxf_translate(Mat4 dest, Vec3f b) {
    register s32 i;
    register f32 *pen;
    for (pen = (f32 *) dest + 1, i = 0; i < 12; pen++, i++) {
        *pen = 0;
    }
    for (pen = (f32 *) dest, i = 0; i < 4; pen += 5, i++) {
        *((u32 *) pen) = 0x3F800000;
    }
    vec3f_copy(&dest[3][0], &b[0]);
}

void mtxf_rot_trans_mul(Vec3s rot, Vec3f trans, Mat4 dest, Mat4 src) {
    register f32 sx = sins(rot[0]);
    register f32 cx = coss(rot[0]);
    register f32 sy = sins(rot[1]);
    register f32 cy = coss(rot[1]);
    register f32 sz = sins(rot[2]);
    register f32 cz = coss(rot[2]);
    register Vec3f entry;

    entry[0] = (cy * cz);
    entry[1] = (cy * sz);
    entry[2] = -sy;
    dest[0][0] = entry[0] * src[0][0] + entry[1] * src[1][0] + entry[2] * src[2][0];
    dest[0][1] = entry[0] * src[0][1] + entry[1] * src[1][1] + entry[2] * src[2][1];
    dest[0][2] = entry[0] * src[0][2] + entry[1] * src[1][2] + entry[2] * src[2][2];

    entry[1] = (sx * sy);
    entry[0] = (entry[1] * cz) - (cx * sz);
    entry[1] = (entry[1] * sz) + (cx * cz);
    entry[2] = (sx * cy);
    dest[1][0] = entry[0] * src[0][0] + entry[1] * src[1][0] + entry[2] * src[2][0];
    dest[1][1] = entry[0] * src[0][1] + entry[1] * src[1][1] + entry[2] * src[2][1];
    dest[1][2] = entry[0] * src[0][2] + entry[1] * src[1][2] + entry[2] * src[2][2];

    entry[1] = (cx * sy);
    entry[0] = (entry[1] * cz) + (sx * sz);
    entry[1] = (entry[1] * sz) - (sx * cz);
    entry[2] = (cx * cy);
    dest[2][0] = entry[0] * src[0][0] + entry[1] * src[1][0] + entry[2] * src[2][0];
    dest[2][1] = entry[0] * src[0][1] + entry[1] * src[1][1] + entry[2] * src[2][1];
    dest[2][2] = entry[0] * src[0][2] + entry[1] * src[1][2] + entry[2] * src[2][2];

    vec3_copy(entry, trans);
    dest[3][0] = entry[0] * src[0][0] + entry[1] * src[1][0] + entry[2] * src[2][0] + src[3][0];
    dest[3][1] = entry[0] * src[0][1] + entry[1] * src[1][1] + entry[2] * src[2][1] + src[3][1];
    dest[3][2] = entry[0] * src[0][2] + entry[1] * src[1][2] + entry[2] * src[2][2] + src[3][2];
    dest[0][3] = dest[1][3] = dest[2][3] = 0;
    ((u32 *) dest)[15] = 0x3F800000;
}

/**
 * Set mtx to a look-at matrix for the camera. The resulting transformation
 * transforms the world as if there exists a camera at position 'from' pointed
 * at the position 'to'. The up-vector is assumed to be (0, 1, 0), but the 'roll'
 * angle allows a bank rotation of the camera.
 */
void mtxf_lookat(Mat4 mtx, Vec3f from, Vec3f to, s32 roll) {
    Vec3f colX, colY, colZ;
    register f32 dx = (to[0] - from[0]);
    register f32 dz = (to[2] - from[2]);
    register f32 invLength = sqrtf(sqr(dx) + sqr(dz));
    invLength = -(1.0f / MAX(invLength, NEAR_ZERO));
    dx *= invLength;
    dz *= invLength;
    f32 sr  = sins(roll);
    colY[1] = coss(roll);
    colY[0] = ( sr * dz);
    colY[2] = (-sr * dx);
    vec3_diff(colZ, to, from);
    vec3f_normalize_negative(colZ);
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
 * Build a matrix that rotates around the z axis, then the x axis, then the y
 * axis, and then translates.
 */
void mtxf_rotate_zxy_and_translate(Mat4 dest, Vec3f translate, Vec3s rotate) {
    register f32 sx   = sins(rotate[0]);
    register f32 cx   = coss(rotate[0]);
    register f32 sy   = sins(rotate[1]);
    register f32 cy   = coss(rotate[1]);
    register f32 sz   = sins(rotate[2]);
    register f32 cz   = coss(rotate[2]);
    register f32 cycz = (cy * cz);
    register f32 cysz = (cy * sz);
    register f32 sycz = (sy * cz);
    register f32 sysz = (sy * sz);
    dest[0][0] = ((sx * sysz) + cycz);
    dest[1][0] = ((sx * sycz) - cysz);
    dest[2][0] = (cx * sy);
    dest[0][1] = (cx * sz);
    dest[1][1] = (cx * cz);
    dest[2][1] = -sx;
    dest[0][2] = ((sx * cysz) - sycz);
    dest[1][2] = ((sx * cycz) + sysz);
    dest[2][2] = cx * cy;
    vec3_copy(dest[3], translate);
    dest[0][3] = dest[1][3] = dest[2][3] = 0.;
    ((u32 *) dest)[15] = 0x3F800000;
}

/**
 * Build a matrix that rotates around the x axis, then the y axis, then the z
 * axis, and then translates.
 */
void mtxf_rotate_xyz_and_translate(Mat4 dest, Vec3f b, Vec3s c) {
    register f32 sx   = sins(c[0]);
    register f32 cx   = coss(c[0]);
    register f32 sy   = sins(c[1]);
    register f32 cy   = coss(c[1]);
    register f32 sz   = sins(c[2]);
    register f32 cz   = coss(c[2]);
    register f32 cxsz = (cx * sz);
    register f32 cxcz = (cx * cz);
    register f32 sxsz = (sx * sz);
    register f32 sxcz = (sx * cz);
    dest[0][0] = (cy * cz);
    dest[0][1] = (cy * sz);
    dest[0][2] = -sy;
    dest[1][0] = ((sxcz * sy) - cxsz);
    dest[1][1] = ((sxsz * sy) + cxcz);
    dest[1][2] = (sx * cy);
    dest[2][0] = ((cxcz * sy) + sxsz);
    dest[2][1] = ((cxsz * sy) - sxcz);
    dest[2][2] = (cx * cy);
    dest[0][3] = 0;
    dest[1][3] = 0;
    dest[2][3] = 0;
    vec3_copy(dest[3], b);
    ((u32 *) dest)[15] = 0x3F800000;
}

/**
 * Set 'dest' to a transformation matrix that turns an object to face the camera.
 * 'mtx' is the look-at matrix from the camera
 * 'position' is the position of the object in the world
 * 'angle' rotates the object while still facing the camera.
 */
void mtxf_billboard(Mat4 dest, Mat4 mtx, Vec3f position, s32 angle) {
    register s32 i;
    register f32 *temp, *temp2;
    temp = (f32 *)dest;
    for (i = 0; i < 16; i++) {
        *temp = 0;
        temp++;
    }
    if (angle == 0x0) {
        dest[0][0] = 1;
        dest[0][1] = 0;
        dest[1][0] = 0;
        dest[1][1] = 1;
    } else {
        dest[0][0] = coss(angle);
        dest[0][1] = sins(angle);
        dest[1][0] = -dest[0][1];
        dest[1][1] =  dest[0][0];
    }
    ((u32 *) dest)[10] = 0x3F800000;
    dest[2][3] = 0;
    ((u32 *) dest)[15] = 0x3F800000;

    temp = (f32 *)dest;
    temp2 = (f32 *)mtx;
    for (i = 0; i < 3; i++) {
        temp[12] = (temp2[0] * position[0]) + (temp2[4] * position[1]) + (temp2[8] * position[2]) + temp2[12];
        temp++;
        temp2++;
    }
}

/**
 * Set 'dest' to a transformation matrix that aligns an object with the terrain
 * based on the normal. Used for enemies.
 * 'upDir' is the terrain normal
 * 'yaw' is the angle which it should face
 * 'pos' is the object's position in the world
 */
void mtxf_align_terrain_normal(Mat4 dest, Vec3f upDir, Vec3f pos, s32 yaw) {
    Vec3f lateralDir;
    Vec3f leftDir;
    Vec3f forwardDir;

    vec3_set(lateralDir, sins(yaw), 0, coss(yaw));
    vec3f_normalize(upDir);

    vec3_cross(leftDir, upDir, lateralDir);
    vec3f_normalize(leftDir);

    vec3_cross(forwardDir, leftDir, upDir);
    vec3f_normalize(forwardDir);

    dest[0][0] = leftDir[0];
    dest[0][1] = leftDir[1];
    dest[0][2] = leftDir[2];
    dest[3][0] = pos[0];

    dest[1][0] = upDir[0];
    dest[1][1] = upDir[1];
    dest[1][2] = upDir[2];
    dest[3][1] = pos[1];

    dest[2][0] = forwardDir[0];
    dest[2][1] = forwardDir[1];
    dest[2][2] = forwardDir[2];
    dest[3][2] = pos[2];

    dest[0][3] = 0;
    dest[1][3] = 0;
    dest[2][3] = 0;
    ((u32 *) dest)[15] = 0x3F800000;
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
    struct Surface *floor;
    Vec3f point0, point1, point2;
    Vec3f forward;
    Vec3f xColumn, yColumn, zColumn;
    f32 avgY;
    f32 minY = -radius * 3;

    point0[0] = pos[0] + radius * sins(yaw + 0x2AAA);
    point0[2] = pos[2] + radius * coss(yaw + 0x2AAA);
    point1[0] = pos[0] + radius * sins(yaw + 0x8000);
    point1[2] = pos[2] + radius * coss(yaw + 0x8000);
    point2[0] = pos[0] + radius * sins(yaw + 0xD555);
    point2[2] = pos[2] + radius * coss(yaw + 0xD555);

    point0[1] = find_floor(point0[0], pos[1] + 150, point0[2], &floor);
    point1[1] = find_floor(point1[0], pos[1] + 150, point1[2], &floor);
    point2[1] = find_floor(point2[0], pos[1] + 150, point2[2], &floor);

    if (point0[1] - pos[1] < minY) {
        point0[1] = pos[1];
    }

    if (point1[1] - pos[1] < minY) {
        point1[1] = pos[1];
    }

    if (point2[1] - pos[1] < minY) {
        point2[1] = pos[1];
    }

    avgY = (point0[1] + point1[1] + point2[1]) / 3;

    vec3_set(forward, sins(yaw), 0, coss(yaw));
    find_vector_perpendicular_to_plane(yColumn, point0, point1, point2);
    vec3f_normalize(yColumn);
    vec3_cross(xColumn, yColumn, forward);
    vec3f_normalize(xColumn);
    vec3_cross(zColumn, xColumn, yColumn);
    vec3f_normalize(zColumn);
    vec3f_copy(mtx[0], xColumn);
    vec3f_copy(mtx[1], yColumn);
    vec3f_copy(mtx[2], zColumn);

    mtx[3][0] = pos[0];
    mtx[3][1] = (avgY < pos[1]) ? pos[1] : avgY;
    mtx[3][2] = pos[2];

    mtx[0][3] = 0;
    mtx[1][3] = 0;
    mtx[2][3] = 0;
    ((u32 *) mtx)[15] = 0x3F800000;
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
    register Vec3f entry;
    register f32 *temp = (f32 *)a;
    register f32 *temp2 = (f32 *)dest;
    register f32 *temp3;
    register s32 i;
    for (i = 0; i < 16; i++) {
        vec3_copy(entry, temp);
        temp3 = (f32 *)b;
        for (; (i & 3) !=3; i++) {
            *temp2 = entry[0] * temp3[0] + entry[1] * temp3[4] + entry[2] * temp3[8];
            temp2++;
            temp3++;
        }
        *temp2 = 0;
        temp += 4;
        temp2++;
    }
    vec3_add(&dest[3][0], &b[3][0]);
    ((u32 *) dest)[15] = 0x3F800000;
}

/**
 * Set matrix 'dest' to 'mtx' scaled by vector s
 */
void mtxf_scale_vec3f(Mat4 dest, Mat4 mtx, register Vec3f s) {
    register f32 *temp = (f32 *)dest;
    register f32 *temp2 = (f32 *)mtx;
    register s32 i;

    for (i = 0; i < 4; i++) {
        temp[0] = temp2[0] * s[0];
        temp[4] = temp2[4] * s[1];
        temp[8] = temp2[8] * s[2];
        temp[12] = temp2[12];
        temp++;
        temp2++;
    }
}

/**
 * Multiply a vector with a transformation matrix, which applies the transformation
 * to the point. Note that the bottom row is assumed to be [0, 0, 0, 1], which is
 * true for transformation matrices if the translation has a w component of 1.
 */
void mtxf_mul_vec3s(Mat4 mtx, Vec3s b) {
    register f32 x = b[0];
    register f32 y = b[1];
    register f32 z = b[2];
    register f32 *temp2 = (f32 *)mtx;
    register s32 i;
    register s16 *c = b;
    for (i = 0; i < 3; i++) {
        c[0] = (x * temp2[0]) + (y * temp2[4]) + (z * temp2[8]) + temp2[12];
        c++;
        temp2++;
    }
}

/**
 * Set 'mtx' to a transformation matrix that rotates around the z axis.
 */
#define MATENTRY(a, b)                          \
    ((s16 *) mtx)[a     ] = (((s32) b) >> 16);  \
    ((s16 *) mtx)[a + 16] = (((s32) b) & 0xFFFF);
void mtxf_rotate_xy(Mtx *mtx, s32 angle) {
    register s32 i = coss(angle) * 65536;
    register s32 j = sins(angle) * 65536;
    register f32 *temp = (f32 *)mtx;
    register s32 k;
    for (k = 0; k < 16; k++) {
        *temp = 0;
        temp++;
    }
    MATENTRY(0,  i)
    MATENTRY(1,  j)
    MATENTRY(4, -j)
    MATENTRY(5,  i)
    ((s16 *) mtx)[10] = 1;
    ((s16 *) mtx)[15] = 1;
}

/**
 * Extract a position given an object's transformation matrix and a camera matrix.
 * This is used for determining the world position of the held object: since objMtx
 * inherits the transformation from both the camera and Mario, it calculates this
 * by taking the camera matrix and inverting its transformation by first rotating
 * objMtx back from screen orientation to world orientation, and then subtracting
 * the camera position.
 */
void get_pos_from_transform_mtx(Vec3f dest, Mat4 objMtx, register Mat4 camMtx) {
    register s32 i;
    register f32 *temp1 = (f32 *)dest;
    register f32 *temp2 = (f32 *)camMtx;
    f32 y[3];
    register f32 *x = y;
    register f32 *temp3 = (f32 *)objMtx;

    for (i = 0; i < 3; i++) {
        *x = (temp3[12] - temp2[12]);
        temp2++;
        temp3++;
        x = (f32 *)(((u32)x) + 4);
    }
    temp2 -=3;;
    for (i = 0; i < 3; i++) {
        *temp1 = x[-3] * temp2[0] + x[-2] * temp2[1] + x[-1] * temp2[2];
        temp1++;
        temp2 += 4;
    }
}


/**
 * Take the vector starting at 'from' pointed at 'to' an retrieve the length
 * of that vector, as well as the yaw and pitch angles.
 * Basically it converts the direction to spherical coordinates.
 */

/// Finds the horizontal distance between two vectors.
void vec3f_get_lateral_dist(Vec3f from, Vec3f to, f32 *lateralDist) {
    register f32 dx = (to[0] - from[0]);
    register f32 dz = (to[2] - from[2]);
    *lateralDist = sqrtf(sqr(dx) + sqr(dz));
}

/// Finds the squared horizontal distance between two vectors. Avoids a sqrtf call.
void vec3f_get_lateral_dist_squared(Vec3f from, Vec3f to, f32 *lateralDist) {
    register f32 dx = (to[0] - from[0]);
    register f32 dz = (to[2] - from[2]);
    *lateralDist = (sqr(dx) + sqr(dz));
}

/// Finds the distance between two vectors.
void vec3f_get_dist(Vec3f from, Vec3f to, f32 *dist) {
    register Vec3f d;
    vec3_diff(d, to, from);
    *dist = vec3_mag(d);
}

/// Finds the squared distance between two vectors. Avoids a sqrtf call.
void vec3f_get_dist_squared(Vec3f from, Vec3f to, f32 *dist) {
    register Vec3f d;
    vec3_diff(d, to, from);
    *dist = vec3_sumsq(d);
}

/// Finds the distance and yaw etween two vectors.
void vec3f_get_dist_and_yaw(Vec3f from, Vec3f to, f32 *dist, s16 *yaw) {
    register Vec3f d;
    vec3_diff(d, to, from);
    *dist = vec3_mag(d);
    *yaw = atan2s(d[2], d[0]);
}

/// Finds the pitch between two vectors.
void vec3f_get_pitch(Vec3f from, Vec3f to, s16 *pitch) {
    register Vec3f d;
    vec3_diff(d, to, from);
    *pitch = atan2s(sqrtf(sqr(d[0]) + sqr(d[2])), d[1]);
}

/// Finds the yaw between two vectors.
void vec3f_get_yaw(Vec3f from, Vec3f to, s16 *yaw) {
    register f32 dx = (to[0] - from[0]);
    register f32 dz = (to[2] - from[2]);
    *yaw = atan2s(dz, dx);
}

/// Finds the pitch and yaw between two vectors.
void vec3f_get_angle(Vec3f from, Vec3f to, s16 *pitch, s16 *yaw) {
    register Vec3f d;
    vec3_diff(d, to, from);
    *pitch = atan2s(sqrtf(sqr(d[0]) + sqr(d[2])), d[1]);
    *yaw = atan2s(d[2], d[0]);
}

/// Finds the horizontal distance and pitch between two vectors.
void vec3f_get_lateral_dist_and_pitch(Vec3f from, Vec3f to, f32 *lateralDist, Angle *pitch) {
    Vec3f d;
    vec3_diff(d, to, from);
    *lateralDist = sqrtf(sqr(d[0]) + sqr(d[2]));
    *pitch          = atan2s(*lateralDist, d[1]);
}

/// Finds the horizontal distance and yaw between two vectors.
void vec3f_get_lateral_dist_and_yaw(Vec3f from, Vec3f to, f32 *lateralDist, Angle *yaw) {
    register f32 dx = (to[0] - from[0]);
    register f32 dz = (to[2] - from[2]);
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
void vec3f_get_dist_and_angle(Vec3f from, Vec3f to, f32 *dist, s16 *pitch, s16 *yaw) {
    register f32 x = to[0] - from[0];
    register f32 y = to[1] - from[1];
    register f32 z = to[2] - from[2];
    register f32 xs = x * x;
    register f32 zs = z * z;

    *dist = sqrtf(xs + zs + y * y);
    *pitch = atan2s(sqrtf(xs + zs), y);
    *yaw = atan2s(z, x);
}
void vec3s_get_dist_and_angle(Vec3s from, Vec3s to, s16 *dist, Angle *pitch, Angle *yaw) {
    Vec3s d;
    vec3_diff(d, to, from);
    register f32 xz = (sqr(d[0]) + sqr(d[2]));
    *dist           = sqrtf(xz + sqr(d[1]));
    *pitch          = atan2s(sqrtf(xz), d[1]);
    *yaw            = atan2s(d[2], d[0]);
}

/// Finds the distance, horizontal distance, and angles between two vectors.
void vec3f_get_dist_and_lateral_dist_and_angle(Vec3f from, Vec3f to, f32 *dist, f32 *lateralDist, Angle *pitch, Angle *yaw) {
    Vec3f d;
    vec3_diff(d, to, from);
    register f32 xz = (sqr(d[0]) + sqr(d[2]));
    *dist           = sqrtf(xz + sqr(d[1]));
    *lateralDist    = sqrtf(xz);
    *pitch          = atan2s(*lateralDist, d[1]);
    *yaw            = atan2s(d[2], d[0]);
}

/**
 * Construct the 'to' point which is distance 'dist' away from the 'from' position,
 * and has the angles pitch and yaw.
 */
void vec3f_set_dist_and_angle(Vec3f from, Vec3f to, f32 dist, s32 pitch, s32 yaw) {
    to[0] = from[0] + dist * coss(pitch) * sins(yaw);
    to[1] = from[1] + dist * sins(pitch);
    to[2] = from[2] + dist * coss(pitch) * coss(yaw);
}
void vec3s_set_dist_and_angle(Vec3s from, Vec3s to, s16 dist, Angle32 pitch, Angle32 yaw) {
    register f32 dcos = (dist * coss(pitch));
    to[0] = (from[0] + (dcos * sins(yaw  )));
    to[1] = (from[1] + (dist * sins(pitch)));
    to[2] = (from[2] + (dcos * coss(yaw  )));
}

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
    return current;
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

s32 approach_f32_signed(f32 *current, f32 target, f32 inc) {
    s32 reachedTarget = FALSE;
    *current += inc;
    if (inc >= 0.0f) {
        if (*current > target) {
            *current = target;
            reachedTarget = TRUE;
        }
    } else {
        if (*current < target) {
            *current = target;
            reachedTarget = TRUE;
        }
    }
    return reachedTarget;
}

/**
 * Approaches an f32 value by taking the difference between the target and current value
 * and adding a fraction of that to the current value.
 * Edits the current value directly, returns TRUE if the target has been reached, FALSE otherwise.
 */
s32 approach_f32_asymptotic_bool(f32 *current, f32 target, f32 multiplier) {
    if (multiplier > 1.f) {
        multiplier = 1.f;
    }
    *current = *current + (target - *current) * multiplier;
    return (*current != target);
}

/**
 * Nearly the same as the above function, returns new value instead.
 */
f32 approach_f32_asymptotic(f32 current, f32 target, f32 multiplier) {
    current = current + (target - current) * multiplier;
    return current;
}

/**
 * Approaches an s16 value in the same fashion as approach_f32_asymptotic_bool, returns TRUE if target
 * is reached. Note: Since this function takes integers as parameters, the last argument is the
 * reciprocal of what it would be in the previous two functions.
 */
s32 approach_s16_asymptotic_bool(s16 *current, s16 target, s16 divisor) {
    s16 temp = *current;

    if (divisor == 0) {
        *current = target;
    } else {
        temp -= target;
        temp -= temp / divisor;
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
        temp -= temp / divisor;
        temp += target;
        current = temp;
    }
    return current;
}

s16 abs_angle_diff(s16 a0, s16 a1) {
    s16 diff = (a1 - a0);
    if (diff == -0x8000) diff = -0x7FFF;
    return ABSI(diff);
}

/**
 * Helper function for atan2s. Does a look up of the arctangent of y/x assuming
 * the resulting angle is in range [0, 0x2000] (1/8 of a circle).
 */
#define atan2_lookup(y, x) ((x == 0) ? 0x0 : atans((y) / (x)))

/**
 * Compute the angle from (0, 0) to (x, y) as a s16. Given that terrain is in
 * the xz-plane, this is commonly called with (z, x) to get a yaw angle.
 */
s16 atan2s(f32 y, f32 x) {
    u16 ret;
    if (x >= 0) {
        if (y >= 0) {
            if (y >= x) {
                ret = atan2_lookup(x, y);
            } else {
                ret = (0x4000 - atan2_lookup(y, x));
            }
        } else {
            y = -y;
            if (y < x) {
                ret = (0x4000 + atan2_lookup(y, x));
            } else {
                ret = (0x8000 - atan2_lookup(x, y));
            }
        }
    } else {
        x = -x;
        if (y < 0) {
            y = -y;
            if (y >= x) {
                ret = (0x8000 + atan2_lookup(x, y));
            } else {
                ret = (0xC000 - atan2_lookup(y, x));
            }
        } else {
            if (y < x) {
                ret = (0xC000 + atan2_lookup(y, x));
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
    return ((f32) atan2s(y, x) * M_PI / 0x8000);
}

// Variables for a spline curve animation (used for the flight path in the grand star cutscene)
Vec4s *gSplineKeyframe;
float gSplineKeyframeFraction;
int gSplineState;

#define CURVE_BEGIN_1 0x1
#define CURVE_BEGIN_2 0x2
#define CURVE_MIDDLE  0x3
#define CURVE_END_1   0x4
#define CURVE_END_2   0x5

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
 * 1: [1, 0, 0, 0]
 * 1->2: [0, 3/12, 7/12, 2/12]
 * 2->3: [0, 1/6, 4/6, 1/6]
 * 3->3: [0, 1/6, 4/6, 1/6] (repeats)
 * 3->4: [0, 1/6, 4/6, 1/6]
 * 4->5: [0, 2/12, 7/12, 3/12]
 * 5: [0, 0, 0, 1]
 *
 * I suspect that the weight formulas will give a 3rd degree B-spline with the
 * common uniform clamped knot vector, e.g. for n points:
 * [0, 0, 0, 0, 1, 2, ... n-1, n, n, n, n]
 * TODO: verify the classification of the spline / figure out how polynomials were computed
 */
void spline_get_weights(Vec4f result, f32 t, UNUSED s32 c) {
    f32 tinv = 1 - t;
    f32 tinv2 = tinv * tinv;
    f32 tinv3 = tinv2 * tinv;
    f32 t2 = t * t;
    f32 t3 = t2 * t;

    switch (gSplineState) {
        case CURVE_BEGIN_1:
            result[0] = tinv3;
            result[1] = t3 * 1.75f - t2 * 4.5f + t * 3.0f;
            result[2] = -t3 * (11 / 12.0f) + t2 * 1.5f;
            result[3] = t3 * (1 / 6.0f);
            break;
        case CURVE_BEGIN_2:
            result[0] = tinv3 * 0.25f;
            result[1] = t3 * (7 / 12.0f) - t2 * 1.25f + t * 0.25f + (7 / 12.0f);
            result[2] = -t3 * 0.5f + t2 * 0.5f + t * 0.5f + (1 / 6.0f);
            result[3] = t3 * (1 / 6.0f);
            break;
        case CURVE_MIDDLE:
            result[0] = tinv3 * (1 / 6.0f);
            result[1] = t3 * 0.5f - t2 + (4 / 6.0f);
            result[2] = -t3 * 0.5f + t2 * 0.5f + t * 0.5f + (1 / 6.0f);
            result[3] = t3 * (1 / 6.0f);
            break;
        case CURVE_END_1:
            result[0] = tinv3 * (1 / 6.0f);
            result[1] = -tinv3 * 0.5f + tinv2 * 0.5f + tinv * 0.5f + (1 / 6.0f);
            result[2] = tinv3 * (7 / 12.0f) - tinv2 * 1.25f + tinv * 0.25f + (7 / 12.0f);
            result[3] = t3 * 0.25f;
            break;
        case CURVE_END_2:
            result[0] = tinv3 * (1 / 6.0f);
            result[1] = -tinv3 * (11 / 12.0f) + tinv2 * 1.5f;
            result[2] = tinv3 * 1.75f - tinv2 * 4.5f + tinv * 3.0f;
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

    if ((gSplineKeyframeFraction += gSplineKeyframe[0][0] / 1000.0f) >= 1) {
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

#define RAY_OFFSET 30.0f /*How many units to extrapolate surfaces when testing for a raycast*/
#define RAY_STEPS 4 /*How many steps to do when casting rays, default to quartersteps.*/

s32 ray_surface_intersect(Vec3f orig, Vec3f dir, f32 dir_length, struct Surface *surface, Vec3f hit_pos, f32 *length) {
    Vec3f v0, v1, v2, e1, e2, h, s, q;
    f32 a, f, u, v;
    Vec3f add_dir;
    Vec3f norm;
    // Ignore certain surface types.
    if (surface->type == SURFACE_INTANGIBLE || surface->flags & SURFACE_FLAG_NO_CAM_COLLISION) return FALSE;
    // Get surface normal and some other stuff
    vec3_set(norm, 0, surface->normal.y, 0);
    vec3_mul_val(norm, RAY_OFFSET);
    vec3_copy(v0, surface->vertex1);
    vec3_copy(v1, surface->vertex2);
    vec3_copy(v2, surface->vertex3);
    vec3f_add( v0, norm);
    vec3f_add( v1, norm);
    vec3f_add( v2, norm);
    vec3f_diff(e1, v1, v0);
    vec3f_diff(e2, v2, v0);
    vec3f_cross(h, dir, e2);
    // Check if we're perpendicular from the surface
    a = vec3f_dot(e1, h);
    if ((a > -NEAR_ZERO) && (a < NEAR_ZERO)) return FALSE;
    // Check if we're making contact with the surface
    f = 1.0f / a;
    vec3f_diff(s, orig, v0);
    u = f * vec3f_dot(s, h);
    if ((u < 0.0f) || (u > 1.0f)) return FALSE;
    vec3f_cross(q, s, e1);
    v = f * vec3f_dot(dir, q);
    if ((v < 0.0f) || ((u + v) > 1.0f)) return FALSE;
    // Get the length between our origin and the surface contact point
    *length = f * vec3f_dot(e2, q);
    if (*length <= 0.00001 || *length > dir_length) return FALSE;
    // Successful contact
    vec3f_copy(add_dir, dir);
    vec3_mul_val(add_dir, *length);
    vec3_sum(hit_pos, orig, add_dir);
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
        top    = (orig[1] + (dir[1] * dir_length));
        bottom = orig[1];
    } else {
        top    = orig[1];
        bottom = (orig[1] + (dir[1] * dir_length));
    }

    // Iterate through every surface of the list
    for (; list != NULL; list = list->next) {
        // Reject surface if out of vertical bounds
        if (list->surface->lowerY > top || list->surface->upperY < bottom) continue;
        // Check intersection between the ray and this surface
        if ((hit = ray_surface_intersect(orig, dir, dir_length, list->surface, chk_hit_pos, &length)) && (length <= *max_length)) {
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
    if (cellX >= 0 && cellX <= (NUM_CELLS - 1) && cellZ >= 0 && cellZ <= (NUM_CELLS - 1)) {
        // Iterate through each surface in this partition
        if ((normalized_dir[1] > -NEAR_ONE) && (flags & RAYCAST_FIND_CEIL)) {
            find_surface_on_ray_list(gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_CEILS].next, orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
            find_surface_on_ray_list(gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_CEILS].next, orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
        }
        if ((normalized_dir[1] < NEAR_ONE) && (flags & RAYCAST_FIND_FLOOR)) {
            find_surface_on_ray_list(gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_FLOORS].next, orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
            find_surface_on_ray_list(gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_FLOORS].next, orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
        }
        if (flags & RAYCAST_FIND_WALL) {
            find_surface_on_ray_list(gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WALLS].next, orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
            find_surface_on_ray_list(gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WALLS].next, orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
        }
        if (flags & RAYCAST_FIND_WATER) {
            find_surface_on_ray_list(gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WATER].next, orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
            find_surface_on_ray_list(gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WATER].next, orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
        }
    }
}

void find_surface_on_ray(Vec3f orig, Vec3f dir, struct Surface **hit_surface, Vec3f hit_pos, s32 flags) {
    s32 cellZ, cellX, cellPrevX, cellPrevZ;
    f32 fCellZ, fCellX;
    Vec3f normalized_dir;
    f32 step;
    s32 i;

    // Set that no surface has been hit
    *hit_surface = NULL;
    vec3_sum(hit_pos, orig, dir);

    // Get normalized direction
    f32 dir_length = vec3_mag(dir);
    f32 max_length = dir_length;
    vec3f_copy(normalized_dir, dir);
    vec3f_normalize(normalized_dir);

    // Get our cell coordinate
    fCellX = (orig[0] + LEVEL_BOUNDARY_MAX) / CELL_SIZE;
    fCellZ = (orig[2] + LEVEL_BOUNDARY_MAX) / CELL_SIZE;
    cellX = fCellX;
    cellZ = fCellZ;
    cellPrevX = cellX;
    cellPrevZ = cellZ;

    // Don't do DDA if straight down
    if (normalized_dir[1] >= NEAR_ONE || normalized_dir[1] <= -NEAR_ONE) {
        find_surface_on_ray_cell(cellX, cellZ, orig, normalized_dir, dir_length, hit_surface, hit_pos, &max_length, flags);
        return;
    }

    // Get cells we cross using DDA
    if (ABS(dir[0]) >= ABS(dir[2])) {
        step = RAY_STEPS * ABS(dir[0]) / CELL_SIZE;
    } else {
        step = RAY_STEPS * ABS(dir[2]) / CELL_SIZE;
    }
    f32 dx = (dir[0] / step) / CELL_SIZE;
    f32 dz = (dir[2] / step) / CELL_SIZE;

    for (i = 0; i < step && *hit_surface == NULL; i++) {
        find_surface_on_ray_cell(cellX, cellZ, orig, normalized_dir, dir_length, hit_surface, hit_pos, &max_length, flags);

        // Move cell coordinate
        fCellX += dx;
        fCellZ += dz;
        cellPrevX = cellX;
        cellPrevZ = cellZ;
        cellX = fCellX;
        cellZ = fCellZ;

        if ((cellPrevX != cellX) && (cellPrevZ != cellZ)) {
            find_surface_on_ray_cell(cellX, cellPrevZ, orig, normalized_dir, dir_length, hit_surface, hit_pos, &max_length, flags);
            find_surface_on_ray_cell(cellPrevX, cellZ, orig, normalized_dir, dir_length, hit_surface, hit_pos, &max_length, flags);
        }
    }
}
