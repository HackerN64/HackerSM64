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

static u16 gRandomSeed16;

// Generate a pseudorandom integer from 0 to 65535 from the random seed, and update the seed.
u16 random_u16(void) {
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

// Get the maximum and minimum of three numbers at the same time.
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

/// Struct the same data size as a Mat4
struct CopyMat4 {
    f32 a[0x10];
};

/// Copy matrix 'src' to 'dest' by casting to a struct CopyMat4 pointer.
void mtxf_copy(Mat4 dest, Mat4 src) {
    *((struct CopyMat4 *) dest) = *((struct CopyMat4 *) src);
}

/// Set mtx to the identity matrix.

void mtxf_identity(Mat4 mtx) {
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.matrix);
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
void mtxf_translate(Mat4 dest, Vec3f b) {
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.matrix);
    s32 i;
    f32 *pen;
    for (pen = ((f32 *) dest + 1), i = 0; i < 12; pen++, i++) {
        *pen = 0;
    }
    for (pen = (f32 *) dest, i = 0; i < 4; pen += 5, i++) {
        *((u32 *) pen) = FLOAT_ONE;
    }
    vec3f_copy(&dest[3][0], &b[0]);
}

/// Build a matrix that rotates around the z axis, then the x axis, then the y axis, and then translates.
void mtxf_rotate_zxy_and_translate(Mat4 dest, Vec3f trans, Vec3s rot) {
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.matrix);
    f32 sx   = sins(rot[0]);
    f32 cx   = coss(rot[0]);
    f32 sy   = sins(rot[1]);
    f32 cy   = coss(rot[1]);
    f32 sz   = sins(rot[2]);
    f32 cz   = coss(rot[2]);
    f32 sysz = (sy * sz);
    f32 cycz = (cy * cz);
    dest[0][0] = ((sysz * sx) + cycz);
    f32 cysz = (cy * sz);
    f32 sycz = (sy * cz);
    dest[1][0] = ((sycz * sx) - cysz);
    dest[2][0] = (cx * sy);
    dest[0][1] = (cx * sz);
    dest[1][1] = (cx * cz);
    dest[2][1] = -sx;
    dest[0][2] = ((cysz * sx) - sycz);
    dest[1][2] = ((cycz * sx) + sysz);
    dest[2][2] = (cx * cy);
    vec3f_copy(dest[3], trans);
    MTXF_END(dest);
}

/// Build a matrix that rotates around the x axis, then the y axis, then the z axis, and then translates.
UNUSED void mtxf_rotate_xyz_and_translate(Mat4 dest, Vec3f trans, Vec3s rot) {
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.matrix);
    f32 sx   = sins(rot[0]);
    f32 cx   = coss(rot[0]);
    f32 sy   = sins(rot[1]);
    f32 cy   = coss(rot[1]);
    f32 sz   = sins(rot[2]);
    f32 cz   = coss(rot[2]);
    dest[0][0] = (cy * cz);
    dest[0][1] = (cy * sz);
    dest[0][2] = -sy;
    f32 sxcz = (sx * cz);
    f32 cxsz = (cx * sz);
    dest[1][0] = ((sxcz * sy) - cxsz);
    f32 sxsz = (sx * sz);
    f32 cxcz = (cx * cz);
    dest[1][1] = ((sxsz * sy) + cxcz);
    dest[1][2] = (sx * cy);
    dest[2][0] = ((cxcz * sy) + sxsz);
    dest[2][1] = ((cxsz * sy) - sxcz);
    dest[2][2] = (cx * cy);
    vec3f_copy(dest[3], trans);
    MTXF_END(dest);
}

/// Build a matrix that rotates around the z axis, then the x axis, then the y axis, and then translates and multiplies.
void mtxf_rotate_zxy_and_translate_and_mul(Vec3s rot, Vec3f trans, Mat4 dest, Mat4 src) {
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.matrix);
    f32 sx = sins(rot[0]);
    f32 cx = coss(rot[0]);
    f32 sy = sins(rot[1]);
    f32 cy = coss(rot[1]);
    f32 sz = sins(rot[2]);
    f32 cz = coss(rot[2]);
    Vec3f entry;
    f32 sysz = (sy * sz);
    f32 cycz = (cy * cz);
    entry[0] = ((sysz * sx) + cycz);
    entry[1] = (sz * cx);
    f32 cysz = (cy * sz);
    f32 sycz = (sy * cz);
    entry[2] = ((cysz * sx) - sycz);
    linear_mtxf_mul_vec3f(src, dest[0], entry);
    entry[0] = ((sycz * sx) - cysz);
    entry[1] = (cz * cx);
    entry[2] = ((cycz * sx) + sysz);
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
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.matrix);
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
    entry[0] = ((sxcz * sy) - cxsz);
    f32 sxsz = (sx * sz);
    f32 cxcz = (cx * cz);
    entry[1] = ((sxsz * sy) + cxcz);
    entry[2] = (sx * cy);
    linear_mtxf_mul_vec3f(src, dest[1], entry);
    entry[0] = ((cxcz * sy) + sxsz);
    entry[1] = ((cxsz * sy) - sxcz);
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
void mtxf_lookat(Mat4 mtx, Vec3f from, Vec3f to, s16 roll) {
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.matrix);
    Vec3f colX, colY, colZ;
    f32 dx = (to[0] - from[0]);
    f32 dz = (to[2] - from[2]);
    f32 invLength = sqrtf(sqr(dx) + sqr(dz));
    invLength = -(1.0f / MAX(invLength, NEAR_ZERO));
    dx *= invLength;
    dz *= invLength;
    f32 sr  = sins(roll);
    colY[1] = coss(roll);
    colY[0] = ( sr * dz);
    colY[2] = (-sr * dx);
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
 * Set 'dest' to a transformation matrix that turns an object to face the camera using either spherical or cylindrical billboarding.
 * 'mtx' is the look-at matrix from the camera.
 * 'camera' is the camera's look direction, only used for cylindrical billboarding
 * 'axis' is the direction of the fixed axis around which the object will be rotated, if using cylindrical billboarding
 * 'position' is the position of the object in the world.
 * 'scale' is the scale of the object.
 * 'angle' rotates the object while still facing the camera.
 * 'isCylindrical' determines which type of billboarding to use.
 */
void mtxf_billboard_generic(Mat4 dest, Mat4 mtx, Vec3f camera, Vec3f axis, Vec3f position, Vec3f scale, s16 angle, s32 isCylindrical) {
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.matrix);
    s32 i;
    f32 sx = scale[0];
    f32 sy = scale[1];
    f32 sz = scale[2];

    if (isCylindrical) {
        Vec3f rotAxis;
        vec3f_copy(rotAxis, axis);
        vec3f_normalize(rotAxis);
        Vec3f rightAxis;
        vec3f_cross(rightAxis, camera, rotAxis);
        vec3f_normalize(rightAxis);
        Vec3f forwardAxis;
        vec3f_cross(forwardAxis, rightAxis, rotAxis);

        vec3f_copy(dest[0], rightAxis);
        vec3f_copy(dest[1], rotAxis);
        vec3f_copy(dest[2], forwardAxis);
    } else {
        Mat4* cameraMat = &gCameraTransform;
        for (i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                dest[i][j] = (*cameraMat)[j][i];
            }
            dest[i][3] = 0.0f;
        }
    }

    if (angle != 0x0) {
        float m00 = dest[0][0];
        float m01 = dest[0][1];
        float m02 = dest[0][2];
        float m10 = dest[1][0];
        float m11 = dest[1][1];
        float m12 = dest[1][2];
        float cosa = coss(angle);
        float sina = sins(angle);
        dest[0][0] = cosa * m00 + sina * m10; 
        dest[0][1] = cosa * m01 + sina * m11; 
        dest[0][2] = cosa * m02 + sina * m12;
        dest[1][0] = -sina * m00 + cosa * m10;
        dest[1][1] = -sina * m01 + cosa * m11;
        dest[1][2] = -sina * m02 + cosa * m12;
    }
    for (i = 0; i < 3; i++) {
        dest[0][i] *= sx;
        dest[1][i] *= sy;
        dest[2][i] *= sz;
    }

    // Translation = input translation + position
    vec3f_copy(dest[3], position);
    vec3f_add(dest[3], mtx[3]);
    dest[3][3] = 1.0f;
}

/**
 * Mostly the same as 'mtxf_align_terrain_normal', but also applies a scale and multiplication.
 * 'upDir' is the terrain normal
 * 'pos' is the object's position in the world
 * 'scale' is the scale of the shadow
 * 'yaw' is the angle which it should face
 */
void mtxf_shadow(Mat4 dest, Vec3f upDir, Vec3f pos, Vec3f scale, s16 yaw) {
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.matrix);
    Vec3f lateralDir;
    Vec3f leftDir;
    Vec3f forwardDir;
    vec3f_set(lateralDir, sins(yaw), 0.0f, coss(yaw));
    vec3f_normalize(upDir);
    vec3f_cross(leftDir, upDir, lateralDir);
    vec3f_normalize(leftDir);
    vec3f_cross(forwardDir, leftDir, upDir);
    vec3f_normalize(forwardDir);

    vec3f_prod(dest[0], leftDir, scale);
    vec3f_prod(dest[1], upDir, scale);
    vec3f_prod(dest[2], forwardDir, scale);
    vec3f_copy(dest[3], pos);
    MTXF_END(dest);
}

/**
 * Set 'dest' to a transformation matrix that aligns an object with the terrain
 * based on the normal. Used for enemies.
 * 'upDir' is the terrain normal
 * 'yaw' is the angle which it should face
 * 'pos' is the object's position in the world
 */
void mtxf_align_terrain_normal(Mat4 dest, Vec3f upDir, Vec3f pos, s16 yaw) {
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.matrix);
    Vec3f lateralDir;
    Vec3f leftDir;
    Vec3f forwardDir;
    vec3f_set(lateralDir, sins(yaw), 0.0f, coss(yaw));
    vec3f_normalize(upDir);
    vec3f_cross(leftDir, upDir, lateralDir);
    vec3f_normalize(leftDir);
    vec3f_cross(forwardDir, leftDir, upDir);
    vec3f_normalize(forwardDir);
    vec3f_copy(dest[0], leftDir);
    vec3f_copy(dest[1], upDir);
    vec3f_copy(dest[2], forwardDir);
    vec3f_copy(dest[3], pos);
    MTXF_END(dest);
}

/**
 * Set 'mtx' to a transformation matrix that aligns an object with the terrain
 * based on 3 height samples in an equilateral triangle around the object.
 * Used for Mario when crawling or sliding.
 * 'yaw' is the angle which it should face
 * 'pos' is the object's position in the world
 * 'radius' is the distance from each triangle vertex to the center
 */
void mtxf_align_terrain_triangle(Mat4 mtx, Vec3f pos, s16 yaw, f32 radius) {
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.matrix);
    struct Surface *floor;
    Vec3f point0, point1, point2;
    Vec3f forward;
    Vec3f xColumn, yColumn, zColumn;
    f32 minY   = (-radius * 3);
    f32 height = (pos[1] + 150);

    point0[0] = (pos[0] + (radius * sins(yaw + DEGREES( 60))));
    point0[2] = (pos[2] + (radius * coss(yaw + DEGREES( 60))));
    point0[1] = find_floor(point0[0], height, point0[2], &floor);
    point1[0] = (pos[0] + (radius * sins(yaw + DEGREES(180))));
    point1[2] = (pos[2] + (radius * coss(yaw + DEGREES(180))));
    point1[1] = find_floor(point1[0], height, point1[2], &floor);
    point2[0] = (pos[0] + (radius * sins(yaw + DEGREES(-60))));
    point2[2] = (pos[2] + (radius * coss(yaw + DEGREES(-60))));
    point2[1] = find_floor(point2[0], height, point2[2], &floor);

    if ((point0[1] - pos[1]) < minY) point0[1] = pos[1];
    if ((point1[1] - pos[1]) < minY) point1[1] = pos[1];
    if ((point2[1] - pos[1]) < minY) point2[1] = pos[1];

    f32 avgY = (point0[1] + point1[1] + point2[1]) / 3.f;

    vec3f_set(forward, sins(yaw), 0.0f, coss(yaw));
    find_vector_perpendicular_to_plane(yColumn, point0, point1, point2);
    vec3f_normalize(yColumn);
    vec3f_cross(xColumn, yColumn, forward);
    vec3f_normalize(xColumn);
    vec3f_cross(zColumn, xColumn, yColumn);
    vec3f_normalize(zColumn);
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
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.matrix);
    Vec3f entry;
    f32 *temp  = (f32 *)a;
    f32 *temp2 = (f32 *)dest;
    f32 *temp3;
    s32 i;
    for (i = 0; i < 16; i++) {
        vec3_copy(entry, temp);
        for (temp3 = (f32 *)b; (i & 3) != 3; i++) {
            *temp2 = ((entry[0] * temp3[0])
                    + (entry[1] * temp3[4])
                    + (entry[2] * temp3[8]));
            temp2++;
            temp3++;
        }
        *temp2 = 0;
        temp += 4;
        temp2++;
    }
    vec3f_add(&dest[3][0], &b[3][0]);
    ((u32 *) dest)[15] = FLOAT_ONE;
}

/**
 * Set matrix 'dest' to 'mtx' scaled by vector s
 */
void mtxf_scale_vec3f(Mat4 dest, Mat4 mtx, Vec3f s) {
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.matrix);
    f32 *temp  = (f32 *)dest;
    f32 *temp2 = (f32 *)mtx;
    s32 i;

    for (i = 0; i < 4; i++) {
        temp[ 0] = temp2[ 0] * s[0];
        temp[ 4] = temp2[ 4] * s[1];
        temp[ 8] = temp2[ 8] * s[2];
        temp[12] =  temp2[12];
        temp++;
        temp2++;
    }
}

/**
 * Set 'mtx' to a transformation matrix that rotates around the z axis.
 */
#define MATENTRY(a, b)                          \
    ((s16 *) mtx)[a     ] = (((s32) b) >> 16);  \
    ((s16 *) mtx)[a + 16] = (((s32) b) & 0xFFFF);
void mtxf_rotate_xy(Mtx *mtx, s16 angle) {
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.matrix);
    s32 i = (coss(angle) * 0x10000);
    s32 j = (sins(angle) * 0x10000);
    f32 *temp = (f32 *)mtx;
    s32 k;
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
 * Similar to approach_s32, but converts to s16 and allows for overflow between 32767 and -32768
 */
Bool32 approach_s16_bool(s16 *current, s16 target, s16 inc, s16 dec) {
    *current = approach_s16(*current, target, inc, dec);
    return (*current != target);
}
s16 approach_s16(s16 current, s16 target, s16 inc, s16 dec) {
    s16 dist = (target - current);
    if (dist >= 0) { // target >= current
        current = ((dist >  inc) ? (current + inc) : target);
    } else { // target < current
        current = ((dist < -dec) ? (current - dec) : target);
    }
    return current;
}

/**
 * Return the value 'current' after it tries to approach target, going up at
 * most 'inc' and going down at most 'dec'.
 */
Bool32 approach_s32_bool(s32 *current, s32 target, s32 inc, s32 dec) {
    *current = approach_s32(*current, target, inc, dec);
    return (*current != target);
}
s32 approach_s32(s32 current, s32 target, s32 inc, s32 dec) {
    s32 dist = (target - current);
    if (dist > 0) { // current < target
        current = ((dist >  inc) ? (current + inc) : target);
    } else if (dist < 0) { // current > target
        current = ((dist < -dec) ? (current - dec) : target);
    }
    return current;
}

/**
 * Return the value 'current' after it tries to approach target, going up at
 * most 'inc' and going down at most 'dec'.
 */
Bool32 approach_f32_bool(f32 *current, f32 target, f32 inc, f32 dec) {
    *current = approach_f32(*current, target, inc, dec);
    return !(*current == target);
}
f32 approach_f32(f32 current, f32 target, f32 inc, f32 dec) {
    f32 dist = (target - current);
    if (dist >= 0.0f) { // target >= current
        current = ((dist >  inc) ? (current + inc) : target);
    } else { // target < current
        current = ((dist < -dec) ? (current - dec) : target);
    }
    return current;
}

s32 approach_f32_signed(f32 *current, f32 target, f32 inc) {
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
s32 approach_f32_asymptotic_bool(f32 *current, f32 target, f32 multiplier) {
    if (multiplier > 1.0f) multiplier = 1.0f;

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
s16 approach_s16_asymptotic_bool(s16 *current, s16 target, s16 divisor) {
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
s16 approach_s16_asymptotic(s16 current, s16 target, s16 divisor) {
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

s16 abs_angle_diff(s16 a0, s16 a1) {
    s16 diff = (a1 - a0);
    if (diff == -0x8000) return 0x7FFF;
    return abss(diff);
}

/**
 * Helper function for atan2s. Does a look up of the arctangent of y/x assuming
 * the resulting angle is in range [0, 0x2000] (1/8 of a circle).
 */
static u16 atan2_lookup(f32 y, f32 x) {
    return x == 0
        ? 0x0
        : atans(y / x);
}

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
    f32 tinv2 = tinv  * tinv;
    f32 tinv3 = tinv2 * tinv;
    f32 t2    = t     * t   ;
    f32 t3    = t2    * t   ;

    switch (gSplineState) {
        case CURVE_BEGIN_1:
            result[0] = tinv3;
            result[1] = ( t3 *        1.75f) - (t2 * 4.5f) + (t * 3.0f);
            result[2] = (-t3 * (11 / 12.0f)) + (t2 * 1.5f);
            result[3] = t3 * (1 / 6.0f);
            break;
        case CURVE_BEGIN_2:
            result[0] = tinv3 * 0.25f;
            result[1] = (t3 * (7 / 12.0f)) - (t2 * 1.25f) + (t * 0.25f) + (7 / 12.0f);
            result[2] = (-t3 * 0.5f) + (t2 * 0.5f) + (t * 0.5f) + (1 / 6.0f);
            result[3] = t3 * (1 / 6.0f);
            break;
        case CURVE_MIDDLE:
            result[0] = tinv3 * (1.0f / 6.0f);
            result[1] = (t3 * 0.5f) - t2 + (4.0f / 6.0f);
            result[2] = (-t3 * 0.5f) + (t2 * 0.5f) + (t * 0.5f) + (1.0f / 6.0f);
            result[3] = t3 * (1.0f / 6.0f);
            break;
        case CURVE_END_1:
            result[0] = tinv3 * (1.0f /  6.0f);
            result[1] = (-tinv3 * 0.5f) + (tinv2 * 0.5f) + (tinv * 0.5f) + (1.0f / 6.0f);
            result[2] = (tinv3 * (7.0f / 12.0f)) - (tinv2 * 1.25f) + (tinv * 0.25f) + (7.0f / 12.0f);
            result[3] = t3 * 0.25f;
            break;
        case CURVE_END_2:
            result[0] = tinv3 * (1.0f / 6.0f);
            result[1] = (-tinv3 * (11.0f / 12.0f)) + (tinv2 * 1.5f);
            result[2] = (tinv3 * 1.75f) - (tinv2 * 4.5f) + (tinv * 3.0f);
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

    gSplineKeyframeFraction += (gSplineKeyframe[0][0] / 1000.0f);
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
    // Check if we're perpendicular or pointing away from the surface.
    if (det < NEAR_ZERO) return FALSE;
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
    if ((*length <= NEAR_ZERO) || (*length > dir_length)) return FALSE;
    // Successful contact.
    // Make 'add_dir' into 'dir' scaled by 'length'.
    Vec3f add_dir;
    vec3_scale_dest(add_dir, dir, *length);
    // Make 'hit_pos' into the sum of 'orig' and 'add_dir'.
    vec3f_sum(hit_pos, orig, add_dir);
    return TRUE;
}

void find_surface_on_ray_list(struct SurfaceNode *list, Vec3f orig, Vec3f dir, f32 dir_length, struct Surface **hit_surface, Vec3f hit_pos, f32 *max_length) {
    s32 hit;
    f32 length;
    Vec3f chk_hit_pos;
    f32 top, bottom;
    PUPPYPRINT_GET_SNAPSHOT();
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
    profiler_collision_update(first);
}

void find_surface_on_ray_cell(s32 cellX, s32 cellZ, Vec3f orig, Vec3f normalized_dir, f32 dir_length, struct Surface **hit_surface, Vec3f hit_pos, f32 *max_length, s32 flags) {
    // Skip if OOB
    if ((cellX >= 0) && (cellX <= (NUM_CELLS - 1)) && (cellZ >= 0) && (cellZ <= (NUM_CELLS - 1))) {
        // Iterate through each surface in this partition
        if ((normalized_dir[1] > -NEAR_ONE) && (flags & RAYCAST_FIND_CEIL)) {
            find_surface_on_ray_list( gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_CEILS ], orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
            find_surface_on_ray_list(gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_CEILS ], orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
        }
        if ((normalized_dir[1] <  NEAR_ONE) && (flags & RAYCAST_FIND_FLOOR)) {
            find_surface_on_ray_list( gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_FLOORS], orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
            find_surface_on_ray_list(gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_FLOORS], orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
        }
        if (flags & RAYCAST_FIND_WALL) {
            find_surface_on_ray_list( gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WALLS ], orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
            find_surface_on_ray_list(gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WALLS ], orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
        }
        if (flags & RAYCAST_FIND_WATER) {
            find_surface_on_ray_list( gStaticSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WATER ], orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
            find_surface_on_ray_list(gDynamicSurfacePartition[cellZ][cellX][SPATIAL_PARTITION_WATER ], orig, normalized_dir, dir_length, hit_surface, hit_pos, max_length);
        }
    }
}

f32 find_surface_on_ray(Vec3f orig, Vec3f dir, struct Surface **hit_surface, Vec3f hit_pos, s32 flags) {
    Vec3f normalized_dir;
    const f32 invcell = 1.0f / CELL_SIZE;
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.collision_raycast);

    // Set that no surface has been hit
    *hit_surface = NULL;
    vec3f_sum(hit_pos, orig, dir);

    // Get normalized direction
    f32 dir_length = vec3_mag(dir);
    f32 max_length = dir_length;
    vec3f_copy(normalized_dir, dir);
    vec3f_normalize(normalized_dir);

    // Get the start and end coords converted to cell-space
    f32 start_cell_coord_x = (orig[0] + LEVEL_BOUNDARY_MAX) * invcell;
    f32 start_cell_coord_z = (orig[2] + LEVEL_BOUNDARY_MAX) * invcell;
    f32 end_cell_coord_x   = (orig[0] + dir[0] + LEVEL_BOUNDARY_MAX) * invcell;
    f32 end_cell_coord_z   = (orig[2] + dir[2] + LEVEL_BOUNDARY_MAX) * invcell;

    // Don't do grid traversal if straight down
    if ((normalized_dir[1] >= NEAR_ONE) || (normalized_dir[1] <= -NEAR_ONE)) {
        find_surface_on_ray_cell((s32)start_cell_coord_x, (s32)start_cell_coord_z, orig, normalized_dir, dir_length, hit_surface, hit_pos, &max_length, flags);
        return max_length;
    }

    // "A Fast Voxel Traversal Algorithm for Ray Tracing" - John Amanatides & Andrew Woo
    // Adapted from implementation at https://www.shadertoy.com/view/XddcWn
    f32 rd_x = end_cell_coord_x - start_cell_coord_x;
    f32 rd_z = end_cell_coord_z - start_cell_coord_z;
    f32 p_x = (s32)start_cell_coord_x;
    f32 p_z = (s32)start_cell_coord_z;
    f32 rdinv_x = 1.0f / rd_x;
    f32 rdinv_z = 1.0f / rd_z;
    f32 stp_x = signum_positive(rd_x);
    f32 stp_z = signum_positive(rd_z);
    f32 delta_x = MIN(rdinv_x * stp_x, 1.0f);
    f32 delta_z = MIN(rdinv_z * stp_z, 1.0f);
    f32 t_max_x = ABS((p_x + MAX(stp_x, 0.0f) - start_cell_coord_x) * rdinv_x);
    f32 t_max_z = ABS((p_z + MAX(stp_z, 0.0f) - start_cell_coord_z) * rdinv_z);

    while (TRUE) {
        find_surface_on_ray_cell((s32)p_x, (s32)p_z, orig, normalized_dir, dir_length, hit_surface, hit_pos, &max_length, flags);
        f32 t_next = MIN(t_max_x, t_max_z);
        if (t_next > 1.0f) {
            break;
        }

        if (t_max_x < t_max_z) {
            t_max_x += delta_x;
            p_x += stp_x;
        }
        else {
            t_max_z += delta_z;
            p_z += stp_z;
        }
    }
    return max_length;
}

// Constructs a float in registers, which can be faster than gcc's default of loading a float from rodata.
// Especially fast for halfword floats, which get loaded with a `lui` + `mtc1`.
static ALWAYS_INLINE float construct_float(const float f)
{
    u32 r;
    float f_out;
    u32 i = *(u32*)(&f);

    if (!__builtin_constant_p(i))
    {
        return *(float*)(&i);
    }

    u32 upper = (i >> 16);
    u32 lower = (i >>  0) & 0xFFFF;

    if ((i & 0xFFFF) == 0) {
        __asm__ ("lui %0, %1"
                                : "=r"(r)
                                : "K"(upper));
    } else if ((i & 0xFFFF0000) == 0) {
        __asm__ ("ori %0, $0, %1"
                                : "+r"(r)
                                : "K"(lower));
    } else {
        __asm__ ("lui %0, %1"
                                : "=r"(r)
                                : "K"(upper));
        __asm__ ("ori %0, %0, %1"
                                : "+r"(r)
                                : "K"(lower));
    }

    __asm__ ("mtc1 %1, %0"
                         : "=f"(f_out)
                         : "r"(r));
    return f_out;
}

// Converts a floating point matrix to a fixed point matrix
// Makes some assumptions about certain fields in the matrix, which will always be true for valid matrices.
OPTIMIZE_OS void mtxf_to_mtx_fast(s16* dst, float* src) {
    PUPPYPRINT_ADD_COUNTER(gPuppyCallCounter.matrix);
    float scale = construct_float(65536.0f / WORLD_SCALE);
    // Iterate over pairs of values in the input matrix
    for (int i = 0; i < 8; i++)
    {
        // Read the first input in the current pair
        float a = src[2 * i + 0];

        // Convert the first input to fixed
        s32 a_int = (s32)(a * scale);
        dst[2 * i +  0] = (s16)(a_int >> 16);
        dst[2 * i + 16] = (s16)(a_int >>  0);

        // If this is the left half of the matrix, convert the second input to fixed
        if ((i & 1) == 0)
        {
            // Read the second input in the current pair
            float b = src[2 * i + 1];
            s32 b_int = (s32)(b * scale);
            dst[2 * i +  1] = (s16)(b_int >> 16);
            dst[2 * i + 17] = (s16)(b_int >>  0);
        }
        // Otherwise, skip the second input because column 4 will always be zero
        // Row 4 column 4 is handled after the loop.
        else
        {
            dst[2 * i +  1] = 0;
            dst[2 * i + 17] = 0;
        }

    }
    // Write 1.0 to the bottom right entry in the output matrix
    // The low half was already set to zero in the loop, so we only need
    //  to set the top half.
    dst[15] = 1;
}
