
/**
 * This is the behavior file for the tilting inverted pyramids in BitFS/LLL.
 * The object essentially just tilts and moves Mario with it.
 */

/**
 * Initialize the object's transform matrix with Y being up.
 */
void bhv_platform_normals_init(void) {
    Mat4 *transform = &o->transform;
    vec3f_set(&o->oTiltingPyramidNormalVec, 0.0f, 1.0f, 0.0f);
    mtxf_align_terrain_normal(*transform, &o->oTiltingPyramidNormalVec, &o->oPosVec, 0);
}

/**
 * Main behavior for the tilting pyramids in LLL/BitFS. These platforms calculate rough normals from Mario's position,
 * then gradually tilt back moving Mario with them.
 */
void bhv_tilting_inverted_pyramid_loop(void) {
#ifndef PLATFORM_DISPLACEMENT_2
    Vec3f posBeforeRotation, posAfterRotation;
    // Mario's position
    Vec3f m;
#endif
    Vec3f targetNormal;
    Mat4 *transform = &o->transform;

    if (gMarioObject->platform == o) {
        vec3_diff(targetNormal, &gMarioObject->oPosVec, &o->oPosVec);
#ifndef PLATFORM_DISPLACEMENT_2
        vec3f_copy(m, gMarioStates[0].pos);
        linear_mtxf_mul_vec3f(*transform, posBeforeRotation, targetNormal);
#endif
        targetNormal[1] = 500.0f;
        vec3f_normalize(targetNormal);
        o->oTiltingPyramidMarioOnPlatform = TRUE;
    } else {
        vec3f_set(targetNormal, 0.0f, 1.0f, 0.0f);
        o->oTiltingPyramidMarioOnPlatform = FALSE;
    }

    // Approach the normals by 0.01f towards the new goal, then create a transform matrix and orient the object. 
    // Outside of the other conditionals since it needs to tilt regardless of whether Mario is on.
    approach_f32_symmetric_bool(&o->oTiltingPyramidNormalX, targetNormal[0], 0.01f);
    approach_f32_symmetric_bool(&o->oTiltingPyramidNormalY, targetNormal[1], 0.01f);
    approach_f32_symmetric_bool(&o->oTiltingPyramidNormalZ, targetNormal[2], 0.01f);
    mtxf_align_terrain_normal(*transform, &o->oTiltingPyramidNormalVec, &o->oPosVec, 0);

#ifndef PLATFORM_DISPLACEMENT_2
    // If Mario is on the platform, adjust his position for the platform tilt.
    if (o->oTiltingPyramidMarioOnPlatform) {
        linear_mtxf_mul_vec3f(*transform, posAfterRotation, targetNormal);
        m[0] += posAfterRotation[0] - posBeforeRotation[0];
        m[1] += posAfterRotation[1] - posBeforeRotation[1];
        m[2] += posAfterRotation[2] - posBeforeRotation[2];
        vec3f_copy(gMarioStates[0].pos, m);
    }
#endif

    o->header.gfx.throwMatrix = transform;
}
