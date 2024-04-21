
/**
 * This is the behavior file for the tilting inverted pyramids in BitFS/LLL.
 * The object essentially just tilts and moves Mario with it.
 */

/**
 * Initialize the object's transform matrix with Y being up.
 */
void bhv_platform_normals_init(void) {
    vec3f_set(&o->oTiltingPyramidNormalVec, 0.0f, 1.0f, 0.0f);
    mtxf_align_terrain_normal(o->transform, &o->oTiltingPyramidNormalVec, &o->oPosVec, 0);
}

/**
 * Main behavior for the tilting pyramids in LLL/BitFS. These platforms calculate rough normals from Mario's position,
 * then gradually tilt back moving Mario with them.
 */
void bhv_tilting_inverted_pyramid_loop(void) {
    Vec3f targetNormal;
    Mat4 *transform = &o->transform;
    s32 marioOnPlatform = (gMarioObject->platform == o);

    if (marioOnPlatform) {
        targetNormal[0] = gMarioStates[0].pos[0] - o->oPosX;
        targetNormal[2] = gMarioStates[0].pos[2] - o->oPosZ;
        targetNormal[1] = 500.0f;
        vec3f_normalize(targetNormal);
    } else {
        // Target normal is directly upwards when Mario is not on the platform.
        vec3f_set(targetNormal, 0.0f, 1.0f, 0.0f);
    }

    // Approach the normals by 0.01f towards the new goal, then create a transform matrix and orient the object. 
    // Outside of the other conditionals since it needs to tilt regardless of whether Mario is on.
    approach_f32_symmetric_bool(&o->oTiltingPyramidNormalX, targetNormal[0], 0.01f);
    approach_f32_symmetric_bool(&o->oTiltingPyramidNormalY, targetNormal[1], 0.01f);
    approach_f32_symmetric_bool(&o->oTiltingPyramidNormalZ, targetNormal[2], 0.01f);
    mtxf_align_terrain_normal(*transform, &o->oTiltingPyramidNormalVec, &o->oPosVec, 0x0);

    o->header.gfx.throwMatrix = transform;
}
