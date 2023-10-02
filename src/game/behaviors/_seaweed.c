// seaweed.inc.c

void bhv_seaweed_init(void) {
    o->header.gfx.animInfo.animFrame = random_float() * 80.0f;
}

void bhv_seaweed_bundle_init(void) {
    struct Object *seaweed;

    seaweed = spawn_object(o, MODEL_SEAWEED, bhvSeaweed);
    vec3i_set(&seaweed->oFaceAngleVec,  0x157C, 0x38BB, 0x2580);
    vec3f_set( seaweed->header.gfx.scale, 1.0f,   1.0f,   1.0f);
    seaweed->header.gfx.animInfo.animFrame = 0;

    seaweed = spawn_object(o, MODEL_SEAWEED, bhvSeaweed);
    vec3i_set(&seaweed->oFaceAngleVec,  0x17D6, 0xA348, 0x0000);
    vec3f_set( seaweed->header.gfx.scale, 0.8f,   0.9f,   0.8f);
    seaweed->header.gfx.animInfo.animFrame = random_float() * 80.0f;

    seaweed = spawn_object(o, MODEL_SEAWEED, bhvSeaweed);
    vec3i_set(&seaweed->oFaceAngleVec,  0x21FC, 0x9E34, 0x1004);
    vec3f_set( seaweed->header.gfx.scale, 0.8f,   0.8f,   0.8f);
    seaweed->header.gfx.animInfo.animFrame = random_float() * 80.0f;

    seaweed = spawn_object(o, MODEL_SEAWEED, bhvSeaweed);
    vec3i_set(&seaweed->oFaceAngleVec,  0x251C, 0xDF94, 0x0000);
    vec3f_set( seaweed->header.gfx.scale, 1.2f,   1.2f,   1.2f);
    seaweed->header.gfx.animInfo.animFrame = random_float() * 80.0f;
}
