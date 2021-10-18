// intro_scene.inc.c

void spawn_child_obj_relative(struct Object *parent, s16 xOffset, s16 yOffset, s16 zOffset, s16 pitchOffset,
                   s16 yawOffset, s16 rollOffset, s16 forwardVel,
                   ModelID32 model, const BehaviorScript *behavior) {
    struct Object *obj = spawn_object(parent, model, behavior);

    obj->header.gfx.animInfo.animFrame = (random_float() * 6.f);
    obj->oEndBirdCutsceneVars9PointX = sCutsceneVars[9].point[0];
    sCutsceneVars[9].point[0] += 1.0f;
    obj->oPosX += xOffset;
    obj->oPosY += yOffset;
    if (gCutsceneTimer > 700) {
        obj->oPosY -= 150.f;
    }
    obj->oPosZ           += zOffset;
    obj->oMoveAnglePitch += pitchOffset;
    obj->oMoveAngleYaw   += yawOffset;
    obj->oMoveAngleRoll  += rollOffset;
    obj->oForwardVel      = forwardVel;
}

void bhv_intro_scene_loop(void) {
    if (gCutsceneObjSpawn != 0) {
        vec3_copy(&o->oPosVec, gCamera->pos);
        o->oMoveAnglePitch = 0x0;
        o->oMoveAngleYaw   = 0x0;

        switch (gCutsceneObjSpawn) {
            case CUTSCENE_OBJ_BEGINNING_LAKITU:
                spawn_object(o, MODEL_LAKITU, bhvBeginningLakitu);
                break;
            case CUTSCENE_OBJ_BEGINNING_PEACH:
                spawn_object(o, MODEL_PEACH, bhvBeginningPeach);
                break;
            case CUTSCENE_OBJ_7_END_BIRDS_1:
                spawn_child_obj_relative(o,    0,  205,   500, 0x1000, 0x6000, -0x1E00, 25, MODEL_BIRDS, bhvEndBirds1);
                spawn_child_obj_relative(o,    0,  205,   800, 0x1800, 0x6000, -0x1400, 35, MODEL_BIRDS, bhvEndBirds1);
                spawn_child_obj_relative(o, -100,  300,   500, 0x0800, 0x6000,  0x0000, 25, MODEL_BIRDS, bhvEndBirds1);
                spawn_child_obj_relative(o,  100, -200,   800, 0x0000, 0x4000,  0x1400, 45, MODEL_BIRDS, bhvEndBirds1);
                spawn_child_obj_relative(o,  -80,  300,   350, 0x1800, 0x5000,  0x0A00, 35, MODEL_BIRDS, bhvEndBirds1);
                spawn_child_obj_relative(o, -300,  300,   500, 0x0800, 0x6000,  0x2800, 25, MODEL_BIRDS, bhvEndBirds1);
                spawn_child_obj_relative(o, -400, -200,   800, 0x0000, 0x4000, -0x1400, 45, MODEL_BIRDS, bhvEndBirds1);
                break;
            case CUTSCENE_OBJ_2_END_BIRDS_1:
                spawn_child_obj_relative(o,   50,  205,   500, 0x1000, 0x6000,  0x0000, 35, MODEL_BIRDS, bhvEndBirds1);
                spawn_child_obj_relative(o,    0,  285,   800, 0x1800, 0x6000,  0x0000, 35, MODEL_BIRDS, bhvEndBirds1);
                break;
            case CUTSCENE_OBJ_5_END_BIRDS_2:
                spawn_child_obj_relative(o, -100, -100,  -700, 0x0000, 0x0000, -0x0F00, 25, MODEL_BIRDS, bhvEndBirds2);
                spawn_child_obj_relative(o, -250,  255,  -200, 0x0000, 0x0000, -0x1400, 25, MODEL_BIRDS, bhvEndBirds2);
                spawn_child_obj_relative(o, -100,  155,  -600, 0x0000, 0x0000, -0x0500, 35, MODEL_BIRDS, bhvEndBirds2);
                spawn_child_obj_relative(o,  250,  200, -1200, 0x0000, 0x0000, -0x0700, 25, MODEL_BIRDS, bhvEndBirds2);
                spawn_child_obj_relative(o, -250,  255,  -700, 0x0000, 0x0000,  0x0000, 25, MODEL_BIRDS, bhvEndBirds2);
                break;
        }

        gCutsceneObjSpawn = CUTSCENE_OBJ_NONE;
    }
}
