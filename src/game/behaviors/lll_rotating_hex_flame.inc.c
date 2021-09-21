// lll_rotating_hex_flame.c.inc

void bhv_lll_rotating_hex_flame_loop(void) {
    f32 relPosX = o->oLllRotatingHexFlameUnkF4;
    f32 relPosY = o->oLllRotatingHexFlameUnkF8;
    f32 relPosZ = o->oLllRotatingHexFlameUnkFC;
    cur_obj_set_pos_relative(o->parentObj, relPosX, relPosY, relPosZ);
    o->oPosY = o->parentObj->oPosY + 100.0f;
    if (o->parentObj->oAction == 3)
        obj_mark_for_deletion(o);
}

void fire_bar_spawn_flames(s16 yaw) {
    struct Object *flameObj;
    s32 i;
    f32 xOffset = sins(yaw) * 200.0f;
    f32 zOffset = coss(yaw) * 200.0f;
    s32 amt = (o->oBehParams2ndByte == 0) ? 4 : 3;
    for (i = 0; i < amt; i++) {
        flameObj = spawn_object(o, MODEL_RED_FLAME, bhvLllRotatingHexFlame);
        flameObj->oLllRotatingHexFlameUnkF4 += xOffset;
        flameObj->oLllRotatingHexFlameUnkF8 = o->oPosY - 200.0f;
        flameObj->oLllRotatingHexFlameUnkFC += zOffset;
        obj_scale_xyz(flameObj, 6.0f, 6.0f, 6.0f);
        xOffset += sins(yaw) * 150.0f;
        zOffset += coss(yaw) * 150.0f;
    }
}

void fire_bar_act_0(void) {
    if (o->oDistanceToMario < 3000.0f)
        o->oAction = 1;
}

void fire_bar_act_1(void) {
    fire_bar_spawn_flames(0);
    fire_bar_spawn_flames(-0x8000);
    o->oAngleVelYaw = 0;
    o->oMoveAngleYaw = 0;
    o->oAction = 2;
}

void fire_bar_act_2(void) {
    o->oAngleVelYaw = -0x100;
    o->oMoveAngleYaw += o->oAngleVelYaw;
    if (o->oDistanceToMario > 3200.0f)
        o->oAction = 3;
}

void fire_bar_act_3(void) {
    o->oAction = 0;
}

void (*sRotatingCwFireBarsActions[])(void) = { fire_bar_act_0, fire_bar_act_1,
                                               fire_bar_act_2, fire_bar_act_3 };

void bhv_lll_rotating_block_fire_bars_loop(void) {
    cur_obj_call_action_function(sRotatingCwFireBarsActions);
    if (o->oBehParams2ndByte == 0)
        load_object_collision_model();
}
