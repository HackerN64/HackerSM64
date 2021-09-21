// sushi.c.inc

void bhv_sushi_shark_collision_loop(void) {
}

void bhv_sushi_shark_loop(void) {
    f32 waterLevel = find_water_level(o->oPosX, o->oPosZ);
    o->oPosX = o->oHomeX + sins(o->oSushiSharkUnkF4) * 1700.0f;
    o->oPosZ = o->oHomeZ + coss(o->oSushiSharkUnkF4) * 1700.0f;
    o->oPosY = waterLevel + o->oHomeY + sins(o->oSushiSharkUnkF4) * 200.0f;
    o->oMoveAngleYaw = o->oSushiSharkUnkF4 + 0x4000;
    o->oSushiSharkUnkF4 += 0x80;
    if (gMarioObject->oPosY - waterLevel > -500.0f)
        if (o->oPosY - waterLevel > -200.0f)
            spawn_object_with_scale(o, MODEL_WAVE_TRAIL, bhvObjectWaveTrail, 4.0f);
    if ((o->oTimer & 0xF) == 0)
        cur_obj_play_sound_2(SOUND_OBJ_SUSHI_SHARK_WATER_SOUND);
    o->oInteractStatus = 0;
}
