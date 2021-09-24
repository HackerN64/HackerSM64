// jrb_ship.c.inc

struct ObjectHitbox sSkullSlidingBoxHitbox = {
    /* interactType: */ INTERACT_DAMAGE,
    /* downOffset: */ 0,
    /* damageOrCoinValue: */ 1,
    /* health: */ 1,
    /* numLootCoins: */ 0,
    /* radius: */ 130,
    /* height: */ 100,
    /* hurtboxRadius: */ 0,
    /* hurtboxHeight: */ 0,
};

void bhv_sunken_ship_part_loop(void) {
    if (o->oDistanceToMario > 10000.0f)
        o->oOpacity = 140;
    else
        o->oOpacity = o->oDistanceToMario * 140.0f / 10000.0;
    cur_obj_disable_rendering();
}

void bhv_ship_part_3_loop(void) {
    s16 initialPitch = o->oFaceAnglePitch;
    s16 initialRoll = o->oFaceAngleRoll;
    cur_obj_set_pos_to_home_with_debug();
    o->oShipPart3LoopPitch += 0x100;
    o->oFaceAnglePitch = sins(o->oShipPart3LoopPitch) * 1024.0f;
    o->oFaceAngleRoll = sins(o->oShipPart3LoopRoll) * 1024.0f;
    o->oAngleVelPitch = o->oFaceAnglePitch - initialPitch;
    o->oAngleVelRoll = o->oFaceAngleRoll - initialRoll;
    if (gMarioObject->oPosY > 1000.0f)
        cur_obj_play_sound_1(SOUND_ENV_BOAT_ROCKING1);
}

void bhv_jrb_sliding_box_loop(void) {
    Mat4 mtx;
    Vec3f shipToBoxPos1;
    Vec3f shipToBoxPos2;
    Vec3s shipRotation;
    struct Object *shipObj;
    if (o->oJrbSlidingBoxShipObj == NULL) {
        shipObj = cur_obj_nearest_object_with_behavior(bhvInSunkenShip3);
        if (shipObj != NULL) {
            o->oJrbSlidingBoxShipObj = shipObj;
            o->oParentRelativePosX = o->oPosX - shipObj->oPosX;
            o->oParentRelativePosY = o->oPosY - shipObj->oPosY;
            o->oParentRelativePosZ = o->oPosZ - shipObj->oPosZ;
        }
    } else {
        shipObj = o->oJrbSlidingBoxShipObj;
        shipRotation[0] = shipObj->oFaceAnglePitch;
        shipRotation[1] = shipObj->oFaceAngleYaw;
        shipRotation[2] = shipObj->oFaceAngleRoll;
        shipToBoxPos1[0] = o->oParentRelativePosX;
        shipToBoxPos1[1] = o->oParentRelativePosY;
        shipToBoxPos1[2] = o->oParentRelativePosZ;
        mtxf_rotate_zxy_and_translate(mtx, shipToBoxPos1, shipRotation);
        linear_mtxf_mul_vec3f(mtx, shipToBoxPos2, shipToBoxPos1);
        o->oPosX = shipObj->oPosX + shipToBoxPos2[0];
        o->oPosY = shipObj->oPosY + shipToBoxPos2[1];
        o->oPosZ = shipObj->oPosZ + shipToBoxPos2[2];
        o->oFaceAnglePitch = shipObj->oFaceAnglePitch;
    }
    o->oJrbSlidingBoxAdditiveZ = sins(o->oJrbSlidingBoxAngle) * 20.0f;
    o->oJrbSlidingBoxAngle += 0x100;
    o->oParentRelativePosZ += o->oJrbSlidingBoxAdditiveZ;
    if (gMarioObject->oPosY > 1000.0f)
        if (absf(o->oJrbSlidingBoxAdditiveZ) > 3.0f)
            cur_obj_play_sound_1(SOUND_AIR_ROUGH_SLIDE);
    obj_set_hitbox(o, &sSkullSlidingBoxHitbox);
    if (!(o->oJrbSlidingBoxAngle & 0x7FFF))
        cur_obj_become_tangible();
    if (obj_check_if_collided_with_object(o, gMarioObject)) {
        o->oInteractStatus = 0;
        cur_obj_become_intangible();
    }
}
