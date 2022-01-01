// lll_hexagonal_ring.inc.c

void hexagonal_ring_spawn_flames(void) {
    struct Object *flameObj = spawn_object(o, MODEL_RED_FLAME, bhvVolcanoFlames);

    flameObj->oPosY += 550.0f;
    flameObj->oMoveAngleYaw = (s16) random_u16();
    flameObj->oForwardVel = random_float() * 40.0f + 20.0f;
    flameObj->oVelY = random_float() * 50.0f + 10.0f;

    f32 size = random_float() * 6.0f + 3.0f;

    obj_scale(flameObj, size);

    if (random_float() < 0.1f) {
        cur_obj_play_sound_2(SOUND_GENERAL_VOLCANO_EXPLOSION);
    }
}

void bhv_lll_rotating_hexagonal_ring_loop(void) {
    o->oCollisionDistance = 4000.0f;
    o->oDrawingDistance = 8000.0f;

    switch (o->oAction) {
        case 0:
            if (gMarioObject->platform == o) {
                o->oAction++;
            }

            o->oAngleVelYaw = 0x100;
            break;

        case 1:
            o->oAngleVelYaw = 256.0f - sins(o->oTimer * 128) * 256.0f;

            if (o->oTimer > 128) {
                o->oAction++;
            }
            break;

        case 2:
            if (gMarioObject->platform != o) {
                o->oAction++;
            }

            if (o->oTimer > 128) {
                o->oAction++;
            }

            o->oAngleVelYaw = 0;
            hexagonal_ring_spawn_flames();
            break;

        case 3:
            o->oAngleVelYaw = sins(o->oTimer * 128) * 256.0f;

            if (o->oTimer > 128) {
                o->oAction = 0;
            }
            break;

        case 4:
            o->oAction = 0;
            break;
    }

    o->oAngleVelYaw = -o->oAngleVelYaw;
    o->oMoveAngleYaw += o->oAngleVelYaw;
}
