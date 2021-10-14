// clam.inc.c

struct ObjectHitbox sClamShellHitbox = {
    /* interactType:      */ INTERACT_CLAM_OR_BUBBA,
    /* downOffset:        */ 0,
    /* damageOrCoinValue: */ 2,
    /* health:            */ 99,
    /* numLootCoins:      */ 0,
    /* radius:            */ 150,
    /* height:            */ 80,
    /* hurtboxRadius:     */ 150,
    /* hurtboxHeight:     */ 80,
};

void clam_act_0(void) {
    if (cur_obj_init_anim_check_frame(0, 25)) {
        cur_obj_play_sound_2(SOUND_GENERAL_CLAM_SHELL_CLOSE);
        spawn_mist_from_global();
        cur_obj_become_tangible();

        o->oClamShakeTimer = 10;
        o->oTimer = 0;
    } else if (o->oTimer > 150 && o->oDistanceToMario < 500.0f) {
        cur_obj_play_sound_2(SOUND_GENERAL_CLAM_SHELL_OPEN);
        o->oAction = 1;
    } else if (o->oClamShakeTimer != 0) {
        o->oClamShakeTimer--;
        cur_obj_shake_y(3.0f);
    }
}

void clam_act_1(void) {
    s16 i;
    s16 bubblesX, bubblesZ;

    if (o->oTimer > 150) {
        o->oAction = 0;
    } else if (obj_is_rendering_enabled() && cur_obj_init_anim_check_frame(1, 8)) {
        for (i = -0x2000; i < 0x2000; i += 0x555) {
            bubblesX = (s16)(100.0f * sins(i));
            bubblesZ = (s16)(100.0f * coss(i));

            spawn_object_relative(0, bubblesX, 30, bubblesZ, o, MODEL_BUBBLE, bhvBubbleMaybe);
        }
    } else if (cur_obj_check_anim_frame(30)) {
        cur_obj_become_intangible();
    }
}

void bhv_clam_loop(void) {
    o->header.gfx.scale[1] = 1.5f;

    switch (o->oAction) {
        case 0:
            clam_act_0();
            break;
        case 1:
            clam_act_1();
            break;
    }

    obj_check_attacks(&sClamShellHitbox, o->oAction);
}
