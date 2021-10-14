// end_birds_1.inc.c

void bhv_end_birds_1_loop(void) {
    Vec3f pos;

    switch (o->oAction) {
        case END_BIRDS_ACT_INIT:
            cur_obj_scale(0.7f);
            vec3f_set(&o->oIntroLakituEndBirds1DestVec, -554.0f, 3044.0f, -1314.0f);
            o->oAction = END_BIRDS_ACT_ACTIVE;
            break;
        case END_BIRDS_ACT_ACTIVE:
            vec3f_copy(pos, &o->oIntroLakituEndBirds1DestVec);
            if (o->oTimer < 100) {
                obj_rotate_towards_point(o, pos, 0, 0, 0x20, 0x20);
            }
            if ((o->oEndBirdCutsceneVars9PointX == 0.0f) && (o->oTimer == 0)) {
                cur_obj_play_sound_2(SOUND_GENERAL_BIRDS_FLY_AWAY);
            }
            if (gCutsceneTimer == 0) {
                obj_mark_for_deletion(o);
            }
            break;
    }

    cur_obj_set_pos_via_transform();
}
