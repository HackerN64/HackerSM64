// end_birds_1.inc.c

void bhv_end_birds_1_loop(void) {
    Vec3f pos;

    switch (gCurrentObject->oAction) {
        case 0:
            cur_obj_scale(0.7f);
            gCurrentObject->oIntroLakituEndBirds1DestX = -554.f;
            gCurrentObject->oIntroLakituEndBirds1DestY = 3044.f;
            gCurrentObject->oIntroLakituEndBirds1DestZ = -1314.f;
            gCurrentObject->oAction += 1;
            break;
        case 1:
            vec3f_set(pos, gCurrentObject->oIntroLakituEndBirds1DestX, gCurrentObject->oIntroLakituEndBirds1DestY,
                      gCurrentObject->oIntroLakituEndBirds1DestZ);

            if (gCurrentObject->oTimer < 100)
                obj_rotate_towards_point(gCurrentObject, pos, 0, 0, 0x20, 0x20);
            if ((gCurrentObject->oEndBirdCutsceneVars9PointX == 0.f) && (gCurrentObject->oTimer == 0))
                cur_obj_play_sound_2(SOUND_GENERAL_BIRDS_FLY_AWAY);
            if (gCutsceneTimer == 0)
                obj_mark_for_deletion(gCurrentObject);
            break;
    }

    cur_obj_set_pos_via_transform();
}
