/**
 * Cutscene that plays when Mario dies on his back.
 */

void cutscene_bbh_death_start(struct Camera *c) {
    Vec3f dir = { 0, 40.f, 60.f };

    offset_rotated(sCutsceneVars[3].point, sMarioCamState->pos, dir, sMarioCamState->faceAngle);
    vec3f_copy(sCutsceneVars[0].point, c->focus);
}

void cutscene_bbh_death_goto_mario(struct Camera *c) {
    cutscene_goto_cvar_pos(c, 400.f, 0x1800, 0, 0x400);
}

/**
 * Cutscene that plays when Mario dies in BBH.
 */
void cutscene_bbh_death(struct Camera *c) {
    cutscene_event(cutscene_bbh_death_start, c, 0, 0);
    cutscene_event(cutscene_bbh_death_goto_mario, c, 0, -1);
    sStatusFlags |= CAM_FLAG_SMOOTH_MOVEMENT;
    set_handheld_shake(HAND_CAM_SHAKE_CUTSCENE);
}

struct Cutscene sCutsceneDeathOnBack[] = {
    { cutscene_bbh_death, CUTSCENE_LOOP }
};
