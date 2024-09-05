// spawn_star_exit.inc.c

void bhv_ccm_touched_star_spawn_loop(void) {
    if (gCCMEnteredSlide & 1) {
        o->oPosY += 100.0f;
        o->oPosX = 2780.0f;
        o->oPosZ = 4666.0f;
        spawn_default_star();
        obj_mark_for_deletion(o);
    }
}
