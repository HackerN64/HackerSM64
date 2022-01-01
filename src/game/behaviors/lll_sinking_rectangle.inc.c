// lll_sinking_rectangle.inc.c

void sinking_rectangular_plat_actions(f32 a0, s32 a1) {
    switch (o->oAction) {
        case 0:
            o->oAction++;
            break;

        case 1:
            o->oPosY -= sins(o->oLllWoodPieceOscillationTimer) * a0;
            o->oLllWoodPieceOscillationTimer += a1;
            break;

        case 2:
            break;
    }
}

void bhv_lll_sinking_rectangular_platform_loop(void) {
    if (o->oMoveAngleYaw != 0) {
        sinking_rectangular_plat_actions(0.4f, 0x100);
    } else {
        o->oFaceAnglePitch = sins(o->oLllWoodPieceOscillationTimer) * 0x200;
        o->oLllWoodPieceOscillationTimer += 0x100;
    }
}

void bhv_lll_sinking_square_platforms_loop(void) {
    sinking_rectangular_plat_actions(0.5f, 0x100);
}
