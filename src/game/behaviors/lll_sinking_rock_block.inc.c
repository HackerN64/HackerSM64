// lll_sinking_rock_block.inc.c

void bhv_lll_sinking_rock_block_loop(void) {
    lll_octagonal_mesh_find_y_offset(&o->oSinkWhenSteppedOnTimer, &o->oSinkWhenSteppedOnYOffsetFromHome, 124, -110);
    o->oGraphYOffset = 0.0f;
    o->oPosY = o->oHomeY + o->oSinkWhenSteppedOnYOffsetFromHome;
}
