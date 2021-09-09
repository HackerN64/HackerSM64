// ddd_warp.c.inc

void bhv_ddd_warp_loop(void) {
#ifndef UNLOCK_ALL
    if (gDddPaintingStatus & BOWSERS_SUB_BEATEN)
#endif
        o->collisionData = segmented_to_virtual(inside_castle_seg7_collision_ddd_warp_2);
#ifndef UNLOCK_ALL
    else
        o->collisionData = segmented_to_virtual(inside_castle_seg7_collision_ddd_warp);
#endif
}
