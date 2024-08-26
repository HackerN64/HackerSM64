/**
 * Exactly the same as BEHIND_MARIO
 */
void mode_water_surface_camera(struct Camera *c) {
    c->nextYaw = mode_behind_mario(c);
}
