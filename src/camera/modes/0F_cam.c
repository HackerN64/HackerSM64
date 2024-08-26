
static UNUSED void unused_mode_0f_camera(struct Camera *c) {
    if (gPlayer1Controller->buttonPressed & U_CBUTTONS) {
        gCameraMovementFlags |= CAM_MOVE_C_UP_MODE;
    }
    c->nextYaw = update_slide_camera(c);
}
