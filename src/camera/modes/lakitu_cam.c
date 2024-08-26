/**
 * The mode used by close and free roam
 */
void mode_lakitu_camera(struct Camera *c) {
    gCameraZoomDist = 800.f;
    mode_default_camera(c);
}
