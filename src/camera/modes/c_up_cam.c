
/**
 * The main update function for C-Up mode
 */
void mode_c_up_camera(struct Camera *c) {
    // Play a sound when entering C-Up mode
    if (!(sCameraSoundFlags & CAM_SOUND_C_UP_PLAYED)) {
        play_sound_cbutton_up();
        sCameraSoundFlags |= CAM_SOUND_C_UP_PLAYED;
    }

    // Zoom in first
    if (gCameraMovementFlags & CAM_MOVING_INTO_MODE) {
        gCameraMovementFlags |= CAM_MOVE_C_UP_MODE;
        move_into_c_up(c);
        return;
    }

    if (!(gCameraMovementFlags & CAM_MOVE_STARTED_EXITING_C_UP)) {
        // Normal update
        move_mario_head_c_up(c);
        update_c_up(c, c->focus, c->pos);
    } else {
        // Exiting C-Up
        if (sStatusFlags & CAM_FLAG_TRANSITION_OUT_OF_C_UP) {
            // Retrieve the previous position and focus
            vec3f_copy(c->pos, sCameraStoreCUp.pos);
            vec3f_add(c->pos, sMarioCamState->pos);
            vec3f_copy(c->focus, sCameraStoreCUp.focus);
            vec3f_add(c->focus, sMarioCamState->pos);
            // Make Mario look forward
            camera_approach_s16_symmetric_bool(&sMarioCamState->headRotation[0], 0, 1024);
            camera_approach_s16_symmetric_bool(&sMarioCamState->headRotation[1], 0, 1024);
        } else {
            // Finished exiting C-Up
            gCameraMovementFlags &= ~(CAM_MOVE_STARTED_EXITING_C_UP | CAM_MOVE_C_UP_MODE);
        }
    }
    sPanDistance = 0.f;

    // Exit C-Up mode
    if (gPlayer1Controller->buttonPressed & (A_BUTTON | B_BUTTON | D_CBUTTONS | L_CBUTTONS | R_CBUTTONS)) {
        exit_c_up(c);
    }
}
