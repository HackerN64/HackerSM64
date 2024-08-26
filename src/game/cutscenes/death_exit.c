/**
 * Cutscene that plays when Mario dies and warps back to the castle.
 */

/**
 * Warp the camera to Mario, then use his faceAngle to calculate the right relative position.
 *
 * cvar0.point is Mario's position
 * cvar0.angle is Mario's faceAngle
 *
 * cvar1 is the camera's position relative to Mario
 * cvar2 is the camera's focus relative to Mario
 */
void cutscene_exit_painting_start(struct Camera *c) {
    struct Surface *floor;
    f32 floorHeight;

    vec3f_set(sCutsceneVars[2].point, 258.f, -352.f, 1189.f);
    vec3f_set(sCutsceneVars[1].point, 65.f, -155.f, 444.f);

    if (gPrevLevel == LEVEL_TTM) {
        sCutsceneVars[1].point[1] = 0.f;
        sCutsceneVars[1].point[2] = 0.f;
    }
    vec3f_copy(sCutsceneVars[0].point, sMarioCamState->pos);
    sCutsceneVars[0].angle[0] = 0;
    sCutsceneVars[0].angle[1] = sMarioCamState->faceAngle[1];
    sCutsceneVars[0].angle[2] = 0;
    offset_rotated(c->focus, sCutsceneVars[0].point, sCutsceneVars[1].point, sCutsceneVars[0].angle);
    offset_rotated(c->pos, sCutsceneVars[0].point, sCutsceneVars[2].point, sCutsceneVars[0].angle);
    floorHeight = find_floor(c->pos[0], c->pos[1] + 10.f, c->pos[2], &floor);

    if (floorHeight != FLOOR_LOWER_LIMIT) {
        if (c->pos[1] < (floorHeight += 60.f)) {
            c->pos[1] = floorHeight;
        }
    }
}

/**
 * Decrease cvar2's x and z offset, moving closer to Mario.
 */
void cutscene_exit_painting_move_to_mario(struct Camera *c) {
    Vec3f pos;

    //! Tricky math: Since offset_rotated() flips Z offsets, you'd expect a positive Z offset to move
    //! the camera into the wall. However, Mario's faceAngle always points into the painting, so a
    //! positive Z offset moves the camera "behind" Mario, away from the painting.
    //!
    //! In the success cutscene, when Mario jumps out face-first, only his gfx angle is updated. His
    //! actual face angle isn't updated until after the cutscene.
    approach_f32_asymptotic_bool(&sCutsceneVars[2].point[0], 178.f, 0.05f);
    approach_f32_asymptotic_bool(&sCutsceneVars[2].point[2], 889.f, 0.05f);
    offset_rotated(pos, sCutsceneVars[0].point, sCutsceneVars[2].point, sCutsceneVars[0].angle);
    c->pos[0] = pos[0];
    c->pos[2] = pos[2];
}

/**
 * Move the camera down to the floor Mario lands on.
 */
void cutscene_exit_painting_move_to_floor(struct Camera *c) {
    struct Surface *floor;
    Vec3f floorHeight;

    vec3f_copy(floorHeight, sMarioCamState->pos);
    floorHeight[1] = find_floor(sMarioCamState->pos[0], sMarioCamState->pos[1] + 10.f, sMarioCamState->pos[2], &floor);

    if (floor != NULL) {
        floorHeight[1] = floorHeight[1] + (sMarioCamState->pos[1] - floorHeight[1]) * 0.7f + 125.f;
        approach_vec3f_asymptotic(c->focus, floorHeight, 0.2f, 0.2f, 0.2f);

        if (floorHeight[1] < c->pos[1]) {
            approach_f32_asymptotic_bool(&c->pos[1], floorHeight[1], 0.05f);
        }
    }
}

/**
 * Cutscene played when Mario leaves a painting, either due to death or collecting a star.
 */
void cutscene_exit_painting(struct Camera *c) {
    cutscene_event(cutscene_exit_painting_start, c, 0, 0);
    cutscene_event(cutscene_exit_painting_move_to_mario, c, 5, -1);
    cutscene_event(cutscene_exit_painting_move_to_floor, c, 5, -1);

    //! Hardcoded position. TTM's painting is close to an opposite wall, so just fix the pos.
    if (gPrevLevel == LEVEL_TTM) {
        vec3f_set(c->pos, -296.f, 1261.f, 3521.f);
    }

    update_camera_yaw(c);
}

struct Cutscene sCutsceneDeathExit[] = {
    { cutscene_exit_painting, 118 },
    { cutscene_exit_painting_end, 0 }
};
