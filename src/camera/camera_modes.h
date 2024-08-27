#pragma once

#include "game/camera.h"

struct CameraMode {
    void (*modeFunc)(struct Camera *c);
    void (*updateFunc)(struct Camera *c);
};

void exit_c_up(struct Camera *c);
void radial_camera_input(struct Camera *c);
void radial_camera_move(struct Camera *c);
void radial_camera_input_default(struct Camera *c);
void update_yaw_and_dist_from_c_up(UNUSED struct Camera *c);

void mode_8_directions_camera(struct Camera *c);
void mode_behind_mario_camera(struct Camera *c);
void mode_boss_fight_camera(struct Camera *c);
void mode_c_up_camera(struct Camera *c);
void mode_cannon_camera(struct Camera *c);
void mode_default_camera(struct Camera *c);
void mode_fixed_camera(struct Camera *c);
void mode_lakitu_camera(struct Camera *c);
void mode_mario_camera(struct Camera *c);
void mode_outward_radial_camera(struct Camera *c);
void mode_parallel_tracking_camera(struct Camera *c);
void mode_radial_camera(struct Camera *c);
void mode_slide_camera(struct Camera *c);
void mode_spiral_stairs_camera(struct Camera *c);
void mode_water_surface_camera(struct Camera *c);
