#pragma once

#include "game/camera.h"

struct CameraMode {
    void (*modeFunc)(struct Camera *c);
    void (*updateFunc)(struct Camera *c);
};

// mode-specific export variables
extern s16 sSpiralStairsYawOffset;

void exit_c_up(struct Camera *c);
void radial_camera_input(struct Camera *c);
void radial_camera_move(struct Camera *c);
void radial_camera_input_default(struct Camera *c);
void update_yaw_and_dist_from_c_up(UNUSED struct Camera *c);
void parallel_tracking_init(struct Camera *c, struct ParallelTrackingPoint *path);

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

void set_camera_height(struct Camera *c, f32 goalHeight);
void lakitu_zoom(f32 rangeDist, s16 rangePitch);
s16 look_down_slopes(s16 camYaw);
void pan_ahead_of_player(struct Camera *c);
s32 mode_behind_mario(struct Camera *c);
void calc_y_to_curr_floor(f32 *posOff, f32 posMul, f32 posBound, f32 *focOff, f32 focMul, f32 focBound);
