#pragma once

#include "types.h"
#include "game/camera.h"

// HACKERSM64_DO: Might rename these to reflect what they are used for instead "SET_45" etc.
enum CameraFov {
    CAM_FOV_NONE,
    CAM_FOV_SET_45,
    CAM_FOV_DEFAULT,
    CAM_FOV_UNUSED_3,
    CAM_FOV_APP_45,
    CAM_FOV_SET_30,
    CAM_FOV_APP_20,
    CAM_FOV_BBH,
    CAM_FOV_UNUSED_8,
    CAM_FOV_APP_80,
    CAM_FOV_APP_30,
    CAM_FOV_APP_60,
    CAM_FOV_ZOOM_30,
    CAM_FOV_SET_29
};

/**
 * Info for the camera's field of view and the FOV shake effect.
 * TODO: to camera_geo
 */
struct CameraFOVStatus {
    /// The current function being used to set the camera's field of view (before any fov shake is applied).
    /*0x00*/ u8 fovFunc;
    /// The current field of view in degrees
    /*0x04*/ f32 fov;

    // Fields used by shake_camera_fov()

    /// The amount to change the current fov by in the fov shake effect.
    /*0x08*/ f32 fovOffset;
    /// A bool set in fov_default() but unused otherwise
    /*0x0C*/ u32 unusedIsSleeping;
    /// The range in degrees to shake fov
    /*0x10*/ f32 shakeAmplitude;
    /// Used to calculate fovOffset, the phase through the shake's period.
    /*0x14*/ s16 shakePhase;
    /// How much to progress through the shake period
    /*0x16*/ s16 shakeSpeed;
    /// How much to decrease shakeAmplitude each frame.
    /*0x18*/ s16 decay;
};

void initialize_camera_fov(void);
void set_camera_fov(f32 fov);
f32 get_camera_fov(void);
void set_fov_shake(s16 amplitude, s16 decay, s16 shakeSpeed);
void set_fov_function(u8 func);
Gfx *geo_camera_fov(s32 callContext, struct GraphNode *g, UNUSED void *context);
Gfx *geo_camera_main(s32 callContext, struct GraphNode *g, void *context);
