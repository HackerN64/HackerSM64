#ifndef GD_SFX_H
#define GD_SFX_H

#include <PR/ultratypes.h>

// Sfx for Mario Head Screen
enum GdSfx {
    GD_SFX_NONE           = (0 << 0), // 0x00
    GD_SFX_HAND_APPEAR    = (1 << 0), // 0x01
    GD_SFX_HAND_DISAPPEAR = (1 << 1), // 0x02
    GD_SFX_UNUSED_COIN    = (1 << 2), // 0x04
    GD_SFX_PINCH_FACE     = (1 << 3), // 0x08
    GD_SFX_PINCH_FACE_2   = (1 << 4), // 0x10
    GD_SFX_LET_GO_FACE    = (1 << 5), // 0x20
    GD_SFX_CAM_ZOOM_IN    = (1 << 6), // 0x40
    GD_SFX_CAM_ZOOM_OUT   = (1 << 7), // 0x80
};

// functions
void gd_reset_sfx(void);
u32 gd_new_sfx_to_play(void);
void gd_sfx_update(void);
void gd_play_sfx(enum GdSfx sfx);

#endif // GD_SFX_H
