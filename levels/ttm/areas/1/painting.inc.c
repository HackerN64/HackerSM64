#include "game/paintings.h"

/// - PAINTING_ID_TTM_SLIDE -

// 0x07012EF8 - 0x07012F78
ALIGNED8 static const Texture *const ttm_seg7_painting_textures_slide[] = {
    ttm_seg7_texture_07004000,
    ttm_seg7_texture_07003000,
};

// 0x07012F00 (PaintingData)
const struct PaintingImage ttm_slide_painting = {
    /* Textures */ ttm_seg7_painting_textures_slide,
    /* Texture Count */ ARRAY_COUNT(ttm_seg7_painting_textures_slide),
    /* Texture w, h */ 64, 32,
    /* Texture Type */ PAINTING_TYPE_IMAGE,
    /* Ripple Trigger */ RIPPLE_TRIGGER_PROXIMITY,
    /* Shaded */ TRUE,
    /* Alpha */ 0xFF,
    /* Size */ 460.8f, 460.8f,
};
