#include "game/paintings.h"

/// - PAINTING_ID_TTM_SLIDE -

// 0x07012EF8 - 0x07012F78
ALIGNED8 static const Texture *const ttm_seg7_painting_textures_slide[] = {
    ttm_seg7_texture_07004000,
    ttm_seg7_texture_07003000,
};

const struct PaintingImage ttm_slide_painting = {
    .textureArray  = ttm_seg7_painting_textures_slide,
    .imageCount    = ARRAY_COUNT(ttm_seg7_painting_textures_slide),
    .textureWidth  = 64,
    .textureHeight = 32,
    .imageType     = PAINTING_IMAGE_TYPE_TEXTURE,
    .rippleTrigger = RIPPLE_TRIGGER_PROXIMITY,
    .shaded        = TRUE,
    .alpha         = 0xFF,
    .sizeX         = 460.8f,
    .sizeY         = 460.8f,
};
