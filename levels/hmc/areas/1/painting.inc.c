#include "game/paintings.h"

/// - PAINTING_ID_HMC_COTMC -

// 0x07024CE0 - 0x070254E0
ALIGNED8 static const Texture hmc_seg7_texture_cotmc_pool_env[] = {
#include "levels/hmc/7.rgba16.inc.c"
};

// 0x07025518 - 0x07025594
const Texture *const hmc_seg7_painting_textures_cotmc[] = {
    hmc_seg7_texture_cotmc_pool_env,
};

const struct PaintingImage cotmc_painting = {
    .textureArray  = hmc_seg7_painting_textures_cotmc,
    .imageCount    = ARRAY_COUNT(hmc_seg7_painting_textures_cotmc),
    .textureWidth  = 32,
    .textureHeight = 32,
    .imageType     = PAINTING_IMAGE_TYPE_ENV_MAP,
    .rippleTrigger = RIPPLE_TRIGGER_CONTINUOUS,
    .shaded        = TRUE,
    .alpha         = 0xFF,
    .sizeX         = 723.968018f,
    .sizeY         = 723.968018f,
};
