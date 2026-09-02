#include "segment_names.h"
#ifdef SKYBOX_SYMBOLS
#define DEFINE_SKYBOX(symbol, enumeration) extern SkyboxTexture symbol##_skybox_ptrlist;
#endif

#ifdef SKYBOX_SYMBOLS_ROM
#define DEFINE_SKYBOX(symbol, enumeration) \
    DECLARE_SEGMENT(symbol##_skybox_yay0) \
    DECLARE_SEGMENT(symbol##_skybox_mio0)
#endif

#ifdef SKYBOX_ENUM
#define DEFINE_SKYBOX(symbol, enumeration) enumeration,
#endif

#ifdef SKYBOX_LD
#define DEFINE_SKYBOX(symbol, enumeration) YAY0_SEG(symbol##_skybox, SEGMENT_SKYBOX << 24)
#endif

/**
 * @brief Defines skybox properties.
 *
 * @param symbol      - Filename of the skybox used for symbol generation.
 * @param enumeration - Name used for the enum.
*/

DEFINE_SKYBOX(water,        BACKGROUND_OCEAN_SKY)
DEFINE_SKYBOX(bitfs,        BACKGROUND_FLAMING_SKY)
DEFINE_SKYBOX(wdw,          BACKGROUND_UNDERWATER_CITY)
DEFINE_SKYBOX(cloud_floor,  BACKGROUND_BELOW_CLOUDS)
DEFINE_SKYBOX(ccm,          BACKGROUND_SNOW_MOUNTAINS)
DEFINE_SKYBOX(ssl,          BACKGROUND_DESERT)
DEFINE_SKYBOX(bbh,          BACKGROUND_HAUNTED)
DEFINE_SKYBOX(bidw,         BACKGROUND_GREEN_SKY)
DEFINE_SKYBOX(clouds,       BACKGROUND_ABOVE_CLOUDS)
DEFINE_SKYBOX(bits,         BACKGROUND_PURPLE_SKY)

#undef DEFINE_SKYBOX
