#include "game/paintings.h"

const GeoLayout painting_geo[] = {
   GEO_CULLING_RADIUS(2000),
   GEO_OPEN_NODE(),
      GEO_ASM(0, geo_painting_draw),
   GEO_CLOSE_NODE(),
   GEO_END(),
};
