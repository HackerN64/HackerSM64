#include <ultra64.h>
#include "sm64.h"
#include "surface_terrains.h"
#include "moving_texture_macros.h"
#include "textures.h"

#include "make_const_nonconst.h"

// Unfortunately this garbage is referenced from get_quad_collection_from_id
// Ideally scrolls should be recoded to be fast64 like
#include "levels/bbh/areas/1/movtext.inc.c"

#include "levels/bbh/leveldata.inc.c"
