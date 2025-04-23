#include <PR/ultratypes.h>
#include <PR/gbi.h>

#include "dialog_ids.h"
#include "surface_terrains.h"
#include "macros.h"
#include "moving_texture_macros.h"
#include "surface_terrains.h"
#include "textures.h"
#include "types.h"

#include "make_const_nonconst.h"

#include "actors/vanilla_actors/sl/texture.inc.c"

#include "actors/vanilla_actors/sl/snow_mound/model.inc.c"
#include "actors/vanilla_actors/sl/unused_cracked_ice/model.inc.c"
#include "actors/vanilla_actors/sl/unused_ice_shard/model.inc.c" // Used in the unused behavior when ground pounding the cracked ice show in the above model.
#include "actors/vanilla_actors/sl/snow_mound/collision.inc.c"
#include "actors/vanilla_actors/sl/unused_cracked_ice/collision.inc.c"
