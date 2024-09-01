#include <ultra64.h>
#include "sm64.h"
#include "surface_terrains.h"
#include "geo_commands.h"

#include "make_const_nonconst.h"

// Note: This bin does not use bin IDs, unlike the other segmented bins.
#include "mario/model.inc.c"

#include "bubble/model.inc.c"

#include "walk_smoke/model.inc.c"

#include "burn_smoke/model.inc.c"

#include "small_water_splash/model.inc.c"

#include "water_wave/model.inc.c"

#include "sparkle/model.inc.c"

#include "water_splash/model.inc.c"

#include "white_particle_small/model.inc.c"

#include "sparkle_animation/model.inc.c"

#ifdef S2DEX_TEXT_ENGINE
#include "src/s2d_engine/s2d_config.h"
#include FONT_C_FILE
#endif


#include "physcube/model.inc.c"
#include "physcube/collision.inc.c"
#include "physsphere/model.inc.c"
#include "physsphere/collision.inc.c"
#include "m_body/model.inc.c"
#include "m_head/model.inc.c"
#include "m_shoulder_l/model.inc.c"
#include "m_shoulder_r/model.inc.c"
#include "m_arm_l/model.inc.c"
#include "m_arm_r/model.inc.c"
#include "m_hand_l/model.inc.c"
#include "m_hand_r/model.inc.c"
#include "m_thigh_l/model.inc.c"
#include "m_thigh_r/model.inc.c"
#include "m_leg_l/model.inc.c"
#include "m_leg_r/model.inc.c"
#include "m_foot_l/model.inc.c"
#include "m_foot_r/model.inc.c"
