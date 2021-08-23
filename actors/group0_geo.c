#include <ultra64.h>
#include "sm64.h"
#include "geo_commands.h"

#include "make_const_nonconst.h"

#include "common1.h"
#include "group0.h"

#include "bubble/geo.inc.c"
#include "walk_smoke/geo.inc.c"
#include "burn_smoke/geo.inc.c"
#include "stomp_smoke/geo.inc.c"
#include "water_wave/geo.inc.c"
#include "sparkle/geo.inc.c"
#include "water_splash/geo.inc.c"
#include "sparkle_animation/geo.inc.c"
#include "mario/geo.inc.c"

#include "game/farcall_helpers.h"

#include "game/behaviors/sparkle_spawn.inc.c"
#include "game/behaviors/bubble.inc.c"
#include "game/behaviors/water_splashes_and_waves.inc.c"
#include "game/behaviors/white_puff.inc.c"
#include "game/behaviors/pole.inc.c"
#include "game/behaviors/warp.inc.c"
#include "game/behaviors/water_objs.inc.c"
#include "game/behaviors/flame_mario.inc.c"
//#include "game/behaviors/sparkle_spawn_star.inc.c"

#include "game/behaviors/sound_spawner.inc.c"
#include "game/behaviors/sound_waterfall.inc.c"
#include "game/behaviors/sound_volcano.inc.c"
#include "game/behaviors/sound_birds.inc.c"
#include "game/behaviors/sound_ambient.inc.c"
#include "game/behaviors/sound_sand.inc.c"
#include "game/behaviors/cloud.inc.c"
#include "game/behaviors/white_puff_explode.inc.c"
#include "game/behaviors/water_wave.inc.c"

//Temp stuff that's here because the behaviours are heavily shared, or have no visible link.
#include "game/behaviors/bully.inc.c"
#include "game/behaviors/express_elevator.inc.c"
#include "game/behaviors/elevator.inc.c"
#include "game/behaviors/tumbling_bridge.inc.c"
#include "game/behaviors/flamethrower.inc.c"
#include "game/behaviors/bouncing_fireball.inc.c"
#include "game/behaviors/rotating_platform.inc.c"
#include "game/behaviors/bowser_key_cutscene.inc.c"
#include "game/behaviors/sparkle_spawn_star.inc.c" //group0
#ifndef VERSION_JP
#include "game/behaviors/music_touch.inc.c"
#endif
#include "game/behaviors/pole_base.inc.c"
#include "game/behaviors/strong_wind_particle.inc.c"
#include "game/behaviors/seesaw_platform.inc.c"
#include "game/behaviors/sliding_platform_2.inc.c"
#include "game/behaviors/animated_floor_switch.inc.c"
#include "game/behaviors/activated_bf_plat.inc.c" //Used in numerous stages.
#include "game/behaviors/ferris_wheel.inc.c" //Used in two bowser stages
#include "game/behaviors/beta_holdable_object.inc.c"
#include "game/behaviors/falling_rising_platform.inc.c" //Used in numerous levels
#include "game/behaviors/grill_door.inc.c" //Used in both BoB and HMC
#include "game/behaviors/rotating_octagonal_plat.inc.c" //Used in RR and BitS
#include "game/behaviors/tilting_inverted_pyramid.inc.c" //Used in numerous levels
#include "game/behaviors/rolling_log.inc.c" //Used in LLL and TTM
#include "game/behaviors/sliding_platform.inc.c" //Used in BitDW and BitFS
#include "game/behaviors/platform_on_track.inc.c" //Used in numerous courses.
#include "game/behaviors/fish.inc.c" //Dude honestly.
#include "game/behaviors/static_checkered_platform.inc.c" //Unused
#include "game/behaviors/floating_platform.inc.c" //Used in JRB and WDW
