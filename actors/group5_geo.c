#include <ultra64.h>
#include "sm64.h"
#include "geo_commands.h"

#include "make_const_nonconst.h"

#include "common1.h"
#include "group5.h"

#include "klepto/geo.inc.c"
#include "eyerok/geo.inc.c"
#include "pokey/geo.inc.c"

#include "level_commands.h"

const LevelScript script_func_global_6[] = {
    LOAD_MODEL_FROM_GEO(MODEL_POKEY_HEAD,              pokey_head_geo),
    LOAD_MODEL_FROM_GEO(MODEL_POKEY_BODY_PART,         pokey_body_part_geo),
    LOAD_MODEL_FROM_GEO(MODEL_TWEESTER,                tweester_geo),
    LOAD_MODEL_FROM_GEO(MODEL_KLEPTO,                  klepto_geo),
    LOAD_MODEL_FROM_GEO(MODEL_EYEROK_LEFT_HAND,        eyerok_left_hand_geo),
    LOAD_MODEL_FROM_GEO(MODEL_EYEROK_RIGHT_HAND,       eyerok_right_hand_geo),
    RETURN(),
};
