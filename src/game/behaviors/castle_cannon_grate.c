#include <ultra64.h>
#include "course_table.h"
#include "global_object_fields.h"
#include "game/object_helpers.h"

void bhv_castle_cannon_grate_init(void) {
    if (save_file_get_total_star_count(gCurrSaveFileNum - 1, COURSE_MIN - 1, COURSE_MAX - 1) >= 120) {
        o->activeFlags = ACTIVE_FLAG_DEACTIVATED;
    }
}
