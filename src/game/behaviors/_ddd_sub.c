#include <ultra64.h>
#include "global_object_fields.h"
#include "object_helpers.h"

// ddd_sub.inc.c

void bhv_bowsers_sub_init(void) {
    if (gCurrActNum != 1) {
        obj_mark_for_deletion(o);
    } else {
        load_object_static_model();
    }
}
