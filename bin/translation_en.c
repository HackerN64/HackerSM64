// SM64 (EU) Segment 19 - English

#include "macros.h"

#include "game/ingame_menu.h"
#include "sounds.h"
#include "seq_ids.h"

#include "make_const_nonconst.h"

// Include text/define_text.inc.c, preprocessed with -I text/us/ to get the
// right translation strings, with symbols renamed as below.
#define seg2_course_name_table course_name_table_eu_en
#define seg2_act_name_table act_name_table_eu_en
#define seg2_dialog_table dialog_table_eu_en

#define DIALOG_FILE "us/dialogs.h"
#define COURSE_FILE "us/courses.h"
#include "text/define_text.inc.c"
#undef DIALOG_FILE
#undef COURSE_FILE
