// == dialog ==
// (defines en_dialog_table etc.)

#include "game/ingame_menu.h"

#undef DEFINE_DIALOG
#define DEFINE_DIALOG(id, _1, _2, _3, _4, str) \
    static const char dialog_text_ ## id[] = { str };

#include DIALOG_FILE

#undef DEFINE_DIALOG
#define DEFINE_DIALOG(id, voice, linesPerBox, leftOffset, width, _) \
    static const struct DialogEntry dialog_entry_ ## id = { \
        voice, linesPerBox, leftOffset, width, dialog_text_ ## id \
    };

#include DIALOG_FILE

#undef DEFINE_DIALOG
#define DEFINE_DIALOG(id, _1, _2, _3, _4, _5) &dialog_entry_ ## id,

const struct DialogEntry *const DIALOG_TABLE[] = {
#include DIALOG_FILE
    NULL
};


// == courses ==
// (defines en_course_name_table etc.)
// The game duplicates this in levels/menu/leveldata.c in EU, so we split
// it out into a separate include file.

#include "define_courses.inc.c"

// == acts ==
// (defines en_act_name_table etc.)

#define COURSE_ACTS(id, name, a,b,c,d,e,f) \
    static const char act_name_ ## id ## _1[] = { a }; \
    static const char act_name_ ## id ## _2[] = { b }; \
    static const char act_name_ ## id ## _3[] = { c }; \
    static const char act_name_ ## id ## _4[] = { d }; \
    static const char act_name_ ## id ## _5[] = { e }; \
    static const char act_name_ ## id ## _6[] = { f };

#define SECRET_STAR(id, name)
#define CASTLE_SECRET_STARS(str)

#undef EXTRA_TEXT
#define EXTRA_TEXT(id, str) \
    static const char extra_text_ ## id[] = { str };

#include COURSE_FILE

#undef COURSE_ACTS
#undef EXTRA_TEXT

#define COURSE_ACTS(id, name, a,b,c,d,e,f) \
    act_name_ ## id ## _1, act_name_ ## id ## _2, act_name_ ## id ## _3, \
    act_name_ ## id ## _4, act_name_ ## id ## _5, act_name_ ## id ## _6,
#define EXTRA_TEXT(id, str) extra_text_ ## id,

const char *const ACT_NAME_TABLE[] = {
#include COURSE_FILE
    NULL
};

#undef COURSE_ACTS
#undef EXTRA_TEXT
