#undef COURSE_ACTS
#define COURSE_ACTS(id, name, a,b,c,d,e,f) \
    static const char GLUE2(COURSE_NAME_TABLE, _ ## id)[] = { name };

#undef SECRET_STAR
#define SECRET_STAR(id, name) \
    static const char GLUE2(COURSE_NAME_TABLE, _ ## id)[] = { name };

#undef CASTLE_SECRET_STARS
#define CASTLE_SECRET_STARS(str) \
    static const char GLUE2(COURSE_NAME_TABLE, _castle_secret_stars)[] = { str };

#define EXTRA_TEXT(id, str)

#include COURSE_FILE

#undef COURSE_ACTS
#undef SECRET_STAR
#undef CASTLE_SECRET_STARS

#define COURSE_ACTS(id, name, a,b,c,d,e,f) GLUE2(COURSE_NAME_TABLE, _ ## id),
#define SECRET_STAR(id, name) GLUE2(COURSE_NAME_TABLE, _ ## id),
#define CASTLE_SECRET_STARS(str) GLUE2(COURSE_NAME_TABLE, _castle_secret_stars),

const char *const COURSE_NAME_TABLE[] = {
#include COURSE_FILE
    NULL
};

#undef COURSE_ACTS
#undef SECRET_STAR
#undef CASTLE_SECRET_STARS

