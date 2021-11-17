#include <ultra64.h>
#include "sm64.h"
#include "segment_symbols.h"
#include "level_commands.h"

#include "levels/intro/header.h"

#include "make_const_nonconst.h"

#include "config.h"

extern const LevelScript level_main_scripts_entry[];
const LevelScript level_script_entry[] = {
    INIT_LEVEL(),
    SLEEP(/*frames*/ 2),
    BLACKOUT(/*active*/ FALSE),
#ifdef TEST_LEVEL
    SET_REG(/*value*/ TEST_LEVEL),
    EXECUTE(/*seg*/ SEGMENT_GLOBAL_LEVEL_SCRIPT, /*script*/ _scriptsSegmentRomStart, /*scriptEnd*/ _scriptsSegmentRomEnd, /*entry*/ level_main_scripts_entry),
#else
    SET_REG(/*value*/ 0),
    EXECUTE(/*seg*/ SEGMENT_MENU_INTRO, /*script*/ _introSegmentRomStart, /*scriptEnd*/ _introSegmentRomEnd, /*entry*/ level_intro_splash_screen),
#endif
    JUMP(/*target*/ level_script_entry),
};
