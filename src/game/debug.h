#ifndef DEBUG_H
#define DEBUG_H

#include <PR/ultratypes.h>

enum DebugPage {
    DEBUG_PAGE_OBJECTINFO,       // 0: objectinfo
    DEBUG_PAGE_CHECKSURFACEINFO, // 1: checkinfo/surfaceinfo
    DEBUG_PAGE_MAPINFO,          // 2: mapinfo
    DEBUG_PAGE_STAGEINFO,        // 3: stageinfo
    DEBUG_PAGE_EFFECTINFO,       // 4: effectinfo
    DEBUG_PAGE_ENEMYINFO         // 5: enemyinfo
};

#ifdef VANILLA_DEBUG
s64 get_current_clock(void);
s64 get_clock_difference(UNUSED s64 cycles);
void set_text_array_x_y(s32 xOffset, s32 yOffset);
void print_debug_top_down_objectinfo(const char *str, s32 number);
void print_debug_top_down_mapinfo(const char *str, s32 number);
void print_debug_bottom_up(const char *str, s32 number);
void debug_unknown_level_select_check(void);
void reset_debug_objectinfo(void);
void stub_debug_control(void);
void try_print_debug_mario_object_info(void);
void try_do_mario_debug_object_spawn(void);
void try_print_debug_mario_level_info(void);
#else
#define get_current_clock()
#define get_clock_difference(cycles)
#define set_text_array_x_y(xOffset, yOffset)
#define print_debug_top_down_objectinfo(str, number)
#define print_debug_top_down_mapinfo(str, number)
#define print_debug_bottom_up(str, number)
#define debug_unknown_level_select_check()
#define reset_debug_objectinfo()
#define stub_debug_control()
#define try_print_debug_mario_object_info()
#define try_do_mario_debug_object_spawn()
#define try_print_debug_mario_level_info()
#endif

extern char *__n64Assert_Filename;
extern u32   __n64Assert_LineNum;
extern char *__n64Assert_Message;
extern void __n64Assert(char *fileName, u32 lineNum, char *message);


#ifdef DEBUG
#define assert(cond, message) do {\
    if ((cond) == FALSE) { \
        __n64Assert(__FILE__, __LINE__, (message)); \
    } \
} while (0);
#else
#define assert(cond, message)
#endif


#endif // DEBUG_H
