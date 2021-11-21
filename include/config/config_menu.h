#pragma once

/*****************
 * MENU SETTINGS *
 *****************/

// -- EXIT COURSE SETTINGS --

// Disable exit course
//#define DISABLE_EXIT_COURSE

// Decides whether you can exit course while moving (has no effect if you disable exit course)
#define EXIT_COURSE_WHILE_MOVING

// Decides which level "exit course" takes you to (has no effect if you disable exit course)
#define EXIT_COURSE_LEVEL LEVEL_CASTLE

// Decides the area node "exit course" takes you to (has no effect if you disable exit course)
#define EXIT_COURSE_AREA 0x01

// Decides the warp node "exit course" takes you to (has no effect if you disable exit course)
#define EXIT_COURSE_NODE 0x1F


// -- Compatibility safeguards. Don't mess with these unless you know what you're doing. --

#ifdef DISABLE_EXIT_COURSE
#undef EXIT_COURSE_WHILE_MOVING
#undef EXIT_COURSE_LEVEL
#undef EXIT_COURSE_AREA
#undef EXIT_COURSE_NODE
#endif // DISABLE_EXIT_COURSE
