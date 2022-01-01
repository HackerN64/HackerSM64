#pragma once

/*****************
 * MENU SETTINGS *
 *****************/

// -- EXIT COURSE SETTINGS --

// Disable exit course
//#define DISABLE_EXIT_COURSE

// Decides whether you can exit course while moving (has no effect if you disable exit course)
#define EXIT_COURSE_WHILE_MOVING

// Decides which level, area and warp ID the "exit course" option takes you to (has no effect if you disable exit course)
// Ensure that the warp exists, or else the game will crash.
#define EXIT_COURSE_LEVEL LEVEL_CASTLE
#define EXIT_COURSE_AREA 0x01
#define EXIT_COURSE_NODE 0x1F
