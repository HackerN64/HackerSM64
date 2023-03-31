#pragma once

#include <ultra64.h>

#include "types.h"
#include "crash_screen.h"


// Address Select constants
#define JUMP_MENU_CHARS_X 20
#define JUMP_MENU_CHARS_Y  5

#define JUMP_MENU_W (TEXT_WIDTH( JUMP_MENU_CHARS_X))
#define JUMP_MENU_H (TEXT_HEIGHT(JUMP_MENU_CHARS_Y))

#define JUMP_MENU_X1 (SCREEN_CENTER_X - (JUMP_MENU_W / 2))
#define JUMP_MENU_Y1 (SCREEN_CENTER_Y - (JUMP_MENU_H / 2))

#define JUMP_MENU_X2 (SCREEN_CENTER_X + (JUMP_MENU_W / 2))
#define JUMP_MENU_Y2 (SCREEN_CENTER_Y + (JUMP_MENU_H / 2))

#define JUMP_MENU_MARGIN_X 10
#define JUMP_MENU_MARGIN_Y 10

// Macros used to modify individual digits in a hexadecimal value.
#define GET_HEX_DIGIT(src, shift)       (((src) >> (shift)) & BITMASK(4))
#define SET_HEX_DIGIT(dst, src, shift)  (((dst) & ~(BITMASK(4) << (shift))) | ((src) << (shift)))


extern _Bool gAddressSelectMenuOpen;


void draw_address_select(void);
void crash_screen_select_address(void);
void open_address_select(uintptr_t dest);
