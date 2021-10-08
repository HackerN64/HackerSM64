#include <ultra64.h>

#ifndef S2D_CONFIG_H
#define S2D_CONFIG_H

/**
 * SET YOUR FONT NAME HERE
 */
#ifndef FONTNAME
    #define FONTNAME impact
#endif


/***********
 * Helpful defines to change
 ***********/

#define BASE_SCALE 1.0f

/******************************
 *
 * ONLY CHANGE THE BELOW CONTENTS IF YOU'RE DEVELOPING
 * FOR A GAME THAT ISN'T SM64
 *
 *****************************/

// magic macros and font header inclusion
#define GLUE(a, b) a##b
#define GLUE2(a, b) GLUE(a, b)
#define STR(x) #x
#define STR2(x) STR(x)

#define __headername(hd) fonts/hd.h
#define __fontinclude(f) src/s2d_engine/fonts/f.c

#define _FONT_HEADER STR2(__headername(FONTNAME))
#define FONT_C_FILE STR2(__fontinclude(FONTNAME))

#include _FONT_HEADER

// get your font symbols defined here
#define s2d_font          GLUE2(FONTNAME, _obj)
#define s2d_dropshadow    GLUE2(FONTNAME, _obj_dropshadow)
#define s2d_tex           GLUE2(FONTNAME, _tex)
#define s2d_kerning_table GLUE2(FONTNAME, _kerning_table)

extern char s2d_kerning_table[];

// Debug: bypass custom matrix setup
#define s2d_mat GLUE2(FONTNAME, _mtx)

// Homebrew users: use this to rename your gdl head
#define gdl_head gDisplayListHead
extern Gfx *gdl_head;

#include "src/game/game_init.h"
#define CONTROLLER_INPUT gPlayer1Controller->buttonPressed
#define CONTROLLER_HELD_INPUT gPlayer1Controller->buttonDown

// Other games/Homebrew users: change these values

// an allocator function of the format void *alloc(size_t bytes)
#define alloc alloc_display_list
extern void *alloc(size_t);

// your init functions for the RDP/RSP
#define my_rdp_init my_rdp_init
#define my_rsp_init my_rsp_init
extern void my_rsp_init(void);
extern void my_rdp_init(void);

// The frame timer that is used to time s2d_type_print
#define s2d_timer gGlobalTimer
extern u32 s2d_timer;

// The equivalent vsprintf in your game (defaults to libultra _Printf)
#define vsprintf(dst, str, fmt, args) _Printf(proutSprintf, dst, fmt, args)
extern char *proutSprintf(char *dst, const char *src, size_t count);


// tab glyph width and height
#define TAB_WIDTH_H TEX_WIDTH * 2
#define TAB_WIDTH_V TEX_WIDTH / 2

// Set these to optimize s2d texture "cache"
#define TEX_WIDTH 16
#define TEX_HEIGHT 16
#define TEX_BITDEPTH 8

#define seg2virt segmented_to_virtual
#define IS_RUNNING_ON_EMULATOR (IO_READ(DPC_PIPEBUSY_REG) == 0)

// Texture resolution (pixels on the texture per pixel on the framebuffer)
#define TEX_RES 1

#define _NUM_CACHE (4096 / (TEX_WIDTH * TEX_HEIGHT * (TEX_BITDEPTH / 8)))

#endif