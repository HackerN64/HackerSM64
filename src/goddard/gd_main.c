#include <PR/ultratypes.h>

#include "debug_utils.h"
#include "gd_main.h"
#include "gd_memory.h"
#include "macros.h"
#include "objects.h"
#include "renderer.h"

/* This file was spilt out of debug_memory.asm based on rodata.
 * The rodata for functions after this "__main__" function have string literals following
 * f32 literal, which implies that this is its own file
 */

// data
s32 gGdMoveScene = TRUE; // @ 801A8050
UNUSED static s32 sUnref801A8054 = TRUE;
f32 D_801A8058 = -600.0f;
s32 gGdUseVtxNormal = TRUE; // @ 801A805C; instead of face normals
UNUSED static s32 sUnrefScnWidth = 320;
UNUSED static s32 sUnrefScnHeight = 240;

// bss
struct GdControl gGdCtrl;     // @ 801B9920; processed controller info
struct GdControl gGdCtrlPrev; // @ 801B9A18; previous frame's controller info
