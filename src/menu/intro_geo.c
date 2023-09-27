#include <PR/ultratypes.h>

#include "game/memory.h"
#include "game/segment2.h"
#include "game/segment7.h"
#include "engine/math_util.h"
#include "engine/colors.h"
#include "intro_geo.h"
#include "sm64.h"
#include "textures.h"
#include "types.h"
#include "buffers/framebuffers.h"
#include "game/game_init.h"
#include "game/save_file.h" //! TODO: Change this to "game/ingame_menu.h" after ASCII/multilang is merged.
#include "audio/external.h"

// frame counts for the zoom in, hold, and zoom out of title model
#define INTRO_STEPS_ZOOM_IN  20
#define INTRO_STEPS_HOLD_1   75
#define INTRO_STEPS_ZOOM_OUT 91

// background types
enum IntroBackgroundTypes {
    INTRO_BACKGROUND_SUPER_MARIO,
    INTRO_BACKGROUND_GAME_OVER
};

struct GraphNodeMore {
    /*0x00*/ struct GraphNode node;
    /*0x14*/ void *todo;
    /*0x18*/ u32 bgTableID;
};

// intro geo bss
static s32 sGameOverFrameCounter;
static s32 sGameOverTableIndex;
static s16 sIntroFrameCounter;
static s32 sTmCopyrightAlpha;

/**
 * Geo callback to render the "Super Mario 64" logo on the title screen
 */
Gfx *geo_intro_super_mario_64_logo(s32 callContext, struct GraphNode *node, UNUSED void *context) {
    struct GraphNode *graphNode = node;
    Gfx *dl = NULL;
    Gfx *dlIter = NULL;

    if (callContext != GEO_CONTEXT_RENDER) {
        sIntroFrameCounter = 0;
    } else if (callContext == GEO_CONTEXT_RENDER) {
        f32 *scaleTable1 = segmented_to_virtual(intro_seg7_table_scale_1);
        f32 *scaleTable2 = segmented_to_virtual(intro_seg7_table_scale_2);
        SET_GRAPH_NODE_LAYER(graphNode->flags, LAYER_OPAQUE);
        Mtx *scaleMat = alloc_display_list(sizeof(*scaleMat));
        dl = alloc_display_list(4 * sizeof(*dl));
        dlIter = dl;
        Vec3f scale;

        // determine scale based on the frame counter
        if (sIntroFrameCounter >= 0 && sIntroFrameCounter < INTRO_STEPS_ZOOM_IN) {
            // zooming in
            vec3f_copy(scale, &scaleTable1[sIntroFrameCounter * 3]);
        } else if (sIntroFrameCounter >= INTRO_STEPS_ZOOM_IN && sIntroFrameCounter < INTRO_STEPS_HOLD_1) {
            // holding
            vec3_same(scale, 1.0f);
        } else if (sIntroFrameCounter >= INTRO_STEPS_HOLD_1 && sIntroFrameCounter < INTRO_STEPS_ZOOM_OUT) {
            // zooming out
            vec3f_copy(scale, &scaleTable2[(sIntroFrameCounter - INTRO_STEPS_HOLD_1) * 3]);
        } else {
            // disappeared
            vec3_zero(scale);
        }
        guScale(scaleMat, scale[0], scale[1], scale[2]);

        gSPMatrix(dlIter++, scaleMat, G_MTX_MODELVIEW | G_MTX_MUL | G_MTX_PUSH);
        gSPDisplayList(dlIter++, &intro_seg7_dl_main_logo);  // draw model
        gSPPopMatrix(dlIter++, G_MTX_MODELVIEW);
        gSPEndDisplayList(dlIter);

        sIntroFrameCounter++;
    }
    return dl;
}

/**
 * Geo callback to render TM and Copyright on the title screen
 */
Gfx *geo_intro_tm_copyright(s32 callContext, struct GraphNode *node, UNUSED void *context) {
    struct GraphNode *graphNode = node;
    Gfx *dl = NULL;
    Gfx *dlIter = NULL;

    if (callContext != GEO_CONTEXT_RENDER) { // reset
        sTmCopyrightAlpha = 0;
    } else if (callContext == GEO_CONTEXT_RENDER) { // draw
        dl = alloc_display_list(5 * sizeof(*dl));
        dlIter = dl;
        gSPDisplayList(dlIter++, dl_proj_mtx_fullscreen);
        gDPSetEnvColor(dlIter++, 255, 255, 255, sTmCopyrightAlpha);
        if (sTmCopyrightAlpha == 255) { // opaque
            SET_GRAPH_NODE_LAYER(graphNode->flags, LAYER_OPAQUE);
            gDPSetRenderMode(dlIter++, G_RM_AA_OPA_SURF, G_RM_AA_OPA_SURF2);
        } else { // blend
            SET_GRAPH_NODE_LAYER(graphNode->flags, LAYER_TRANSPARENT);
            gDPSetRenderMode(dlIter++, G_RM_AA_XLU_SURF, G_RM_AA_XLU_SURF2);
        }
        gSPDisplayList(dlIter++, &intro_seg7_dl_copyright_trademark); // draw model
        gSPEndDisplayList(dlIter);

        // Once the "Super Mario 64" logo has just about zoomed fully, fade in the "TM" and copyright text
        if (sIntroFrameCounter >= 19) {
            sTmCopyrightAlpha += 26;
            if (sTmCopyrightAlpha > 255) {
                sTmCopyrightAlpha = 255;
            }
        }
    }
    return dl;
}

// intro screen background display lists for each of four 80x20 textures
static const Gfx *introBackgroundDlRows[] = {
    title_screen_bg_dl_0A000130,
    title_screen_bg_dl_0A000148,
    title_screen_bg_dl_0A000160,
    title_screen_bg_dl_0A000178
};

// intro screen background texture X offsets
static float xCoords[] = {
    0, 80, 160, 240,
    0, 80, 160, 240,
    0, 80, 160, 240,
};

// intro screen background texture Y offsets
static float yCoords[] = {
    160, 160, 160, 160,
     80,  80,  80,  80,
      0,   0,   0,   0,
};

// table that points to either the "Super Mario 64" or "Game Over" tables
static const Texture *const *textureTables[] = { mario_title_texture_table, game_over_texture_table };

/**
 * Generates a display list for a single background tile
 *
 * @param index            which tile to render (value from 0 to 11)
 * @param backgroundTable  array describing which image to use for each tile (0 denotes a "Super Mario 64" image, and 1 denotes a "Game Over" image)
 */
static Gfx *intro_backdrop_one_image(s32 index, s8 *backgroundTable) {
    Mtx *mtx = alloc_display_list(sizeof(*mtx));
    Gfx *displayList = alloc_display_list(36 * sizeof(*displayList));
    Gfx *displayListIter = displayList;
    const Texture *const *vIntroBgTable = segmented_to_virtual(textureTables[backgroundTable[index]]);
    s32 i;

    guTranslate(mtx, xCoords[index], yCoords[index], 0.0f);
    gSPMatrix(displayListIter++, mtx, G_MTX_MODELVIEW | G_MTX_LOAD | G_MTX_PUSH);
    gSPDisplayList(displayListIter++, &title_screen_bg_dl_0A000118);
    for (i = 0; i < 4; ++i) {
        gDPLoadTextureBlock(displayListIter++, vIntroBgTable[i], G_IM_FMT_RGBA, G_IM_SIZ_16b, 80, 20, 0,
                            G_TX_CLAMP, G_TX_CLAMP, 7, 6, G_TX_NOLOD, G_TX_NOLOD)
        gSPDisplayList(displayListIter++, introBackgroundDlRows[i]);
    }
    gSPPopMatrix(displayListIter++, G_MTX_MODELVIEW);
    gSPEndDisplayList(displayListIter);
    return displayList;
}

static s8 introBackgroundIndexTable[] = {
    INTRO_BACKGROUND_SUPER_MARIO, INTRO_BACKGROUND_SUPER_MARIO, INTRO_BACKGROUND_SUPER_MARIO,
    INTRO_BACKGROUND_SUPER_MARIO, INTRO_BACKGROUND_SUPER_MARIO, INTRO_BACKGROUND_SUPER_MARIO,
    INTRO_BACKGROUND_SUPER_MARIO, INTRO_BACKGROUND_SUPER_MARIO, INTRO_BACKGROUND_SUPER_MARIO,
    INTRO_BACKGROUND_SUPER_MARIO, INTRO_BACKGROUND_SUPER_MARIO, INTRO_BACKGROUND_SUPER_MARIO,
};

// only one table of indexes listed
static s8 *introBackgroundTables[] = { introBackgroundIndexTable };

/**
 * Geo callback to render the intro background tiles
 */
Gfx *geo_intro_regular_backdrop(s32 callContext, struct GraphNode *node, UNUSED void *context) {
    struct GraphNodeMore *graphNode = (struct GraphNodeMore *) node;
    s32 index = graphNode->bgTableID & 0xff; // TODO: word at offset 0x18 of struct GraphNode (always ends up being 0)
    s8 *backgroundTable = introBackgroundTables[index];
    Gfx *dl = NULL;
    Gfx *dlIter = NULL;
    s32 i;

    if (callContext == GEO_CONTEXT_RENDER) {  // draw
        dl = alloc_display_list(16 * sizeof(*dl));
        dlIter = dl;
        SET_GRAPH_NODE_LAYER(graphNode->node.flags, LAYER_OPAQUE);
        gSPDisplayList(dlIter++, &dl_proj_mtx_fullscreen);
        gSPDisplayList(dlIter++, &title_screen_bg_dl_start);
        for (i = 0; i < 12; ++i) {
            gSPDisplayList(dlIter++, intro_backdrop_one_image(i, backgroundTable));
        }
        gSPDisplayList(dlIter++, &title_screen_bg_dl_end);
        gSPEndDisplayList(dlIter);
    }
    return dl;
}

static s8 gameOverBackgroundTable[] = {
    INTRO_BACKGROUND_GAME_OVER, INTRO_BACKGROUND_GAME_OVER, INTRO_BACKGROUND_GAME_OVER,
    INTRO_BACKGROUND_GAME_OVER, INTRO_BACKGROUND_GAME_OVER, INTRO_BACKGROUND_GAME_OVER,
    INTRO_BACKGROUND_GAME_OVER, INTRO_BACKGROUND_GAME_OVER, INTRO_BACKGROUND_GAME_OVER,
    INTRO_BACKGROUND_GAME_OVER, INTRO_BACKGROUND_GAME_OVER, INTRO_BACKGROUND_GAME_OVER,
};

// order of tiles that are flipped from "Game Over" to "Super Mario 64"
static s8 flipOrder[] = {
    0, 1, 2, 3, 7, 11, 10, 9, 8, 4, 5, 6
};

/**
 * Geo callback to render the Game Over background tiles
 */
Gfx *geo_intro_gameover_backdrop(s32 callContext, struct GraphNode *node, UNUSED void *context) {
    struct GraphNode *graphNode = node;
    Gfx *dl = NULL;
    Gfx *dlIter = NULL;
    s32 i, j;

    if (callContext != GEO_CONTEXT_RENDER) {  // reset
        sGameOverFrameCounter = 0;
        sGameOverTableIndex = -2;
        for (i = 0; i < ARRAY_COUNT(gameOverBackgroundTable); ++i) {
            gameOverBackgroundTable[i] = INTRO_BACKGROUND_GAME_OVER;
        }
    } else { // draw
        dl = alloc_display_list(16 * sizeof(*dl));
        dlIter = dl;
        if (sGameOverTableIndex == -2) {
            if (sGameOverFrameCounter == 180) {
                sGameOverTableIndex++;
                sGameOverFrameCounter = 0;
            }
        } else {
            // transition tile from "Game Over" to "Super Mario 64"
            if (sGameOverTableIndex != 11 && !(sGameOverFrameCounter & 0x1)) {
                sGameOverTableIndex++;
                gameOverBackgroundTable[flipOrder[sGameOverTableIndex]] = INTRO_BACKGROUND_SUPER_MARIO;
            }
        }
        if (sGameOverTableIndex != 11) {
            sGameOverFrameCounter++;
        }
        SET_GRAPH_NODE_LAYER(graphNode->flags, LAYER_OPAQUE);

        // draw all the tiles
        gSPDisplayList(dlIter++, &dl_proj_mtx_fullscreen);
        gSPDisplayList(dlIter++, &title_screen_bg_dl_start);
        for (j = 0; j < ARRAY_COUNT(gameOverBackgroundTable); ++j) {
            gSPDisplayList(dlIter++, intro_backdrop_one_image(j, gameOverBackgroundTable));
        }
        gSPDisplayList(dlIter++, &title_screen_bg_dl_end);
        gSPEndDisplayList(dlIter);
    }
    return dl;
}

#ifdef GODDARD_EASTER_EGG
extern Gfx title_screen_bg_dl_face_easter_egg_begin[];
extern Gfx title_screen_bg_dl_face_easter_egg_end[];

// Data
s8 sFaceVisible[] = {
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 0, 0, 0, 0, 1, 1,
    1, 1, 0, 0, 0, 0, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
};

s8 sFaceToggleOrder[] = {
     0,  1,  2,  3,  4,  5,  6,  7,
    15, 23, 31, 39, 47, 46, 45, 44,
    43, 42, 41, 40, 32, 24, 16,  8,
     9, 10, 11, 12, 13, 14, 22, 30,
    38, 37, 36, 35, 34, 33, 25, 17,
};

s8 sFaceCounter = 0;

void intro_gen_face_texrect(Gfx **dlIter) {
    s32 x;
    s32 y;

    for (y = 0; y < 6; y++) {
        for (x = 0; x < 8; x++) {
            if (sFaceVisible[y*8 + x] != 0) {
                gSPTextureRectangle((*dlIter)++, (x * 40) << 2, (y * 40) << 2, (x * 40 + 39) << 2, (y * 40 + 39) << 2, 0,
                                    0, 0, 4 << 10, 1 << 10);
            }
        }
    }
}

Gfx *intro_draw_face(u16 *image, s32 imageW, s32 imageH) {
    Gfx *dlIter;

    Gfx *dl = alloc_display_list(130 * sizeof(Gfx));

    if (dl == NULL) {
        return dl;
    } else {
        dlIter = dl;
    }

    gSPDisplayList(dlIter++, title_screen_bg_dl_face_easter_egg_begin);

    gDPLoadTextureBlock(dlIter++, VIRTUAL_TO_PHYSICAL(image), G_IM_FMT_RGBA, G_IM_SIZ_16b, imageW, imageH, 0, G_TX_CLAMP | G_TX_NOMIRROR, G_TX_CLAMP | G_TX_NOMIRROR, 6, 6, G_TX_NOLOD, G_TX_NOLOD);

    intro_gen_face_texrect(&dlIter);

    gSPDisplayList(dlIter++, title_screen_bg_dl_face_easter_egg_end);

    gSPEndDisplayList(dlIter++);

    return dl;
}

RGBA16Return32 *intro_sample_frame_buffer(s32 imageW, s32 imageH, s32 sampleW, s32 sampleH, s32 xOffset, s32 yOffset) {
    s32 pixel;
    f32 size = (1.0f / (sampleW * sampleH));
    ColorRGBf color;
    s32 iy, ix, sy, sx;
    s32 idy, idx, sdy;
    RGBA16 *fb = gFramebuffers[sRenderingFramebuffer];
    RGBA16 *image = alloc_display_list((imageW * imageH) * sizeof(RGBA16));

    if (image == NULL) {
        return NULL;
    }

    for ((iy = 0); (iy < imageH); (iy++)) {
        idy = ((sampleH * iy) + yOffset);
        for ((ix = 0); (ix < imageW); (ix++)) {
            vec3_zero(color);
            idx = ((sampleW * ix) + xOffset);

            for ((sy = 0); (sy < sampleH); (sy++)) {
                sdy = ((SCREEN_WIDTH * (idy + sy)) + idx);
                for ((sx = 0); (sx < sampleW); (sx++)) {
                    // pixel = SCREEN_WIDTH * (sampleH * iy + sy + yOffset) + (sampleW * ix + xOffset) + sx;
                    pixel = fb[sdy + sx];
                    color[0] += RGBA16_R(pixel);
                    color[1] += RGBA16_G(pixel);
                    color[2] += RGBA16_B(pixel);
                }
            }

            image[imageH * iy + ix] = ((R_RGBA16((RGBA16)((color[0] * size) + 0.5f)) & 0xFFFF) |
                                       (G_RGBA16((RGBA16)((color[1] * size) + 0.5f)) & 0xFFFF) |
                                       (B_RGBA16((RGBA16)((color[2] * size) + 0.5f)) & 0xFFFF) | MSK_RGBA16_A);
        }
    }

    return (RGBA16Return32 *)image;
}

Gfx *geo_intro_face_easter_egg(s32 callContext, struct GraphNode *node, UNUSED void *context) {
    struct GraphNodeGenerated *genNode = (struct GraphNodeGenerated *)node;
    Gfx *dl = NULL;
    s32 i;

    if (callContext != GEO_CONTEXT_RENDER) {
        for (i = 0; i < 48; i++) {
            sFaceVisible[i] = 0;
        }

    } else if (callContext == GEO_CONTEXT_RENDER) {
        if (sFaceCounter == 0) {
            if (gPlayer1Controller->buttonPressed & Z_TRIG) {
                play_sound(SOUND_MENU_STAR_SOUND, gGlobalSoundSource);
                sFaceVisible[0] ^= 1;
                sFaceCounter++;
            }
        } else {
            sFaceVisible[sFaceToggleOrder[sFaceCounter++]] ^= 1;
            if (sFaceCounter >= 40) {
                sFaceCounter = 0;
            }
        }

        // Draw while the first or last face is visible.
        if (sFaceVisible[0] == 1 || sFaceVisible[17] == 1) {
            RGBA16 *image = (RGBA16 *)intro_sample_frame_buffer(40, 40, 2, 2, 120, 80);
            if (image != NULL) {
                SET_GRAPH_NODE_LAYER(genNode->fnNode.node.flags, LAYER_OPAQUE);
                dl = intro_draw_face(image, 40, 40);
            }
        }
    }

    return dl;
}
#endif

#ifdef ENABLE_RUMBLE
//! TODO: Move rumble pak graphic textures here once build order is fixed.

//! TODO: Use DEFINE_LANGUAGE_ARRAY after ASCII/multilang is merged.
Texture* title_texture_rumble_pak_language_array[] = {
    title_texture_rumble_pak_en,
 #if MULTILANG
  #ifdef ENABLE_FRENCH
    title_texture_rumble_pak_fr,
  #endif // ENABLE_FRENCH
  #ifdef ENABLE_GERMAN
    title_texture_rumble_pak_de,
  #endif // ENABLE_GERMAN
  #ifdef ENABLE_JAPANESE
    title_texture_rumble_pak_jp,
  #endif // ENABLE_JAPANESE
  #if defined(ENABLE_SPANISH_SPAIN) || defined(ENABLE_SPANISH_LATIN_AMERICA)
    title_texture_rumble_pak_es,
  #endif // (ENABLE_SPANISH_SPAIN || ENABLE_SPANISH_LATIN_AMERICA)
 #endif // MULTILANG
};

Gfx* geo_intro_rumble_pak_graphic(s32 callContext, struct GraphNode* node, UNUSED void*context) {
    struct GraphNodeGenerated* genNode = (struct GraphNodeGenerated*)node;
    Gfx* dlIter;
    Gfx* dl = NULL;
    s8 backgroundTileSix = 0;

    if (callContext != GEO_CONTEXT_RENDER) {
        dl = NULL;
    } else if (callContext == GEO_CONTEXT_RENDER) {
        SET_GRAPH_NODE_LAYER(genNode->fnNode.node.flags, LAYER_OPAQUE);
        s32 introContext = (genNode->parameter & 0xFF);
        if (introContext == INTRO_CONTEXT_NORMAL) {
            backgroundTileSix = introBackgroundIndexTable[6];
        } else if (introContext == INTRO_CONTEXT_GAME_OVER) {
            backgroundTileSix = gameOverBackgroundTable[6];
        }
        if (backgroundTileSix == INTRO_BACKGROUND_SUPER_MARIO) {
            dl = alloc_display_list(
                sizeof((Gfx[]){gsSPDisplayList(0)}) +
                SIZEOF_GFX_TEXRECT_RGBA16 +
                SIZEOF_GFX_TEXRECT_RGBA16 +
                sizeof((Gfx[]){gsSPDisplayList(0)}) +
                sizeof((Gfx[]){gsSPEndDisplayList()})
            );

            if (dl != NULL) {
                dlIter = dl;
                gSPDisplayList(dlIter++, &dl_texrect_rgba16_begin);
                texrect_rgba16(&dlIter, title_texture_rumble_pak_language_array[LANGUAGE_ENGLISH], RUMBLE_TEXT_W, RUMBLE_TEXT_H, RUMBLE_TEXT_X, RUMBLE_TEXT_Y, RUMBLE_TEXT_W, RUMBLE_TEXT_H);
                texrect_rgba16(&dlIter, title_texture_rumble_pak_controller,                       RUMBLE_CONT_W, RUMBLE_CONT_H, RUMBLE_CONT_X, RUMBLE_CONT_Y, RUMBLE_CONT_W, RUMBLE_CONT_H);
                gSPDisplayList(dlIter++, &dl_texrect_rgba16_end);
                gSPEndDisplayList(dlIter);
            }
        } else {
            dl = NULL;
        }
    }

    return dl;
}
#endif // ENABLE_RUMBLE
