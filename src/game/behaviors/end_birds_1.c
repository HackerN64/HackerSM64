#include <ultra64.h>
#include "global_object_fields.h"
#include "engine/math_util.h"
#include "game/camera.h"
#include "game/object_helpers.h"
#include "game/spawn_sound.h"

/* Intro Cutscene Lakitu - End Birds */
#define /*0x108*/ O_INTRO_LAKITU_END_BIRDS_DEST_INDEX   0x20
#define /*0x108*/ O_INTRO_LAKITU_END_BIRDS_DEST_X_INDEX (O_INTRO_LAKITU_END_BIRDS_DEST_INDEX + 0) // 0x20
#define /*0x10C*/ O_INTRO_LAKITU_END_BIRDS_DEST_Y_INDEX (O_INTRO_LAKITU_END_BIRDS_DEST_INDEX + 1) // 0x21
#define /*0x110*/ O_INTRO_LAKITU_END_BIRDS_DEST_Z_INDEX (O_INTRO_LAKITU_END_BIRDS_DEST_INDEX + 2) // 0x22
#define /*0x108*/ oIntroLakituEndBirds1DestVec      OBJECT_FIELD_F32(O_INTRO_LAKITU_END_BIRDS_DEST_INDEX)
#define /*0x110*/ oIntroLakituEndBirds1DestX        OBJECT_FIELD_F32(O_INTRO_LAKITU_END_BIRDS_DEST_X_INDEX)
#define /*0x10C*/ oIntroLakituEndBirds1DestY        OBJECT_FIELD_F32(O_INTRO_LAKITU_END_BIRDS_DEST_Y_INDEX)
#define /*0x108*/ oIntroLakituEndBirds1DestZ        OBJECT_FIELD_F32(O_INTRO_LAKITU_END_BIRDS_DEST_Z_INDEX)

void bhv_end_birds_1_loop(void) {
    Vec3f pos;

    switch (o->oAction) {
        case END_BIRDS_ACT_INIT:
            cur_obj_scale(0.7f);
            vec3f_set(&o->oIntroLakituEndBirds1DestVec, -554.0f, 3044.0f, -1314.0f);
            o->oAction = END_BIRDS_ACT_ACTIVE;
            break;
        case END_BIRDS_ACT_ACTIVE:
            vec3f_copy(pos, &o->oIntroLakituEndBirds1DestVec);
            if (o->oTimer < 100) {
                obj_rotate_towards_point(o, pos, 0, 0, 0x20, 0x20);
            }
            if ((o->oEndBirdCutsceneVars9PointX == 0.0f) && (o->oTimer == 0)) {
                cur_obj_play_sound_2(SOUND_GENERAL_BIRDS_FLY_AWAY);
            }
            if (gCutsceneTimer == 0) {
                obj_mark_for_deletion(o);
            }
            break;
    }

    cur_obj_set_pos_via_transform();
}
