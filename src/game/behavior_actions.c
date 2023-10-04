#include <PR/ultratypes.h>

#include "types.h"
#include "actors/common1.h"
#include "actors/group12.h"
#include "actors/group13.h"
#include "area.h"
#include "audio/external.h"
#include "behavior_actions.h"
#include "behavior_data.h"
#include "camera.h"
#include "debug.h"
#include "dialog_ids.h"
#include "engine/behavior_script.h"
#include "engine/graph_node.h"
#include "engine/math_util.h"
#include "engine/surface_collision.h"
#include "engine/surface_load.h"
#include "game_init.h"
#include "ingame_menu.h"
#include "interaction.h"
#include "level_misc_macros.h"
#include "level_table.h"
#include "level_update.h"
#include "levels/bob/header.h"
#include "levels/bowser_3/header.h"
#include "levels/castle_inside/header.h"
#include "levels/hmc/header.h"
#include "main.h"
#include "mario.h"
#include "mario_actions_cutscene.h"
#include "mario_step.h"
#include "obj_behaviors.h"
#include "object_constants.h"
#include "object_helpers.h"
#include "object_list_processor.h"
#include "paintings.h"
#include "platform_displacement.h"
#include "rendering_graph_node.h"
#include "save_file.h"
#include "seq_ids.h"
#include "sm64.h"
#include "spawn_object.h"
#include "spawn_sound.h"
#include "rumble_init.h"
#include "puppylights.h"

// not in behavior file
static struct SpawnParticlesInfo sMistParticles = {
    /* behParam:        */ 2,
    /* count:           */ 20,
    /* model:           */ MODEL_MIST,
    /* offsetY:         */ 0,
    /* forwardVelBase:  */ 40,
    /* forwardVelRange: */ 5,
    /* velYBase:        */ 30,
    /* velYRange:       */ 20,
    /* gravity:         */ 252,
    /* dragStrength:    */ 30,
    /* sizeBase:        */ 330.0f,
    /* sizeRange:       */ 10.0f,
};

// generate_wind_puffs/dust (something like that)
void spawn_mist_particles_variable(s32 count, s32 offsetY, f32 size) {
    sMistParticles.sizeBase = size;
    sMistParticles.sizeRange = size / 20.0f;
    sMistParticles.offsetY = offsetY;

    if (count == 0) {
        sMistParticles.count = 20;
    } else if (count > 20) {
        sMistParticles.count = count;
    } else {
        sMistParticles.count = 4;
    }

    cur_obj_spawn_particles(&sMistParticles);
}

// not sure what this is doing here. not in a behavior file.
Gfx *geo_move_mario_part_from_parent(s32 callContext, UNUSED struct GraphNode *node, Mat4 mtx) {

    if (callContext == GEO_CONTEXT_RENDER) {
        struct Object *obj = (struct Object *) gCurGraphNodeObject;
        if (obj == gMarioObject && obj->prevObj != NULL) {
            obj_update_pos_from_parent_transformation(mtx, obj->prevObj);
            obj_set_gfx_pos_from_pos(obj->prevObj);
        }
    }

    return NULL;
}

// not in behavior file
// n is the number of objects to spawn, r if the rate of change of phase (frequency?)
void spawn_sparkle_particles(s32 n, s32 radius, s32 height, s32 r) {
    static s16 spawnSparkleParticleAngle = 0x0;
    s32 i;
    s16 separation = 0x10000 / n; // Evenly spread around a circle

    for (i = 0; i < n; i++) {
        spawn_object_relative(OBJ_BP_NONE, sins(spawnSparkleParticleAngle + i * separation) * radius, (i + 1) * height,
                              coss(spawnSparkleParticleAngle + i * separation) * radius, o, MODEL_NONE, bhvSparkleSpawn);
    }

    spawnSparkleParticleAngle += r * 0x100;
}

void set_obj_anim_with_accel_and_sound(s16 frame1, s16 frame2, s32 sound) {
    s32 range = o->header.gfx.animInfo.animAccel / 0x10000;
    if (range == 0) {
        range = 1;
    }

    if (cur_obj_check_anim_frame_in_range(frame1, range) || cur_obj_check_anim_frame_in_range(frame2, range)) {
        cur_obj_play_sound_2(sound);
    }
}
