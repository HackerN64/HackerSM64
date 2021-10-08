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
#include "obj_behaviors_2.h"
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

#define o gCurrentObject

void bhv_bobomb_anchor_mario_loop(void) {
    common_anchor_mario_behavior(50.0f, 50.0f, 64);
}

void obj_set_speed_to_zero(void) {
    o->oForwardVel = o->oVelY = 0.0f;
}

Gfx *geo_update_held_mario_pos(s32 run, UNUSED struct GraphNode *node, Mat4 mtx) {
    Mat4 sp20;
    struct Object *obj;

    if (run == TRUE) {
        obj = (struct Object *) gCurGraphNodeObject;
        if (obj->prevObj != NULL) {
            create_transformation_from_matrices(sp20, mtx, *gCurGraphNodeCamera->matrixPtr);
            obj_update_pos_from_parent_transformation(sp20, obj->prevObj);
            obj_set_gfx_pos_from_pos(obj->prevObj);
        }
    }
    return NULL;
}

s32 update_angle_from_move_flags(s32 *angle) {
    if (o->oMoveFlags & OBJ_MOVE_HIT_WALL) {
        *angle = o->oWallAngle;
        return 1;
    } else if (o->oMoveFlags & OBJ_MOVE_HIT_EDGE) {
        *angle = o->oMoveAngleYaw + 0x8000;
        return -1;
    }
    return 0;
}

s32 mario_is_far_below_object(f32 arg0) {
    if (arg0 < o->oPosY - gMarioObject->oPosY) {
        return TRUE;
    } else {
        return FALSE;
    }
}

void obj_set_secondary_camera_focus(void) {
    gSecondCameraFocus = o;
}

void common_anchor_mario_behavior(f32 sp28, f32 sp2C, s32 obj) {
    switch (o->parentObj->oCommonAnchorAction) {
        case 0:
            break;
        case 1:
            obj_set_gfx_pos_at_obj_pos(gMarioObject, o);
            break;
        case 2:
            gMarioObject->oInteractStatus |= (obj + INT_STATUS_MARIO_UNK2);
            gMarioStates[0].forwardVel = sp28;
            gMarioStates[0].vel[1] = sp2C;
            o->parentObj->oCommonAnchorAction = 0;
            break;
        case 3:
            gMarioObject->oInteractStatus |=
                (INT_STATUS_MARIO_UNK2 + INT_STATUS_MARIO_UNK6); // loads 2 interactions at once?
            gMarioStates[0].forwardVel = 10.0f;
            gMarioStates[0].vel[1] = 10.0f;
            o->parentObj->oCommonAnchorAction = 0;
            break;
    }
    o->oMoveAngleYaw = o->parentObj->oMoveAngleYaw;
    if (o->parentObj->activeFlags == ACTIVE_FLAG_DEACTIVATED)
        obj_mark_for_deletion(o);
}

void bhv_chuckya_anchor_mario_loop(void) {
    common_anchor_mario_behavior(40.0f, 40.0f, 64);
}

s32 approach_forward_vel(f32 *arr, f32 spC, f32 sp10) {
    s32 sp4 = 0;
    if (arr[0] > spC) {
        arr[0] -= sp10;
        if (arr[0] < spC)
            arr[0] = spC;
    } else if (arr[0] < spC) {
        arr[0] += sp10;
        if (arr[0] > spC)
            arr[0] = spC;
    } else
        sp4 = 1;
    return sp4;
}

/**
 * Wait 50 frames, then play the race starting sound, disable time stop, and
 * optionally begin the timer.
 */
s32 obj_begin_race(s32 noTimer) {
    if (o->oTimer == 50) {
        cur_obj_play_sound_2(SOUND_GENERAL_RACE_GUN_SHOT);

        if (!noTimer) {
            play_music(SEQ_PLAYER_LEVEL, SEQUENCE_ARGS(4, SEQ_LEVEL_SLIDE), 0);

            level_control_timer(TIMER_CONTROL_SHOW);
            level_control_timer(TIMER_CONTROL_START);

            o->parentObj->oKoopaRaceEndpointRaceBegun = TRUE;
        }

        // Unfreeze mario and disable time stop to begin the race
        set_mario_npc_dialog(MARIO_DIALOG_STOP);
        disable_time_stop_including_mario();
    } else if (o->oTimer > 50) {
        return TRUE;
    }

    return FALSE;
}

s32 sCapSaveFlags[] = { SAVE_FLAG_HAVE_WING_CAP, SAVE_FLAG_HAVE_METAL_CAP, SAVE_FLAG_HAVE_VANISH_CAP };

// Boo Roll
s16 sBooHitRotations[] = { 6047, 5664, 5292, 4934, 4587, 4254, 3933, 3624, 3329, 3046, 2775,
                     2517, 2271, 2039, 1818, 1611, 1416, 1233, 1063, 906,  761,  629,
                     509,  402,  308,  226,  157,  100,  56,   25,   4,    0 };

// not in behavior file
struct SpawnParticlesInfo sMistParticles = { 2, 20, MODEL_MIST, 0, 40, 5, 30, 20, 252, 30, 330.0f, 10.0f };

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

void bhv_bobomb_bully_death_smoke_init(void) {
    o->oPosY -= 300.0f;

    cur_obj_scale(10.0f);
}

void bhv_respawner_loop(void) {
    struct Object *spawnedObject;

    if (!is_point_within_radius_of_mario(o->oPosX, o->oPosY, o->oPosZ, o->oRespawnerMinSpawnDist)) {
        spawnedObject = spawn_object(o, o->oRespawnerModelToRespawn, o->oRespawnerBehaviorToRespawn);
        spawnedObject->oBehParams = o->oBehParams;
        o->activeFlags = ACTIVE_FLAG_DEACTIVATED;
    }
}

void create_respawner(s32 model, const BehaviorScript *behToSpawn, s32 minSpawnDist) {
    struct Object *respawner = spawn_object_abs_with_rot(o, 0, MODEL_NONE, bhvRespawner, o->oHomeX,
                                                         o->oHomeY, o->oHomeZ, 0, 0, 0);
    respawner->oBehParams = o->oBehParams;
    respawner->oRespawnerModelToRespawn = model;
    respawner->oRespawnerMinSpawnDist = minSpawnDist;
    respawner->oRespawnerBehaviorToRespawn = behToSpawn;
}

// not sure what this is doing here. not in a behavior file.
Gfx *geo_move_mario_part_from_parent(s32 callContext, UNUSED struct GraphNode *node, Mat4 mtx) {
    Mat4 mtx2;

    if (callContext == GEO_CONTEXT_RENDER) {
        struct Object *obj = (struct Object *) gCurGraphNodeObject;
        if (obj == gMarioObject && obj->prevObj != NULL) {
            create_transformation_from_matrices(mtx2, mtx, *gCurGraphNodeCamera->matrixPtr);
            obj_update_pos_from_parent_transformation(mtx2, obj->prevObj);
            obj_set_gfx_pos_from_pos(obj->prevObj);
        }
    }
    return NULL;
}

static s16 sSpawnSparkleParticleAngle = 0x0;

// not in behavior file
// n is the number of objects to spawn, r if the rate of change of phase (frequency?)
void spawn_sparkle_particles(s32 n, s32 radius, s32 height, s32 r) {
    s32 i;
    s16 separation = 0x10000 / n; // Evenly spread around a circle
    for (i = 0; i < n; i++) {
        spawn_object_relative(0, sins(sSpawnSparkleParticleAngle + i * separation) * radius, (i + 1) * height,
                              coss(sSpawnSparkleParticleAngle + i * separation) * radius, o, MODEL_NONE, bhvSparkleSpawn);
    }

    sSpawnSparkleParticleAngle += r * 0x100;
}

// Not in behavior file, duplicate of vec3f_copy except without bad return.
// Used in a few behavior files.
void vec3f_copy_2(Vec3f dest, Vec3f src) {
    dest[0] = src[0];
    dest[1] = src[1];
    dest[2] = src[2];
}

s32 set_obj_anim_with_accel_and_sound(s16 a0, s16 a1, s32 a2) {
    f32 range;
    if ((range = o->header.gfx.animInfo.animAccel / (f32) 0x10000) == 0)
        range = 1.0f;
    if (cur_obj_check_anim_frame_in_range(a0, range) || cur_obj_check_anim_frame_in_range(a1, range)) {
        cur_obj_play_sound_2(a2);
		return TRUE;
    }
    return FALSE;
}

void spawn_default_star(f32 sp20, f32 sp24, f32 sp28) {
    struct Object *obj = NULL;
    obj = spawn_star(obj, sp20, sp24, sp28);
    obj->oBehParams2ndByte = 0;
}

void spawn_red_coin_cutscene_star(f32 sp20, f32 sp24, f32 sp28) {
    struct Object *obj = NULL;
    obj = spawn_star(obj, sp20, sp24, sp28);
    obj->oBehParams2ndByte = 1;
}

void spawn_no_exit_star(f32 sp20, f32 sp24, f32 sp28) {
    struct Object *obj = NULL;
    obj = spawn_star(obj, sp20, sp24, sp28);
    obj->oBehParams2ndByte = 1;
    obj->oInteractionSubtype |= INT_SUBTYPE_NO_EXIT;
}

struct Object *spawn_star(struct Object *obj, f32 sp34, f32 sp38, f32 sp3C) {
    obj = spawn_object_abs_with_rot(o, 0, MODEL_STAR, bhvStarSpawnCoordinates, o->oPosX, o->oPosY, o->oPosZ, 0, 0, 0);
    obj->oBehParams = o->oBehParams;
    obj->oHomeX = sp34;
    obj->oHomeY = sp38;
    obj->oHomeZ = sp3C;
    obj->oFaceAnglePitch = 0;
    obj->oFaceAngleRoll = 0;
    return obj;
}

void bhv_spawn_star_no_level_exit(u32 sp20) {
    struct Object *obj = spawn_object(o, MODEL_STAR, bhvSpawnedStarNoLevelExit);
    obj->oBehParams = sp20 << 24;
    obj->oInteractionSubtype = INT_SUBTYPE_NO_EXIT;
    obj_set_angle(obj, 0, 0, 0);
}

//These are very commonly used and are better placed here.
#include "game/behaviors/break_particles.inc.c"
#include "game/behaviors/wind.inc.c"
#include "game/behaviors/ground_particles.inc.c" //Probably common1
