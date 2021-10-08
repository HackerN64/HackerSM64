#ifndef OBJECT_CONSTANTS_H
#define OBJECT_CONSTANTS_H

// This file contains macros that provide descriptive names for
// field-specific and object-specific constants, e.g. actions.

/* activeFlags */
#define ACTIVE_FLAG_DEACTIVATED                     (0 <<  0) // 0x0000
#define ACTIVE_FLAG_ACTIVE                          (1 <<  0) // 0x0001
#define ACTIVE_FLAG_FAR_AWAY                        (1 <<  1) // 0x0002
#define ACTIVE_FLAG_UNUSED                          (1 <<  2) // 0x0004
#define ACTIVE_FLAG_IN_DIFFERENT_ROOM               (1 <<  3) // 0x0008
#define ACTIVE_FLAG_UNIMPORTANT                     (1 <<  4) // 0x0010
#define ACTIVE_FLAG_INITIATED_TIME_STOP             (1 <<  5) // 0x0020
#define ACTIVE_FLAG_MOVE_THROUGH_GRATE              (1 <<  6) // 0x0040
#define ACTIVE_FLAG_DITHERED_ALPHA                  (1 <<  7) // 0x0080
#define ACTIVE_FLAG_ALLOCATED                       (1 <<  8) // 0x0100
#define ACTIVE_FLAG_DESTRUCTIVE_OBJ_DONT_DESTROY    (1 <<  9) // 0x0200
#define ACTIVE_FLAG_IGNORE_ENV_BOXES                (1 << 10) // 0x0400

/* respawnInfoType */
#define RESPAWN_INFO_TYPE_NULL                      0x00
#define RESPAWN_INFO_TYPE_32                        0x01
#define RESPAWN_INFO_TYPE_16                        0x02

/* respawnInfo */
#define RESPAWN_INFO_DONT_RESPAWN                   0xFF

/* oFlags */
#define OBJ_FLAG_UPDATE_GFX_POS_AND_ANGLE           (1 <<  0) // 0x00000001
#define OBJ_FLAG_MOVE_XZ_USING_FVEL                 (1 <<  1) // 0x00000002
#define OBJ_FLAG_MOVE_Y_WITH_TERMINAL_VEL           (1 <<  2) // 0x00000004
#define OBJ_FLAG_SET_FACE_YAW_TO_MOVE_YAW           (1 <<  3) // 0x00000008
#define OBJ_FLAG_SET_FACE_ANGLE_TO_MOVE_ANGLE       (1 <<  4) // 0x00000010
#define OBJ_FLAG_UPDATE_TRANSFORM_FOR_THROW_MATRIX  (1 <<  5) // 0x00000020
#define OBJ_FLAG_COMPUTE_DIST_TO_MARIO              (1 <<  6) // 0x00000040
#define OBJ_FLAG_ACTIVE_FROM_AFAR                   (1 <<  7) // 0x00000080
#define OBJ_FLAG_PLAYER                             (1 <<  8) // 0x00000100
#define OBJ_FLAG_TRANSFORM_RELATIVE_TO_PARENT       (1 <<  9) // 0x00000200
#define OBJ_FLAG_HOLDABLE                           (1 << 10) // 0x00000400
#define OBJ_FLAG_SET_THROW_MATRIX_FROM_TRANSFORM    (1 << 11) // 0x00000800
#define OBJ_FLAG_1000                               (1 << 12) // 0x00001000
#define OBJ_FLAG_COMPUTE_ANGLE_TO_MARIO             (1 << 13) // 0x00002000
#define OBJ_FLAG_PERSISTENT_RESPAWN                 (1 << 14) // 0x00004000
#define OBJ_FLAG_VELOCITY_PLATFORM                  (1 << 15) // 0x00008000
#define OBJ_FLAG_DONT_CALC_COLL_DIST                (1 << 16) // 0x00010000
#define OBJ_FLAG_EMIT_LIGHT                         (1 << 17) // 0x00020000
#define OBJ_FLAG_UCODE_LARGE                        (1 << 18) // 0x00040000
#define OBJ_FLAG_SILHOUETTE                         (1 << 19) // 0x00080000
#define OBJ_FLAG_OCCLUDE_SILHOUETTE                 (1 << 20) // 0x00100000
#define OBJ_FLAG_OPACITY_FROM_CAMERA_DIST           (1 << 21) // 0x00200000
#define OBJ_FLAG_UCODE_SMALL                        (1 << 22) // 0x00400000
#define OBJ_FLAG_HITBOX_WAS_SET                     (1 << 30) // 0x40000000

/* oHeldState */
#define HELD_FREE                                   0x00
#define HELD_HELD                                   0x01
#define HELD_THROWN                                 0x02
#define HELD_DROPPED                                0x03

/* oDialogState */
#define DIALOG_STATUS_ENABLE_TIME_STOP              0x00
#define DIALOG_STATUS_INTERRUPT                     0x01
#define DIALOG_STATUS_START_DIALOG                  0x02
#define DIALOG_STATUS_STOP_DIALOG                   0x03
#define DIALOG_STATUS_DISABLE_TIME_STOP             0x04

#define DIALOG_FLAG_NONE                            (0 << 0) // 0x00
#define DIALOG_FLAG_TURN_TO_MARIO                   (1 << 0) // 0x01 // cutscene only
#define DIALOG_FLAG_TEXT_DEFAULT                    (1 << 1) // 0x02
#define DIALOG_FLAG_TEXT_RESPONSE                   (1 << 2) // 0x04 // non-cutscene only
#define DIALOG_FLAG_UNK_CAPSWITCH                   (1 << 3) // 0x08 // not defined
#define DIALOG_FLAG_TIME_STOP_ENABLED               (1 << 4) // 0x10

/* oMoveFlags */
#define OBJ_MOVE_LANDED                             (1 <<  0) // 0x0001
#define OBJ_MOVE_ON_GROUND                          (1 <<  1) // 0x0002  // mutually exclusive to OBJ_MOVE_LANDED
#define OBJ_MOVE_LEFT_GROUND                        (1 <<  2) // 0x0004
#define OBJ_MOVE_ENTERED_WATER                      (1 <<  3) // 0x0008
#define OBJ_MOVE_AT_WATER_SURFACE                   (1 <<  4) // 0x0010
#define OBJ_MOVE_UNDERWATER_OFF_GROUND              (1 <<  5) // 0x0020
#define OBJ_MOVE_UNDERWATER_ON_GROUND               (1 <<  6) // 0x0040
#define OBJ_MOVE_IN_AIR                             (1 <<  7) // 0x0080
#define OBJ_MOVE_OUT_SCOPE                          (1 <<  8) // 0x0100
#define OBJ_MOVE_HIT_WALL                           (1 <<  9) // 0x0200
#define OBJ_MOVE_HIT_EDGE                           (1 << 10) // 0x0400
#define OBJ_MOVE_ABOVE_LAVA                         (1 << 11) // 0x0800
#define OBJ_MOVE_LEAVING_WATER                      (1 << 12) // 0x1000
#define OBJ_MOVE_BOUNCE                             (1 << 13) // 0x2000
#ifndef VERSION_JP
#define OBJ_MOVE_ABOVE_DEATH_BARRIER                (1 << 14) // 0x4000
#endif

#define OBJ_MOVE_MASK_ON_GROUND (OBJ_MOVE_LANDED | OBJ_MOVE_ON_GROUND)
#define OBJ_MOVE_MASK_IN_WATER (\
    OBJ_MOVE_ENTERED_WATER |\
    OBJ_MOVE_AT_WATER_SURFACE |\
    OBJ_MOVE_UNDERWATER_OFF_GROUND |\
    OBJ_MOVE_UNDERWATER_ON_GROUND)

/* oActiveParticleFlags */
#define ACTIVE_PARTICLE_NONE                        (0 <<  0) // 0x00000000
#define ACTIVE_PARTICLE_DUST                        (1 <<  0) // 0x00000001
#define ACTIVE_PARTICLE_UNUSED_1                    (1 <<  1) // 0x00000002
#define ACTIVE_PARTICLE_UNUSED_2                    (1 <<  2) // 0x00000004
#define ACTIVE_PARTICLE_SPARKLES                    (1 <<  3) // 0x00000008
#define ACTIVE_PARTICLE_H_STAR                      (1 <<  4) // 0x00000010
#define ACTIVE_PARTICLE_BUBBLE                      (1 <<  5) // 0x00000020
#define ACTIVE_PARTICLE_WATER_SPLASH                (1 <<  6) // 0x00000040
#define ACTIVE_PARTICLE_IDLE_WATER_WAVE             (1 <<  7) // 0x00000080
#define ACTIVE_PARTICLE_SHALLOW_WATER_WAVE          (1 <<  8) // 0x00000100
#define ACTIVE_PARTICLE_PLUNGE_BUBBLE               (1 <<  9) // 0x00000200
#define ACTIVE_PARTICLE_WAVE_TRAIL                  (1 << 10) // 0x00000400
#define ACTIVE_PARTICLE_FIRE                        (1 << 11) // 0x00000800
#define ACTIVE_PARTICLE_SHALLOW_WATER_SPLASH        (1 << 12) // 0x00001000
#define ACTIVE_PARTICLE_LEAF                        (1 << 13) // 0x00002000
#define ACTIVE_PARTICLE_DIRT                        (1 << 14) // 0x00004000
#define ACTIVE_PARTICLE_MIST_CIRCLE                 (1 << 15) // 0x00008000
#define ACTIVE_PARTICLE_SNOW                        (1 << 16) // 0x00010000
#define ACTIVE_PARTICLE_BREATH                      (1 << 17) // 0x00020000
#define ACTIVE_PARTICLE_V_STAR                      (1 << 18) // 0x00040000
#define ACTIVE_PARTICLE_TRIANGLE                    (1 << 19) // 0x00080000

/* oBehParams2ndByte */
#define OBJ_BP_NONE                                         0x0

/* oAction */
#define OBJ_ACT_PROJECTILE_HIT_MARIO                       -0x1

/* oAction */
#define OBJ_ACT_LAVA_DEATH                                  100
#define OBJ_ACT_DEATH_PLANE_DEATH                           101

#define OBJ_ACT_HORIZONTAL_KNOCKBACK                        100
#define OBJ_ACT_VERTICAL_KNOCKBACK                          101
#define OBJ_ACT_SQUISHED                                    102

/* oAnimState */
#define OBJ_ANIM_STATE_INIT_ANIM                           -0x1
#define OBJ_ANIM_STATE_DEFAULT                              0x0
    /* cur_obj_update_blinking */
#define OBJ_BLINKING_ANIM_STATE_EYES_OPEN                   0x0
#define OBJ_BLINKING_ANIM_STATE_EYES_CLOSED                 0x1

/* Animations */
#define OBJ_ANIM_NONE                                      -0x1

/* gTTCSpeedSetting */
#define TTC_SPEED_SLOW                                      0x0
#define TTC_SPEED_FAST                                      0x1
#define TTC_SPEED_RANDOM                                    0x2
#define TTC_SPEED_STOPPED                                   0x3

/* geo_update_layer_transparency */
#define TRANSPARENCY_ANIM_STATE_OPAQUE                      0x0
#define TRANSPARENCY_ANIM_STATE_TRANSPARENT                 0x1

/* Orange Number */
    /* oBehParams2ndByte, oAnimState */
    #define ORANGE_NUMBER_0                                 0x0
    #define ORANGE_NUMBER_1                                 0x1
    #define ORANGE_NUMBER_2                                 0x2
    #define ORANGE_NUMBER_3                                 0x3
    #define ORANGE_NUMBER_4                                 0x4
    #define ORANGE_NUMBER_5                                 0x5
    #define ORANGE_NUMBER_6                                 0x6
    #define ORANGE_NUMBER_7                                 0x7
    #define ORANGE_NUMBER_8                                 0x8
    #define ORANGE_NUMBER_9                                 0x9
    #define ORANGE_NUMBER_A                                 0xA
    #define ORANGE_NUMBER_B                                 0xB
    #define ORANGE_NUMBER_C                                 0xC
    #define ORANGE_NUMBER_D                                 0xD
    #define ORANGE_NUMBER_E                                 0xE
    #define ORANGE_NUMBER_F                                 0xF

/* Coin Type */
    /* coinType */
    #define COIN_TYPE_NONE                                  0x0
    #define COIN_TYPE_YELLOW                                0x1
    #define COIN_TYPE_BLUE                                  0x2

/* Hidden Blue Coin */
    /* oAction */
    #define HIDDEN_BLUE_COIN_ACT_INACTIVE                   0x0
    #define HIDDEN_BLUE_COIN_ACT_WAITING                    0x1
    #define HIDDEN_BLUE_COIN_ACT_ACTIVE                     0x2

/* Blue Coin Switch */
    /* oAction */
    #define BLUE_COIN_SWITCH_ACT_IDLE                       0x0
    #define BLUE_COIN_SWITCH_ACT_RECEDING                   0x1
    #define BLUE_COIN_SWITCH_ACT_TICKING                    0x2
    #define BLUE_COIN_SWITCH_ACT_EXTENDING                  0x3

/* Moving Blue Coin */
    /* oAction */
    #define MOV_BCOIN_ACT_STILL                             0x0
    #define MOV_BCOIN_ACT_MOVING                            0x1
    #define MOV_BCOIN_ACT_SLOWING_DOWN                      0x2
    #define MOV_BCOIN_ACT_STOPPED                           0x3
    #define MOV_BCOIN_ACT_FLICKERING                        0x4

/* Yellow Coin */
    /* oBehParams2ndByte */
    #define YELLOW_COIN_BP_ONE_COIN                         0x1

/* Moving Yellow Coin */
    /* oAction */
    #define MOV_YCOIN_ACT_IDLE                              0x0
    #define MOV_YCOIN_ACT_BLINKING                          0x1
    /* oSubAction */

/* Coin Formation */
    /* (oBehParams2ndByte & 0x7) */
    #define COIN_FORMATION_BP_HORIZONTAL_LINE               0x0
    #define COIN_FORMATION_BP_VERTICAL_LINE                 0x1
    #define COIN_FORMATION_BP_HORIZONTAL_RING               0x2
    #define COIN_FORMATION_BP_VERTICAL_RING                 0x3
    #define COIN_FORMATION_BP_ARROW                         0x4
    /* oAction */
    #define COIN_FORMATION_ACT_INACTIVE                     0x0
    #define COIN_FORMATION_ACT_ACTIVE                       0x1
    #define COIN_FORMATION_ACT_DEACTIVATE                   0x2

/* Coin Inside Boo */
    /* oAction */
    #define COIN_INSIDE_BOO_ACT_CARRIED                     0x0
    #define COIN_INSIDE_BOO_ACT_DROPPED                     0x1

/* 1-Up Mushroom */
    /* oBehParams2ndByte */
    #define MUSHROOM_BP_REQUIRES_NONE                       0x0
    #define MUSHROOM_BP_REQUIRES_BOWSER_1                   0x1
    #define MUSHROOM_BP_REQUIRES_BOWSER_2                   0x2
    /* oAction */
    #define MUSHROOM_ACT_INIT                               0x0
    #define MUSHROOM_ACT_MOVING                             0x1
    #define MUSHROOM_ACT_DISAPPEARING                       0x2
    #define MUSHROOM_ACT_LOOP_IN_AIR                        0x3

/* Bob-omb */
    /* oBehParams2ndByte */
    #define BOBOMB_BP_STYPE_GENERIC                         0x0
    #define BOBOMB_BP_STYPE_STATIONARY                      0x1
    /* oAction */
    #define BOBOMB_ACT_PATROL                               0x0
    #define BOBOMB_ACT_LAUNCHED                             0x1
    #define BOBOMB_ACT_CHASE_MARIO                          0x2
    #define BOBOMB_ACT_EXPLODE                              0x3
    /* oAnimState */
    #define BOBOMB_ANIM_STATE_EYES_OPEN                     OBJ_BLINKING_ANIM_STATE_EYES_OPEN
    #define BOBOMB_ANIM_STATE_EYES_CLOSED                   OBJ_BLINKING_ANIM_STATE_EYES_CLOSED
    /* Animations */
    #define BOBOMB_ANIM_WALKING                             0x0
    #define BOBOMB_ANIM_HELD                                0x1

/* Bob-omb Buddy */
    /* oBehParams2ndByte */
    #define BOBOMB_BUDDY_BP_STYPE_GENERIC                   0x0
    #define BOBOMB_BUDDY_BP_STYPE_BOB_GRASS_KBB             0x1
    #define BOBOMB_BUDDY_BP_STYPE_BOB_CANNON_KBB            0x2
    #define BOBOMB_BUDDY_BP_STYPE_BOB_GRASS                 0x3
    /* oAction */
    #define BOBOMB_BUDDY_ACT_IDLE                           0x0
    #define BOBOMB_BUDDY_ACT_TURN_TO_TALK                   0x2
    #define BOBOMB_BUDDY_ACT_TALK                           0x3
    /* oBobombBuddyRole */
    #define BOBOMB_BUDDY_ROLE_ADVICE                        0x0
    #define BOBOMB_BUDDY_ROLE_CANNON                        0x1
    /* oBobombBuddyCannonStatus */
    #define BOBOMB_BUDDY_CANNON_UNOPENED                    0x0
    #define BOBOMB_BUDDY_CANNON_OPENING                     0x1
    #define BOBOMB_BUDDY_CANNON_OPENED                      0x2
    #define BOBOMB_BUDDY_CANNON_STOP_TALKING                0x3
    /* oBobombBuddyHasTalkedToMario */
    #define BOBOMB_BUDDY_HAS_NOT_TALKED                     0x0
    #define BOBOMB_BUDDY_HAS_TALKED                         0x2

/* Bowser */
    /* oBehParams2ndByte */
    #define BOWSER_BP_BITDW                                 0x0
    #define BOWSER_BP_BITFS                                 0x1
    #define BOWSER_BP_BITS                                  0x2
    /* Tail oAction */
    #define BOWSER_ACT_TAIL_DEFAULT                         0x00
    #define BOWSER_ACT_TAIL_THROWN                          0x01
    #define BOWSER_ACT_TAIL_TOUCHED_MARIO                   0x02
    /* oAction */
    #define BOWSER_ACT_DEFAULT                              0x00
    #define BOWSER_ACT_THROWN                               0x01
    #define BOWSER_ACT_JUMP_ONTO_STAGE                      0x02
    #define BOWSER_ACT_DANCE                                0x03
    #define BOWSER_ACT_DEAD                                 0x04
    #define BOWSER_ACT_WAIT                                 0x05
    #define BOWSER_ACT_INTRO_WALK                           0x06
    #define BOWSER_ACT_CHARGE_MARIO                         0x07
    #define BOWSER_ACT_SPIT_FIRE_INTO_SKY                   0x08
    #define BOWSER_ACT_SPIT_FIRE_ONTO_FLOOR                 0x09
    #define BOWSER_ACT_HIT_EDGE                             0x0A
    #define BOWSER_ACT_TURN_FROM_EDGE                       0x0B
    #define BOWSER_ACT_HIT_MINE                             0x0C
    #define BOWSER_ACT_BIG_JUMP                             0x0D
    #define BOWSER_ACT_WALK_TO_MARIO                        0x0E
    #define BOWSER_ACT_BREATH_FIRE                          0x0F
    #define BOWSER_ACT_TELEPORT                             0x10
    #define BOWSER_ACT_QUICK_JUMP                           0x11
    #define BOWSER_ACT_UNUSED_SLOW_WALK                     0x12
    #define BOWSER_ACT_TILT_LAVA_PLATFORM                   0x13
    /* oAnimState */
    #define BOWSER_ANIM_STATE_OPAQUE                        TRANSPARENCY_ANIM_STATE_OPAQUE
    #define BOWSER_ANIM_STATE_TRANSPARENT                   TRANSPARENCY_ANIM_STATE_TRANSPARENT
    #define BOWSER_ANIM_STATE_INVISIBLE                     0x02
    /* Animations */
    #define BOWSER_ANIM_STAND_UP                            0x00
    #define BOWSER_ANIM_STAND_UP_UNUSED                     0x01 // slightly different
    #define BOWSER_ANIM_SHAKING                             0x02
    #define BOWSER_ANIM_GRABBED                             0x03
    #define BOWSER_ANIM_BROKEN                              0x04 // broken animation
    #define BOWSER_ANIM_FALL_DOWN                           0x05 // unused
    #define BOWSER_ANIM_BREATH                              0x06
    #define BOWSER_ANIM_JUMP                                0x07 // unused, short jump, replaced by start/stop
    #define BOWSER_ANIM_JUMP_STOP                           0x08
    #define BOWSER_ANIM_JUMP_START                          0x09
    #define BOWSER_ANIM_DANCE                               0x0A
    #define BOWSER_ANIM_BREATH_UP                           0x0B
    #define BOWSER_ANIM_IDLE                                0x0C
    #define BOWSER_ANIM_SLOW_GAIT                           0x0D
    #define BOWSER_ANIM_LOOK_DOWN_STOP_WALK                 0x0E
    #define BOWSER_ANIM_LOOK_UP_START_WALK                  0x0F
    #define BOWSER_ANIM_FLIP_DOWN                           0x10
    #define BOWSER_ANIM_LAY_DOWN                            0x11
    #define BOWSER_ANIM_RUN_START                           0x12
    #define BOWSER_ANIM_RUN                                 0x13
    #define BOWSER_ANIM_RUN_STOP                            0x14
    #define BOWSER_ANIM_RUN_SLIP                            0x15
    #define BOWSER_ANIM_BREATH_QUICK                        0x16
    #define BOWSER_ANIM_EDGE_MOVE                           0x17
    #define BOWSER_ANIM_EDGE_STOP                           0x18
    #define BOWSER_ANIM_FLIP                                0x19
    #define BOWSER_ANIM_STAND_UP_FROM_FLIP                  0x1A
    /* oBowserCamAct */
    #define BOWSER_CAM_ACT_IDLE                             0x0
    #define BOWSER_CAM_ACT_WALK                             0x1
    #define BOWSER_CAM_ACT_END                              0x2
    /* oBowserStatus */
    #define BOWSER_STATUS_ANGLE_MARIO                       (1 <<  1) // 0x00000002
    #define BOWSER_STATUS_ANGLE_CENTRE                      (1 <<  2) // 0x00000004
    #define BOWSER_STATUS_DIST_MARIO                        (1 <<  3) // 0x00000008
    #define BOWSER_STATUS_DIST_CENTRE                       (1 <<  4) // 0x00000010
    #define BOWSER_STATUS_BIG_JUMP                          (1 << 16) // 0x00010000
    #define BOWSER_STATUS_FIRE_SKY                          (1 << 17) // 0x00020000
    /* oBowserGrabbedStatus */
    #define BOWSER_GRAB_STATUS_NONE                         0x0
    #define BOWSER_GRAB_STATUS_GRABBED                      0x1
    #define BOWSER_GRAB_STATUS_HOLDING                      0x2
    /* oSubAction */
        /* BOWSER_ACT_THROWN */
    #define BOWSER_SUB_ACT_THROWN_BOUNCE                    0x0
    #define BOWSER_SUB_ACT_THROWN_STOP                      0x1
        /* BOWSER_ACT_DEAD */
    #define BOWSER_SUB_ACT_DEAD_FLY_BACK                    0x0
    #define BOWSER_SUB_ACT_DEAD_BOUNCE                      0x1
    #define BOWSER_SUB_ACT_DEAD_WAIT                        0x2
    #define BOWSER_SUB_ACT_DEAD_DEFAULT_END                 0x3
    #define BOWSER_SUB_ACT_DEAD_DEFAULT_END_OVER            0x4
    #define BOWSER_SUB_ACT_DEAD_FINAL_END                   0xA
    #define BOWSER_SUB_ACT_DEAD_FINAL_END_OVER              0xB
        /* BOWSER_ACT_INTRO_WALK */
    #define BOWSER_SUB_ACT_INTRO_WALK_LOOK_UP               0x0
    #define BOWSER_SUB_ACT_INTRO_WALK_SLOWLY                0x1
    #define BOWSER_SUB_ACT_INTRO_WALK_STOP                  0x2
        /* BOWSER_ACT_CHARGE_MARIO */
    #define BOWSER_SUB_ACT_CHARGE_START                     0x0
    #define BOWSER_SUB_ACT_CHARGE_RUN                       0x1
    #define BOWSER_SUB_ACT_CHARGE_END                       0x2
    #define BOWSER_SUB_ACT_CHARGE_SLIP                      0x3
        /* BOWSER_ACT_TELEPORT */
    #define BOWSER_SUB_ACT_TELEPORT_START                   0x0
    #define BOWSER_SUB_ACT_TELEPORT_MOVE                    0x1
    #define BOWSER_SUB_ACT_TELEPORT_STOP                    0x2
        /* BOWSER_ACT_SPIT_FIRE_ONTO_FLOOR */
    #define BOWSER_SUB_ACT_SPIT_FIRE_FLOOR_START            0x0
    #define BOWSER_SUB_ACT_SPIT_FIRE_FLOOR_STOP             0x1
        /* BOWSER_ACT_HIT_EDGE */
    #define BOWSER_SUB_ACT_HIT_EDGE_START                   0x0
    #define BOWSER_SUB_ACT_HIT_EDGE_STOP                    0x1
        /* BOWSER_ACT_TURN_FROM_EDGE */
    #define BOWSER_SUB_ACT_TURN_FROM_EDGE_START             0x0
    #define BOWSER_SUB_ACT_TURN_FROM_EDGE_STOP              0x1
    #define BOWSER_SUB_ACT_TURN_FROM_EDGE_END               0x2
        /* BOWSER_ACT_HIT_MINE */
    #define BOWSER_SUB_ACT_HIT_MINE_START                   0x0
    #define BOWSER_SUB_ACT_HIT_MINE_FALL                    0x1
    #define BOWSER_SUB_ACT_HIT_MINE_STOP                    0x2
        /* BOWSER_ACT_JUMP_ONTO_STAGE */
    #define BOWSER_SUB_ACT_JUMP_ON_STAGE_IDLE               0x0
    #define BOWSER_SUB_ACT_JUMP_ON_STAGE_START              0x1
    #define BOWSER_SUB_ACT_JUMP_ON_STAGE_LAND               0x2
    #define BOWSER_SUB_ACT_JUMP_ON_STAGE_STOP               0x3
        /* BOWSER_ACT_BIG_JUMP */
    #define BOWSER_SUB_ACT_BIG_JUMP_START                   0x0
    #define BOWSER_SUB_ACT_BIG_JUMP_LAND                    0x1
    #define BOWSER_SUB_ACT_BIG_JUMP_STOP                    0x2
        /* BOWSER_ACT_WALK_TO_MARIO */
    #define BOWSER_SUB_ACT_WALK_TO_MARIO_START              0x0
    #define BOWSER_SUB_ACT_WALK_TO_MARIO_WALKING            0x1
    #define BOWSER_SUB_ACT_WALK_TO_MARIO_STOP               0x2
        /* BOWSER_ACT_QUICK_JUMP */
    #define BOWSER_SUB_ACT_QUICK_JUMP_START                 0x0
    #define BOWSER_SUB_ACT_QUICK_JUMP_LAND                  0x1
    #define BOWSER_SUB_ACT_QUICK_JUMP_STOP                  0x2

/* Bowser BITS Platform */
    /* oAction */
    #define BOWSER_BITS_PLAT_ACT_START                      0x0
    #define BOWSER_BITS_PLAT_ACT_CHECK                      0x1
    #define BOWSER_BITS_PLAT_ACT_FALL                       0x2
    /* oSubAction */
        /* BOWSER_BITS_PLAT_ACT_CHECK */
    #define BOWSER_BITS_PLAT_SUB_ACT_CHECK_RESET_TIMER      0x0
    #define BOWSER_BITS_PLAT_SUB_ACT_CHECK_DEBUG_FALL       0x1

/* Bowser Flame */
    /* oAction */
    #define BOWSER_FLAME_ACT_FLOATING                       0x0
    #define BOWSER_FLAME_ACT_LANDED                         0x1

/* Blue Bowser Flame */
    /* oBehParams2ndByte */
    #define BOWSER_FLAME_BLUE_BP_SPAWN_RED_FLAMES           0x0
    #define BOWSER_FLAME_BLUE_BP_SPAWN_BLUE_FLAMES          0x1

/* Bowser Floating Flame */
    /* oBehParams2ndByte */
    #define BOWSER_FLOATING_FLAME_SPAWN_RED_FLAME           0x0
    #define BOWSER_FLOATING_FLAME_SPAWN_BLUE_FLAME_1        0x1
    #define BOWSER_FLOATING_FLAME_SPAWN_BLUE_FLAME_2        0x2

/* Bowser Key */
    /* oAction */
    #define BOWSER_KEY_ACT_BOUNCING                         0x0
    #define BOWSER_KEY_ACT_LANDED                           0x1

/* Bowser Key Cutscene */
    /* Animations */
    #define BOWSER_KEY_CUTSCENE_ANIM_UNLOCK_DOOR            0x0
    #define BOWSER_KEY_CUTSCENE_ANIM_COURSE_EXIT            0x1

/* Fish Spawer */
    /* oBehParams2ndByte */
    #define FISH_SPAWNER_BP_MANY_BLUE                       0x0
    #define FISH_SPAWNER_BP_FEW_BLUE                        0x1
    #define FISH_SPAWNER_BP_MANY_CYAN                       0x2
    #define FISH_SPAWNER_BP_FEW_CYAN                        0x3
    /* oAction */
    #define FISH_SPAWNER_ACT_SPAWN                          0x0
    #define FISH_SPAWNER_ACT_IDLE                           0x1
    #define FISH_SPAWNER_ACT_RESPAWN                        0x2

/* Fish */
    /* oAction */
    #define FISH_ACT_INIT                                   0x0
    #define FISH_ACT_ROAM                                   0x1
    #define FISH_ACT_FLEE                                   0x2
    /* Animations */
    #define FISH_ANIM_DEFAULT                               0x0

/* Blue Fish Spawner */
    /* oAction: bhv_blue_fish_spawn_loop */
    #define BLUE_FISH_ACT_SPAWN                             0x0
    #define BLUE_FISH_ACT_ROOM                              0x1
    #define BLUE_FISH_ACT_DUPLICATE                         0x2

/* Blue Fish */
    /* oAction */
    #define BLUE_FISH_ACT_DIVE                              0x0
    #define BLUE_FISH_ACT_TURN                              0x1
    #define BLUE_FISH_ACT_ASCEND                            0x2
    #define BLUE_FISH_ACT_TURN_BACK                         0x3
    /* Animations */
    #define BLUE_FISH_ANIM_DEFAULT                          0x0

/* Blue Fish */
    /* oAction */
    #define BLUE_FISH_ACT_DIVE                              0x0
    #define BLUE_FISH_ACT_TURN                              0x1
    #define BLUE_FISH_ACT_ASCEND                            0x2
    #define BLUE_FISH_ACT_TURN_BACK                         0x3
    /* Animations */
    #define BLUE_FISH_ANIM_DEFAULT                          0x0

/* Cheep Cheep Spawner */
    /* oAction */
    #define BUB_SPAWNER_ACT_SPAWN_BUBS                      0x0
    #define BUB_SPAWNER_ACT_IDLE                            0x1
    #define BUB_SPAWNER_ACT_REMOVE_BUBS                     0x2
    #define BUB_SPAWNER_ACT_RESET                           0x3

/* Cheep Cheep */
    /* oAction */
    #define BUB_ACT_INIT                                    0x0
    #define BUB_ACT_SWIMMING_TOWARDS_MARIO                  0x1
    #define BUB_ACT_SWIMMING_AWAY_FROM_MARIO                0x2
    /* Animations */
    #define BUB_ANIM_SWIM                                   0x0

/* Bubba */
    /* oAction */
    #define BUBBA_ACT_IDLE                                  0x0
    #define BUBBA_ACT_ATTACK                                0x1
    /* oAnimState */
    #define BUBBA_ANIM_STATE_CLOSED_MOUTH                   0x0
    #define BUBBA_ANIM_STATE_OPEN_MOUTH                     0x1

/* Seaweed */
    /* Animations */
    #define SEAWEED_ANIM_WAVE                               0x0

/* Clam */
    /* oAction */
    #define CLAM_ACT_CLOSING                                0x0
    #define CLAM_ACT_OPENING                                0x1
    /* Animations */
    #define CLAM_ANIM_CLOSING                               0x0
    #define CLAM_ANIM_OPENING                               0x1

/* Purple Switch */
    /* oBehParams2ndByte */
    #define PURPLE_SWITCH_BP_NO_TICK                        0x0
    #define PURPLE_SWITCH_BP_ANIMATES                       0x1
    #define PURPLE_SWITCH_BP_REVEAL_HIDDEN                  0x2
    /* oAction */
    #define PURPLE_SWITCH_ACT_IDLE                          0x0
    #define PURPLE_SWITCH_ACT_PRESSED                       0x1
    #define PURPLE_SWITCH_ACT_TICKING                       0x2
    #define PURPLE_SWITCH_ACT_UNPRESSED                     0x3
    #define PURPLE_SWITCH_ACT_WAIT_FOR_MARIO_TO_GET_OFF     0x4

/* Animates on Floor Switch */
    /* oBehParams2ndByte */
    #define ANIMATES_ON_FLOOR_SWITCH_BP_BITS_STAIRS         0x0
    #define ANIMATES_ON_FLOOR_SWITCH_BP_BITDW_STAIRS        0x1
    #define ANIMATES_ON_FLOOR_SWITCH_BP_RR_TRIGANGLES       0x2

/* Openable Grill */
    /* oBehParams2ndByte */
    #define OPENABLE_GRILL_BP_BOB                           0x0
    #define OPENABLE_GRILL_BP_HMC                           0x1
    /* oAction */
    #define OEPNABLE_GRILL_ACT_SPAWN                        0x0
    #define OEPNABLE_GRILL_IDLE_CLOSED                      0x1
    #define OEPNABLE_GRILL_OPENING                          0x2
    #define OEPNABLE_GRILL_IDLE_OPEN                        0x3

/* Openable Grill Door */
    /* oBehParams2ndByte */
    #define OPENABLE_GRILL_DOOR_BP_SIDE_FLIPPED            -0x1
    #define OPENABLE_GRILL_DOOR_BP_SIDE_UNFLIPPED           0x1
    /* oAction */
    #define OPENABLE_GRILL_DOOR_ACT_CLOSED                  0x0
    #define OPENABLE_GRILL_DOOR_ACT_OPENING                 0x1
    #define OPENABLE_GRILL_DOOR_ACT_OPEN                    0x2

/* Breakable Box */
    /* oBehParams2ndByte */
    #define BREAKABLE_BOX_BP_NO_COINS                       0x0
    #define BREAKABLE_BOX_BP_3_COINS                        0x1
    #define BREAKABLE_BOX_BP_5_COINS                        0x2
    #define BREAKABLE_BOX_BP_LARGE                          0x3
    /* oAction */
    #define BREAKABLE_BOX_ACT_HIDDEN                        0x0
    #define BREAKABLE_BOX_ACT_ACTIVE                        0x1
    #define BREAKABLE_BOX_ACT_BROKEN                        0x2
    /* oAnimState */
    #define BREAKABLE_BOX_ANIM_STATE_CRAZY_BOX              0x0
    #define BREAKABLE_BOX_ANIM_STATE_CORK_BOX               0x1

/* Small Breakable Box */
    /* oAction */
    #define BREAKABLE_BOX_SMALL_ACT_MOVE                    0x0

/* Jumping Box */
    /* oAction */
    #define JUMPING_BOX_ACT_IDLE                            0x0
    #define JUMPING_BOX_ACT_DROPPED                         0x1
    /* oSubAction */
        /* JUMPING_BOX_ACT_IDLE */
    #define JUMPING_BOX_SUB_ACT_IDLE_BOUNCING               0x0
    #define JUMPING_BOX_SUB_ACT_IDLE_RESET_TIMER            0x1

/* Exclamation Box */
    /* oBehParams2ndByte, ExclamationBoxContents->id */
    #define EXCLAMATION_BOX_BP_WING_CAP                     0x0
    #define EXCLAMATION_BOX_BP_METAL_CAP                    0x1
    #define EXCLAMATION_BOX_BP_VANISH_CAP                   0x2
    #define EXCLAMATION_BOX_BP_KOOPA_SHELL                  0x3
    #define EXCLAMATION_BOX_BP_COINS_1                      0x4
    #define EXCLAMATION_BOX_BP_COINS_3                      0x5
    #define EXCLAMATION_BOX_BP_COINS_10                     0x6
    #define EXCLAMATION_BOX_BP_1UP_WALKING                  0x7
    #define EXCLAMATION_BOX_BP_STAR_1                       0x8
    #define EXCLAMATION_BOX_BP_1UP_RUNNING_AWAY             0x9
    #define EXCLAMATION_BOX_BP_STAR_2                       0xA
    #define EXCLAMATION_BOX_BP_STAR_3                       0xB
    #define EXCLAMATION_BOX_BP_STAR_4                       0xC
    #define EXCLAMATION_BOX_BP_STAR_5                       0xD
    #define EXCLAMATION_BOX_BP_STAR_6                       0xE
    #define EXCLAMATION_BOX_BP_NULL                         0x63
    /* oAction */
    #define EXCLAMATION_BOX_ACT_INIT                        0x0
    #define EXCLAMATION_BOX_ACT_OUTLINE                     0x1
    #define EXCLAMATION_BOX_ACT_ACTIVE                      0x2
    #define EXCLAMATION_BOX_ACT_SCALING                     0x3
    #define EXCLAMATION_BOX_ACT_EXPLODE                     0x4
    #define EXCLAMATION_BOX_ACT_WAIT_FOR_RESPAWN            0x5
    /* oAnimState */
    #define EXCLAMATION_BOX_ANIM_STATE_RED                  0x0
    #define EXCLAMATION_BOX_ANIM_STATE_GREEN                0x1
    #define EXCLAMATION_BOX_ANIM_STATE_BLUE                 0x2
    #define EXCLAMATION_BOX_ANIM_STATE_YELLOW               0x3

/* Cap Switch */
    /* oAction */
    #define CAP_SWITCH_ACT_INIT                             0x0
    #define CAP_SWITCH_ACT_IDLE                             0x1
    #define CAP_SWITCH_ACT_BEING_PRESSED                    0x2
    #define CAP_SWITCH_ACT_DONE                             0x3

/* Mario Cap */
    /* oAction */
    #define CAP_ACT_MOVE                                    0x0
    #define CAP_ACT_QUICKSAND                               0xA
    #define CAP_ACT_MOVING_QUICKSAND                        0xB
    #define CAP_ACT_INSTANT_QUICKSAND                       0xC
    #define CAP_ACT_INSTANT_MOVING_QUICKSAND                0xD

/* Koopa Shell */
    /* oAction */
    #define KOOPA_SHELL_ACT_MARIO_NOT_RIDING                0x0
    #define KOOPA_SHELL_ACT_MARIO_RIDING                    0x1

/* Koopa Shell Underwater */
    /* oAction */
    #define KOOPA_SHELL_UNDERWATER_ACT_DEFAULT              0x0

/* Cannon Trap Door */
    /* oAction */
    #define CANNON_TRAP_DOOR_ACT_CLOSED                     0x0
    #define CANNON_TRAP_DOOR_ACT_CAM_ZOOM                   0x1
    #define CANNON_TRAP_DOOR_ACT_OPENING                    0x2
    #define CANNON_TRAP_DOOR_ACT_OPEN                       0x3

/* Opened Cannon */
    /* oAction */
    #define OPENED_CANNON_ACT_IDLE                          0x0
    #define OPENED_CANNON_ACT_READY                         0x1
    #define OPENED_CANNON_ACT_SHOOT                         0x2
    #define OPENED_CANNON_ACT_RESETTING                     0x3
    #define OPENED_CANNON_ACT_RISING                        0x4
    #define OPENED_CANNON_ACT_RAISE_BARREL                  0x5
    #define OPENED_CANNON_ACT_TURNING_YAW                   0x6

/* Door */
    /* oAction */
    #define DOOR_ACT_CLOSED                                 0x0
    #define DOOR_ACT_PULLED                                 0x1
    #define DOOR_ACT_PUSHED                                 0x2
    #define DOOR_ACT_WARP_PULLED                            0x3
    #define DOOR_ACT_WARP_PUSHED                            0x4
    /* Animations */
    #define DOOR_ANIM_CLOSED                                0x0
    #define DOOR_ANIM_PULLED                                0x1
    #define DOOR_ANIM_PUSHED                                0x2
    #define DOOR_ANIM_WARP_PULLED                           0x3
    #define DOOR_ANIM_WARP_PUSHED                           0x4

/* Star Door */
    /* oAction */
    #define STAR_DOOR_ACT_CLOSED                            0x0
    #define STAR_DOOR_ACT_OPENING                           0x1
    #define STAR_DOOR_ACT_OPEN                              0x2
    #define STAR_DOOR_ACT_CLOSING                           0x3
    #define STAR_DOOR_ACT_RESET                             0x4

/* Castle Trap Door */
    /* oAction */
    #define CASTLE_FLOOR_TRAP_ACT_OPEN_DETECT               0x0
    #define CASTLE_FLOOR_TRAP_ACT_OPEN                      0x1
    #define CASTLE_FLOOR_TRAP_ACT_CLOSE_DETECT              0x2
    #define CASTLE_FLOOR_TRAP_ACT_CLOSE                     0x3
    #define CASTLE_FLOOR_TRAP_ACT_ROTATE                    0x4

/* Castle Flag */
    /* Animations */
    #define CASTLE_FLAG_ANIM_WAVE                           0x0

/* Homing Amp */
    /* oAction */
    #define HOMING_AMP_ACT_INACTIVE                         0x0
    #define HOMING_AMP_ACT_APPEAR                           0x1
    #define HOMING_AMP_ACT_CHASE                            0x2
    #define HOMING_AMP_ACT_GIVE_UP                          0x3
    #define HOMING_AMP_ACT_ATTACK_COOLDOWN                  0x4

/* Amp */
    /* oBehParams2ndByte */
    #define AMP_BP_ROT_RADIUS_200                           0x0
    #define AMP_BP_ROT_RADIUS_300                           0x1
    #define AMP_BP_ROT_RADIUS_400                           0x2
    #define AMP_BP_ROT_RADIUS_0                             0x3
    /* oAction */
    #define AMP_ACT_IDLE                                    0x2
    #define AMP_ACT_ATTACK_COOLDOWN                         0x4
    /* oAnimState */
    #define AMP_ANIM_STATE_OFF                              0x0
    #define AMP_ANIM_STATE_ON                               0x1
    /* Animations */
    #define AMP_ANIM_DEFAULT                                0x0

/* Butterfly */
    /* oAction */
    #define BUTTERFLY_ACT_RESTING                           0x0
    #define BUTTERFLY_ACT_FOLLOW_MARIO                      0x1
    #define BUTTERFLY_ACT_RETURN_HOME                       0x2
    /* Animations */
    #define BUTTERFLY_ANIM_FLYING                           0x0
    #define BUTTERFLY_ANIM_RESTING                          0x1

/* Hoot */
    /* oAction */
    #define HOOT_ACT_ASCENT                                 0x0
    #define HOOT_ACT_CARRY                                  0x1
    #define HOOT_ACT_TIRED                                  0x2
    /* oHootAvailability */
    #define HOOT_AVAIL_ASLEEP_IN_TREE                       0x0
    #define HOOT_AVAIL_WANTS_TO_TALK                        0x1
    #define HOOT_AVAIL_READY_TO_FLY                         0x2
    /* Animations */
    #define HOOT_ANIM_DEFAULT                               0x0
    #define HOOT_ANIM_HOLDING_MARIO                         0x1

/* Bully (all variants) */
    /* oBehParams2ndByte */
    #define BULLY_BP_SIZE_SMALL                             0x0
    #define BULLY_BP_SIZE_BIG                               0x1
    /* oAction */
    #define BULLY_ACT_PATROL                                0x0
    #define BULLY_ACT_CHASE_MARIO                           0x1
    #define BULLY_ACT_KNOCKBACK                             0x2
    #define BULLY_ACT_BACK_UP                               0x3
    #define BULLY_ACT_INACTIVE                              0x4
    #define BULLY_ACT_ACTIVATE_AND_FALL                     0x5
    /* oBullySubtype */
    #define BULLY_STYPE_GENERIC                             0x00
    #define BULLY_STYPE_MINION                              0x01
    #define BULLY_STYPE_CHILL                               0x10
    /* Animations */
    #define BULLY_ANIM_WALKING                              0x0
    #define BULLY_ANIM_RUNNING                              0x1
    #define BULLY_ANIM_UNUSED                               0x2
    #define BULLY_ANIM_BOAST                                0x3

/* Water Ring (all variants) */
    /* oAction */
    #define WATER_RING_ACT_NOT_COLLECTED                    0x0
    #define WATER_RING_ACT_COLLECTED                        0x1
    /* Animations */
    #define WATER_RING_ANIM_WOBBLE                          0x0

/* Jet Stream Water Ring Spawner */
    /* oAction */
    #define JS_RING_SPAWNER_ACT_ACTIVE                      0x0
    #define JS_RING_SPAWNER_ACT_INACTIVE                    0x1

/* Hidden Star */
    /* oAction */
    #define HIDDEN_STAR_ACT_INACTIVE                        0x0
    #define HIDDEN_STAR_ACT_ACTIVE                          0x1

/* Spawn Star stay at position cutscene */
    /* oBehParams2ndByte */
    #define SPAWN_STAR_POS_CUTSCENE_BP_SPAWN_AT_MARIO       0x0
    #define SPAWN_STAR_POS_CUTSCENE_BP_SPAWN_AT_HOME        0x1
    /* oAction */
    #define SPAWN_STAR_POS_CUTSCENE_ACT_START               0x0
    #define SPAWN_STAR_POS_CUTSCENE_ACT_BOUNCE              0x1
    #define SPAWN_STAR_POS_CUTSCENE_ACT_END                 0x2
    #define SPAWN_STAR_POS_CUTSCENE_ACT_SLOW_STAR_ROTATION  0x3

/* Spawn Star arc to position cutscene */
    /* oBehParams2ndByte */
    #define SPAWN_STAR_ARC_CUTSCENE_BP_DEFAULT_STAR         0x0
    #define SPAWN_STAR_ARC_CUTSCENE_BP_HIDDEN_STAR          0x1
    /* oAction */
    #define SPAWN_STAR_ARC_CUTSCENE_ACT_START               0x0
    #define SPAWN_STAR_ARC_CUTSCENE_ACT_GO_TO_HOME          0x1
    #define SPAWN_STAR_ARC_CUTSCENE_ACT_BOUNCE              0x2
    #define SPAWN_STAR_ARC_CUTSCENE_ACT_END                 0x3

/* Celebration Star */
    /* oAction */
    #define CELEB_STAR_ACT_SPIN_AROUND_MARIO                0x0
    #define CELEB_STAR_ACT_FACE_CAMERA                      0x1

/* Grand Star */
    /* oAction */
    #define GRAND_STAR_ACT_APPEAR                           0x0
    #define GRAND_STAR_ACT_JUMP                             0x1
    #define GRAND_STAR_ACT_WAIT_FOR_INTERACTION             0x2
    /* oSubAction */
        /* GRAND_STAR_ACT_JUMP */
    #define GRAND_STAR_SUB_ACT_START_JUMP                   0x0
    #define GRAND_STAR_SUB_ACT_CONTINUE_JUMP                0x1

/* LLL Drawbridge */
    /* oAction */
    #define LLL_DRAWBRIDGE_ACT_LOWER                        0x0
    #define LLL_DRAWBRIDGE_ACT_RAISE                        0x1

/* LLL Volcano Trap */
    /* oAction */
    #define LLL_VOLCANO_TRAP_ACT_WAIT                       0x0
    #define LLL_VOLCANO_TRAP_ACT_FALL                       0x1
    #define LLL_VOLCANO_TRAP_ACT_LAND                       0x2
    #define LLL_VOLCANO_TRAP_ACT_RISE                       0x3

/* LLL Floating Wood Bridge */
    /* oAction */
    #define LLL_FLOATING_WOOD_ACT_INACTIVE                  0x0
    #define LLL_FLOATING_WOOD_ACT_ACTIVE                    0x1
    #define LLL_FLOATING_WOOD_ACT_REMOVE_PIECES             0x2

/* LLL Hexagonal Ring */
    /* oAction */
    #define LLL_HEXAGONAL_RING_ACT_MARIO_OFF_PLATFORM       0x0
    #define LLL_HEXAGONAL_RING_ACT_MARIO_ON_PLATFORM        0x1
    #define LLL_HEXAGONAL_RING_ACT_SPAWN_FLAMES             0x2
    #define LLL_HEXAGONAL_RING_ACT_MARIO_LEFT_PLATFORM      0x3
    #define LLL_HEXAGONAL_RING_ACT_RESET                    0x4

/* LLL Octagonal Rotating Mesh */
    /* oAction */
    #define LLL_OCTAGONAL_ROTATING_MESH_ACT_RESET           0x0
    #define LLL_OCTAGONAL_ROTATING_MESH_ACT_MOVE            0x1

/* LLL Fire Bar */
    /* oBehParams2ndByte */
    #define LLL_FIRE_BAR_BP_DEFAULT                         0x4
    /* oAction */
    #define LLL_FIRE_BAR_ACT_INACTIVE                       0x0
    #define LLL_FIRE_BAR_ACT_SPAWN_FLAMES                   0x1
    #define LLL_FIRE_BAR_ACT_ACTIVE                         0x2
    #define LLL_FIRE_BAR_ACT_REMOVE_FLAMES                  0x3

/* BITFS Sinking Cage Platform with pole */
    /* oBehParams2ndByte */
    #define SINKING_POLE_PLATFORM_BP_LOW                    0x0
    #define SINKING_POLE_PLATFORM_BP_HIGH                   0x1

/* Tumbling Bridge Platform */
    /* oAction */
    #define TUMBLING_BRIDGE_PLATFORM_ACT_IDLE               0x0
    #define TUMBLING_BRIDGE_PLATFORM_ACT_UNSTABLE           0x1
    #define TUMBLING_BRIDGE_PLATFORM_ACT_FALL               0x2
    #define TUMBLING_BRIDGE_PLATFORM_ACT_END                0x3

/* Tumbling Bridge */
    /* oBehParams2ndByte, bridgeID */
    #define TUMBLING_BRIDGE_BP_WF                           0x0
    #define TUMBLING_BRIDGE_BP_BBH                          0x1
    #define TUMBLING_BRIDGE_BP_LLL                          0x2
    #define TUMBLING_BRIDGE_BP_BITFS                        0x3
    /* oAction */
    #define TUMBLING_BRIDGE_ACT_FAR                         0x0
    #define TUMBLING_BRIDGE_ACT_SPAWN_SECTIONS              0x1
    #define TUMBLING_BRIDGE_ACT_NEAR                        0x2
    #define TUMBLING_BRIDGE_ACT_RESET                       0x3

/* Bomp (both variants) */
    /* oAction */
    #define BOMP_ACT_WAIT                                   0x0
    #define BOMP_ACT_POKE_OUT                               0x1
    #define BOMP_ACT_EXTEND                                 0x2
    #define BOMP_ACT_RETRACT                                0x3

/* WF Tower Platform Group */
    /* oAction */
    #define WF_TOWER_PLATFORM_GROUP_ACT_INACTIVE            0x0
    #define WF_TOWER_PLATFORM_GROUP_ACT_SPAWN_PLATFORMS     0x1
    #define WF_TOWER_PLATFORM_GROUP_ACT_ACTIVE              0x2
    #define WF_TOWER_PLATFORM_GROUP_ACT_REMOVE_PLATFORMS    0x3

/* WF Tower Sliding Platform */
    /* oAction */
    #define WF_TOWER_SLIDING_PLATFORM_ACT_BACKWARD          0x0
    #define WF_TOWER_SLIDING_PLATFORM_ACT_FORWARD           0x1

/* WF Tower Elevator Platform */
    /* oAction */
    #define WF_TOWER_ELEVATOR_PLATFORM_ACT_BOTTOM           0x0
    #define WF_TOWER_ELEVATOR_PLATFORM_ACT_MOVING_UP        0x1
    #define WF_TOWER_ELEVATOR_PLATFORM_ACT_TOP              0x2
    #define WF_TOWER_ELEVATOR_PLATFORM_ACT_MOVING_DOWN      0x3

/* WF Sliding Brick Platform */
    /* oBehParams2ndByte */
    #define WF_SLID_BRICK_PTFM_BP_MOV_VEL_10                0x1
    #define WF_SLID_BRICK_PTFM_BP_MOV_VEL_15                0x2
    #define WF_SLID_BRICK_PTFM_BP_MOV_VEL_20                0x3
    /* oAction */
    #define WF_SLID_BRICK_PTFM_ACT_WAIT                     0x0
    #define WF_SLID_BRICK_PTFM_ACT_EXTEND                   0x1
    #define WF_SLID_BRICK_PTFM_ACT_RETRACT                  0x2

/* Sliding Platform */
    #define SLIDING_PLATFORM_TYPE_MASK                      0x0380
    #define SLIDING_PLATFORM_LENGTH_MASK                    0x003F
    #define SLIDING_PLATFORM_DIRECTION_MASK                 0x0040
    /* collisionDataIndex */
    #define SLIDING_PLATFORM_BP_BITS_SLIDING_PLATFORM       0x0
    #define SLIDING_PLATFORM_BP_BITS_TWIN_SLIDING_PLATFORMS 0x1
    #define SLIDING_PLATFORM_BP_BITFS_MOVING_SQUARE         0x2
    #define SLIDING_PLATFORM_BP_BITFS_SLIDING_PLATFORM      0x3
    #define SLIDING_PLATFORM_BP_RR_SLIDING_PLATFORM         0x4
    #define SLIDING_PLATFORM_BP_RR_PYRAMID                  0x5
    #define SLIDING_PLATFORM_BP_NULL                        0x6
    #define SLIDING_PLATFORM_BP_BITDW_SLIDING_PLATFORM      0x7

/* BITDW Pyramid Platforms */
    /* oAction */
    #define BITDW_PYRAMID_PLATFORM_ACT_INIT_DIRECTION       0x0
    #define BITDW_PYRAMID_PLATFORM_ACT_MOVE_0               0x1
    #define BITDW_PYRAMID_PLATFORM_ACT_MOVE_90              0x2
    #define BITDW_PYRAMID_PLATFORM_ACT_MOVE_180             0x3
    #define BITDW_PYRAMID_PLATFORM_ACT_MOVE_270             0x4

/* Rotating Platform */
    /* oAction */
    #define ROTATING_PLATFORM_ACT_STOPPED                   0x0
    #define ROTATING_PLATFORM_ACT_MOVING                    0x1

/* Seesaw Platform */
    /* oBehParams2ndByte */
    #define SEESAW_PLATFORM_BP_BITDW                        0x0
    #define SEESAW_PLATFORM_BP_BITS                         0x1
    #define SEESAW_PLATFORM_BP_BITS_W_SHAPED                0x2
    #define SEESAW_PLATFORM_BP_BOB_BRIDGE                   0x3
    #define SEESAW_PLATFORM_BP_BITFS                        0x4
    #define SEESAW_PLATFORM_BP_RR                           0x5
    #define SEESAW_PLATFORM_BP_RR_L_SHAPED                  0x6
    #define SEESAW_PLATFORM_BP_VCUTM                        0x7

/* Fake Moneybag Coin */
    /* oAction */
    #define FAKE_MONEYBAG_COIN_ACT_IDLE                     0x0
    #define FAKE_MONEYBAG_COIN_ACT_TRANSFORM                0x1

/* Moneybag */
    /* oAction */
    #define MONEYBAG_ACT_APPEAR                             0x0
    #define MONEYBAG_ACT_UNUSED_APPEAR                      0x1
    #define MONEYBAG_ACT_MOVE_AROUND                        0x2
    #define MONEYBAG_ACT_RETURN_HOME                        0x3
    #define MONEYBAG_ACT_DISAPPEAR                          0x4
    #define MONEYBAG_ACT_DEATH                              0x5
    /* oMoneybagJumpState */
    #define MONEYBAG_JUMP_LANDING                           0x0
    #define MONEYBAG_JUMP_PREPARE                           0x1
    #define MONEYBAG_JUMP_JUMP                              0x2
    #define MONEYBAG_JUMP_JUMP_AND_BOUNCE                   0x3
    #define MONEYBAG_JUMP_WALK_AROUND                       0x4
    #define MONEYBAG_JUMP_WALK_HOME                         0x5
    /* Animations */
    #define MONEYBAG_ANIM_IDLE                              0x0
    #define MONEYBAG_ANIM_PREPARE_JUMP                      0x1
    #define MONEYBAG_ANIM_JUMP                              0x2
    #define MONEYBAG_ANIM_LAND                              0x3
    #define MONEYBAG_ANIM_WALK                              0x4

/* Bowling Ball */
    /* oAction */
    #define BBALL_ACT_INITIALIZE                            0x0
    #define BBALL_ACT_ROLL                                  0x1

/* Bowling Ball + Bowling Ball Spawner (all variants) */
    /* oBehParams2ndByte */
    #define BBALL_BP_STYPE_BOB_UPPER                        0x0
    #define BBALL_BP_STYPE_TTM                              0x1
    #define BBALL_BP_STYPE_BOB_LOWER                        0x2
    #define BBALL_BP_STYPE_THI_LARGE                        0x3
    #define BBALL_BP_STYPE_THI_SMALL                        0x4

/* Bowling Ball (Free) */
    /* oAction */
    #define FREE_BBALL_ACT_IDLE                             0x0
    #define FREE_BBALL_ACT_ROLL                             0x1
    #define FREE_BBALL_ACT_RESET                            0x2

/* THI Top */
    /* oAction */
    #define THI_TOP_ACT_IDLE                                0x0
    #define THI_TOP_ACT_DRAIN_WATER                         0x1

/* Beta Chest Lid */
    /* oAction */
    #define BETA_CHEST_ACT_IDLE_CLOSED                      0x0
    #define BETA_CHEST_ACT_OPENING                          0x1
    #define BETA_CHEST_ACT_IDLE_OPEN                        0x2

/* Treasure Chest Top */
    /* oAction */
    #define TREASURE_CHEST_TOP_ACT_CLOSED                   0x0
    #define TREASURE_CHEST_TOP_ACT_OPENING                  0x1
    #define TREASURE_CHEST_TOP_ACT_OPENED                   0x2
    #define TREASURE_CHEST_TOP_ACT_CLOSING                  0x3

/* Treasure Chest Bottom */
    /* oBehParams2ndByte */
    #define TREASURE_CHEST_BP_1                             0x1
    #define TREASURE_CHEST_BP_2                             0x2
    #define TREASURE_CHEST_BP_3                             0x3
    #define TREASURE_CHEST_BP_4                             0x4
    /* oAction */
    #define TREASURE_CHEST_BOTTOM_ACT_CLOSE                 0x0
    #define TREASURE_CHEST_BOTTOM_ACT_OPENING               0x1
    #define TREASURE_CHEST_BOTTOM_ACT_OPENED                0x2

/* Treasure Chest Manager */
    /* oAction */
    #define TREASURE_CHEST_ACT_SUCCESS_SOUND                0x0
    #define TREASURE_CHEST_ACT_REWARD                       0x1
    #define TREASURE_CHEST_ACT_END                          0x2

/* BBH Tilting Trap Platform */
    /* oAction */
    #define BBH_TILTING_TRAP_PLATFORM_ACT_MARIO_ON          0x0
    #define BBH_TILTING_TRAP_PLATFORM_ACT_MARIO_OFF         0x1

/* Boo in Castle */
    /* oAction */
    #define BOO_IN_CASTLE_ACT_INIT                          0x0
    #define BOO_IN_CASTLE_ACT_IDLE                          0x1
    #define BOO_IN_CASTLE_ACT_FLEE                          0x2

/* Boo */
    /* oBehParams2ndByte */
    #define BOO_BP_GHOST_HUNT                               0x0
    #define BOO_BP_NORMAL                                   0x1
    #define BOO_BP_MERRY_GO_ROUND                           0x2
    /* oAction */
    #define BOO_ACT_STOPPED                                 0x0
    #define BOO_ACT_CHASING_MARIO                           0x1
    #define BOO_ACT_BOUNCED_ON                              0x2
    #define BOO_ACT_ATTACKED                                0x3
    #define BOO_ACT_DEATH                                   0x4
    #define BOO_ACT_MERRY_GO_ROUND_WAIT                     0x5
    /* oBooDeathStatus */
    #define BOO_DEATH_STATUS_ALIVE                          0x0
    #define BOO_DEATH_STATUS_DYING                          0x1
    #define BOO_DEATH_STATUS_DEAD                           0x2
    /* attackStatus */
    #define BOO_NOT_ATTACKED                                0x0
    #define BOO_ATTACKED                                    0x1
    #define BOO_BOUNCED_ON                                 -0x1

/* Big Boo */
    /* oBehParams2ndByte */
    #define BIG_BOO_BP_GHOST_HUNT                           0x0
    #define BIG_BOO_BP_MERRY_GO_ROUND                       0x1
    #define BIG_BOO_BP_BALCONY                              0x2

/* Fishing Boo */
    /* oAction */
    #define FISHING_BOO_ACT_0                               0x0
    #define FISHING_BOO_ACT_1                               0x1
    #define FISHING_BOO_ACT_2                               0x2
    #define FISHING_BOO_ACT_3                               0x3
    #define FISHING_BOO_ACT_4                               0x4
    #define FISHING_BOO_ACT_5                               0x5
    #define FISHING_BOO_ACT_6                               0x6
    #define FISHING_BOO_ACT_SPAWN_FLAMES                    0x7
    #define FISHING_BOO_ACT_8                               0x8
    #define FISHING_BOO_ACT_9                               0x9
    #define FISHING_BOO_ACT_10                              0xA

/* Beta Boo Key */
    /* oAction */
    #define BETA_BOO_KEY_ACT_IN_BOO                         0x0
    #define BETA_BOO_KEY_ACT_DROPPING                       0x1
    #define BETA_BOO_KEY_ACT_DROPPED                        0x2

/* Boo Cage */
    /* oAction */
    #define BOO_CAGE_ACT_IN_BOO                             0x0
    #define BOO_CAGE_ACT_FALLING                            0x1
    #define BOO_CAGE_ACT_ON_GROUND                          0x2
    #define BOO_CAGE_ACT_MARIO_JUMPING_IN                   0x3
    #define BOO_CAGE_ACT_USELESS                            0x4

/* BBH Staircase */
    /* oBehParams2ndByte */
    #define BOO_STAIRCASE_BP_0                              0x0
    #define BOO_STAIRCASE_BP_1                              0x1
    #define BOO_STAIRCASE_BP_2                              0x2
    /* oAction */
    #define BOO_STAIRCASE_ACT_INIT                          0x0
    #define BOO_STAIRCASE_ACT_RISE                          0x1
    #define BOO_STAIRCASE_ACT_WOBBLE                        0x2
    #define BOO_STAIRCASE_ACT_PLAY_JINGLE                   0x3

/* BBH Haunted Bookshelf */
    /* oAction */
    #define HAUNTED_BOOKSHELF_ACT_IDLE                      0x0
    #define HAUNTED_BOOKSHELF_ACT_RECEDE                    0x1

/* BBH Bookshelf Manager */
    /* oAction */
    #define BOOKSHELF_MANAGER_ACT_SPAWN_SWITCHES            0x0
    #define BOOKSHELF_MANAGER_ACT_CHECK_ACTIVATE            0x1
    #define BOOKSHELF_MANAGER_ACT_ACTIVE                    0x2
    #define BOOKSHELF_MANAGER_ACT_RECEDE                    0x3
    #define BOOKSHELF_MANAGER_ACT_END                       0x4

/* BBH Book Switch */
    /* oBehParams2ndByte */
    #define BOOK_SWITCH_BP_CHOICE_1                         0x0
    #define BOOK_SWITCH_BP_CHOICE_2                         0x1
    #define BOOK_SWITCH_BP_CHOICE_3                         0x2
    /* oAction */
    #define BOOK_SWITCH_ACT_UNPRESSED                       0x0
    #define BOOK_SWITCH_ACT_ACTIVE                          0x1
    #define BOOK_SWITCH_ACT_PRESSED                         0x2

/* Flying Bookend */
    /* oAction */
    #define FLYING_BOOKEND_ACT_INIT                         0x0
    #define FLYING_BOOKEND_ACT_GROW                         0x1
    #define FLYING_BOOKEND_ACT_TURN_TOWARD_MARIO            0x2
    #define FLYING_BOOKEND_ACT_FLY_FORWARD                  0x3
    /* Animations */
    #define FLYING_BOOKEND_ANIM_SPAWN                       0x0
    #define FLYING_BOOKEND_ANIM_BITE                        0x1
    #define FLYING_BOOKEND_ANIM_GROW                        0x2

/* BBH Merry-Go-Round */
    /* oAction */
    #define BBH_MERRY_GO_ROUND_ACT_SPAWN_BOOS               0x0
    #define BBH_MERRY_GO_ROUND_ACT_WAIT                     0x1
    #define BBH_MERRY_GO_ROUND_ACT_STOPPED                  0x2
    /* gMarioCurrentRoom */
    #define BBH_NEAR_MERRY_GO_ROUND_ROOM                    0xA
    #define BBH_DYNAMIC_SURFACE_ROOM                        0x0
    #define BBH_OUTSIDE_ROOM                                0xD

/* Coffin Spawner */
    /* oAction */
    #define COFFIN_SPAWNER_ACT_COFFINS_UNLOADED             0x0
    #define COFFIN_SPAWNER_ACT_COFFINS_LOADED               0x1

/* Coffin */
    /* oBehParams2ndByte */
    #define COFFIN_BP_STATIONARY                            0x0
    #define COFFIN_BP_MOVING                                0x1
    /* oAction */
    #define COFFIN_ACT_IDLE                                 0x0
    #define COFFIN_ACT_STAND_UP                             0x1

/* WDW Arrow Lift */
    /* oAction */
    #define ARROW_LIFT_ACT_IDLE                             0x0
    #define ARROW_LIFT_ACT_MOVING_AWAY                      0x1
    #define ARROW_LIFT_ACT_MOVING_BACK                      0x2

/* Toad */
    /* Animations */
    #define TOAD_ANIM_0                                     0x0 //! anim name
    #define TOAD_ANIM_6                                     0x6 //! anim name

/* Intro Peach */
    /* oAction */
    #define PEACH_ACT_INIT                                  0x0
    #define PEACH_ACT_FADE_1                                0x1
    #define PEACH_ACT_UNFADE                                0x2
    #define PEACH_ACT_FADE_2                                0x3
    /* Animations */
    #define PEACH_ANIM_0                                    0x0 //! anim name

/* Yoshi */

/* Yoshi */
    /* oAction */
    #define YOSHI_ACT_IDLE                                  0x0
    #define YOSHI_ACT_WALK                                  0x1
    #define YOSHI_ACT_TALK                                  0x2
    #define YOSHI_ACT_WALK_JUMP_OFF_ROOF                    0x3
    #define YOSHI_ACT_FINISH_JUMPING_AND_DESPAWN            0x4
    #define YOSHI_ACT_GIVE_PRESENT                          0x5
    #define YOSHI_ACT_CREDITS                               0xA
    /* Animations */
    #define YOSHI_ANIM_IDLE                                 0x0
    #define YOSHI_ANIM_WALK                                 0x1
    #define YOSHI_ANIM_JUMP                                 0x2

/* Koopa (General) */
    /* oBehParams2ndByte */
    #define KOOPA_BP_UNSHELLED                              0x0
    #define KOOPA_BP_NORMAL                                 0x1
    #define KOOPA_BP_KOOPA_THE_QUICK_BASE                   0x2
    #define KOOPA_BP_KOOPA_THE_QUICK_BOB                    (KOOPA_BP_KOOPA_THE_QUICK_BASE + KOOPA_THE_QUICK_BOB_INDEX)
    #define KOOPA_BP_KOOPA_THE_QUICK_THI                    (KOOPA_BP_KOOPA_THE_QUICK_BASE + KOOPA_THE_QUICK_THI_INDEX)
    #define KOOPA_BP_TINY                                   0x4
    /* oAnimState */
    #define KOOPA_ANIM_STATE_EYES_OPEN                      OBJ_BLINKING_ANIM_STATE_EYES_OPEN
    #define KOOPA_ANIM_STATE_EYES_CLOSED                    OBJ_BLINKING_ANIM_STATE_EYES_CLOSED
    /* Animations */
    #define KOOPA_ANIM_SHELLED_UNUSED3                      0x0
    #define KOOPA_ANIM_SHELLED_RUN_AWAY                     0x1
    #define KOOPA_ANIM_UNSHELLED_LYING                      0x2
    #define KOOPA_ANIM_UNSHELLED_RUN                        0x3
    #define KOOPA_ANIM_UNUSED_4                             0x4
    #define KOOPA_ANIM_SHELLED_LYING                        0x5
    #define KOOPA_ANIM_STAND_UP                             0x6
    #define KOOPA_ANIM_STOPPED                              0x7
    #define KOOPA_ANIM_UNUSED_8                             0x8
    #define KOOPA_ANIM_WALK                                 0x9
    #define KOOPA_ANIM_SHELLED_WALK_STOP                    0xA
    #define KOOPA_ANIM_WALK_START                           0xB
    #define KOOPA_ANIM_THE_QUICK_JUMP                       0xC
    #define KOOPA_ANIM_THE_QUICK_LAND                       0xD

/* Unshelled Koopa */
    /* oAction */
    #define KOOPA_UNSHELLED_ACT_RUN                         0x0
    #define KOOPA_UNSHELLED_ACT_DIVE                        0x1
    #define KOOPA_UNSHELLED_ACT_LYING                       0x2
    // #define KOOPA_UNSHELLED_ACT_UNUSED3                     0x3

/* Shelled Koopa */
    /* oAction */
    #define KOOPA_SHELLED_ACT_STOPPED                       0x0
    #define KOOPA_SHELLED_ACT_WALK                          0x1
    #define KOOPA_SHELLED_ACT_RUN_FROM_MARIO                0x2
    #define KOOPA_SHELLED_ACT_LYING                         0x3
    #define KOOPA_SHELLED_ACT_DIE                           0x4
    /* oSubAction */
        /* KOOPA_SHELLED_ACT_WALK */
    #define KOOPA_SHELLED_SUB_ACT_START_WALK                0x0
    #define KOOPA_SHELLED_SUB_ACT_WALK                      0x1
    #define KOOPA_SHELLED_SUB_ACT_STOP_WALK                 0x2

/* Koopa The Quick */
    /* oAction */
    #define KOOPA_THE_QUICK_ACT_WAIT_BEFORE_RACE            0x0
    #define KOOPA_THE_QUICK_ACT_UNUSED1                     0x1
    #define KOOPA_THE_QUICK_ACT_SHOW_INIT_TEXT              0x2
    #define KOOPA_THE_QUICK_ACT_RACE                        0x3
    #define KOOPA_THE_QUICK_ACT_DECELERATE                  0x4
    #define KOOPA_THE_QUICK_ACT_STOP                        0x5
    #define KOOPA_THE_QUICK_ACT_AFTER_RACE                  0x6
    /* oSubAction */
        /* KOOPA_THE_QUICK_ACT_RACE */
    #define KOOPA_THE_QUICK_SUB_ACT_START_RUN               0x0
    #define KOOPA_THE_QUICK_SUB_ACT_RUN                     0x1
    #define KOOPA_THE_QUICK_SUB_ACT_JUMP                    0x2
    /* oKoopaTheQuickRaceIndex */
    #define KOOPA_THE_QUICK_BOB_INDEX                       0x0
    #define KOOPA_THE_QUICK_THI_INDEX                       0x1

/* Koopa Flag */
    /* Animations */
    #define KOOPA_FLAG_ANIM_WAVE                            0x0

/* Pokey */
    /* oAction */
    #define POKEY_ACT_UNINITIALIZED                         0x0
    #define POKEY_ACT_WANDER                                0x1
    #define POKEY_ACT_UNLOAD_PARTS                          0x2
    /* oAnimState */
    #define POKEY_ANIM_STATE_NONSTANDARD_ACTION             0x1

/* Pokey Body Part */
    /* oBehParams2ndByte */
    #define POKEY_PART_BP_HEAD                              0x0
    #define POKEY_PART_BP_LOWEST                            (POKEY_NUM_SEGMENTS - 0x1)

/* Swoop */
    /* oAction */
    #define SWOOP_ACT_IDLE                                  0x0
    #define SWOOP_ACT_MOVE                                  0x1
    /* Animations */
    #define SWOOP_ANIM_FLY                                  0x0
    #define SWOOP_ANIM_IDLE                                 0x1

/* Fly guy */
    /* oBehParams2ndByte */
    #define FLY_GUY_BP_LUNGES                               0x0
    #define FLY_GUY_BP_SHOOTS_FIRE                          0x1
    /* oAction */
    #define FLY_GUY_ACT_IDLE                                0x0
    #define FLY_GUY_ACT_APPROACH_MARIO                      0x1
    #define FLY_GUY_ACT_LUNGE                               0x2
    #define FLY_GUY_ACT_SHOOT_FIRE                          0x3
    /* Animations */
    #define FLY_GUY_ANIM_FLYING                             0x0

/* Goomba triplet spawner */
    /* oBehParams2ndByte */
    #define GOOMBA_TRIPLET_SPAWNER_BP_SIZE_MASK             0x00000003
    #define GOOMBA_TRIPLET_SPAWNER_BP_EXTRA_GOOMBAS_MASK    0x000000FC
    /* oAction */
    #define GOOMBA_TRIPLET_SPAWNER_ACT_UNLOADED             0x0
    #define GOOMBA_TRIPLET_SPAWNER_ACT_LOADED               0x1

/* Goomba */
    /* oBehParams2ndByte */
    #define GOOMBA_SIZE_REGULAR                             0x0
    #define GOOMBA_SIZE_HUGE                                0x1
    #define GOOMBA_SIZE_TINY                                0x2
    #define GOOMBA_BP_SIZE_MASK                             0x3
    #define GOOMBA_BP_TRIPLET_FLAG_MASK                     0x000000FC
    /* oAction */
    #define GOOMBA_ACT_WALK                                 0x0
    #define GOOMBA_ACT_ATTACKED_MARIO                       0x1
    #define GOOMBA_ACT_JUMP                                 0x2
    /* oAnimState */
    #define GOOMBA_ANIM_STATE_EYES_OPEN                     OBJ_BLINKING_ANIM_STATE_EYES_OPEN
    #define GOOMBA_ANIM_STATE_EYES_CLOSED                   OBJ_BLINKING_ANIM_STATE_EYES_CLOSED
    /* Animations */
    #define GOOMBA_ANIM_DEFAULT                             0x0

/* Chain chomp */
    /* oAction */
    #define CHAIN_CHOMP_ACT_UNINITIALIZED                   0x0
    #define CHAIN_CHOMP_ACT_MOVE                            0x1
    #define CHAIN_CHOMP_ACT_UNLOAD_CHAIN                    0x2
    /* oSubAction */
        /* CHAIN_CHOMP_ACT_MOVE */
    #define CHAIN_CHOMP_SUB_ACT_TURN                        0x0
    #define CHAIN_CHOMP_SUB_ACT_LUNGE                       0x1
    /* oChainChompReleaseStatus */
    #define CHAIN_CHOMP_NOT_RELEASED                        0x0
    #define CHAIN_CHOMP_RELEASED_TRIGGER_CUTSCENE           0x1
    #define CHAIN_CHOMP_RELEASED_LUNGE_AROUND               0x2
    #define CHAIN_CHOMP_RELEASED_BREAK_GATE                 0x3
    #define CHAIN_CHOMP_RELEASED_JUMP_AWAY                  0x4
    #define CHAIN_CHOMP_RELEASED_END_CUTSCENE               0x5
    /* Animations */
    #define CHAIN_CHOMP_ANIM_CHOMPING                       0x0

/* Chain chomp chain part */
    /* oBehParams2ndByte */
    #define CHAIN_CHOMP_CHAIN_PART_BP_PIVOT                 0x0

/* Wooden post */
    /* oBehParams */
    #define WOODEN_POST_BP_NO_COINS_MASK                    0x0000FF00

/* Wiggler */
    /* oAction */
    #define WIGGLER_ACT_UNINITIALIZED                       0x0
    #define WIGGLER_ACT_WALK                                0x1
    #define WIGGLER_ACT_KNOCKBACK                           0x2
    #define WIGGLER_ACT_JUMPED_ON                           0x3
    #define WIGGLER_ACT_SHRINK                              0x4
    #define WIGGLER_ACT_FALL_THROUGH_FLOOR                  0x5
    /* oWigglerTextStatus */
    #define WIGGLER_TEXT_STATUS_AWAIT_DIALOG                0x0
    #define WIGGLER_TEXT_STATUS_SHOWING_DIALOG              0x1
    #define WIGGLER_TEXT_STATUS_COMPLETED_DIALOG            0x2
    /* Animations */
    #define WIGGLER_ANIM_WALK                               0x0

/* Spiny */
    /* oAction */
    #define SPINY_ACT_WALK                                  0x0
    #define SPINY_ACT_HELD_BY_LAKITU                        0x1
    #define SPINY_ACT_THROWN_BY_LAKITU                      0x2
    #define SPINY_ACT_ATTACKED_MARIO                        0x3
    /* Animations */
    #define SPINY_ANIM_DEFAULT                              0x0

/* Enemy lakitu */
    /* oAction */
    #define ENEMY_LAKITU_ACT_UNINITIALIZED                  0x0
    #define ENEMY_LAKITU_ACT_MAIN                           0x1
    /* oSubAction */
        /* ENEMY_LAKITU_ACT_MAIN */
    #define ENEMY_LAKITU_SUB_ACT_NO_SPINY                   0x0
    #define ENEMY_LAKITU_SUB_ACT_HOLD_SPINY                 0x1
    #define ENEMY_LAKITU_SUB_ACT_THROW_SPINY                0x2
    /* Animations */
    #define ENEMY_LAKITU_ANIM_SPAWN                         0x0
    #define ENEMY_LAKITU_ANIM_NO_SPINY                      0x1
    #define ENEMY_LAKITU_ANIM_THROW_SPINY                   0x2
    #define ENEMY_LAKITU_ANIM_HOLD_SPINY                    0x3

/* Cloud */
    /* oBehParams2ndByte */
    #define CLOUD_BP_FWOOSH                                 0x0
    #define CLOUD_BP_LAKITU_CLOUD                           0x1
    #define CLOUD_BP_FWOOSH_FACE                            0x5
    /* oAction */
    #define CLOUD_ACT_SPAWN_PARTS                           0x0
    #define CLOUD_ACT_MAIN                                  0x1
    #define CLOUD_ACT_UNLOAD                                0x2
    #define CLOUD_ACT_FWOOSH_HIDDEN                         0x3

/* Intro Lakitu */
    /* oAction */
    #define INTRO_LAKITU_ACT_INIT                           0x0
    #define INTRO_LAKITU_ACT_CUTSCENE_INTRO_1               0x1
    #define INTRO_LAKITU_ACT_CUTSCENE_INTRO_2               0x2
    #define INTRO_LAKITU_ACT_CUTSCENE_INTRO_3               0x3
    #define INTRO_LAKITU_ACT_CUTSCENE_END_WAVING_1         0x64
    #define INTRO_LAKITU_ACT_CUTSCENE_END_WAVING_2         0x65
    #define INTRO_LAKITU_ACT_CUTSCENE_END_WAVING_3         0x66
    /* Animations */
    #define INTRO_LAKITU_ACT_DEFAULT                        0x0

/* Camera Lakitu */
    /* oBehParams2ndByte */
    #define CAMERA_LAKITU_BP_FOLLOW_CAMERA                  0x0
    #define CAMERA_LAKITU_BP_INTRO                          0x1
    /* oAction */
    #define CAMERA_LAKITU_INTRO_ACT_TRIGGER_CUTSCENE        0x0
    #define CAMERA_LAKITU_INTRO_ACT_SPAWN_CLOUD             0x1
    #define CAMERA_LAKITU_INTRO_ACT_SHOW_DIALOG             0x2
    /* Animations */
    #define CAMERA_LAKITU_ANIM_DEFAULT                      0x0

/* RR Cruiser Wing */
    /* oBehParams2ndByte */
    #define CRUISER_WING_BP_CLOCKWISE                       0x0
    #define CRUISER_WING_BP_COUNTERCLOCKWISE                0x1

/* Manta Ray */
    /* oAction */
    #define MANTA_ACT_SPAWN_RINGS                           0x0
    #define MANTA_ACT_NO_RINGS                              0x1
    /* Animations */
    #define MANTA_ANIM_SWIM                                 0x0

/* Sushi Shark */
    /* Animations */
    #define SUSHI_ANIM_SWIM                                 0x0

/* Monty mole */
    /* oBehParams2ndByte */
    #define MONTY_MOLE_BP_NO_ROCK                           0x0
    /* oAction */
    #define MONTY_MOLE_ACT_SELECT_HOLE                      0x0
    #define MONTY_MOLE_ACT_RISE_FROM_HOLE                   0x1
    #define MONTY_MOLE_ACT_SPAWN_ROCK                       0x2
    #define MONTY_MOLE_ACT_BEGIN_JUMP_INTO_HOLE             0x3
    #define MONTY_MOLE_ACT_THROW_ROCK                       0x4
    #define MONTY_MOLE_ACT_JUMP_INTO_HOLE                   0x5
    #define MONTY_MOLE_ACT_HIDE                             0x6
    #define MONTY_MOLE_ACT_JUMP_OUT_OF_HOLE                 0x7
    /* Animations */
    #define MONTY_MOLE_ANIM_JUMP_INTO_HOLE                  0x0
    #define MONTY_MOLE_ANIM_RISE                            0x1
    #define MONTY_MOLE_ANIM_GET_ROCK                        0x2
    #define MONTY_MOLE_ANIM_BEGIN_JUMP_INTO_HOLE            0x3
    #define MONTY_MOLE_ANIM_JUMP_OUT_OF_HOLE_DOWN           0x4
    #define MONTY_MOLE_ANIM_UNUSED_5                        0x5
    #define MONTY_MOLE_ANIM_UNUSED_6                        0x6
    #define MONTY_MOLE_ANIM_UNUSED_7                        0x7
    #define MONTY_MOLE_ANIM_THROW_ROCK                      0x8
    #define MONTY_MOLE_ANIM_JUMP_OUT_OF_HOLE_UP             0x9

/* Monty mole rock */
    /* oAction */
    #define MONTY_MOLE_ROCK_ACT_HELD                        0x0
    #define MONTY_MOLE_ROCK_ACT_MOVE                        0x1

/* Ukiki */
    /* oBehParams2ndByte */
    #define UKIKI_CAGE                                      0x0
    #define UKIKI_CAP                                       0x1
    /* oAction */
    #define UKIKI_ACT_IDLE                                  0x0
    #define UKIKI_ACT_RUN                                   0x1
    #define UKIKI_ACT_TURN_TO_MARIO                         0x2
    #define UKIKI_ACT_JUMP                                  0x3
    #define UKIKI_ACT_GO_TO_CAGE                            0x4
    #define UKIKI_ACT_WAIT_TO_RESPAWN                       0x5
    #define UKIKI_ACT_UNUSED_TURN                           0x6
    #define UKIKI_ACT_RETURN_HOME                           0x7
    /* oSubAction */
        /* UKIKI_ACT_IDLE, UKIKI_ACT_WAIT_TO_RESPAWN, UKIKI_ACT_UNUSED_TURN */
    #define UKIKI_SUB_ACT_TAUNT_NONE                        0x0
    #define UKIKI_SUB_ACT_TAUNT_ITCH                        0x1
    #define UKIKI_SUB_ACT_TAUNT_SCREECH                     0x2
    #define UKIKI_SUB_ACT_TAUNT_JUMP_CLAP                   0x3
    #define UKIKI_SUB_ACT_TAUNT_HANDSTAND                   0x4
        /* UKIKI_ACT_JUMP */
    #define UKIKI_SUB_ACT_JUMP_JUMP                         0x0
    #define UKIKI_SUB_ACT_JUMP_LAND                         0x1
        /* UKIKI_ACT_GO_TO_CAGE */
    #define UKIKI_SUB_ACT_CAGE_RUN_TO_CAGE                  0x0
    #define UKIKI_SUB_ACT_CAGE_WAIT_FOR_MARIO               0x1
    #define UKIKI_SUB_ACT_CAGE_TALK_TO_MARIO                0x2
    #define UKIKI_SUB_ACT_CAGE_TURN_TO_CAGE                 0x3
    #define UKIKI_SUB_ACT_CAGE_JUMP_TO_CAGE                 0x4
    #define UKIKI_SUB_ACT_CAGE_LAND_ON_CAGE                 0x5
    #define UKIKI_SUB_ACT_CAGE_SPIN_ON_CAGE                 0x6
    #define UKIKI_SUB_ACT_CAGE_DESPAWN                      0x7
    /* oUkikiTextState */
    #define UKIKI_TEXT_DEFAULT                              0x0
    #define UKIKI_TEXT_CAGE_TEXTBOX                         0x1
    #define UKIKI_TEXT_GO_TO_CAGE                           0x2
    #define UKIKI_TEXT_STOLE_CAP                            0x3
    #define UKIKI_TEXT_HAS_CAP                              0x4
    #define UKIKI_TEXT_GAVE_CAP_BACK                        0x5
    #define UKIKI_TEXT_DO_NOT_LET_GO                        0x6
    #define UKIKI_TEXT_STEAL_CAP                            0x7
    /* Animations */
    #define UKIKI_ANIM_RUN                                  0x0
    #define UKIKI_ANIM_UNUSED_WALK                          0x1
    #define UKIKI_ANIM_UNUSED_APOSE                         0x2
    #define UKIKI_ANIM_UNUSED_DEATH                         0x3
    #define UKIKI_ANIM_SCREECH                              0x4
    #define UKIKI_ANIM_JUMP_CLAP                            0x5
    #define UKIKI_ANIM_UNUSED_HOP                           0x6
    #define UKIKI_ANIM_LAND                                 0x7
    #define UKIKI_ANIM_JUMP                                 0x8
    #define UKIKI_ANIM_ITCH                                 0x9
    #define UKIKI_ANIM_HANDSTAND                            0xA
    #define UKIKI_ANIM_TURN                                 0xB
    #define UKIKI_ANIM_HELD                                 0xC
    /* oAnimState */
    #define UKIKI_ANIM_STATE_DEFAULT                        0x0
    #define UKIKI_ANIM_STATE_EYE_CLOSED                     0x1
    #define UKIKI_ANIM_STATE_CAP_ON                         0x2
    /* oUkikiHasCap */
    #define UKIKI_CAP_ON                                    0x1 //!

/* Ukiki Cage Star */
    /* oAction */
    #define UKIKI_CAGE_STAR_ACT_IN_CAGE                     0x0
    #define UKIKI_CAGE_STAR_ACT_SPAWN_STAR                  0x1

/* Ukiki Cage */
    /* oAction */
    #define UKIKI_CAGE_ACT_WAIT_FOR_UKIKI                   0x0
    #define UKIKI_CAGE_ACT_SPIN                             0x1
    #define UKIKI_CAGE_ACT_FALL                             0x2
    #define UKIKI_CAGE_ACT_HIDE                             0x3

/* Piranha Plant */
    /* oAction */
    #define PIRANHA_PLANT_ACT_IDLE                          0x0
    #define PIRANHA_PLANT_ACT_SLEEPING                      0x1
    #define PIRANHA_PLANT_ACT_BITING                        0x2
    #define PIRANHA_PLANT_ACT_WOKEN_UP                      0x3
    #define PIRANHA_PLANT_ACT_STOPPED_BITING                0x4
    #define PIRANHA_PLANT_ACT_ATTACKED                      0x5
    #define PIRANHA_PLANT_ACT_SHRINK_AND_DIE                0x6
    #define PIRANHA_PLANT_ACT_WAIT_TO_RESPAWN               0x7
    #define PIRANHA_PLANT_ACT_RESPAWN                       0x8
    /* Animations */
    #define PIRANHA_PLANT_ANIM_BITE                         0x0
    #define PIRANHA_PLANT_ANIM_UNUSED_1                     0x1
    #define PIRANHA_PLANT_ANIM_FALLING_OVER                 0x2
    #define PIRANHA_PLANT_ANIM_UNUSED_3                     0x3
    #define PIRANHA_PLANT_ANIM_UNUSED_4                     0x4
    #define PIRANHA_PLANT_ANIM_UNUSED_5                     0x5
    #define PIRANHA_PLANT_ANIM_STOP_BITING                  0x6
    #define PIRANHA_PLANT_ANIM_UNUSED_7                     0x7
    #define PIRANHA_PLANT_ANIM_SLEEPING                     0x8

/* Piranha Plant Bubble */
    /* oAction */
    #define PIRANHA_PLANT_BUBBLE_ACT_IDLE                   0x0
    #define PIRANHA_PLANT_BUBBLE_ACT_GROW_SHRINK_LOOP       0x1
    #define PIRANHA_PLANT_BUBBLE_ACT_BURST                  0x2

/* Checkerboard Platform Group */
    /* oBehParams2ndByte */
    #define CHECKERBOARD_PLATFORM_GROUP_BP_SET_DEFAULT      0x00
    #define CHECKERBOARD_PLATFORM_GROUP_BP_DEFAULT_MAX      0x41

/* Checkerboard Platform */
    /* oBehParams2ndByte */
    #define CHECKERBOARD_PLATFORM_BP_MOVE_UP                0x0
    #define CHECKERBOARD_PLATFORM_BP_MOVE_DOWN              0x1
    /* oAction */
    #define CHECKERBOARD_PLATFORM_ACT_MOVE_VERTICALLY       0x0
    #define CHECKERBOARD_PLATFORM_ACT_MOVE_UP               0x1
    #define CHECKERBOARD_PLATFORM_ACT_ROTATE_UP             0x2
    #define CHECKERBOARD_PLATFORM_ACT_MOVE_DOWN             0x3
    #define CHECKERBOARD_PLATFORM_ACT_ROTATE_DOWN           0x4

/* Platform on track */
    /* oAction */
    #define PLATFORM_ON_TRACK_ACT_INIT                      0x0
    #define PLATFORM_ON_TRACK_ACT_WAIT_FOR_MARIO            0x1
    #define PLATFORM_ON_TRACK_ACT_MOVE_ALONG_TRACK          0x2
    #define PLATFORM_ON_TRACK_ACT_PAUSE_BRIEFLY             0x3
    #define PLATFORM_ON_TRACK_ACT_FALL                      0x4
    /* oBehParams >> 16 */
    #define PLATFORM_ON_TRACK_BP_MASK_PATH                  0xF
    #define PLATFORM_ON_TRACK_BP_MASK_TYPE                  (0x7 << 4)
    #define PLATFORM_ON_TRACK_BP_RETURN_TO_START            (1 <<  8)
    #define PLATFORM_ON_TRACK_BP_DONT_DISAPPEAR             (1 <<  9)
    #define PLATFORM_ON_TRACK_BP_DONT_TURN_YAW              (1 << 10)
    #define PLATFORM_ON_TRACK_BP_DONT_TURN_ROLL             (1 << 11)
    /* oPlatformOnTrackType */
    #define PLATFORM_ON_TRACK_TYPE_CARPET                   0x0
    #define PLATFORM_ON_TRACK_TYPE_SKI_LIFT                 0x1
    #define PLATFORM_ON_TRACK_TYPE_CHECKERED                0x2
    #define PLATFORM_ON_TRACK_TYPE_GRATE                    0x3

/* HMC Controllable Platform */
    /* oBehParams2ndByte / sControllablePlatformDirectionState */
    #define DIRECTION_STATE_STOPPED                         0x0
    #define DIRECTION_STATE_SOUTH                           0x1
    #define DIRECTION_STATE_NORTH                           0x2
    #define DIRECTION_STATE_EAST                            0x3
    #define DIRECTION_STATE_WEST                            0x4
    #define DIRECTION_STATE_HIT_WALL                        0x5
    #define DIRECTION_STATE_DISAPPEARING                    0x6
    /* oControllablePlatformWallHitDirection */
    #define MOVE_DIRECTION_NORTH                            0x1
    #define MOVE_DIRECTION_SOUTH                            0x2
    #define MOVE_DIRECTION_WEST                             0x3
    #define MOVE_DIRECTION_EAST                             0x4

/* HMC Controllable Platform Button */
    /* oAction */
    #define CONTROLLABLE_PLATFORM_ACT_UNPRESSED             0x0
    #define CONTROLLABLE_PLATFORM_BUTTON_ACT_PRESSED        0x1
    #define CONTROLLABLE_PLATFORM_BUTTON_ACT_CHECK_UNPRESS  0x2

/* Elevator */
    /* oBehParams2ndByte */
    #define ELEVATOR_BP_HMC_WORK                            0x0
    #define ELEVATOR_BP_HMC_EMERGENCY_EXIT                  0x1
    #define ELEVATOR_BP_HMC_NAVIGATING_THE_TOXIC_MAZE       0x2
    #define ELEVATOR_BP_HMC_LAKE                            0x3
    #define ELEVATOR_BP_BBH_MESH                            0x4
    #define ELEVATOR_BP_RAINBOW_RIDE                        0x5
    #define ELEVATOR_BP_UNUSED                              0x6
    /* oAction */
    #define ELEVATOR_ACT_IDLE                               0x0
    #define ELEVATOR_ACT_MOVING_UP                          0x1
    #define ELEVATOR_ACT_MOVING_DOWN                        0x2
    #define ELEVATOR_ACT_LANDED                             0x3
    #define ELEVATOR_ACT_LANDED_RR                          0x4
    /* oElevatorType */
    #define ELEVATOR_TYPE_DEFAULT                           0x0
    #define ELEVATOR_TYPE_RR                                0x1
    #define ELEVATOR_TYPE_ABOVE_HOME                        0x2 // roomless?

/* WDW Express Elevator */
    /* oAction */
    #define EXPRESS_ELEVATOR_ACT_IDLE                       0x0
    #define EXPRESS_ELEVATOR_ACT_MOVING_DOWN                0x1
    #define EXPRESS_ELEVATOR_ACT_PAUSING_AT_BOTTOM          0x2
    #define EXPRESS_ELEVATOR_ACT_MOVING_UP                  0x3
    #define EXPRESS_ELEVATOR_ACT_DONE                       0x4

/* Pyramid elevator */
    /* oAction */
    #define PYRAMID_ELEVATOR_IDLE                           0x0
    #define PYRAMID_ELEVATOR_START_MOVING                   0x1
    #define PYRAMID_ELEVATOR_CONSTANT_VELOCITY              0x2
    #define PYRAMID_ELEVATOR_AT_BOTTOM                      0x3

/* Pyramid top */
    /* oAction */
    #define PYRAMID_TOP_ACT_CHECK_IF_SOLVED                 0x0
    #define PYRAMID_TOP_ACT_SPINNING                        0x1
    #define PYRAMID_TOP_ACT_EXPLODE                         0x2

/* Pyramid wall */
    /* oBehParams2ndByte */
    #define PYRAMID_WALL_BP_POSITION_HIGH                   0x0
    #define PYRAMID_WALL_BP_POSITION_MIDDLE                 0x1
    #define PYRAMID_WALL_BP_POSITION_LOW                    0x2
    /* oAction */
    #define PYRAMID_WALL_ACT_MOVING_DOWN                    0x0
    #define PYRAMID_WALL_ACT_MOVING_UP                      0x1

/* Tox Box */
    /* oBehParams2ndByte */
    #define TOX_BOX_BP_PATTERN_1                            0x0
    #define TOX_BOX_BP_PATTERN_2                            0x1
    #define TOX_BOX_BP_PATTERN_3                            0x2
    /* oAction */
    #define TOX_BOX_ACT_END                                -0x1
    #define TOX_BOX_ACT_INIT                                0x0
    #define TOX_BOX_ACT_STEP                                0x1
    #define TOX_BOX_ACT_WAIT                                0x2
    #define TOX_BOX_ACT_MOVE_FORWARD                        0x3
    #define TOX_BOX_ACT_MOVE_BACKWARD                       0x4
    #define TOX_BOX_ACT_MOVE_DOWN                           0x5
    #define TOX_BOX_ACT_MOVE_UP                             0x6

/* Penguins (general) */
    /* Walking sounds */
    #define PENGUIN_SOUND_WALK_BABY                         0x0
    #define PENGUIN_SOUND_WALK_BIG                          0x1
    /* geo_switch_tuxie_mother_eyes */
    #define PENGUIN_ANIM_STATE_EYES_OPEN                    0x0
    #define PENGUIN_ANIM_STATE_EYES_HALF_CLOSED             0x1
    #define PENGUIN_ANIM_STATE_EYES_CLOSED                  0x2
    #define PENGUIN_ANIM_STATE_EYES_ANGRY                   0x3
    #define PENGUIN_ANIM_STATE_EYES_SAD                     0x4
    /* Animations */
    #define PENGUIN_ANIM_WALK                               0x0
    #define PENGUIN_ANIM_DIVE_SLIDE                         0x1
    #define PENGUIN_ANIM_STAND_UP                           0x2
    #define PENGUIN_ANIM_IDLE                               0x3

/* Racing Penguin */
    /* oBehParams2ndByte */
    #define RACING_PENGUIN_BP_NORMAL                        0x0
    #define RACING_PENGUIN_BP_120_STARS                     0x1
    /* oAction */
    #define RACING_PENGUIN_ACT_WAIT_FOR_MARIO               0x0
    #define RACING_PENGUIN_ACT_SHOW_INIT_TEXT               0x1
    #define RACING_PENGUIN_ACT_PREPARE_FOR_RACE             0x2
    #define RACING_PENGUIN_ACT_RACE                         0x3
    #define RACING_PENGUIN_ACT_FINISH_RACE                  0x4
    #define RACING_PENGUIN_ACT_SHOW_FINAL_TEXT              0x5

/* Mother Penguin */
    /* oAction */
    #define MOTHER_PENGUIN_ACT_IDLE                         0x0
    #define MOTHER_PENGUIN_ACT_RECEIVE_BABY                 0x1
    #define MOTHER_PENGUIN_ACT_RECEIVED_BABY                0x2
    /* oSubAction */
        /* MOTHER_PENGUIN_ACT_IDLE */
    #define MOTHER_PENGUIN_SUB_ACT_READY_TO_ASK             0x0
    #define MOTHER_PENGUIN_SUB_ACT_ASK_FOR_BABY             0x1
    #define MOTHER_PENGUIN_SUB_ACT_ALREADY_ASKED            0x2
        /* MOTHER_PENGUIN_ACT_RECEIVE_BABY */
    #define MOTHER_PENGUIN_SUB_ACT_RECEIVE_BABY             0x0
    #define MOTHER_PENGUIN_SUB_ACT_CORRECT_BABY             0x1
    #define MOTHER_PENGUIN_SUB_ACT_WRONG_BABY               0x2
        /* MOTHER_PENGUIN_ACT_RECEIVED_BABY */
    #define MOTHER_PENGUIN_SUB_ACT_CHASE_MARIO              0x0
    #define MOTHER_PENGUIN_SUB_ACT_STOP_CHASING_MARIO       0x1

/* Small Penguin */
    /* oAction */
    #define SMALL_PENGUIN_ACT_WALKING                       0x0
    #define SMALL_PENGUIN_ACT_WALKING_TOWARD_MARIO          0x1
    #define SMALL_PENGUIN_ACT_WALKING_AWAY_FROM_MARIO       0x2
    #define SMALL_PENGUIN_ACT_DIVE_SLIDING                  0x3
    #define SMALL_PENGUIN_ACT_DIVE_SLIDING_STOP             0x4
    #define SMALL_PENGUIN_ACT_NEAR_MOTHER                   0x5

/* SL walking Penguin */
    /* oAction */
    #define SL_WALKING_PENGUIN_ACT_MOVING_FORWARDS          0x0
    #define SL_WALKING_PENGUIN_ACT_TURNING_BACK             0x1
    #define SL_WALKING_PENGUIN_ACT_RETURNING                0x2
    #define SL_WALKING_PENGUIN_ACT_TURNING_FORWARDS         0x3

/* Snowman wind */
    /* oSubAction */
    #define SL_SNOWMAN_WIND_ACT_IDLE                        0x0
    #define SL_SNOWMAN_WIND_ACT_TALKING                     0x1
    #define SL_SNOWMAN_WIND_ACT_BLOWING                     0x2

/* Snow Mound */
    /* oAction */
    #define SNOW_MOUND_ACT_MOVE                             0x0
    #define SNOW_MOUND_ACT_SINK                             0x1

/* Snowman Head */
    /* oAction */
    #define SNOWMANS_HEAD_ACT_ASK                           0x0
    #define SNOWMANS_HEAD_ACT_NONE                          0x1
    #define SNOWMANS_HEAD_ACT_JUMPING                       0x2
    #define SNOWMANS_HEAD_ACT_LAND                          0x3
    #define SNOWMANS_HEAD_ACT_THANK                         0x4

/* Snowman Bottom */
    /* oAction */
    #define SNOWMANS_BOTTOM_ACT_WAITING                     0x0
    #define SNOWMANS_BOTTOM_ACT_FOLLOW_PATH                 0x1
    #define SNOWMANS_BOTTOM_ACT_FINAL_STRETCH               0x2
    #define SNOWMANS_BOTTOM_ACT_REACH_END                   0x3
    #define SNOWMANS_BOTTOM_ACT_COLLISION                   0x4

/* Water Bomb Cannon */
    /* oBehParams2ndByte */
    #define WATER_BOMB_CANNON_BP_ACTIVE                     0x0
    /* oAction */
    #define WATER_BOMB_CANNON_ACT_HIDDEN                    0x0
    #define WATER_BOMB_CANNON_ACT_ACTIVE                    0x1
    #define WATER_BOMB_CANNON_ACT_HIDE                      0x2

/* Water bomb */
    /* oAction */
    #define WATER_BOMB_ACT_SHOT_FROM_CANNON                 0x0
    #define WATER_BOMB_ACT_INIT                             0x1
    #define WATER_BOMB_ACT_DROP                             0x2
    #define WATER_BOMB_ACT_EXPLODE                          0x3

/* TTC Painting Clock Arm */
    /* oAction */
    #define CLOCK_ARM_ACT_CHECK_DEFAULT                     0x0
    #define CLOCK_ARM_ACT_MOVING                            0x1
    #define CLOCK_ARM_ACT_STOP                              0x2

/* TTC rotating solid */
    /* oBehParams2ndByte */
    #define TTC_ROTATING_SOLID_BP_CUBE                      0x0
    #define TTC_ROTATING_SOLID_BP_TRIANGULAR_PRISM          0x1

/* TTC moving bar */
    /* oAction */
    #define TTC_MOVING_BAR_ACT_WAIT                         0x0
    #define TTC_MOVING_BAR_ACT_PULL_BACK                    0x1
    #define TTC_MOVING_BAR_ACT_EXTEND                       0x2
    #define TTC_MOVING_BAR_ACT_RETRACT                      0x3

/* TTC cog */
    /* oBehParams2ndByte */
    #define TTC_COG_BP_SHAPE_MASK                           0x00000002
    #define TTC_COG_BP_SHAPE_HEXAGON                        (0 << 1)
    #define TTC_COG_BP_SHAPE_TRIANGLE                       (1 << 1)
    #define TTC_COG_BP_DIR_MASK                             0x00000001
    #define TTC_COG_BP_DIR_CCW                              (0 << 0) // TODO: Check these
    #define TTC_COG_BP_DIR_CW                               (1 << 0)

/* TTC 2D Rotator */
    /* oBehParams2ndByte */
    #define TTC_2D_ROTATOR_BP_HAND                          0x0
    #define TTC_2D_ROTATOR_BP_2D_COG                        0x1

/* TTC Treadmill */
    /* oBehParams2ndByte */
    #define TREADMILL_BP_LARGE                              0x0
    #define TREADMILL_BP_SMALL                              0x1
    #define TREADMILL_BP_SIZE_MASK                          0x1

/* Activated Back-and-Forth Platform */
    /* ((u16)(o->oBehParams >> 16) &         0x0300) >> 8 aka platform type */
    #define ACTIVATED_BF_PLAT_TYPE_BITS_ARROW_PLAT          0x0
    #define ACTIVATED_BF_PLAT_TYPE_BITFS_MESH_PLAT          0x1
    #define ACTIVATED_BF_PLAT_TYPE_BITFS_ELEVATOR           0x2

/* Unagi */
    /* oBehParams2ndByte */
    #define UNAGI_BP_IN_SHIP                                0x0
    #define UNAGI_BP_IN_CAVE                                0x1
    #define UNAGI_BP_START_SWIMMING                         0x2
    /* oAction */
    #define UNAGI_ACT_SHIP_RESET_PATH                       0x0
    #define UNAGI_ACT_SHIP_PATH                             0x1
    #define UNAGI_ACT_RETURN_TO_CAVE                        0x2
    #define UNAGI_ACT_IN_CAVE                               0x3
    #define UNAGI_ACT_CAVE_PATH                             0x4
    /* oSubAction */
        /* UNAGI_ACT_SHIP_RESET_PATH */
    #define UNAGI_SUB_ACT_SHIP_RESET_PATH_WAIT_FOR_MARIO    0x0
    #define UNAGI_SUB_ACT_SHIP_RESET_PATH_DO_RESET          0x1
    /* oAnimState */
    #define UNAGI_ANIM_STATE_NO_STAR                        0x0
    #define UNAGI_ANIM_STATE_HAS_STAR                       0x1
    #define UNAGI_ANIM_STATE_HAS_TRANSPARENT_STAR           0x2
    /* Animations */
    #define UNAGI_ANIM_YAWN                                 0x0
    #define UNAGI_ANIM_BITE                                 0x1
    #define UNAGI_ANIM_SWIM                                 0x2
    #define UNAGI_ANIM_STATIC_STRAIGHT                      0x3
    #define UNAGI_ANIM_IDLE_2_1                             0x4
    #define UNAGI_ANIM_OPEN_MOUTH                           0x5
    #define UNAGI_ANIM_IDLE_2                               0x6

/* Unagi Sub Object */
    /* oBehParams2ndByte */
    #define UNAGI_PART_BP_BACK                             -0x4
    #define UNAGI_PART_BP_CONTROL_DISTANCE                  0x3
    #define UNAGI_PART_BP_FRONT                             0x4

/* Dorrie */
    /* oAction */
    #define DORRIE_ACT_MOVE                                 0x0
    #define DORRIE_ACT_LOWER_HEAD                           0x1
    #define DORRIE_ACT_RAISE_HEAD                           0x2
    /* Animations */
    #define DORRIE_ANIM_UNUSED                              0x0
    #define DORRIE_ANIM_RAISE_HEAD                          0x1
    #define DORRIE_ANIM_LOWER_HEAD                          0x2

/* Mad piano */
    /* oAction */
    #define MAD_PIANO_ACT_WAIT                              0x0
    #define MAD_PIANO_ACT_ATTACK                            0x1
    /* Animations */
    #define MAD_PIANO_ANIM_SLEEPING                         0x0
    #define MAD_PIANO_ANIM_CHOMPING                         0x1

/* Haunted Chair */
    /* oAction */
    #define HAUNTED_CHAIR_ACT_FALL_OR_SPIN                  0x0
    #define HAUNTED_CHAIR_ACT_FLY                           0x1
    /* Animations */
    #define HAUNTED_CHAIR_ANIM_DEFAULT                      0x0

/* Fire piranha plant */
    /* oAction */
    #define FIRE_PIRANHA_PLANT_ACT_HIDE                     0x0
    #define FIRE_PIRANHA_PLANT_ACT_GROW                     0x1
    /* Animations */
    #define FIRE_PIRANHA_PLANT_ANIM_SHRINK                  0x0
    #define FIRE_PIRANHA_PLANT_ANIM_UNUSED_1                0x1
    #define FIRE_PIRANHA_PLANT_ANIM_ATTACKED                0x2
    #define FIRE_PIRANHA_PLANT_ANIM_UNUSED_3                0x3
    #define FIRE_PIRANHA_PLANT_ANIM_GROW                    0x4

/* Fire spitter */
    /* oAction */
    #define FIRE_SPITTER_ACT_IDLE                           0x0
    #define FIRE_SPITTER_ACT_SPIT_FIRE                      0x1

/* Moving Flame */
    /* oBehParams2ndByte */
    #define MOVING_FLAME_BP_1FRAME                          0x0
    #define MOVING_FLAME_BP_MOVE                            0x1

/* Fly Guy Flame */
    /* oBehParams2ndByte */
    #define MOVING_FLAME_PARTICLE_BP_UNK1                   0x1

/* Flamethrower */
    /* oBehParams2ndByte */
    #define FLAMETHROWER_BP_0                               0x0
    #define FLAMETHROWER_BP_BLUE                            0x1
    #define FLAMETHROWER_BP_SLOW                            0x2
    #define FLAMETHROWER_BP_TALL_HITBOX                     0x3
    #define FLAMETHROWER_BP_UPWARDS                         0x4
    /* oAction */
    #define FLAMETHROWER_ACT_IDLE                           0x0
    #define FLAMETHROWER_ACT_BLOW_FIRE                      0x1
    #define FLAMETHROWER_ACT_COOLDOWN                       0x2

/* Bouncing Fireball Flame */
    /* oAction */
    #define BOUNCING_FLAME_ACT_SPAWNED                      0x0
    #define BOUNCING_FLAME_ACT_LANDED                       0x1

/* Bouncing Fireball */
    /* oAction */
    #define BOUNCING_FIREBALL_SPAWNER_ACT_IDLE              0x0
    #define BOUNCING_FIREBALL_SPAWNER_ACT_SPAWN_FLAME       0x1
    #define BOUNCING_FIREBALL_SPAWNER_ACT_COOLDOWN          0x2

/* Eyerok boss */
    /* oAction */
    #define EYEROK_BOSS_ACT_SLEEP                           0x0
    #define EYEROK_BOSS_ACT_WAKE_UP                         0x1
    #define EYEROK_BOSS_ACT_SHOW_INTRO_TEXT                 0x2
    #define EYEROK_BOSS_ACT_FIGHT                           0x3
    #define EYEROK_BOSS_ACT_DIE                             0x4
    /* oSubAction */
        /* EYEROK_BOSS_ACT_WAKE_UP */
    #define EYEROK_BOSS_SUB_ACT_WAKE_LOWER_VOLUME           0x0
    #define EYEROK_BOSS_SUB_ACT_WAKE_WAIT_FOR_DIALOG        0x1

/* Eyerok hand */
    /* oBehParams2ndByte */
    #define EYEROK_BP_LEFT_HAND                            -0x1
    #define EYEROK_BP_RIGHT_HAND                            0x1
    /* oAction */
    #define EYEROK_HAND_ACT_SLEEP                           0x0
    #define EYEROK_HAND_ACT_IDLE                            0x1
    #define EYEROK_HAND_ACT_OPEN                            0x2
    #define EYEROK_HAND_ACT_SHOW_EYE                        0x3
    #define EYEROK_HAND_ACT_CLOSE                           0x4
    #define EYEROK_HAND_ACT_RETREAT                         0x5
    #define EYEROK_HAND_ACT_TARGET_MARIO                    0x6
    #define EYEROK_HAND_ACT_SMASH                           0x7
    #define EYEROK_HAND_ACT_FIST_PUSH                       0x8
    #define EYEROK_HAND_ACT_FIST_SWEEP                      0x9
    #define EYEROK_HAND_ACT_BEGIN_DOUBLE_POUND              0xA // raising for double smash
    #define EYEROK_HAND_ACT_DOUBLE_POUND                    0xB // double smashing
    #define EYEROK_HAND_ACT_ATTACKED                        0xC
    #define EYEROK_HAND_ACT_RECOVER                         0xD
    #define EYEROK_HAND_ACT_BECOME_ACTIVE                   0xE
    #define EYEROK_HAND_ACT_DIE                             0xF
    /* oAnimState */
    #define EYEROK_HAND_ANIM_STATE_EYE_OPEN                 0x0
    #define EYEROK_HAND_ANIM_STATE_EYE_MOSTLY_OPEN          0x1
    #define EYEROK_HAND_ANIM_STATE_EYE_MOSTLY_CLOSED        0x2
    #define EYEROK_HAND_ANIM_STATE_EYE_CLOSED               0x3
    /* oEyerokHandAnimStateIndex */
    #define EYEROK_HAND_ANIM_STATE_INDEX_EYE_OPEN_1         0x0
    #define EYEROK_HAND_ANIM_STATE_INDEX_EYE_CLOSING        0x1 // mostly open
    #define EYEROK_HAND_ANIM_STATE_INDEX_EYE_CLOSED         0x2
    #define EYEROK_HAND_ANIM_STATE_INDEX_EYE_OPENING_1      0x3 // mostly closed
    #define EYEROK_HAND_ANIM_STATE_INDEX_EYE_OPENING_2      0x4 // mostly open
    #define EYEROK_HAND_ANIM_STATE_INDEX_EYE_OPEN_2         0x5
    /* Animations */
    #define EYEROK_HAND_ANIM_RECOVER                        0x0
    #define EYEROK_HAND_ANIM_DEATH                          0x1
    #define EYEROK_HAND_ANIM_IDLE                           0x2
    #define EYEROK_HAND_ANIM_ATTACKED                       0x3
    #define EYEROK_HAND_ANIM_OPEN                           0x4
    #define EYEROK_HAND_ANIM_SHOW_EYE                       0x5
    #define EYEROK_HAND_ANIM_SLEEPING                       0x6
    #define EYEROK_HAND_ANIM_CLOSE                          0x7

/* King Bobomb */
    /* oAction */
    #define KING_BOBOMB_ACT_INACTIVE                        0x0
    #define KING_BOBOMB_ACT_ACTIVATE                        0x1
    #define KING_BOBOMB_ACT_ACTIVE                          0x2
    #define KING_BOBOMB_ACT_GRABBED_MARIO                   0x3
    #define KING_BOBOMB_ACT_BEEN_THROWN                     0x4
    #define KING_BOBOMB_ACT_RETURN_HOME                     0x5
    #define KING_BOBOMB_ACT_HIT_GROUND                      0x6
    #define KING_BOBOMB_ACT_DEATH                           0x7
    #define KING_BOBOMB_ACT_STOP_MUSIC                      0x8
    /* oSubAction */
        /* KING_BOBOMB_ACT_INACTIVE */
    #define KING_BOBOMB_SUB_ACT_INACTIVE_INIT               0x0
    #define KING_BOBOMB_SUB_ACT_INACTIVE_DIALOG             0x1
        /* KING_BOBOMB_ACT_GRABBED_MARIO */
    #define KING_BOBOMB_SUB_ACT_GRABBED_MARIO_GRAB          0x0
    #define KING_BOBOMB_SUB_ACT_GRABBED_MARIO_HOLDING       0x1
    #define KING_BOBOMB_SUB_ACT_GRABBED_MARIO_THROW         0x2
        /* KING_BOBOMB_ACT_BEEN_THROWN */
    #define KING_BOBOMB_SUB_ACT_THROWN_FALL                 0x0
    #define KING_BOBOMB_SUB_ACT_THROWN_STAND_UP             0x1
    #define KING_BOBOMB_SUB_ACT_THROWN_END                  0x2
        /* KING_BOBOMB_ACT_RETURN_HOME */
    #define KING_BOBOMB_SUB_ACT_RETURN_HOME_JUMP            0x0
    #define KING_BOBOMB_SUB_ACT_RETURN_HOME_LANDING         0x1
    #define KING_BOBOMB_SUB_ACT_RETURN_HOME_LANDING_END     0x2
    #define KING_BOBOMB_SUB_ACT_RETURN_HOME_WAIT_FOR_DIALOG 0x3
    #define KING_BOBOMB_SUB_ACT_RETURN_HOME_DIALOG          0x4
        /* KING_BOBOMB_ACT_HIT_GROUND */
    #define KING_BOBOMB_SUB_ACT_HIT_GROUND_HIT              0x0
    #define KING_BOBOMB_SUB_ACT_HIT_GROUND_STAND_UP         0x1
    #define KING_BOBOMB_SUB_ACT_HIT_GROUND_START_WALKING    0x2
    /* Animations */
    #define KING_BOBOMB_ANIM_GRAB_MARIO                     0x0
    #define KING_BOBOMB_ANIM_HOLDING_MARIO                  0x1
    #define KING_BOBOMB_ANIM_HIT_GROUND                     0x2
    #define KING_BOBOMB_ANIM_UNUSED_3                       0x3
    #define KING_BOBOMB_ANIM_STOMP                          0x4
    #define KING_BOBOMB_ANIM_IDLE                           0x5
    #define KING_BOBOMB_ANIM_HELD                           0x6
    #define KING_BOBOMB_ANIM_T_POSE                         0x7
    #define KING_BOBOMB_ANIM_JUMP                           0x8
    #define KING_BOBOMB_ANIM_THROW_MARIO                    0x9
    #define KING_BOBOMB_ANIM_STAND_UP                       0xA
    #define KING_BOBOMB_ANIM_WALKING                        0xB

/* Chuckya */
    /* oAction */
    #define CHUCKYA_ACT_MOVING                              0x0
    #define CHUCKYA_ACT_GRABBED_MARIO                       0x1
    #define CHUCKYA_ACT_THROWN                              0x2
    #define CHUCKYA_ACT_RELEASE_MARIO                       0x3
    /* oSubAction */
        /* CHUCKYA_ACT_MOVING */
    #define CHUCKYA_SUB_ACT_TURN_TOWARD_MARIO               0x0
    #define CHUCKYA_SUB_ACT_ACCELERATE                      0x1
    #define CHUCKYA_SUB_ACT_STOP                            0x2
    #define CHUCKYA_SUB_ACT_TURN_TOWARD_HOME                0x3
        /* CHUCKYA_ACT_GRABBED_MARIO */
    #define CHUCKYA_SUB_ACT_GRAB_MARIO                      0x0
    #define CHUCKYA_SUB_ACT_HOLD_MARIO                      0x1
    #define CHUCKYA_SUB_ACT_THROW_MARIO                     0x2
    /* oCommonAnchorAction */
    #define COMMON_ANCHOR_ACT_INACTIVE                      0x0
    #define COMMON_ANCHOR_ACT_SET_MARIO_GFX_TO_POS          0x1
    #define COMMON_ANCHOR_ACT_THROW_MARIO                   0x2
    #define COMMON_ANCHOR_ACT_DROP_MARIO                    0x3
    /* Animations */
    #define CHUCKYA_ANIM_GRAB_MARIO                         0x0
    #define CHUCKYA_ANIM_THROW_1                            0x1
    #define CHUCKYA_ANIM_HELD                               0x2
    #define CHUCKYA_ANIM_THROW_2                            0x3
    #define CHUCKYA_ANIM_IDLE                               0x4
    #define CHUCKYA_ANIM_SPAWN                              0x5

/* Heave Ho */
    /* oAction */
    #define HEAVE_HO_ACT_INACTIVE                           0x0
    #define HEAVE_HO_ACT_WINDING_UP                         0x1
    #define HEAVE_HO_ACT_MOVING                             0x2
    #define HEAVE_HO_ACT_THROW_MARIO                        0x3
    /* Animations */
    #define HEAVE_HO_ANIM_MOVING                            0x0
    #define HEAVE_HO_ANIM_THROW                             0x1
    #define HEAVE_HO_ANIM_WINDING_UP                        0x2

/* Klepto */
    /* oBehParams2ndByte */
    #define KLEPTO_BP_NO_STAR                               0x0
    #define KLEPTO_BP_HAS_STAR                              0x1
    /* oAction */
    #define KLEPTO_ACT_CIRCLE_TARGET_HOLDING                0x0
    #define KLEPTO_ACT_APPROACH_TARGET_HOLDING              0x1
    #define KLEPTO_ACT_WAIT_FOR_MARIO                       0x2
    #define KLEPTO_ACT_TURN_TOWARD_MARIO                    0x3
    #define KLEPTO_ACT_DIVE_AT_MARIO                        0x4
    #define KLEPTO_ACT_RESET_POSITION                       0x5
    #define KLEPTO_ACT_STRUCK_BY_MARIO                      0x6
    #define KLEPTO_ACT_RETREAT                              0x7
    /* oSubAction */
        /* KLEPTO_ACT_DIVE_AT_MARIO */
    #define KLEPTO_SUB_ACT_DIVE_TURN_PITCH                  0x0
    #define KLEPTO_SUB_ACT_DIVE_STOP                        0x1
    /* oAnimState */
    #define KLEPTO_ANIM_STATE_HOLDING_NOTHING               0x0
    #define KLEPTO_ANIM_STATE_HOLDING_CAP                   0x1
    #define KLEPTO_ANIM_STATE_HOLDING_STAR                  0x2
    #define KLEPTO_ANIM_STATE_HOLDING_TRANSPARENT_STAR      0x3
    /* Animations */
    #define KLEPTO_ANIM_DIVE_0                              0x0
    #define KLEPTO_ANIM_STRUCK_BY_MARIO                     0x1
    #define KLEPTO_ANIM_DIVE_AT_MARIO_2                     0x2
    #define KLEPTO_ANIM_DIVE_AT_MARIO_3                     0x3
    #define KLEPTO_ANIM_DIVE_AT_MARIO_4                     0x4
    #define KLEPTO_ANIM_DIVE_FLAP_5                         0x5
    #define KLEPTO_ANIM_DIVE_FLAP_6                         0x6

/* Bird */
    /* oBehParams2ndByte */
    #define BIRD_BP_SPAWNED                                 0x0
    #define BIRD_BP_SPAWNER                                 0x1
    /* oAction */
    #define BIRD_ACT_INACTIVE                               0x0
    #define BIRD_ACT_FLY                                    0x1
    /* Animation */
    #define BIRD_ANIM_FLY                                   0x0

/* End Birds */
    /* oAction */
    #define END_BIRDS_ACT_INIT                              0x0
    #define END_BIRDS_ACT_ACTIVE                            0x1
    /* Animations */
    #define END_BIRDS_ANIM_FLY                              0x0

/* Birds Sound */
    /* oAction */
    #define SOUND_BIRDS_BP_OBJ2_BIRD_CHIRP1                 0x0
    #define SOUND_BIRDS_BP_GENERAL2_BIRD_CHIRP2             0x1
    #define SOUND_BIRDS_BP_OBJ_BIRD_CHIRP3                  0x2

/* Scuttlebug Spawner */
    /* oAction */
    #define SCUTTLEBUG_SPAWNER_ACT_ACTIVE                   0x0
    #define SCUTTLEBUG_SPAWNER_ACT_INACTIVE                 0x1

/* Scuttlebug */
    /* oSubAction */
    #define SCUTTLEBUG_SUB_ACT_RESET                        0x0
    #define SCUTTLEBUG_SUB_ACT_MOVING                       0x1
    #define SCUTTLEBUG_SUB_ACT_HIT_WALL                     0x2
    #define SCUTTLEBUG_SUB_ACT_ALERT                        0x3
    #define SCUTTLEBUG_SUB_ACT_JUMP                         0x4
    #define SCUTTLEBUG_SUB_ACT_LAND                         0x5
    /* Animations */
    #define SCUTTLEBUG_ANIM_JUMP                            0x0
    #define SCUTTLEBUG_ANIM_WALK                            0x1

/* Skeeter */
    /* oAction */
    #define SKEETER_ACT_IDLE                                0x0
    #define SKEETER_ACT_LUNGE                               0x1
    #define SKEETER_ACT_WALK                                0x2
    /* Animations */
    #define SKEETER_ANIM_WATER_LUNGE                        0x0
    #define SKEETER_ANIM_WATER_IDLE                         0x1
    #define SKEETER_ANIM_WALK                               0x2
    #define SKEETER_ANIM_IDLE                               0x3

/* Snufit */
    /* oAction */
    #define SNUFIT_ACT_IDLE                                 0x0
    #define SNUFIT_ACT_SHOOT                                0x1

/* Snufit Balls (gottem) */
    /* oAction */
    #define SNUFIT_BALL_ACT_MOVE                            0x0
    #define SNUFIT_BALL_ACT_HIT_MARIO                       0x1

/* Tweester */
    /* oAction */
    #define TWEESTER_ACT_IDLE                               0x0
    #define TWEESTER_ACT_CHASE                              0x1
    #define TWEESTER_ACT_HIDE                               0x2
    /* oSubAction */
        /* TWEESTER_ACT_IDLE */
    #define TWEESTER_SUB_ACT_WAIT                           0x0
    #define TWEESTER_SUB_ACT_GROW                           0x1
        /* TWEESTER_ACT_CHASE */
    #define TWEESTER_SUB_ACT_CHASE_MARIO                    0x0
    #define TWEESTER_SUB_ACT_CHASE_HOME                     0x1

/* Triplet butterfly */
    /* oBehParams2ndByte */
    #define TRIPLET_BUTTERFLY_BP_BUTTERFLY_NUM              0x3
    #define TRIPLET_BUTTERFLY_BP_NO_BOMBS                   0x4
    /* oAction */
    #define TRIPLET_BUTTERFLY_ACT_INIT                      0x0
    #define TRIPLET_BUTTERFLY_ACT_WANDER                    0x1
    #define TRIPLET_BUTTERFLY_ACT_ACTIVATE                  0x2
    #define TRIPLET_BUTTERFLY_ACT_EXPLODE                   0x3
    /* oTripletButterflyType */
    #define TRIPLET_BUTTERFLY_TYPE_NORMAL                  -0x1
    #define TRIPLET_BUTTERFLY_TYPE_EXPLODES                 0x0
    #define TRIPLET_BUTTERFLY_TYPE_SPAWN_1UP                0x1


/* Changing Water Level */
    /* oAction */
    #define WATER_LEVEL_ACT_INIT                            0x0
    #define WATER_LEVEL_ACT_IDLE                            0x1

/* Water Level Diamond */
    /* oAction */
    // Loading
    #define WATER_LEVEL_DIAMOND_ACT_INIT                    0x0
    // Idling when Mario isn't inside its hitbox
    #define WATER_LEVEL_DIAMOND_ACT_IDLE                    0x1
    // While the water level is changing
    #define WATER_LEVEL_DIAMOND_ACT_CHANGE_WATER_LEVEL      0x2
    // After the water level has changed but Mario hasn't left its hitbox yet
    #define WATER_LEVEL_DIAMOND_ACT_IDLE_SPINNING           0x3

/* Castle Water Level Pillars */
    /* oAction */
    #define WATER_PILLAR_ACT_UNPOUNDED_IDLE                 0x0
    #define WATER_PILLAR_ACT_LOWER_SELF                     0x1
    #define WATER_PILLAR_ACT_POUNDED_1                      0x2
    #define WATER_PILLAR_ACT_POUNDED_2                      0x3
    #define WATER_PILLAR_ACT_DRAIN_WATER                    0x4
    #define WATER_PILLAR_ACT_END                            0x5

/* Mips */
    /* oBehParams2ndByte */
    #define MIPS_BP_STAR_1                                  0x0
    #define MIPS_BP_STAR_2                                  0x1
    /* oAction */
    #define MIPS_ACT_WAIT_FOR_NEARBY_MARIO                  0x0
    #define MIPS_ACT_FOLLOW_PATH                            0x1
    #define MIPS_ACT_WAIT_FOR_ANIMATION_DONE                0x2
    #define MIPS_ACT_FALL_DOWN                              0x3
    #define MIPS_ACT_IDLE                                   0x4
    /* oMipsStarStatus */
    #define MIPS_STAR_STATUS_HAVENT_SPAWNED_STAR            0x0
    #define MIPS_STAR_STATUS_SHOULD_SPAWN_STAR              0x1
    #define MIPS_STAR_STATUS_ALREADY_SPAWNED_STAR           0x2
    /* Animations */
    #define MIPS_ANIM_IDLE                                  0x0
    #define MIPS_ANIM_HOPPING                               0x1
    #define MIPS_ANIM_THROWN                                0x2
    #define MIPS_ANIM_UNUSED                                0x3
    #define MIPS_ANIM_HELD                                  0x4

/* Falling Pillar */
    /* oAction */
    #define FALLING_PILLAR_ACT_IDLE                         0x0
    #define FALLING_PILLAR_ACT_TURNING                      0x1
    #define FALLING_PILLAR_ACT_FALLING                      0x2

/* Bowser Puzzle */
    /* oAction */
    #define BOWSER_PUZZLE_ACT_SPAWN_PIECES                  0x0
    #define BOWSER_PUZZLE_ACT_WAIT_FOR_COMPLETE             0x1
    #define BOWSER_PUZZLE_ACT_DONE                          0x2
    /* oBowserPuzzleCompletionFlags */
    #define BOWSER_PUZZLE_COMPLETION_FLAGS_NONE             0x0
    #define BOWSER_PUZZLE_COMPLETION_FLAG_MARIO_ON_PLATFORM 0x1
    #define BOWSER_PUZZLE_COMPLETION_FLAG_PUZZLE_COMPLETE   0x2

/* Bowser Puzzle Piece */
    /* oAction */
    #define BOWSER_PUZZLE_PIECE_ACT_IDLE                    0x0
    #define BOWSER_PUZZLE_PIECE_ACT_LEFT                    0x1
    #define BOWSER_PUZZLE_PIECE_ACT_RIGHT                   0x2
    #define BOWSER_PUZZLE_PIECE_ACT_UP                      0x3
    #define BOWSER_PUZZLE_PIECE_ACT_DOWN                    0x4

/* Spindrift */
    /* oAction */
    #define SPINDRIFT_ACT_ACTIVE                            0x0
    #define SPINDRIFT_ACT_HIT_MARIO                         0x1
    /* oAction */
    #define SPINDRIFT_ANIM_DEFAULT                          0x0

/* Mr Blizzard */
    /* oBehParams2ndByte */
    #define MR_BLIZZARD_STYPE_NO_CAP                        0x0
    #define MR_BLIZZARD_STYPE_JUMPING                       0x1
    /* oAction */
    #define MR_BLIZZARD_ACT_SPAWN_SNOWBALL                  0x0
    #define MR_BLIZZARD_ACT_HIDE_UNHIDE                     0x1
    #define MR_BLIZZARD_ACT_RISE_FROM_GROUND                0x2
    #define MR_BLIZZARD_ACT_ROTATE                          0x3
    #define MR_BLIZZARD_ACT_THROW_SNOWBALL                  0x4
    #define MR_BLIZZARD_ACT_BURROW                          0x5
    #define MR_BLIZZARD_ACT_DEATH                           0x6
    #define MR_BLIZZARD_ACT_JUMP                            0x7
    /* oAnimState */
    #define MR_BLIZZARD_ANIM_STATE_NO_CAP                   0x0
    #define MR_BLIZZARD_ANIM_STATE_HAS_CAP                  0x1
    /* Animations */
    #define MR_BLIZZARD_ANIM_SPAWN_SNOWBALL                 0x0
    #define MR_BLIZZARD_ANIM_THROW_SNOWBALL                 0x1

/* Mr Blizzard Snowball */
    /* oAction */
    #define MR_BLIZZARD_SNOWBALL_ACT_INIT                   0x0
    #define MR_BLIZZARD_SNOWBALL_ACT_LAUNCH                 0x1
    #define MR_BLIZZARD_SNOWBALL_ACT_COLLISION              0x2

/* Mr I */
    /* oBehParams2ndByte */
    #define MR_I_BP_NORMAL                                  0x0
    #define MR_I_BP_LARGE_WITH_STAR                         0x1
    /* oAction */
    #define MR_I_BODY_ACT_FAR_AWAY                          0x0
    #define MR_I_BODY_ACT_IDLE                              0x1
    #define MR_I_BODY_ACT_LOOKING_AT_MARIO                  0x2
    #define MR_I_BODY_ACT_SPIN_DEATH                        0x3

/* Mr I & Piranha Plant Particle */
    /* oAction */
    #define MR_I_PIRANHA_PARTICLE_ACT_MOVE                  0x0
    #define MR_I_PIRANHA_PARTICLE_ACT_INTERACTED            0x1

/* Bullet Bill */
    /* oAction */
    #define BULLET_BILL_ACT_RESET                           0x0
    #define BULLET_BILL_ACT_IDLE                            0x1
    #define BULLET_BILL_ACT_CHASING_MARIO                   0x2
    #define BULLET_BILL_ACT_HIT                             0x3

/* Thwomp & Grindel */
    /* oAction */
    #define GRINDEL_THWOMP_ACT_RISING                       0x0
    #define GRINDEL_THWOMP_ACT_FLOATING                     0x1
    #define GRINDEL_THWOMP_ACT_FALLING                      0x2
    #define GRINDEL_THWOMP_ACT_LAND                         0x3
    #define GRINDEL_THWOMP_ACT_ON_GROUND                    0x4

/* Whomp */
    /* oBehParams2ndByte */
    #define WHOMP_BP_NORMAL                                 0x0
    #define WHOMP_BP_KING                                   0x1
    /* oAction */
    #define WHOMP_ACT_INIT                                  0x0
    #define WHOMP_ACT_PATROL                                0x1
    #define WHOMP_ACT_KING_CHASE                            0x2
    #define WHOMP_ACT_PREPARE_JUMP                          0x3
    #define WHOMP_ACT_JUMP                                  0x4
    #define WHOMP_ACT_LAND                                  0x5
    #define WHOMP_ACT_ON_GROUND_GENERAL                     0x6
    #define WHOMP_ACT_TURN                                  0x7
    #define WHOMP_ACT_DIE                                   0x8
    #define WHOMP_ACT_STOP_BOSS_MUSIC                       0x9
    /* oSubAction */
        /* WHOMP_ACT_INIT */
    #define WHOMP_SUB_ACT_INIT_IDLE                         0x0
    #define WHOMP_SUB_ACT_INIT_DIALOG                       0x1
        /* WHOMP_ACT_LAND */
    #define WHOMP_SUB_ACT_LAND_EFFECTS                      0x0
    #define WHOMP_SUB_ACT_LAND_ON_GROUND                    0x1
        /* WHOMP_ACT_ON_GROUND_GENERAL */
    #define WHOMP_SUB_ACT_GROUND_LYING_DOWN                 0x0
    #define WHOMP_SUB_ACT_GROUND_PREPARE_STAND_UP           0x1
    #define WHOMP_SUB_ACT_GROUND_STAND_UP                   0xA
        /* WHOMP_ACT_TURN */
    #define WHOMP_SUB_ACT_TURN_TURNING                      0x0
    #define WHOMP_SUB_ACT_TURN_END                          0x1
    /* Animations */
    #define WHOMP_ANIM_WALK                                 0x0
    #define WHOMP_ANIM_JUMP                                 0x1

/* WF Kickable Board */
    /* oAction */
    #define KICKABLE_BOARD_ACT_IDLE_VERTICAL                0x0
    #define KICKABLE_BOARD_ACT_ROCKING                      0x1
    #define KICKABLE_BOARD_ACT_FALLING                      0x2
    #define KICKABLE_BOARD_ACT_IDLE_HORIZONTAL              0x3
    /* check_mario_attacking */
    #define WF_ATTACK_NONE                                  0x0
    #define WF_ATTACK_GROUND                                0x1
    #define WF_ATTACK_AIR                                   0x2

/* White Puff Explosion */
    /* oBehParams2ndByte */
    #define WHITE_PUFF_EXPLODE_BP_FAST_FADE                 0x2
    #define WHITE_PUFF_EXPLODE_BP_SLOW_FADE                 0x3

/* Dirt Particle */
    /* oAnimState */
    #define TINY_DIRT_PARTICLE_ANIM_STATE_RED               0x0
    #define TINY_DIRT_PARTICLE_ANIM_STATE_GREEN             0x1
    #define TINY_DIRT_PARTICLE_ANIM_STATE_BLUE              0x2
    #define TINY_DIRT_PARTICLE_ANIM_STATE_DIRT              0x3
    #define TINY_DIRT_PARTICLE_ANIM_STATE_YELLOW            0x4
    #define TINY_DIRT_PARTICLE_ANIM_STATE_BILLBOARD         0x5

/* Cartoon Star Particle */
    /* oAnimState */
    #define CARTOON_STAR_PARTICLE_ANIM_STATE_RED            0x0
    #define CARTOON_STAR_PARTICLE_ANIM_STATE_GREEN          0x1
    #define CARTOON_STAR_PARTICLE_ANIM_STATE_BLUE           0x2
    #define CARTOON_STAR_PARTICLE_ANIM_STATE_YELLOW         0x3
    #define CARTOON_STAR_PARTICLE_ANIM_STATE_BILLBOARD      0x4

/* Music Touch */
    /* oAction */
    #define MUSIC_TOUCH_ACT_PLAY_SOUND                      0x0
    #define MUSIC_TOUCH_ACT_DONE                            0x1

/* Intro Scene */
    /* gCutsceneObjSpawn */
    #define CUTSCENE_OBJ_NONE                               0x0
    #define CUTSCENE_OBJ_BEGINNING_PEACH                    0x5
    #define CUTSCENE_OBJ_BEGINNING_LAKITU                   0x6
    #define CUTSCENE_OBJ_7_END_BIRDS_1                      0x7
    #define CUTSCENE_OBJ_5_END_BIRDS_2                      0x8
    #define CUTSCENE_OBJ_2_END_BIRDS_1                      0x9

#endif // OBJECT_CONSTANTS_H
