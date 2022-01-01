#ifndef MACRO_PRESETS_H
#define MACRO_PRESETS_H

#include "macro_preset_names.h"
#include "behavior_data.h"
#include "model_ids.h"
#include "object_constants.h"

struct MacroPreset {
    /*0x00*/ const BehaviorScript *behavior;
    /*0x04*/ ModelID16 model;
    /*0x06*/ s16 param;
};

struct MacroPreset MacroObjectPresets[] = {
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_yellow_coin
    {bhvOneCoin,                        MODEL_YELLOW_COIN,                  0}, // macro_yellow_coin_2
    {bhvMovingBlueCoin,                 MODEL_BLUE_COIN,                    0}, // macro_moving_blue_coin
    {bhvBlueCoinSliding,                MODEL_BLUE_COIN,                    0}, // macro_sliding_blue_coin - unused
    {bhvRedCoin,                        MODEL_RED_COIN,                     0}, // macro_red_coin
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_5
    {bhvCoinFormation,                  MODEL_NONE, (                       0                                                            )}, // macro_coin_line_horizontal
    {bhvCoinFormation,                  MODEL_NONE, (                             COIN_FORMATION_FLAG_RING                               )}, // macro_coin_ring_horizontal
    {bhvCoinFormation,                  MODEL_NONE, (                             COIN_FORMATION_FLAG_ARROW                              )}, // macro_coin_arrow
    {bhvCoinFormation,                  MODEL_NONE, (COIN_FORMATION_FLAG_FLYING                                                          )}, // macro_coin_line_horizontal_flying
    {bhvCoinFormation,                  MODEL_NONE, (COIN_FORMATION_FLAG_FLYING |                            COIN_FORMATION_FLAG_VERTICAL)}, // macro_coin_line_vertical
    {bhvCoinFormation,                  MODEL_NONE, (COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_RING                               )}, // macro_coin_ring_horizontal_flying
    {bhvCoinFormation,                  MODEL_NONE, (COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_RING | COIN_FORMATION_FLAG_VERTICAL)}, // macro_coin_ring_vertical
    {bhvCoinFormation,                  MODEL_NONE, (COIN_FORMATION_FLAG_FLYING | COIN_FORMATION_FLAG_ARROW                              )}, // macro_coin_arrow_flying - unused
    {bhvHiddenStarTrigger,              MODEL_NONE,                         0}, // macro_hidden_star_trigger
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_15
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_16
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_17
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_18
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_19
    {bhvUnusedFakeStar,                 MODEL_STAR,                         0}, // macro_fake_star - unused
    {bhvMessagePanel,                   MODEL_WOODEN_SIGNPOST,              0}, // macro_wooden_signpost
    {bhvCannonClosed,                   MODEL_DL_CANNON_LID,                0}, // macro_cannon_closed
    {bhvBobombBuddyOpensCannon,         MODEL_BOBOMB_BUDDY,                 0}, // macro_bobomb_buddy_opens_cannon
    {bhvButterfly,                      MODEL_BUTTERFLY,                    0}, // macro_butterfly - unused
    {bhvBouncingFireball,               MODEL_NONE,                         0}, // macro_bouncing_fireball_copy - unused
    {bhvFishSpawner,                    MODEL_NONE,                         FISH_SPAWNER_BP_MANY_BLUE}, // macro_fish_group_3 - unused
    {bhvFishSpawner,                    MODEL_NONE,                         FISH_SPAWNER_BP_FEW_BLUE }, // macro_fish_group
    {bhvBetaFishSplashSpawner,          MODEL_NONE,                         0}, // macro_beta_fish_splash_spawner
    {bhvHidden1upInPoleSpawner,         MODEL_NONE,                         0}, // macro_hidden_1up_in_pole
    {bhvGoomba,                         MODEL_GOOMBA,                       GOOMBA_SIZE_HUGE}, // macro_huge_goomba
    {bhvGoomba,                         MODEL_GOOMBA,                       GOOMBA_SIZE_TINY}, // macro_tiny_goomba
    {bhvGoombaTripletSpawner,           MODEL_NONE,                         0}, // macro_goomba_triplet_spawner
    {bhvGoombaTripletSpawner,           MODEL_NONE,                         8}, // macro_goomba_quintuplet_spawner - unused
    {bhvSignOnWall,                     MODEL_NONE,                         0}, // macro_sign_on_wall
    {bhvChuckya,                        MODEL_CHUCKYA,                      0}, // macro_chuckya
    {bhvCannon,                         MODEL_CANNON_BASE,                  0}, // macro_cannon_open
    {bhvGoomba,                         MODEL_GOOMBA,                       0}, // macro_goomba
    {bhvHomingAmp,                      MODEL_AMP,                          0}, // macro_homing_amp
    {bhvCirclingAmp,                    MODEL_AMP,                          0}, // macro_circling_amp
    {bhvYellowCoin,                     MODEL_UNKNOWN_7D,                   0}, // macro_unknown_40 - unused (was bhvCarrySomething1)
    {bhvBetaTrampolineTop,              MODEL_TRAMPOLINE,                   0}, // macro_beta_trampoline_top - unused
    {bhvFreeBowlingBall,                MODEL_BOWLING_BALL,                 0}, // macro_free_bowling_ball - unused
    {bhvSnufit,                         MODEL_SNUFIT,                       0}, // macro_snufit
    {bhvRecoveryHeart,                  MODEL_HEART,                        0}, // macro_recovery_heart
    {bhv1upSliding,                     MODEL_1UP,                          0}, // macro_1up_sliding
    {bhv1Up,                            MODEL_1UP,                          MUSHROOM_BP_REQUIRES_NONE}, // macro_1up
    {bhv1upJumpOnApproach,              MODEL_1UP,                          0}, // macro_1up_jump_on_approach - unused
    {bhvHidden1up,                      MODEL_1UP,                          0}, // macro_hidden_1up
    {bhvHidden1upTrigger,               MODEL_NONE,                         0}, // macro_hidden_1up_trigger
    {bhv1Up,                            MODEL_1UP,                          MUSHROOM_BP_REQUIRES_BOWSER_1}, // macro_1up_2
    {bhv1Up,                            MODEL_1UP,                          MUSHROOM_BP_REQUIRES_BOWSER_2}, // macro_1up_3
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_52
    {bhvBlueCoinSwitch,                 MODEL_BLUE_COIN_SWITCH,             0}, // macro_blue_coin_switch
    {bhvHiddenBlueCoin,                 MODEL_BLUE_COIN,                    0}, // macro_hidden_blue_coin
    {bhvCapSwitch,                      MODEL_CAP_SWITCH,                   CAP_SWITCH_BP_WING_CAP  }, // macro_wing_cap_switch - unused
    {bhvCapSwitch,                      MODEL_CAP_SWITCH,                   CAP_SWITCH_BP_METAL_CAP }, // macro_metal_cap_switch - unused
    {bhvCapSwitch,                      MODEL_CAP_SWITCH,                   CAP_SWITCH_BP_VANISH_CAP}, // macro_vanish_cap_switch - unused
    {bhvCapSwitch,                      MODEL_CAP_SWITCH,                   CAP_SWITCH_BP_YELLOW_CAP}, // macro_yellow_cap_switch - unused
    {bhvWaterLevelDiamond,              MODEL_BREAKABLE_BOX,                0}, // macro_water_level_breakable_box - unused
    {bhvExclamationBox,                 MODEL_EXCLAMATION_BOX,              EXCLAMATION_BOX_BP_WING_CAP  }, // macro_box_wing_cap
    {bhvExclamationBox,                 MODEL_EXCLAMATION_BOX,              EXCLAMATION_BOX_BP_METAL_CAP } , // macro_box_metal_cap
    {bhvExclamationBox,                 MODEL_EXCLAMATION_BOX,              EXCLAMATION_BOX_BP_VANISH_CAP}, // macro_box_vanish_cap
    {bhvExclamationBox,                 MODEL_EXCLAMATION_BOX,              EXCLAMATION_BOX_BP_KOOPA_SHELL}, // macro_box_koopa_shell
    {bhvExclamationBox,                 MODEL_EXCLAMATION_BOX,              EXCLAMATION_BOX_BP_COINS_1}, // macro_box_one_coin - unused
    {bhvExclamationBox,                 MODEL_EXCLAMATION_BOX,              EXCLAMATION_BOX_BP_COINS_3}, // macro_box_three_coins
    {bhvExclamationBox,                 MODEL_EXCLAMATION_BOX,              EXCLAMATION_BOX_BP_COINS_10}, // macro_box_ten_coins
    {bhvExclamationBox,                 MODEL_EXCLAMATION_BOX,              EXCLAMATION_BOX_BP_1UP_WALKING}, // macro_box_1up
    {bhvExclamationBox,                 MODEL_EXCLAMATION_BOX,              EXCLAMATION_BOX_BP_STAR_1}, // macro_box_star_1
    {bhvBreakableBox,                   MODEL_BREAKABLE_BOX,                BREAKABLE_BOX_BP_NO_COINS}, // macro_breakable_box_no_coins
    {bhvBreakableBox,                   MODEL_BREAKABLE_BOX,                BREAKABLE_BOX_BP_3_COINS}, // macro_breakable_box_three_coins
    {bhvPushableMetalBox,               MODEL_METAL_BOX,                    0}, // macro_pushable_metal_box
    {bhvBreakableBoxSmall,              MODEL_BREAKABLE_BOX,                0}, // macro_breakable_box_small
    {bhvFloorSwitchHiddenObjects,       MODEL_PURPLE_SWITCH,                0}, // macro_floor_switch_hidden_objects
    {bhvHiddenObject,                   MODEL_BREAKABLE_BOX,                BREAKABLE_BOX_BP_NO_COINS}, // macro_hidden_box
    {bhvHiddenObject,                   MODEL_BREAKABLE_BOX,                BREAKABLE_BOX_BP_3_COINS }, // macro_hidden_object_2 - unused
    {bhvHiddenObject,                   MODEL_BREAKABLE_BOX,                BREAKABLE_BOX_BP_5_COINS }, // macro_hidden_object_3 - unused
    {bhvBreakableBox,                   MODEL_BREAKABLE_BOX,                BREAKABLE_BOX_BP_LARGE   }, // macro_breakable_box_giant
    {bhvKoopaShellUnderwater,           MODEL_KOOPA_SHELL,                  0}, // macro_koopa_shell_underwater
    {bhvExclamationBox,                 MODEL_EXCLAMATION_BOX,              EXCLAMATION_BOX_BP_1UP_RUNNING_AWAY}, // macro_box_1up_running_away
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_80
    {bhvBulletBill,                     MODEL_BULLET_BILL,                  0}, // macro_bullet_bill_cannon - unused
    {bhvHeaveHo,                        MODEL_HEAVE_HO,                     0}, // macro_heave_ho
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_83
    {bhvThwomp2,                        MODEL_THWOMP,                       0}, // macro_thwomp - unused (was thwomp2)
    {bhvFireSpitter,                    MODEL_BOWLING_BALL,                 0}, // macro_fire_spitter
    {bhvFlyGuy,                         MODEL_FLYGUY,                       FLY_GUY_BP_SHOOTS_FIRE}, // macro_fire_fly_guy
    {bhvJumpingBox,                     MODEL_BREAKABLE_BOX,                0}, // macro_jumping_box
    {bhvTripletButterfly,               MODEL_BUTTERFLY,                    TRIPLET_BUTTERFLY_BP_0       }, // macro_butterfly_triplet
    {bhvTripletButterfly,               MODEL_BUTTERFLY,                    TRIPLET_BUTTERFLY_BP_NO_BOMBS}, // macro_butterfly_triplet_2
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_90
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_91
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_92
    {bhvSmallBully,                     MODEL_BULLY,                        0}, // macro_bully
    {bhvSmallBully,                     MODEL_BULLY_BOSS,                   0}, // macro_bully_2 - unused
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_95
    {bhvYellowCoin,                     MODEL_UNKNOWN_58,                   0}, // macro_unknown_96 - unused (was bhvStub1D0C)
    {bhvBouncingFireball,               MODEL_NONE,                         0}, // macro_bouncing_fireball
    {bhvFlamethrower,                   MODEL_NONE,                         FLAMETHROWER_BP_UPWARDS}, // macro_flamethrower
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_99
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_100
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_101
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_102
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_103
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_104
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_105
    {bhvWoodenPost,                     MODEL_WOODEN_POST,                  0}, // macro_wooden_post
    {bhvWaterBombSpawner,               MODEL_NONE,                         0}, // macro_water_bomb_spawner
    {bhvEnemyLakitu,                    MODEL_ENEMY_LAKITU,                 0}, // macro_enemy_lakitu
    {bhvKoopa,                          MODEL_KOOPA_WITH_SHELL,             KOOPA_BP_KOOPA_THE_QUICK_BASE}, // macro_bob_koopa_the_quick - unused
    {bhvKoopaRaceEndpoint,              MODEL_NONE,                         0}, // macro_koopa_race_endpoint - unused
    {bhvBobomb,                         MODEL_BLACK_BOBOMB,                 BOBOMB_BP_STYPE_GENERIC   }, // macro_bobomb
    {bhvWaterBombCannon,                MODEL_CANNON_BASE,                  0}, // macro_water_bomb_cannon_copy - unused
    {bhvBobombBuddyOpensCannon,         MODEL_BOBOMB_BUDDY,                 0}, // macro_bobomb_buddy_opens_cannon_copy - unused
    {bhvWaterBombCannon,                MODEL_CANNON_BASE,                  0}, // macro_water_bomb_cannon
    {bhvBobomb,                         MODEL_BLACK_BOBOMB,                 BOBOMB_BP_STYPE_STATIONARY}, // macro_bobomb_still
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_116
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_117
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_118
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_119
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_120
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_121
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_122
    {bhvUnusedFakeStar,                 MODEL_UNKNOWN_54,                   0}, // macro_unknown_123 - unused
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_124
    {bhvUnagi,                          MODEL_UNAGI,                        0}, // macro_unagi - unused
    {bhvSushiShark,                     MODEL_SUSHI,                        0}, // macro_sushi - unused
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_127
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_128
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_129
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_130
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_131
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_132
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_133
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_134
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_135
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_136
    {bhvStaticObject,                   MODEL_KLEPTO,                       0}, // macro_static_klepto - unused
    {bhvTweester,                       MODEL_TWEESTER,                     0}, // macro_tornado - unused
    {bhvPokey,                          MODEL_NONE,                         0}, // macro_pokey
    {bhvPokey,                          MODEL_NONE,                         0}, // macro_pokey_copy - unused
    {bhvToxBox,                         MODEL_SSL_TOX_BOX,                  0}, // macro_tox_box - unused
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_142
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_143
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_144
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_145
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_146
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_147
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_148
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_149
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_150
    {bhvMontyMole,                      MODEL_MONTY_MOLE,                   MONTY_MOLE_BP_NO_ROCK}, // macro_monty_mole_2 - unused
    {bhvMontyMole,                      MODEL_MONTY_MOLE,                   MONTY_MOLE_BP_ROCK}, // macro_monty_mole
    {bhvMontyMoleHole,                  MODEL_DL_MONTY_MOLE_HOLE,           0}, // macro_monty_mole_hole
    {bhvFlyGuy,                         MODEL_FLYGUY,                       0}, // macro_fly_guy
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_155
    {bhvWigglerHead,                    MODEL_WIGGLER_HEAD,                 0}, // macro_wiggler - unused
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_157
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_158
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_159
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_160
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_161
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_162
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_163
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_164
    {bhvSpindrift,                      MODEL_SPINDRIFT,                    0}, // macro_spindrift
    {bhvMrBlizzard,                     MODEL_MR_BLIZZARD_HIDDEN,           MR_BLIZZARD_STYPE_NO_CAP}, // macro_mr_blizzard
    {bhvMrBlizzard,                     MODEL_MR_BLIZZARD_HIDDEN,           MR_BLIZZARD_STYPE_NO_CAP}, // macro_mr_blizzard_copy - unused
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_168
    {bhvSmallPenguin,                   MODEL_PENGUIN,                      0}, // macro_small_penguin - unused
    {bhvTuxiesMother,                   MODEL_PENGUIN,                      0}, // macro_tuxies_mother - unused
    {bhvTuxiesMother,                   MODEL_PENGUIN,                      0}, // macro_tuxies_mother_copy - unused
    {bhvMrBlizzard,                     MODEL_MR_BLIZZARD_HIDDEN,           MR_BLIZZARD_STYPE_JUMPING}, // macro_mr_blizzard_2 - unused
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_173
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_174
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_175
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_176
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_177
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_178
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_179
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_180
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_181
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_182
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_183
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_184
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_185
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_186
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_187
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_188
    {bhvHauntedChair,                   MODEL_HAUNTED_CHAIR,                0}, // macro_haunted_chair_copy - unused
    {bhvHauntedChair,                   MODEL_HAUNTED_CHAIR,                0}, // macro_haunted_chair
    {bhvHauntedChair,                   MODEL_HAUNTED_CHAIR,                0}, // macro_haunted_chair_copy2 - unused
    {bhvGhostHuntBoo,                   MODEL_BOO,                          0}, // macro_boo - unused
    {bhvGhostHuntBoo,                   MODEL_BOO,                          0}, // macro_boo_copy - unused
    {bhvCourtyardBooTriplet,            MODEL_BOO,                          0}, // macro_boo_group - unused
    {bhvBooWithCage,                    MODEL_BOO,                          0}, // macro_boo_with_cage - unused
    {bhvAlphaBooKey,                    MODEL_BETA_BOO_KEY,                 0}, // macro_beta_key - unused
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_197
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_198
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_199
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_200
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_201
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_202
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_203
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_204
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_205
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_206
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_207
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_208
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_209
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_210
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_211
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_212
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_213
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_214
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_215
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_216
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_217
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_218
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_219
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_220
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_221
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_222
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_223
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_224
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_225
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_226
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_227
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_228
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_229
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_230
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_231
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_232
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_233
    {bhvBubSpawner,                     MODEL_NONE,                         0}, // macro_bub_spawner
    {bhvSeaweedBundle,                  MODEL_NONE,                         0}, // macro_seaweed_bundle
    {bhvBetaChestBottom,                MODEL_TREASURE_CHEST_BASE,          0}, // macro_beta_chest - unused
    {bhvBowserBomb,                     MODEL_WATER_MINE,                   0}, // macro_water_mine - unused
    {bhvFishSpawner,                    MODEL_NONE,                         FISH_SPAWNER_BP_MANY_CYAN}, // macro_fish_group_4 - unused
    {bhvFishSpawner,                    MODEL_NONE,                         FISH_SPAWNER_BP_FEW_CYAN }, // macro_fish_group_2
    {bhvJetStreamRingSpawner,           MODEL_WATER_RING,                   0}, // macro_jet_stream_ring_spawner - unused
    {bhvJetStreamRingSpawner,           MODEL_WATER_RING,                   0}, // macro_jet_stream_ring_spawner_copy - unused
    {bhvSkeeter,                        MODEL_SKEETER,                      0}, // macro_skeeter
    {bhvClamShell,                      MODEL_CLAM_SHELL,                   0}, // macro_clam_shell
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_244
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_245
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_246
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_247
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_248
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_249
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_250
    {bhvMacroUkiki,                     MODEL_UKIKI,                        UKIKI_BP_CAGE}, // macro_ukiki - unused
    {bhvMacroUkiki,                     MODEL_UKIKI,                        UKIKI_BP_CAP}, // macro_ukiki_2 - unused
    {bhvPiranhaPlant,                   MODEL_PIRANHA_PLANT,                0}, // macro_piranha_plant - unused
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_254
    {bhvSmallWhomp,                     MODEL_WHOMP,                        0}, // macro_whomp
    {bhvChainChomp,                     MODEL_CHAIN_CHOMP,                  0}, // macro_chain_chomp
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_257
    {bhvKoopa,                          MODEL_KOOPA_WITH_SHELL,             KOOPA_BP_NORMAL   }, // macro_koopa
    {bhvKoopa,                          MODEL_KOOPA_WITHOUT_SHELL,          KOOPA_BP_UNSHELLED}, // macro_koopa_shellless - unused
    {bhvWoodenPost,                     MODEL_WOODEN_POST,                  0}, // macro_wooden_post_copy - unused
    {bhvFirePiranhaPlant,               MODEL_PIRANHA_PLANT,                FIRE_PIRANHA_PLANT_BP_NORMAL}, // macro_fire_piranha_plant
    {bhvFirePiranhaPlant,               MODEL_PIRANHA_PLANT,                FIRE_PIRANHA_PLANT_BP_THI   }, // macro_fire_piranha_plant_2 - unused
    {bhvKoopa,                          MODEL_KOOPA_WITH_SHELL,             KOOPA_BP_TINY}, // macro_thi_koopa_the_quick
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_264
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_265
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_266
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_267
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_268
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_269
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_270
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_271
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_272
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_273
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_274
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_275
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_276
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_277
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_278
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_279
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_280
    {bhvMoneybagHidden,                 MODEL_YELLOW_COIN,                  0}, // macro_moneybag
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_282
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_283
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_284
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_285
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_286
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_287
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_288
    {bhvSwoop,                          MODEL_SWOOP,                        SWOOP_BP_0}, // macro_swoop
    {bhvSwoop,                          MODEL_SWOOP,                        SWOOP_BP_1}, // macro_swoop_2
    {bhvMrI,                            MODEL_NONE,                         0}, // macro_mr_i
    {bhvScuttlebugSpawn,                MODEL_NONE,                         0}, // macro_scuttlebug_spawner
    {bhvScuttlebug,                     MODEL_SCUTTLEBUG,                   0}, // macro_scuttlebug
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_294
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_295
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_296
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_297
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_298
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_299
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_300
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_301
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_302
    {bhvYellowCoin,                     MODEL_UNKNOWN_54,                   0}, // macro_unknown_303 - unused
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_304
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_305
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_306
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_307
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_308
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_309
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_310
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_311
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_312
    {bhvTTCRotatingSolid,               MODEL_TTC_ROTATING_CUBE,            TTC_ROTATING_SOLID_BP_CUBE            }, // macro_ttc_rotating_cube
    {bhvTTCRotatingSolid,               MODEL_TTC_ROTATING_PRISM,           TTC_ROTATING_SOLID_BP_TRIANGULAR_PRISM}, // macro_ttc_rotating_prism
    {bhvTTCPendulum,                    MODEL_TTC_PENDULUM,                 0}, // macro_ttc_pendulum
    {bhvTTCTreadmill,                   MODEL_TTC_LARGE_TREADMILL,          TREADMILL_BP_LARGE}, // macro_ttc_large_treadmill
    {bhvTTCTreadmill,                   MODEL_TTC_SMALL_TREADMILL,          TREADMILL_BP_SMALL}, // macro_ttc_small_treadmill
    {bhvTTCMovingBar,                   MODEL_TTC_PUSH_BLOCK,               0}, // macro_ttc_push_block
    {bhvTTCCog,                         MODEL_TTC_ROTATING_HEXAGON,         TTC_COG_BP_SHAPE_HEXAGON }, // macro_ttc_rotating_hexagon
    {bhvTTCCog,                         MODEL_TTC_ROTATING_TRIANGLE,        TTC_COG_BP_SHAPE_TRIANGLE}, // macro_ttc_rotating_triangle
    {bhvTTCPitBlock,                    MODEL_TTC_PIT_BLOCK,                TTC_PIT_BLOCK_BP_0}, // macro_ttc_pit_block
    {bhvTTCPitBlock,                    MODEL_TTC_PIT_BLOCK_UNUSED,         TTC_PIT_BLOCK_BP_1}, // macro_ttc_pit_block_2 - unused
    {bhvTTCElevator,                    MODEL_TTC_ELEVATOR_PLATFORM,        0}, // macro_ttc_elevator_platform
    {bhvTTC2DRotator,                   MODEL_TTC_CLOCK_HAND,               TTC_2D_ROTATOR_BP_HAND}, // macro_ttc_clock_hand
    {bhvTTCSpinner,                     MODEL_TTC_SPINNER,                  0}, // macro_ttc_spinner
    {bhvTTC2DRotator,                   MODEL_TTC_SMALL_GEAR,               TTC_2D_ROTATOR_BP_2D_COG}, // macro_ttc_small_gear
    {bhvTTC2DRotator,                   MODEL_TTC_LARGE_GEAR,               TTC_2D_ROTATOR_BP_2D_COG}, // macro_ttc_large_gear
    {bhvTTCTreadmill,                   MODEL_TTC_LARGE_TREADMILL,          TREADMILL_BP_UNKNOWN | TREADMILL_BP_LARGE}, // macro_ttc_large_treadmill_2
    {bhvTTCTreadmill,                   MODEL_TTC_SMALL_TREADMILL,          TREADMILL_BP_UNKNOWN | TREADMILL_BP_SMALL}, // macro_ttc_small_treadmill_2
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_330
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_331
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_332
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_333
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_334
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_335
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_336
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_337
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_338
    {bhvExclamationBox,                 MODEL_EXCLAMATION_BOX,              EXCLAMATION_BOX_BP_STAR_2}, // macro_box_star_2
    {bhvExclamationBox,                 MODEL_EXCLAMATION_BOX,              EXCLAMATION_BOX_BP_STAR_3}, // macro_box_star_3
    {bhvExclamationBox,                 MODEL_EXCLAMATION_BOX,              EXCLAMATION_BOX_BP_STAR_4}, // macro_box_star_4
    {bhvExclamationBox,                 MODEL_EXCLAMATION_BOX,              EXCLAMATION_BOX_BP_STAR_5}, // macro_box_star_5 - unused
    {bhvExclamationBox,                 MODEL_EXCLAMATION_BOX,              EXCLAMATION_BOX_BP_STAR_6}, // macro_box_star_6
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_344
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_345
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_346
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_347
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_348
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_349
    {bhvSlidingPlatform2,               MODEL_BITS_SLIDING_PLATFORM,        0}, // macro_bits_sliding_platform - unused
    {bhvSlidingPlatform2,               MODEL_BITS_TWIN_SLIDING_PLATFORMS,  0}, // macro_bits_twin_sliding_platforms - unused
    {bhvAnotherTiltingPlatform,         MODEL_BITDW_SLIDING_PLATFORM,       0}, // macro_bits_tilting_platform - unused
    {bhvOctagonalPlatformRotating,      MODEL_BITS_OCTAGONAL_PLATFORM,      0}, // macro_bits_octagonal_platform - unused
    {bhvAnimatesOnFloorSwitchPress,     MODEL_BITS_STAIRCASE,               0}, // macro_bits_staircase - unused
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_355
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_356
    {bhvFerrisWheelAxle,                MODEL_BITS_FERRIS_WHEEL_AXLE,       0}, // macro_bits_ferris_wheel_axle - unused
    {bhvActivatedBackAndForthPlatform,  MODEL_BITS_ARROW_PLATFORM,          0}, // macro_bits_arrow_platform - unused
    {bhvSeesawPlatform,                 MODEL_BITS_SEESAW_PLATFORM,         0}, // macro_bits_seesaw_platform - unused
    {bhvSeesawPlatform,                 MODEL_BITS_TILTING_W_PLATFORM,      0}, // macro_bits_tilting_w_platform - unused
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_361
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_362
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_363
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_364
#ifdef FLOOMBAS
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}, // macro_empty_365
    {bhvFloomba,                        MODEL_GOOMBA,                       GOOMBA_SIZE_REGULAR}, // macro_floomba
    {bhvFloomba,                        MODEL_GOOMBA,                       GOOMBA_SIZE_HUGE   }, // macro_huge_floomba
    {bhvFloomba,                        MODEL_GOOMBA,                       GOOMBA_SIZE_TINY   }, // macro_tiny_floomba
    {bhvFloombaTripletSpawner,          MODEL_NONE,                         0}, // macro_floomba_triplet_spawner
    {bhvFloombaTripletSpawner,          MODEL_NONE,                         8}  // macro_floomba_quintuplet_spawner - unused
#else
    {bhvYellowCoin,                     MODEL_YELLOW_COIN,                  0}  // macro_empty_365
#endif
};

#endif // MACRO_PRESETS_H
