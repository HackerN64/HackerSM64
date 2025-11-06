#ifndef MODEL_IDS_H
#define MODEL_IDS_H

enum ModelIDs {
    MODEL_NONE,

    /* Global models that are loaded for every level */

    MODEL_MARIO,        // mario_geo
    MODEL_LUIGI,        // unused

    MODEL_BUBBLY_TREE,        // bubbly_tree_geo
    MODEL_COURTYARD_SPIKY_TREE,        // spiky_tree_geo
    MODEL_SNOW_TREE,        // snow_tree_geo
    MODEL_SSL_PALM_TREE,        // palm_tree_geo
    MODEL_CASTLE_DOOR,        // castle_door_geo
    MODEL_WOODEN_DOOR,        // wooden_door_geo
    MODEL_BBH_HAUNTED_DOOR,        // haunted_door_geo
    MODEL_METAL_DOOR,        // metal_door_geo
    MODEL_HMC_HAZY_MAZE_DOOR,        // hazy_maze_door_geo
    MODEL_CASTLE_DOOR_0_STARS,        // castle_door_0_star_geo
    MODEL_CASTLE_DOOR_1_STAR,        // castle_door_1_star_geo
    MODEL_CASTLE_DOOR_3_STARS,        // castle_door_3_stars_geo
    MODEL_CASTLE_KEY_DOOR,        // key_door_geo
    MODEL_CCM_CABIN_DOOR,        // cabin_door_geo
    MODEL_WF_TOWER_TRAPEZOID_PLATORM,        // wf_geo_000AF8 - unused
    MODEL_WF_TOWER_SQUARE_PLATORM,        // wf_geo_000B10
    MODEL_WF_TOWER_SQUARE_PLATORM_UNUSED,        // wf_geo_000B38 - unused & duplicated
    MODEL_WF_TOWER_SQUARE_PLATORM_ELEVATOR,        // wf_geo_000B60 - elevator platorm

    // Level model IDs

    // bbh
    MODEL_BBH_STAIRCASE_STEP,        // geo_bbh_0005B0
    MODEL_BBH_TILTING_FLOOR_PLATFORM,        // geo_bbh_0005C8
    MODEL_BBH_TUMBLING_PLATFORM,        // geo_bbh_0005E0
    MODEL_BBH_TUMBLING_PLATFORM_PART,        // geo_bbh_0005F8
    MODEL_BBH_MOVING_BOOKSHELF,        // geo_bbh_000610
    MODEL_BBH_MESH_ELEVATOR,        // geo_bbh_000628
    MODEL_BBH_MERRY_GO_ROUND,        // geo_bbh_000640
    MODEL_BBH_WOODEN_TOMB,        // geo_bbh_000658

    // ccm
    MODEL_CCM_ROPEWAY_LIFT,        // ccm_geo_0003D0
    MODEL_CCM_SNOWMAN_HEAD,        // ccm_geo_00040C

    // castle
    MODEL_CASTLE_BOWSER_TRAP,        // castle_geo_000F18
    MODEL_CASTLE_WATER_LEVEL_PILLAR,        // castle_geo_001940
    MODEL_CASTLE_CLOCK_MINUTE_HAND,        // castle_geo_001530
    MODEL_CASTLE_CLOCK_HOUR_HAND,        // castle_geo_001548
    MODEL_CASTLE_CLOCK_PENDULUM,        // castle_geo_001518

    // hmc
    MODEL_HMC_METAL_PLATFORM,        // hmc_geo_0005A0
    MODEL_HMC_METAL_ARROW_PLATFORM,        // hmc_geo_0005B8
    MODEL_HMC_ELEVATOR_PLATFORM,        // hmc_geo_0005D0
    MODEL_HMC_ROLLING_ROCK,        // hmc_geo_000548
    MODEL_HMC_ROCK_PIECE,        // hmc_geo_000570 - unused
    MODEL_HMC_ROCK_SMALL_PIECE,        // hmc_geo_000588 - unused
    MODEL_HMC_RED_GRILLS,        // hmc_geo_000530

    // ssl
    MODEL_SSL_PYRAMID_TOP,        // ssl_geo_000618
    MODEL_SSL_GRINDEL,        // ssl_geo_000734
    MODEL_SSL_SPINDEL,        // ssl_geo_000764
    MODEL_SSL_MOVING_PYRAMID_WALL,        // ssl_geo_000794
    MODEL_SSL_PYRAMID_ELEVATOR,        // ssl_geo_0007AC

    // bob
    MODEL_BOB_CHAIN_CHOMP_GATE,        // bob_geo_000440
    MODEL_BOB_SEESAW_PLATFORM,        // bob_geo_000458
    MODEL_BOB_BARS_GRILLS,        // bob_geo_000470

    // sl
    MODEL_SL_SNOW_TRIANGLE,        // sl_geo_000390
    MODEL_SL_CRACKED_ICE,        // sl_geo_000360 - unused
    MODEL_SL_CRACKED_ICE_CHUNK,        // sl_geo_000378 - unused

    // wdw
    MODEL_WDW_SQUARE_FLOATING_PLATFORM,        // wdw_geo_000580
    MODEL_WDW_ARROW_LIFT,        // wdw_geo_000598
    MODEL_WDW_WATER_LEVEL_DIAMOND,        // wdw_geo_0005C0
    MODEL_WDW_HIDDEN_PLATFORM,        // wdw_geo_0005E8
    MODEL_WDW_EXPRESS_ELEVATOR,        // wdw_geo_000610
    MODEL_WDW_RECTANGULAR_FLOATING_PLATFORM,        // wdw_geo_000628
    MODEL_WDW_ROTATING_PLATFORM,        // wdw_geo_000640

    // jrb
    MODEL_JRB_SHIP_LEFT_HALF_PART,        // jrb_geo_000978
    MODEL_JRB_SHIP_BACK_LEFT_PART,        // jrb_geo_0009B0
    MODEL_JRB_SHIP_RIGHT_HALF_PART,        // jrb_geo_0009E8
    MODEL_JRB_SHIP_BACK_RIGHT_PART,        // jrb_geo_000A00
    MODEL_JRB_SUNKEN_SHIP,        // jrb_geo_000990
    MODEL_JRB_SUNKEN_SHIP_BACK,        // jrb_geo_0009C8
    MODEL_JRB_ROCK,        // jrb_geo_000930
    MODEL_JRB_SLIDING_BOX,        // jrb_geo_000960
    MODEL_JRB_FALLING_PILLAR,        // jrb_geo_000900
    MODEL_JRB_FALLING_PILLAR_BASE,        // jrb_geo_000918
    MODEL_JRB_FLOATING_PLATFORM,        // jrb_geo_000948

    // thi
    MODEL_THI_HUGE_ISLAND_TOP,        // thi_geo_0005B0
    MODEL_THI_TINY_ISLAND_TOP,        // thi_geo_0005C8

    // ttc
    MODEL_TTC_ROTATING_CUBE,        // ttc_geo_000240
    MODEL_TTC_ROTATING_PRISM,        // ttc_geo_000258
    MODEL_TTC_PENDULUM,        // ttc_geo_000270
    MODEL_TTC_LARGE_TREADMILL,        // ttc_geo_000288
    MODEL_TTC_SMALL_TREADMILL,        // ttc_geo_0002A8
    MODEL_TTC_PUSH_BLOCK,        // ttc_geo_0002C8
    MODEL_TTC_ROTATING_HEXAGON,        // ttc_geo_0002E0
    MODEL_TTC_ROTATING_TRIANGLE,        // ttc_geo_0002F8
    MODEL_TTC_PIT_BLOCK,        // ttc_geo_000310 - has 2 vertical stripes
    MODEL_TTC_PIT_BLOCK_UNUSED,        // ttc_geo_000328 - has 3 vertical stripes, unused
    MODEL_TTC_ELEVATOR_PLATFORM,        // ttc_geo_000340
    MODEL_TTC_CLOCK_HAND,        // ttc_geo_000358
    MODEL_TTC_SPINNER,        // ttc_geo_000370
    MODEL_TTC_SMALL_GEAR,        // ttc_geo_000388
    MODEL_TTC_LARGE_GEAR,        // ttc_geo_0003A0

    // rr
    MODEL_RR_SLIDING_PLATFORM,        // rr_geo_0008C0
    MODEL_RR_FLYING_CARPET,        // rr_geo_000848
    MODEL_RR_OCTAGONAL_PLATFORM,        // rr_geo_0008A8
    MODEL_RR_ROTATING_BRIDGE_PLATFORM,        // rr_geo_000878
    MODEL_RR_TRIANGLE_PLATFORM,        // rr_geo_0008D8 - unused
    MODEL_RR_CRUISER_WING,        // rr_geo_000890
    MODEL_RR_SEESAW_PLATFORM,        // rr_geo_000908
    MODEL_RR_L_SHAPED_PLATFORM,        // rr_geo_000940 - unused
    MODEL_RR_SWINGING_PLATFORM,        // rr_geo_000860
    MODEL_RR_DONUT_PLATFORM,        // rr_geo_000920
    MODEL_RR_ELEVATOR_PLATFORM,        // rr_geo_0008F0
    MODEL_RR_TRICKY_TRIANGLES,        // rr_geo_000958
    MODEL_RR_TRICKY_TRIANGLES_FRAME1,        // rr_geo_000970
    MODEL_RR_TRICKY_TRIANGLES_FRAME2,        // rr_geo_000988
    MODEL_RR_TRICKY_TRIANGLES_FRAME3,        // rr_geo_0009A0
    MODEL_RR_TRICKY_TRIANGLES_FRAME4,        // rr_geo_0009B8

    // bitdw
    MODEL_BITDW_SQUARE_PLATFORM,        // geo_bitdw_000558
    MODEL_BITDW_SEESAW_PLATFORM,        // geo_bitdw_000540
    MODEL_BITDW_SLIDING_PLATFORM,        // geo_bitdw_000528
    MODEL_BITDW_FERRIS_WHEEL_AXLE,        // geo_bitdw_000570
    MODEL_BITDW_BLUE_PLATFORM,        // geo_bitdw_000588
    MODEL_BITDW_STAIRCASE_FRAME4,        // geo_bitdw_0005A0
    MODEL_BITDW_STAIRCASE_FRAME3,        // geo_bitdw_0005B8
    MODEL_BITDW_STAIRCASE_FRAME2,        // geo_bitdw_0005D0
    MODEL_BITDW_STAIRCASE_FRAME1,        // geo_bitdw_0005E8
    MODEL_BITDW_STAIRCASE,        // geo_bitdw_000600

    // vcutm
    MODEL_VCUTM_SEESAW_PLATFORM,        // vcutm_geo_0001F0

    // bitfs
    MODEL_BITFS_PLATFORM_ON_TRACK,        // bitfs_geo_000758
    MODEL_BITFS_TILTING_SQUARE_PLATFORM,        // bitfs_geo_0006C0
    MODEL_BITFS_SINKING_PLATFORMS,        // bitfs_geo_000770
    MODEL_BITFS_BLUE_POLE,        // bitfs_geo_0006A8
    MODEL_BITFS_SINKING_CAGE_PLATFORM,        // bitfs_geo_000690
    MODEL_BITFS_ELEVATOR,        // bitfs_geo_000678
    MODEL_BITFS_STRETCHING_PLATFORMS,        // bitfs_geo_000708
    MODEL_BITFS_SEESAW_PLATFORM,        // bitfs_geo_000788
    MODEL_BITFS_MOVING_SQUARE_PLATFORM,        // bitfs_geo_000728
    MODEL_BITFS_SLIDING_PLATFORM,        // bitfs_geo_000740
    MODEL_BITFS_TUMBLING_PLATFORM_PART,        // bitfs_geo_0006D8
    MODEL_BITFS_TUMBLING_PLATFORM,        // bitfs_geo_0006F0

    // bits
    MODEL_BITS_SLIDING_PLATFORM,        // bits_geo_0005E0
    MODEL_BITS_TWIN_SLIDING_PLATFORMS,        // bits_geo_0005F8
    MODEL_BITS_OCTAGONAL_PLATFORM,        // bits_geo_000610
    MODEL_BITS_BLUE_PLATFORM,        // bits_geo_000628
    MODEL_BITS_FERRIS_WHEEL_AXLE,        // bits_geo_000640
    MODEL_BITS_ARROW_PLATFORM,        // bits_geo_000658
    MODEL_BITS_SEESAW_PLATFORM,        // bits_geo_000670
    MODEL_BITS_TILTING_W_PLATFORM,        // bits_geo_000688
    MODEL_BITS_STAIRCASE,        // bits_geo_0006A0
    MODEL_BITS_STAIRCASE_FRAME1,        // bits_geo_0006B8
    MODEL_BITS_STAIRCASE_FRAME2,        // bits_geo_0006D0
    MODEL_BITS_STAIRCASE_FRAME3,        // bits_geo_0006E8
    MODEL_BITS_STAIRCASE_FRAME4,        // bits_geo_000700
    MODEL_BITS_WARP_PIPE,        // warp_pipe_geo

    // lll
    MODEL_LLL_DRAWBRIDGE_PART,        // lll_geo_000B20
    MODEL_LLL_ROTATING_BLOCK_FIRE_BARS,        // lll_geo_000B38
    MODEL_LLL_ROTATING_HEXAGONAL_RING,        // lll_geo_000BB0
    MODEL_LLL_SINKING_RECTANGULAR_PLATFORM,        // lll_geo_000BC8
    MODEL_LLL_SINKING_SQUARE_PLATFORMS,        // lll_geo_000BE0
    MODEL_LLL_TILTING_SQUARE_PLATFORM,        // lll_geo_000BF8
    MODEL_LLL_BOWSER_PIECE_1,        // lll_geo_bowser_puzzle_piece_1
    MODEL_LLL_BOWSER_PIECE_2,        // lll_geo_bowser_puzzle_piece_2
    MODEL_LLL_BOWSER_PIECE_3,        // lll_geo_bowser_puzzle_piece_3
    MODEL_LLL_BOWSER_PIECE_4,        // lll_geo_bowser_puzzle_piece_4
    MODEL_LLL_BOWSER_PIECE_5,        // lll_geo_bowser_puzzle_piece_5
    MODEL_LLL_BOWSER_PIECE_6,        // lll_geo_bowser_puzzle_piece_6
    MODEL_LLL_BOWSER_PIECE_7,        // lll_geo_bowser_puzzle_piece_7
    MODEL_LLL_BOWSER_PIECE_8,        // lll_geo_bowser_puzzle_piece_8
    MODEL_LLL_BOWSER_PIECE_9,        // lll_geo_bowser_puzzle_piece_9
    MODEL_LLL_BOWSER_PIECE_10,        // lll_geo_bowser_puzzle_piece_10
    MODEL_LLL_BOWSER_PIECE_11,        // lll_geo_bowser_puzzle_piece_11
    MODEL_LLL_BOWSER_PIECE_12,        // lll_geo_bowser_puzzle_piece_12
    MODEL_LLL_BOWSER_PIECE_13,        // lll_geo_bowser_puzzle_piece_13
    MODEL_LLL_BOWSER_PIECE_14,        // lll_geo_bowser_puzzle_piece_14
    MODEL_LLL_MOVING_OCTAGONAL_MESH_PLATFORM,        // lll_geo_000B08
    MODEL_LLL_SINKING_ROCK_BLOCK,        // lll_geo_000DD0
    MODEL_LLL_ROLLING_LOG,        // lll_geo_000DE8
    MODEL_LLL_WOOD_BRIDGE,        // lll_geo_000B50
    MODEL_LLL_LARGE_WOOD_BRIDGE,        // lll_geo_000B68
    MODEL_LLL_FALLING_PLATFORM,        // lll_geo_000B80
    MODEL_LLL_LARGE_FALLING_PLATFORM,        // lll_geo_000B98
    MODEL_LLL_VOLCANO_FALLING_TRAP,        // lll_geo_000EA8

    // ddd
    MODEL_DDD_BOWSER_SUB_DOOR,        // ddd_geo_000478
    MODEL_DDD_BOWSER_SUB,        // ddd_geo_0004A0
    MODEL_DDD_POLE,        // ddd_geo_000450

    // wf
    MODEL_WF_BREAKABLE_WALL_RIGHT,        // wf_geo_000B78
    MODEL_WF_BREAKABLE_WALL_LEFT,        // wf_geo_000B90
    MODEL_WF_KICKABLE_BOARD,        // wf_geo_000BA8
    MODEL_WF_TOWER_DOOR,        // wf_geo_000BE0
    MODEL_WF_KICKABLE_BOARD_FELLED,        // wf_geo_000BC8

    // castle grounds
    MODEL_CASTLE_GROUNDS_VCUTM_GRILL,        // castle_grounds_geo_00070C
    MODEL_CASTLE_GROUNDS_FLAG,        // castle_grounds_geo_000660
    MODEL_CASTLE_GROUNDS_CANNON_GRILL,        // castle_grounds_geo_000724

    // bowser 2
    MODEL_BOWSER_2_TILTING_ARENA,        // bowser_2_geo_000170

    // bowser 3
    MODEL_BOWSER_3_FALLING_PLATFORM_1,        // bowser_3_geo_000290
    MODEL_BOWSER_3_FALLING_PLATFORM_2,        // bowser_3_geo_0002A8
    MODEL_BOWSER_3_FALLING_PLATFORM_3,        // bowser_3_geo_0002C0
    MODEL_BOWSER_3_FALLING_PLATFORM_4,        // bowser_3_geo_0002D8
    MODEL_BOWSER_3_FALLING_PLATFORM_5,        // bowser_3_geo_0002F0
    MODEL_BOWSER_3_FALLING_PLATFORM_6,        // bowser_3_geo_000308
    MODEL_BOWSER_3_FALLING_PLATFORM_7,        // bowser_3_geo_000320
    MODEL_BOWSER_3_FALLING_PLATFORM_8,        // bowser_3_geo_000338
    MODEL_BOWSER_3_FALLING_PLATFORM_9,        // bowser_3_geo_000350
    MODEL_BOWSER_3_FALLING_PLATFORM_10,        // bowser_3_geo_000368

    // ttm
    MODEL_TTM_ROLLING_LOG,        // ttm_geo_000730
    MODEL_TTM_STAR_CAGE,        // ttm_geo_000710
    MODEL_TTM_BLUE_SMILEY,        // ttm_geo_000D14
    MODEL_TTM_YELLOW_SMILEY,        // ttm_geo_000D4C
    MODEL_TTM_STAR_SMILEY,        // ttm_geo_000D84
    MODEL_TTM_MOON_SMILEY,        // ttm_geo_000DBC

    // actor model IDs

    // first set of actor bins (0x54-0x63)
    // group 1
    MODEL_BULLET_BILL,        // bullet_bill_geo
    MODEL_YELLOW_SPHERE,        // yellow_sphere_geo
    MODEL_HOOT,        // hoot_geo
    MODEL_YOSHI_EGG,        // yoshi_egg_geo
    MODEL_THWOMP,        // thwomp_geo
    MODEL_HEAVE_HO,        // heave_ho_geo

    // group 2
    MODEL_BLARGG,        // blargg_geo
    MODEL_BULLY,        // bully_geo
    MODEL_BULLY_BOSS,        // bully_boss_geo

    // group 3
    MODEL_WATER_BOMB,        // water_bomb_geo
    MODEL_WATER_BOMB_SHADOW,        // water_bomb_shadow_geo
    MODEL_KING_BOBOMB,        // king_bobomb_geo

    // group 4
    MODEL_MANTA_RAY,        // manta_seg5_geo_05008D14
    MODEL_UNAGI,        // unagi_geo
    MODEL_SUSHI,        // sushi_geo
    MODEL_DL_WHIRLPOOL,        // whirlpool_seg5_dl_05013CB8
    MODEL_CLAM_SHELL,        // clam_shell_geo

    // group 5
    MODEL_POKEY_HEAD,        // pokey_head_geo
    MODEL_POKEY_BODY_PART,        // pokey_body_part_geo
    MODEL_TWEESTER,        // tweester_geo
    MODEL_KLEPTO,        // klepto_geo
    MODEL_EYEROK_LEFT_HAND,        // eyerok_left_hand_geo
    MODEL_EYEROK_RIGHT_HAND,        // eyerok_right_hand_geo

    // group 6
    MODEL_DL_MONTY_MOLE_HOLE,        // monty_mole_hole_seg5_dl_05000840
    MODEL_MONTY_MOLE,        // monty_mole_geo
    MODEL_UKIKI,        // ukiki_geo
    MODEL_FWOOSH,        // fwoosh_geo

    // group 7
    MODEL_SPINDRIFT,        // spindrift_geo
    MODEL_MR_BLIZZARD_HIDDEN,        // mr_blizzard_hidden_geo
    MODEL_MR_BLIZZARD,        // mr_blizzard_geo
    MODEL_PENGUIN,        // penguin_geo

    // group 8
    MODEL_CAP_SWITCH_EXCLAMATION,        // cap_switch_exclamation_seg5_dl_05002E00
    MODEL_CAP_SWITCH,        // cap_switch_geo
    MODEL_CAP_SWITCH_BASE,        // cap_switch_base_seg5_dl_05003120

    // group 9
    MODEL_BOO,        // boo_geo
    MODEL_BETA_BOO_KEY,        // small_key_geo
    MODEL_HAUNTED_CHAIR,        // haunted_chair_geo
    MODEL_MAD_PIANO,        // mad_piano_geo
    MODEL_BOOKEND_PART,        // bookend_part_geo
    MODEL_BOOKEND,        // bookend_geo
    MODEL_HAUNTED_CAGE,        // haunted_cage_geo

    // group 10
    MODEL_BIRDS,        // birds_geo
    MODEL_YOSHI,        // yoshi_geo

    // group 11
    MODEL_ENEMY_LAKITU,        // enemy_lakitu_geo
    MODEL_SPINY_BALL,        // spiny_ball_geo
    MODEL_SPINY,        // spiny_geo
    MODEL_WIGGLER_HEAD,        // wiggler_head_geo
    MODEL_WIGGLER_BODY,        // wiggler_body_geo
    MODEL_BUBBA,        // bubba_geo

    // second set of actor bins, (0x64-0x73)
    // group 12
    MODEL_BOWSER,        // bowser_geo
    MODEL_BOWSER_BOMB,        // bowser_bomb_geo - Spawns as a chill object in bowser's behavior command, causing an explosion if it touches a bomb
    MODEL_BOWSER_SMOKE,        // bowser_impact_smoke_geo
    MODEL_BOWSER_FLAMES,        // bowser_flames_geo
    MODEL_BOWSER_WAVE,        // invisible_bowser_accessory_geo
    MODEL_BOWSER_NO_SHADOW,        // bowser_geo_no_shadow

    // group 13
    MODEL_BUB,        // cheep_cheep_geo
    MODEL_TREASURE_CHEST_BASE,        // treasure_chest_base_geo
    MODEL_TREASURE_CHEST_LID,        // treasure_chest_lid_geo
    MODEL_CYAN_FISH,        // cyan_fish_geo
    MODEL_WATER_RING,        // water_ring_geo
    MODEL_SKEETER,        // skeeter_geo

    // group 14
    MODEL_PIRANHA_PLANT,        // piranha_plant_geo
    MODEL_WHOMP,        // whomp_geo
    MODEL_KOOPA_WITH_SHELL,        // koopa_with_shell_geo
    MODEL_METALLIC_BALL,        // metallic_ball_geo
    MODEL_CHAIN_CHOMP,        // chain_chomp
    MODEL_KOOPA_FLAG,        // koopa_flag_geo
    MODEL_WOODEN_POST,        // wooden_post_geo

    // group 15
    MODEL_MIPS,        // mips_geo
    MODEL_BOO_CASTLE,        // boo_castle_geo
    MODEL_LAKITU,        // lakitu_geo

    // group 16
    MODEL_CHILL_BULLY,        // chilly_chief_geo
    MODEL_BIG_CHILL_BULLY,        // chilly_chief_big_geo
    MODEL_MONEYBAG,        // moneybag_geo

    // group 17
    MODEL_SWOOP,        // swoop_geo
    MODEL_SCUTTLEBUG,        // scuttlebug_geo
    MODEL_MR_I_IRIS,        // mr_i_iris_geo
    MODEL_MR_I_BODY,        // mr_i_geo
    MODEL_MR_I, // backwards compatibility
    MODEL_DORRIE,        // dorrie_geo

    // other models
    MODEL_YELLOW_COIN,        // yellow_coin_geo
    MODEL_YELLOW_COIN_NO_SHADOW,        // yellow_coin_no_shadow_geo
    MODEL_BLUE_COIN,        // blue_coin_geo
    MODEL_BLUE_COIN_NO_SHADOW,        // blue_coin_no_shadow_geo
    MODEL_HEART,        // heart_geo
    MODEL_TRANSPARENT_STAR,        // transparent_star_geo
    MODEL_STAR,        // star_geo
    MODEL_TTM_SLIDE_EXIT_PODIUM,        // ttm_geo_000DF4
    MODEL_WOODEN_SIGNPOST,        // wooden_signpost_geo
    MODEL_CANNON_BARREL,        // cannon_barrel_geo
    MODEL_CANNON_BASE,        // cannon_base_geo
    MODEL_BREAKABLE_BOX,        // breakable_box_geo
    MODEL_BREAKABLE_BOX_NO_SHADOW,        // breakable_box_no_shadow_geo
    MODEL_BREAKABLE_BOX_SMALL, // backwards compatibility
    MODEL_EXCLAMATION_BOX_OUTLINE,        // exclamation_box_outline_geo
    MODEL_EXCLAMATION_POINT,        // exclamation_point_seg8_dl_08025F08
    MODEL_MARIOS_WINGED_METAL_CAP,        // marios_winged_metal_cap_geo
    MODEL_MARIOS_METAL_CAP,        // marios_metal_cap_geo
    MODEL_MARIOS_WING_CAP,        // marios_wing_cap_geo
    MODEL_MARIOS_CAP,        // marios_cap_geo
    MODEL_EXCLAMATION_BOX,        // exclamation_box_geo
    MODEL_DIRT_ANIMATION,        // dirt_animation_geo
    MODEL_CARTOON_STAR,        // cartoon_star_geo
    MODEL_BLUE_COIN_SWITCH,        // blue_coin_switch_geo
    MODEL_MIST,        // mist_geo
    MODEL_SPARKLES_ANIMATION,        // sparkles_animation_geo
    MODEL_RED_FLAME,        // red_flame_geo
    MODEL_BLUE_FLAME,        // blue_flame_geo
    MODEL_BURN_SMOKE,        // burn_smoke_geo
    MODEL_SPARKLES,        // sparkles_geo
    MODEL_SMOKE,        // smoke_geo
    MODEL_WHITE_PARTICLE_DL,        // white_particle_dl
    MODEL_SAND_DUST,        // sand_seg3_dl_particle
    MODEL_WHITE_PARTICLE,        // white_particle_dl
    MODEL_PEBBLE,        // pebble_seg3_dl_0301CB00
    MODEL_LEAVES,        // leaves_geo
    MODEL_WAVE_TRAIL,        // wave_trail_geo
    MODEL_WHITE_PARTICLE_SMALL,        // white_particle_small_dl
    MODEL_SMALL_WATER_SPLASH,        // small_water_splash_geo
    MODEL_IDLE_WATER_WAVE,        // idle_water_wave_geo
    MODEL_WATER_SPLASH,        // water_splash_geo
    MODEL_BUBBLE,        // bubble_geo
    MODEL_PURPLE_MARBLE,        // purple_marble_geo
    MODEL_WF_SLIDING_PLATFORM,        // wf_geo_000A98
    MODEL_WF_SMALL_BOMP,        // wf_geo_000A00
    MODEL_WF_ROTATING_WOODEN_PLATFORM,        // wf_geo_000A58
    MODEL_WF_TUMBLING_BRIDGE_PART,        // wf_geo_000AB0
    MODEL_WF_LARGE_BOMP,        // wf_geo_000A40
    MODEL_WF_TUMBLING_BRIDGE,        // wf_geo_000AC8
    MODEL_WATER_MINE,        // water_mine_geo
    MODEL_BOWLING_BALL,        // bowling_ball_geo
    MODEL_TRAMPOLINE,        // springboard_top_geo (unused)
    MODEL_TRAMPOLINE_CENTER,        // springboard_spring_geo (unused)
    MODEL_TRAMPOLINE_BASE,        // springboard_bottom_geo (unused)
    MODEL_FISH,        // fish_geo - fish without shadow, used
    MODEL_FISH_SHADOW,        // fish_shadow_geo - fish with shadow, unused
    MODEL_BUTTERFLY,        // butterfly_geo
    MODEL_BLACK_BOBOMB,        // black_bobomb_geo
    MODEL_KOOPA_SHELL,        // koopa_shell_geo
    MODEL_KOOPA_WITHOUT_SHELL,        // koopa_without_shell_geo
    MODEL_GOOMBA,        // goomba_geo
    MODEL_SEAWEED,        // seaweed_geo
    MODEL_AMP,        // dAmpGeo
    MODEL_BOBOMB_BUDDY,        // bobomb_buddy_geo
    MODEL_SSL_TOX_BOX,        // ssl_geo_000630
    MODEL_BOWSER_KEY_CUTSCENE,        // bowser_key_cutscene_geo
    MODEL_DL_CANNON_LID,        // cannon_closed_seg8_dl_080048E0
    MODEL_CHECKERBOARD_PLATFORM,        // checkerboard_platform_geo
    MODEL_RED_FLAME_SHADOW,        // red_flame_shadow_geo
    MODEL_BOWSER_KEY,        // bowser_key_geo
    MODEL_EXPLOSION,        // explosion_geo
    MODEL_SNUFIT,        // snufit_geo
    MODEL_PURPLE_SWITCH,        // purple_switch_geo
    MODEL_CASTLE_STAR_DOOR_30_STARS,        // castle_geo_000F00
    MODEL_CASTLE_STAR_DOOR_50_STARS,        // castle_geo_000F00
    MODEL_CCM_SNOWMAN_BASE,        // ccm_geo_0003F0
    MODEL_1UP,        // mushroom_1up_geo
    MODEL_CASTLE_STAR_DOOR_8_STARS,        // castle_geo_000F00
    MODEL_CASTLE_STAR_DOOR_70_STARS,        // castle_geo_000F00
    MODEL_RED_COIN,        // red_coin_geo
    MODEL_RED_COIN_NO_SHADOW,        // red_coin_no_shadow_geo
    MODEL_METAL_BOX,        // metal_box_geo
    MODEL_METAL_BOX_DL,        // metal_box_dl
    MODEL_NUMBER,        // number_geo
    MODEL_FLYGUY,        // shyguy_geo
    MODEL_TOAD,        // toad_geo
    MODEL_PEACH,        // peach_geo
    MODEL_CHUCKYA,        // chuckya_geo
    MODEL_WHITE_PUFF,        // white_puff_geo
    MODEL_TRAJECTORY_MARKER_BALL,        // bowling_ball_track_geo - duplicate used in SSL Pyramid small sized and as a track ball

#ifdef IA8_30FPS_COINS
    MODEL_SILVER_COIN,        // silver_coin_geo
    MODEL_SILVER_COIN_NO_SHADOW,        // silver_coin_no_shadow_geo
#endif

    /* Various unnamed vanilla static level models */
    MODEL_LEVEL_GEOMETRY_03,
    MODEL_LEVEL_GEOMETRY_04,
    MODEL_LEVEL_GEOMETRY_05,
    MODEL_LEVEL_GEOMETRY_06,
    MODEL_LEVEL_GEOMETRY_07,
    MODEL_LEVEL_GEOMETRY_08,
    MODEL_LEVEL_GEOMETRY_09,
    MODEL_LEVEL_GEOMETRY_0A,
    MODEL_LEVEL_GEOMETRY_0B,
    MODEL_LEVEL_GEOMETRY_0C,
    MODEL_LEVEL_GEOMETRY_0D,
    MODEL_LEVEL_GEOMETRY_0E,
    MODEL_LEVEL_GEOMETRY_0F,
    MODEL_LEVEL_GEOMETRY_10,
    MODEL_LEVEL_GEOMETRY_11,
    MODEL_LEVEL_GEOMETRY_12,
    MODEL_LEVEL_GEOMETRY_13,
    MODEL_LEVEL_GEOMETRY_14,
    MODEL_LEVEL_GEOMETRY_15,
    MODEL_LEVEL_GEOMETRY_16,
    
    // Menu Models
    MODEL_MAIN_MENU_MARIO_SAVE_BUTTON, // main_menu_geo_0001D0
    MODEL_MAIN_MENU_RED_ERASE_BUTTON, // main_menu_geo_000290
    MODEL_MAIN_MENU_BLUE_COPY_BUTTON, // main_menu_geo_0002B8
    MODEL_MAIN_MENU_YELLOW_FILE_BUTTON, // main_menu_geo_0002E0
    MODEL_MAIN_MENU_GREEN_SCORE_BUTTON, // main_menu_geo_000308
    MODEL_MAIN_MENU_MARIO_SAVE_BUTTON_FADE, // main_menu_geo_000200
    MODEL_MAIN_MENU_MARIO_NEW_BUTTON, // main_menu_geo_000230
    MODEL_MAIN_MENU_MARIO_NEW_BUTTON_FADE, // main_menu_geo_000260
    MODEL_MAIN_MENU_PURPLE_SOUND_BUTTON, // main_menu_geo_000330
    MODEL_MAIN_MENU_GENERIC_BUTTON, // main_menu_geo_000358

    // level model aliases to level geometry IDs. Possibly a relic from an older level
    // format that used to rely on level geometry objects. (seen in WF, LLL, etc)
    MODEL_LLL_ROTATING_HEXAGONAL_PLATFORM,   // lll_geo_000A78
    MODEL_WF_GIANT_POLE,   // wf_geo_000AE0
    MODEL_WF_ROTATING_PLATFORM,   // wf_geo_0009B8
    MODEL_WARP_PIPE,   // warp_pipe_geo

    MODEL_ID_COUNT,
};

#endif // MODEL_IDS_H
