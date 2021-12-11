#ifndef MARIO_ACTIONS_CUTSCENE_H
#define MARIO_ACTIONS_CUTSCENE_H

#include <PR/ultratypes.h>

#include "macros.h"
#include "types.h"

enum ActionStatesReadingSign { // act_reading_sign
    ACT_STATE_READING_SIGN_START_DIALOG,
    ACT_STATE_READING_SIGN_TURN_TOWARD_SIGN,
    ACT_STATE_READING_SIGN_IN_DIALOG
};

enum ActionStatesStarDance { // general_star_dance_handler, act_star_dance, act_star_dance_water
    ACT_STATE_STAR_DANCE_CUTSCENE,
    ACT_STATE_STAR_DANCE_DO_SAVE,
    ACT_STATE_STAR_DANCE_RETURN
};

enum ActionStatesQuicksandDeath { // act_quicksand_death
    ACT_STATE_QUICKSAND_DEATH_INIT,
    ACT_STATE_QUICKSAND_DEATH_SINK,
    ACT_STATE_QUICKSAND_DEATH_DEAD,
};

enum ActionStatesUnlockingStarDoor { // act_unlocking_star_door
    ACT_STATE_UNLOCKING_STAR_DOOR_MOVE_POS,
    ACT_STATE_UNLOCKING_STAR_DOOR_SUMMON_STAR,
    ACT_STATE_UNLOCKING_STAR_DOOR_APPROACH_DOOR,
    ACT_STATE_UNLOCKING_STAR_DOOR_IN_DIALOG
};

enum ActionStatesWarpDoorSpawn { // act_warp_door_spawn
    ACT_STATE_WARP_DOOR_SPAWN_SET_INTERACT_STATUS,
    ACT_STATE_WARP_DOOR_EXIT_DOOR
};

enum ActionStatesSpawnSpinAirborne { // act_spawn_spin_airborne
    ACT_STATE_SPAWN_SPIN_AIRBORNE_SPINNING,
    ACT_STATE_SPAWN_SPIN_AIRBORNE_FALLING
};

enum ActionStatesExitSaveDialog { // act_exit_land_save_dialog
    ACT_STATE_EXIT_LAND_SAVE_DIALOG_DETERMINE_TYPE,
    ACT_STATE_EXIT_LAND_SAVE_DIALOG_KEY,
    ACT_STATE_EXIT_LAND_SAVE_DIALOG_NO_CAP,
    ACT_STATE_EXIT_LAND_SAVE_DIALOG_WITH_CAP
};

enum ActionStatesBBHEnterSpin { // act_bbh_enter_spin
    ACT_STATE_BBH_ENTER_SPIN_SET_Y_VEL,
    ACT_STATE_BBH_ENTER_SPIN_SPINNING,
    ACT_STATE_BBH_ENTER_SPIN_WAIT_FOR_ANIM,
    ACT_STATE_BBH_ENTER_SPIN_DIVE,
    ACT_STATE_BBH_ENTER_SPIN_END
};

enum ActionStatesBBHEnterJump { // act_bbh_enter_jump
    ACT_STATE_BBH_ENTER_JUMP_INIT,
    ACT_STATE_BBH_ENTER_JUMP_MOVE
};

enum ActionStatesSquished { // act_squished
    ACT_STATE_SQUISHED_SCALING,
    ACT_STATE_SQUISHED_CHECK_HEIGHT,
    ACT_STATE_SQUISHED_UNSQUISH
};

enum ActionStatesJumboStarCutsceneFalling { // jumbo_star_cutscene_falling
    ACT_STATE_JUMBO_STAR_CUTSCENE_FALLING_FALL,
    ACT_STATE_JUMBO_STAR_CUTSCENE_FALLING_LAND
};

enum ActionStatesJumboStarCutsceneTakingOff { // jumbo_star_cutscene_taking_off
    ACT_STATE_JUMBO_STAR_CUTSCENE_TAKING_OFF_DANCE,
    ACT_STATE_JUMBO_STAR_CUTSCENE_TAKING_OFF_JUMPING
};


enum ActionStatesJumboStarCutsceneFlying { // jumbo_star_cutscene_flying
    ACT_STATE_JUMBO_STAR_CUTSCENE_FLYING_INIT,
    ACT_STATE_JUMBO_STAR_CUTSCENE_FLYING_ANGLES,
    ACT_STATE_JUMBO_STAR_CUTSCENE_FLYING_FALL
};

enum ActionStatesEndPeachCutsceneSummonJumboStar { // end_peach_cutscene_summon_jumbo_star
    ACT_STATE_END_PEACH_CUTSCENE_SUMMON_JUMBO_STAR_RAISE_HAND,
    ACT_STATE_END_PEACH_CUTSCENE_SUMMON_JUMBO_STAR_LOWER_HAND
};

enum ActionStatesEndPeachCutsceneDialog1 { // end_peach_cutscene_dialog_1
    ACT_STATE_END_PEACH_CUTSCENE_DIALOG_1_TAKE_OFF_CAP,
    ACT_STATE_END_PEACH_CUTSCENE_DIALOG_1_LOOK_UP
};

enum ActionStatesEndPeachCutsceneRunToCastle { // end_peach_cutscene_run_to_castle
    ACT_STATE_END_PEACH_CUTSCENE_RUN_TO_CASTLE_WALK_LOOK_UP,
    ACT_STATE_END_PEACH_CUTSCENE_RUN_TO_CASTLE_LOOK_BACK_RUN
};

enum ActionStatesEndPeachCutsceneFadeOut { // end_peach_cutscene_fade_out
    ACT_STATE_END_PEACH_CUTSCENE_FADE_OUT_WARP,
    ACT_STATE_END_PEACH_CUTSCENE_FADE_OUT_END
};

enum ActionStatesEndWavingCutscene { // act_end_waving_cutscene
    ACT_STATE_END_WAVING_CUTSCENE_INIT,
    ACT_STATE_END_WAVING_CUTSCENE_WAVING
};

enum ActionArgsMarioDialog { // set_mario_npc_dialog
    MARIO_DIALOG_STOP,
    MARIO_DIALOG_LOOK_FRONT, // no head turn
    MARIO_DIALOG_LOOK_UP,
    MARIO_DIALOG_LOOK_DOWN
};

enum DialogStates { // set_mario_npc_dialog
    MARIO_DIALOG_STATUS_NONE,
    MARIO_DIALOG_STATUS_START,
    MARIO_DIALOG_STATUS_SPEAK
};

enum EndToadAnimIndex {
    END_TOAD_INDEX_WEST,
    END_TOAD_INDEX_EAST,
};

void print_displaying_credits_entry(void);
void bhv_end_peach_loop(void);
void bhv_end_toad_loop(void);
Gfx *geo_switch_peach_eyes(s32 callContext, struct GraphNode *node, UNUSED s32 context);
s32 mario_ready_to_speak(void);
s32 set_mario_npc_dialog(s32 actionArg);
s32 mario_execute_cutscene_action(struct MarioState *m);
extern Vp sEndCutsceneVp;
extern struct CreditsEntry *sDispCreditsEntry;

#endif // MARIO_ACTIONS_CUTSCENE_H
