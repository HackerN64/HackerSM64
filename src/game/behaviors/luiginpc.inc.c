// luigi.inc.c

static struct ObjectHitbox sLuigiHitbox = {
    /* interactType:      */ INTERACT_GRABBABLE,
    /* downOffset:        */ 0,
    /* damageOrCoinValue: */ 0,
    /* health:            */ 0,
    /* numLootCoins:      */ 0,
    /* radius:            */ 65,
    /* height:            */ 113,
    /* hurtboxRadius:     */ 0,
    /* hurtboxHeight:     */ 0,
};

void bhv_luigi_init(void) {
    o->oGravity = 2.5f;
    o->oFriction = 0.8f;
    o->oBuoyancy = 1.3f;
    o->oInteractionSubtype = INT_SUBTYPE_NPC;
}

void luigi_act_idle(void) {
    s16 animFrame = o->header.gfx.animInfo.animFrame;

    // vec3f_copy(&o->oLuigiBuddyPosCopyVec, &o->oPosVec);

    object_step();

    if (o->oDistanceToMario < 1000.0f) {
        o->oMoveAngleYaw = approach_s16_symmetric(o->oMoveAngleYaw, o->oAngleToMario, 0x140);
    }

    if (o->oInteractStatus == INT_STATUS_INTERACTED) {
        o->oAction = LUIGI_ACT_TURN_TO_TALK;
    }
}

/**
 * Function for the Bob-omb Buddy cannon guy.
 * dialogFirstText is the first dialogID called when Bob-omb Buddy
 * starts to talk to Mario to prepare the cannon(s) for him.
 * Then the camera goes to the nearest cannon, to play the "prepare cannon" cutscene
 * dialogSecondText is called after Bob-omb Buddy has the cannon(s) ready and
 * then tells Mario that is "Ready for blastoff".
 */
void luigi_cannon_dialog(s16 dialogFirstText, s16 dialogSecondText) {
    struct Object *cannonClosed;
    s16 buddyText, cutscene;

}

void luigi_act_talk(void) {

    if (set_mario_npc_dialog(MARIO_DIALOG_LOOK_FRONT) == MARIO_DIALOG_STATUS_SPEAK) {
        o->activeFlags |= ACTIVE_FLAG_INITIATED_TIME_STOP;
                if (cutscene_object_with_dialog(CUTSCENE_DIALOG, o, o->oBehParams2ndByte)
                    != BOBOMB_BP_STYPE_GENERIC) {
                    set_mario_npc_dialog(MARIO_DIALOG_STOP);{
       SAVE_FLAG_COLLECTED_LUIGI_KEY = 1;
        spawn_object(o, MODEL_BOWSER_KEY, bhvBowserKey);
        cur_obj_play_sound_2(SOUND_GENERAL2_BOWSER_KEY);
        }

                    o->activeFlags &= ~ACTIVE_FLAG_INITIATED_TIME_STOP;
                    o->oLuigiHasTalkedToMario = LUIGI_HAS_NOT_TALKED;
                    o->oInteractStatus = INT_STATUS_NONE;
                    o->oAction = LUIGI_ACT_IDLE;
                }

    if (SAVE_FLAG_COLLECTED_LUIGI_KEY == 1){
        obj_mark_for_deletion(o);
    }            
        

    }
}

void luigi_act_turn_to_talk(void) {
    s16 animFrame = o->header.gfx.animInfo.animFrame;


    o->oMoveAngleYaw = approach_s16_symmetric(o->oMoveAngleYaw, o->oAngleToMario, 0x1000);

    if ((s16) o->oMoveAngleYaw == (s16) o->oAngleToMario) {
        o->oAction = LUIGI_ACT_TALK;
    }

    cur_obj_play_sound_2(SOUND_ACTION_READ_SIGN);
}

void luigi_actions(void) {
    switch (o->oAction) {
        case LUIGI_ACT_IDLE:
            luigi_act_idle();
            break;

        case LUIGI_ACT_TURN_TO_TALK:
            luigi_act_turn_to_talk();
            break;

        case LUIGI_ACT_TALK:
            luigi_act_talk();
            break;
    }

    set_object_visibility(o, 3000);
}

void bhv_luigi_loop(void) {
    luigi_actions();

    o->oInteractStatus = INT_STATUS_NONE;
}