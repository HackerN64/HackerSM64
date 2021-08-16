struct SpawnParticlesInfo StoneForma = {
    2, 25, MODEL_MIST, -50.0f, -20, 0, 20, 40, 252, 30, 15.0f, 0.0f
};
struct SpawnParticlesInfo StoneFormb = {
    2, 25, MODEL_MIST, 60.0f, -20, 0, 20, 40, 252, 30, 15.0f, 0.0f
};

void cutscene_mario(void) {
    
    static int cutsceneTimer;
    if (gPlayer1Controller->buttonPressed & R_JPAD) {
        if (gCamera->cutscene != 2) {
            gMarioState->action = ACT_WAITING_FOR_DIALOG;
            gMarioObject->header.gfx.sharedChild = gLoadedGraphNodes[MODEL_NONE];
            gCamera->cutscene = 2;
            cutsceneTimer = 0;
        }
        else {
            gMarioState->action = ACT_IDLE;
            gMarioObject->header.gfx.sharedChild = gLoadedGraphNodes[MODEL_MARIO];
            gCamera->cutscene = 0;
        }
    }

    if (gCamera->cutscene == 2) {
        
        gLakituState.goalFocus[0] = o->oPosX;
         gLakituState.goalFocus[1] = o->oPosY;
         gLakituState.goalFocus[2] = o->oPosZ;

    if (cutsceneTimer < 40) {
        o->header.gfx.sharedChild = gLoadedGraphNodes[MODEL_CUTSCENE_MARIO];
        gLakituState.goalFocus[0] = o->oPosX;
         gLakituState.goalFocus[1] = o->oPosY - 50.0f;
         gLakituState.goalFocus[2] = o->oPosZ;
        gLakituState.goalPos[0] = o->oPosX - 150.0f;
        gLakituState.goalPos[1] = o->oPosY - 30.0f;
        gLakituState.goalPos[2] = o->oPosZ;
        if (cutsceneTimer > 15) {
            if (cutsceneTimer == 16) { 
                        cur_obj_spawn_particles(&StoneForma);
        play_sound(SOUND_STONE_PATCH_BREAK, gMarioState->marioObj->header.gfx.cameraToObject);
            }
            o->header.gfx.sharedChild = gLoadedGraphNodes[MODEL_CUTSCENE_MARIO_STONE];
        }
    }


    else if (cutsceneTimer >= 40) { 
        cur_obj_init_animation(0);
        o->header.gfx.sharedChild = gLoadedGraphNodes[MODEL_CUTSCENE_MARIO];
         gLakituState.goalFocus[0] = o->oPosX;
         gLakituState.goalFocus[1] = o->oPosY + 60.0f;
         gLakituState.goalFocus[2] = o->oPosZ;
        gLakituState.goalPos[0] = o->oPosX - 170.0f;
        gLakituState.goalPos[1] = o->oPosY + 60.0f;
        gLakituState.goalPos[2] = o->oPosZ;
        if (cutsceneTimer > 85) {
            if (cutsceneTimer == 86) { 
                        cur_obj_spawn_particles(&StoneFormb);
        play_sound(SOUND_STONE_PATCH_BREAK, gMarioState->marioObj->header.gfx.cameraToObject);
            }
            o->header.gfx.sharedChild = gLoadedGraphNodes[MODEL_CUTSCENE_MARIO_STONE];
        }
    }

    cutsceneTimer += 1;

}

}