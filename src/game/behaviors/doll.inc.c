

void cutscene_doll_loop() {
    if (gPlayer1Controller->buttonPressed & R_JPAD) {
    gCamera->cutscene = 3;
    }


if (gCamera->cutscene == 3) {
    o->oDollCutsceneTimer += 1;
    if (o->oDollCutsceneTimer < 60) {
        gLakituState.goalFocus[0] = o->oPosX;
         gLakituState.goalFocus[1] = o->oPosY + 150.0f;
         gLakituState.goalFocus[2] = o->oPosZ + 150.0f;
        gLakituState.goalPos[0] = o->oPosX - 150.0f;
        gLakituState.goalPos[1] = o->oPosY + 150.0f;
        gLakituState.goalPos[2] = o->oPosZ + 350.0f;
        gMarioState->action = ACT_WAITING_FOR_DIALOG;
        
    }
    else {
        gLakituState.goalFocus[0] = o->oPosX;
         gLakituState.goalFocus[1] = o->oPosY + 150.0f;
         gLakituState.goalFocus[2] = o->oPosZ;
        gLakituState.goalPos[0] = o->oPosX;
        gLakituState.goalPos[1] = o->oPosY + 150.0f;
        gLakituState.goalPos[2] = o->oPosZ + 250.0f;
    }
}
}

Gfx *geo_switch_doll_mouth(s32 run, struct GraphNode *node, UNUSED Mat4 *mtx) {
    struct Object *obj;
    struct GraphNodeSwitchCase *switchCase;
    if (run == TRUE) {
    obj = (struct Object *) gCurGraphNodeObject;
        switchCase = (struct GraphNodeSwitchCase *) node;
        switchCase->selectedCase = 2;
        
    //if (gCamera->cutscene == 1) {
        
        if (segmented_to_virtual(bhvCutsceneDoll) == obj->behavior && gMarioState && gMarioObject) {
            if (obj->oDollCutsceneTimer > 90) {
                switchCase->selectedCase = 1;
        }
        }
    }
    //}
    return NULL;
}