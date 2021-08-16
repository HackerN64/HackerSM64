void camera_controller(void) {
    if (gPlayer1Controller->buttonPressed & U_JPAD) {
        if (gCamera->cutscene != 1) {
            gMarioState->action = ACT_WAITING_FOR_DIALOG;
            gMarioObject->header.gfx.sharedChild = gLoadedGraphNodes[MODEL_NONE];
            gCamera->cutscene = 1;
        }
        else {
            gMarioState->action = ACT_IDLE;
            gMarioObject->header.gfx.sharedChild = gLoadedGraphNodes[MODEL_MARIO];
            gCamera->cutscene = 0;
        }
    }

    if (gCamera->cutscene == 1) {
        f32 sp;
        struct Object *nearestOBJ;
        //nearestOBJ = cur_obj_find_nearest_object_with_behavior(bhvRosedrift, &sp);

        gLakituState.goalPos[0] = o->oPosX;
        gLakituState.goalPos[1] = o->oPosY;
        gLakituState.goalPos[2] = o->oPosZ;
        /*
        gLakituState.goalFocus[0] = nearestOBJ->oPosX;
         gLakituState.goalFocus[1] = nearestOBJ->oPosY;
         gLakituState.goalFocus[2] = nearestOBJ->oPosZ;
*/
        o->oPosX += gPlayer1Controller->rawStickY / 12;
        o->oPosZ += gPlayer1Controller->rawStickX / 12;

        if (gPlayer1Controller->buttonDown & A_BUTTON) {
            o->oPosY += 10.0f;
        }
        if (gPlayer1Controller->buttonDown & B_BUTTON) {
            o->oPosY -= 10.0f;
        }
    }

}