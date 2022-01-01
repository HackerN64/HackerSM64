// warp.inc.c

void bhv_warp_loop(void) {
    if (o->oTimer == 0) {
        u16 radius = GET_BPARAM1(o->oBehParams);

        if (radius == 0) {
            o->hitboxRadius = 50.0f;
        } else if (radius == 0xFF) {
            o->hitboxRadius = 10000.0f;
        } else {
            o->hitboxRadius = radius * 10.0f;
        }
        o->hitboxHeight = 50.0f;
    }

    o->oInteractStatus = INT_STATUS_NONE;
}

// identical to the above function except for o->hitboxRadius
void bhv_fading_warp_loop() {
    if (o->oTimer == 0) {
        u16 radius = GET_BPARAM1(o->oBehParams);

        if (radius == 0) {
            o->hitboxRadius = 85.0f;
        } else if (radius == 0xFF) {
            o->hitboxRadius = 10000.0f;
        } else {
            o->hitboxRadius = radius * 10.0f;
        }
        o->hitboxHeight = 50.0f;
    }

    o->oInteractStatus = INT_STATUS_NONE;
}
