// clock_arm.inc.c

/**
 * Main loop of the hour and minute hands of the Tick Tock Clock painting.
 */
void bhv_rotating_clock_arm_loop(void) {
    u16 rollAngle = o->oFaceAngleRoll;

    if (o->oAction == TTC_PAINTING_CLOCK_ARM_WAIT) {
        // Make sure Mario not in a painting & 3 frames pass before allowing him to
        //   change the Tick Tock Clock speed setting.
        // Probably a safety check for when you leave the level through the painting
        //   to make sure the setting isn't accidentally locked in as you fly out.
        if (gEnteredPaintingObject == NULL && o->oTimer > 3) {
            o->oAction++; // TTC_PAINTING_CLOCK_ARM_ACT_MOVING
        }
    } else if (o->oAction == TTC_PAINTING_CLOCK_ARM_ACT_MOVING) {
        // If Mario is entering the Tick Tock Clock painting...
        if (gEnteredPaintingObject != NULL && GET_BPARAM1(gEnteredPaintingObject->oBehParams) == PAINTING_ID_CASTLE_TTC) {
            // And this is the minute hand...
            if (cur_obj_has_behavior(bhvClockMinuteHand)) {
                // Set Tick Tick Clock's speed based on the angle of the hand.
                // The angle actually counting down from 0xFFFF to 0 so
                //   11 o'clock is a small value and 1 o'clock is a large value.
                //! These are not perfectly symmetrical.
                if (rollAngle < 0x0AAA) { // > 345 degrees from 12 o'clock.
                    gTTCSpeedSetting = TTC_SPEED_STOPPED;
                } else if (rollAngle < 0x6AA4) { // 210..345 degrees from 12 o'clock. Should be 0x6AAA.
                    gTTCSpeedSetting = TTC_SPEED_FAST;
                } else if (rollAngle < 0x954C) { // 150..210 degrees from 12 o'clock. Should be 0x9555.
                    gTTCSpeedSetting = TTC_SPEED_RANDOM;
                } else if (rollAngle < 0xF546) { // 15..150 degrees from 12 o'clock. Should be 0xF555.
                    gTTCSpeedSetting = TTC_SPEED_SLOW;
                } else { // < 15 degrees from 12 o'clock.
                    gTTCSpeedSetting = TTC_SPEED_STOPPED;
                }
            }

            // Increment the action to stop animating the hands.
            o->oAction++; // TTC_PAINTING_CLOCK_ARM_ACT_STOPPED
        }
    } else if (o->oAction == TTC_PAINTING_CLOCK_ARM_ACT_STOPPED) {
        if (gEnteredPaintingObject == NULL) {
            o->oAction = TTC_PAINTING_CLOCK_ARM_ACT_MOVING;
        }
    }

    // Only rotate the hands until Mario enters the painting.
    if (o->oAction != TTC_PAINTING_CLOCK_ARM_ACT_STOPPED) {
        cur_obj_rotate_face_angle_using_vel();
    }
}
