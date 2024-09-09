#ifndef OBJECT_FIELDS_H
#define OBJECT_FIELDS_H

/**
 * The array [0x88, 0x1C8) in struct Object consists of fields that can vary by
 * object type. These macros provide access to these fields.
 */

#ifdef OBJECT_FIELDS_INDEX_DIRECTLY
#define OBJECT_FIELD_U32(index)           index
#define OBJECT_FIELD_S32(index)           index
#define OBJECT_FIELD_S16(index, subIndex) index
#define OBJECT_FIELD_F32(index)           index
#define OBJECT_FIELD_S16P(index)          index
#define OBJECT_FIELD_S32P(index)          index
#define OBJECT_FIELD_ANIMS(index)         index
#define OBJECT_FIELD_WAYPOINT(index)      index
#define OBJECT_FIELD_CHAIN_SEGMENT(index) index
#define OBJECT_FIELD_OBJ(index)           index
#define OBJECT_FIELD_SURFACE(index)       index
#define OBJECT_FIELD_VPTR(index)          index
#define OBJECT_FIELD_CVPTR(index)         index
#else
#define OBJECT_FIELD_U32(index)           rawData.asU32[index]
#define OBJECT_FIELD_S32(index)           rawData.asS32[index]
#define OBJECT_FIELD_S16(index, subIndex) rawData.asS16[index][subIndex]
#define OBJECT_FIELD_F32(index)           rawData.asF32[index]
#if !IS_64_BIT
#define OBJECT_FIELD_S16P(index)          rawData.asS16P[index]
#define OBJECT_FIELD_S32P(index)          rawData.asS32P[index]
#define OBJECT_FIELD_ANIMS(index)         rawData.asAnims[index]
#define OBJECT_FIELD_WAYPOINT(index)      rawData.asWaypoint[index]
#define OBJECT_FIELD_CHAIN_SEGMENT(index) rawData.asChainSegment[index]
#define OBJECT_FIELD_OBJ(index)           rawData.asObject[index]
#define OBJECT_FIELD_SURFACE(index)       rawData.asSurface[index]
#define OBJECT_FIELD_VPTR(index)          rawData.asVoidPtr[index]
#define OBJECT_FIELD_CVPTR(index)         rawData.asConstVoidPtr[index]
#else
#define OBJECT_FIELD_S16P(index)          ptrData.asS16P[index]
#define OBJECT_FIELD_S32P(index)          ptrData.asS32P[index]
#define OBJECT_FIELD_ANIMS(index)         ptrData.asAnims[index]
#define OBJECT_FIELD_WAYPOINT(index)      ptrData.asWaypoint[index]
#define OBJECT_FIELD_CHAIN_SEGMENT(index) ptrData.asChainSegment[index]
#define OBJECT_FIELD_OBJ(index)           ptrData.asObject[index]
#define OBJECT_FIELD_SURFACE(index)       ptrData.asSurface[index]
#define OBJECT_FIELD_VPTR(index)          ptrData.asVoidPtr[index]
#define OBJECT_FIELD_CVPTR(index)         ptrData.asConstVoidPtr[index]
#endif
#endif

// 0x088 (0x00), the first field, is object-specific and defined below the common fields.
/* Common fields */
#define /*0x08C*/ oFlags                      OBJECT_FIELD_U32(0x01)
#define /*0x090*/ oDialogResponse             OBJECT_FIELD_S16(0x02, 0)
#define /*0x092*/ oDialogState                OBJECT_FIELD_S16(0x02, 1)
#define /*0x094*/ oUnk94                      OBJECT_FIELD_U32(0x03)
// 0x98 unused/removed.
#define /*0x09C*/ oIntangibleTimer            OBJECT_FIELD_S32(0x05)
/* Position vector */
#define /*0x0A0*/ O_POS_INDEX                 0x06
#define /*0x0A0*/ oPosVec                     OBJECT_FIELD_F32(O_POS_INDEX)
#define /*0x0A0*/ oPosX                       OBJECT_FIELD_F32(O_POS_INDEX + 0)
#define /*0x0A4*/ oPosY                       OBJECT_FIELD_F32(O_POS_INDEX + 1)
#define /*0x0A8*/ oPosZ                       OBJECT_FIELD_F32(O_POS_INDEX + 2)
/* Velocity vector */
#define /*0x0AC*/ O_VEL_INDEX                 0x09
#define /*0x0AC*/ oVelVec                     OBJECT_FIELD_F32(O_VEL_INDEX)
#define /*0x0AC*/ oVelX                       OBJECT_FIELD_F32(O_VEL_INDEX + 0)
#define /*0x0B0*/ oVelY                       OBJECT_FIELD_F32(O_VEL_INDEX + 1)
#define /*0x0B4*/ oVelZ                       OBJECT_FIELD_F32(O_VEL_INDEX + 2)
/* Local Position vector */
#define /*0x0B8*/ O_LOCAL_VEL_INDEX                             0x0C
#define /*0x0B8*/ O_LOCAL_VEL_X_INDEX                           (O_LOCAL_VEL_INDEX + 0) // 0x0C
#define /*0x0BC*/ O_LOCAL_VEL_Y_INDEX                           (O_LOCAL_VEL_INDEX + 1) // 0x0D
#define /*0x0C0*/ O_LOCAL_VEL_Z_INDEX                           (O_LOCAL_VEL_INDEX + 2) // 0x0E
#define /*0x0B8*/ oLocalVelVec                                  OBJECT_FIELD_F32(O_LOCAL_VEL_INDEX)
#define /*0x0B8*/ oLeftVel                                      OBJECT_FIELD_F32(O_LOCAL_VEL_X_INDEX)
#define /*0x0BC*/ oUpVel                                        OBJECT_FIELD_F32(O_LOCAL_VEL_Y_INDEX)
#define /*0x0C0*/ oForwardVel                                   OBJECT_FIELD_F32(O_LOCAL_VEL_Z_INDEX) // moved
/* Move Angle vector */
#define /*0x0C4*/ O_MOVE_ANGLE_INDEX                            0x0F
#define /*0x0C4*/ O_MOVE_ANGLE_PITCH_INDEX                      (O_MOVE_ANGLE_INDEX + 0) // 0x0F
#define /*0x0C8*/ O_MOVE_ANGLE_YAW_INDEX                        (O_MOVE_ANGLE_INDEX + 1) // 0x10
#define /*0x0CC*/ O_MOVE_ANGLE_ROLL_INDEX                       (O_MOVE_ANGLE_INDEX + 2) // 0x11
#define /*0x0C4*/ oMoveAngleVec                                 OBJECT_FIELD_S32(O_MOVE_ANGLE_INDEX)
#define /*0x0C4*/ oMoveAnglePitch                               OBJECT_FIELD_S32(O_MOVE_ANGLE_PITCH_INDEX)
#define /*0x0C8*/ oMoveAngleYaw                                 OBJECT_FIELD_S32(O_MOVE_ANGLE_YAW_INDEX)
#define /*0x0CC*/ oMoveAngleRoll                                OBJECT_FIELD_S32(O_MOVE_ANGLE_ROLL_INDEX)
/* Face Angle vector */
#define /*0x0D0*/ O_FACE_ANGLE_INDEX                            0x12
#define /*0x0D0*/ O_FACE_ANGLE_PITCH_INDEX                      (O_FACE_ANGLE_INDEX + 0) // 0x12
#define /*0x0D4*/ O_FACE_ANGLE_YAW_INDEX                        (O_FACE_ANGLE_INDEX + 1) // 0x13
#define /*0x0D8*/ O_FACE_ANGLE_ROLL_INDEX                       (O_FACE_ANGLE_INDEX + 2) // 0x14
#define /*0x0D0*/ oFaceAngleVec                                 OBJECT_FIELD_S32(O_FACE_ANGLE_INDEX)
#define /*0x0D0*/ oFaceAnglePitch                               OBJECT_FIELD_S32(O_FACE_ANGLE_PITCH_INDEX)
#define /*0x0D4*/ oFaceAngleYaw                                 OBJECT_FIELD_S32(O_FACE_ANGLE_YAW_INDEX)
#define /*0x0D8*/ oFaceAngleRoll                                OBJECT_FIELD_S32(O_FACE_ANGLE_ROLL_INDEX)
/* Common fields */
#define /*0x0DC*/ oGraphYOffset                                 OBJECT_FIELD_F32(0x15)
#define /*0x0E0*/ oActiveParticleFlags                          OBJECT_FIELD_U32(0x16)
#define /*0x0E4*/ oGravity                                      OBJECT_FIELD_F32(0x17)
#define /*0x0E8*/ oFloorHeight                                  OBJECT_FIELD_F32(0x18)
#define /*0x0EC*/ oMoveFlags                                    OBJECT_FIELD_U32(0x19)
#define /*0x0F0*/ oAnimState                                    OBJECT_FIELD_S32(0x1A)
// 0x0F4-0x110 (0x1B-0x22) are object specific and defined below the common fields.
/* Angle Velocity vector */
#define /*0x114*/ O_ANGLE_VEL_INDEX                             0x23
#define /*0x114*/ O_ANGLE_VEL_PITCH_INDEX                       (O_ANGLE_VEL_INDEX + 0) // 0x23
#define /*0x118*/ O_ANGLE_VEL_YAW_INDEX                         (O_ANGLE_VEL_INDEX + 1) // 0x24
#define /*0x11C*/ O_ANGLE_VEL_ROLL_INDEX                        (O_ANGLE_VEL_INDEX + 2) // 0x25
#define /*0x114*/ oAngleVelVec                                  OBJECT_FIELD_S32(O_ANGLE_VEL_INDEX)
#define /*0x114*/ oAngleVelPitch                                OBJECT_FIELD_S32(O_ANGLE_VEL_PITCH_INDEX)
#define /*0x118*/ oAngleVelYaw                                  OBJECT_FIELD_S32(O_ANGLE_VEL_YAW_INDEX)
#define /*0x11C*/ oAngleVelRoll                                 OBJECT_FIELD_S32(O_ANGLE_VEL_ROLL_INDEX)
/* Common fields */
#define /*0x120*/ oAnimations                                   OBJECT_FIELD_ANIMS(0x26)
#define /*0x124*/ oHeldState                                    OBJECT_FIELD_U32(0x27)
#define /*0x128*/ oWallHitboxRadius                             OBJECT_FIELD_F32(0x28)
#define /*0x12C*/ oDragStrength                                 OBJECT_FIELD_F32(0x29)
#define /*0x130*/ oInteractType                                 OBJECT_FIELD_U32(0x2A)
#define /*0x134*/ oInteractStatus                               OBJECT_FIELD_S32(0x2B)
/* Parent Relative Position vector */
#define /*0x138*/ O_PARENT_RELATIVE_POS_INDEX                   0x2C
#define /*0x138*/ O_PARENT_RELATIVE_POS_X_INDEX                 (O_PARENT_RELATIVE_POS_INDEX + 0) // 0x2C
#define /*0x13C*/ O_PARENT_RELATIVE_POS_Y_INDEX                 (O_PARENT_RELATIVE_POS_INDEX + 1) // 0x2D
#define /*0x140*/ O_PARENT_RELATIVE_POS_Z_INDEX                 (O_PARENT_RELATIVE_POS_INDEX + 2) // 0x2E
#define /*0x138*/ oParentRelativePosVec                         OBJECT_FIELD_F32(O_PARENT_RELATIVE_POS_INDEX)
#define /*0x138*/ oParentRelativePosX                           OBJECT_FIELD_F32(O_PARENT_RELATIVE_POS_X_INDEX)
#define /*0x13C*/ oParentRelativePosY                           OBJECT_FIELD_F32(O_PARENT_RELATIVE_POS_Y_INDEX)
#define /*0x140*/ oParentRelativePosZ                           OBJECT_FIELD_F32(O_PARENT_RELATIVE_POS_Z_INDEX)
/* Common fields */
#define /*0x144*/ oBehParams2ndByte                             OBJECT_FIELD_S32(0x2F)
// 0x148 unused, possibly a third param byte.
#define /*0x14C*/ oAction                                       OBJECT_FIELD_S32(0x31)
#define /*0x150*/ oSubAction                                    OBJECT_FIELD_S32(0x32)
#define /*0x154*/ oTimer                                        OBJECT_FIELD_S32(0x33)
#define /*0x158*/ oBounciness                                   OBJECT_FIELD_F32(0x34)
#define /*0x15C*/ oDistanceToMario                              OBJECT_FIELD_F32(0x35)
#define /*0x160*/ oAngleToMario                                 OBJECT_FIELD_S32(0x36)
/* Home Position vector */
#define /*0x164*/ O_HOME_INDEX                                  0x37
#define /*0x164*/ O_HOME_X_INDEX                                (O_HOME_INDEX + 0) // 0x37
#define /*0x168*/ O_HOME_Y_INDEX                                (O_HOME_INDEX + 1) // 0x38
#define /*0x16C*/ O_HOME_Z_INDEX                                (O_HOME_INDEX + 2) // 0x39
#define /*0x164*/ oHomeVec                                      OBJECT_FIELD_F32(O_HOME_INDEX)
#define /*0x164*/ oHomeX                                        OBJECT_FIELD_F32(O_HOME_X_INDEX)
#define /*0x168*/ oHomeY                                        OBJECT_FIELD_F32(O_HOME_Y_INDEX)
#define /*0x16C*/ oHomeZ                                        OBJECT_FIELD_F32(O_HOME_Z_INDEX)
/* Common fields */
#define /*0x170*/ oFriction                                     OBJECT_FIELD_F32(0x3A)
#define /*0x174*/ oBuoyancy                                     OBJECT_FIELD_F32(0x3B)
#define /*0x178*/ oSoundStateID                                 OBJECT_FIELD_S32(0x3C)
#define /*0x17C*/ oOpacity                                      OBJECT_FIELD_S32(0x3D)
#define /*0x180*/ oDamageOrCoinValue                            OBJECT_FIELD_S32(0x3E)
#define /*0x184*/ oHealth                                       OBJECT_FIELD_S32(0x3F)
#define /*0x188*/ oBehParams                                    OBJECT_FIELD_U32(0x40)
#define /*0x18C*/ oPrevAction                                   OBJECT_FIELD_S32(0x41)
#define /*0x190*/ oInteractionSubtype                           OBJECT_FIELD_U32(0x42)
#define /*0x194*/ oCollisionDistance                            OBJECT_FIELD_F32(0x43)
#define /*0x198*/ oNumLootCoins                                 OBJECT_FIELD_S32(0x44)
#define /*0x19C*/ oDrawingDistance                              OBJECT_FIELD_F32(0x45)
#define /*0x1A0*/ oRoom                                         OBJECT_FIELD_S32(0x46)
// 0x1A4 is unused, possibly related to 0x1A8 in removed macro purposes.
#define /*0x1A8*/ oUnusedCoinParams                             OBJECT_FIELD_U32(0x48)
// 0x1AC-0x1B2 (0x48-0x4A) are object specific and defined below the common fields.
#define /*0x1B4*/ oWallAngle                  OBJECT_FIELD_S32(0x4B)
#define /*0x1B8*/ oFloorType                  OBJECT_FIELD_S16(0x4C, 0)
#define /*0x1BA*/ oFloorRoom                  OBJECT_FIELD_S16(0x4C, 1)
#define /*0x1BC*/ oAngleToHome                OBJECT_FIELD_S32(0x4D)
#define /*0x1C0*/ oFloor                      OBJECT_FIELD_SURFACE(0x4E)
#define /*0x1C4*/ oDeathSound                 OBJECT_FIELD_S32(0x4F)

/* Pathed (see obj_follow_path) */
#define /*0x0FC*/ oPathedStartWaypoint     OBJECT_FIELD_WAYPOINT(0x1D)
#define /*0x100*/ oPathedPrevWaypoint      OBJECT_FIELD_WAYPOINT(0x1E)
#define /*0x104*/ oPathedPrevWaypointFlags OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oPathedTargetPitch       OBJECT_FIELD_S32(0x20)
#define /*0x10C*/ oPathedTargetYaw         OBJECT_FIELD_S32(0x21)

/* Mario */
#define /*0x0F4*/ oMarioParticleFlags    OBJECT_FIELD_S32(0x1B)
#define /*0x108*/ oMarioReadingSignDYaw  OBJECT_FIELD_S32(0x20)
#define /*0x10C*/ oMarioCannonObjectYaw  OBJECT_FIELD_S32(0x21)
#define /*0x10C*/ oMarioTornadoYawVel    OBJECT_FIELD_S32(0x21)
#define /*0x10C*/ oMarioReadingSignDPosX OBJECT_FIELD_F32(0x21)
#define /*0x110*/ oMarioPolePos          OBJECT_FIELD_F32(0x22)
#define /*0x110*/ oMarioCannonInputYaw   OBJECT_FIELD_S32(0x22)
#define /*0x110*/ oMarioTornadoPosY      OBJECT_FIELD_F32(0x22)
#define /*0x110*/ oMarioReadingSignDPosZ OBJECT_FIELD_F32(0x22)
#define /*0x110*/ oMarioWhirlpoolPosY    OBJECT_FIELD_F32(0x22)
#define /*0x110*/ oMarioJumboStarCutscenePosZ OBJECT_FIELD_F32(0x22)
#define /*0x110*/ oMarioBurnTimer        OBJECT_FIELD_S32(0x22)
#define /*0x110*/ oMarioLongJumpIsSlow   OBJECT_FIELD_S32(0x22)
#define /*0x110*/ oMarioSteepJumpYaw     OBJECT_FIELD_S32(0x22)
#define /*0x110*/ oMarioWalkingPitch     OBJECT_FIELD_S32(0x22)

/* 1-Up Hidden */
#define /*0x0F4*/ o1UpHiddenTimesTriggered                      OBJECT_FIELD_S32(0x1B)

/* Activated Back and Forth Platform */
#define /*0x0F4*/ oActivatedBackAndForthPlatformMaxOffset    OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oActivatedBackAndForthPlatformOffset       OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oActivatedBackAndForthPlatformVel          OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oActivatedBackAndForthPlatformCountdown    OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oActivatedBackAndForthPlatformStartYaw     OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oActivatedBackAndForthPlatformVertical     OBJECT_FIELD_S32(0x20)
#define /*0x10C*/ oActivatedBackAndForthPlatformFlipRotation OBJECT_FIELD_S32(0x21)

/* Amp */
#define /*0x0F4*/ oAmpRadiusOfRotation OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oAmpYPhase           OBJECT_FIELD_S32(0x1C)

/* Homing Amp */
#define /*0x0F4*/ oHomingAmpLockedOn OBJECT_FIELD_S32(0x1B)
#define /*0x0FC*/ oHomingAmpAvgY     OBJECT_FIELD_F32(0x1D)

/* Arrow Lift */
#define /*0x0F4*/ oArrowLiftDisplacement OBJECT_FIELD_F32(0x1B)

/* Back-and-Forth Platform */
#define /*0x0F4*/ oBackAndForthPlatformDirection  OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oBackAndForthPlatformPathLength OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oBackAndForthPlatformDistance   OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oBackAndForthPlatformVel        OBJECT_FIELD_F32(0x1E)

/* Bird */
#define /*0x0F4*/ oBirdSpeed                OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ O_BIRD_TARGET_ANGLE_INDEX 0x1C
#define /*0x0F8*/ O_BIRD_TARGET_PITCH_INDEX (O_BIRD_TARGET_ANGLE_INDEX + 0) // 0x1C
#define /*0x0FC*/ O_BIRD_TARGET_YAW_INDEX   (O_BIRD_TARGET_ANGLE_INDEX + 1) // 0x1D
#define /*0x100*/ O_BIRD_TARGET_ROLL_INDEX  (O_BIRD_TARGET_ANGLE_INDEX + 2) // 0x1E
#define /*0x0F8*/ oBirdTargetAngleVec       OBJECT_FIELD_S32(O_BIRD_TARGET_ANGLE_INDEX)
#define /*0x0F8*/ oBirdTargetPitch          OBJECT_FIELD_S32(O_BIRD_TARGET_PITCH_INDEX)
#define /*0x0FC*/ oBirdTargetYaw            OBJECT_FIELD_S32(O_BIRD_TARGET_YAW_INDEX)
#define /*0x100*/ oBirdTargetRoll           OBJECT_FIELD_S32(O_BIRD_TARGET_ROLL_INDEX) // unused

/* Bird Chirp Chirp */
#define /*0x0F4*/ oCheepCheepSpawnerSpawnAmount OBJECT_FIELD_S32(0x1B)

/* End Birds */
#define /*0x104*/ oEndBirdCutsceneVars9PointX OBJECT_FIELD_F32(0x1F)

/* Hidden Blue Coin */
#define /*0x0F8*/ oHiddenBlueCoinSwitch OBJECT_FIELD_OBJ(0x1C)

/* Bob-omb */
#define /*0x0F4*/ oBobombBlinkTimer OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oBobombFuseLit    OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oBobombFuseTimer  OBJECT_FIELD_S32(0x1D)

/* Bob-omb Buddy */
#define /*0x0F4*/ oBobombBuddyBlinkTimer       OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oBobombBuddyHasTalkedToMario OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oBobombBuddyRole             OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oBobombBuddyCannonStatus     OBJECT_FIELD_S32(0x1E)
#define /*0x108*/ O_BOBOMB_BUDDY_POS_COPY_INDEX   0x20
#define /*0x108*/ oBobombBuddyPosCopyVec       OBJECT_FIELD_F32(O_BOBOMB_BUDDY_POS_COPY_INDEX)
#define /*0x108*/ oBobombBuddyPosXCopy         OBJECT_FIELD_F32(O_BOBOMB_BUDDY_POS_COPY_INDEX + 0)
#define /*0x10C*/ oBobombBuddyPosYCopy         OBJECT_FIELD_F32(O_BOBOMB_BUDDY_POS_COPY_INDEX + 1)
#define /*0x110*/ oBobombBuddyPosZCopy         OBJECT_FIELD_F32(O_BOBOMB_BUDDY_POS_COPY_INDEX + 2)

/* Bob-omb Explosion Bubble */
#define /*0x0FC*/ oBobombExpBubGfxScaleFacX OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oBobombExpBubGfxScaleFacY OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oBobombExpBubGfxExpRateX  OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oBobombExpBubGfxExpRateY  OBJECT_FIELD_S32(0x20)

/* Bomp (Small) */
#define /*0x100*/ oSmallBompInitX OBJECT_FIELD_F32(0x1E)

/* Boo */
#define /*0x088*/ oBooDeathStatus           OBJECT_FIELD_S32(0x00)
#define /*0x0F4*/ oBooTargetOpacity         OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oBooBaseScale             OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oBooOscillationTimer      OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oBooMoveYawDuringHit      OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oBooMoveYawBeforeHit      OBJECT_FIELD_F32(0x1F)
#define /*0x108*/ oBooParentBigBoo          OBJECT_FIELD_OBJ(0x20)
#define /*0x10C*/ oBooNegatedAggressiveness OBJECT_FIELD_F32(0x21)
#define /*0x110*/ oBooInitialMoveYaw        OBJECT_FIELD_S32(0x22)
#define /*0x1B0*/ oBooTurningSpeed          OBJECT_FIELD_S16(0x4A, 0)

/* Big Boo */
#define /*0x1AC*/ oBigBooNumMinionBoosKilled OBJECT_FIELD_S32(0x49)

/* Bookend */
#define /*0x0F4*/ oBookendTargetPitch OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oBookendTargetRoll  OBJECT_FIELD_S32(0x1C)

/* Book Switch */
#define /*0x0F4*/ oBookSwitchDistFromHome OBJECT_FIELD_F32(0x1B)

/* Book Switch Manager */
#define /*0x0F4*/ oBookSwitchManagerNumCorrectChoices OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oBookSwitchManagerIsActive          OBJECT_FIELD_S32(0x1C)

/* Haunted Bookshelf */
#define /*0x088*/ oHauntedBookshelfShouldOpen OBJECT_FIELD_S32(0x00)

/* Bouncing FireBall */
#define /*0x0F4*/ oBouncingFireBallSpawnerRandomCooldown OBJECT_FIELD_S32(0x1B)

/* Bowling Ball */
#define /*0x0F4*/ oBowlingBallTargetYaw OBJECT_FIELD_S32(0x1B)
// 0x1D-0x21 reserved for pathing

/* Bowling Ball Spawner (Generic) */
#define /*0x0F4*/ oBBallSpawnerMaxSpawnDist OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oBBallSpawnerSpawnOdds    OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oBBallSpawnerPeriodMinus1 OBJECT_FIELD_S32(0x1D)

/* Bowser */
#define /*0x088*/ oBowserCamAct          OBJECT_FIELD_S32(0x00)
#define /*0x0F4*/ oBowserStatus          OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oBowserTimer           OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oBowserDistToCenter    OBJECT_FIELD_F32(0x1D)
#define /*0x106*/ oBowserBitsJustJump    OBJECT_FIELD_S16(0x1F, 1)
#define /*0x108*/ oBowserRandSplitFloor  OBJECT_FIELD_S16(0x20, 0)
#define /*0x10A*/ oBowserHeldAnglePitch  OBJECT_FIELD_S16(0x20, 1)
#define /*0x10D*/ oBowserHeldAngleVelYaw OBJECT_FIELD_S16(0x21, 0)
#define /*0x10E*/ oBowserGrabbedStatus   OBJECT_FIELD_S16(0x21, 1)
#define /*0x110*/ oBowserIsReacting      OBJECT_FIELD_S16(0x22, 0)
#define /*0x112*/ oBowserAngleToCenter   OBJECT_FIELD_S16(0x22, 1)
#define /*0x1AC*/ oBowserTargetOpacity   OBJECT_FIELD_S16(0x49, 0)
#define /*0x1AE*/ oBowserEyesTimer       OBJECT_FIELD_S16(0x49, 1)
#define /*0x1B0*/ oBowserEyesShut        OBJECT_FIELD_S16(0x4A, 0)
#define /*0x1B2*/ oBowserRainbowLight    OBJECT_FIELD_S16(0x4A, 1)

/* Bowser Shockwave */
#define /*0x0F4*/ oBowserShockWaveScale OBJECT_FIELD_F32(0x1B)

/* Bowser Key Cutscene */
#define /*0x0F4*/ oBowserKeyScale OBJECT_FIELD_F32(0x1B)

/* Bowser Puzzle */
#define /*0x0F4*/ oBowserPuzzleCompletionFlags OBJECT_FIELD_S32(0x1B)

/* Bowser Puzzle Piece */
#define /*0x0FC*/ O_BOWSER_PUZZLE_PIECE_OFFSET_INDEX         0x1D
#define /*0x0FC*/ O_BOWSER_PUZZLE_PIECE_OFFSET_X_INDEX       (O_BOWSER_PUZZLE_PIECE_OFFSET_INDEX + 0) // 0x1D
#define /*0x100*/ O_BOWSER_PUZZLE_PIECE_OFFSET_Y_INDEX       (O_BOWSER_PUZZLE_PIECE_OFFSET_INDEX + 1) // 0x1E
#define /*0x104*/ O_BOWSER_PUZZLE_PIECE_OFFSET_Z_INDEX       (O_BOWSER_PUZZLE_PIECE_OFFSET_INDEX + 2) // 0x1F
#define /*0x0FC*/ oBowserPuzzlePieceOffsetVec                OBJECT_FIELD_F32(O_BOWSER_PUZZLE_PIECE_OFFSET_INDEX)
#define /*0x0FC*/ oBowserPuzzlePieceOffsetX                  OBJECT_FIELD_F32(O_BOWSER_PUZZLE_PIECE_OFFSET_X_INDEX)
#define /*0x100*/ oBowserPuzzlePieceOffsetY                  OBJECT_FIELD_F32(O_BOWSER_PUZZLE_PIECE_OFFSET_Y_INDEX)
#define /*0x104*/ oBowserPuzzlePieceOffsetZ                  OBJECT_FIELD_F32(O_BOWSER_PUZZLE_PIECE_OFFSET_Z_INDEX)
#define /*0x108*/ oBowserPuzzlePieceContinuePerformingAction OBJECT_FIELD_S32(0x20)
#define /*0x10C*/ oBowserPuzzlePieceActionList               OBJECT_FIELD_VPTR(0x21)
#define /*0x110*/ oBowserPuzzlePieceNextAction               OBJECT_FIELD_VPTR(0x22)

/* Bubba */
#define /*0x0F4*/ oBubbaMovePitch                  OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oBubbaRandomTimer                OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oBubbaHitWall                    OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oBubbaLungeTimer                 OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oBubbaNextTargetPitchTowardMario OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oBubbaAirVelY                    OBJECT_FIELD_F32(0x20)
#define /*0x10C*/ oBubbaJumpHeight                 OBJECT_FIELD_F32(0x21)
#define /*0x1AC*/ oBubbaTargetPitch                OBJECT_FIELD_S16(0x49, 0)
#define /*0x1AE*/ oBubbaTargetYaw                  OBJECT_FIELD_S16(0x49, 1)
#define /*0x1B0*/ oBubbaPitchVel                   OBJECT_FIELD_S16(0x4A, 0)
#define /*0x1B2*/ oBubbaYawVel                     OBJECT_FIELD_S16(0x4A, 1)

/* Bullet Bill */
#define /*0x0F8*/ oBulletBillInitialMoveYaw OBJECT_FIELD_S32(0x1C)

/* Bully (all variants) */
#define /*0x0F4*/ oBullySubtype                   OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ O_BULLY_PREV_INDEX              0x1C
#define /*0x0F8*/ O_BULLY_PREV_X_INDEX            (O_BULLY_PREV_INDEX + 0) // 0x1C
#define /*0x0FC*/ O_BULLY_PREV_Y_INDEX            (O_BULLY_PREV_INDEX + 1) // 0x1D
#define /*0x100*/ O_BULLY_PREV_Z_INDEX            (O_BULLY_PREV_INDEX + 2) // 0x1E
#define /*0x0F8*/ oBullyPrevVec                   OBJECT_FIELD_F32(O_BULLY_PREV_INDEX)
#define /*0x0F8*/ oBullyPrevX                     OBJECT_FIELD_F32(O_BULLY_PREV_X_INDEX)
#define /*0x0FC*/ oBullyPrevY                     OBJECT_FIELD_F32(O_BULLY_PREV_Y_INDEX)
#define /*0x100*/ oBullyPrevZ                     OBJECT_FIELD_F32(O_BULLY_PREV_Z_INDEX)
#define /*0x104*/ oBullyKBTimerAndMinionKOCounter OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oBullyMarioCollisionAngle       OBJECT_FIELD_S32(0x20)

/* Butterfly */
#define /*0x0F4*/ oButterflyYPhase                              OBJECT_FIELD_S32(0x1B)

/* Triplet Butterfly */
#define /*0x0F4*/ oTripletButterflyScale             OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oTripletButterflySpeed             OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oTripletButterflyBaseYaw           OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oTripletButterflyTargetPitch       OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oTripletButterflyTargetYaw         OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oTripletButterflyType              OBJECT_FIELD_S32(0x20)
#define /*0x10C*/ oTripletButterflyModel             OBJECT_FIELD_S32(0x21)
#define /*0x110*/ oTripletButterflySelectedButterfly OBJECT_FIELD_S32(0x22)
#define /*0x1AC*/ oTripletButterflyScalePhase        OBJECT_FIELD_S32(0x49)

/* Cannon */
#define /*0x0F4*/ oCannonAngle              OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oCannonTimeSinceActivated OBJECT_FIELD_S32(0x1C)
#define /*0x10C*/ oCannonIsActive           OBJECT_FIELD_S32(0x21)

/* Cap */
#define /*0x0F4*/ oCapDoScaleVertically OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oCapScaleAngle        OBJECT_FIELD_S32(0x1C)

/* Chain Chomp */
#define /*0x0F4*/ oChainChompSegments                     OBJECT_FIELD_CHAIN_SEGMENT(0x1B)
#define /*0x0F8*/ oChainChompMaxDistFromPivotPerChainPart OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oChainChompMaxDistBetweenChainParts     OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oChainChompDistToPivot                  OBJECT_FIELD_F32(0x1E)
#define /*0x104*/ oChainChompSignedMaxDistBetweenChainParts OBJECT_FIELD_F32(0x1F)
#define /*0x108*/ oChainChompRestrictedByChain            OBJECT_FIELD_S32(0x20)
#define /*0x10C*/ oChainChompTargetPitch                  OBJECT_FIELD_S32(0x21)
#define /*0x110*/ oChainChompNumLunges                    OBJECT_FIELD_S32(0x22)
#define /*0x1AC*/ oChainChompReleaseStatus                OBJECT_FIELD_S32(0x49)
#define /*0x1B0*/ oChainChompHitGate                      OBJECT_FIELD_S32(0x4A)

/* Checkerboard Platform */
#define /*0x0F8*/ oCheckerBoardPlatformRotateAction OBJECT_FIELD_S32(0x1C) // oAction like
#define /*0x0FC*/ oCheckerBoardPlatformHeight       OBJECT_FIELD_S32(0x1D)
#define /*0x1AC*/ oCheckerBoardPlatformRadius       OBJECT_FIELD_F32(0x49)

/* Cheep Cheep */
#define /*0x0F4*/ oCheepCheepWaterLevel     OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oCheepCheepTargetY        OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oCheepCheepTargetYOffset  OBJECT_FIELD_F32(0x1D)
#define /*0x104*/ oCheepCheepMaxYOffset     OBJECT_FIELD_F32(0x1F)
#define /*0x108*/ oCheepCheepRandomSwimAway OBJECT_FIELD_F32(0x20)

/* Chuckya */
#define /*0x088*/ oCommonAnchorAction            OBJECT_FIELD_S32(0x00)
#define /*0x0F8*/ oChuckyaUnused                 OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oChuckyaSubActionTimer         OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oChuckyaNumPlayerEscapeActions OBJECT_FIELD_S32(0x1E)

/* Clam */
#define /*0x0F4*/ oClamShakeTimer OBJECT_FIELD_S32(0x1B)

/* Cloud */
#define /*0x0F4*/ oCloudCenterX              OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oCloudCenterY              OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oCloudBlowing              OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oCloudGrowSpeed            OBJECT_FIELD_F32(0x1E)
#define /*0x1AC*/ oCloudFwooshMovementRadius OBJECT_FIELD_S16(0x49, 0)

/* Coin */
#define /*0x0F4*/ oCoinRespawnBits  OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oCoinSnapToGround OBJECT_FIELD_S32(0x1C)
#define /*0x110*/ oCoinBaseYVel     OBJECT_FIELD_F32(0x22)
#define /*0x1B0*/ oCoinBounceTimer  OBJECT_FIELD_S32(0x4A)

/* Collision Particle */
#define /*0x0F4*/ oCollisionParticleScale OBJECT_FIELD_F32(0x1B)

/* Controllable Platform */
#define /*0x0F8*/ oControllablePlatformWallHitDirection OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oControllablePlatformInitPosY         OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oControllablePlatformIsFarFromMario   OBJECT_FIELD_S32(0x1E)

/* Breakable Box Small (Small Cork Box) */
#define /*0x0F4*/ oBreakableBoxSmallReleased            OBJECT_FIELD_S32(0x1B)
#define /*0x0FC*/ oBreakableBoxSmallFramesSinceReleased OBJECT_FIELD_S32(0x1D)

/* Jumping Box (Crazy Box) */
#define /*0x0F4*/ oJumpingBoxUnusedTimerMin OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oJumpingBoxRandomTimer    OBJECT_FIELD_S32(0x1C)

/* RR Cruiser Wing */
#define /*0x0F4*/ oRRCruiserWingInitYaw   OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oRRCruiserWingInitPitch OBJECT_FIELD_S32(0x1C)

/* Donut Platform Spawner */
#define /*0x0F4*/ oDonutPlatformSpawnerSpawnedPlatforms OBJECT_FIELD_S32(0x1B)

/* Door */
#define /*0x088*/ oDoorIsRendering  OBJECT_FIELD_S32(0x00)
#define /*0x0F8*/ oDoorSelfRoom     OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oDoorForwardRoom  OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oDoorBackwardRoom OBJECT_FIELD_S32(0x1E)

/* Dorrie */
#define /*0x0F4*/ oDorrieDistToHome         OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oDorrieOffsetY            OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oDorrieVelY               OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oDorrieForwardDistToMario OBJECT_FIELD_F32(0x1E)
#define /*0x104*/ oDorrieYawVel             OBJECT_FIELD_S32(0x1F)
#define /*0x10C*/ oDorrieLiftingMario       OBJECT_FIELD_S32(0x21)
#define /*0x1AC*/ oDorrieGroundPounded      OBJECT_FIELD_S16(0x49, 0)
#define /*0x1AE*/ oDorrieAngleToHome        OBJECT_FIELD_S16(0x49, 1)
#define /*0x1B0*/ oDorrieNeckAngle          OBJECT_FIELD_S16(0x4A, 0)
#define /*0x1B2*/ oDorrieHeadRaiseSpeed     OBJECT_FIELD_S16(0x4A, 1)

/* Elevator */
#define /*0x0F4*/ oElevatorMinY OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oElevatorMaxY OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oElevatorMidY OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oElevatorType OBJECT_FIELD_S32(0x1E)

/* Exclamation Box */
#define /*0x0F4*/ oExclamationBoxHorizontalScale OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oExclamationBoxVerticalScale   OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oExclamationBoxScaleAngle      OBJECT_FIELD_S32(0x1D)

/* Eyerok Boss */
#define /*0x0F8*/ oEyerokBossNumHands         OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oEyerokBossAttackPhase      OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oEyerokBossActiveHand       OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oEyerokBossAttackCountdown  OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oEyerokBossFightSideZ       OBJECT_FIELD_F32(0x20)
#define /*0x10C*/ oEyerokBossClampedMarioPosZ OBJECT_FIELD_F32(0x21)
#define /*0x110*/ oEyerokBossOffsetFromHome   OBJECT_FIELD_F32(0x22)
#define /*0x1AC*/ oEyerokBossActiveHandId     OBJECT_FIELD_S32(0x49)

/* Eyerok Hand */
#define /*0x0F4*/ oEyerokHandWakeUpTimer    OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oEyerokReceivedAttack     OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oEyerokHandAnimStateIndex OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oEyerokHandEyeShownTimer  OBJECT_FIELD_S32(0x1E)

/* Falling Pillar */
#define /*0x0F4*/ oFallingPillarPitchAcceleration OBJECT_FIELD_F32(0x1B)

/* Fire Spitter */
#define /*0x0F4*/ oFireSpitterScaleVel OBJECT_FIELD_F32(0x1B)

/* Blue Fish */
#define /*0x0F4*/ oBlueFishRandomVel   OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oBlueFishRandomTime  OBJECT_FIELD_S32(0x1C)
#define /*0x100*/ oBlueFishRandomAngle OBJECT_FIELD_F32(0x1E)

/* Fish Group */
#define /*0x0F4*/ oFishWaterLevel     OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oFishGoalY          OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oFishHeightOffset   OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oFishYawVel         OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oFishRoamDistance   OBJECT_FIELD_F32(0x1F)
#define /*0x108*/ oFishGoalVel        OBJECT_FIELD_F32(0x20)
#define /*0x10C*/ oFishDepthDistance  OBJECT_FIELD_F32(0x21)
#define /*0x110*/ oFishActiveDistance OBJECT_FIELD_F32(0x22)

/* Flame */
#define /*0x0F4*/ oFlameScale            OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oFlameSpeedTimerOffset OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oFlameUnusedRand       OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oFlameBowser           OBJECT_FIELD_OBJ(0x1E)

/* Blue Flame */
#define /*0x0F8*/ oBlueFlameNextScale OBJECT_FIELD_F32(0x1C)

/* Small Piranha Flame */
#define /*0x0F4*/ oSmallPiranhaFlameStartSpeed     OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oSmallPiranhaFlameEndSpeed       OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oSmallPiranhaFlameModel          OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oSmallPiranhaFlameNextFlameTimer OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oSmallPiranhaFlameSpeed          OBJECT_FIELD_F32(0x1F)

/* Moving Flame */
#define /*0x0F4*/ oMovingFlameTimer OBJECT_FIELD_S32(0x1B)

/* Flamethrower */
#define /*0x110*/ oFlameThowerTimeRemaining OBJECT_FIELD_S32(0x22)

/* Floating Platform */
#define /*0x0F4*/ oFloatingPlatformIsOnFloor                OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oFloatingPlatformMarioWeightWobbleOffset  OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oFloatingPlatformHeightOffset             OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oFloatingPlatformWaterSurfaceWobbleOffset OBJECT_FIELD_S32(0x1E)

/* Floor Switch Press Animation */
#define /*0x0F4*/ oFloorSwitchPressAnimationTickTimer        OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oFloorSwitchPressAnimationDoubleFrame      OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oFloorSwitchPressAnimationDoResetTime      OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oFloorSwitchPressAnimationSwitchNotTicking OBJECT_FIELD_S32(0x1E)

/* Fly Guy */
#define /*0x0F4*/ oFlyGuyIdleTimer        OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oFlyGuyOscTimer         OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oFlyGuyUnusedJitter     OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oFlyGuyLungeYDecel      OBJECT_FIELD_F32(0x1E)
#define /*0x104*/ oFlyGuyLungeTargetPitch OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oFlyGuyTargetRoll       OBJECT_FIELD_S32(0x20)
#define /*0x10C*/ oFlyGuyScaleVel         OBJECT_FIELD_F32(0x21)

/* Grand Star */
#define /*0x108*/ oGrandStarArcTime OBJECT_FIELD_S32(0x20)

/* Horizontal Grindel */
#define /*0x0F4*/ oHorizontalGrindelTargetYaw  OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oHorizontalGrindelDistToHome OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oHorizontalGrindelOnGround   OBJECT_FIELD_S32(0x1D)

/* Goomba */
#define /*0x0F4*/ oGoombaSize                OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oGoombaScale               OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oGoombaWalkTimer           OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oGoombaTargetYaw           OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oGoombaBlinkTimer          OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oGoombaTurningAwayFromWall OBJECT_FIELD_S32(0x20)
#define /*0x10C*/ oGoombaRelativeSpeed       OBJECT_FIELD_F32(0x21)
#ifdef FLOOMBAS
#define /*0x110*/ oIsFloomba OBJECT_FIELD_S32(0x22)

#ifdef INTRO_FLOOMBAS
#define /*0x1AC*/ oZoomCounter OBJECT_FIELD_U32(0x49)
#define /*0x1B0*/ oZoomPosZ    OBJECT_FIELD_F32(0x4A)
#endif
#endif

/* Haunted Chair */
#define /*0x0F4*/ oHauntedChairSpinTimer          OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oHauntedChairPitchVel           OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oHauntedChairRollVel            OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oHauntedChairFallFromPianoAngle OBJECT_FIELD_S32P(0x1E)
#define /*0x104*/ oHauntedChairFallTargetAngle    OBJECT_FIELD_S32(0x1F)

/* Heave-Ho */
#define /*0x088*/ oHeaveHoThrowState OBJECT_FIELD_S32(0x00)
#define /*0x0F4*/ oHeaveHoTimedSpeed OBJECT_FIELD_F32(0x1B)

/* Hidden Object */
#define /*0x0F4*/ oHiddenObjectSwitchObj OBJECT_FIELD_OBJ(0x1B)

/* Hoot */
#define /*0x0F4*/ oHootAvailability     OBJECT_FIELD_S32(0x1B)
#define /*0x110*/ oHootMarioReleaseTime OBJECT_FIELD_S32(0x22)

/* Horizontal Movement */
#define /*0x0F4*/ oLllOctMeshWaveTimer       OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oLllOctMeshActionOffset    OBJECT_FIELD_S32(0x1C)
#define /*0x100*/ oLllOctMeshWaveYOffset     OBJECT_FIELD_F32(0x1E)
#define /*0x104*/ oLllOctMeshStandTimer      OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oLllOctMeshYOffsetFromHome OBJECT_FIELD_F32(0x20)

/* Kickable Board */
#define /*0x0F4*/ oKickableBoardRockingAngleAmount OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oKickableBoardRockingTimer       OBJECT_FIELD_S32(0x1C)

 /* King Bob-omb */
#define /*0x088*/ oKingBobombHoldingMarioState        OBJECT_FIELD_S32(0x00)
#define /*0x0F8*/ oKingBobombIsJumping                OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oKingBobombPlayerGrabEscapeActions  OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oKingBobombShouldStomp              OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oKingBobombStationaryTimer          OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oKingBobombPlayerGrabEscapeCooldown OBJECT_FIELD_S32(0x20)
#define /*0x10C*/ oKingBobombBlinkTimer               OBJECT_FIELD_S32(0x21)

/* Klepto */
#define /*0x0F4*/ oKleptoDistanceToTarget      OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oKleptoHomeYOffset           OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oKleptoHalfLateralDistToHome OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oKleptoSpeed                 OBJECT_FIELD_F32(0x1E)
#define /*0x104*/ O_KLEPTO_START_POS_INDEX     0x1F
#define /*0x104*/ O_KLEPTO_START_POS_X_INDEX   (O_KLEPTO_START_POS_INDEX + 0) // 0x1F
#define /*0x108*/ O_KLEPTO_START_POS_Y_INDEX   (O_KLEPTO_START_POS_INDEX + 1) // 0x20
#define /*0x10C*/ O_KLEPTO_START_POS_Z_INDEX   (O_KLEPTO_START_POS_INDEX + 2) // 0x21
#define /*0x104*/ oKleptoStartPosVec           OBJECT_FIELD_F32(O_KLEPTO_START_POS_INDEX)
#define /*0x104*/ oKleptoStartPosX             OBJECT_FIELD_F32(O_KLEPTO_START_POS_X_INDEX)
#define /*0x108*/ oKleptoStartPosY             OBJECT_FIELD_F32(O_KLEPTO_START_POS_Y_INDEX)
#define /*0x10C*/ oKleptoStartPosZ             OBJECT_FIELD_F32(O_KLEPTO_START_POS_Z_INDEX)
#define /*0x110*/ oKleptoTimeUntilTargetChange OBJECT_FIELD_S32(0x22)
#define /*0x1AC*/ oKleptoTargetNumber          OBJECT_FIELD_S16(0x49, 0)
#define /*0x1AE*/ oKleptoDiveTimer             OBJECT_FIELD_S16(0x49, 1)
#define /*0x1B0*/ oKleptoPitchToTarget         OBJECT_FIELD_S16(0x4A, 0)
#define /*0x1B2*/ oKleptoYawToTarget           OBJECT_FIELD_S16(0x4A, 1)

/* Koopa */
#define /*0x0F4*/ oKoopaAgility                     OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oKoopaMovementType                OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oKoopaTargetYaw                   OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oKoopaUnshelledTimeUntilTurn      OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oKoopaTurningAwayFromWall         OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oKoopaDistanceToMario             OBJECT_FIELD_F32(0x20)
#define /*0x10C*/ oKoopaAngleToMario                OBJECT_FIELD_S32(0x21)
#define /*0x110*/ oKoopaBlinkTimer                  OBJECT_FIELD_S32(0x22)
#define /*0x1AC*/ oKoopaCountdown                   OBJECT_FIELD_S16(0x49, 0)
#define /*0x1AE*/ oKoopaTheQuickRaceIndex           OBJECT_FIELD_S16(0x49, 1)
#define /*0x1B0*/ oKoopaTheQuickInitTextboxCooldown OBJECT_FIELD_S16(0x4A, 0)
// 0x1D-0x21 for koopa the quick reserved for pathing

/* Koopa Race Endpoint */
#define /*0x0F4*/ oKoopaRaceEndpointRaceBegun     OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oKoopaRaceEndpointKoopaFinished OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oKoopaRaceEndpointRaceStatus    OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oKoopaRaceEndpointDialog        OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oKoopaRaceEndpointRaceEnded     OBJECT_FIELD_S32(0x1F)

/* Koopa Shell Flame */
#define /*0x0F4*/ oKoopaShellFlameUnused OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oKoopaShellFlameScale  OBJECT_FIELD_F32(0x1C)

/* Camera Lakitu */
#define /*0x0F4*/ oCameraLakituBlinkTimer     OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oCameraLakituSpeed          OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oCameraLakituCircleRadius   OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oCameraLakituFinishedDialog OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oCameraLakituIntroMusicPlayed OBJECT_FIELD_S32(0x1F)
#define /*0x1AC*/ oCameraLakituPitchVel       OBJECT_FIELD_S16(0x49, 0)
#define /*0x1AE*/ oCameraLakituYawVel         OBJECT_FIELD_S16(0x49, 1)

/* Evil Lakitu */
#define /*0x0F4*/ oEnemyLakituNumSpinies           OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oEnemyLakituBlinkTimer           OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oEnemyLakituSpinyCooldown        OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oEnemyLakituFaceForwardCountdown OBJECT_FIELD_S32(0x1E)

/* Intro Cutscene Lakitu */
#define /*0x0F8*/ oIntroLakituSplineSegmentProgress OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oIntroLakituSplineSegment         OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oIntroLakituDistToBirdsX          OBJECT_FIELD_F32(0x1E)
#define /*0x104*/ oIntroLakituDistToBirdsZ          OBJECT_FIELD_F32(0x1F)
#define /*0x108*/ O_INTRO_LAKITU_END_BIRDS_DEST_INDEX   0x20
#define /*0x108*/ O_INTRO_LAKITU_END_BIRDS_DEST_X_INDEX (O_INTRO_LAKITU_END_BIRDS_DEST_INDEX + 0) // 0x20
#define /*0x10C*/ O_INTRO_LAKITU_END_BIRDS_DEST_Y_INDEX (O_INTRO_LAKITU_END_BIRDS_DEST_INDEX + 1) // 0x21
#define /*0x110*/ O_INTRO_LAKITU_END_BIRDS_DEST_Z_INDEX (O_INTRO_LAKITU_END_BIRDS_DEST_INDEX + 2) // 0x22
#define /*0x108*/ oIntroLakituEndBirds1DestVec      OBJECT_FIELD_F32(O_INTRO_LAKITU_END_BIRDS_DEST_INDEX)
#define /*0x110*/ oIntroLakituEndBirds1DestX        OBJECT_FIELD_F32(O_INTRO_LAKITU_END_BIRDS_DEST_X_INDEX)
#define /*0x10C*/ oIntroLakituEndBirds1DestY        OBJECT_FIELD_F32(O_INTRO_LAKITU_END_BIRDS_DEST_Y_INDEX)
#define /*0x108*/ oIntroLakituEndBirds1DestZ        OBJECT_FIELD_F32(O_INTRO_LAKITU_END_BIRDS_DEST_Z_INDEX)
#define /*0x1AC*/ oIntroLakituCloud                 OBJECT_FIELD_OBJ(0x49)

/* Main Menu Button */
#define /*0x0F4*/ oMenuButtonState       OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oMenuButtonTimer       OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ O_MENU_BUTTON_ORIG_POS_INDEX   0x1D
#define /*0x0FC*/ O_MENU_BUTTON_ORIG_POS_X_INDEX (O_MENU_BUTTON_ORIG_POS_INDEX + 0) // 0x1D
#define /*0x100*/ O_MENU_BUTTON_ORIG_POS_Y_INDEX (O_MENU_BUTTON_ORIG_POS_INDEX + 1) // 0x1E
#define /*0x104*/ O_MENU_BUTTON_ORIG_POS_Z_INDEX (O_MENU_BUTTON_ORIG_POS_INDEX + 2) // 0x1F
#define /*0x0FC*/ oMenuButtonOrigPosVec  OBJECT_FIELD_F32(O_MENU_BUTTON_ORIG_POS_INDEX)
#define /*0x0FC*/ oMenuButtonOrigPosX    OBJECT_FIELD_F32(O_MENU_BUTTON_ORIG_POS_X_INDEX)
#define /*0x100*/ oMenuButtonOrigPosY    OBJECT_FIELD_F32(O_MENU_BUTTON_ORIG_POS_Y_INDEX)
#define /*0x104*/ oMenuButtonOrigPosZ    OBJECT_FIELD_F32(O_MENU_BUTTON_ORIG_POS_Z_INDEX)
#define /*0x108*/ oMenuButtonScale       OBJECT_FIELD_F32(0x20)
#define /*0x10C*/ oMenuButtonActionPhase OBJECT_FIELD_S32(0x21)

/* Manta Ray */
#define /*0x0F4*/ O_MANTA_TARGET_ANGLE_INDEX 0x1B
#define /*0x0F4*/ O_MANTA_TARGET_PITCH_INDEX (O_MANTA_TARGET_ANGLE_INDEX + 0) // 0x1B
#define /*0x0F8*/ O_MANTA_TARGET_YAW_INDEX   (O_MANTA_TARGET_ANGLE_INDEX + 1) // 0x1C
#define /*0x0FC*/ O_MANTA_TARGET_ROLL_INDEX  (O_MANTA_TARGET_ANGLE_INDEX + 2) // 0x1D
#define /*0x0F4*/ oMantaTargetAngleVec       OBJECT_FIELD_S32(O_MANTA_TARGET_ANGLE_INDEX)
#define /*0x0F4*/ oMantaTargetPitch          OBJECT_FIELD_S32(O_MANTA_TARGET_PITCH_INDEX)
#define /*0x0F8*/ oMantaTargetYaw            OBJECT_FIELD_S32(O_MANTA_TARGET_YAW_INDEX)
#define /*0x0FC*/ oMantaTargetRoll           OBJECT_FIELD_S32(O_MANTA_TARGET_ROLL_INDEX) // unused

/* Merry-Go-Round */
#define /*0x088*/ oMerryGoRoundStopped         OBJECT_FIELD_S32(0x00)
#define /*0x0F8*/ oMerryGoRoundMusicShouldPlay OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oMerryGoRoundMarioIsOutside  OBJECT_FIELD_S32(0x1D)

/* Merry-Go-Round Boo Manager */
#define /*0x088*/ oMerryGoRoundBooManagerNumBoosKilled  OBJECT_FIELD_S32(0x00)
#define /*0x0FC*/ oMerryGoRoundBooManagerNumBoosSpawned OBJECT_FIELD_S32(0x1D)

/* Mips */
#define /*0x0F4*/ oMipsStarStatus         OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oMipsStartWaypointIndex OBJECT_FIELD_S32(0x1C)
 // 0x1D-0x21 reserved for pathing
#define /*0x1AC*/ oMipsForwardVelocity    OBJECT_FIELD_F32(0x49)

/* Moneybag */
#define /*0x0F4*/ oMoneybagJumpState OBJECT_FIELD_S32(0x1B)

/* Monty Mole */
#define /*0x0F4*/ oMontyMoleCurrentHole           OBJECT_FIELD_OBJ(0x1B)
#define /*0x0F8*/ oMontyMoleHeightRelativeToFloor OBJECT_FIELD_F32(0x1C)

/* Monty Mole Hole */
#define /*0x0F4*/ oMontyMoleHoleCooldown OBJECT_FIELD_S32(0x1B)

/* Mr. Blizzard */
#define /*0x0F4*/ oMrBlizzardScale             OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oMrBlizzardHeldObj           OBJECT_FIELD_OBJ(0x1C)
#define /*0x0FC*/ oMrBlizzardGraphYVel         OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oMrBlizzardTimer             OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oMrBlizzardDizziness         OBJECT_FIELD_F32(0x1F)
#define /*0x108*/ oMrBlizzardChangeInDizziness OBJECT_FIELD_F32(0x20)
#define /*0x10C*/ oMrBlizzardGraphYOffset      OBJECT_FIELD_F32(0x21)
#define /*0x110*/ oMrBlizzardDistFromHome      OBJECT_FIELD_S32(0x22)
#define /*0x1AC*/ oMrBlizzardTargetMoveYaw     OBJECT_FIELD_S32(0x49)

/* Mr. I */
#define /*0x0F4*/ oMrISpinAngle           OBJECT_FIELD_S32(0x1B)
#define /*0x0FC*/ oMrISpinAmount          OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oMrISpinDirection       OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oMrIParticleTimer       OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oMrIParticleTimerTarget OBJECT_FIELD_S32(0x20)
#define /*0x10C*/ oMrIScale               OBJECT_FIELD_F32(0x21)
#define /*0x110*/ oMrIBlinking            OBJECT_FIELD_S32(0x22)

/* Object Respawner */
#define /*0x0F4*/ oRespawnerModelToRespawn    OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oRespawnerMinSpawnDist      OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oRespawnerBehaviorToRespawn OBJECT_FIELD_CVPTR(0x1D)

/* Orange Number */
#define /*0x110*/ oOrangeNumberOffset OBJECT_FIELD_S32(0x22)

/* Openable Grill */
#define /*0x088*/ oOpenableGrillIsOpen         OBJECT_FIELD_S32(0x00)
#define /*0x0F4*/ oOpenableGrillFloorSwitchObj OBJECT_FIELD_OBJ(0x1B)

/* Intro Cutscene Peach */
#define /*0x108*/ oIntroPeachYawFromFocus OBJECT_FIELD_F32(0x20)
#define /*0x10C*/ oIntroPeachPitchFromFocus OBJECT_FIELD_F32(0x21)
#define /*0x110*/ oIntroPeachDistToCamera OBJECT_FIELD_F32(0x22)

/* Racing Penguin */
#define /*0x0F4*/ oRacingPenguinInitTextCooldown       OBJECT_FIELD_S32(0x1B)
// 0x1D-0x21 reserved for pathing
#define /*0x110*/ oRacingPenguinWeightedNewTargetSpeed OBJECT_FIELD_F32(0x22)
#define /*0x1AC*/ oRacingPenguinFinalTextbox           OBJECT_FIELD_S16(0x49, 0)
#define /*0x1AE*/ oRacingPenguinMarioWon               OBJECT_FIELD_S16(0x49, 1)
#define /*0x1B0*/ oRacingPenguinReachedBottom          OBJECT_FIELD_S16(0x4A, 0)
#define /*0x1B2*/ oRacingPenguinMarioCheated           OBJECT_FIELD_S16(0x4A, 1)

/* Small Penguin */
#define /*0x088*/ oSmallPenguinReturnedToMother    OBJECT_FIELD_S32(0x00)
#define /*0x100*/ oSmallPenguinStoredAction        OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oSmallPenguinNextForwardVel      OBJECT_FIELD_F32(0x1F)
#define /*0x108*/ oSmallPenguinRandomDistanceCheck OBJECT_FIELD_F32(0x20)
#define /*0x110*/ oSmallPenguinYawIncrement        OBJECT_FIELD_S32(0x22)

/* SL Walking Penguin */
#define /*0x100*/ oSLWalkingPenguinWindCollisionXPos OBJECT_FIELD_F32(0x1E)
#define /*0x104*/ oSLWalkingPenguinWindCollisionZPos OBJECT_FIELD_F32(0x1F)
#define /*0x10C*/ oSLWalkingPenguinCurStep           OBJECT_FIELD_S32(0x21)
#define /*0x110*/ oSLWalkingPenguinCurStepTimer      OBJECT_FIELD_S32(0x22)

/* Piranha Plant */
#define /*0x0F4*/ oPiranhaPlantSleepMusicState OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oPiranhaPlantScale           OBJECT_FIELD_F32(0x1C)

/* Fire Piranha Plant */
#define /*0x0F4*/ oFirePiranhaPlantNeutralScale   OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oFirePiranhaPlantScale          OBJECT_FIELD_F32(0x1C) // Shared with above obj? Coincidence?
#define /*0x0FC*/ oFirePiranhaPlantActive         OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oFirePiranhaPlantDeathSpinTimer OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oFirePiranhaPlantDeathSpinVel   OBJECT_FIELD_F32(0x1F)

/* Rolling Log */
#define /*0x0F4*/ oRollingLogMaxDist OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oRollingLogX       OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oRollingLogZ       OBJECT_FIELD_F32(0x1D)

/* Falling Rising Bitfs Platform */
#define /*0x0F4*/ oBitfsPlatformTimer   OBJECT_FIELD_S32(0x1B)

/* Falling Bowser Bits Platform */
#define /*0x0F8*/ oBitsPlatformBowser   OBJECT_FIELD_OBJ(0x1C)
#define /*0x0FC*/ oBitsPlatformTimer    OBJECT_FIELD_S32(0x1D)

/* WF Platform */
#define /*0x10C*/ oPlatformWFTowerForwardVel   OBJECT_FIELD_F32(0x21)
#define /*0x110*/ oPlatformWFTowerMoveDistance OBJECT_FIELD_F32(0x22)

/* Platform on Tracks */
#define /*0x088*/ oPlatformOnTrackBaseBallIndex          OBJECT_FIELD_S32(0x00)
#define /*0x0F4*/ oPlatformOnTrackDistMovedSinceLastBall OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oPlatformOnTrackSkiLiftRollVel         OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oPlatformOnTrackStartWaypoint          OBJECT_FIELD_WAYPOINT(0x1D)
#define /*0x100*/ oPlatformOnTrackPrevWaypoint           OBJECT_FIELD_WAYPOINT(0x1E)
#define /*0x104*/ oPlatformOnTrackPrevWaypointFlags      OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oPlatformOnTrackPitch                  OBJECT_FIELD_S32(0x20)
#define /*0x10C*/ oPlatformOnTrackYaw                    OBJECT_FIELD_S32(0x21)
#define /*0x110*/ oPlatformOnTrackOffsetY                OBJECT_FIELD_F32(0x22)
#define /*0x1AC*/ oPlatformOnTrackIsNotSkiLift           OBJECT_FIELD_S16(0x49, 0)
#define /*0x1AE*/ oPlatformOnTrackIsNotHMC               OBJECT_FIELD_S16(0x49, 1)
#define /*0x1B0*/ oPlatformOnTrackType                   OBJECT_FIELD_S16(0x4A, 0)
#define /*0x1B2*/ oPlatformOnTrackWasStoodOn             OBJECT_FIELD_S16(0x4A, 1)

/* Platform Spawner */
#define /*0x0F4*/ oPlatformSpawnerWFTowerPlatformNum  OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oPlatformSpawnerWFTowerYawOffset    OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oPlatformSpawnerWFTowerDYaw         OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oPlatformSpawnerWFTowerRadius       OBJECT_FIELD_F32(0x1E)
#define /*0x104*/ oPlatformSpawnerWFTowerMoveDistance OBJECT_FIELD_F32(0x1F)
#define /*0x108*/ oPlatformSpawnerWFTowerForwardVel   OBJECT_FIELD_F32(0x20)

/* Pokey */
#define /*0x0F4*/ oPokeyAliveBodyPartFlags  OBJECT_FIELD_U32(0x1B)
#define /*0x0F8*/ oPokeyNumAliveBodyParts   OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oPokeyBottomBodyPartSize  OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oPokeyHeadWasKilled       OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oPokeyTargetYaw           OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oPokeyChangeTargetTimer   OBJECT_FIELD_S32(0x20)
#define /*0x10C*/ oPokeyTurningAwayFromWall OBJECT_FIELD_S32(0x21)

/* Pokey Body Part */
#define /*0x0F8*/ oPokeyBodyPartDeathDelayAfterHeadKilled OBJECT_FIELD_S32(0x1C)
#define /*0x110*/ oPokeyBodyPartBlinkTimer                OBJECT_FIELD_S32(0x22)

/* DDD Pole */
#define /*0x0F4*/ oDDDPoleVel       OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oDDDPoleMaxOffset OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oDDDPoleOffset    OBJECT_FIELD_F32(0x1D)

/* Pyramid Top */
#define /*0x0F4*/ oPyramidTopPillarsTouched OBJECT_FIELD_S32(0x1B)

/* Pyramid Top Explosion */
#define /*0x0F4*/ oPyramidTopFragmentsScale OBJECT_FIELD_F32(0x1B)

/* Volcano Trap */
#define /*0x0F4*/ oVolcanoTrapPitchVel OBJECT_FIELD_F32(0x1B)

/* Lll Rotating Hex Flame */
#define /*0x0F4*/ O_LLL_ROTATING_HEX_FLAME_RELATIVE_POS_INDEX   0x1B
#define /*0x0F4*/ O_LLL_ROTATING_HEX_FLAME_RELATIVE_POS_X_INDEX (O_LLL_ROTATING_HEX_FLAME_RELATIVE_POS_INDEX + 0) // 0x1B
#define /*0x0F8*/ O_LLL_ROTATING_HEX_FLAME_RELATIVE_POS_Y_INDEX (O_LLL_ROTATING_HEX_FLAME_RELATIVE_POS_INDEX + 1) // 0x1C
#define /*0x0FC*/ O_LLL_ROTATING_HEX_FLAME_RELATIVE_POS_Z_INDEX (O_LLL_ROTATING_HEX_FLAME_RELATIVE_POS_INDEX + 2) // 0x1D
#define /*0x0F4*/ oLllRotatingHexFlameRelativePosVec            OBJECT_FIELD_F32(O_LLL_ROTATING_HEX_FLAME_RELATIVE_POS_INDEX)
#define /*0x0F4*/ oLllRotatingHexFlameRelativePosX              OBJECT_FIELD_F32(O_LLL_ROTATING_HEX_FLAME_RELATIVE_POS_X_INDEX)
#define /*0x0F8*/ oLllRotatingHexFlameRelativePosY              OBJECT_FIELD_F32(O_LLL_ROTATING_HEX_FLAME_RELATIVE_POS_Y_INDEX)
#define /*0x0FC*/ oLllRotatingHexFlameRelativePosZ              OBJECT_FIELD_F32(O_LLL_ROTATING_HEX_FLAME_RELATIVE_POS_Z_INDEX)

/* Scuttlebug */
#define /*0x0F4*/ oScuttlebugHasNoLootCoins OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oScuttlebugIsAtttacking   OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oScuttlebugTimer          OBJECT_FIELD_S32(0x1D)

/* Scuttlebug Spawner */
#define /*0x088*/ oScuttlebugSpawnerIsDeactivated        OBJECT_FIELD_S32(0x00)
#define /*0x0F4*/ oScuttlebugSpawnerSpawnWithNoLootCoins OBJECT_FIELD_S32(0x1B)

/* Seesaw Platform */
#define /*0x0F4*/ oSeesawPlatformPitchVel OBJECT_FIELD_F32(0x1B)

/* Ship Part 3 */
#define /*0x0F4*/ oShipPart3LoopPitch OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oShipPart3LoopRoll  OBJECT_FIELD_S32(0x1C)

/* Sink When Stepped On */
#define /*0x104*/ oSinkWhenSteppedOnTimer           OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oSinkWhenSteppedOnYOffsetFromHome OBJECT_FIELD_F32(0x20)

/* Skeeter */
#define /*0x0F4*/ oSkeeterTargetAngle         OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oSkeeterTurningAwayFromWall OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oSkeeterTargetForwardVel    OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oSkeeterWaitTime            OBJECT_FIELD_S32(0x1E)
#define /*0x1AC*/ oSkeeterAngleVel            OBJECT_FIELD_S16(0x49, 0)

/* Jrb Sliding Box */
#define /*0x0F4*/ oJrbSlidingBoxShipObj   OBJECT_FIELD_OBJ(0x1B)
#define /*0x0F8*/ oJrbSlidingBoxAngle     OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oJrbSlidingBoxAdditiveZ OBJECT_FIELD_F32(0x1D)

/* WF Sliding Brick Platform */
#define /*0x0F4*/ oWFSlidBrickPtfmMovVel OBJECT_FIELD_F32(0x1B)

/* Smoke */
#define /*0x0F4*/ oSmokeTimer OBJECT_FIELD_S32(0x1B)

/* Snowman's Bottom */
#define /*0x0F4*/ oSnowmansBottomScale                  OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oSnowmansBottomTargetYaw              OBJECT_FIELD_S32(0x1C)
#define /*0x1AC*/ oSnowmansBottomHitCheckpointNearMario OBJECT_FIELD_S32(0x49)
// 0x1D-0x21 reserved for pathing

/* Snowman's Head */
#define /*0x0F4*/ oSnowmansHeadDialogActive OBJECT_FIELD_S32(0x1B)

/* Snowman Wind Blowing */
#define /*0x0F4*/ oSLSnowmanWindOriginalYaw OBJECT_FIELD_S32(0x1B)

/* Snufit */
#define /*0x0F4*/ oSnufitRecoil          OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oSnufitScale           OBJECT_FIELD_F32(0x1C)
// 0x1D
#define /*0x100*/ oSnufitCircularPeriod  OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oSnufitBodyScalePeriod OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oSnufitBodyBaseScale   OBJECT_FIELD_S32(0x20)
#define /*0x10C*/ oSnufitBullets         OBJECT_FIELD_S32(0x21)
#define /*0x1AC*/ oSnufitBodyScale       OBJECT_FIELD_S16(0x49, 0)

/* Spindel */
#define /*0x0F4*/ oSpindelMoveTimer     OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oSpindelMoveDirection OBJECT_FIELD_S32(0x1C)

/* Spinning Heart */
#define /*0x0F4*/ oSpinningHeartTotalSpin   OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oSpinningHeartPlayedSound OBJECT_FIELD_S32(0x1C)

/* Spiny */
#define /*0x0F4*/ oSpinyTimeUntilTurn       OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oSpinyTargetYaw           OBJECT_FIELD_S32(0x1C)
#define /*0x100*/ oSpinyTurningAwayFromWall OBJECT_FIELD_S32(0x1E)

/* Sound Effect */
#define /*0x0F4*/ oSoundEffectBits      OBJECT_FIELD_S32(0x1B)

/* Star Spawn */
#define /*0x0F4*/ oStarSpawnDisFromHome OBJECT_FIELD_F32(0x1B)
#define /*0x0FC*/ oStarSpawnVelY        OBJECT_FIELD_F32(0x1D)

/* Hidden Star */
// Secrets/Red Coins
#define /*0x0F4*/ oHiddenStarTriggerCounter OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oHiddenStarTriggerTotal OBJECT_FIELD_S32(0x1C)

/* Sealed Door Star */
#define /*0x108*/ oUnlockDoorStarState  OBJECT_FIELD_U32(0x20)
#define /*0x10C*/ oUnlockDoorStarTimer  OBJECT_FIELD_S32(0x21)
#define /*0x110*/ oUnlockDoorStarYawVel OBJECT_FIELD_S32(0x22)

/* Celebration Star */
#define /*0x0F4*/ oCelebStarIsBowserKey        OBJECT_FIELD_S32(0x1B)
#define /*0x108*/ oCelebStarDiameterOfRotation OBJECT_FIELD_S32(0x20)

/* Star Selector */
#define /*0x0F4*/ oStarSelectorType  OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oStarSelectorTimer OBJECT_FIELD_S32(0x1C)
#define /*0x108*/ oStarSelectorSize  OBJECT_FIELD_F32(0x20)

/* Sushi Shark */
#define /*0x0F4*/ oSushiSharkAngleFromHome OBJECT_FIELD_S32(0x1B)

/* Swing Platform */
#define /*0x0F4*/ oSwingPlatformAngle OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oSwingPlatformSpeed OBJECT_FIELD_F32(0x1C)

/* Swoop */
#define /*0x0F4*/ oSwoopBonkCountdown OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ O_SWOOP_TARGET_ANGLE_INDEX                    0x1C
#define /*0x0F8*/ O_SWOOP_TARGET_PITCH_INDEX                    (O_SWOOP_TARGET_ANGLE_INDEX + 0) // 0x1C
#define /*0x0F8*/ O_SWOOP_TARGET_YAW_INDEX                      (O_SWOOP_TARGET_ANGLE_INDEX + 1) // 0x1D
#define /*0x0F8*/ O_SWOOP_TARGET_ROLL_INDEX                     (O_SWOOP_TARGET_ANGLE_INDEX + 2) // 0x1E
#define /*0x0F8*/ oSwoopTargetAngleVec OBJECT_FIELD_S32(O_SWOOP_TARGET_ANGLE_INDEX)
#define /*0x0F8*/ oSwoopTargetPitch   OBJECT_FIELD_S32(O_SWOOP_TARGET_PITCH_INDEX)
#define /*0x0FC*/ oSwoopTargetYaw     OBJECT_FIELD_S32(O_SWOOP_TARGET_YAW_INDEX)
#define /*0x100*/ oSwoopTargetRoll    OBJECT_FIELD_S32(O_SWOOP_TARGET_ROLL_INDEX) // unused

/* Thwomp */
#define /*0x0F4*/ oThwompRandomTimer OBJECT_FIELD_S32(0x1B)

/* Tilting Platform */
#define /*0x0F4*/ O_TILTING_PYRAMID_NORMAL_INDEX   0x1B
#define /*0x0F4*/ O_TILTING_PYRAMID_NORMAL_X_INDEX (O_TILTING_PYRAMID_NORMAL_INDEX + 0) // 0x1B
#define /*0x0F8*/ O_TILTING_PYRAMID_NORMAL_Y_INDEX (O_TILTING_PYRAMID_NORMAL_INDEX + 1) // 0x1C
#define /*0x0FC*/ O_TILTING_PYRAMID_NORMAL_Z_INDEX (O_TILTING_PYRAMID_NORMAL_INDEX + 2) // 0x1D
#define /*0x0F4*/ oTiltingPyramidNormalVec       OBJECT_FIELD_F32(O_TILTING_PYRAMID_NORMAL_INDEX)
#define /*0x0F4*/ oTiltingPyramidNormalX         OBJECT_FIELD_F32(O_TILTING_PYRAMID_NORMAL_X_INDEX)
#define /*0x0F8*/ oTiltingPyramidNormalY         OBJECT_FIELD_F32(O_TILTING_PYRAMID_NORMAL_Y_INDEX)
#define /*0x0FC*/ oTiltingPyramidNormalZ         OBJECT_FIELD_F32(O_TILTING_PYRAMID_NORMAL_Z_INDEX)

/* Toad Message */
#define /*0x108*/ oToadMessageDialogId       OBJECT_FIELD_U32(0x20)
#define /*0x10C*/ oToadMessageRecentlyTalked OBJECT_FIELD_S32(0x21)
#define /*0x110*/ oToadMessageState          OBJECT_FIELD_S32(0x22)

/* Tox Box */
#define /*0x1AC*/ oToxBoxMovementPattern OBJECT_FIELD_VPTR(0x49)
#define /*0x1B0*/ oToxBoxMovementStep    OBJECT_FIELD_S32(0x4A)

/* TTC Rotating Solid */
#define /*0x0F4*/ oTTCRotatingSolidNumTurns      OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oTTCRotatingSolidNumSides      OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oTTCRotatingSolidRotationDelay OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oTTCRotatingSolidVelY          OBJECT_FIELD_F32(0x1E)
#define /*0x104*/ oTTCRotatingSolidSoundTimer    OBJECT_FIELD_S32(0x1F)

/* TTC Pendulum */
#define /*0x0F4*/ oTTCPendulumAccelDir   OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oTTCPendulumAngle      OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oTTCPendulumAngleVel   OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oTTCPendulumAngleAccel OBJECT_FIELD_F32(0x1E)
#define /*0x104*/ oTTCPendulumDelay      OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oTTCPendulumSoundTimer OBJECT_FIELD_S32(0x20)

/* TTC Treadmill */
#define /*0x0F4*/ oTTCTreadmillBigSurface      OBJECT_FIELD_S16P(0x1B)
#define /*0x0F8*/ oTTCTreadmillSmallSurface    OBJECT_FIELD_S16P(0x1C)
#define /*0x0FC*/ oTTCTreadmillSpeed           OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oTTCTreadmillTargetSpeed     OBJECT_FIELD_F32(0x1E)
#define /*0x104*/ oTTCTreadmillTimeUntilSwitch OBJECT_FIELD_S32(0x1F)

/* TTC Moving Bar */
#define /*0x0F4*/ oTTCMovingBarDelay        OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oTTCMovingBarStoppedTimer OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oTTCMovingBarOffset       OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oTTCMovingBarSpeed        OBJECT_FIELD_F32(0x1E)
#define /*0x104*/ oTTCMovingBarStartOffset  OBJECT_FIELD_F32(0x1F)

/* TTC Cog */
#define /*0x0F4*/ oTTCCogDir       OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oTTCCogSpeed     OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oTTCCogTargetVel OBJECT_FIELD_F32(0x1D)

/* TTC Pit Block */
#define /*0x0F4*/ oTTCPitBlockPeakY    OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oTTCPitBlockDir      OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oTTCPitBlockWaitTime OBJECT_FIELD_S32(0x1D)

/* TTC Elevator */
#define /*0x0F4*/ oTTCElevatorDir      OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oTTCElevatorPeakY    OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oTTCElevatorMoveTime OBJECT_FIELD_S32(0x1D)

/* TTC 2D Rotator */
#define /*0x0F4*/ oTTC2DRotatorMinTimeUntilNextTurn OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oTTC2DRotatorTargetYaw            OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oTTC2DRotatorIncrement            OBJECT_FIELD_S32(0x1D)
#define /*0x104*/ oTTC2DRotatorRandomDirTimer       OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oTTC2DRotatorSpeed                OBJECT_FIELD_S32(0x20)

/* TTC Spinner */
#define /*0x0F4*/ oTTCSpinnerDir     OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oTTCChangeDirTimer OBJECT_FIELD_S32(0x1C)

/* Beta Trampoline */
#define /*0x110*/ oBetaTrampolineMarioOnTrampoline OBJECT_FIELD_S32(0x22)

/* Treasure Chest */
#define /*0x0F4*/ oTreasureChestNumOpenedChests OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oTreasureChestDoCloseChests   OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oTreasureChestAboveWater      OBJECT_FIELD_S32(0x1D)

/* Tree Snow Or Leaf */
#define /*0x0F4*/ oTreeSnowOrLeafSidewardAngle    OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oTreeSnowOrLeafSidewardVel      OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oTreeSnowOrLeafSidewardAngleVel OBJECT_FIELD_S32(0x1D)

/* Tumbling Bridge */
#define /*0x0F4*/ oTumblingBridgeRollAccel OBJECT_FIELD_S32(0x1B)

/* Tweester */
#define /*0x0F4*/ oTweesterScaleTimer OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oTweesterUnused     OBJECT_FIELD_S32(0x1C)

/* Ukiki */
#define /*0x0F4*/ oUkikiTauntCounter   OBJECT_FIELD_S16(0x1B, 0)
#define /*0x0F6*/ oUkikiTauntsToBeDone OBJECT_FIELD_S16(0x1B, 1)
// 0x1D-0x21 reserved for pathing
#define /*0x110*/ oUkikiChaseFleeRange OBJECT_FIELD_F32(0x22)
#define /*0x1AC*/ oUkikiTextState      OBJECT_FIELD_S16(0x49, 0)
#define /*0x1AE*/ oUkikiTextboxTimer   OBJECT_FIELD_S16(0x49, 1)
#define /*0x1B0*/ oUkikiCageSpinTimer  OBJECT_FIELD_S16(0x4A, 0)
#define /*0x1B2*/ oUkikiHasCap         OBJECT_FIELD_S16(0x4A, 1)

/* Ukiki Cage*/
#define /*0x088*/ oUkikiCageNextAction OBJECT_FIELD_S32(0x00)

/* Unagi */
#define /*0x0F4*/ oUnagiDistFromHome         OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oUnagiNextForwardVel       OBJECT_FIELD_F32(0x1C)
// 0x1D-0x21 reserved for pathing
#define /*0x110*/ oUnagiTargetNextForwardVel OBJECT_FIELD_F32(0x22)
#define /*0x1AC*/ oUnagiDistanceToMario      OBJECT_FIELD_F32(0x49)
#define /*0x1B0*/ oUnagiInitMoveYaw          OBJECT_FIELD_S16(0x4A, 0)
#define /*0x1B2*/ oUnagiHasStar              OBJECT_FIELD_S16(0x4A, 1)

/* Water Bomb */
#define /*0x0F8*/ oWaterBombVerticalStretch OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oWaterBombStretchSpeed    OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oWaterBombOnGround        OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oWaterBombNumBounces      OBJECT_FIELD_F32(0x1F)

/* Water Bomb Spawner */
#define /*0x0F4*/ oWaterBombSpawnerBombActive  OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oWaterBombSpawnerTimeToSpawn OBJECT_FIELD_S32(0x1C)

/* Water Bomb Cannon */
#define /*0x0F4*/ oWaterCannonIdleTimer       OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oWaterCannonRotationTimer   OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oWaterCannonTargetMovePitch OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oWaterCannonTargetFaceYaw   OBJECT_FIELD_S32(0x1E)

/* Cannon Barrel Bubbles */
#define /*0x0F4*/ oCannonBarrelBubblesForwardVelCheck OBJECT_FIELD_F32(0x1B)

/* Water Level Pillar */
#define /*0x0F8*/ oWaterLevelPillarDrained OBJECT_FIELD_S32(0x1C)

/* Water Level Trigger */
#define /*0x0F4*/ oWaterLevelTriggerAmbientWaves     OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oWaterLevelTriggerTargetWaterLevel OBJECT_FIELD_S32(0x1C)

/* Water Objects */
#define /*0x0F4*/ oWaterObjScaleXAngle    OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oWaterObjScaleYAngle    OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oWaterObjScaleXAngleVel OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oWaterObjScaleYAngleVel OBJECT_FIELD_S32(0x1E)

/* Water Ring (both variants) */
#define /*0x0F4*/ O_WATER_RING_SCALE_PHASE_INDEX   0x1B
#define /*0x0F4*/ O_WATER_RING_SCALE_PHASE_X_INDEX (O_WATER_RING_SCALE_PHASE_INDEX + 0) // 0x1B
#define /*0x0F8*/ O_WATER_RING_SCALE_PHASE_Y_INDEX (O_WATER_RING_SCALE_PHASE_INDEX + 1) // 0x1C
#define /*0x0FC*/ O_WATER_RING_SCALE_PHASE_Z_INDEX (O_WATER_RING_SCALE_PHASE_INDEX + 2) // 0x1D
#define /*0x0F4*/ oWaterRingScalePhaseVec          OBJECT_FIELD_S32(O_WATER_RING_SCALE_PHASE_INDEX)
#define /*0x0F4*/ oWaterRingScalePhaseX            OBJECT_FIELD_S32(O_WATER_RING_SCALE_PHASE_X_INDEX)
#define /*0x0F8*/ oWaterRingScalePhaseY            OBJECT_FIELD_S32(O_WATER_RING_SCALE_PHASE_Y_INDEX)
#define /*0x0FC*/ oWaterRingScalePhaseZ            OBJECT_FIELD_S32(O_WATER_RING_SCALE_PHASE_Z_INDEX)
#define /*0x100*/ O_WATER_RING_NORMAL_INDEX        0x1E
#define /*0x100*/ O_WATER_RING_NORMAL_X_INDEX      (O_WATER_RING_NORMAL_INDEX + 0) // 0x1E
#define /*0x104*/ O_WATER_RING_NORMAL_Y_INDEX      (O_WATER_RING_NORMAL_INDEX + 1) // 0x1F
#define /*0x108*/ O_WATER_RING_NORMAL_Z_INDEX      (O_WATER_RING_NORMAL_INDEX + 2) // 0x20
#define /*0x100*/ oWaterRingNormalVec              OBJECT_FIELD_F32(O_WATER_RING_NORMAL_INDEX)
#define /*0x100*/ oWaterRingNormalX                OBJECT_FIELD_F32(O_WATER_RING_NORMAL_X_INDEX)
#define /*0x104*/ oWaterRingNormalY                OBJECT_FIELD_F32(O_WATER_RING_NORMAL_Y_INDEX)
#define /*0x108*/ oWaterRingNormalZ                OBJECT_FIELD_F32(O_WATER_RING_NORMAL_Z_INDEX)
#define /*0x10C*/ oWaterRingMarioDistInFront       OBJECT_FIELD_F32(0x21)
#define /*0x110*/ oWaterRingIndex                  OBJECT_FIELD_S32(0x22)
#define /*0x1AC*/ oWaterRingAvgScale               OBJECT_FIELD_F32(0x49)

/* Water Ring Spawner (Jet Stream Ring Spawner and Manta Ray) */
#define /*0x1AC*/ oWaterRingSpawnerRingsCollected OBJECT_FIELD_S32(0x49)

/* Water Ring Manager (Jet Stream Ring Spawner and Manta Ray Ring Manager) */
#define /*0x0F4*/ oWaterRingMgrNextRingIndex     OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oWaterRingMgrLastRingCollected OBJECT_FIELD_S32(0x1C)

/* Wave Trail */
#define /*0x0F8*/ oWaveTrailSize OBJECT_FIELD_F32(0x1C)

/* Whirlpool */
#define /*0x0F4*/ oWhirlpoolInitFacePitch OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oWhirlpoolInitFaceRoll  OBJECT_FIELD_S32(0x1C)

/* White Puff Explode */
#define /*0x0F4*/ oWhitePuffScaleX      OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oWhitePuffOpacityDiff OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oWhitePuffSlowFade    OBJECT_FIELD_S32(0x1D)

/* White Wind Particle */
#define /*0x0F4*/ oStrongWindParticlePenguinObj OBJECT_FIELD_OBJ(0x1B)

/* Whomp */
#define /*0x0F8*/ oWhompShakeVal OBJECT_FIELD_S32(0x1C)

/* Wiggler */
#define /*0x0F4*/ oWigglerFallThroughFloorsHeight OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oWigglerSegments                OBJECT_FIELD_CHAIN_SEGMENT(0x1C)
#define /*0x0FC*/ oWigglerWalkAnimSpeed           OBJECT_FIELD_F32(0x1D)
#define /*0x104*/ oWigglerSquishSpeed             OBJECT_FIELD_F32(0x1F)
#define /*0x108*/ oWigglerTimeUntilRandomTurn     OBJECT_FIELD_S32(0x20)
#define /*0x10C*/ oWigglerTargetYaw               OBJECT_FIELD_S32(0x21)
#define /*0x110*/ oWigglerWalkAwayFromWallTimer   OBJECT_FIELD_S32(0x22)
#define /*0x1AC*/ oWigglerUnused                  OBJECT_FIELD_S16(0x49, 0)
#define /*0x1AE*/ oWigglerTextStatus              OBJECT_FIELD_S16(0x49, 1)

/* Lll Wood Piece */
#define /*0x0F4*/ oLllWoodPieceOscillationTimer OBJECT_FIELD_S32(0x1B)

/* Wooden Post */
#define /*0x0F4*/ oWoodenPostTotalMarioAngle  OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oWoodenPostPrevAngleToMario OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oWoodenPostSpeedY           OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oWoodenPostMarioPounding    OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ oWoodenPostOffsetY          OBJECT_FIELD_F32(0x1F)

/* Yoshi */
#define /*0x0F4*/ oYoshiBlinkTimer OBJECT_FIELD_S32(0x1B)
#define /*0x0FC*/ oYoshiChosenHome OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ oYoshiTargetYaw  OBJECT_FIELD_S32(0x1E)


/*Custom general defines:

For general s32 ints, use o->oF4, oF8, oFC, o100, o104, o108, o10C, and o110

For floats, apply the prefix "oFloat" before the index. For object pointers, apply "oObj", and for surface pointers, apply "oSurf"

Examples: o->oFloatF4, o->oSurf10C

s16 variables are also supported, and using them effectly can double the number of available members. The full list of s16 defines is:

os16F4
os16F6
os16F8
os16FA
os16FC
os16FE
os16100
os16102
os16104
os16106
os16108
os1610A
os1610C
os1610E
os16110
os16112*/

#define /*0x0F4*/ oF4                                           OBJECT_FIELD_S32(0x1B)
#define /*0x0F8*/ oF8                                           OBJECT_FIELD_S32(0x1C)
#define /*0x0FC*/ oFC                                           OBJECT_FIELD_S32(0x1D)
#define /*0x100*/ o100                                          OBJECT_FIELD_S32(0x1E)
#define /*0x104*/ o104                                          OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ o108                                          OBJECT_FIELD_S32(0x20)
#define /*0x10C*/ o10C                                          OBJECT_FIELD_S32(0x21)
#define /*0x110*/ o110                                          OBJECT_FIELD_S32(0x22)

#define /*0x0F4*/ oFloatF4                                      OBJECT_FIELD_F32(0x1B)
#define /*0x0F8*/ oFloatF8                                      OBJECT_FIELD_F32(0x1C)
#define /*0x0FC*/ oFloatFC                                      OBJECT_FIELD_F32(0x1D)
#define /*0x100*/ oFloat100                                     OBJECT_FIELD_F32(0x1E)
#define /*0x104*/ oFloat104                                     OBJECT_FIELD_F32(0x1F)
#define /*0x108*/ oFloat108                                     OBJECT_FIELD_F32(0x20)
#define /*0x10C*/ oFloat10C                                     OBJECT_FIELD_F32(0x21)
#define /*0x110*/ oFloat110                                     OBJECT_FIELD_F32(0x22)

#define /*0x0F4*/ oObjF4                                        OBJECT_FIELD_OBJ(0x1B)
#define /*0x0F8*/ oObjF8                                        OBJECT_FIELD_OBJ(0x1C)
#define /*0x0FC*/ oObjFC                                        OBJECT_FIELD_OBJ(0x1D)
#define /*0x100*/ oObj100                                       OBJECT_FIELD_OBJ(0x1E)
#define /*0x104*/ oObj104                                       OBJECT_FIELD_OBJ(0x1F)
#define /*0x108*/ oObj108                                       OBJECT_FIELD_OBJ(0x20)
#define /*0x10C*/ oObj10C                                       OBJECT_FIELD_OBJ(0x21)
#define /*0x110*/ oObj110                                       OBJECT_FIELD_OBJ(0x22)

#define /*0x0F4*/ oSurfF4                                   OBJECT_FIELD_SURFACE(0x1B)
#define /*0x0F8*/ oSurfF8                                   OBJECT_FIELD_SURFACE(0x1C)
#define /*0x0FC*/ oSurfFC                                   OBJECT_FIELD_SURFACE(0x1D)
#define /*0x100*/ oSurf100                                  OBJECT_FIELD_SURFACE(0x1E)
#define /*0x104*/ oSurf104                                  OBJECT_FIELD_SURFACE(0x1F)
#define /*0x108*/ oSurf108                                  OBJECT_FIELD_SURFACE(0x20)
#define /*0x10C*/ oSurf10C                                  OBJECT_FIELD_SURFACE(0x21)
#define /*0x110*/ oSurf110                                  OBJECT_FIELD_SURFACE(0x22)

#define /*0x0F4*/ os16F4                                        OBJECT_FIELD_S16(0x1B, 0)
#define /*0x0F6*/ os16F6                                        OBJECT_FIELD_S16(0x1B, 1)
#define /*0x0F8*/ os16F8                                        OBJECT_FIELD_S16(0x1C, 0)
#define /*0x0FA*/ os16FA                                        OBJECT_FIELD_S16(0x1C, 1)
#define /*0x0FC*/ os16FC                                        OBJECT_FIELD_S16(0x1D, 0)
#define /*0x0FE*/ os16FE                                        OBJECT_FIELD_S16(0x1D, 1)
#define /*0x100*/ os16100                                       OBJECT_FIELD_S16(0x1E, 0)
#define /*0x102*/ os16102                                       OBJECT_FIELD_S16(0x1E, 1)
#define /*0x104*/ os16104                                       OBJECT_FIELD_S16(0x1F, 0)
#define /*0x106*/ os16106                                       OBJECT_FIELD_S16(0x1F, 1)
#define /*0x108*/ os16108                                       OBJECT_FIELD_S16(0x20, 0)
#define /*0x10A*/ os1610A                                       OBJECT_FIELD_S16(0x20, 1)
#define /*0x10C*/ os1610C                                       OBJECT_FIELD_S16(0x21, 0)
#define /*0x10E*/ os1610E                                       OBJECT_FIELD_S16(0x21, 1)
#define /*0x110*/ os16110                                       OBJECT_FIELD_S16(0x22, 0)
#define /*0x112*/ os16112                                       OBJECT_FIELD_S16(0x22, 1)


#endif // OBJECT_FIELDS_H
