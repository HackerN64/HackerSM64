#ifndef GLOBAL_OBJECT_FIELDS_H
#define GLOBAL_OBJECT_FIELDS_H

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
#ifdef PUPPYLIGHTS
#define /*0x1C4*/ oLightID                                      OBJECT_FIELD_S32(0x50)
#endif

/* Pathed (see obj_follow_path) */
#define /*0x0FC*/ oPathedStartWaypoint     OBJECT_FIELD_WAYPOINT(0x1D)
#define /*0x100*/ oPathedPrevWaypoint      OBJECT_FIELD_WAYPOINT(0x1E)
#define /*0x104*/ oPathedPrevWaypointFlags OBJECT_FIELD_S32(0x1F)
#define /*0x108*/ oPathedTargetPitch       OBJECT_FIELD_S32(0x20)
#define /*0x10C*/ oPathedTargetYaw         OBJECT_FIELD_S32(0x21)

/* Special Object Macro */
#define /*0x108*/ oMacroUnk108 OBJECT_FIELD_F32(0x20)
#define /*0x10C*/ oMacroUnk10C OBJECT_FIELD_F32(0x21)
#define /*0x110*/ oMacroUnk110 OBJECT_FIELD_F32(0x22)

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


#endif // GLOBAL_OBJECT_FIELDS_H
