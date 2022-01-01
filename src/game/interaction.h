#ifndef INTERACTION_H
#define INTERACTION_H

#include <PR/ultratypes.h>

#include "types.h"

enum ObjInteractTypes {
    INTERACT_NONE                 = /* 0x00000000 */ (0 <<  0),
    INTERACT_HOOT                 = /* 0x00000001 */ (1 <<  0),
    INTERACT_GRABBABLE            = /* 0x00000002 */ (1 <<  1),
    INTERACT_DOOR                 = /* 0x00000004 */ (1 <<  2),
    INTERACT_DAMAGE               = /* 0x00000008 */ (1 <<  3),
    INTERACT_COIN                 = /* 0x00000010 */ (1 <<  4),
    INTERACT_CAP                  = /* 0x00000020 */ (1 <<  5),
    INTERACT_POLE                 = /* 0x00000040 */ (1 <<  6),
    INTERACT_KOOPA                = /* 0x00000080 */ (1 <<  7),
    INTERACT_SPINY_WALKING        = /* 0x00000100 */ (1 <<  8),
    INTERACT_BREAKABLE            = /* 0x00000200 */ (1 <<  9),
    INTERACT_STRONG_WIND          = /* 0x00000400 */ (1 << 10),
    INTERACT_WARP_DOOR            = /* 0x00000800 */ (1 << 11),
    INTERACT_STAR_OR_KEY          = /* 0x00001000 */ (1 << 12),
    INTERACT_WARP                 = /* 0x00002000 */ (1 << 13),
    INTERACT_CANNON_BASE          = /* 0x00004000 */ (1 << 14),
    INTERACT_BOUNCE_TOP           = /* 0x00008000 */ (1 << 15),
    INTERACT_WATER_RING           = /* 0x00010000 */ (1 << 16),
    INTERACT_BULLY                = /* 0x00020000 */ (1 << 17),
    INTERACT_FLAME                = /* 0x00040000 */ (1 << 18),
    INTERACT_KOOPA_SHELL          = /* 0x00080000 */ (1 << 19),
    INTERACT_BOUNCE_TOP2          = /* 0x00100000 */ (1 << 20),
    INTERACT_MR_BLIZZARD          = /* 0x00200000 */ (1 << 21),
    INTERACT_HIT_FROM_BELOW       = /* 0x00400000 */ (1 << 22),
    INTERACT_TEXT                 = /* 0x00800000 */ (1 << 23),
    INTERACT_TORNADO              = /* 0x01000000 */ (1 << 24),
    INTERACT_WHIRLPOOL            = /* 0x02000000 */ (1 << 25),
    INTERACT_CLAM_OR_BUBBA        = /* 0x04000000 */ (1 << 26),
    INTERACT_BBH_ENTRANCE         = /* 0x08000000 */ (1 << 27),
    INTERACT_SNUFIT_BULLET        = /* 0x10000000 */ (1 << 28),
    INTERACT_SHOCK                = /* 0x20000000 */ (1 << 29),
    INTERACT_IGLOO_BARRIER        = /* 0x40000000 */ (1 << 30),
    INTERACT_UNKNOWN_31           = /* 0x80000000 */ (1 << 31),

    INTERACT_MASK_NO_OBJ_COLLISIONS = (INTERACT_COIN | INTERACT_CAP | INTERACT_STRONG_WIND | INTERACT_STAR_OR_KEY | INTERACT_WARP | INTERACT_WATER_RING | INTERACT_FLAME)
};

enum InteractSubtypes {
    // INTERACT_WARP
    INT_SUBTYPE_FADING_WARP         = /* 0x00000001 */ (1 <<  0),
    // Damaging interactions
    INT_SUBTYPE_DELAY_INVINCIBILITY = /* 0x00000002 */ (1 <<  1),
    INT_SUBTYPE_BIG_KNOCKBACK       = /* 0x00000008 */ (1 <<  3), /* Used by Bowser, sets Mario's forward velocity to 40 on hit */
    // INTERACT_GRABBABLE
    INT_SUBTYPE_GRABS_MARIO         = /* 0x00000004 */ (1 <<  2), /* Also makes the object heavy */
    INT_SUBTYPE_HOLDABLE_NPC        = /* 0x00000010 */ (1 <<  4), /* Allows the object to be gently dropped, and sets vertical speed to 0 when dropped with no forwards velocity */
    INT_SUBTYPE_DROP_IMMEDIATELY    = /* 0x00000040 */ (1 <<  6), /* This gets set by grabbable NPCs that talk to Mario to make him drop them after the dialog is finished */
    INT_SUBTYPE_KICKABLE            = /* 0x00000100 */ (1 <<  8),
    INT_SUBTYPE_NOT_GRABBABLE       = /* 0x00000200 */ (1 <<  9), /* Used by Heavy-Ho to allow it to throw Mario, without Mario being able to pick it up */
    // INTERACT_DOOR
    INT_SUBTYPE_STAR_DOOR           = /* 0x00000020 */ (1 <<  5),
    // INTERACT_BOUNCE_TOP
    INT_SUBTYPE_TWIRL_BOUNCE        = /* 0x00000080 */ (1 <<  7),
    // INTERACT_STAR_OR_KEY
    INT_SUBTYPE_NO_EXIT             = /* 0x00000400 */ (1 << 10),
    INT_SUBTYPE_GRAND_STAR          = /* 0x00000800 */ (1 << 11),
    // INTERACT_TEXT
    INT_SUBTYPE_SIGN                = /* 0x00001000 */ (1 << 12),
    INT_SUBTYPE_NPC                 = /* 0x00004000 */ (1 << 14),
    // INTERACT_CLAM_OR_BUBBA
    INT_SUBTYPE_EATS_MARIO          = /* 0x00002000 */ (1 << 13),
    
};

enum AttackType {
    ATTACK_NONE,
    ATTACK_PUNCH,
    ATTACK_KICK_OR_TRIP,
    ATTACK_FROM_ABOVE,
    ATTACK_GROUND_POUND_OR_TWIRL,
    ATTACK_FAST_ATTACK,
    ATTACK_FROM_BELOW
};

enum InteractStatus {
    INT_STATUS_NONE                 = (0 <<  0), /* 0x00000000 */
    // Mario Interaction Status
    INT_STATUS_MARIO_STUNNED        = (1 <<  0), /* 0x00000001 */
    INT_STATUS_MARIO_KNOCKBACK_DMG  = (1 <<  1), /* 0x00000002 */
    INT_STATUS_MARIO_THROWN_BY_OBJ  = (1 <<  2), /* 0x00000004 */
    INT_STATUS_MARIO_DROP_OBJECT    = (1 <<  3), /* 0x00000008 */
    INT_STATUS_MARIO_SHOCKWAVE      = (1 <<  4), /* 0x00000010 */
    INT_STATUS_MARIO_UNK5           = (1 <<  5), /* 0x00000020 */
    INT_STATUS_MARIO_DROPPED_BY_OBJ = (1 <<  6), /* 0x00000040 */
    INT_STATUS_MARIO_DROP_FROM_HOOT = (1 <<  7), /* 0x00000080 */
    // Object Interaction Status
    INT_STATUS_TOUCHED_MARIO        = (1 <<  0), /* 0x00000001 */
    INT_STATUS_GRABBED_MARIO        = (1 << 11), /* 0x00000800 */
    INT_STATUS_ATTACKED_MARIO       = (1 << 13), /* 0x00002000 */
    INT_STATUS_WAS_ATTACKED         = (1 << 14), /* 0x00004000 */
    INT_STATUS_INTERACTED           = (1 << 15), /* 0x00008000 */
    INT_STATUS_DOOR_PULLED          = (1 << 16), /* 0x00010000 */
    INT_STATUS_DOOR_PUSHED          = (1 << 17), /* 0x00020000 */
    INT_STATUS_WARP_DOOR_PULLED     = (1 << 18), /* 0x00040000 */
    INT_STATUS_WARP_DOOR_PUSHED     = (1 << 19), /* 0x00080000 */
    INT_STATUS_TRAP_TURN            = (1 << 20), /* 0x00100000 */
    INT_STATUS_HIT_MINE             = (1 << 21), /* 0x00200000 */
    INT_STATUS_STOP_RIDING          = (1 << 22), /* 0x00400000 */
    INT_STATUS_TOUCHED_BOB_OMB      = (1 << 23), /* 0x00800000 */

    INT_STATUS_ATTACK_MASK          =               0x000000FF
};

enum Interactions {
    INT_NONE                       = (0 << 0), // 0x00
    INT_GROUND_POUND_OR_TWIRL      = (1 << 0), // 0x01
    INT_PUNCH                      = (1 << 1), // 0x02
    INT_KICK                       = (1 << 2), // 0x04
    INT_TRIP                       = (1 << 3), // 0x08
    INT_SLIDE_KICK                 = (1 << 4), // 0x10
    INT_FAST_ATTACK_OR_SHELL       = (1 << 5), // 0x20
    INT_HIT_FROM_ABOVE             = (1 << 6), // 0x40
    INT_HIT_FROM_BELOW             = (1 << 7), // 0x80
    INT_ATTACK_NOT_FROM_BELOW      = (INT_GROUND_POUND_OR_TWIRL | INT_PUNCH | INT_KICK | INT_TRIP | INT_SLIDE_KICK | INT_FAST_ATTACK_OR_SHELL | INT_HIT_FROM_ABOVE                     ),
    INT_ANY_ATTACK                 = (INT_GROUND_POUND_OR_TWIRL | INT_PUNCH | INT_KICK | INT_TRIP | INT_SLIDE_KICK | INT_FAST_ATTACK_OR_SHELL | INT_HIT_FROM_ABOVE | INT_HIT_FROM_BELOW),
    INT_ATTACK_NOT_WEAK_FROM_ABOVE = (INT_GROUND_POUND_OR_TWIRL | INT_PUNCH | INT_KICK | INT_TRIP |                                                                  INT_HIT_FROM_BELOW),
};

s16 mario_obj_angle_to_object(struct MarioState *m, struct Object *obj);
void mario_stop_riding_object(struct MarioState *m);
void mario_grab_used_object(struct MarioState *m);
void mario_drop_held_object(struct MarioState *m);
void mario_throw_held_object(struct MarioState *m);
void mario_stop_riding_and_holding(struct MarioState *m);
u32  does_mario_have_normal_cap_on_head(struct MarioState *m);
void mario_blow_off_cap(struct MarioState *m, f32 capSpeed);
u32  mario_lose_cap_to_enemy(u32 enemyType);
void mario_retrieve_cap(void);
struct Object *mario_get_collided_object(struct MarioState *m, u32 interactType);
u32  mario_check_object_grab(struct MarioState *m);
u32  get_door_save_file_flag(struct Object *door);
void mario_process_interactions(struct MarioState *m);
void mario_handle_special_floors(struct MarioState *m);

#endif // INTERACTION_H
