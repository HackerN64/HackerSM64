#ifndef OBJ_BEHAVIORS_2_H
#define OBJ_BEHAVIORS_2_H

#include <PR/ultratypes.h>

#include "types.h"

enum AttackHandler {
    ATTACK_HANDLER_NOP,
    ATTACK_HANDLER_DIE_IF_HEALTH_NON_POSITIVE,
    ATTACK_HANDLER_KNOCKBACK,
    ATTACK_HANDLER_SQUISHED,
    ATTACK_HANDLER_SPECIAL_KOOPA_LOSE_SHELL,
    ATTACK_HANDLER_SET_SPEED_TO_ZERO,
    ATTACK_HANDLER_SPECIAL_WIGGLER_JUMPED_ON,
    ATTACK_HANDLER_SPECIAL_HUGE_GOOMBA_WEAKLY_ATTACKED,
    ATTACK_HANDLER_SQUISHED_WITH_BLUE_COIN
};

void shelled_koopa_attack_handler(s32 attackType);
void obj_spit_fire(s16 relativePosX, s16 relativePosY, s16 relativePosZ, f32 scale, ModelID32 model,
                   f32 startSpeed, f32 endSpeed, s16 movePitch);
void obj_set_speed_to_zero(void);

#endif // OBJ_BEHAVIORS_2_H
