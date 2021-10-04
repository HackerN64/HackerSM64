#ifndef OBJ_BEHAVIORS_2_H
#define OBJ_BEHAVIORS_2_H

#include <PR/ultratypes.h>

#include "types.h"

#define ATTACK_HANDLER_NOP                                 0x0
#define ATTACK_HANDLER_DIE_IF_HEALTH_NON_POSITIVE          0x1
#define ATTACK_HANDLER_KNOCKBACK                           0x2
#define ATTACK_HANDLER_SQUISHED                            0x3
#define ATTACK_HANDLER_SPECIAL_KOOPA_LOSE_SHELL            0x4
#define ATTACK_HANDLER_SET_SPEED_TO_ZERO                   0x5
#define ATTACK_HANDLER_SPECIAL_WIGGLER_JUMPED_ON           0x6
#define ATTACK_HANDLER_SPECIAL_HUGE_GOOMBA_WEAKLY_ATTACKED 0x7
#define ATTACK_HANDLER_SQUISHED_WITH_BLUE_COIN             0x8

void shelled_koopa_attack_handler(s32 attackType);
void obj_spit_fire(s16 relativePosX, s16 relativePosY, s16 relativePosZ, f32 scale, s32 model,
                   f32 startSpeed, f32 endSpeed, s16 movePitch);
void obj_set_speed_to_zero(void);

#endif // OBJ_BEHAVIORS_2_H
