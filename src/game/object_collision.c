#include <PR/ultratypes.h>

#include "sm64.h"
#include "debug.h"
#include "interaction.h"
#include "mario.h"
#include "object_list_processor.h"
#include "spawn_object.h"
#include "engine/math_util.h"

UNUSED struct Object *debug_print_obj_collision(struct Object *a) {
    struct Object *currCollidedObj;
    s32 i;

    for (i = 0; i < a->numCollidedObjs; i++) {
        print_debug_top_down_objectinfo("ON", 0);
        currCollidedObj = a->collidedObjs[i];
        if (currCollidedObj != gMarioObject) {
            return currCollidedObj;
        }
    }
    return NULL;
}

s32 detect_object_hitbox_overlap(struct Object *a, struct Object *b) {
    f32 dya_bottom = a->oPosY - a->hitboxDownOffset;
    f32 dyb_bottom = b->oPosY - b->hitboxDownOffset;
    f32 dx = a->oPosX - b->oPosX;
    f32 dz = a->oPosZ - b->oPosZ;
    f32 collisionRadius = a->hitboxRadius + b->hitboxRadius;
    f32 distance = sqr(dx) + sqr(dz);

    if (sqr(collisionRadius) > distance) {
        f32 dya_top = a->hitboxHeight + dya_bottom;
        f32 dyb_top = b->hitboxHeight + dyb_bottom;

        if (dya_bottom > dyb_top
            || dya_top < dyb_bottom
            || a->numCollidedObjs >= 4
            || b->numCollidedObjs >= 4) {
            return FALSE;
        }
        a->collidedObjs[a->numCollidedObjs] = b;
        b->collidedObjs[b->numCollidedObjs] = a;
        a->collidedObjInteractTypes |= b->oInteractType;
        b->collidedObjInteractTypes |= a->oInteractType;
        a->numCollidedObjs++;
        b->numCollidedObjs++;
        return TRUE;
    }

    return FALSE;
}

s32 detect_object_hurtbox_overlap(struct Object *a, struct Object *b) {
    f32 dya_bottom = a->oPosY - a->hitboxDownOffset;
    f32 dyb_bottom = b->oPosY - b->hitboxDownOffset;
    f32 dx = a->oPosX - b->oPosX;
    f32 dz = a->oPosZ - b->oPosZ;
    f32 collisionRadius = a->hurtboxRadius + b->hurtboxRadius;
    f32 distance = sqr(dx) + sqr(dz);

    if (a == gMarioObject) {
        b->oInteractionSubtype |= INT_SUBTYPE_DELAY_INVINCIBILITY;
    }

    if (sqr(collisionRadius) > distance) {
        f32 dya_top = a->hitboxHeight  + dya_bottom;
        f32 dyb_top = b->hurtboxHeight + dyb_bottom;

        if (dya_bottom > dyb_top || dya_top < dyb_bottom) {
            return FALSE;
        }
        if (a == gMarioObject) {
            b->oInteractionSubtype &= ~INT_SUBTYPE_DELAY_INVINCIBILITY;
        }
        return TRUE;
    }

    return FALSE;
}

void clear_object_collision(struct Object *a) {
    struct Object *nextObj = (struct Object *) a->header.next;

    while (nextObj != a) {
        nextObj->numCollidedObjs = 0;
        nextObj->collidedObjInteractTypes = 0;
        if (nextObj->oIntangibleTimer > 0) {
            nextObj->oIntangibleTimer--;
        }
        nextObj = (struct Object *) nextObj->header.next;
    }
}

void check_collision_in_list(struct Object *a, struct Object *b, struct Object *c) {
    if (a->oIntangibleTimer == 0) {
        while (b != c) {
            if (b->oIntangibleTimer == 0) {
                if (detect_object_hitbox_overlap(a, b) && b->hurtboxRadius != 0.0f) {
                    detect_object_hurtbox_overlap(a, b);
                }
            }
            b = (struct Object *) b->header.next;
        }
    }
}

void check_player_object_collision(void) {
    struct Object *playerObj = (struct Object *) &gObjectLists[OBJ_LIST_PLAYER];
    struct Object   *nextObj = (struct Object *) playerObj->header.next;

    while (nextObj != playerObj) {
        check_collision_in_list(nextObj, (struct Object *) nextObj->header.next, playerObj);
        check_collision_in_list(nextObj,
                      (struct Object *)  gObjectLists[OBJ_LIST_POLELIKE].next,
                      (struct Object *) &gObjectLists[OBJ_LIST_POLELIKE]);
        check_collision_in_list(nextObj,
                      (struct Object *)  gObjectLists[OBJ_LIST_LEVEL].next,
                      (struct Object *) &gObjectLists[OBJ_LIST_LEVEL]);
        check_collision_in_list(nextObj,
                      (struct Object *)  gObjectLists[OBJ_LIST_GENACTOR].next,
                      (struct Object *) &gObjectLists[OBJ_LIST_GENACTOR]);
        check_collision_in_list(nextObj,
                      (struct Object *)  gObjectLists[OBJ_LIST_PUSHABLE].next,
                      (struct Object *) &gObjectLists[OBJ_LIST_PUSHABLE]);
        check_collision_in_list(nextObj,
                      (struct Object *)  gObjectLists[OBJ_LIST_SURFACE].next,
                      (struct Object *) &gObjectLists[OBJ_LIST_SURFACE]);
        check_collision_in_list(nextObj,
                      (struct Object *)  gObjectLists[OBJ_LIST_DESTRUCTIVE].next,
                      (struct Object *) &gObjectLists[OBJ_LIST_DESTRUCTIVE]);
        nextObj = (struct Object *) nextObj->header.next;
    }
}

void check_pushable_object_collision(void) {
    struct Object *pushableObj = (struct Object *) &gObjectLists[OBJ_LIST_PUSHABLE];
    struct Object *nextObj = (struct Object *) pushableObj->header.next;

    while (nextObj != pushableObj) {
        check_collision_in_list(nextObj, (struct Object *) nextObj->header.next, pushableObj);
        nextObj = (struct Object *) nextObj->header.next;
    }
}

void check_destructive_object_collision(void) {
    struct Object *destructiveObj = (struct Object *) &gObjectLists[OBJ_LIST_DESTRUCTIVE];
    struct Object *nextObj = (struct Object *) destructiveObj->header.next;

    while (nextObj != destructiveObj) {
        if (nextObj->oDistanceToMario < 2000.0f && !(nextObj->activeFlags & ACTIVE_FLAG_DESTRUCTIVE_OBJ_DONT_DESTROY)) {
            check_collision_in_list(nextObj, (struct Object *) nextObj->header.next, destructiveObj);
            check_collision_in_list(nextObj, (struct Object *) gObjectLists[OBJ_LIST_GENACTOR].next,
                          (struct Object *) &gObjectLists[OBJ_LIST_GENACTOR]);
            check_collision_in_list(nextObj, (struct Object *) gObjectLists[OBJ_LIST_PUSHABLE].next,
                          (struct Object *) &gObjectLists[OBJ_LIST_PUSHABLE]);
            check_collision_in_list(nextObj, (struct Object *) gObjectLists[OBJ_LIST_SURFACE].next,
                          (struct Object *) &gObjectLists[OBJ_LIST_SURFACE]);
        }
        nextObj = (struct Object *) nextObj->header.next;
    }
}

void detect_object_collisions(void) {
    clear_object_collision((struct Object *) &gObjectLists[OBJ_LIST_POLELIKE]);
    clear_object_collision((struct Object *) &gObjectLists[OBJ_LIST_PLAYER]);
    clear_object_collision((struct Object *) &gObjectLists[OBJ_LIST_PUSHABLE]);
    clear_object_collision((struct Object *) &gObjectLists[OBJ_LIST_GENACTOR]);
    clear_object_collision((struct Object *) &gObjectLists[OBJ_LIST_LEVEL]);
    clear_object_collision((struct Object *) &gObjectLists[OBJ_LIST_SURFACE]);
    clear_object_collision((struct Object *) &gObjectLists[OBJ_LIST_DESTRUCTIVE]);
    check_player_object_collision();
    check_destructive_object_collision();
    check_pushable_object_collision();
}
