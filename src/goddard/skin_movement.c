#include <PR/ultratypes.h>

#include "debug_utils.h"
#include "gd_math.h"
#include "gd_types.h"
#include "joints.h"
#include "macros.h"
#include "objects.h"
#include "skin.h"
#include "skin_movement.h"

/* bss */
s32 sResetWeightVtxNum;
struct ObjWeight *sResetCurWeight;
static Mat4f sHead2Mtx;

/* @ 22FF30 for 0xDC */
/* called with ObjNext->unk1A8 (variable obj ptr?) ->unk20 or ->unk24 ptr*/
// TODO: figure out the proper object type for a0
void scale_verts(struct ObjGroup *group) {
    register f32 scale;
    register struct ListNode *link;
    struct ObjVertex *vtx;

    for (link = group->firstMember; link != NULL; link = link->next) {
        vtx   = (struct ObjVertex *) link->obj;
        scale = vtx->scaleFactor;
        if (scale != 0.0f) {
            vtx->pos.x = (vtx->initPos.x * scale);
            vtx->pos.y = (vtx->initPos.y * scale);
            vtx->pos.z = (vtx->initPos.z * scale);
        } else {
            vtx->pos.x = vtx->pos.y = vtx->pos.z = 0.0f;
        }
    }
}

/* @ 23000C for 0x58; orig name: func8018183C*/
void move_skin(struct ObjNet *net) {
    if (net->shapePtr != NULL) {
        scale_verts(net->shapePtr->scaledVtxGroup);
    }
}

/* @ 230064 for 0x13C*/
void func_80181894(struct ObjJoint *joint) {
    register struct ObjGroup *weightGroup = joint->weightGrp; // baseGroup? weights Only?
    struct GdVec3f stackVec;
    register struct ObjWeight *curWeight;
    register struct ObjVertex *connectedVtx;
    register struct ListNode *link;
    register f32 scaleFactor;
    struct GdObj *linkedObj;

    if (weightGroup != NULL) {
        for (link = weightGroup->firstMember; link != NULL; link = link->next) {
            linkedObj = link->obj;
            curWeight = (struct ObjWeight *) linkedObj;

            if (curWeight->weightVal > 0.0f) {
                stackVec.x = curWeight->vec20.x;
                stackVec.y = curWeight->vec20.y;
                stackVec.z = curWeight->vec20.z;
                gd_rotate_and_translate_vec3f(&stackVec, &joint->matE8);

                connectedVtx = curWeight->vtx;
                scaleFactor = curWeight->weightVal;

                connectedVtx->pos.x += (stackVec.x * scaleFactor);
                connectedVtx->pos.y += (stackVec.y * scaleFactor);
                connectedVtx->pos.z += (stackVec.z * scaleFactor);
            }
        }
    }
}

/* @ 2301A0 for 0x110 */
void reset_weight_vtx(struct ObjVertex *vtx) {
    struct GdVec3f localVec;

    if (sResetWeightVtxNum++ == sResetCurWeight->vtxId) {  // found matching vertex
        sResetCurWeight->vtx = vtx;
        localVec.x = vtx->pos.x;
        localVec.y = vtx->pos.y;
        localVec.z = vtx->pos.z;

        gd_rotate_and_translate_vec3f(&localVec, &sHead2Mtx);
        sResetCurWeight->vec20.x = localVec.x;
        sResetCurWeight->vec20.y = localVec.y;
        sResetCurWeight->vec20.z = localVec.z;

        vtx->scaleFactor -= sResetCurWeight->weightVal;
    }
}

void reset_weight(struct ObjWeight *weight) {
    struct ObjGroup *skinGroup;

    sResetCurWeight = weight;
    sResetWeightVtxNum = 0;
    if ((skinGroup = gGdSkinNet->skinGrp) != NULL) {
        // Go through every vertex in the skin group, and reset the weight if the vertex is managed by the weight
        apply_to_obj_types_in_group(OBJ_TYPE_VERTICES, (applyproc_t) reset_weight_vtx, skinGroup);
    } else {
        fatal_printf("reset_weight(): Skin net has no SkinGroup");
    }

    if (weight->vtx == NULL) {
        fatal_printf("reset_weight(): Skin vertex ID %d not found", weight->vtxId);
    }
}

void reset_joint_weights(struct ObjJoint *joint) {
    gd_inverse_mat4f(&joint->matE8, &sHead2Mtx);
    struct ObjGroup *group = joint->weightGrp;
    if (group != NULL) {
        apply_to_obj_types_in_group(OBJ_TYPE_WEIGHTS, (applyproc_t) reset_weight, group);
    }
}
