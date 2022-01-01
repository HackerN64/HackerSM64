#include <PR/ultratypes.h>

#include "sm64.h"
#include "object_helpers.h"
#include "macro_special_objects.h"
#include "object_list_processor.h"

#include "behavior_data.h"

#include "macro_presets.h"

#include "special_presets.h"

/*
 * Converts the rotation value supplied by macro objects into one
 * that can be used by in-game objects.
 */
s32 convert_rotation(s16 inRotation) {
    u16 rotation = ((u16)(inRotation & 0xFF) << 8);
    switch (rotation) {
        case 0x3F00: rotation = 0x4000; break;
        case 0x7F00: rotation = 0x8000; break;
        case 0xBF00: rotation = 0xC000; break;
        case 0xFF00: rotation = 0x0000; break;
    }
    return (s16) rotation;
}

/*
 * Spawns an object at an absolute location with rotation around the y-axis and
 * parameters filling up the upper 2 bytes of newObj->oBehParams.
 * The object will not spawn if 'behavior' is NULL.
 */
void spawn_macro_abs_yrot_2params(ModelID32 model, const BehaviorScript *behavior, s16 x, s16 y, s16 z, s16 ry, s16 params) {
    if (behavior != NULL) {
        struct Object *newObj =
            spawn_object_abs_with_rot(&gMacroObjectDefaultParent, 0, model, behavior, x, y, z, 0, convert_rotation(ry), 0);
        newObj->oBehParams = ((u32) params) << 16;
    }
}

/*
 * Spawns an object at an absolute location with rotation around the y-axis and
 * a single parameter filling up the upper byte of newObj->oBehParams.
 * The object will not spawn if 'behavior' is NULL.
 */
void spawn_macro_abs_yrot_param1(ModelID32 model, const BehaviorScript *behavior, s16 x, s16 y, s16 z, s16 ry, s16 param) {
    if (behavior != NULL) {
        struct Object *newObj =
            spawn_object_abs_with_rot(&gMacroObjectDefaultParent, 0, model, behavior, x, y, z, 0, convert_rotation(ry), 0);
        newObj->oBehParams = ((u32) param) << 24;
    }
}

/*
 * Spawns an object at an absolute location with currently 3 unknown variables that get converted to
 * floats. Oddly enough, this function doesn't care if 'behavior' is NULL or not.
 */
void spawn_macro_abs_special(ModelID32 model, const BehaviorScript *behavior, s16 x, s16 y, s16 z, s16 unkA, s16 unkB, s16 unkC) {
    struct Object *newObj =
        spawn_object_abs_with_rot(&gMacroObjectDefaultParent, 0, model, behavior, x, y, z, 0, 0, 0);

    // Are all three of these values unused?
    newObj->oMacroUnk108 = (f32) unkA;
    newObj->oMacroUnk10C = (f32) unkB;
    newObj->oMacroUnk110 = (f32) unkC;
}

UNUSED static void spawn_macro_coin_unknown(const BehaviorScript *behavior, s16 a1[]) {
    struct Object *obj;
    ModelID16 model = bhvYellowCoin == behavior ? MODEL_YELLOW_COIN : MODEL_NONE;

    obj = spawn_object_abs_with_rot(&gMacroObjectDefaultParent, 0, model, behavior,
                                    a1[1], a1[2], a1[3], 0, convert_rotation(a1[0]), 0);

    obj->oUnusedCoinParams = a1[4];
    obj->oBehParams = (a1[4] & 0xFF) >> 16;
}

struct LoadedMacroObject {
    /*0x00*/ Angle yaw;
    /*0x02*/ Vec3s pos;
    /*0x08*/ s16 params;
};

void spawn_macro_objects(s32 areaIndex, MacroObject *macroObjList) {
    s32 presetID;
    struct LoadedMacroObject macroObject;
    struct Object *newObj;
    struct MacroPreset preset;

    gMacroObjectDefaultParent.header.gfx.areaIndex = areaIndex;
    gMacroObjectDefaultParent.header.gfx.activeAreaIndex = areaIndex;

    while (TRUE) {
        if (*macroObjList == -1) { // An encountered value of -1 means the list has ended.
            break;
        }

        presetID = (*macroObjList & 0x1FF) - 31; // Preset identifier for MacroObjectPresets array

        if (presetID < 0) {
            break;
        }

        // Set macro object properties from the list
        macroObject.yaw    = ((*macroObjList++ >> 9) & 0x7F) << 1; // Y-Rotation
        macroObject.pos[0] = *macroObjList++;                      // X position
        macroObject.pos[1] = *macroObjList++;                      // Y position
        macroObject.pos[2] = *macroObjList++;                      // Z position
        macroObject.params = *macroObjList++;                      // Behavior params

        // Get the preset values from the MacroObjectPresets list.
        preset = MacroObjectPresets[presetID];

        // If the preset has a defined param, replace the lower bits with the preset param.
        // The lower bits are later used for bparam2.
        if (preset.param != 0) {
            macroObject.params = (macroObject.params & 0xFF00) + (preset.param & 0x00FF);
        }

        // If object has been killed (bparam3 check), prevent it from respawning
        if ((GET_BPARAM3(macroObject.params) & RESPAWN_INFO_DONT_RESPAWN) != RESPAWN_INFO_DONT_RESPAWN) {
            // Spawn the new macro object.
            newObj = spawn_object_abs_with_rot(
                         &gMacroObjectDefaultParent,        // Parent object
                         0,                                 // Unused
                         preset.model,                      // Model ID
                         preset.behavior,                   // Behavior address
                         macroObject.pos[0],                // X-position
                         macroObject.pos[1],                // Y-position
                         macroObject.pos[2],                // Z-position
                         0x0,                               // X-rotation
                         convert_rotation(macroObject.yaw), // Y-rotation
                         0x0                                // Z-rotation
                     );

            newObj->oUnusedCoinParams =    macroObject.params;
            newObj->oBehParams        = (((macroObject.params & 0x00FF) << 16) // Set 2nd byte from lower bits (shifted).
                                        | (macroObject.params & 0xFF00));      // Set 3rd byte from upper bits.
            newObj->oBehParams2ndByte =   (macroObject.params & 0x00FF);       // Set 2nd byte from lower bits.
            newObj->respawnInfoType = RESPAWN_INFO_TYPE_MACRO_OBJECT;
            newObj->respawnInfo = macroObjList - 1;
            newObj->parentObj = newObj;
        }
    }
}

void spawn_macro_objects_hardcoded(s32 areaIndex, MacroObject *macroObjList) {
    // This version of macroObjList has the preset and Y-Rotation separated,
    // and lacks behavior params. Might be an early version of the macro object list?
    Vec3s pos;
    s16 macroObjPreset;
    s16 yaw;

    gMacroObjectDefaultParent.header.gfx.areaIndex = areaIndex;
    gMacroObjectDefaultParent.header.gfx.activeAreaIndex = areaIndex;

    while (TRUE) {
        macroObjPreset = *macroObjList++;

        if (macroObjPreset < 0) {
            break;
        }

        pos[0] = *macroObjList++;
        pos[1] = *macroObjList++;
        pos[2] = *macroObjList++;
        yaw    = *macroObjList++;

        // Spawn objects based on hardcoded presets, and most seem to be for Big Boo's Haunt.
        // However, BBH doesn't use this function so this might just be an early test?
        switch (macroObjPreset) {
            case  0: spawn_macro_abs_yrot_2params(MODEL_NONE,                       bhvBooStaircase,           pos[0], pos[1], pos[2], yaw, 0); break;
            case  1: spawn_macro_abs_yrot_2params(MODEL_BBH_TILTING_FLOOR_PLATFORM, bhvBbhTiltingTrapPlatform, pos[0], pos[1], pos[2], yaw, 0); break;
            case  2: spawn_macro_abs_yrot_2params(MODEL_BBH_TUMBLING_PLATFORM,      bhvBbhTumblingBridge,      pos[0], pos[1], pos[2], yaw, 0); break;
            case  3: spawn_macro_abs_yrot_2params(MODEL_BBH_MOVING_BOOKSHELF,       bhvHauntedBookshelf,       pos[0], pos[1], pos[2], yaw, 0); break;
            case  4: spawn_macro_abs_yrot_2params(MODEL_BBH_MESH_ELEVATOR,          bhvMeshElevator,           pos[0], pos[1], pos[2], yaw, 0); break;
            case 20: spawn_macro_abs_yrot_2params(MODEL_YELLOW_COIN,                bhvYellowCoin,             pos[0], pos[1], pos[2], yaw, 0); break;
            case 21: spawn_macro_abs_yrot_2params(MODEL_YELLOW_COIN,                bhvYellowCoin,             pos[0], pos[1], pos[2], yaw, 0); break;
            default: break;
        }
    }
}

void spawn_special_objects(s32 areaIndex, TerrainData **specialObjList) {
    s32 i;
    s32 offset;
    Vec3s pos;
    s16 extraParams[4];
    ModelID16 model;
    u8 type;
    u8 presetID;
    u8 defaultParam;
    const BehaviorScript *behavior;

    s32 numOfSpecialObjects = *(*specialObjList)++;

    gMacroObjectDefaultParent.header.gfx.areaIndex = areaIndex;
    gMacroObjectDefaultParent.header.gfx.activeAreaIndex = areaIndex;

    for (i = 0; i < numOfSpecialObjects; i++) {
        presetID = *(*specialObjList)++;
        pos[0]   = *(*specialObjList)++;
        pos[1]   = *(*specialObjList)++;
        pos[2]   = *(*specialObjList)++;

        offset = 0;
        while (TRUE) {
            if (SpecialObjectPresets[offset].preset_id == presetID) {
                break;
            }
            offset++;
        }

        model = SpecialObjectPresets[offset].model;
        behavior = SpecialObjectPresets[offset].behavior;
        type = SpecialObjectPresets[offset].type;
        defaultParam = SpecialObjectPresets[offset].defParam;

        switch (type) {
            case SPTYPE_NO_YROT_OR_PARAMS:
                spawn_macro_abs_yrot_2params(model, behavior, pos[0], pos[1], pos[2], 0, 0);
                break;
            case SPTYPE_YROT_NO_PARAMS:
                extraParams[0] = *(*specialObjList)++; // Y-rotation
                spawn_macro_abs_yrot_2params(model, behavior, pos[0], pos[1], pos[2], extraParams[0], 0);
                break;
            case SPTYPE_PARAMS_AND_YROT:
                extraParams[0] = *(*specialObjList)++; // Y-rotation
                extraParams[1] = *(*specialObjList)++; // Params
                spawn_macro_abs_yrot_2params(model, behavior, pos[0], pos[1], pos[2], extraParams[0], extraParams[1]);
                break;
            case SPTYPE_UNKNOWN:
                extraParams[0] = *(*specialObjList)++; // Unknown, gets put into obj->oMacroUnk108 as a float
                extraParams[1] = *(*specialObjList)++; // Unknown, gets put into obj->oMacroUnk10C as a float
                extraParams[2] = *(*specialObjList)++; // Unknown, gets put into obj->oMacroUnk110 as a float
                spawn_macro_abs_special(model, behavior, pos[0], pos[1], pos[2], extraParams[0], extraParams[1], extraParams[2]);
                break;
            case SPTYPE_DEF_PARAM_AND_YROT:
                extraParams[0] = *(*specialObjList)++; // Y-rotation
                spawn_macro_abs_yrot_param1(model, behavior, pos[0], pos[1], pos[2], extraParams[0], defaultParam);
                break;
            default:
                break;
        }
    }
}

#ifdef NO_SEGMENTED_MEMORY
u32 get_special_objects_size(s16 *data) {
    s16 *startPos = data;
    s32 i;
    u8 presetID;
    s32 offset;

    s32 numOfSpecialObjects = *data++;

    for (i = 0; i < numOfSpecialObjects; i++) {
        presetID = (u8) *data++;
        data += 3;
        offset = 0;

        while (TRUE) {
            if (SpecialObjectPresets[offset].preset_id == presetID) {
                break;
            }
            offset++;
        }

        switch (SpecialObjectPresets[offset].type) {
            case SPTYPE_NO_YROT_OR_PARAMS:
                break;
            case SPTYPE_YROT_NO_PARAMS:
                data++;
                break;
            case SPTYPE_PARAMS_AND_YROT:
                data += 2;
                break;
            case SPTYPE_UNKNOWN:
                data += 3;
                break;
            case SPTYPE_DEF_PARAM_AND_YROT:
                data++;
                break;
            default:
                break;
        }
    }

    return data - startPos;
}
#endif
