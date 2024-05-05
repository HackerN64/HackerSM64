#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/cs_main.h"
#include "crash_screen/cs_descriptions.h"

#include "cs_segments.h"


// -- SEGMENTS --

#include "segment_data.inc.c"

typedef struct SegmentNameList {
    const IdNamePair* list;
    const char* name;
} SegmentNameList;
ALIGNED8 static const SegmentNameList sSegmentNames[NUM_TABLE_SEGS] = {
    [SEG_TABLE_MAIN               ] = { .list = sROMSegNames_00_main,                .name = "main",                },
    [SEG_TABLE_RENDER             ] = { .list = sROMSegNames_01_render,              .name = "render",              },
    [SEG_TABLE_SEGMENT2           ] = { .list = sROMSegNames_02_segment2,            .name = "hud gfx",             },
    [SEG_TABLE_COMMON1_YAY0       ] = { .list = sROMSegNames_03_common1_gfx,         .name = "common1 gfx",         },
    [SEG_TABLE_GROUP0_YAY0        ] = { .list = sROMSegNames_04_group0_gfx,          .name = "group0 gfx",          },
    [SEG_TABLE_GROUPA_YAY0        ] = { .list = sROMSegNames_05_groupA_gfx,          .name = "groupA gfx",          },
    [SEG_TABLE_GROUPB_YAY0        ] = { .list = sROMSegNames_06_groupB_gfx,          .name = "groupB gfx",          },
    [SEG_TABLE_LEVEL_DATA         ] = { .list = sROMSegNames_07_level_data,          .name = "level data",          },
    [SEG_TABLE_COMMON0_YAY0       ] = { .list = sROMSegNames_08_common0_gfx,         .name = "common0 gfx",         },
    [SEG_TABLE_TEXTURE            ] = { .list = sROMSegNames_09_texture,             .name = "textures bin",        },
    [SEG_TABLE_SKYBOX             ] = { .list = sROMSegNames_10_skybox,              .name = "skybox bin",          },
    [SEG_TABLE_EFFECT_YAY0        ] = { .list = sROMSegNames_11_effect_gfx,          .name = "effects gfx",         },
    [SEG_TABLE_GROUPA_GEO         ] = { .list = sROMSegNames_12_groupA_geo,          .name = "groupA geo",          },
    [SEG_TABLE_GROUPB_GEO         ] = { .list = sROMSegNames_13_groupB_geo,          .name = "groupB geo",          },
    [SEG_TABLE_LEVEL_SCRIPT       ] = { .list = sROMSegNames_14_level_script,        .name = "level script",        },
    [SEG_TABLE_COMMON0_GEO        ] = { .list = sROMSegNames_15_common0_geo,         .name = "common0 geo",         },
    [SEG_TABLE_LEVEL_ENTRY        ] = { .list = sROMSegNames_16_entry,               .name = "level entry",         },
    [SEG_TABLE_MARIO_ANIMS        ] = { .list = sROMSegNames_17_mario_anims,         .name = "mario anims",         },
    [SEG_TABLE_UNKNOWN_18         ] = { .list = NULL,                                .name = "unknown 18",          },
    [SEG_TABLE_BEHAVIOR_DATA      ] = { .list = sROMSegNames_19_behavior,            .name = "bhv data",            },
    [SEG_TABLE_MENU_INTRO         ] = { .list = sROMSegNames_20_menu_intro,          .name = "menu/intro",          },
    [SEG_TABLE_GLOBAL_LEVEL_SCRIPT] = { .list = sROMSegNames_21_global_level_script, .name = "global level script", },
    [SEG_TABLE_COMMON1_GEO        ] = { .list = sROMSegNames_22_common1_geo,         .name = "common1 geo",         },
    [SEG_TABLE_GROUP0_GEO         ] = { .list = sROMSegNames_23_group0_geo,          .name = "group0 geo",          },
    [SEG_TABLE_DEMO_INPUTS        ] = { .list = sROMSegNames_24_demo_inputs,         .name = "demo inputs",         },
    [SEG_TABLE_EU_TRANSLATION     ] = { .list = sROMSegNames_25_eu_translation,      .name = "languages",           },
    [SEG_TABLE_UNKNOWN_26         ] = { .list = NULL,                                .name = "unknown 26",          },
    [SEG_TABLE_UNKNOWN_27         ] = { .list = NULL,                                .name = "unknown 27",          },
    [SEG_TABLE_UNKNOWN_28         ] = { .list = NULL,                                .name = "unknown 28",          },
    [SEG_TABLE_UNKNOWN_29         ] = { .list = NULL,                                .name = "unknown 29",          },
    [SEG_TABLE_UNKNOWN_30         ] = { .list = NULL,                                .name = "unknown 30",          },
    [SEG_TABLE_UNKNOWN_31         ] = { .list = NULL,                                .name = "unknown 31",          },
};
typedef struct HardcodedSegmentInfo {
    /*0x00*/ const char* name;
    /*0x00*/ const Address pStart;
    /*0x04*/ const Address pEnd;
    /*0x08*/ const Address vStart;
    /*0x0C*/ const Address vEnd;
} HardcodedSegmentInfo; /*0x10*/
#define HARDCODED_SEG(_name, _seg, _romType, _startType, _endType) {    \
    .name   = _name,                                                    \
    .pStart = (const Address)_##_seg##Segment##_romType##Start,         \
    .pEnd   = (const Address)_##_seg##Segment##_romType##End,           \
    .vStart = (const Address)_##_seg##Segment##_startType##Start,       \
    .vEnd   = (const Address)_##_seg##Segment##_endType##End,           \
}
static const HardcodedSegmentInfo sHardcodedSegmentInfos[NUM_HARDCODED_SEGS] = {
    [SEG_HARDCODED_MAIN        ] = HARDCODED_SEG("main",         main,         Rom,    , Bss),
    [SEG_HARDCODED_ENGINE      ] = HARDCODED_SEG("engine",       engine,       Rom,    , Bss),
    [SEG_HARDCODED_CRASH_SCREEN] = HARDCODED_SEG("crash screen", crashScreen,  Rom,    , Bss),
    [SEG_HARDCODED_FRAMEBUFFERS] = HARDCODED_SEG("framebuffers", framebuffers, Bss, Bss, Bss),
    [SEG_HARDCODED_ZBUFFER     ] = HARDCODED_SEG("zbuffer",      zbuffer,      Bss, Bss, Bss),
    [SEG_HARDCODED_BUFFERS     ] = HARDCODED_SEG("buffers",      buffers,      Bss, Bss, Bss),
    [SEG_HARDCODED_MAPDATA     ] = HARDCODED_SEG("map symbols",  mapData,      Rom,    ,    ),
    [SEG_HARDCODED_GODDARD     ] = HARDCODED_SEG("goddard",      goddard,      Rom,    , Bss),
};

_Bool is_segment_hardcoded(SegmentsList segment) {
    return (IS_HARDCODED_FIRST() ? (segment < SEG_TABLE_START) : (segment >= SEG_HARDCODED_START));
}

_Bool is_segment_loaded(SegmentsList segment) {
    //! TODO: Is it possible to check if hardcoded segments are loaded?
    if (is_segment_hardcoded(segment)) {
        return TRUE;
    }

    return (FITS_IN_ARRAY(segment, sSegmentROMTable) ? (sSegmentROMTable[SEGMENT_LIST_TO_TABLE(segment)] != (Address)NULL) : FALSE);
}

static const HardcodedSegmentInfo* get_hardcoded_segment_info_from_list(SegmentsList segment) {
    HardcodedSegments hseg = SEGMENT_LIST_TO_HARDCODED(segment);
    return (FITS_IN_ARRAY(hseg, sHardcodedSegmentInfos) ? &sHardcodedSegmentInfos[hseg] : NULL);
}

// Get the name of a segment.
const char* get_segment_name(TableSegments segment) {
    return (FITS_IN_ARRAY(segment, sSegmentNames) ? sSegmentNames[segment].name : "unknown");
}
const char* cs_get_segment_name(SegmentsList segment) {
    if (is_segment_hardcoded(segment)) {
        const HardcodedSegmentInfo* info = get_hardcoded_segment_info_from_list(segment);
        return ((info != NULL) ? info->name : "unknown");
    } else {
        return get_segment_name(SEGMENT_LIST_TO_TABLE(segment));
    }
}
// Get the name of what is currently loaded in a segment.
const char* get_segment_sub_name(TableSegments segment) {
    const char* ret = NULL;

    Address romAddr = sSegmentROMTable[segment];
    if (romAddr == (uintptr_t)NULL) {
        return "unloaded";
    }

    const IdNamePair* list = sSegmentNames[segment].list;
    if (list != NULL) {
        ret = get_name_from_null_terminated_id_list(romAddr, list);
    }

    return str_null_fallback(ret, "unknown");
}
const char* cs_get_segment_sub_name(SegmentsList segment) {
    const char* ret = NULL;

    if (!is_segment_hardcoded(segment)) {
        ret = get_segment_sub_name(SEGMENT_LIST_TO_TABLE(segment));
    }

    return str_null_fallback(ret, "unknown");
}

Address cs_get_segment_vAddr(SegmentsList segment) {
    if (is_segment_hardcoded(segment)) {
        const HardcodedSegmentInfo* info = get_hardcoded_segment_info_from_list(segment);
        return ((info != NULL) ? info->vStart : (Address)NULL);
    } else {
        return (Address)get_segment_base_addr(SEGMENT_LIST_TO_TABLE(segment));
    }
}

Address cs_get_segment_pAddr(SegmentsList segment) {
    if (is_segment_hardcoded(segment)) {
        const HardcodedSegmentInfo* info = get_hardcoded_segment_info_from_list(segment);
        return ((info != NULL) ? info->pStart : (Address)NULL);
    } else {
        TableSegments tseg = SEGMENT_LIST_TO_TABLE(segment);
        return (FITS_IN_ARRAY(tseg, sSegmentROMTable) ? sSegmentROMTable[tseg] : (Address)NULL);
    }
}

size_t cs_get_segment_size(SegmentsList segment) {
    if (is_segment_hardcoded(segment)) {
        const HardcodedSegmentInfo* info = get_hardcoded_segment_info_from_list(segment);
        return ((info != NULL) ? (info->vEnd - info->vStart) : 0);
    } else {
        TableSegments tseg = SEGMENT_LIST_TO_TABLE(segment);
        return (FITS_IN_ARRAY(tseg, sSegmentSizes) ? sSegmentSizes[tseg] : (Address)NULL);
    }
}
