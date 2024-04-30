#pragma once


#include "segments.h"
#include "crash_screen/cs_descriptions.h"
#include "buffers/buffers.h"


#define DEF_ROM_SEG_IMPL(_name, _str) { .id = (uintptr_t)_##_name##SegmentRomStart, .name = EXPAND_AND_STRINGIFY(_str), } 
#define DEF_ROM_SEG(_name)            DEF_ROM_SEG_IMPL(_name,                      _name)
#define DEF_ROM_SEG_YAY0(_name)       DEF_ROM_SEG_IMPL(_name##_yay0,               _name)
#define DEF_ROM_SEG_GEO(_name)        DEF_ROM_SEG_IMPL(_name##_geo,                _name)
#define DEF_ROM_SEG_SKY_YAY0(_name)   DEF_ROM_SEG_IMPL(_name##_skybox_yay0,        _name)
#define DEF_ROM_SEG_SEG7(_name)       DEF_ROM_SEG_IMPL(_name##_segment_7,          _name)
#define DEF_ROM_SEG_LANG_YAY0(_name)  DEF_ROM_SEG_IMPL(translation_##_name##_yay0, _name)
#define DEF_ROM_SEG_GLOBAL_VAR(_name) { .id = (uintptr_t)_name, .name = EXPAND_AND_STRINGIFY(_name), }

// 00: SEGMENT_MAIN
static const IdNamePair sROMSegNames_00_main[] = {
    DEF_ROM_SEG(main),
    ID_LIST_END(),
};
// 01: SEGMENT_RENDER
static const IdNamePair sROMSegNames_01_render[] = {
    { .id = OS_K0_TO_PHYSICAL(gGfxPools[0].buffer), .name = "gGfxPools[0]", },
    { .id = OS_K0_TO_PHYSICAL(gGfxPools[1].buffer), .name = "gGfxPools[1]", },
    ID_LIST_END(),
};
// 02: SEGMENT_SEGMENT2
static const IdNamePair sROMSegNames_02_segment2[] = {
    DEF_ROM_SEG_YAY0(segment2),
    ID_LIST_END(),
};
// 03: SEGMENT_COMMON1_YAY0
static const IdNamePair sROMSegNames_03_common1_gfx[] = {
    DEF_ROM_SEG_YAY0(common1),
    ID_LIST_END(),
};
// 04: SEGMENT_GROUP0_YAY0
static const IdNamePair sROMSegNames_04_group0_gfx[] = {
    DEF_ROM_SEG_YAY0(group0),
    DEF_ROM_SEG(boot),
    ID_LIST_END(),
};
// 05: SEGMENT_GROUPA_YAY0
static const IdNamePair sROMSegNames_05_groupA_gfx[] = {
    DEF_ROM_SEG_YAY0(group1),
    DEF_ROM_SEG_YAY0(group2),
    DEF_ROM_SEG_YAY0(group3),
    DEF_ROM_SEG_YAY0(group4),
    DEF_ROM_SEG_YAY0(group5),
    DEF_ROM_SEG_YAY0(group6),
    DEF_ROM_SEG_YAY0(group7),
    DEF_ROM_SEG_YAY0(group8),
    DEF_ROM_SEG_YAY0(group9),
    DEF_ROM_SEG_YAY0(group10),
    DEF_ROM_SEG_YAY0(group11),
    ID_LIST_END(),
};
// 06: SEGMENT_GROUPB_YAY0
static const IdNamePair sROMSegNames_06_groupB_gfx[] = {
    DEF_ROM_SEG_YAY0(group12),
    DEF_ROM_SEG_YAY0(group13),
    DEF_ROM_SEG_YAY0(group14),
    DEF_ROM_SEG_YAY0(group15),
    DEF_ROM_SEG_YAY0(group16),
    DEF_ROM_SEG_YAY0(group17),
    ID_LIST_END(),
};
// 07: SEGMENT_LEVEL_DATA
static const IdNamePair sROMSegNames_07_level_data[] = {
#define STUB_LEVEL(_0, _1, _2, _3, _4, _5, _6, _7, _8)
#define DEFINE_LEVEL(textname, _1, _2, folder, _4, _5, _6, _7, _8, _9, _10) DEF_ROM_SEG_SEG7(folder),
#include "levels/level_defines.h"
#undef STUB_LEVEL
#undef DEFINE_LEVEL
    DEF_ROM_SEG_YAY0(debug_level_select),
    ID_LIST_END(),
};
// 08: SEGMENT_COMMON0_YAY0
static const IdNamePair sROMSegNames_08_common0_gfx[] = {
    DEF_ROM_SEG_YAY0(common0),
    ID_LIST_END(),
};
// 09: SEGMENT_TEXTURE
static const IdNamePair sROMSegNames_09_texture[] = {
    DEF_ROM_SEG_YAY0(fire),
    DEF_ROM_SEG_YAY0(spooky),
    DEF_ROM_SEG_YAY0(generic),
    DEF_ROM_SEG_YAY0(water),
    DEF_ROM_SEG_YAY0(sky),
    DEF_ROM_SEG_YAY0(snow),
    DEF_ROM_SEG_YAY0(cave),
    DEF_ROM_SEG_YAY0(machine),
    DEF_ROM_SEG_YAY0(mountain),
    DEF_ROM_SEG_YAY0(grass),
    DEF_ROM_SEG_YAY0(outside),
    DEF_ROM_SEG_YAY0(inside),
    DEF_ROM_SEG_YAY0(effect),
    ID_LIST_END(),
};
// 10: SEGMENT_SKYBOX
static const IdNamePair sROMSegNames_10_skybox[] = {
    DEF_ROM_SEG_SKY_YAY0(water),
    DEF_ROM_SEG_SKY_YAY0(ccm),
    DEF_ROM_SEG_SKY_YAY0(clouds),
    DEF_ROM_SEG_SKY_YAY0(bitfs),
    DEF_ROM_SEG_SKY_YAY0(wdw),
    DEF_ROM_SEG_SKY_YAY0(cloud_floor),
    DEF_ROM_SEG_SKY_YAY0(ssl),
    DEF_ROM_SEG_SKY_YAY0(bbh),
    DEF_ROM_SEG_SKY_YAY0(bidw),
    DEF_ROM_SEG_SKY_YAY0(bits),
    DEF_ROM_SEG_YAY0(title_screen_bg), // For some reason the game uses the skybox segment for this.
    ID_LIST_END(),
};
// 11: SEGMENT_COMMON0_YAY0
static const IdNamePair sROMSegNames_11_effect_gfx[] = {
    DEF_ROM_SEG_YAY0(effect),
    ID_LIST_END(),
};
// 12: SEGMENT_GROUPA_GEO
static const IdNamePair sROMSegNames_12_groupA_geo[] = {
    DEF_ROM_SEG_GEO(group1),
    DEF_ROM_SEG_GEO(group2),
    DEF_ROM_SEG_GEO(group3),
    DEF_ROM_SEG_GEO(group4),
    DEF_ROM_SEG_GEO(group5),
    DEF_ROM_SEG_GEO(group6),
    DEF_ROM_SEG_GEO(group7),
    DEF_ROM_SEG_GEO(group8),
    DEF_ROM_SEG_GEO(group9),
    DEF_ROM_SEG_GEO(group10),
    DEF_ROM_SEG_GEO(group11),
    ID_LIST_END(),
};
// 13: SEGMENT_GROUPB_GEO
static const IdNamePair sROMSegNames_13_groupB_geo[] = {
    DEF_ROM_SEG_GEO(group12),
    DEF_ROM_SEG_GEO(group13),
    DEF_ROM_SEG_GEO(group14),
    DEF_ROM_SEG_GEO(group15),
    DEF_ROM_SEG_GEO(group16),
    DEF_ROM_SEG_GEO(group17),
    ID_LIST_END(),
};
// 14: SEGMENT_LEVEL_SCRIPT
static const IdNamePair sROMSegNames_14_level_script[] = {
#define STUB_LEVEL(_0, _1, _2, _3, _4, _5, _6, _7, _8)
#define DEFINE_LEVEL(textname, _1, _2, folder, _4, _5, _6, _7, _8, _9, _10) DEF_ROM_SEG(folder),
#include "levels/level_defines.h"
#undef STUB_LEVEL
#undef DEFINE_LEVEL
    DEF_ROM_SEG(ending),
    ID_LIST_END(),
};
// 15: SEGMENT_COMMON0_GEO
static const IdNamePair sROMSegNames_15_common0_geo[] = {
    DEF_ROM_SEG_GEO(common0),
    ID_LIST_END(),
};
// 16: SEGMENT_LEVEL_ENTRY
static const IdNamePair sROMSegNames_16_entry[] = {
    DEF_ROM_SEG(entry),
    ID_LIST_END(),
};
// 17: SEGMENT_MARIO_ANIMS
static const IdNamePair sROMSegNames_17_mario_anims[] = {
    DEF_ROM_SEG_GLOBAL_VAR(gMarioAnims),
    ID_LIST_END(),
};
// 18: SEGMENT_UNKNOWN_18
// 19: SEGMENT_BEHAVIOR_DATA
static const IdNamePair sROMSegNames_19_behavior[] = {
    DEF_ROM_SEG(behavior),
    ID_LIST_END(),
};
// 20: SEGMENT_MENU_INTRO
static const IdNamePair sROMSegNames_20_menu_intro[] = {
    DEF_ROM_SEG(intro),
    DEF_ROM_SEG(menu),
    DEF_ROM_SEG(ending),
    ID_LIST_END(),
};
// 21: SEGMENT_GLOBAL_LEVEL_SCRIPT
static const IdNamePair sROMSegNames_21_global_level_script[] = {
    DEF_ROM_SEG(scripts),
    ID_LIST_END(),
};
// 22: SEGMENT_COMMON1_GEO
static const IdNamePair sROMSegNames_22_common1_geo[] = {
    DEF_ROM_SEG_GEO(common1),
    ID_LIST_END(),
};
// 23: SEGMENT_GROUP0_GEO
static const IdNamePair sROMSegNames_23_group0_geo[] = {
    DEF_ROM_SEG_GEO(group0),
    ID_LIST_END(),
};
// 24: SEGMENT_DEMO_INPUTS
static const IdNamePair sROMSegNames_24_demo_inputs[] = {
    DEF_ROM_SEG_GLOBAL_VAR(gDemoInputs),
    ID_LIST_END(),
};
// 25: SEGMENT_EU_TRANSLATION
//! TODO: Update this when ASCII PR is merged:
static const IdNamePair sROMSegNames_25_eu_translation[] = {
#if MULTILANG
    DEF_ROM_SEG_IMPL(translation_en_yay0, english),
    DEF_ROM_SEG_IMPL(translation_fr_yay0, french),
    DEF_ROM_SEG_IMPL(translation_de_yay0, german),
#endif // MULTILANG
    ID_LIST_END(),
};
// 26: SEGMENT_UNKNOWN_26
// 27: SEGMENT_UNKNOWN_27
// 28: SEGMENT_UNKNOWN_28
// 29: SEGMENT_UNKNOWN_29
// 30: SEGMENT_UNKNOWN_30
// 31: SEGMENT_UNKNOWN_31
