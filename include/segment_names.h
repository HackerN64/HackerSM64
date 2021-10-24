#ifndef SEGMENT_NAMES_H
#define SEGMENT_NAMES_H

/**
 * TLB Segment Names
 * 
 * Most are set in sm64.ld, Makefile.split, and in level scripts, some are set elsewuere.
 * If you change these, make sure to also change them in Makefile.split and in your level scripts.
 * 
 * TODO: Make this an enum without breaking sm64.ld
 * TODO: Replace the remaining hardcoded segment numbers in level scripts with these
 * TODO: Find a way to use these with SEG_ADDRESS in Makefile.split
 */

#define SEGMENT_MAIN                 0x00 // | segment  0 | General Segment, includes most of /src/ (engine, buffers, goddard, audio, etc.)
#define SEGMENT_RENDER               0x01 // | segment  1 | SPTask, GFX pool buffer
#define SEGMENT_SEGMENT2             0x02 // | segment  2 | Segment 2 (Fonts, Text, etc)
#define SEGMENT_COMMON1_YAY0         0x03 // | segment  3 | /actors/common1_yay0
#define SEGMENT_GROUP0_YAY0          0x04 // | segment  4 | /actors/group0_yay0, also includes boot (/src/boot/) & gd_dynlists (/src/goddard/dynlists/)
#define SEGMENT_GROUPA_YAY0          0x05 // | segment  5 | Actor group A yay0 (group1  - group11)
#define SEGMENT_GROUPB_YAY0          0x06 // | segment  6 | Actor group B yay0 (group12 - group17)
#define SEGMENT_LEVEL_DATA           0x07 // | segment  7 | Level Data, also includes intro_segment_7, menu_segment_7, & /menu/debug_level_select/
#define SEGMENT_COMMON0_YAY0         0x08 // | segment  8 | /actors/common0_yay0
#define SEGMENT_TEXTURE              0x09 // | segment  9 | Shared texture bins (/textures/)
#define SEGMENT_SKYBOX               0x0A // | segment 10 | Skybox textures (/textures/skyboxes/), includes title screen background (/textures/title_screen_bg/)
#define SEGMENT_EFFECT_YAY0          0x0B // | segment 11 | effect_yay0
#define SEGMENT_GROUPA_GEO           0x0C // | segment 12 | Actor group A geo (group1  - group11)
#define SEGMENT_GROUPB_GEO           0x0D // | segment 13 | Actor group B geo (group12 - group17)
#define SEGMENT_ENDING_SCRIPT        0x0E // | segment 14 | level_ending_entry (/levels/ending/)
#define SEGMENT_COMMON0_GEO          0x0F // | segment 15 | /actors/common0_geo
#define SEGMENT_LEVEL_ENTRY          0x10 // | segment 16 | Level Script Entry
#define SEGMENT_MARIO_ANIMS          0x11 // | segment 17 | Mario Animations
#define SEGMENT_UNKNOWN_18           0x12 // | segment 18 | Unknown/Unused?
#define SEGMENT_BEHAVIOR_DATA        0x13 // | segment 19 | Behavior Data
#define SEGMENT_MENU_INTRO           0x14 // | segment 20 | /src/menu/, /levels/menu/, /levels/intro/
#define SEGMENT_GLOBAL_LEVEL_SCRIPT  0x15 // | segment 21 | Global level scripts: /levels/scripts
#define SEGMENT_COMMON1_GEO          0x16 // | segment 22 | /actors/common1_geo
#define SEGMENT_GROUP0_GEO           0x17 // | segment 23 | /actors/group0_geo
#define SEGMENT_DEMO_INPUTS          0x18 // | segment 24 | Demo Inputs List
#define SEGMENT_EU_TRANSLATION       0x19 // | segment 25 | EU language translations
#define SEGMENT_UNKNOWN_26           0x1A // | segment 26 | Unknown/Unused?
#define SEGMENT_UNKNOWN_27           0x1B // | segment 27 | Unknown/Unused?
#define SEGMENT_UNKNOWN_28           0x1C // | segment 28 | Unknown/Unused?
#define SEGMENT_UNKNOWN_29           0x1D // | segment 29 | Unknown/Unused?
#define SEGMENT_UNKNOWN_30           0x1E // | segment 30 | Unknown/Unused?
#define SEGMENT_UNKNOWN_31           0x1F // | segment 31 | Unknown/Unused?

#define SEG_ADDRESS(segment) ((segment) << 24)

#endif