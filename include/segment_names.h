#ifndef SEGMENT_NAMES_H
#define SEGMENT_NAMES_H

/**
 * TLB Segment Names
 * 
 * Most are set in sm64.ld, Makefile.split, and in level scripts, some are set elsewuere.
 * If you change these numbers, make sure to also change them in Makefile.split,
 *  and in your level scripts (in places marked with *seg*).
 * 
 * TODO: Make this an enum without breaking sm64.ld
 * TODO: Find a way to use these with SEG_ADDRESS in sm64.ld without breaking Fast64
 * TODO: Find a way to use these with SEG_ADDRESS in Makefile.split
 */

#define SEGMENT_MAIN                 0x00 // | Segment  0 | General Segment, includes most of /src/ (engine, buffers, goddard, audio, etc.)
#define SEGMENT_RENDER               0x01 // | Segment  1 | SPTask, GFX pool buffer
#define SEGMENT_SEGMENT2             0x02 // | Segment  2 | Segment 2 (Fonts, Text, etc)
#define SEGMENT_COMMON1_YAY0         0x03 // | Segment  3 | /actors/common1_yay0
#define SEGMENT_GROUP0_YAY0          0x04 // | Segment  4 | /actors/group0_yay0, also includes boot (/src/boot/) & gd_dynlists (/src/goddard/dynlists/)
#define SEGMENT_GROUPA_YAY0          0x05 // | Segment  5 | Actor group A yay0 (group1  - group11)
#define SEGMENT_GROUPB_YAY0          0x06 // | Segment  6 | Actor group B yay0 (group12 - group17)
#define SEGMENT_LEVEL_DATA           0x07 // | Segment  7 | Level Data, also includes intro_segment_7, menu_segment_7, & /menu/debug_level_select/
#define SEGMENT_COMMON0_YAY0         0x08 // | Segment  8 | /actors/common0_yay0
#define SEGMENT_TEXTURE              0x09 // | Segment  9 | Shared texture bins (/textures/)
#define SEGMENT_SKYBOX               0x0A // | Segment 10 | Skybox textures (/textures/skyboxes/), includes title screen background (/textures/title_screen_bg/)
#define SEGMENT_EFFECT_YAY0          0x0B // | Segment 11 | effect_yay0
#define SEGMENT_GROUPA_GEO           0x0C // | Segment 12 | Actor group A geo (group1  - group11)
#define SEGMENT_GROUPB_GEO           0x0D // | Segment 13 | Actor group B geo (group12 - group17)
#define SEGMENT_LEVEL_SCRIPT         0x0E // | Segment 14 | level_ending_entry (/levels/ending/), DEFINE_LEVEL
#define SEGMENT_COMMON0_GEO          0x0F // | Segment 15 | /actors/common0_geo
#define SEGMENT_LEVEL_ENTRY          0x10 // | Segment 16 | Level Script Entry
#define SEGMENT_MARIO_ANIMS          0x11 // | Segment 17 | Mario Animations
#define SEGMENT_UNKNOWN_18           0x12 // | Segment 18 | Unknown/Unused?
#define SEGMENT_BEHAVIOR_DATA        0x13 // | Segment 19 | Behavior Data
#define SEGMENT_MENU_INTRO           0x14 // | Segment 20 | /src/menu/, /levels/menu/, /levels/intro/
#define SEGMENT_GLOBAL_LEVEL_SCRIPT  0x15 // | Segment 21 | Global level scripts: /levels/scripts
#define SEGMENT_COMMON1_GEO          0x16 // | Segment 22 | /actors/common1_geo
#define SEGMENT_GROUP0_GEO           0x17 // | Segment 23 | /actors/group0_geo
#define SEGMENT_DEMO_INPUTS          0x18 // | Segment 24 | Demo Inputs List
#define SEGMENT_EU_TRANSLATION       0x19 // | Segment 25 | EU language translations
#define SEGMENT_UNKNOWN_26           0x1A // | Segment 26 | Unknown/Unused?
#define SEGMENT_UNKNOWN_27           0x1B // | Segment 27 | Unknown/Unused?
#define SEGMENT_UNKNOWN_28           0x1C // | Segment 28 | Unknown/Unused?
#define SEGMENT_UNKNOWN_29           0x1D // | Segment 29 | Unknown/Unused?
#define SEGMENT_UNKNOWN_30           0x1E // | Segment 30 | Unknown/Unused?
#define SEGMENT_UNKNOWN_31           0x1F // | Segment 31 | Unknown/Unused?

#define SEG_ADDRESS(segment) ((segment) << 24)

#endif
