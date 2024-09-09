#ifndef SEGMENT_SYMBOLS_H
#define SEGMENT_SYMBOLS_H

#ifndef NO_SEGMENTED_MEMORY
#define DECLARE_SEGMENT_SIDE(name, type, side) \
    extern u8 _##name##Segment##type##side[];
#define DECLARE_SEGMENT_IMPL(name, type) \
    DECLARE_SEGMENT_SIDE(name, type, Start) \
    DECLARE_SEGMENT_SIDE(name, type, End)
#define DECLARE_SEGMENT(name) \
    DECLARE_SEGMENT_IMPL(name, Rom) \
    DECLARE_SEGMENT_IMPL(name, )

// .bss:
#define DECLARE_NOLOAD(name) \
    DECLARE_SEGMENT_IMPL(name, Bss)
// .text:
#define DECLARE_TEXT(name) \
    DECLARE_SEGMENT_IMPL(name, Text)
// Segment and .bss:
#define DECLARE_SEGMENT_NOLOAD(name) \
    DECLARE_SEGMENT(name) \
    DECLARE_NOLOAD(name)
// Segment and .text:
#define DECLARE_SEGMENT_TEXT(name) \
    DECLARE_SEGMENT(name) \
    DECLARE_TEXT(name)
// Segment and .bss and .text:
#define DECLARE_SEGMENT_NOLOAD_TEXT(name) \
    DECLARE_SEGMENT(name) \
    DECLARE_NOLOAD(name) \
    DECLARE_TEXT(name)
// Compressed segment:
#define DECLARE_COMPRESSED(name) \
    DECLARE_SEGMENT(name##_yay0) \
    DECLARE_SEGMENT(name##_mio0)
// Skybox segment:
#define DECLARE_SKYBOX(name) \
    DECLARE_COMPRESSED(name##_skybox)
// Actor segment:
#define DECLARE_ACTOR_SEGMENT(name) \
    DECLARE_COMPRESSED(name) \
    DECLARE_SEGMENT(name##_geo) \
    DECLARE_NOLOAD(name##_geo) \
    DECLARE_TEXT(name##_geo)
// Level segment:
#define DECLARE_LEVEL_SEGMENT(name) \
    DECLARE_SEGMENT(name) \
    DECLARE_NOLOAD(name) \
    DECLARE_SEGMENT(name##_segment_7) \
    DECLARE_TEXT(name) \
    DECLARE_TEXT(name##_geo)



// Common:
DECLARE_ACTOR_SEGMENT(common0)  // Segment 8 (common0 gfx) | Segment 15 (common0 geo)
DECLARE_ACTOR_SEGMENT(common1)  // Segment 3 (common1 gfx) | Segment 22 (common1 geo)
DECLARE_ACTOR_SEGMENT(group0)   // Segment 4 (group0 gfx)  | Segment 23 (group0 geo)
// GroupA:
DECLARE_ACTOR_SEGMENT(group1)   // Segment 5 (groupA gfx)  | Segment 12 (groupA geo)
DECLARE_ACTOR_SEGMENT(group2)   // Segment 5 (groupA gfx)  | Segment 12 (groupA geo)
DECLARE_ACTOR_SEGMENT(group3)   // Segment 5 (groupA gfx)  | Segment 12 (groupA geo)
DECLARE_ACTOR_SEGMENT(group4)   // Segment 5 (groupA gfx)  | Segment 12 (groupA geo)
DECLARE_ACTOR_SEGMENT(group5)   // Segment 5 (groupA gfx)  | Segment 12 (groupA geo)
DECLARE_ACTOR_SEGMENT(group6)   // Segment 5 (groupA gfx)  | Segment 12 (groupA geo)
DECLARE_ACTOR_SEGMENT(group7)   // Segment 5 (groupA gfx)  | Segment 12 (groupA geo)
DECLARE_ACTOR_SEGMENT(group8)   // Segment 5 (groupA gfx)  | Segment 12 (groupA geo)
DECLARE_ACTOR_SEGMENT(group9)   // Segment 5 (groupA gfx)  | Segment 12 (groupA geo)
DECLARE_ACTOR_SEGMENT(group10)  // Segment 5 (groupA gfx)  | Segment 12 (groupA geo)
DECLARE_ACTOR_SEGMENT(group11)  // Segment 5 (groupA gfx)  | Segment 12 (groupA geo)
// GroupB:
DECLARE_ACTOR_SEGMENT(group12)  // Segment 6 (groupB gfx)  | Segment 13 (groupB geo)
DECLARE_ACTOR_SEGMENT(group13)  // Segment 6 (groupB gfx)  | Segment 13 (groupB geo)
DECLARE_ACTOR_SEGMENT(group14)  // Segment 6 (groupB gfx)  | Segment 13 (groupB geo)
DECLARE_ACTOR_SEGMENT(group15)  // Segment 6 (groupB gfx)  | Segment 13 (groupB geo)
DECLARE_ACTOR_SEGMENT(group16)  // Segment 6 (groupB gfx)  | Segment 13 (groupB geo)
DECLARE_ACTOR_SEGMENT(group17)  // Segment 6 (groupB gfx)  | Segment 13 (groupB geo)

// Misc:
DECLARE_SEGMENT_TEXT(boot)                  // Segment 4 (group0 gfx)
DECLARE_NOLOAD(zbuffer)                     // Hardcoded
DECLARE_NOLOAD(buffers)                     // Hardcoded
#ifdef HVQM
DECLARE_NOLOAD(hvqmwork)                    // Hardcoded
DECLARE_NOLOAD(adpcmbuf)                    // Hardcoded
DECLARE_NOLOAD(hvqbuf)                      // Hardcoded
DECLARE_SEGMENT(capcom)                     // Hardcoded
#endif // HVQM
DECLARE_SEGMENT_NOLOAD_TEXT(main)           // Hardcoded
DECLARE_SEGMENT_NOLOAD_TEXT(engine)         // Hardcoded
DECLARE_SEGMENT_NOLOAD_TEXT(crashScreen)    // Hardcoded
DECLARE_SEGMENT_NOLOAD(framebuffers)        // Hardcoded
DECLARE_SEGMENT(entry)                      // Segment 16 (level entry)
DECLARE_SEGMENT_NOLOAD_TEXT(behavior)       // Segment 19 (bhv data)
DECLARE_SEGMENT_NOLOAD_TEXT(goddard)        // Hardcoded
#ifdef KEEP_MARIO_HEAD
DECLARE_TEXT(libgoddard)                    // Hardcoded (part of goddard segment)
DECLARE_SEGMENT(gd_dynlists)                // Segment 4 (group0 gfx)
#endif // KEEP_MARIO_HEAD
DECLARE_SEGMENT(scripts)                    // Segment 21 (global level script)
DECLARE_SEGMENT(assets)                     // Hardcoded
DECLARE_SEGMENT(mapData)                    // Hardcoded

// Segment 7 (level data) | Segment 14 (level script) | Segment 20 (menu/intro)
DECLARE_LEVEL_SEGMENT(menu)
DECLARE_LEVEL_SEGMENT(intro)
DECLARE_LEVEL_SEGMENT(ending)

// Segment 7 (level data) | Segment 14 (level script)
#define STUB_LEVEL(_0, _1, _2, _3, _4, _5, _6, _7, _8)
#define DEFINE_LEVEL(_0, _1, _2, folder, _4, _5, _6, _7, _8, _9, _10) DECLARE_LEVEL_SEGMENT(folder)
#include "levels/level_defines.h"
#undef STUB_LEVEL
#undef DEFINE_LEVEL

// Segment 2 (hud gfx)
DECLARE_COMPRESSED(segment2)

// Segment 10 (skybox)
DECLARE_SKYBOX(water)
DECLARE_SKYBOX(ccm)
DECLARE_SKYBOX(clouds)
DECLARE_SKYBOX(bitfs)
DECLARE_SKYBOX(wdw)
DECLARE_SKYBOX(cloud_floor)
DECLARE_SKYBOX(ssl)
DECLARE_SKYBOX(bbh)
DECLARE_SKYBOX(bidw)
DECLARE_SKYBOX(bits)

// Segment 9 (texture)
DECLARE_COMPRESSED(fire)
DECLARE_COMPRESSED(spooky)
DECLARE_COMPRESSED(generic)
DECLARE_COMPRESSED(water)
DECLARE_COMPRESSED(sky)
DECLARE_COMPRESSED(snow)
DECLARE_COMPRESSED(cave)
DECLARE_COMPRESSED(machine)
DECLARE_COMPRESSED(mountain)
DECLARE_COMPRESSED(grass)
DECLARE_COMPRESSED(outside)
DECLARE_COMPRESSED(inside)
DECLARE_COMPRESSED(effect)
DECLARE_COMPRESSED(title_screen_bg)

// Segment 7 (level data)
DECLARE_COMPRESSED(debug_level_select)

#ifdef MULTILANG
// Segment 25 (languages)
DECLARE_COMPRESSED(translation_en)
#ifdef ENABLE_FRENCH
DECLARE_COMPRESSED(translation_fr)
#endif
#ifdef ENABLE_GERMAN
DECLARE_COMPRESSED(translation_de)
#endif
#ifdef ENABLE_JAPANESE
DECLARE_COMPRESSED(translation_jp)
#endif
#ifdef ENABLE_SPANISH
DECLARE_COMPRESSED(translation_es)
#endif
#endif // MULTILANG

#endif // !NO_SEGMENTED_MEMORY

#endif // SEGMENT_SYMBOLS_H
