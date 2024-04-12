#ifndef SURFACE_TERRAINS_H
#define SURFACE_TERRAINS_H

#include "config.h"

// Surface Types
enum SurfaceTypes {
    SURFACE_NULL = -1,
    SURFACE_DEFAULT,                    // 0x0000 // Environment default
    SURFACE_BURNING,                    // 0x0001 // Lava / Frostbite (in SL), but is used mostly for Lava
    SURFACE_NEW_WATER,                  // 0x0002 // Custom water surface
    SURFACE_NEW_WATER_BOTTOM,           // 0x0003 // Custom water surface bottom marker
    SURFACE_0004,                       // 0x0004 // Unused, has no function and has parameters
    SURFACE_HANGABLE,                   // 0x0005 // Ceiling that Mario can climb on
    SURFACE_0006,                       // 0x0006 // Unused
    SURFACE_SUPER_SLIPPERY,             // 0x0007 // Super slippery surface, good for delimiting your level
    SURFACE_0008,                       // 0x0008 // Unused
    SURFACE_SLOW,                       // 0x0009 // Slow down Mario, unused
    SURFACE_DEATH_PLANE,                // 0x000A // Death floor. Warps to ID of force parameter's second byte if set, otherwise warps to 0xF3 if it exists, otherwise defaults to ID 0xF1.
    SURFACE_CLOSE_CAMERA,               // 0x000B // Close camera
    SURFACE_000C,                       // 0x000C // unused
    SURFACE_WATER,                      // 0x000D // Water, has no action, used on some waterboxes below
    SURFACE_FLOWING_WATER,              // 0x000E // Water (flowing), has parameters
    SURFACE_000F,                       // 0x000F // Unused
    SURFACE_0010,                       // 0x0010 // Unused
    SURFACE_0011,                       // 0x0011 // Unused
    SURFACE_INTANGIBLE,                 // 0x0012 // Intangible (Separates BBH mansion from merry-go-round, for room usage)
    SURFACE_VERY_SLIPPERY,              // 0x0013 // Very slippery, mostly used for slides
    SURFACE_SLIPPERY,                   // 0x0014 // Slippery
    SURFACE_NOT_SLIPPERY,               // 0x0015 // Non-slippery, climbable
    SURFACE_TTM_VINES,                  // 0x0016 // TTM vines, has no action defined
    SURFACE_0017,                       // 0x0017 // Unused
    SURFACE_0018,                       // 0x0018 // Unused
    SURFACE_0019,                       // 0x0019 // Unused
    SURFACE_MGR_MUSIC,                  // 0x001A // Plays the Merry go round music, see handle_merry_go_round_music in bbh_merry_go_round.inc.c for more details
    SURFACE_INSTANT_WARP_1B,            // 0x001B // Instant warp to another area, used to warp between areas in WDW and the endless stairs to warp back
    SURFACE_INSTANT_WARP_1C,            // 0x001C // Instant warp to another area, used to warp between areas in WDW
    SURFACE_INSTANT_WARP_1D,            // 0x001D // Instant warp to another area, used to warp between areas in DDD, SSL and TTM
    SURFACE_INSTANT_WARP_1E,            // 0x001E // Instant warp to another area, used to warp between areas in DDD, SSL and TTM
    SURFACE_001F,                       // 0x001F // Unused
    SURFACE_0020,                       // 0x0020 // Unused
    SURFACE_SHALLOW_QUICKSAND,          // 0x0021 // Shallow Quicksand (depth of 10 units)
    SURFACE_DEEP_QUICKSAND,             // 0x0022 // Quicksand (lethal, slow, depth of 160 units)
    SURFACE_INSTANT_QUICKSAND,          // 0x0023 // Quicksand (lethal, instant)
    SURFACE_DEEP_MOVING_QUICKSAND,      // 0x0024 // Moving quicksand (flowing, depth of 160 units)
    SURFACE_SHALLOW_MOVING_QUICKSAND,   // 0x0025 // Moving quicksand (flowing, depth of 25 units)
    SURFACE_QUICKSAND,                  // 0x0026 // Moving quicksand (60 units)
    SURFACE_MOVING_QUICKSAND,           // 0x0027 // Moving quicksand (flowing, depth of 60 units)
    SURFACE_WALL_MISC,                  // 0x0028 // Used for some walls, Cannon to adjust the camera, and some objects like Warp Pipe
    SURFACE_NOISE_DEFAULT,              // 0x0029 // Default floor with noise
    SURFACE_NOISE_SLIPPERY,             // 0x002A // Slippery floor with noise
    SURFACE_002B,                       // 0x002B // Unused
    SURFACE_HORIZONTAL_WIND,            // 0x002C // Horizontal wind, has parameters
    SURFACE_INSTANT_MOVING_QUICKSAND,   // 0x002D // Quicksand (lethal, flowing)
    SURFACE_ICE,                        // 0x002E // Slippery Ice, in snow levels and THI's water floor
    SURFACE_LOOK_UP_WARP,               // 0x002F // Look up and warp (Wing cap entrance)
    SURFACE_HARD,                       // 0x0030 // Hard floor (Always has fall damage)
    SURFACE_0031,                       // 0x0031 // Unused
    SURFACE_WARP,                       // 0x0032 // Surface warp. Warps to ID of force parameter's second byte if set, otherwise defaults to ID 0xF3.
    SURFACE_TIMER_START,                // 0x0033 // Timer start (Peach's secret slide)
    SURFACE_TIMER_END,                  // 0x0034 // Timer stop (Peach's secret slide)
    SURFACE_HARD_SLIPPERY,              // 0x0035 // Hard and slippery (Always has fall damage)
    SURFACE_HARD_VERY_SLIPPERY,         // 0x0036 // Hard and very slippery (Always has fall damage)
    SURFACE_HARD_NOT_SLIPPERY,          // 0x0037 // Hard and Non-slippery (Always has fall damage)
    SURFACE_VERTICAL_WIND,              // 0x0038 // Death at bottom with vertical wind. Warps to ID of force parameter's second byte if set, otherwise warps to 0xF3 if it exists, otherwise defaults to ID 0xF1.
    SURFACE_0039,                       // 0x0039 // Unused
    SURFACE_003A,                       // 0x003A // Unused
    SURFACE_003B,                       // 0x003B // Unused
    SURFACE_003C,                       // 0x003C // Unused
    SURFACE_003D,                       // 0x003D // Unused
    SURFACE_003E,                       // 0x003E // Unused
    SURFACE_003F,                       // 0x003F // Unused
    SURFACE_BOSS_FIGHT_CAMERA = 0x65,   // 0x0065 // Wide camera for BoB and WF bosses
    SURFACE_CAMERA_FREE_ROAM,           // 0x0066 // Free roam camera for THI and TTC
    SURFACE_0067,                       // 0x0067 // Unused
    SURFACE_THI3_WALLKICK,              // 0x0068 // Surface where there's a wall kick section in THI 3rd area, has no action defined
    SURFACE_CAMERA_8_DIR,               // 0x0069 // Surface that enables far camera for platforms, used in THI
    SURFACE_006A,                       // 0x006A // Unused
    SURFACE_006B,                       // 0x006B // Unused
    SURFACE_006C,                       // 0x006C // Unused
    SURFACE_006D,                       // 0x006D // Unused
    SURFACE_CAMERA_MIDDLE,              // 0x006E // Surface camera that returns to the middle, used on the 4 pillars of SSL
    SURFACE_CAMERA_ROTATE_RIGHT,        // 0x006F // Surface camera that rotates to the right (Bowser 1 & THI)
    SURFACE_CAMERA_ROTATE_LEFT,         // 0x0070 // Surface camera that rotates to the left (BoB & TTM)
    SURFACE_0071,                       // 0x0071 // Unused
    SURFACE_CAMERA_BOUNDARY,            // 0x0072 // Intangible Area, only used to restrict camera movement
    SURFACE_NOISE_VERY_SLIPPERY_73,     // 0x0073 // Very slippery floor with noise, unused
    SURFACE_NOISE_VERY_SLIPPERY_74,     // 0x0074 // Very slippery floor with noise, unused
    SURFACE_NOISE_VERY_SLIPPERY,        // 0x0075 // Very slippery floor with noise, used in CCM
    SURFACE_NO_CAM_COLLISION,           // 0x0076 // Surface with no cam collision flag
    SURFACE_NO_CAM_COLLISION_77,        // 0x0077 // Surface with no cam collision flag, unused
    SURFACE_NO_CAM_COL_VERY_SLIPPERY,   // 0x0078 // Surface with no cam collision flag, very slippery with noise (THI)
    SURFACE_NO_CAM_COL_SLIPPERY,        // 0x0079 // Surface with no cam collision flag, slippery with noise (CCM, PSS and TTM slides)
    SURFACE_SWITCH,                     // 0x007A // Surface with no cam collision flag, non-slippery with noise, used by switches and Dorrie
    SURFACE_VANISH_CAP_WALLS,           // 0x007B // Vanish cap walls, pass through them with Vanish Cap
    SURFACE_007C,                       // 0x007C // Unused
    SURFACE_007D,                       // 0x007D // Unused
    SURFACE_007E,                       // 0x007E // Unused
    SURFACE_007F,                       // 0x007F // Unused
    SURFACE_0080,                       // 0x0080 // Unused
    SURFACE_0081,                       // 0x0081 // Unused
    SURFACE_0082,                       // 0x0082 // Unused
    SURFACE_0083,                       // 0x0083 // Unused
    SURFACE_0084,                       // 0x0084 // Unused
    SURFACE_0085,                       // 0x0085 // Unused
    SURFACE_0086,                       // 0x0086 // Unused
    SURFACE_0087,                       // 0x0087 // Unused
    SURFACE_0088,                       // 0x0088 // Unused
    SURFACE_0089,                       // 0x0089 // Unused
    SURFACE_008A,                       // 0x008A // Unused
    SURFACE_008B,                       // 0x008B // Unused
    SURFACE_008C,                       // 0x008C // Unused
    SURFACE_008D,                       // 0x008D // Unused
    SURFACE_008E,                       // 0x008E // Unused
    SURFACE_008F,                       // 0x008F // Unused
    SURFACE_0090,                       // 0x0090 // Unused
    SURFACE_0091,                       // 0x0091 // Unused
    SURFACE_0092,                       // 0x0092 // Unused
    SURFACE_0093,                       // 0x0093 // Unused
    SURFACE_0094,                       // 0x0094 // Unused
    SURFACE_0095,                       // 0x0095 // Unused
    SURFACE_0096,                       // 0x0096 // Unused
    SURFACE_0097,                       // 0x0097 // Unused
    SURFACE_0098,                       // 0x0098 // Unused
    SURFACE_0099,                       // 0x0099 // Unused
    SURFACE_009A,                       // 0x009A // Unused
    SURFACE_009B,                       // 0x009B // Unused
    SURFACE_009C,                       // 0x009C // Unused
    SURFACE_009D,                       // 0x009D // Unused
    SURFACE_009E,                       // 0x009E // Unused
    SURFACE_009F,                       // 0x009F // Unused
    SURFACE_00A0,                       // 0x00A0 // Unused
    SURFACE_00A1,                       // 0x00A1 // Unused
    SURFACE_00A2,                       // 0x00A2 // Unused
    SURFACE_00A3,                       // 0x00A3 // Unused
    SURFACE_00A4,                       // 0x00A4 // Unused
    SURFACE_00A5,                       // 0x00A5 // Unused
    SURFACE_PAINTING_WOBBLE_A6,         // 0x00A6 // Painting wobble (BoB Left)
    SURFACE_PAINTING_WOBBLE_A7,         // 0x00A7 // Painting wobble (BoB Middle)
    SURFACE_PAINTING_WOBBLE_A8,         // 0x00A8 // Painting wobble (BoB Right)
    SURFACE_PAINTING_WOBBLE_A9,         // 0x00A9 // Painting wobble (CCM Left)
    SURFACE_PAINTING_WOBBLE_AA,         // 0x00AA // Painting wobble (CCM Middle)
    SURFACE_PAINTING_WOBBLE_AB,         // 0x00AB // Painting wobble (CCM Right)
    SURFACE_PAINTING_WOBBLE_AC,         // 0x00AC // Painting wobble (WF Left)
    SURFACE_PAINTING_WOBBLE_AD,         // 0x00AD // Painting wobble (WF Middle)
    SURFACE_PAINTING_WOBBLE_AE,         // 0x00AE // Painting wobble (WF Right)
    SURFACE_PAINTING_WOBBLE_AF,         // 0x00AF // Painting wobble (JRB Left)
    SURFACE_PAINTING_WOBBLE_B0,         // 0x00B0 // Painting wobble (JRB Middle)
    SURFACE_PAINTING_WOBBLE_B1,         // 0x00B1 // Painting wobble (JRB Right)
    SURFACE_PAINTING_WOBBLE_B2,         // 0x00B2 // Painting wobble (LLL Left)
    SURFACE_PAINTING_WOBBLE_B3,         // 0x00B3 // Painting wobble (LLL Middle)
    SURFACE_PAINTING_WOBBLE_B4,         // 0x00B4 // Painting wobble (LLL Right)
    SURFACE_PAINTING_WOBBLE_B5,         // 0x00B5 // Painting wobble (SSL Left)
    SURFACE_PAINTING_WOBBLE_B6,         // 0x00B6 // Painting wobble (SSL Middle)
    SURFACE_PAINTING_WOBBLE_B7,         // 0x00B7 // Painting wobble (SSL Right)
    SURFACE_PAINTING_WOBBLE_B8,         // 0x00B8 // Painting wobble (Unused - Left)
    SURFACE_PAINTING_WOBBLE_B9,         // 0x00B9 // Painting wobble (Unused - Middle)
    SURFACE_PAINTING_WOBBLE_BA,         // 0x00BA // Painting wobble (Unused - Right)
    SURFACE_PAINTING_WOBBLE_BB,         // 0x00BB // Painting wobble (DDD - Left), makes the painting wobble if touched
    SURFACE_PAINTING_WOBBLE_BC,         // 0x00BC // Painting wobble (Unused, DDD - Middle)
    SURFACE_PAINTING_WOBBLE_BD,         // 0x00BD // Painting wobble (Unused, DDD - Right)
    SURFACE_PAINTING_WOBBLE_BE,         // 0x00BE // Painting wobble (WDW Left)
    SURFACE_PAINTING_WOBBLE_BF,         // 0x00BF // Painting wobble (WDW Middle)
    SURFACE_PAINTING_WOBBLE_C0,         // 0x00C0 // Painting wobble (WDW Right)
    SURFACE_PAINTING_WOBBLE_C1,         // 0x00C1 // Painting wobble (THI Tiny - Left)
    SURFACE_PAINTING_WOBBLE_C2,         // 0x00C2 // Painting wobble (THI Tiny - Middle)
    SURFACE_PAINTING_WOBBLE_C3,         // 0x00C3 // Painting wobble (THI Tiny - Right)
    SURFACE_PAINTING_WOBBLE_C4,         // 0x00C4 // Painting wobble (TTM Left)
    SURFACE_PAINTING_WOBBLE_C5,         // 0x00C5 // Painting wobble (TTM Middle)
    SURFACE_PAINTING_WOBBLE_C6,         // 0x00C6 // Painting wobble (TTM Right)
    SURFACE_PAINTING_WOBBLE_C7,         // 0x00C7 // Painting wobble (Unused, TTC - Left)
    SURFACE_PAINTING_WOBBLE_C8,         // 0x00C8 // Painting wobble (Unused, TTC - Middle)
    SURFACE_PAINTING_WOBBLE_C9,         // 0x00C9 // Painting wobble (Unused, TTC - Right)
    SURFACE_PAINTING_WOBBLE_CA,         // 0x00CA // Painting wobble (Unused, SL - Left)
    SURFACE_PAINTING_WOBBLE_CB,         // 0x00CB // Painting wobble (Unused, SL - Middle)
    SURFACE_PAINTING_WOBBLE_CC,         // 0x00CC // Painting wobble (Unused, SL - Right)
    SURFACE_PAINTING_WOBBLE_CD,         // 0x00CD // Painting wobble (THI Huge - Left)
    SURFACE_PAINTING_WOBBLE_CE,         // 0x00CE // Painting wobble (THI Huge - Middle)
    SURFACE_PAINTING_WOBBLE_CF,         // 0x00CF // Painting wobble (THI Huge - Right)
    SURFACE_PAINTING_WOBBLE_D0,         // 0x00D0 // Painting wobble (HMC & COTMC - Left), makes the painting wobble if touched
    SURFACE_PAINTING_WOBBLE_D1,         // 0x00D1 // Painting wobble (Unused, HMC & COTMC - Middle)
    SURFACE_PAINTING_WOBBLE_D2,         // 0x00D2 // Painting wobble (Unused, HMC & COTMC - Right)
    SURFACE_PAINTING_WARP_D3,           // 0x00D3 // Painting warp (BoB Left)
    SURFACE_PAINTING_WARP_D4,           // 0x00D4 // Painting warp (BoB Middle)
    SURFACE_PAINTING_WARP_D5,           // 0x00D5 // Painting warp (BoB Right)
    SURFACE_PAINTING_WARP_D6,           // 0x00D6 // Painting warp (CCM Left)
    SURFACE_PAINTING_WARP_D7,           // 0x00D7 // Painting warp (CCM Middle)
    SURFACE_PAINTING_WARP_D8,           // 0x00D8 // Painting warp (CCM Right)
    SURFACE_PAINTING_WARP_D9,           // 0x00D9 // Painting warp (WF Left)
    SURFACE_PAINTING_WARP_DA,           // 0x00DA // Painting warp (WF Middle)
    SURFACE_PAINTING_WARP_DB,           // 0x00DB // Painting warp (WF Right)
    SURFACE_PAINTING_WARP_DC,           // 0x00DC // Painting warp (JRB Left)
    SURFACE_PAINTING_WARP_DD,           // 0x00DD // Painting warp (JRB Middle)
    SURFACE_PAINTING_WARP_DE,           // 0x00DE // Painting warp (JRB Right)
    SURFACE_PAINTING_WARP_DF,           // 0x00DF // Painting warp (LLL Left)
    SURFACE_PAINTING_WARP_E0,           // 0x00E0 // Painting warp (LLL Middle)
    SURFACE_PAINTING_WARP_E1,           // 0x00E1 // Painting warp (LLL Right)
    SURFACE_PAINTING_WARP_E2,           // 0x00E2 // Painting warp (SSL Left)
    SURFACE_PAINTING_WARP_E3,           // 0x00E3 // Painting warp (SSL Medium)
    SURFACE_PAINTING_WARP_E4,           // 0x00E4 // Painting warp (SSL Right)
    SURFACE_PAINTING_WARP_E5,           // 0x00E5 // Painting warp (Unused - Left)
    SURFACE_PAINTING_WARP_E6,           // 0x00E6 // Painting warp (Unused - Medium)
    SURFACE_PAINTING_WARP_E7,           // 0x00E7 // Painting warp (Unused - Right)
    SURFACE_PAINTING_WARP_E8,           // 0x00E8 // Painting warp (DDD - Left)
    SURFACE_PAINTING_WARP_E9,           // 0x00E9 // Painting warp (DDD - Middle)
    SURFACE_PAINTING_WARP_EA,           // 0x00EA // Painting warp (DDD - Right)
    SURFACE_PAINTING_WARP_EB,           // 0x00EB // Painting warp (WDW Left)
    SURFACE_PAINTING_WARP_EC,           // 0x00EC // Painting warp (WDW Middle)
    SURFACE_PAINTING_WARP_ED,           // 0x00ED // Painting warp (WDW Right)
    SURFACE_PAINTING_WARP_EE,           // 0x00EE // Painting warp (THI Tiny - Left)
    SURFACE_PAINTING_WARP_EF,           // 0x00EF // Painting warp (THI Tiny - Middle)
    SURFACE_PAINTING_WARP_F0,           // 0x00F0 // Painting warp (THI Tiny - Right)
    SURFACE_PAINTING_WARP_F1,           // 0x00F1 // Painting warp (TTM Left)
    SURFACE_PAINTING_WARP_F2,           // 0x00F2 // Painting warp (TTM Middle)
    SURFACE_PAINTING_WARP_F3,           // 0x00F3 // Painting warp (TTM Right)
    SURFACE_TTC_PAINTING_1,             // 0x00F4 // Painting warp (TTC Left)
    SURFACE_TTC_PAINTING_2,             // 0x00F5 // Painting warp (TTC Medium)
    SURFACE_TTC_PAINTING_3,             // 0x00F6 // Painting warp (TTC Right)
    SURFACE_PAINTING_WARP_F7,           // 0x00F7 // Painting warp (SL Left)
    SURFACE_PAINTING_WARP_F8,           // 0x00F8 // Painting warp (SL Middle)
    SURFACE_PAINTING_WARP_F9,           // 0x00F9 // Painting warp (SL Right)
    SURFACE_PAINTING_WARP_FA,           // 0x00FA // Painting warp (THI Huge - Left)
    SURFACE_PAINTING_WARP_FB,           // 0x00FB // Painting warp (THI Huge - Middle)
    SURFACE_PAINTING_WARP_FC,           // 0x00FC // Painting warp (THI Huge - Right)
    SURFACE_WOBBLING_WARP,              // 0x00FD // Pool warp (HMC & DDD)
    SURFACE_00FE,                       // 0x00FE // Unused
    SURFACE_TRAPDOOR,                   // 0x00FF // Bowser Left trapdoor, has no action defined
};

// From Surface 0x1B to 0x1E
#define INSTANT_WARP_INDEX_START  0x00 // Equal and greater than Surface 0x1B
#define INSTANT_WARP_INDEX_STOP   0x04 // Less than Surface 0x1F

#define SURFACE_IS_NEW_WATER(cmd)               (((cmd) == SURFACE_NEW_WATER) || ((cmd) == SURFACE_NEW_WATER_BOTTOM))
#define SURFACE_IS_QUICKSAND(cmd)               ((((cmd) >= SURFACE_SHALLOW_QUICKSAND) && ((cmd) <= SURFACE_MOVING_QUICKSAND)) || ((cmd) == SURFACE_INSTANT_MOVING_QUICKSAND))
#define SURFACE_IS_NOT_HARD(cmd)                (((cmd) != SURFACE_HARD) && !((cmd) >= SURFACE_HARD_SLIPPERY && ((cmd) <= SURFACE_HARD_NOT_SLIPPERY)))
#define SURFACE_IS_PAINTING_WOBBLE(cmd)         (((cmd) >= SURFACE_PAINTING_WOBBLE_A6) && ((cmd) <= SURFACE_PAINTING_WOBBLE_D2))
#define SURFACE_IS_PAINTING_WOBBLE_LEFT(cmd)    ((((cmd) - SURFACE_PAINTING_WOBBLE_A6) % 3) == 0)
#define SURFACE_IS_PAINTING_WOBBLE_MIDDLE(cmd)  ((((cmd) - SURFACE_PAINTING_WOBBLE_A7) % 3) == 0)
#define SURFACE_IS_PAINTING_WOBBLE_RIGHT(cmd)   ((((cmd) - SURFACE_PAINTING_WOBBLE_A8) % 3) == 0)
#define SURFACE_IS_PAINTING_WARP(cmd)           (((cmd) >= SURFACE_PAINTING_WARP_D3) && ((cmd) < SURFACE_WOBBLING_WARP)) // skips SURFACE_WOBBLING_WARP
#define SURFACE_IS_PAINTING_WARP_LEFT(cmd)      ((((cmd) - SURFACE_PAINTING_WARP_D3  ) % 3) == 0)
#define SURFACE_IS_PAINTING_WARP_MIDDLE(cmd)    ((((cmd) - SURFACE_PAINTING_WARP_D4  ) % 3) == 0)
#define SURFACE_IS_PAINTING_WARP_RIGHT(cmd)     ((((cmd) - SURFACE_PAINTING_WARP_D5  ) % 3) == 0)
#define SURFACE_IS_INSTANT_WARP(cmd)            (((cmd) >= SURFACE_INSTANT_WARP_1B) && ((cmd) < SURFACE_INSTANT_WARP_1B + INSTANT_WARP_INDEX_STOP))
#define SURFACE_IS_WARP(cmd)                    (((cmd) == SURFACE_LOOK_UP_WARP) || ((cmd) == SURFACE_WOBBLING_WARP) || SURFACE_IS_PAINTING_WARP(cmd) || SURFACE_IS_INSTANT_WARP(cmd))
#define SURFACE_IS_UNSAFE(cmd)                  (((cmd) == SURFACE_BURNING) || SURFACE_IS_QUICKSAND(cmd) || SURFACE_IS_WARP(cmd))

enum SurfaceClass {
    SURFACE_CLASS_DEFAULT,
    SURFACE_CLASS_VERY_SLIPPERY,
    SURFACE_CLASS_SLIPPERY,
    SURFACE_CLASS_NOT_SLIPPERY
};

enum SurfaceFlags {
    SURFACE_FLAGS_NONE            = (0 << 0), // 0x0000
    SURFACE_FLAG_DYNAMIC          = (1 << 0), // 0x0001
    SURFACE_FLAG_NO_CAM_COLLISION = (1 << 1), // 0x0002
};

// These are effectively unique "surface" types like those defined higher
// And they are used as collision commands to load certain functions
enum TerrainLoadCmd {
    TERRAIN_LOAD_VERTICES = 0x40, // Begins vertices list for collision triangles
    TERRAIN_LOAD_CONTINUE,        // Stop loading vertices but continues to load other collision commands
    TERRAIN_LOAD_END,             // End the collision list
    TERRAIN_LOAD_ENVIRONMENT      // Loads water/HMC gas
};

#define TERRAIN_LOAD_IS_SURFACE_TYPE_LOW(cmd)  (cmd <  0x40)
#define TERRAIN_LOAD_IS_SURFACE_TYPE_HIGH(cmd) (cmd >= 0x65)

// Terrain types defined by the level script command terrain_type (cmd_31)
enum TerrainType {
    TERRAIN_GRASS,
    TERRAIN_STONE,
    TERRAIN_SNOW,
    TERRAIN_SAND,
    TERRAIN_SPOOKY,
    TERRAIN_WATER,
    TERRAIN_SLIDE,
    TERRAIN_MASK = 0x7
};

// These collision commands are unique "surface" types like those defined higher

// Collision Data Routine Initiate
#define COL_INIT() TERRAIN_LOAD_VERTICES

// Collision Vertices Read Initiate
#define COL_VERTEX_INIT(vtxNum) vtxNum

// Collision Vertex
#define COL_VERTEX(x, y, z) x, y, z

// Collision Tris Initiate
#define COL_TRI_INIT(surfType, triNum) surfType, triNum

// Collision Tri
#ifdef ALL_SURFACES_HAVE_FORCE
#define COL_TRI(v1, v2, v3) v1, v2, v3, 0
#else
#define COL_TRI(v1, v2, v3) v1, v2, v3
#endif

// Collision Tri With Special Params
#define COL_TRI_SPECIAL(v1, v2, v3, param) v1, v2, v3, param

// Collision Tris Stop Loading
#define COL_TRI_STOP() TERRAIN_LOAD_CONTINUE

// End Collision Data
#define COL_END() TERRAIN_LOAD_END

// Water Boxes Initiate
#define COL_WATER_BOX_INIT(num) TERRAIN_LOAD_ENVIRONMENT, num

// Water Box
#define COL_WATER_BOX(id, x1, z1, x2, z2, y) id, x1, z1, x2, z2, y

// Trajectories
#define TRAJECTORY_POS(trajId, x, y, z) \
    trajId, x, y, z

#define TRAJECTORY_END() \
    -1

#endif // SURFACE_TERRAINS_H
