#ifndef LEVEL_UPDATE_H
#define LEVEL_UPDATE_H

#include <PR/ultratypes.h>

#include "types.h"

enum TimerControl {
    TIMER_CONTROL_SHOW,
    TIMER_CONTROL_START,
    TIMER_CONTROL_STOP,
    TIMER_CONTROL_HIDE
};

enum WarpOperation {
    WARP_OP_NONE,
    WARP_OP_LOOK_UP,
    WARP_OP_SPIN_SHRINK,
    WARP_OP_WARP_DOOR,
    WARP_OP_WARP_OBJECT,
    WARP_OP_TELEPORT,
    WARP_OP_TRIGGERS_LEVEL_SELECT = 0x10,
    WARP_OP_STAR_EXIT,
    WARP_OP_DEATH,
    WARP_OP_WARP_FLOOR,
    WARP_OP_GAME_OVER,
    WARP_OP_CREDITS_END,
    WARP_OP_DEMO_NEXT,
    WARP_OP_CREDITS_START,
    WARP_OP_CREDITS_NEXT,
    WARP_OP_DEMO_END
};

enum SpecialWarpDestinations {
    WARP_SPECIAL_LEVEL_SELECT        = -9,
    WARP_SPECIAL_INTRO_SPLASH_SCREEN = -8,
    WARP_SPECIAL_MARIO_HEAD_DIZZY    = -3,
    WARP_SPECIAL_MARIO_HEAD_REGULAR  = -2,
    WARP_SPECIAL_ENDING              = -1,
    WARP_SPECIAL_NONE                =  0,
};

enum WarpDoorFlags {
    WARP_FLAGS_NONE           = (0 << 0), // 0x00
    WARP_FLAG_DOOR_PULLED     = (1 << 0), // 0x01
    WARP_FLAG_DOOR_FLIP_MARIO = (1 << 1), // 0x02
    WARP_FLAG_DOOR_IS_WARP    = (1 << 2), // 0x04
};

enum MarioSpawnType {
    MARIO_SPAWN_NONE,
    MARIO_SPAWN_DOOR_WARP,
    MARIO_SPAWN_IDLE,
    MARIO_SPAWN_PIPE,
    MARIO_SPAWN_TELEPORT,
    MARIO_SPAWN_INSTANT_ACTIVE = 0x10,
    MARIO_SPAWN_SWIMMING,
    MARIO_SPAWN_AIRBORNE,
    MARIO_SPAWN_HARD_AIR_KNOCKBACK,
    MARIO_SPAWN_SPIN_AIRBORNE_CIRCLE,
    MARIO_SPAWN_DEATH,
    MARIO_SPAWN_SPIN_AIRBORNE,
    MARIO_SPAWN_FLYING,
    MARIO_SPAWN_PAINTING_STAR_COLLECT = 0x20,
    MARIO_SPAWN_PAINTING_DEATH,
    MARIO_SPAWN_AIRBORNE_STAR_COLLECT,
    MARIO_SPAWN_AIRBORNE_DEATH,
    MARIO_SPAWN_LAUNCH_STAR_COLLECT,
    MARIO_SPAWN_LAUNCH_DEATH,
    MARIO_SPAWN_UNUSED_38,
    MARIO_SPAWN_FADE_FROM_BLACK
};

struct CreditsEntry {
    /*0x00*/ u8 levelNum;
    /*0x01*/ u8 areaIndex;
    /*0x02*/ u8 actNum;
    /*0x03*/ s8 marioAngle;
    /*0x04*/ Vec3s marioPos;
    /*0x0C*/ const char **string;
};

extern struct CreditsEntry *gCurrCreditsEntry;

extern struct MarioState gMarioStates[];
extern struct MarioState *gMarioState;

extern s16 sCurrPlayMode;
extern s16 sTransitionTimer;
extern void (*sTransitionUpdate)(s16 *);
extern void load_language_text(void);

struct WarpDest {
    u8 type;
    u8 levelNum;
    u8 areaIdx;
    u8 nodeId;
    u32 arg;
};

extern struct WarpDest sWarpDest;

extern s16 sSpecialWarpDest;
extern s16 sDelayedWarpOp;
extern s16 sDelayedWarpTimer;
extern s16 sSourceWarpNodeId;
extern s32 sDelayedWarpArg;
extern s8 sTimerRunning;

struct HudDisplay {
    /*0x00*/ s16 lives;
    /*0x02*/ s16 coins;
    /*0x04*/ s16 stars;
    /*0x06*/ s16 wedges;
    /*0x08*/ s16 keys;
    /*0x0A*/ s16 flags;
    /*0x0C*/ u16 timer;
#ifdef BREATH_METER
             u16 breath;
#endif
};

extern struct HudDisplay gHudDisplay;
extern s8 gNeverEnteredCastle;

enum HUDDisplayFlag {
    HUD_DISPLAY_FLAG_LIVES            = (1 <<  0), // 0x0001
    HUD_DISPLAY_FLAG_COIN_COUNT       = (1 <<  1), // 0x0002
    HUD_DISPLAY_FLAG_STAR_COUNT       = (1 <<  2), // 0x0004
    HUD_DISPLAY_FLAG_CAMERA_AND_POWER = (1 <<  3), // 0x0008
    HUD_DISPLAY_FLAG_KEYS             = (1 <<  4), // 0x0010
    HUD_DISPLAY_FLAG_UNKNOWN_0020     = (1 <<  5), // 0x0020
    HUD_DISPLAY_FLAG_TIMER            = (1 <<  6), // 0x0040
#ifdef BREATH_METER
    HUD_DISPLAY_FLAG_BREATH_METER     = (1 << 14), // 0x4000
#endif
    HUD_DISPLAY_FLAG_EMPHASIZE_POWER  = (1 << 15), // 0x8000

    HUD_DISPLAY_NONE                  = (0 <<  0), // 0x0000
    HUD_DISPLAY_DEFAULT = (HUD_DISPLAY_FLAG_LIVES | HUD_DISPLAY_FLAG_COIN_COUNT | HUD_DISPLAY_FLAG_STAR_COUNT | HUD_DISPLAY_FLAG_CAMERA_AND_POWER | HUD_DISPLAY_FLAG_KEYS | HUD_DISPLAY_FLAG_UNKNOWN_0020)
};

enum PlayModes {
    PLAY_MODE_NORMAL,
    PLAY_MODE_UNUSED,
    PLAY_MODE_PAUSED,
    PLAY_MODE_CHANGE_AREA,
    PLAY_MODE_CHANGE_LEVEL,
    PLAY_MODE_FRAME_ADVANCE
};

enum WarpTypes {
    WARP_TYPE_NOT_WARPING,
    WARP_TYPE_CHANGE_LEVEL,
    WARP_TYPE_CHANGE_AREA,
    WARP_TYPE_SAME_AREA
};

enum WarpNodes {
    WARP_NODE_MAIN_ENTRY    = 0x0A,
    WARP_NODE_DEFAULT       = 0xF0,
    WARP_NODE_DEATH         = 0xF1,
    WARP_NODE_LOOK_UP       = 0xF2,
    WARP_NODE_WARP_FLOOR    = 0xF3,
    WARP_NODE_CREDITS_MIN   = 0xF8,
    WARP_NODE_CREDITS_START = 0xF8,
    WARP_NODE_CREDITS_NEXT  = 0xF9,
    WARP_NODE_CREDITS_END   = 0xFA
};

u16 level_control_timer(s32 timerOp);
void fade_into_special_warp(u32 arg, u32 color);
void load_level_init_text(u32 arg);
s16 level_trigger_warp(struct MarioState *m, s32 warpOp);
void level_set_transition(s16 length, void (*updateFunction)());

s32 lvl_init_or_update(                  s16 initOrUpdate, UNUSED s32 levelNum);
s32 lvl_init_from_save_file(      UNUSED s16 initOrUpdate,        s32 levelNum);
s32 lvl_set_current_level(        UNUSED s16 initOrUpdate,        s32 levelNum);
s32 lvl_play_the_end_screen_sound(UNUSED s16 initOrUpdate, UNUSED s32 levelNum);
void basic_update(void);

#endif // LEVEL_UPDATE_H
