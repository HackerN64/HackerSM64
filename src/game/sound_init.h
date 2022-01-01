#ifndef SOUND_INIT_H
#define SOUND_INIT_H

#include <PR/ultratypes.h>

#include "macros.h"

enum SoundMenuFlags {
    SOUND_MENU_FLAG_HANDAPPEAR      = (1 << 0), // 0x001
    SOUND_MENU_FLAG_HANDISAPPEAR    = (1 << 1), // 0x002
    SOUND_MENU_FLAG_UNKNOWN1        = (1 << 2), // 0x004
    SOUND_MENU_FLAG_PINCHMARIOFACE  = (1 << 3), // 0x008
    SOUND_MENU_FLAG_PINCHMARIOFACE2 = (1 << 4), // 0x010
    SOUND_MENU_FLAG_LETGOMARIOFACE  = (1 << 5), // 0x020
    SOUND_MENU_FLAG_CAMERAZOOMIN    = (1 << 6), // 0x040
    SOUND_MENU_FLAG_CAMERAZOOMOUT   = (1 << 7), // 0x080
    SOUND_MENU_FLAG_EXTRA           = (1 << 8), // 0x100
};

enum SoundMenuMode {
    SOUND_MENU_MODE_STEREO,
    SOUND_MENU_MODE_MONO,
    SOUND_MENU_MODE_HEADSET
};

void reset_volume(void);
void raise_background_noise(s32 a);
void lower_background_noise(s32 a);
void disable_background_sound(void);
void enable_background_sound(void);
void set_sound_mode(u16 soundMode);
void play_menu_sounds(s16 soundMenuFlags);
void play_painting_eject_sound(void);
void play_infinite_stairs_music(void);
void set_background_music(u16 a, u16 seqArgs, s16 fadeTimer);
void fadeout_music(s16 fadeOutTime);
void fadeout_level_music(s16 fadeTimer);
void play_cutscene_music(u16 seqArgs);
void play_shell_music(void);
void stop_shell_music(void);
void play_cap_music(u16 seqArgs);
void fadeout_cap_music(void);
void stop_cap_music(void);
void audio_game_loop_tick(void);
void thread4_sound(UNUSED void *arg);

#endif // SOUND_INIT_H
