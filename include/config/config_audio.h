#pragma once

/******************
 * AUDIO SETTINGS *
 ******************/

// Fixes the castle music sometimes triggering after getting a dialog.
#define CASTLE_MUSIC_FIX

// Increases the audio heap size to allow for more concurrent notes to be played and for more custom sequences/banks to be imported (not supported for SH).
#define EXPAND_AUDIO_HEAP

// Uses a much better implementation of reverb over vanilla's fake echo reverb. Great for caves or eerie levels, as well as just a better audio experience in general.
// Reverb parameters can be configured in audio/synthesis.c to meet desired aesthetic/performance needs. Currently US/JP only. Hurts emulator and console performance.
// #define BETTER_REVERB
