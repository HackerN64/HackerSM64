#pragma once

/******************
 * AUDIO SETTINGS *
 ******************/

/**
 * Fixes the castle music sometimes triggering after getting a dialog.
 */
#define CASTLE_MUSIC_FIX

/**
 * Increase audio heap size to allow for larger/more custom sequences/banks/sfx to be imported without causing issues (not supported for SH).
 */
#define EXPAND_AUDIO_HEAP

/**
 * The maximum number of notes (sfx inclusive) that can sound at any given time (not supported for SH).
 * Lower values may cause notes to get cut more easily but can potentially improve performance slightly.
 * Lower values may cause problems with streamed audio if a sequence used for it is missing channel priority data.
 * Vanilla by default only generally allocates 16 or 20 notes at once. Memory usage is always determined by the largest of the two values here (~7200 bytes per US/JP note).
 */
#define MAX_SIMULTANEOUS_NOTES_EMULATOR 40
#define MAX_SIMULTANEOUS_NOTES_CONSOLE 24

/**
 * Use a much better implementation of reverb over vanilla's fake echo reverb. Great for caves or eerie levels, as well as just a better audio experience in general.
 * Reverb parameters can be configured in audio/synthesis.c to meet desired aesthetic/performance needs. Currently US/JP only. Hurts emulator and console performance.
 */
// #define BETTER_REVERB
