#ifndef AUDIO_SYNTHESIS_H
#define AUDIO_SYNTHESIS_H

#include "internal.h"

#ifdef VERSION_SH
#define DEFAULT_LEN_1CH 0x180
#define DEFAULT_LEN_2CH 0x300
#else
#define DEFAULT_LEN_1CH 0x140
#define DEFAULT_LEN_2CH 0x280
#endif

#if defined(VERSION_EU) || defined(VERSION_SH)
#define MAX_UPDATES_PER_FRAME 5
#else
#define MAX_UPDATES_PER_FRAME 4
#endif

enum ChannelIndexes {
    SYNTH_CHANNEL_LEFT,
    SYNTH_CHANNEL_RIGHT,
    SYNTH_CHANNEL_STEREO_COUNT,
};

#ifdef BETTER_REVERB

#define REVERB_WINDOW_SIZE_MAX 0x2000


/* ------------ BETTER REVERB GENERAL PARAMETERS ------------ */

#define NUM_ALLPASS 12 // Maximum number of delay filters to use with better reverb; do not change this value if you don't know what you're doing.
#define BETTER_REVERB_PTR_SIZE ALIGN16(NUM_ALLPASS * sizeof(s16*) * SYNTH_CHANNEL_STEREO_COUNT) // Allocation space consumed by dynamically allocated pointers

// Minimum size requirement determined by ((all delaysL and delaysR values) / (2 ^ (downsampleRate - 1)) * sizeof(s16) + BETTER_REVERB_PTR_SIZE).
// The default value can be increased or decreased in conjunction with the values in delaysL/R.
// This can be significantly decreased if a downsample rate of 1 is not being used or if filter count is less than NUM_ALLPASS,
// as this default is configured to handle the emulator RCVI settings.
#define BETTER_REVERB_SIZE ALIGN16(0xEDE0 + BETTER_REVERB_PTR_SIZE)


/* ------ BETTER REVERB LIGHTWEIGHT PARAMETER OVERRIDES ------ */

// Filter count works differently than normal when used with light settings and can support numbers that are not multiples of 3.
// A value of 2 is generally recommended for most similar behavior to non-lightweight reverb.
// This can be reduced to 1 to save additional runtime overhead, but will reduce some reverb saturation as consequence.
// Similarly this can be increased from 2, but likely won't have beneficial outcomes worth the runtime expense compared to the modification of other parameters without using light settings.
#define BETTER_REVERB_FILTER_COUNT_LIGHT 2
#define BETTER_REVERB_GAIN_INDEX_LIGHT 0xA0 // Advanced parameter; used to tune the outputs of every filter except for the final one (multiples of 0x10 will compile more efficiently)
#define BETTER_REVERB_REVERB_INDEX_LIGHT 0x30 // Advanced parameter; used to tune the reuse of the previously processed output sample (multiples of 0x10 will compile more efficiently)


/* ------------ BETTER REVERB EXTERNED VARIABLES ------------ */

extern u8 toggleBetterReverb;
extern u8 gBetterReverbPresetValue;
extern u8 activeBetterReverbPreset;
extern u8 betterReverbLightweight;
extern s8 betterReverbDownsampleRate;
extern u8 monoReverb;
extern s32 reverbFilterCount;
extern s32 betterReverbWindowsSize;
extern s32 betterReverbRevIndex;
extern s32 betterReverbGainIndex;
extern u8 *gReverbMults[SYNTH_CHANNEL_STEREO_COUNT];


/* ------------ BETTER REVERB EXTERNED FUNCTIONS ------------ */

void initialize_better_reverb_buffers(void);
void set_better_reverb_buffers(u32 *inputDelaysL, u32 *inputDelaysR);


/* -------------- BETTER REVERB STATIC ASSERTS -------------- */

STATIC_ASSERT(NUM_ALLPASS % 3 == 0, "NUM_ALLPASS must be a multiple of 3!");
STATIC_ASSERT(BETTER_REVERB_FILTER_COUNT_LIGHT > 0, "BETTER_REVERB_FILTER_COUNT_LIGHT must be greater than 0!");
STATIC_ASSERT(BETTER_REVERB_FILTER_COUNT_LIGHT <= NUM_ALLPASS, "BETTER_REVERB_FILTER_COUNT_LIGHT cannot be larger than NUM_ALLPASS!");

#else

#define BETTER_REVERB_SIZE 0

#ifdef VERSION_EU
#define REVERB_WINDOW_SIZE_MAX 0x1000
#else
#define REVERB_WINDOW_SIZE_MAX 0x1000
#endif

#endif

#if defined(VERSION_JP) || defined(VERSION_US)
#define REVERB_WINDOW_HEAP_SIZE \
( \
    (REVERB_WINDOW_SIZE_MAX * sizeof(s16) * 2) \
    + (4 * (16 * sizeof(s16))) \
    + (4 /* gAudioUpdatesPerFrame */ * (2 * DEFAULT_LEN_2CH)) \
)
#else
#define REVERB_WINDOW_HEAP_SIZE \
( \
    ((REVERB_WINDOW_SIZE_MAX * sizeof(s16) * 2) \
    + (4 * (16 * sizeof(s16))) \
    + (4 /* gAudioUpdatesPerFrame */ * (2 * DEFAULT_LEN_2CH))) \
    * 4 /* gNumSynthesisReverbs */ \
)
#endif

struct ReverbRingBufferItem {
    s16 numSamplesAfterDownsampling;
    s16 chunkLen; // never read
    s16 *toDownsampleLeft;
    s16 *toDownsampleRight; // data pointed to by left and right are adjacent in memory
    s32 startPos; // start pos in ring buffer
    s16 lengthA; // first length in ring buffer (from startPos, at most until end)
    s16 lengthB; // second length in ring buffer (from pos 0)
}; // size = 0x14

struct SynthesisReverb {
    /*0x00, 0x00, 0x00*/ u8 resampleFlags;
    /*0x01, 0x01, 0x01*/ u8 useReverb;
    /*0x02, 0x02, 0x02*/ u8 framesLeftToIgnore;
    /*0x03, 0x03, 0x03*/ u8 curFrame;
#if defined(VERSION_EU) || defined(VERSION_SH)
    /*      0x04, 0x04*/ u8 downsampleRate;
#ifdef VERSION_SH
    /*            0x05*/ s8 unk5;
#endif
    /*      0x06, 0x06*/ u16 windowSize; // same as bufSizePerChannel
#endif
#ifdef VERSION_SH
    /*            0x08*/ u16 unk08;
#endif
    /*0x04, 0x08, 0x0A*/ u16 reverbGain;
    /*0x06, 0x0A, 0x0C*/ u16 resampleRate;
#ifdef VERSION_SH
    /*            0x0E*/ u16 panRight;
    /*            0x10*/ u16 panLeft;
#endif
    /*0x08, 0x0C, 0x14*/ s32 nextRingBufferPos;
    /*0x0C, 0x10, 0x18*/ s32 unkC; // never read
    /*0x10, 0x14, 0x1C*/ s32 bufSizePerChannel;
    struct {
        s16 *left;
        s16 *right;
    } ringBuffer;
    /*0x1C, 0x20, 0x28*/ s16 *resampleStateLeft;
    /*0x20, 0x24, 0x2C*/ s16 *resampleStateRight;
    /*0x24, 0x28, 0x30*/ s16 *unk24; // never read
    /*0x28, 0x2C, 0x34*/ s16 *unk28; // never read
    /*0x2C, 0x30, 0x38*/ struct ReverbRingBufferItem items[2][MAX_UPDATES_PER_FRAME];
#if defined(VERSION_EU) || defined(VERSION_SH)
    // Only used in sh:
    /*            0x100*/ s16 *unk100;
    /*            0x104*/ s16 *unk104;
    /*            0x108*/ s16 *unk108;
    /*            0x10C*/ s16 *unk10C;
#endif
}; // 0xCC <= size <= 0x100
#if defined(VERSION_EU) || defined(VERSION_SH)
extern struct SynthesisReverb gSynthesisReverbs[4];
extern s8 gNumSynthesisReverbs;
extern struct NoteSubEu *gNoteSubsEu;
extern f32 gLeftVolRampings[3][1024];
extern f32 gRightVolRampings[3][1024];
extern f32 *gCurrentLeftVolRamping; // Points to any of the three left buffers above
extern f32 *gCurrentRightVolRamping; // Points to any of the three right buffers above
#else
extern struct SynthesisReverb gSynthesisReverb;
#endif

#ifdef VERSION_SH
extern s16 D_SH_803479B4;
#endif

u64 *synthesis_execute(u64 *cmdBuf, s32 *writtenCmds, s16 *aiBuf, s32 bufLen);
#if defined(VERSION_JP) || defined(VERSION_US)
void note_init_volume(struct Note *note);
void note_set_vel_pan_reverb(struct Note *note, f32 velocity, f32 pan, u8 reverbVol);
void note_set_frequency(struct Note *note, f32 frequency);
void note_enable(struct Note *note);
void note_disable(struct Note *note);
#endif

#endif // AUDIO_SYNTHESIS_H
