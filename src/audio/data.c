#include <ultra64.h>

#include "data.h"
#include "effects.h"

extern struct OSMesgQueue OSMesgQueue0;
extern struct OSMesgQueue OSMesgQueue1;
extern struct OSMesgQueue OSMesgQueue2;
extern struct OSMesgQueue OSMesgQueue3;

// Since the audio session is just one now, the reverb settings are duplicated to match the original audio setting scenario.
// It's a bit hacky but whatever lol. Index range must be defined, since it's needed by the compiler.
// To increase reverb window sizes beyond 64, please increase the REVERB_WINDOW_SIZE_MAX in heap.c by a factor of 0x40.
#ifdef VERSION_EU
struct ReverbSettingsEU sReverbSettings[8] = {
    { /*Downsample Rate*/ 1, /*Window Size*/ 64, /*Gain*/ 0x2FFF },
    { /*Downsample Rate*/ 1, /*Window Size*/ 40, /*Gain*/ 0x47FF },
    { /*Downsample Rate*/ 1, /*Window Size*/ 64, /*Gain*/ 0x2FFF },
    { /*Downsample Rate*/ 1, /*Window Size*/ 60, /*Gain*/ 0x3FFF },
    { /*Downsample Rate*/ 1, /*Window Size*/ 48, /*Gain*/ 0x4FFF },
    { /*Downsample Rate*/ 1, /*Window Size*/ 64, /*Gain*/ 0x2FFF }, // Duplicate of the first index
    { /*Downsample Rate*/ 1, /*Window Size*/ 40, /*Gain*/ 0x47FF }, // Duplicate of the second index
    { /*Downsample Rate*/ 1, /*Window Size*/ 40, /*Gain*/ 0x37FF },
};
/**
1: Frequency
2: Unk1 - Should be 1
3: Simultaneous Notes
4: Number of Reverberations
5: Unk2 - Should be 0
6: Volume
7: Unk3 - Should be 0
8: Persistent Sequence Memory
9: Persistent Bank Memory
10: Temporary Sequence Memory
11: Temporary Bank Memory
*/

struct AudioSessionSettingsEU gAudioSessionPresets[] = {
    { /*1*/ 32000,/*2*/ 1,/*3*/ MAX_SIMULTANEOUS_NOTES,/*4*/ 1,/*5*/ 0, &sReverbSettings[0],/*6*/ 0x7FFF,/*7*/ 0,/*8*/ PERSISTENT_SEQ_MEM,/*9*/ PERSISTENT_BANK_MEM,/*10*/ TEMPORARY_SEQ_MEM,/*11*/ TEMPORARY_BANK_MEM },
};
#endif

#ifdef BETTER_REVERB
// Each entry represents an array of variable audio buffer sizes / delays for each respective filter.
u32 delaysArr[][NUM_ALLPASS] = {
    { /* 0 */ 
        4, 4, 4,
        4, 4, 4,
        4, 4, 4,
        4, 4, 4,
    },
    { /* 1 */ 
        1080, 1352, 1200,
        1200, 1232, 1432,
        1384, 1048, 1352,
         928, 1504, 1512,
    },
    { /* 2 */ 
        1384, 1352, 1048,
         928, 1512, 1504,
        1080, 1200, 1352,
        1200, 1432, 1232,
    },
};

// Each entry represents an array of multipliers applied to the final output of each group of 3 filters.
u8 reverbMultsArr[][NUM_ALLPASS / 3] = {
    /* 0 */ {0x00, 0x00, 0x00, 0x00},
    /* 1 */ {0xD7, 0x6F, 0x36, 0x22},
    /* 2 */ {0xCF, 0x73, 0x38, 0x1F},
};

/**
 * Format:
 * - useLightweightSettings (Reduce some runtime configurability options in favor of a slight speed boost during processing; Light configurability settings are found in synthesis.h)
 * - downsampleRate         (Higher values exponentially reduce the number of input samples to process, improving perfomance at cost of quality)
 * - isMono                 (Only process reverb on the left channel and share it with the right channel, improving performance at cost of quality)
 * - filterCount            (Number of filters to process data with; in general, more filters means higher quality at the cost of performance demand; always 3 with light settings)
 * 
 * - windowSize             (Size of circular reverb buffer; higher values work better for a more open soundscape, lower is better for a more compact sound)
 * - gain                   (Amount of audio retransmitted into the circular reverb buffer, emulating decay; higher values represent a lengthier decay period)
 * - gainIndex              (Advanced parameter; used to tune the outputs of every first two of three filters; overridden when using light settings)
 * - reverbIndex            (Advanced parameter; used to tune the incoming output of every third filter; overridden when using light settings)
 * 
 * - *delaysL               (Advanced parameter; array of variable audio buffer sizes / delays for each respective filter [left channel]; overridden when using light settings)
 * - *delaysR               (Advanced parameter; array of variable audio buffer sizes / delays for each respective filter [right channel]; overridden when using light settings)
 * - *reverbMultsL          (Advanced parameter; array of multipliers applied to the final output of each group of 3 filters [left channel])
 * - *reverbMultsR          (Advanced parameter; array of multipliers applied to the final output of each group of 3 filters [right channel])
 * 
 * NOTE: The first entry will always be used by default when not using the level commands to specify a preset.
 * Please reference the HackerSM64 Wiki for more descriptive documentation of these parameters and usage of BETTER_REVERB in general.
 */
struct BetterReverbSettings gBetterReverbSettings[] = {
    { /* Preset 0 - Vanilla Reverb [Default Preset] */
        .useLightweightSettings = FALSE,   // Ignored with vanilla reverb
        .downsampleRate = -1,              // Signifies use of vanilla reverb
        .isMono = FALSE,                   // Ignored with vanilla reverb
        .filterCount = NUM_ALLPASS,        // Ignored with vanilla reverb

        .windowSize = -1,                  // Use vanilla preset window size
        .gain = -1,                        // Use vanilla preset gain value
        .gainIndex = 0x00,                 // Ignored with vanilla reverb
        .reverbIndex = 0x00,               // Ignored with vanilla reverb

        .delaysL = delaysArr[0],           // Ignored with vanilla reverb
        .delaysR = delaysArr[0],           // Ignored with vanilla reverb
        .reverbMultsL = reverbMultsArr[0], // Ignored with vanilla reverb
        .reverbMultsR = reverbMultsArr[0], // Ignored with vanilla reverb
    },
    { /* Preset 1 - Sample Console Configuration */
        .useLightweightSettings = TRUE,
        .downsampleRate = 2,
        .isMono = FALSE,
        .filterCount = (NUM_ALLPASS - 9),  // Ignored with lightweight settings

        .windowSize = 0x0E00,
        .gain = 0x43FF,
        .gainIndex = 0xA0,                 // Ignored with lightweight settings
        .reverbIndex = 0x30,               // Ignored with lightweight settings

        .delaysL = delaysArr[1],
        .delaysR = delaysArr[2],
        .reverbMultsL = reverbMultsArr[1], // Ignored with lightweight settings
        .reverbMultsR = reverbMultsArr[2], // Ignored with lightweight settings
    },
    { /* Preset 2 - Sample Emulator Configuration (RCVI Hack or Emulator CPU Overclocking Required!) */
        .useLightweightSettings = FALSE,
        .downsampleRate = 1,
        .isMono = FALSE,
        .filterCount = NUM_ALLPASS,

        .windowSize = 0x0E00,
        .gain = 0x28FF,
        .gainIndex = 0xA0,
        .reverbIndex = 0x60,

        .delaysL = delaysArr[1],
        .delaysR = delaysArr[2],
        .reverbMultsL = reverbMultsArr[1],
        .reverbMultsR = reverbMultsArr[2],
    },
};
#endif

// Format:
// - frequency
// - max number of simultaneous notes
// - reverb downsample rate (makes the ring buffer be downsampled to save memory)
// - reverb window size (ring buffer size, length affects reverb delay)
// - reverb gain (0 = min reverb, 32767 = max reverb, 32769 to 65535 = louder and louder...)
// - volume
// - memory used for persistent sequences
// - memory used for persistent banks
// - memory used for temporary sequences
// - memory used for temporary banks

// To increase reverb window sizes beyond 0x1000, please increase the REVERB_WINDOW_SIZE_MAX in heap.c.
#if defined(VERSION_JP) || defined(VERSION_US)
struct ReverbSettingsUS gReverbSettings[18] = {
    { 1, 0x0C00, 0x2FFF },
    { 1, 0x0A00, 0x47FF },
    { 1, 0x1000, 0x2FFF },
    { 1, 0x0E00, 0x3FFF },
    { 1, 0x0C00, 0x4FFF },
    { 1, 0x0C00, 0x2FFF },
    { 1, 0x0A00, 0x47FF },
    { 1, 0x0800, 0x37FF },
    { 1, 0x0800, 0x2FFF },
    { 1, 0x0800, 0x3FFF },
    { 1, 0x1000, 0x3FFF },
    { 1, 0x1000, 0x2FFF },
    { 1, 0x0C00, 0x3FFF },
    { 1, 0x0800, 0x4FFF },
    { 1, 0x0800, 0x2FFF },
    { 1, 0x0800, 0x2FFF },
    { 1, 0x0800, 0x2FFF },
    { 1, 0x0800, 0x2FFF },
};

struct AudioSessionSettings gAudioSessionSettings = { 32000, MAX_SIMULTANEOUS_NOTES, 0x7FFF, PERSISTENT_SEQ_MEM, PERSISTENT_BANK_MEM, TEMPORARY_SEQ_MEM, TEMPORARY_BANK_MEM };
#endif

// gAudioCosineTable[k] = round((2**15 - 1) * cos(pi/2 * k / 127)). Unused.
#if defined(VERSION_JP) || defined(VERSION_US)
u16 gAudioCosineTable[128] = {
    0x7FFF, 32764, 32757, 32744, 32727, 32704, 32677, 32644, 32607, 32564, 32517, 32464, 32407,
    32344,  32277, 32205, 32127, 32045, 31958, 31866, 31770, 31668, 31561, 31450, 31334, 31213,
    31087,  30957, 30822, 30682, 30537, 30388, 30234, 30075, 29912, 29744, 29572, 29395, 29214,
    29028,  28838, 28643, 28444, 28241, 28033, 27821, 27605, 27385, 27160, 26931, 26698, 26461,
    26220,  25975, 25726, 25473, 25216, 24956, 24691, 24423, 24151, 23875, 23596, 23313, 23026,
    22736,  22442, 22145, 21845, 21541, 21234, 20924, 20610, 20294, 19974, 19651, 19325, 18997,
    18665,  18331, 17993, 17653, 17310, 16965, 16617, 16266, 15913, 15558, 15200, 14840, 14477,
    14113,  13746, 13377, 13006, 12633, 12258, 11881, 11503, 11122, 10740, 10357,  9971,  9584,
     9196,   8806,  8415,  8023,  7630,  7235,  6839,  6442,  6044,  5646,  5246,  4845,  4444,
     4042,   3640,  3237,  2833,  2429,  2025,  1620,  1216,   810,   405,     0,
};
#endif

// Transforms a pitch scale factor in -127..127 into a frequency scale factor
// between -1 and +1 octave.
// gPitchBendFrequencyScale[k] = (0.5 * 2^(k/127))
#ifndef VERSION_SH
#if defined(VERSION_EU)
f32 gPitchBendFrequencyScale[256] = {
    0.5f,
#else
f32 gPitchBendFrequencyScale[255] = {
#endif
    0.500000f, 0.502736f, 0.505488f, 0.508254f, 0.511036f, 0.513833f, 0.516645f, 0.519472f, 0.522315f,
    0.525174f, 0.528048f, 0.530938f, 0.533843f, 0.536765f, 0.539702f, 0.542656f, 0.545626f, 0.548612f,
    0.551614f, 0.554633f, 0.557669f, 0.560721f, 0.563789f, 0.566875f, 0.569977f, 0.573097f, 0.576233f,
    0.579387f, 0.582558f, 0.585746f, 0.588951f, 0.592175f, 0.595415f, 0.598674f, 0.601950f, 0.605245f,
    0.608557f, 0.611888f, 0.615236f, 0.618603f, 0.621989f, 0.625393f, 0.628815f, 0.632257f, 0.635717f,
    0.639196f, 0.642694f, 0.646212f, 0.649748f, 0.653304f, 0.656880f, 0.660475f, 0.664089f, 0.667724f,
    0.671378f, 0.675052f, 0.678747f, 0.682461f, 0.686196f, 0.689952f, 0.693727f, 0.697524f, 0.701341f,
    0.705180f, 0.709039f, 0.712919f, 0.716821f, 0.720744f, 0.724689f, 0.728655f, 0.732642f, 0.736652f,
    0.740684f, 0.744737f, 0.748813f, 0.752911f, 0.757031f, 0.761175f, 0.765340f, 0.769529f, 0.773740f,
    0.777975f, 0.782232f, 0.786513f, 0.790818f, 0.795146f, 0.799497f, 0.803873f, 0.808272f, 0.812696f,
    0.817144f, 0.821616f, 0.826112f, 0.830633f, 0.835179f, 0.839750f, 0.844346f, 0.848966f, 0.853613f,
    0.858284f, 0.862982f, 0.867704f, 0.872453f, 0.877228f, 0.882029f, 0.886856f, 0.891709f, 0.896590f,
    0.901496f, 0.906430f, 0.911391f, 0.916379f, 0.921394f, 0.926436f, 0.931507f, 0.936604f, 0.941730f,
    0.946884f, 0.952066f, 0.957277f, 0.962516f, 0.967783f, 0.973080f, 0.978405f, 0.983760f, 0.989144f,
    0.994557f, 1.000000f, 1.005473f, 1.010975f, 1.016508f, 1.022071f, 1.027665f, 1.033289f, 1.038944f,
    1.044630f, 1.050347f, 1.056095f, 1.061875f, 1.067687f, 1.073530f, 1.079405f, 1.085312f, 1.091252f,
    1.097224f, 1.103229f, 1.109267f, 1.115337f, 1.121441f, 1.127579f, 1.133750f, 1.139955f, 1.146193f,
    1.152466f, 1.158773f, 1.165115f, 1.171491f, 1.177903f, 1.184349f, 1.190831f, 1.197348f, 1.203901f,
    1.210489f, 1.217114f, 1.223775f, 1.230473f, 1.237207f, 1.243978f, 1.250786f, 1.257631f, 1.264514f,
    1.271434f, 1.278392f, 1.285389f, 1.292423f, 1.299497f, 1.306608f, 1.313759f, 1.320949f, 1.328178f,
    1.335447f, 1.342756f, 1.350104f, 1.357493f, 1.364922f, 1.372392f, 1.379903f, 1.387455f, 1.395048f,
    1.402683f, 1.410360f, 1.418078f, 1.425839f, 1.433642f, 1.441488f, 1.449377f, 1.457309f, 1.465285f,
    1.473304f, 1.481367f, 1.489474f, 1.497626f, 1.505822f, 1.514063f, 1.522349f, 1.530681f, 1.539058f,
    1.547481f, 1.555950f, 1.564465f, 1.573027f, 1.581636f, 1.590292f, 1.598995f, 1.607746f, 1.616545f,
    1.625392f, 1.634287f, 1.643231f, 1.652224f, 1.661266f, 1.670358f, 1.679500f, 1.688691f, 1.697933f,
    1.707225f, 1.716569f, 1.725963f, 1.735409f, 1.744906f, 1.754456f, 1.764058f, 1.773712f, 1.783419f,
    1.793179f, 1.802993f, 1.812860f, 1.822782f, 1.832757f, 1.842788f, 1.852873f, 1.863013f, 1.873209f,
    1.883461f, 1.893768f, 1.904132f, 1.914553f, 1.925031f, 1.935567f, 1.946159f, 1.956810f, 1.967520f,
    1.978287f, 1.989114f, 2.000000f
};

// Frequencies for notes using the standard twelve-tone equal temperament scale.
// For indices 0..116, gNoteFrequencies[k] = 2^((k-39)/12).
// For indices 117..128, gNoteFrequencies[k] = 0.5 * 2^((k-39)/12).
// The 39 in the formula refers to piano key 40 (middle C, at 256 Hz) being
// the reference frequency, which is assigned value 1.
// clang-format off
f32 gNoteFrequencies[128] = {
    0.105112f,  0.111362f,  0.117984f,  0.125f, 0.132433f, 0.140308f,  0.148651f,  0.15749f,  0.166855f, 0.176777f, 0.187288f,  0.198425f,
    0.210224f,  0.222725f,  0.235969f,  0.25f,  0.264866f, 0.280616f,  0.297302f,  0.31498f,  0.33371f,  0.353553f, 0.374577f,  0.39685f,
    0.420448f,  0.445449f,  0.471937f,  0.5f,   0.529732f, 0.561231f,  0.594604f,  0.629961f, 0.66742f,  0.707107f, 0.749154f,  0.793701f,
    0.840897f,  0.890899f,  0.943875f,  1.0f,   1.059463f, 1.122462f,  1.189207f,  1.259921f, 1.33484f,  1.414214f, 1.498307f,  1.587401f,
    1.681793f,  1.781798f,  1.887749f,  2.0f,   2.118926f, 2.244924f,  2.378414f,  2.519842f, 2.66968f,  2.828428f, 2.996615f,  3.174803f,
    3.363586f,  3.563596f,  3.775498f,  4.0f,   4.237853f, 4.489849f,  4.756829f,  5.039685f, 5.33936f,  5.656855f, 5.993229f,  6.349606f,
    6.727173f,  7.127192f,  7.550996f,  8.0f,   8.475705f, 8.979697f,  9.513658f,  10.07937f, 10.67872f, 11.31371f, 11.986459f, 12.699211f,
    13.454346f, 14.254383f, 15.101993f, 16.0f,  16.95141f, 17.959394f, 19.027315f, 20.15874f, 21.35744f, 22.62742f, 23.972918f, 25.398422f,
    26.908691f, 28.508766f, 30.203985f, 32.0f,  33.90282f, 35.91879f,  38.05463f,  40.31748f, 42.71488f, 45.25484f, 47.945835f, 50.796844f,
    53.817383f, 57.017532f, 60.40797f,  64.0f,  67.80564f, 71.83758f,  76.10926f,  80.63496f, 85.42976f, 45.25484f, 47.945835f, 50.796844f,
    53.817383f, 57.017532f, 60.40797f,  64.0f,  67.80564f, 71.83758f,  76.10926f,  80.63496f
};
// clang-format on

// goes up by ~12 at each step for the first 4 values (starting from 0), then by ~6
u8 gDefaultShortNoteVelocityTable[16] = {
    12, 25, 38, 51, 57, 64, 71, 76, 83, 89, 96, 102, 109, 115, 121, 127,
};

// goes down by 26 at each step for the first 4 values (starting from 255), then by ~12
u8 gDefaultShortNoteDurationTable[16] = {
    229, 203, 177, 151, 139, 126, 113, 100, 87, 74, 61, 48, 36, 23, 10, 0,
};

#if defined(VERSION_JP) || defined(VERSION_US)
// gVibratoCurve[k] = k*8
s8 gVibratoCurve[16] = { 0, 8, 16, 24, 32, 40, 48, 56, 64, 72, 80, 88, 96, 104, 112, 120 };
#endif

struct AdsrEnvelope gDefaultEnvelope[] = {
    { BSWAP16(4), BSWAP16(32000) },    // go from 0 to 32000 over the course of 16ms
    { BSWAP16(1000), BSWAP16(32000) }, // stay there for 4.16 seconds
    { BSWAP16(ADSR_HANG), 0 }          // then continue staying there
};
#endif

#ifdef VERSION_EU
struct NoteSubEu gZeroNoteSub = { 0 };
struct NoteSubEu gDefaultNoteSub = { 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, { NULL } };
#endif

#if defined(VERSION_EU) || defined(VERSION_SH)
s16 sSawtoothWaves[256] = {
    0,       1023,   2047,    3071,   4095,    5119,   6143,    7167,   8191,    9215,   10239,
    11263,   0x2FFF, 13311,   0x37FF, 15359,   0x3FFF, 17407,   0x47FF, 19455,   0x4FFF, 21503,
    22527,   23551,  24575,   25599,  26623,   27647,  28671,   29695,  30719,   31743,  -0x7FFF,
    -31743,  -30719, -29695,  -28671, -27647,  -26623, -25599,  -24575, -23551,  -22527, -21503,
    -0x4FFF, -19455, -0x47FF, -17407, -0x3FFF, -15359, -0x37FF, -13311, -0x2FFF, -11263, -10239,
    -9215,   -8191,  -7167,   -6143,  -5119,   -4095,  -3071,   -2047,  -1023,
    0,      0x7FF,  0xFFF,  0x17FF, 0x1FFF, 0x27FF, 0x2FFF, 0x37FF, 0x3FFF, 0x47FF, 0x4FFF,
    0x57FF, 0x5FFF, 0x67FF, 0x6FFF, 0x77FF, 0x8001, 0x8801, 0x9001, 0x9801, 0xa001, 0xa801,
    0xb001, 0xb801, 0xc001, 0xc801, 0xd001, 0xd801, 0xe001, 0xe801, 0xf001, 0xf801, 0x0000,
    0x07ff, 0x0fff, 0x17ff, 0x1fff, 0x27ff, 0x2fff, 0x37ff, 0x3fff, 0x47ff, 0x4fff, 0x57ff,
    0x5fff, 0x67ff, 0x6fff, 0x77ff, 0x8001, 0x8801, 0x9001, 0x9801, 0xa001, 0xa801, 0xb001,
    0xb801, 0xc001, 0xc801, 0xd001, 0xd801, 0xe001, 0xe801, 0xf001, 0xf801,
    0x0000, 0x0fff, 0x1fff, 0x2fff, 0x3fff, 0x4fff, 0x5fff, 0x6fff,
    0x8001, 0x9001, 0xa001, 0xb001, 0xc001, 0xd001, 0xe001, 0xf001,
    0x0000, 0x0fff, 0x1fff, 0x2fff, 0x3fff, 0x4fff, 0x5fff, 0x6fff,
    0x8001, 0x9001, 0xa001, 0xb001, 0xc001, 0xd001, 0xe001, 0xf001,
    0x0000, 0x0fff, 0x1fff, 0x2fff, 0x3fff, 0x4fff, 0x5fff, 0x6fff,
    0x8001, 0x9001, 0xa001, 0xb001, 0xc001, 0xd001, 0xe001, 0xf001,
    0x0000, 0x0fff, 0x1fff, 0x2fff, 0x3fff, 0x4fff, 0x5fff, 0x6fff,
    0x8001, 0x9001, 0xa001, 0xb001, 0xc001, 0xd001, 0xe001, 0xf001,
    0x0000, 0x1fff, 0x3fff, 0x5fff, 0x8001, 0xa001, 0xc001, 0xe001,
    0x0000, 0x1fff, 0x3fff, 0x5fff, 0x8001, 0xa001, 0xc001, 0xe001,
    0x0000, 0x1fff, 0x3fff, 0x5fff, 0x8001, 0xa001, 0xc001, 0xe001,
    0x0000, 0x1fff, 0x3fff, 0x5fff, 0x8001, 0xa001, 0xc001, 0xe001,
    0x0000, 0x1fff, 0x3fff, 0x5fff, 0x8001, 0xa001, 0xc001, 0xe001,
    0x0000, 0x1fff, 0x3fff, 0x5fff, 0x8001, 0xa001, 0xc001, 0xe001,
    0x0000, 0x1fff, 0x3fff, 0x5fff, 0x8001, 0xa001, 0xc001, 0xe001,
    0x0000, 0x1fff, 0x3fff, 0x5fff, 0x8001, 0xa001, 0xc001, 0xe001
};
s16 sTriangleWaves[256] = {
    0x0000, 0x07ff, 0x0fff, 0x17ff, 0x1fff, 0x27ff, 0x2fff, 0x37ff, 0x3fff, 0x47ff, 0x4fff, 0x57ff,
    0x5fff, 0x67ff, 0x6fff, 0x77ff, 0x7fff, 0x77ff, 0x6fff, 0x67ff, 0x5fff, 0x57ff, 0x4fff, 0x47ff,
    0x3fff, 0x37ff, 0x2fff, 0x27ff, 0x1fff, 0x17ff, 0x0fff, 0x07ff, 0x0000, 0xf801, 0xf001, 0xe801,
    0xe001, 0xd801, 0xd001, 0xc801, 0xc001, 0xb801, 0xb001, 0xa801, 0xa001, 0x9801, 0x9001, 0x8801,
    0x8001, 0x8801, 0x9001, 0x9801, 0xa001, 0xa801, 0xb001, 0xb801, 0xc001, 0xc801, 0xd001, 0xd801,
    0xe001, 0xe801, 0xf001, 0xf801, 0x0000, 0x0fff, 0x1fff, 0x2fff, 0x3fff, 0x4fff, 0x5fff, 0x6fff,
    0x7fff, 0x6fff, 0x5fff, 0x4fff, 0x3fff, 0x2fff, 0x1fff, 0x0fff, 0x0000, 0xf001, 0xe001, 0xd001,
    0xc001, 0xb001, 0xa001, 0x9001, 0x8001, 0x9001, 0xa001, 0xb001, 0xc001, 0xd001, 0xe001, 0xf001,
    0x0000, 0x0fff, 0x1fff, 0x2fff, 0x3fff, 0x4fff, 0x5fff, 0x6fff, 0x7fff, 0x6fff, 0x5fff, 0x4fff,
    0x3fff, 0x2fff, 0x1fff, 0x0fff, 0x0000, 0xf001, 0xe001, 0xd001, 0xc001, 0xb001, 0xa001, 0x9001,
    0x8001, 0x9001, 0xa001, 0xb001, 0xc001, 0xd001, 0xe001, 0xf001, 0x0000, 0x1fff, 0x3fff, 0x5fff,
    0x7fff, 0x5fff, 0x3fff, 0x1fff, 0x0000, 0xe001, 0xc001, 0xa001, 0x8001, 0xa001, 0xc001, 0xe001,
    0x0000, 0x1fff, 0x3fff, 0x5fff, 0x7fff, 0x5fff, 0x3fff, 0x1fff, 0x0000, 0xe001, 0xc001, 0xa001,
    0x8001, 0xa001, 0xc001, 0xe001, 0x0000, 0x1fff, 0x3fff, 0x5fff, 0x7fff, 0x5fff, 0x3fff, 0x1fff,
    0x0000, 0xe001, 0xc001, 0xa001, 0x8001, 0xa001, 0xc001, 0xe001, 0x0000, 0x1fff, 0x3fff, 0x5fff,
    0x7fff, 0x5fff, 0x3fff, 0x1fff, 0x0000, 0xe001, 0xc001, 0xa001, 0x8001, 0xa001, 0xc001, 0xe001,
    0x0000, 0x3fff, 0x7fff, 0x3fff, 0x0000, 0xc001, 0x8001, 0xc001, 0x0000, 0x3fff, 0x7fff, 0x3fff,
    0x0000, 0xc001, 0x8001, 0xc001, 0x0000, 0x3fff, 0x7fff, 0x3fff, 0x0000, 0xc001, 0x8001, 0xc001,
    0x0000, 0x3fff, 0x7fff, 0x3fff, 0x0000, 0xc001, 0x8001, 0xc001, 0x0000, 0x3fff, 0x7fff, 0x3fff,
    0x0000, 0xc001, 0x8001, 0xc001, 0x0000, 0x3fff, 0x7fff, 0x3fff, 0x0000, 0xc001, 0x8001, 0xc001,
    0x0000, 0x3fff, 0x7fff, 0x3fff, 0x0000, 0xc001, 0x8001, 0xc001, 0x0000, 0x3fff, 0x7fff, 0x3fff,
    0x0000, 0xc001, 0x8001, 0xc001,
};
s16 sSineWaves[256] = {
    0x0000, 0x0c8b, 0x18f8, 0x2527, 0x30fb, 0x3c56, 0x471c, 0x5133, 0x5a81, 0x62f1, 0x6a6c, 0x70e1,
    0x7640, 0x7a7c, 0x7d89, 0x7f61, 0x7fff, 0x7f61, 0x7d89, 0x7a7c, 0x7640, 0x70e1, 0x6a6c, 0x62f1,
    0x5a81, 0x5133, 0x471c, 0x3c56, 0x30fb, 0x2527, 0x18f8, 0x0c8b, 0x0000, 0xf375, 0xe708, 0xdad9,
    0xcf05, 0xc3aa, 0xb8e4, 0xaecd, 0xa57f, 0x9d0f, 0x9594, 0x8f1f, 0x89c0, 0x8584, 0x8277, 0x809f,
    0x8001, 0x809f, 0x8277, 0x8584, 0x89c0, 0x8f1f, 0x9594, 0x9d0f, 0xa57f, 0xaecd, 0xb8e4, 0xc3aa,
    0xcf05, 0xdad9, 0xe708, 0xf375, 0x0000, 0x18f8, 0x30fb, 0x471c, 0x5a81, 0x6a6c, 0x7640, 0x7d89,
    0x7fff, 0x7d89, 0x7640, 0x6a6c, 0x5a81, 0x471c, 0x30fb, 0x18f8, 0x0000, 0xe708, 0xcf05, 0xb8e4,
    0xa57f, 0x9594, 0x89c0, 0x8277, 0x8001, 0x8277, 0x89c0, 0x9594, 0xa57f, 0xb8e4, 0xcf05, 0xe708,
    0x0000, 0x18f8, 0x30fb, 0x471c, 0x5a81, 0x6a6c, 0x7640, 0x7d89, 0x7fff, 0x7d89, 0x7640, 0x6a6c,
    0x5a81, 0x471c, 0x30fb, 0x18f8, 0x0000, 0xe708, 0xcf05, 0xb8e4, 0xa57f, 0x9594, 0x89c0, 0x8277,
    0x8001, 0x8277, 0x89c0, 0x9594, 0xa57f, 0xb8e4, 0xcf05, 0xe708, 0x0000, 0x30fb, 0x5a81, 0x7640,
    0x7fff, 0x7640, 0x5a81, 0x30fb, 0x0000, 0xcf05, 0xa57f, 0x89c0, 0x8001, 0x89c0, 0xa57f, 0xcf05,
    0x0000, 0x30fb, 0x5a81, 0x7640, 0x7fff, 0x7640, 0x5a81, 0x30fb, 0x0000, 0xcf05, 0xa57f, 0x89c0,
    0x8001, 0x89c0, 0xa57f, 0xcf05, 0x0000, 0x30fb, 0x5a81, 0x7640, 0x7fff, 0x7640, 0x5a81, 0x30fb,
    0x0000, 0xcf05, 0xa57f, 0x89c0, 0x8001, 0x89c0, 0xa57f, 0xcf05, 0x0000, 0x30fb, 0x5a81, 0x7640,
    0x7fff, 0x7640, 0x5a81, 0x30fb, 0x0000, 0xcf05, 0xa57f, 0x89c0, 0x8001, 0x89c0, 0xa57f, 0xcf05,
    0x0000, 0x5a81, 0x7fff, 0x5a81, 0x0000, 0xa57f, 0x8001, 0xa57f, 0x0000, 0x5a81, 0x7fff, 0x5a81,
    0x0000, 0xa57f, 0x8001, 0xa57f, 0x0000, 0x5a81, 0x7fff, 0x5a81, 0x0000, 0xa57f, 0x8001, 0xa57f,
    0x0000, 0x5a81, 0x7fff, 0x5a81, 0x0000, 0xa57f, 0x8001, 0xa57f, 0x0000, 0x5a81, 0x7fff, 0x5a81,
    0x0000, 0xa57f, 0x8001, 0xa57f, 0x0000, 0x5a81, 0x7fff, 0x5a81, 0x0000, 0xa57f, 0x8001, 0xa57f,
    0x0000, 0x5a81, 0x7fff, 0x5a81, 0x0000, 0xa57f, 0x8001, 0xa57f, 0x0000, 0x5a81, 0x7fff, 0x5a81,
    0x0000, 0xa57f, 0x8001, 0xa57f,
};
s16 sSquareWaves[256] = {
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x7fff, 0x7fff, 0x7fff, 0x7fff, 0x7fff, 0x7fff, 0x7fff, 0x7fff,
    0x7fff, 0x7fff, 0x7fff, 0x7fff, 0x7fff, 0x7fff, 0x7fff, 0x7fff, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x8001, 0x8001, 0x8001, 0x8001, 0x8001, 0x8001, 0x8001, 0x8001, 0x8001, 0x8001, 0x8001, 0x8001,
    0x8001, 0x8001, 0x8001, 0x8001, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x7fff, 0x7fff, 0x7fff, 0x7fff, 0x7fff, 0x7fff, 0x7fff, 0x7fff, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x8001, 0x8001, 0x8001, 0x8001, 0x8001, 0x8001, 0x8001, 0x8001,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x7fff, 0x7fff, 0x7fff, 0x7fff,
    0x7fff, 0x7fff, 0x7fff, 0x7fff, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x8001, 0x8001, 0x8001, 0x8001, 0x8001, 0x8001, 0x8001, 0x8001, 0x0000, 0x0000, 0x0000, 0x0000,
    0x7fff, 0x7fff, 0x7fff, 0x7fff, 0x0000, 0x0000, 0x0000, 0x0000, 0x8001, 0x8001, 0x8001, 0x8001,
    0x0000, 0x0000, 0x0000, 0x0000, 0x7fff, 0x7fff, 0x7fff, 0x7fff, 0x0000, 0x0000, 0x0000, 0x0000,
    0x8001, 0x8001, 0x8001, 0x8001, 0x0000, 0x0000, 0x0000, 0x0000, 0x7fff, 0x7fff, 0x7fff, 0x7fff,
    0x0000, 0x0000, 0x0000, 0x0000, 0x8001, 0x8001, 0x8001, 0x8001, 0x0000, 0x0000, 0x0000, 0x0000,
    0x7fff, 0x7fff, 0x7fff, 0x7fff, 0x0000, 0x0000, 0x0000, 0x0000, 0x8001, 0x8001, 0x8001, 0x8001,
    0x0000, 0x0000, 0x7fff, 0x7fff, 0x0000, 0x0000, 0x8001, 0x8001, 0x0000, 0x0000, 0x7fff, 0x7fff,
    0x0000, 0x0000, 0x8001, 0x8001, 0x0000, 0x0000, 0x7fff, 0x7fff, 0x0000, 0x0000, 0x8001, 0x8001,
    0x0000, 0x0000, 0x7fff, 0x7fff, 0x0000, 0x0000, 0x8001, 0x8001, 0x0000, 0x0000, 0x7fff, 0x7fff,
    0x0000, 0x0000, 0x8001, 0x8001, 0x0000, 0x0000, 0x7fff, 0x7fff, 0x0000, 0x0000, 0x8001, 0x8001,
    0x0000, 0x0000, 0x7fff, 0x7fff, 0x0000, 0x0000, 0x8001, 0x8001, 0x0000, 0x0000, 0x7fff, 0x7fff,
    0x0000, 0x0000, 0x8001, 0x8001,
};
s16 sEuUnknownWave6[256] = {
    0x0000, 0x9ba7, 0x9b41, 0x6c9b, 0x9450, 0xadda, 0x569e, 0x189a, 0x69bf, 0xb79d, 0x6fe9, 0x08ec,
    0x0d34, 0x1aea, 0xce76, 0xad86, 0x2710, 0xa038, 0x7e28, 0x2fd8, 0x3af8, 0x3bfa, 0xd10b, 0x84c7,
    0xcd7f, 0x18f4, 0xd4c8, 0x76f8, 0x8994, 0xaa11, 0x73fb, 0x6c01, 0x0000, 0x93ff, 0x8c05, 0x55ef,
    0x766c, 0x8907, 0x2b38, 0xe70d, 0x3281, 0x7b38, 0x2ef5, 0xc407, 0xc508, 0xd027, 0x81d8, 0x5fc9,
    0xd8f0, 0x5279, 0x318a, 0xe517, 0xf2cc, 0xf713, 0x9017, 0x4864, 0x9641, 0xe765, 0xa962, 0x5227,
    0x6bb0, 0x9364, 0x64bf, 0x645a, 0x0000, 0x9b41, 0x9450, 0x569e, 0x69bf, 0x6fe9, 0x0d34, 0xce76,
    0x2710, 0x7e28, 0x3af8, 0xd10b, 0xcd7f, 0xd4c8, 0x8994, 0x73fb, 0x0000, 0x8c05, 0x766c, 0x2b38,
    0x3281, 0x2ef5, 0xc508, 0x81d8, 0xd8f0, 0x318a, 0xf2cc, 0x9017, 0x9641, 0xa962, 0x6bb0, 0x64bf,
    0x0000, 0x9b41, 0x9450, 0x569e, 0x69bf, 0x6fe9, 0x0d34, 0xce76, 0x2710, 0x7e28, 0x3af8, 0xd10b,
    0xcd7f, 0xd4c8, 0x8994, 0x73fb, 0x0000, 0x8c05, 0x766c, 0x2b38, 0x3281, 0x2ef5, 0xc508, 0x81d8,
    0xd8f0, 0x318a, 0xf2cc, 0x9017, 0x9641, 0xa962, 0x6bb0, 0x64bf, 0x0000, 0x9450, 0x69bf, 0x0d34,
    0x2710, 0x3af8, 0xcd7f, 0x8994, 0x0000, 0x766c, 0x3281, 0xc508, 0xd8f0, 0xf2cc, 0x9641, 0x6bb0,
    0x0000, 0x9450, 0x69bf, 0x0d34, 0x2710, 0x3af8, 0xcd7f, 0x8994, 0x0000, 0x766c, 0x3281, 0xc508,
    0xd8f0, 0xf2cc, 0x9641, 0x6bb0, 0x0000, 0x9450, 0x69bf, 0x0d34, 0x2710, 0x3af8, 0xcd7f, 0x8994,
    0x0000, 0x766c, 0x3281, 0xc508, 0xd8f0, 0xf2cc, 0x9641, 0x6bb0, 0x0000, 0x9450, 0x69bf, 0x0d34,
    0x2710, 0x3af8, 0xcd7f, 0x8994, 0x0000, 0x766c, 0x3281, 0xc508, 0xd8f0, 0xf2cc, 0x9641, 0x6bb0,
    0x0000, 0x69bf, 0x2710, 0xcd7f, 0x0000, 0x3281, 0xd8f0, 0x9641, 0x0000, 0x69bf, 0x2710, 0xcd7f,
    0x0000, 0x3281, 0xd8f0, 0x9641, 0x0000, 0x69bf, 0x2710, 0xcd7f, 0x0000, 0x3281, 0xd8f0, 0x9641,
    0x0000, 0x69bf, 0x2710, 0xcd7f, 0x0000, 0x3281, 0xd8f0, 0x9641, 0x0000, 0x69bf, 0x2710, 0xcd7f,
    0x0000, 0x3281, 0xd8f0, 0x9641, 0x0000, 0x69bf, 0x2710, 0xcd7f, 0x0000, 0x3281, 0xd8f0, 0x9641,
    0x0000, 0x69bf, 0x2710, 0xcd7f, 0x0000, 0x3281, 0xd8f0, 0x9641, 0x0000, 0x69bf, 0x2710, 0xcd7f,
    0x0000, 0x3281, 0xd8f0, 0x9641,
};
s16 gEuUnknownWave7[256] = {
    0x0000, 0x3fbc, 0x4eb4, 0x4f21, 0x6a49, 0x806f, 0x7250, 0x6a7b, 0x8d2e, 0xac0a, 0x98d6, 0x7832,
    0x7551, 0x71ca, 0x4eee, 0x3731, 0x4e20, 0x644d, 0x4a50, 0x23ba, 0x1b09, 0x119a, 0xe914, 0xccbe,
    0xe14e, 0xf8a3, 0xe47e, 0xc937, 0xd181, 0xde39, 0xcfc6, 0xcf94, 0x0000, 0x306c, 0x303a, 0x21c7,
    0x2e7f, 0x36c8, 0x1b82, 0x075e, 0x1eb2, 0x3341, 0x16ec, 0xee67, 0xe4f7, 0xdc45, 0xb5b0, 0x9bb4,
    0xb1e0, 0xc8ce, 0xb112, 0x8e37, 0x8aaf, 0x87cd, 0x672a, 0x53f7, 0x72d2, 0x9584, 0x8db0, 0x7f92,
    0x95b7, 0xb0de, 0xb14c, 0xc045, 0x0000, 0x4eb4, 0x6a49, 0x7250, 0x8d2e, 0x98d6, 0x7551, 0x4eee,
    0x4e20, 0x4a50, 0x1b09, 0xe914, 0xe14e, 0xe47e, 0xd181, 0xcfc6, 0x0000, 0x303a, 0x2e7f, 0x1b82,
    0x1eb2, 0x16ec, 0xe4f7, 0xb5b0, 0xb1e0, 0xb112, 0x8aaf, 0x672a, 0x72d2, 0x8db0, 0x95b7, 0xb14c,
    0x0000, 0x4eb4, 0x6a49, 0x7250, 0x8d2e, 0x98d6, 0x7551, 0x4eee, 0x4e20, 0x4a50, 0x1b09, 0xe914,
    0xe14e, 0xe47e, 0xd181, 0xcfc6, 0x0000, 0x303a, 0x2e7f, 0x1b82, 0x1eb2, 0x16ec, 0xe4f7, 0xb5b0,
    0xb1e0, 0xb112, 0x8aaf, 0x672a, 0x72d2, 0x8db0, 0x95b7, 0xb14c, 0x0000, 0x6a49, 0x8d2e, 0x7551,
    0x4e20, 0x1b09, 0xe14e, 0xd181, 0x0000, 0x2e7f, 0x1eb2, 0xe4f7, 0xb1e0, 0x8aaf, 0x72d2, 0x95b7,
    0x0000, 0x6a49, 0x8d2e, 0x7551, 0x4e20, 0x1b09, 0xe14e, 0xd181, 0x0000, 0x2e7f, 0x1eb2, 0xe4f7,
    0xb1e0, 0x8aaf, 0x72d2, 0x95b7, 0x0000, 0x6a49, 0x8d2e, 0x7551, 0x4e20, 0x1b09, 0xe14e, 0xd181,
    0x0000, 0x2e7f, 0x1eb2, 0xe4f7, 0xb1e0, 0x8aaf, 0x72d2, 0x95b7, 0x0000, 0x6a49, 0x8d2e, 0x7551,
    0x4e20, 0x1b09, 0xe14e, 0xd181, 0x0000, 0x2e7f, 0x1eb2, 0xe4f7, 0xb1e0, 0x8aaf, 0x72d2, 0x95b7,
    0x0000, 0x8d2e, 0x4e20, 0xe14e, 0x0000, 0x1eb2, 0xb1e0, 0x72d2, 0x0000, 0x8d2e, 0x4e20, 0xe14e,
    0x0000, 0x1eb2, 0xb1e0, 0x72d2, 0x0000, 0x8d2e, 0x4e20, 0xe14e, 0x0000, 0x1eb2, 0xb1e0, 0x72d2,
    0x0000, 0x8d2e, 0x4e20, 0xe14e, 0x0000, 0x1eb2, 0xb1e0, 0x72d2, 0x0000, 0x8d2e, 0x4e20, 0xe14e,
    0x0000, 0x1eb2, 0xb1e0, 0x72d2, 0x0000, 0x8d2e, 0x4e20, 0xe14e, 0x0000, 0x1eb2, 0xb1e0, 0x72d2,
    0x0000, 0x8d2e, 0x4e20, 0xe14e, 0x0000, 0x1eb2, 0xb1e0, 0x72d2, 0x0000, 0x8d2e, 0x4e20, 0xe14e,
    0x0000, 0x1eb2, 0xb1e0, 0x72d2,
};
s16 *gWaveSamples[6] = { sSawtoothWaves, sTriangleWaves, sSineWaves, sSquareWaves, sEuUnknownWave6, gEuUnknownWave7 };

#else
// !VERSION_EU

s16 sSineWave[0x40] = {
    0,      3211,   6392,   9511,   12539,   15446,  18204,  20787,  23169,  25329,  27244,
    28897,  30272,  31356,  32137,  32609,   0x7FFF, 32609,  32137,  31356,  30272,  28897,
    27244,  25329,  23169,  20787,  18204,   15446,  12539,  9511,   6392,   3211,   0,
    -3211,  -6392,  -9511,  -12539, -15446,  -18204, -20787, -23169, -25329, -27244, -28897,
    -30272, -31356, -32137, -32609, -0x7FFF, -32609, -32137, -31356, -30272, -28897, -27244,
    -25329, -23169, -20787, -18204, -15446,  -12539, -9511,  -6392,  -3211,
};

s16 sSquareWave[0x40] = {
    0,       0,       0,       0,       0,       0,       0,       0,       0,       0,       0,
    0,       0,       0,       0,       0,       0x7FFF,  0x7FFF,  0x7FFF,  0x7FFF,  0x7FFF,  0x7FFF,
    0x7FFF,  0x7FFF,  0x7FFF,  0x7FFF,  0x7FFF,  0x7FFF,  0x7FFF,  0x7FFF,  0x7FFF,  0x7FFF,  0,
    0,       0,       0,       0,       0,       0,       0,       0,       0,       0,       0,
    0,       0,       0,       0,       -0x7FFF, -0x7FFF, -0x7FFF, -0x7FFF, -0x7FFF, -0x7FFF, -0x7FFF,
    -0x7FFF, -0x7FFF, -0x7FFF, -0x7FFF, -0x7FFF, -0x7FFF, -0x7FFF, -0x7FFF, -0x7FFF,
};
s16 sTriangleWave[0x40] = {
    0,       0x7FF,   0xFFF,   0x17FF,  0x1FFF,  0x27FF,  0x2FFF,  0x37FF,  0x3FFF,  0x47FF,  0x4FFF,
    0x57FF,  0x5FFF,  0x67FF,  0x6FFF,  0x77FF,  0x7FFF,  0x77FF,  0x6FFF,  0x67FF,  0x5FFF,  0x57FF,
    0x4FFF,  0x47FF,  0x3FFF,  0x37FF,  0x2FFF,  0x27FF,  0x1FFF,  0x17FF,  0xFFF,   0x7FF,   0,
    -0x7FF,  -0xFFF,  -0x17FF, -0x1FFF, -10239,  -0x2FFF, -0x37FF, -0x3FFF, -0x47FF, -0x4FFF, -22527,
    -24575,  -26623,  -28671,  -30719,  -0x7FFF, -30719,  -28671,  -26623,  -24575,  -22527,  -0x4FFF,
    -0x47FF, -0x3FFF, -0x37FF, -0x2FFF, -0x27FF, -0x1FFF, -0x17FF, -0xFFF,  -0x7FF,
};

s16 sSawtoothWave[0x40] = {
    0,       1023,   2047,    3071,   4095,    5119,   6143,    7167,   8191,    9215,   10239,
    11263,   0x2FFF, 13311,   0x37FF, 15359,   0x3FFF, 17407,   0x47FF, 19455,   0x4FFF, 21503,
    22527,   23551,  24575,   25599,  26623,   27647,  28671,   29695,  30719,   31743,  -0x7FFF,
    -31743,  -30719, -29695,  -28671, -27647,  -26623, -25599,  -24575, -23551,  -22527, -21503,
    -0x4FFF, -19455, -0x47FF, -17407, -0x3FFF, -15359, -0x37FF, -13311, -0x2FFF, -11263, -10239,
    -9215,   -8191,  -7167,   -6143,  -5119,   -4095,  -3071,   -2047,  -1023,
};
s16 *gWaveSamples[4] = {
    sSawtoothWave,
    sTriangleWave,
    sSineWave,
    sSquareWave
};
#endif

#ifdef VERSION_SH
s32 unk_sh_data_0[2] = {0, 0};
f32 gPitchBendFrequencyScale[256] = {
    0.5f,      0.5f,      0.502736f, 0.505488f, 0.508254f, 0.511036f, 0.513833f, 0.516645f, 0.519472f,
    0.522315f, 0.525174f, 0.528048f, 0.530938f, 0.533843f, 0.536765f, 0.539702f, 0.542656f, 0.545626f,
    0.548612f, 0.551614f, 0.554633f, 0.557669f, 0.560721f, 0.563789f, 0.566875f, 0.569977f, 0.573097f,
    0.576233f, 0.579387f, 0.582558f, 0.585746f, 0.588951f, 0.592175f, 0.595415f, 0.598674f, 0.601950f,
    0.605245f, 0.608557f, 0.611888f, 0.615236f, 0.618603f, 0.621989f, 0.625393f, 0.628815f, 0.632257f,
    0.635717f, 0.639196f, 0.642694f, 0.646212f, 0.649748f, 0.653304f, 0.656880f, 0.660475f, 0.664089f,
    0.667724f, 0.671378f, 0.675052f, 0.678747f, 0.682461f, 0.686196f, 0.689952f, 0.693727f, 0.697524f,
    0.701341f, 0.705180f, 0.709039f, 0.712919f, 0.716821f, 0.720744f, 0.724689f, 0.728655f, 0.732642f,
    0.736652f, 0.740684f, 0.744737f, 0.748813f, 0.752911f, 0.757031f, 0.761175f, 0.765340f, 0.769529f,
    0.773740f, 0.777975f, 0.782232f, 0.786513f, 0.790818f, 0.795146f, 0.799497f, 0.803873f, 0.808272f,
    0.812696f, 0.817144f, 0.821616f, 0.826112f, 0.830633f, 0.835179f, 0.839750f, 0.844346f, 0.848966f,
    0.853613f, 0.858284f, 0.862982f, 0.867704f, 0.872453f, 0.877228f, 0.882029f, 0.886856f, 0.891709f,
    0.896590f, 0.901496f, 0.906430f, 0.911391f, 0.916379f, 0.921394f, 0.926436f, 0.931507f, 0.936604f,
    0.941730f, 0.946884f, 0.952066f, 0.957277f, 0.962516f, 0.967783f, 0.973080f, 0.978405f, 0.983760f,
    0.989144f, 0.994557f, 1.0f,      1.005473f, 1.010975f, 1.016508f, 1.022071f, 1.027665f, 1.033289f,
    1.038944f, 1.044630f, 1.050347f, 1.056095f, 1.061875f, 1.067687f, 1.073530f, 1.079405f, 1.085312f,
    1.091252f, 1.097224f, 1.103229f, 1.109267f, 1.115337f, 1.121441f, 1.127579f, 1.133750f, 1.139955f,
    1.146193f, 1.152466f, 1.158773f, 1.165115f, 1.171491f, 1.177903f, 1.184349f, 1.190831f, 1.197348f,
    1.203901f, 1.210489f, 1.217114f, 1.223775f, 1.230473f, 1.237207f, 1.243978f, 1.250786f, 1.257631f,
    1.264514f, 1.271434f, 1.278392f, 1.285389f, 1.292423f, 1.299497f, 1.306608f, 1.313759f, 1.320949f,
    1.328178f, 1.335447f, 1.342756f, 1.350104f, 1.357493f, 1.364922f, 1.372392f, 1.379903f, 1.387455f,
    1.395048f, 1.402683f, 1.410360f, 1.418078f, 1.425839f, 1.433642f, 1.441488f, 1.449377f, 1.457309f,
    1.465285f, 1.473304f, 1.481367f, 1.489474f, 1.497626f, 1.505822f, 1.514063f, 1.522349f, 1.530681f,
    1.539058f, 1.547481f, 1.555950f, 1.564465f, 1.573027f, 1.581636f, 1.590292f, 1.598995f, 1.607746f,
    1.616545f, 1.625392f, 1.634287f, 1.643231f, 1.652224f, 1.661266f, 1.670358f, 1.679500f, 1.688691f,
    1.697933f, 1.707225f, 1.716569f, 1.725963f, 1.735409f, 1.744906f, 1.754456f, 1.764058f, 1.773712f,
    1.783419f, 1.793179f, 1.802993f, 1.812860f, 1.822782f, 1.832757f, 1.842788f, 1.852873f, 1.863013f,
    1.873209f, 1.883461f, 1.893768f, 1.904132f, 1.914553f, 1.925031f, 1.935567f, 1.946159f, 1.956810f,
    1.967520f, 1.978287f, 1.989114f, 2.0f
};
#endif

#ifdef VERSION_SH
f32 unk_sh_data_1[] = {
  0.890899f,  0.890899f,  0.89171f,   0.892521f,  0.893333f,  0.894146f,  0.89496f,   0.895774f,
  0.89659f,   0.897406f,  0.898222f,  0.89904f,   0.899858f,  0.900677f,  0.901496f,  0.902317f,
  0.903138f,  0.90396f,   0.904783f,  0.905606f,  0.90643f,   0.907255f,  0.908081f,  0.908907f,
  0.909734f,  0.910562f,  0.911391f,  0.91222f,   0.91305f,   0.913881f,  0.914713f,  0.915545f,
  0.916379f,  0.917213f,  0.918047f,  0.918883f,  0.919719f,  0.920556f,  0.921394f,  0.922232f,
  0.923072f,  0.923912f,  0.924752f,  0.925594f,  0.926436f,  0.927279f,  0.928123f,  0.928968f,
  0.929813f,  0.93066f,   0.931507f,  0.932354f,  0.933203f,  0.934052f,  0.934902f,  0.935753f,
  0.936604f,  0.937457f,  0.93831f,   0.939164f,  0.940019f,  0.940874f,  0.94173f,   0.942587f,
  0.943445f,  0.944304f,  0.945163f,  0.946023f,  0.946884f,  0.947746f,  0.948608f,  0.949472f,
  0.950336f,  0.951201f,  0.952066f,  0.952933f,  0.9538f,    0.954668f,  0.955537f,  0.956406f,
  0.957277f,  0.958148f,  0.95902f,   0.959893f,  0.960766f,  0.961641f,  0.962516f,  0.963392f,
  0.964268f,  0.965146f,  0.966024f,  0.966903f,  0.967783f,  0.968664f,  0.969546f,  0.970428f,
  0.971311f,  0.972195f,  0.97308f,   0.973965f,  0.974852f,  0.975739f,  0.976627f,  0.977516f,
  0.978405f,  0.979296f,  0.980187f,  0.981079f,  0.981972f,  0.982865f,  0.98376f,   0.984655f,
  0.985551f,  0.986448f,  0.987346f,  0.988244f,  0.989144f,  0.990044f,  0.990945f,  0.991847f,
  0.992749f,  0.993653f,  0.994557f,  0.995462f,  0.996368f,  0.997275f,  0.998182f,  0.999091f,
  1.0f,       1.00091f,   1.001821f,  1.002733f,  1.003645f,  1.004559f,  1.005473f,  1.006388f,
  1.007304f,  1.00822f,   1.009138f,  1.010056f,  1.010975f,  1.011896f,  1.012816f,  1.013738f,
  1.014661f,  1.015584f,  1.016508f,  1.017433f,  1.018359f,  1.019286f,  1.020214f,  1.021142f,
  1.022071f,  1.023002f,  1.023933f,  1.024864f,  1.025797f,  1.026731f,  1.027665f,  1.0286f,
  1.029536f,  1.030473f,  1.031411f,  1.03235f,   1.033289f,  1.03423f,   1.035171f,  1.036113f,
  1.037056f,  1.038f,     1.038944f,  1.03989f,   1.040836f,  1.041783f,  1.042731f,  1.04368f,
  1.04463f,   1.045581f,  1.046532f,  1.047485f,  1.048438f,  1.049392f,  1.050347f,  1.051303f,
  1.05226f,   1.053217f,  1.054176f,  1.055135f,  1.056095f,  1.057056f,  1.058018f,  1.058981f,
  1.059945f,  1.06091f,   1.061875f,  1.062842f,  1.063809f,  1.064777f,  1.065746f,  1.066716f,
  1.067687f,  1.068658f,  1.069631f,  1.070604f,  1.071578f,  1.072554f,  1.07353f,   1.074507f,
  1.075485f,  1.076463f,  1.077443f,  1.078424f,  1.079405f,  1.080387f,  1.08137f,   1.082355f,
  1.08334f,   1.084325f,  1.085312f,  1.0863f,    1.087289f,  1.088278f,  1.089268f,  1.09026f,
  1.091252f,  1.092245f,  1.093239f,  1.094234f,  1.09523f,   1.096226f,  1.097224f,  1.098223f,
  1.099222f,  1.100222f,  1.101224f,  1.102226f,  1.103229f,  1.104233f,  1.105238f,  1.106244f,
  1.10725f,   1.108258f,  1.109267f,  1.110276f,  1.111287f,  1.112298f,  1.11331f,   1.114323f,
  1.115337f,  1.116352f,  1.117368f,  1.118385f,  1.119403f,  1.120422f,  1.121441f,  1.122462f,
};

// Shindou moved these variables down here. :/
// clang-format off
f32 gNoteFrequencies[128] = {
    0.105112f,  0.111362f,  0.117984f,  0.125f, 0.132433f, 0.140308f,  0.148651f,  0.15749f,  0.166855f, 0.176777f, 0.187288f,  0.198425f,
    0.210224f,  0.222725f,  0.235969f,  0.25f,  0.264866f, 0.280616f,  0.297302f,  0.31498f,  0.33371f,  0.353553f, 0.374577f,  0.39685f,
    0.420448f,  0.445449f,  0.471937f,  0.5f,   0.529732f, 0.561231f,  0.594604f,  0.629961f, 0.66742f,  0.707107f, 0.749154f,  0.793701f,
    0.840897f,  0.890899f,  0.943875f,  1.0f,   1.059463f, 1.122462f,  1.189207f,  1.259921f, 1.33484f,  1.414214f, 1.498307f,  1.587401f,
    1.681793f,  1.781798f,  1.887749f,  2.0f,   2.118926f, 2.244924f,  2.378414f,  2.519842f, 2.66968f,  2.828428f, 2.996615f,  3.174803f,
    3.363586f,  3.563596f,  3.775498f,  4.0f,   4.237853f, 4.489849f,  4.756829f,  5.039685f, 5.33936f,  5.656855f, 5.993229f,  6.349606f,
    6.727173f,  7.127192f,  7.550996f,  8.0f,   8.475705f, 8.979697f,  9.513658f,  10.07937f, 10.67872f, 11.31371f, 11.986459f, 12.699211f,
    13.454346f, 14.254383f, 15.101993f, 16.0f,  16.95141f, 17.959394f, 19.027315f, 20.15874f, 21.35744f, 22.62742f, 23.972918f, 25.398422f,
    26.908691f, 28.508766f, 30.203985f, 32.0f,  33.90282f, 35.91879f,  38.05463f,  40.31748f, 42.71488f, 45.25484f, 47.945835f, 50.796844f,
    53.817383f, 57.017532f, 60.40797f,  64.0f,  67.80564f, 71.83758f,  76.10926f,  80.63496f, 85.42976f, 45.25484f, 47.945835f, 50.796844f,
    53.817383f, 57.017532f, 60.40797f,  64.0f,  67.80564f, 71.83758f,  76.10926f,  80.63496f
};
// clang-format on

u8 gDefaultShortNoteVelocityTable[16] = {
    12, 25, 38, 51, 57, 64, 71, 76, 83, 89, 96, 102, 109, 115, 121, 127,
};

u8 gDefaultShortNoteDurationTable[16] = {
    229, 203, 177, 151, 139, 126, 113, 100, 87, 74, 61, 48, 36, 23, 10, 0,
};

struct AdsrEnvelope gDefaultEnvelope[] = {
    { BSWAP16(4), BSWAP16(32000) },    // go from 0 to 32000 over the course of 16ms
    { BSWAP16(1000), BSWAP16(32000) }, // stay there for 4.16 seconds
    { BSWAP16(ADSR_HANG), 0 }          // then continue staying there
};

u8 unk_sh_data2[4] = { 0, 0, 0, 0 };

struct NoteSubEu gZeroNoteSub = { 0 };
struct NoteSubEu gDefaultNoteSub = {
    1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, { NULL },
#ifdef VERSION_SH
    0
#endif
};

u16 gHeadsetPanQuantization[0x40] = {
0x3C, 0x3A, 0x38, 0x36, 0x34, 0x32, 0x30, 0x2E,
0x2C, 0x2A, 0x28, 0x26, 0x24, 0x22, 0x20, 0x1E,
0x1C, 0x1A, 0x18, 0x16, 0x14, 0x12, 0x10, 0x0E,
0x0C, 0x0A, 0x08, 0x06, 0x04, 0x02,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
};
#endif

#ifdef VERSION_EU
u8 euUnknownData_8030194c[4] = { 0x40, 0x20, 0x10, 0x08 };
u16 gHeadsetPanQuantization[0x10] = {
    0x40, 0x40, 0x30, 0x30, 0x20, 0x20, 0x10, 0, 0, 0,
};
#elif !defined(VERSION_SH)
#ifdef ENABLE_STEREO_HEADSET_EFFECTS
u16 gHeadsetPanQuantization[10] = { 0x40, 0x30, 0x20, 0x10, 0, 0, 0, 0, 0, 0 };
#endif
#endif

#if defined(VERSION_EU) || defined(VERSION_SH)
s16 euUnknownData_80301950[64] = {
    0, 0, 0,   0, 0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
    0, 0, 0, 500, 0, 0, 0, 0, 0, 0, 0, 500, 0, 0, 0, 0, 0, 0, 0, 500, 0, 0, 0, 0, 0, 0, 0, 500, 0, 0, 0, 0,
};
#endif

#ifdef ENABLE_STEREO_HEADSET_EFFECTS
// Linearly interpolated between
// f(0/2 * 127) = 1
// f(1/2 * 127) = 1/sqrt(2)
// f(2/2 * 127) = 0
f32 gHeadsetPanVolume[128] = {
    1.0f,      0.995386f, 0.990772f, 0.986157f, 0.981543f, 0.976929f, 0.972315f, 0.967701f, 0.963087f,
    0.958472f, 0.953858f, 0.949244f, 0.94463f,  0.940016f, 0.935402f, 0.930787f, 0.926173f, 0.921559f,
    0.916945f, 0.912331f, 0.907717f, 0.903102f, 0.898488f, 0.893874f, 0.88926f,  0.884646f, 0.880031f,
    0.875417f, 0.870803f, 0.866189f, 0.861575f, 0.856961f, 0.852346f, 0.847732f, 0.843118f, 0.838504f,
    0.83389f,  0.829276f, 0.824661f, 0.820047f, 0.815433f, 0.810819f, 0.806205f, 0.801591f, 0.796976f,
    0.792362f, 0.787748f, 0.783134f, 0.77852f,  0.773906f, 0.769291f, 0.764677f, 0.760063f, 0.755449f,
    0.750835f, 0.74622f,  0.741606f, 0.736992f, 0.732378f, 0.727764f, 0.72315f,  0.718535f, 0.713921f,
    0.709307f, 0.70537f,  0.70211f,  0.69885f,  0.695591f, 0.692331f, 0.689071f, 0.685811f, 0.682551f,
    0.679291f, 0.676031f, 0.672772f, 0.669512f, 0.666252f, 0.662992f, 0.659732f, 0.656472f, 0.653213f,
    0.649953f, 0.646693f, 0.643433f, 0.640173f, 0.636913f, 0.633654f, 0.630394f, 0.627134f, 0.623874f,
    0.620614f, 0.617354f, 0.614094f, 0.610835f, 0.607575f, 0.604315f, 0.601055f, 0.597795f, 0.594535f,
    0.591276f, 0.588016f, 0.584756f, 0.581496f, 0.578236f, 0.574976f, 0.571717f, 0.568457f, 0.565197f,
    0.561937f, 0.558677f, 0.555417f, 0.552157f, 0.548898f, 0.545638f, 0.542378f, 0.539118f, 0.535858f,
    0.532598f, 0.529339f, 0.526079f, 0.522819f, 0.519559f, 0.516299f, 0.513039f, 0.50978f,  0.50652f,
    0.50326f,  0.5f
};

// Linearly interpolated between
// f(0/4 * 127) = 1/sqrt(2)
// f(1/4 * 127) = 1
// f(2/4 * 127) = 1/sqrt(2)
// f(3/4 * 127) = 0
// f(4/4 * 127) = 1/sqrt(8)
f32 gStereoPanVolume[128] = {
    0.707f,    0.716228f, 0.725457f, 0.734685f, 0.743913f, 0.753142f, 0.76237f,  0.771598f, 0.780827f,
    0.790055f, 0.799283f, 0.808512f, 0.81774f,  0.826968f, 0.836197f, 0.845425f, 0.854654f, 0.863882f,
    0.87311f,  0.882339f, 0.891567f, 0.900795f, 0.910024f, 0.919252f, 0.92848f,  0.937709f, 0.946937f,
    0.956165f, 0.965394f, 0.974622f, 0.98385f,  0.993079f, 0.997693f, 0.988465f, 0.979236f, 0.970008f,
    0.960779f, 0.951551f, 0.942323f, 0.933095f, 0.923866f, 0.914638f, 0.905409f, 0.896181f, 0.886953f,
    0.877724f, 0.868496f, 0.859268f, 0.850039f, 0.840811f, 0.831583f, 0.822354f, 0.813126f, 0.803898f,
    0.794669f, 0.785441f, 0.776213f, 0.766984f, 0.757756f, 0.748528f, 0.739299f, 0.730071f, 0.720843f,
    0.711614f, 0.695866f, 0.673598f, 0.651331f, 0.629063f, 0.606795f, 0.584528f, 0.56226f,  0.539992f,
    0.517724f, 0.495457f, 0.473189f, 0.450921f, 0.428654f, 0.406386f, 0.384118f, 0.36185f,  0.339583f,
    0.317315f, 0.295047f, 0.27278f,  0.250512f, 0.228244f, 0.205976f, 0.183709f, 0.161441f, 0.139173f,
    0.116905f, 0.094638f, 0.07237f,  0.050102f, 0.027835f, 0.005567f, 0.00835f,  0.019484f, 0.030618f,
    0.041752f, 0.052886f, 0.06402f,  0.075154f, 0.086287f, 0.097421f, 0.108555f, 0.119689f, 0.130823f,
    0.141957f, 0.153091f, 0.164224f, 0.175358f, 0.186492f, 0.197626f, 0.20876f,  0.219894f, 0.231028f,
    0.242161f, 0.253295f, 0.264429f, 0.275563f, 0.286697f, 0.297831f, 0.308965f, 0.320098f, 0.331232f,
    0.342366f, 0.3535f
};
#endif

// gDefaultVolume[k] = cos(pi/2 * k / 127)
f32 gDefaultPanVolume[128] = {
    1.0f,      0.999924f, 0.999694f, 0.999312f, 0.998776f, 0.998088f, 0.997248f, 0.996254f, 0.995109f,
    0.993811f, 0.992361f, 0.990759f, 0.989006f, 0.987101f, 0.985045f, 0.982839f, 0.980482f, 0.977976f,
    0.97532f,  0.972514f, 0.96956f,  0.966457f, 0.963207f, 0.959809f, 0.956265f, 0.952574f, 0.948737f,
    0.944755f, 0.940629f, 0.936359f, 0.931946f, 0.92739f,  0.922692f, 0.917853f, 0.912873f, 0.907754f,
    0.902497f, 0.897101f, 0.891567f, 0.885898f, 0.880093f, 0.874153f, 0.868079f, 0.861873f, 0.855535f,
    0.849066f, 0.842467f, 0.835739f, 0.828884f, 0.821901f, 0.814793f, 0.807561f, 0.800204f, 0.792725f,
    0.785125f, 0.777405f, 0.769566f, 0.76161f,  0.753536f, 0.745348f, 0.737045f, 0.72863f,  0.720103f,
    0.711466f, 0.70272f,  0.693867f, 0.684908f, 0.675843f, 0.666676f, 0.657406f, 0.648036f, 0.638567f,
    0.629f,    0.619337f, 0.609579f, 0.599728f, 0.589785f, 0.579752f, 0.56963f,  0.559421f, 0.549126f,
    0.538748f, 0.528287f, 0.517745f, 0.507124f, 0.496425f, 0.485651f, 0.474802f, 0.46388f,  0.452888f,
    0.441826f, 0.430697f, 0.419502f, 0.408243f, 0.396921f, 0.385538f, 0.374097f, 0.362598f, 0.351044f,
    0.339436f, 0.327776f, 0.316066f, 0.304308f, 0.292503f, 0.280653f, 0.268761f, 0.256827f, 0.244854f,
    0.232844f, 0.220798f, 0.208718f, 0.196606f, 0.184465f, 0.172295f, 0.160098f, 0.147877f, 0.135634f,
    0.12337f,  0.111087f, 0.098786f, 0.086471f, 0.074143f, 0.061803f, 0.049454f, 0.037097f, 0.024734f,
    0.012368f, 0.0f
};

#if defined(VERSION_JP) || defined(VERSION_US)
#include "volramping.c.in"
#endif

#ifdef VERSION_SH
u16 unk_sh_data_3[] = {
    // 30 entries
    // pattern:
    // A B
    // C D
    // C B
    // A E
    0x1000, 0x1000,
    0x1000, 0x1000,
    0x1000, 0x1000,
    0x1000, 0x1000,

    0x0E6F, 0x1091,
    0x11EF, 0x1267,
    0x11EF, 0x1091,
    0x0E6F, 0x0BB8,

    0x0C1D, 0x111D,
    0x1494, 0x15D2,
    0x1494, 0x111D,
    0x0C1D, 0x068E,

    0x0838, 0x116E,
    0x18A6, 0x1B61,
    0x18A6, 0x116E,
    0x0838, 0x0001,

    0x0227, 0x0F42,
    0x1B75, 0x206B,
    0x1B75, 0x0F42,
    0x0227, 0xFA2B,

    0xFC28, 0x0AAE,
    0x1BE7, 0x2394,
    0x1BE7, 0x0AAE,
    0xFC28, 0xF874,

    0xF809, 0x0582,
    0x1C36, 0x2788,
    0x1C36, 0x0582,
    0xF809, 0xFAEB,

    0xF5F0, 0x0001,
    0x1E34, 0x2F71,
    0x1E34, 0x0001,
    0xF5F0, 0x0000,

    0xF8AD, 0xFAF3,
    0x19ED, 0x2EB6,
    0x19ED, 0xFAF3,
    0xF8AD, 0x04AC,

    0xFCC0, 0xF6FF,
    0x178B, 0x3207,
    0x178B, 0xF6FF,
    0xFCC0, 0x065F,

    0x01A5, 0xF44E,
    0x1510, 0x36B4,
    0x1510, 0xF44E,
    0x01A5, 0x047B,

    0x05C1, 0xF3CC,
    0x1145, 0x3988,
    0x1145, 0xF3CC,
    0x05C1, 0x0001,

    0x07B9, 0xF517,
    0x0D20, 0x3C4C,
    0x0D20, 0xF517,
    0x07B9, 0xFBD4,

    0x07C0, 0xF71C,
    0x09A1, 0x4528,
    0x09A1, 0xF71C,
    0x07C0, 0xF9B7,

    0x058F, 0xFA43,
    0x05DC, 0x585F,
    0x05DC, 0xFA43,
    0x058F, 0xFAB3,
};

u16 unk_sh_data_4[] = {
    0xFA73, 0xFA42,
    0xFA27, 0x5866,
    0xFA27, 0xFA42,
    0xFA73, 0xFAB2,

    0xF842, 0xF71B,
    0xF661, 0x452B,
    0xF661, 0xF71B,
    0xF842, 0xF9B5,

    0xF848, 0xF516,
    0xF2E1, 0x3C4D,
    0xF2E1, 0xF516,
    0xF848, 0xFBD2,

    0xFA3F, 0xF3CA,
    0xEEBD, 0x3989,
    0xEEBD, 0xF3CA,
    0xFA3F, 0xFFFF,

    0xFE5B, 0xF44C,
    0xEAF2, 0x36B5,
    0xEAF2, 0xF44C,
    0xFE5B, 0x0479,

    0x0340, 0xF6FD,
    0xE877, 0x3207,
    0xE877, 0xF6FD,
    0x0340, 0x065E,

    0x0753, 0xFAF1,
    0xE615, 0x2EB5,
    0xE615, 0xFAF1,
    0x0753, 0x04AB,

    0x0A12, 0xFFFF,
    0xE1CD, 0x2F71,
    0xE1CD, 0xFFFF,
    0x0A12, 0x0000,

    0x07FA, 0x057F,
    0xE3CA, 0x2789,
    0xE3CA, 0x057F,
    0x07FA, 0xFAEA,

    0x03DB, 0x0AAC,
    0xE41A, 0x2394,
    0xE41A, 0x0AAC,
    0x03DB, 0xF873,

    0xFDDC, 0x0F41,
    0xE489, 0x206E,
    0xE489, 0x0F41,
    0xFDDC, 0xFA28,

    0xF7CA, 0x116E,
    0xE758, 0x1B63,
    0xE758, 0x116E,
    0xF7CA, 0xFFFF,

    0xF3E4, 0x111D,
    0xEB6A, 0x15D4,
    0xEB6A, 0x111D,
    0xF3E4, 0x068B,

    0xF192, 0x1092,
    0xEE11, 0x1268,
    0xEE11, 0x1092,
    0xF192, 0x0BB6,

    0xF05F, 0x1026,
    0xEF89, 0x1093,
    0xEF89, 0x1026,
    0xF05F, 0x0EEB,

    0x0000, 0x0000,
    0x0000, 0x0000,
    0x7FFF, 0xD001,
    0x3FFF, 0xF001,
    0x5FFF, 0x9001,
    0x7FFF, 0x8001
};
#endif

#ifndef VERSION_SH
s16 gTatumsPerBeat = TATUMS_PER_BEAT;
s32 gAudioHeapSize = DOUBLE_SIZE_ON_64_BIT(AUDIO_HEAP_SIZE);
s32 gAudioInitPoolSize = DOUBLE_SIZE_ON_64_BIT(AUDIO_INIT_POOL_SIZE);
volatile s32 gAudioLoadLock = AUDIO_LOCK_UNINITIALIZED;
#endif

#if defined(VERSION_EU)
u8 bufferDelete2[12] = { 0 };
u8 D_EU_80302010 = 0;
u8 D_EU_80302014 = 0;

struct OSMesgQueue *OSMesgQueues[4] = {
    &OSMesgQueue0,
    &OSMesgQueue1,
    &OSMesgQueue2,
    &OSMesgQueue3
};
#endif

// .bss

volatile s32 gAudioFrameCount;

#if defined(VERSION_EU) || defined(VERSION_SH)
s32 gCurrAudioFrameDmaCount;
#else
volatile s32 gCurrAudioFrameDmaCount;
#endif

s32 gAudioTaskIndex;
s32 gCurrAiBufferIndex;

u64 *gAudioCmdBuffers[2];
u64 *gAudioCmd;

struct SPTask *gAudioTask;
struct SPTask gAudioTasks[2];

#if defined(VERSION_EU) || defined(VERSION_SH)
f32 D_EU_802298D0;
s32 gRefreshRate;
#endif

ALIGNED8 s16 *gAiBuffers[NUMAIBUFFERS];
s16 gAiBufferLengths[NUMAIBUFFERS];

u32 gAudioRandom;

#if defined(VERSION_EU) || defined(VERSION_SH)
s32 gAudioErrorFlags;
#endif

#ifdef VERSION_SH
volatile u32 gAudioLoadLockSH;
struct EuAudioCmd sAudioCmd[0x100];
u8 D_SH_80350F18;
u8 D_SH_80350F19;

OSMesg D_SH_80350F1C[1];
OSMesgQueue D_SH_80350F20; // address written to D_SH_80350F38
OSMesgQueue *D_SH_80350F38;

OSMesg D_SH_80350F40[4];
OSMesgQueue D_SH_80350F50; // address written to D_SH_80350F68
OSMesgQueue *D_SH_80350F68;

OSMesg D_SH_80350F6C[1];
OSMesgQueue D_SH_80350F70; // address written to D_SH_80350F88
OSMesgQueue *D_SH_80350F88;

OSMesg D_SH_80350F8C[1];
OSMesgQueue D_SH_80350F90; // address written to D_SH_80350F90
OSMesgQueue *D_SH_80350FA8;
#endif

u64 gAudioGlobalsEndMarker;
