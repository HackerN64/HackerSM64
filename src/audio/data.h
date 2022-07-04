#ifndef AUDIO_DATA_H
#define AUDIO_DATA_H

#include <PR/ultratypes.h>

#include "internal.h"
#include "types.h"
#include "synthesis.h"

#define AUDIO_LOCK_UNINITIALIZED 0x00000000
#define AUDIO_LOCK_NOT_LOADING 0x76557364
#define AUDIO_LOCK_LOADING 0x19710515

#define NUMAIBUFFERS 3

#if defined(VERSION_EU)
#define DMA_BUF_SIZE_0 0x400
#define DMA_BUF_SIZE_1 0x200
#else
#define DMA_BUF_SIZE_0 (144 * 9)
#define DMA_BUF_SIZE_1 (160 * 9)
#endif

#ifdef EXPAND_AUDIO_HEAP
#define PERSISTENT_SEQ_MEM 0x8200
#define PERSISTENT_BANK_MEM 0xDC00
#define TEMPORARY_SEQ_MEM 0xE800
#define TEMPORARY_BANK_MEM 0x5500
#define BANK_SETS_ALLOC 0x400
#define EXT_AUDIO_INIT_POOL_SIZE 0x2000
#else
#define PERSISTENT_SEQ_MEM 0x4100
#define PERSISTENT_BANK_MEM 0x6E00
#define TEMPORARY_SEQ_MEM 0x7400
#define TEMPORARY_BANK_MEM 0x2A80
#define BANK_SETS_ALLOC 0x100
#define EXT_AUDIO_INIT_POOL_SIZE 0x0
#endif

#define SEQ_BANK_MEM (PERSISTENT_SEQ_MEM + PERSISTENT_BANK_MEM + TEMPORARY_SEQ_MEM + TEMPORARY_BANK_MEM)

// constant .data
#if defined(VERSION_EU) || defined(VERSION_SH)
extern struct AudioSessionSettingsEU gAudioSessionPresets[];
extern struct ReverbSettingsEU sReverbSettings[8];
#else
extern struct AudioSessionSettings gAudioSessionPresets[1];
extern struct ReverbSettingsUS gReverbSettings[18];
#endif
extern u16 D_80332388[128]; // unused

#if defined(VERSION_EU) || defined(VERSION_SH)
extern f32 gPitchBendFrequencyScale[256];
#else
extern f32 gPitchBendFrequencyScale[255];
#endif
extern f32 gNoteFrequencies[128];

extern u8 gDefaultShortNoteVelocityTable[16];
extern u8 gDefaultShortNoteDurationTable[16];
extern s8 gVibratoCurve[16];
extern struct AdsrEnvelope gDefaultEnvelope[3];

#if defined(VERSION_EU) || defined(VERSION_SH)
extern s16 gEuUnknownWave7[256];
extern s16 *gWaveSamples[6];
#else
extern s16 *gWaveSamples[4];
#endif

#if defined(VERSION_EU) || defined(VERSION_SH)
extern u8 euUnknownData_8030194c[4];
#ifdef VERSION_EU
extern u16 gHeadsetPanQuantization[0x10];
#else
extern u16 gHeadsetPanQuantization[0x40];
#endif
extern s16 euUnknownData_80301950[64];
extern struct NoteSubEu gZeroNoteSub;
extern struct NoteSubEu gDefaultNoteSub;
#else
extern u16 gHeadsetPanQuantization[10];
#endif
extern f32 gHeadsetPanVolume[128];
extern f32 gStereoPanVolume[128];
extern f32 gDefaultPanVolume[128];

extern f32 gVolRampingLhs136[128];
extern f32 gVolRampingRhs136[128];
extern f32 gVolRampingLhs144[128];
extern f32 gVolRampingRhs144[128];
extern f32 gVolRampingLhs128[128];
extern f32 gVolRampingRhs128[128];

// non-constant .data
extern s16 gTatumsPerBeat;
extern s32 gAudioHeapSize; // AUDIO_HEAP_SIZE
extern s32 gAudioInitPoolSize; // AUDIO_INIT_POOL_SIZE
extern volatile s32 gAudioLoadLock;

// .bss
extern volatile s32 gAudioFrameCount;

// number of DMAs performed during this frame
#if defined(VERSION_EU) || defined(VERSION_SH)
extern s32 gCurrAudioFrameDmaCount;
#else
extern volatile s32 gCurrAudioFrameDmaCount;
#endif

extern s32 gAudioTaskIndex;
extern s32 gCurrAiBufferIndex;

extern u64 *gAudioCmdBuffers[2];
extern u64 *gAudioCmd;

extern struct SPTask *gAudioTask;
extern struct SPTask gAudioTasks[2];

#if defined(VERSION_EU) || defined(VERSION_SH)
extern f32 D_EU_802298D0;
extern s32 gRefreshRate;
#endif

extern s16 *gAiBuffers[NUMAIBUFFERS];
extern s16 gAiBufferLengths[NUMAIBUFFERS];
#if defined(VERSION_SH)
#define AIBUFFER_LEN 0xb00
#elif defined(VERSION_EU)
#define AIBUFFER_LEN (0xa0 * 17)
#else
#define AIBUFFER_LEN (0xa0 * 16)
#endif

extern u32 gAudioRandom;

#if defined(VERSION_US) || defined(VERSION_JP)
#define NOTES_BUFFER_SIZE \
( \
    MAX_SIMULTANEOUS_NOTES * ((4 /* updatesPerFrame */ * 20 * 2 * sizeof(u64)) \
    + ALIGN16(sizeof(struct Note)) \
    + (DMA_BUF_SIZE_0 * 3) \
    + DMA_BUF_SIZE_1 \
    + ALIGN16(sizeof(struct NoteSynthesisBuffers))) \
    + (320 * 2 * sizeof(u64)) /* gMaxAudioCmds */ \
)
#else // Probably SH incompatible but that's an entirely different headache to save at this point tbh
#define NOTES_BUFFER_SIZE \
( \
    MAX_SIMULTANEOUS_NOTES * ((4 /* updatesPerFrame */ * 0x10 * 2 * sizeof(u64)) \
    + ALIGN16(sizeof(struct Note)) \
    + (DMA_BUF_SIZE_0 * 3 * 1 /* presetUnk4 */) \
    + (DMA_BUF_SIZE_1) \
    + ALIGN16(sizeof(struct NoteSynthesisBuffers)) \
    + ALIGN16(4 /* updatesPerFrame */ * sizeof(struct NoteSubEu))) \
    + ((0x300 + (4 /* numReverbs */ * 0x20)) * 2 * sizeof(u64)) /* gMaxAudioCmds */ \
)
#endif

#ifdef VERSION_SH
extern f32 unk_sh_data_1[];

extern volatile u32 gAudioLoadLockSH;

extern u8 D_SH_80350F18;
extern u8 D_SH_80350F19;

extern OSMesg D_SH_80350F1C[1];
extern OSMesgQueue D_SH_80350F20; // address written to D_SH_80350F38
extern OSMesgQueue *D_SH_80350F38;

extern OSMesg D_SH_80350F40[4];
extern OSMesgQueue D_SH_80350F50; // address written to D_SH_80350F68
extern OSMesgQueue *D_SH_80350F68;

extern OSMesg D_SH_80350F6C[1];
extern OSMesgQueue D_SH_80350F70; // address written to D_SH_80350F88
extern OSMesgQueue *D_SH_80350F88;

extern OSMesg D_SH_80350F8C[1];
extern OSMesgQueue D_SH_80350F90; // address written to D_SH_80350F90
extern OSMesgQueue *D_SH_80350FA8;
#endif

#if defined(VERSION_EU) || defined(VERSION_SH)
#define AUDIO_INIT_POOL_SIZE (0x2B00 + BANK_SETS_ALLOC + EXT_AUDIO_INIT_POOL_SIZE)
#else
#define AUDIO_INIT_POOL_SIZE (0x2400 + BANK_SETS_ALLOC + EXT_AUDIO_INIT_POOL_SIZE)
#endif

// TODO: needs validation once EU can compile. EU is very likely incorrect!
#define AUDIO_HEAP_SIZE (SEQ_BANK_MEM + AUDIO_INIT_POOL_SIZE + NOTES_BUFFER_SIZE + BETTER_REVERB_SIZE + REVERB_WINDOW_HEAP_SIZE)

#ifdef VERSION_SH
extern u32 D_SH_80315EF0;
extern u16 D_SH_80315EF4;
extern u16 D_SH_80315EF8;
extern u16 D_SH_80315EFC;
#endif

#endif // AUDIO_DATA_H
