#ifndef VERSION_SH
#include <ultra64.h>

#include "synthesis.h"
#include "heap.h"
#include "data.h"
#include "load.h"
#include "seqplayer.h"
#include "internal.h"
#include "external.h"
#include "game/game_init.h"
#include "game/debug.h"
#include "engine/math_util.h"

#define DMEM_ADDR_TEMP 0x0
#define DMEM_ADDR_RESAMPLED 0x20
#define DMEM_ADDR_RESAMPLED2 0x160
#define DMEM_ADDR_UNCOMPRESSED_NOTE 0x180
#define DMEM_ADDR_NOTE_PAN_TEMP 0x200
#define DMEM_ADDR_STEREO_STRONG_TEMP_DRY 0x200
#define DMEM_ADDR_STEREO_STRONG_TEMP_WET 0x340
#define DMEM_ADDR_COMPRESSED_ADPCM_DATA 0x3f0
#define DMEM_ADDR_LEFT_CH 0x4c0
#define DMEM_ADDR_RIGHT_CH 0x600
#define DMEM_ADDR_WET_LEFT_CH 0x740
#define DMEM_ADDR_WET_RIGHT_CH 0x880

#define aSetLoadBufferPair(pkt, c, off)                                                                \
    aSetBuffer(pkt, 0, c + DMEM_ADDR_WET_LEFT_CH, 0, DEFAULT_LEN_1CH - c);                             \
    aLoadBuffer(pkt, VIRTUAL_TO_PHYSICAL2(gSynthesisReverb.ringBuffer.left + (off)));                  \
    aSetBuffer(pkt, 0, c + DMEM_ADDR_WET_RIGHT_CH, 0, DEFAULT_LEN_1CH - c);                            \
    aLoadBuffer(pkt, VIRTUAL_TO_PHYSICAL2(gSynthesisReverb.ringBuffer.right + (off)));

#define aSetSaveBufferPair(pkt, c, d, off)                                                             \
    aSetBuffer(pkt, 0, 0, c + DMEM_ADDR_WET_LEFT_CH, d);                                               \
    aSaveBuffer(pkt, VIRTUAL_TO_PHYSICAL2(gSynthesisReverb.ringBuffer.left +  (off)));                 \
    aSetBuffer(pkt, 0, 0, c + DMEM_ADDR_WET_RIGHT_CH, d);                                              \
    aSaveBuffer(pkt, VIRTUAL_TO_PHYSICAL2(gSynthesisReverb.ringBuffer.right + (off)));

#define VOLRAMPING_MASK (~(0x8000 | ((1 << (15 - VOL_RAMPING_EXPONENT)) - 1)))


#ifdef BETTER_REVERB
// Do not touch these values manually, unless you want potential for problems.
u8 gBetterReverbPresetValue = 0;
u8 activeBetterReverbPreset = 0;
u8 toggleBetterReverb = FALSE;
u8 betterReverbLightweight = FALSE;
u8 monoReverb;
s8 betterReverbDownsampleRate;
static s32         reverbMults[SYNTH_CHANNEL_STEREO_COUNT][NUM_ALLPASS / 3] = {0};
static s32          allpassIdx[SYNTH_CHANNEL_STEREO_COUNT][NUM_ALLPASS] = {0};
static s32  betterReverbDelays[SYNTH_CHANNEL_STEREO_COUNT][NUM_ALLPASS] = {0};
static s32 historySamplesLight[SYNTH_CHANNEL_STEREO_COUNT];
static s16         **delayBufs[SYNTH_CHANNEL_STEREO_COUNT];
u8 *gReverbMults[SYNTH_CHANNEL_STEREO_COUNT];
s32 reverbLastFilterIndex;
s32 reverbFilterCount;
s32 betterReverbWindowsSize;
s32 betterReverbRevIndex; // This one is okay to adjust whenever
s32 betterReverbGainIndex; // This one is okay to adjust whenever
#endif

struct VolumeChange {
    u16 sourceLeft;
    u16 sourceRight;
    u16 targetLeft;
    u16 targetRight;
};

u64 *synthesis_do_one_audio_update(s16 *aiBuf, u32 bufLen, u64 *cmd, s32 updateIndex);
u64 *synthesis_process_notes(s16 *aiBuf, u32 bufLen, u64 *cmd);
u64 *load_wave_samples(u64 *cmd, struct Note *note, s32 nSamplesToLoad);
#ifdef ENABLE_STEREO_HEADSET_EFFECTS
u64 *process_envelope(u64 *cmd, struct Note *note, s32 nSamples, u16 inBuf, s32 headsetPanSettings);
u64 *note_apply_headset_pan_effects(u64 *cmd, struct Note *note, s32 bufLen, s32 flags, s32 leftRight);
#else
u64 *process_envelope(u64 *cmd, struct Note *note, s32 nSamples, u16 inBuf);
#endif

struct SynthesisReverb gSynthesisReverb;

f32 *currentRampingTableLeft;
f32 *currentRampingTableRight;

#ifdef BETTER_REVERB
static void reverb_samples(s16 *start, s16 *end, s16 *downsampleBuffer, s32 channel) {
    s16 *curDelaySample;
    s32 historySample;
    s32 tmpCarryover;
    s32 outSampleTotal;
    s32 i;
    s32 j;
    s32 k;

    s32 downsampleIncrement = gReverbDownsampleRate;
    s32 *delaysLocal = betterReverbDelays[channel];
    s32 *reverbMultsLocal = reverbMults[channel];
    s32 *allpassIdxLocal = allpassIdx[channel];
    s16 **delayBufsLocal = delayBufs[channel];

    s32 lastFilterIndex = reverbLastFilterIndex;
    s32 revIndex = betterReverbRevIndex;
    s32 gainIndex = betterReverbGainIndex;

    // Set j outside of the loop only. Because we're forcing filter count to always be a multiple of 3, we can count on j always being 0 when exiting the second for loop.
    j = 0;

    for (; start < end; start++, downsampleBuffer += downsampleIncrement) {
        // Mix the very last filter output with new incoming sample
        tmpCarryover = ((delayBufsLocal[lastFilterIndex][allpassIdxLocal[lastFilterIndex]] * revIndex) >> 8) + *downsampleBuffer;
        outSampleTotal = 0;
        i = 0;
        k = 0;

        for (; i <= lastFilterIndex; ++i, ++j) {
            curDelaySample = &delayBufsLocal[i][allpassIdxLocal[i]];
            historySample = *curDelaySample;

            if (j == 2) {
                j = -1;
                outSampleTotal += ((historySample * reverbMultsLocal[k++]) >> 8);
                *curDelaySample = CLAMP_S16(tmpCarryover);
                if (i != lastFilterIndex)
                    tmpCarryover = ((historySample * revIndex) >> 8)/* + *downsampleBuffer*/;
            } else {
                tmpCarryover += (historySample * (-gainIndex)) >> 8;
                *curDelaySample = CLAMP_S16(tmpCarryover);
                tmpCarryover = ((tmpCarryover * gainIndex) >> 8) + historySample;
            }

            if (++allpassIdxLocal[i] == delaysLocal[i]) allpassIdxLocal[i] = 0;
        }

        *start = CLAMP_S16(outSampleTotal);
    }
}

static void reverb_samples_light(s16 *start, s16 *end, s16 *downsampleBuffer, s32 channel) {
    s16 *curDelaySample;
    s32 historySample;
    s32 tmpCarryover;
    s32 i;

    s32 downsampleIncrement = gReverbDownsampleRate;
    s32 *delaysLocal = betterReverbDelays[channel];
    s32 *allpassIdxLocal = allpassIdx[channel];
    s16 **delayBufsLocal = delayBufs[channel];

    // Get history sample from last processing tick
    tmpCarryover = historySamplesLight[channel];

    for (; start < end; start++, downsampleBuffer += downsampleIncrement) {
        // Mix previous sample with new incoming sample
        tmpCarryover = ((tmpCarryover * BETTER_REVERB_REVERB_INDEX_LIGHT) >> 8) + *downsampleBuffer;

        for (i = 0; i < BETTER_REVERB_FILTER_COUNT_LIGHT; ++i) {
            curDelaySample = &delayBufsLocal[i][allpassIdxLocal[i]];
            historySample = *curDelaySample;

            tmpCarryover += ((historySample * (-BETTER_REVERB_GAIN_INDEX_LIGHT)) >> 8);
            *curDelaySample = CLAMP_S16(tmpCarryover);
            tmpCarryover = ((tmpCarryover * BETTER_REVERB_GAIN_INDEX_LIGHT) >> 8) + historySample;

            if (++allpassIdxLocal[i] == delaysLocal[i]) allpassIdxLocal[i] = 0;
        }

        // Lightweight does not use the final filter type at all, unlike standard reverb processing
        *start = CLAMP_S16(tmpCarryover);
    }
    
    // Copy history sample to temporary buffer for processing next tick
    historySamplesLight[channel] = tmpCarryover;
}

void initialize_better_reverb_buffers(void) {
    delayBufs[SYNTH_CHANNEL_LEFT] = (s16**) soundAlloc(&gBetterReverbPool, BETTER_REVERB_PTR_SIZE);
    delayBufs[SYNTH_CHANNEL_RIGHT] = &delayBufs[SYNTH_CHANNEL_LEFT][NUM_ALLPASS];
}

void set_better_reverb_buffers(u32 *inputDelaysL, u32 *inputDelaysR) {
    s32 bufOffset = 0;
    s32 filterCount = reverbFilterCount;
    u32 *inputDelayPtrs[SYNTH_CHANNEL_STEREO_COUNT] = {
        [SYNTH_CHANNEL_LEFT]  = inputDelaysL,
        [SYNTH_CHANNEL_RIGHT] = inputDelaysR,
    };

    if (betterReverbLightweight)
        filterCount = BETTER_REVERB_FILTER_COUNT_LIGHT;

    gBetterReverbPool.cur = gBetterReverbPool.start + BETTER_REVERB_PTR_SIZE; // Reset reverb data pool

    // Don't bother setting any buffers if BETTER_REVERB is disabled
    if (!toggleBetterReverb)
        return;

    // NOTE: Using filterCount over NUM_ALLPASS will report less memory usage with fewer filters, but poses an additional
    // risk to anybody testing on console with performance compromises, as emulator can be easily overlooked.
    for (s32 channel = 0; channel < SYNTH_CHANNEL_STEREO_COUNT; channel++) {
        historySamplesLight[channel] = 0;
        for (s32 filter = 0; filter < filterCount; filter++) {
            betterReverbDelays[channel][filter] = (s32) (inputDelayPtrs[channel][filter] / gReverbDownsampleRate);
            delayBufs[channel][filter] = soundAlloc(&gBetterReverbPool, betterReverbDelays[channel][filter] * sizeof(s16));
            bufOffset += betterReverbDelays[channel][filter];
        }
    }

    aggress_args(bufOffset * sizeof(s16) + BETTER_REVERB_PTR_SIZE <= BETTER_REVERB_SIZE, "BETTER_REVERB_SIZE is too small for this preset!\nSize should be at least 0x%06x!", bufOffset * sizeof(s16) + BETTER_REVERB_PTR_SIZE);

    bzero(allpassIdx, sizeof(allpassIdx));
}
#endif

void prepare_reverb_ring_buffer(s32 chunkLen, u32 updateIndex) {
    struct ReverbRingBufferItem *item;
    s32 srcPos, dstPos;
    s32 nSamples;
    s32 excessiveSamples;

    if (gSynthesisReverb.framesLeftToIgnore == 0) {
#ifdef BETTER_REVERB
        if (!toggleBetterReverb && gReverbDownsampleRate != 1) {
#else
        if (gReverbDownsampleRate != 1) {
#endif
            // Now that the RSP has finished, downsample the samples produced two frames ago by skipping
            // samples.
            item = &gSynthesisReverb.items[gSynthesisReverb.curFrame][updateIndex];

            // Touches both left and right since they are adjacent in memory
            osInvalDCache(item->toDownsampleLeft, DEFAULT_LEN_2CH);

            for (srcPos = 0, dstPos = 0; dstPos < item->lengthA / 2;
                 srcPos += gReverbDownsampleRate, dstPos++) {
                gSynthesisReverb.ringBuffer.left[dstPos + item->startPos] = item->toDownsampleLeft[srcPos];
                gSynthesisReverb.ringBuffer.right[dstPos + item->startPos] = item->toDownsampleRight[srcPos];
            }
            for (dstPos = 0; dstPos < item->lengthB / 2; srcPos += gReverbDownsampleRate, dstPos++) {
                gSynthesisReverb.ringBuffer.left[dstPos] = item->toDownsampleLeft[srcPos];
                gSynthesisReverb.ringBuffer.right[dstPos] = item->toDownsampleRight[srcPos];
            }
        }
#ifdef BETTER_REVERB
        else if (toggleBetterReverb) {
            s32 loopCounts[2];

            s16 *betterReverbDownsampleBuffers[SYNTH_CHANNEL_STEREO_COUNT][ARRAY_COUNT(loopCounts)]; // StartA and StartB for both channels
            s16 *betterReverbSampleBuffers[SYNTH_CHANNEL_STEREO_COUNT][ARRAY_COUNT(loopCounts)]; // Output reverb buffers
            void (*reverbFunc)(s16*, s16*, s16*, s32) = betterReverbLightweight ? reverb_samples_light : reverb_samples; // Function pointers for both heavy and lightweight reverb functions

            item = &gSynthesisReverb.items[gSynthesisReverb.curFrame][updateIndex];
            loopCounts[0] = item->lengthA / 2;
            loopCounts[1] = item->lengthB / 2;

            if (gReverbDownsampleRate != 1) {
                // NOTE: / HACKERSM64 TODO: Commenting this check seems to improve runtime by about 100 microseconds (per 30fps frame),
                // but idk enough about why it was added here in vanilla to comfortably remove it. Is it supposed to act as an
                // optimization (that isn't actually an optimization) or is it a safety measure since it's loaded from the RSP?
                osInvalDCache(item->toDownsampleLeft, DEFAULT_LEN_2CH);
            }
            
            betterReverbSampleBuffers[SYNTH_CHANNEL_LEFT][0] = &gSynthesisReverb.ringBuffer.left[item->startPos];
            betterReverbSampleBuffers[SYNTH_CHANNEL_LEFT][1] = &gSynthesisReverb.ringBuffer.left[0];
            betterReverbSampleBuffers[SYNTH_CHANNEL_RIGHT][0] = &gSynthesisReverb.ringBuffer.right[item->startPos];
            betterReverbSampleBuffers[SYNTH_CHANNEL_RIGHT][1] = &gSynthesisReverb.ringBuffer.right[0];
            if (gReverbDownsampleRate > 1) {
                betterReverbDownsampleBuffers[SYNTH_CHANNEL_LEFT][0] = &item->toDownsampleLeft[0];
                betterReverbDownsampleBuffers[SYNTH_CHANNEL_LEFT][1] = &item->toDownsampleLeft[loopCounts[0] * gReverbDownsampleRate];
                betterReverbDownsampleBuffers[SYNTH_CHANNEL_RIGHT][0] = &item->toDownsampleRight[0];
                betterReverbDownsampleBuffers[SYNTH_CHANNEL_RIGHT][1] = &item->toDownsampleRight[loopCounts[0] * gReverbDownsampleRate];
            } else {
                betterReverbDownsampleBuffers[SYNTH_CHANNEL_LEFT][0] = betterReverbSampleBuffers[SYNTH_CHANNEL_LEFT][0];
                betterReverbDownsampleBuffers[SYNTH_CHANNEL_LEFT][1] = betterReverbSampleBuffers[SYNTH_CHANNEL_LEFT][1];
                betterReverbDownsampleBuffers[SYNTH_CHANNEL_RIGHT][0] = betterReverbSampleBuffers[SYNTH_CHANNEL_RIGHT][0];
                betterReverbDownsampleBuffers[SYNTH_CHANNEL_RIGHT][1] = betterReverbSampleBuffers[SYNTH_CHANNEL_RIGHT][1];
            }

            if (gSoundMode == SOUND_MODE_MONO || monoReverb) {
                for (srcPos = 0; srcPos < ARRAY_COUNT(loopCounts); srcPos++) { // LengthA and LengthB processing
                    s16 *downsampleBufferL = betterReverbDownsampleBuffers[SYNTH_CHANNEL_LEFT][srcPos];
                    s16 *downsampleBufferR = betterReverbDownsampleBuffers[SYNTH_CHANNEL_RIGHT][srcPos];
                    for (dstPos = 0; dstPos < loopCounts[srcPos]; dstPos += gReverbDownsampleRate) { // Individual sample processing
                        downsampleBufferL[dstPos] = ((s32) downsampleBufferL[dstPos] + (s32) downsampleBufferR[dstPos]) >> 1; // Merge stereo samples into left channel
                    }
                }
                for (srcPos = 0; srcPos < ARRAY_COUNT(loopCounts); srcPos++) { // LengthA and LengthB processing
                    // Call core reverb processing function, either reverb_samples() or reverb_samples_light()
                    (*reverbFunc)(betterReverbSampleBuffers[SYNTH_CHANNEL_LEFT][srcPos], betterReverbSampleBuffers[SYNTH_CHANNEL_LEFT][srcPos] + loopCounts[srcPos], betterReverbDownsampleBuffers[SYNTH_CHANNEL_LEFT][srcPos], SYNTH_CHANNEL_LEFT);
                    bcopy(betterReverbSampleBuffers[SYNTH_CHANNEL_LEFT][srcPos], betterReverbSampleBuffers[SYNTH_CHANNEL_RIGHT][srcPos], loopCounts[srcPos] * sizeof(s16));
                }
            } else {
                for (dstPos = 0; dstPos < SYNTH_CHANNEL_STEREO_COUNT; dstPos++) { // left and right channels
                    for (srcPos = 0; srcPos < ARRAY_COUNT(loopCounts); srcPos++) { // LengthA and LengthB processing
                        // Call core reverb processing function, either reverb_samples() or reverb_samples_light()
                        (*reverbFunc)(betterReverbSampleBuffers[dstPos][srcPos], betterReverbSampleBuffers[dstPos][srcPos] + loopCounts[srcPos], betterReverbDownsampleBuffers[dstPos][srcPos], dstPos);
                    }
                }
            }
        }
#endif
    }
    item = &gSynthesisReverb.items[gSynthesisReverb.curFrame][updateIndex];

    s32 numSamplesAfterDownsampling = chunkLen / gReverbDownsampleRate;
    if (((numSamplesAfterDownsampling + gSynthesisReverb.nextRingBufferPos) - gSynthesisReverb.bufSizePerChannel) < 0) {
        // There is space in the ring buffer before it wraps around
        item->lengthA = numSamplesAfterDownsampling * 2;
        item->lengthB = 0;
        item->startPos = (s32) gSynthesisReverb.nextRingBufferPos;
        gSynthesisReverb.nextRingBufferPos += numSamplesAfterDownsampling;
    } else {
        // Ring buffer wrapped around
        excessiveSamples =
            (numSamplesAfterDownsampling + gSynthesisReverb.nextRingBufferPos) - gSynthesisReverb.bufSizePerChannel;
        nSamples = numSamplesAfterDownsampling - excessiveSamples;
        item->lengthA = nSamples * 2;
        item->lengthB = excessiveSamples * 2;
        item->startPos = gSynthesisReverb.nextRingBufferPos;
        gSynthesisReverb.nextRingBufferPos = excessiveSamples;
    }
    // These fields are never read later
    item->numSamplesAfterDownsampling = numSamplesAfterDownsampling;
    item->chunkLen = chunkLen;
}

// bufLen will be divisible by 16
u64 *synthesis_execute(u64 *cmdBuf, s32 *writtenCmds, s16 *aiBuf, s32 bufLen) {
    u32 chunkLen;
    s32 i;
    u32 *aiBufPtr = (u32 *) aiBuf;
    u64 *cmd = cmdBuf + 1;
    s32 v0;

    aSegment(cmdBuf, 0, 0);

#ifdef BETTER_REVERB
    s32 filterCountDiv3 = reverbFilterCount / 3;
    reverbFilterCount = filterCountDiv3 * 3; // reverbFilterCount should always be a multiple of 3.

    if (reverbFilterCount > NUM_ALLPASS) {
        reverbFilterCount = NUM_ALLPASS;
    } else if (reverbFilterCount < 3) {
        reverbFilterCount = 3;
    }

    reverbLastFilterIndex = reverbFilterCount - 1;

    // Update reverbMults every audio frame just in case gReverbMults is ever to change.
    if (gReverbMults[SYNTH_CHANNEL_LEFT] != NULL && gReverbMults[SYNTH_CHANNEL_RIGHT] != NULL) {
        for (i = 0; i < filterCountDiv3; ++i) {
            reverbMults[SYNTH_CHANNEL_LEFT][i] = gReverbMults[SYNTH_CHANNEL_LEFT][i];
            reverbMults[SYNTH_CHANNEL_RIGHT][i] = gReverbMults[SYNTH_CHANNEL_RIGHT][i];
        }
    }

    // If there's only one reverb multiplier set, adjust these to match so one channel doesn't end up potentially overpowering the other.
    if (filterCountDiv3 == 1) {
        reverbMults[SYNTH_CHANNEL_LEFT][0] = (reverbMults[SYNTH_CHANNEL_RIGHT][0] + reverbMults[SYNTH_CHANNEL_LEFT][0]) / 2;
        reverbMults[SYNTH_CHANNEL_RIGHT][0] = reverbMults[SYNTH_CHANNEL_LEFT][0];
    }
#endif

    for (i = gAudioUpdatesPerFrame; i > 0; i--) {
        if (i == 1) {
            // 'bufLen' will automatically be divisible by 8, no need to round
            chunkLen = bufLen;
        } else {
            v0 = bufLen / i;
            // chunkLen = v0 rounded to nearest multiple of 8
            chunkLen = v0 - (v0 & 7);

            if ((v0 & 7) >= 4) {
                chunkLen += 8;
            }
        }

        AUDIO_PROFILER_SWITCH(PROFILER_TIME_SUB_AUDIO_UPDATE, PROFILER_TIME_SUB_AUDIO_SEQUENCES);
        AUDIO_PROFILER_START_SHARED(PROFILER_TIME_SUB_AUDIO_SEQUENCES, PROFILER_TIME_SUB_AUDIO_SEQUENCES_SCRIPT);

        process_sequences(i - 1);

        AUDIO_PROFILER_COMPLETE_AND_SWITCH(PROFILER_TIME_SUB_AUDIO_SEQUENCES_PROCESSING, PROFILER_TIME_SUB_AUDIO_SEQUENCES, PROFILER_TIME_SUB_AUDIO_SYNTHESIS);
        AUDIO_PROFILER_START_SHARED(PROFILER_TIME_SUB_AUDIO_SYNTHESIS, PROFILER_TIME_SUB_AUDIO_SYNTHESIS_ENVELOPE_REVERB);

        if (gSynthesisReverb.useReverb) {
            prepare_reverb_ring_buffer(chunkLen, gAudioUpdatesPerFrame - i);
        }
        cmd = synthesis_do_one_audio_update((s16 *) aiBufPtr, chunkLen * 2, cmd, gAudioUpdatesPerFrame - i);

        AUDIO_PROFILER_COMPLETE_AND_SWITCH(PROFILER_TIME_SUB_AUDIO_SYNTHESIS_ENVELOPE_REVERB, PROFILER_TIME_SUB_AUDIO_SYNTHESIS, PROFILER_TIME_SUB_AUDIO_UPDATE);

        bufLen -= chunkLen;
        aiBufPtr += chunkLen;
    }
    if (gSynthesisReverb.framesLeftToIgnore != 0) {
        gSynthesisReverb.framesLeftToIgnore--;
    }
    gSynthesisReverb.curFrame ^= 1;
    *writtenCmds = cmd - cmdBuf;
    return cmd;
}

u64 *synthesis_do_one_audio_update(s16 *aiBuf, u32 bufLen, u64 *cmd, s32 updateIndex) {
    s16 ra;
    s16 t4;
    struct ReverbRingBufferItem *v1;

    v1 = &gSynthesisReverb.items[gSynthesisReverb.curFrame][updateIndex];

    if (!gSynthesisReverb.useReverb) {
        aClearBuffer(cmd++, DMEM_ADDR_LEFT_CH, DEFAULT_LEN_2CH);

        AUDIO_PROFILER_SWITCH(PROFILER_TIME_SUB_AUDIO_SYNTHESIS_ENVELOPE_REVERB, PROFILER_TIME_SUB_AUDIO_SYNTHESIS_PROCESSING);
        cmd = synthesis_process_notes(aiBuf, bufLen, cmd);
        AUDIO_PROFILER_SWITCH(PROFILER_TIME_SUB_AUDIO_SYNTHESIS_PROCESSING, PROFILER_TIME_SUB_AUDIO_SYNTHESIS_ENVELOPE_REVERB);
    } else {
        if (gReverbDownsampleRate == 1) {
            // Put the oldest samples in the ring buffer into the wet channels
            aSetLoadBufferPair(cmd++, 0, v1->startPos);
            if (v1->lengthB != 0) {
                // Ring buffer wrapped
                aSetLoadBufferPair(cmd++, v1->lengthA, 0);
            }

            // Use the reverb sound as initial sound for this audio update
            aDMEMMove(cmd++, DMEM_ADDR_WET_LEFT_CH, DMEM_ADDR_LEFT_CH, DEFAULT_LEN_2CH);

            // (Hopefully) lower the volume of the wet channels. New reverb will later be mixed into
            // these channels.
            aSetBuffer(cmd++, 0, 0, 0, DEFAULT_LEN_2CH);
            // 0x8000 here is -100%
            aMix(cmd++, 0, /*gain*/ 0x8000 + gSynthesisReverb.reverbGain, /*in*/ DMEM_ADDR_WET_LEFT_CH,
                 /*out*/ DMEM_ADDR_WET_LEFT_CH);
        } else {
            // Same as above but upsample the previously downsampled samples used for reverb first
            t4 = (v1->startPos & 7) * 2;
            ra = ALIGN16(v1->lengthA + t4);
            aSetLoadBufferPair(cmd++, 0, v1->startPos - t4 / 2);
            if (v1->lengthB != 0) {
                // Ring buffer wrapped
                aSetLoadBufferPair(cmd++, ra, 0);
            }
            aSetBuffer(cmd++, 0, t4 + DMEM_ADDR_WET_LEFT_CH, DMEM_ADDR_LEFT_CH, bufLen);
            aResample(cmd++, gSynthesisReverb.resampleFlags, (u16) gSynthesisReverb.resampleRate, VIRTUAL_TO_PHYSICAL2(gSynthesisReverb.resampleStateLeft));
            aSetBuffer(cmd++, 0, t4 + DMEM_ADDR_WET_RIGHT_CH, DMEM_ADDR_RIGHT_CH, bufLen);
            aResample(cmd++, gSynthesisReverb.resampleFlags, (u16) gSynthesisReverb.resampleRate, VIRTUAL_TO_PHYSICAL2(gSynthesisReverb.resampleStateRight));
#ifdef BETTER_REVERB
            // NOTE: Technically using an if/else here means using BETTER_REVERB vanilla presets with downsampling won't match 1-to-1 in volume with BETTER_REVERB being disabled.
            // This chunk is actually preferable to what vanilla uses, but was mainly ifdef'd here as a means of documenting BETTER_REVERB changes for other non-HackerSM64 repos.
            // Please use this chunk over the latter if matching BETTER_REVERB behavior ever becomes a future priority.
            aDMEMMove(cmd++, DMEM_ADDR_LEFT_CH, DMEM_ADDR_WET_LEFT_CH, DEFAULT_LEN_2CH);
            aSetBuffer(cmd++, 0, 0, 0, DEFAULT_LEN_2CH);
            aMix(cmd++, 0, /*gain*/ 0x8000 + gSynthesisReverb.reverbGain, /*in*/ DMEM_ADDR_WET_LEFT_CH, /*out*/ DMEM_ADDR_WET_LEFT_CH);
#else
            aSetBuffer(cmd++, 0, 0, 0, DEFAULT_LEN_2CH);
            aMix(cmd++, 0, /*gain*/ 0x8000 + gSynthesisReverb.reverbGain, /*in*/ DMEM_ADDR_LEFT_CH, /*out*/ DMEM_ADDR_LEFT_CH);
            aDMEMMove(cmd++, DMEM_ADDR_LEFT_CH, DMEM_ADDR_WET_LEFT_CH, DEFAULT_LEN_2CH);
#endif
        }

        AUDIO_PROFILER_SWITCH(PROFILER_TIME_SUB_AUDIO_SYNTHESIS_ENVELOPE_REVERB, PROFILER_TIME_SUB_AUDIO_SYNTHESIS_PROCESSING);
        cmd = synthesis_process_notes(aiBuf, bufLen, cmd);
        AUDIO_PROFILER_SWITCH(PROFILER_TIME_SUB_AUDIO_SYNTHESIS_PROCESSING, PROFILER_TIME_SUB_AUDIO_SYNTHESIS_ENVELOPE_REVERB);

        if (gReverbDownsampleRate == 1) {
            aSetSaveBufferPair(cmd++, 0, v1->lengthA, v1->startPos);
            if (v1->lengthB != 0) {
                // Ring buffer wrapped
                aSetSaveBufferPair(cmd++, v1->lengthA, v1->lengthB, 0);
            }
        } else {
            // Downsampling is done later by CPU when RSP is done, therefore we need to have double
            // buffering. Left and right buffers are adjacent in memory.
            aSetBuffer(cmd++, 0, 0, DMEM_ADDR_WET_LEFT_CH, DEFAULT_LEN_2CH);
            aSaveBuffer(cmd++, VIRTUAL_TO_PHYSICAL2(gSynthesisReverb.items[gSynthesisReverb.curFrame][updateIndex].toDownsampleLeft));
            gSynthesisReverb.resampleFlags = 0;
        }
    }
    return cmd;
}

u64 *synthesis_process_notes(s16 *aiBuf, u32 bufLen, u64 *cmd) {
    s32 noteIndex;                           // sp174
    struct Note *note;                       // s7
    struct AudioBankSample *audioBookSample; // sp164, sp138
    struct AdpcmLoop *loopInfo;              // sp160, sp134
    s16 *curLoadedBook = NULL;               // sp154, sp130
    s32 noteFinished;                        // 150 t2, sp124
    s32 restart;                             // 14c t3, sp120
    s32 flags;                               // sp148, sp11C
    s32 sp130 = 0;  //sp128, sp104
    s32 nAdpcmSamplesProcessed; // signed required for US
    s32 t0;
    u8 *sampleAddr;                          // sp120, spF4
    s32 s6;

    // Might have been used to store (samplesLenFixedPoint >> 16), but doing so causes strange
    // behavior with the break near the end of the loop, causing US and JP to need a goto instead
    s32 samplesLenAdjusted; // 108
    s32 s2;
    s32 endPos;             // sp110,    spE4
    s32 nSamplesToProcess;  // sp10c/a0, spE0

#ifdef ENABLE_STEREO_HEADSET_EFFECTS
    s32 leftRight;
#endif
    s32 s3;
    s32 s5; //s4

    u32 samplesLenFixedPoint;    // v1_1
    s32 nSamplesInThisIteration; // v1_2
    u32 a3;
    u8 *v0_2;
    s32 nParts;                 // spE8, spBC
    s32 curPart;                // spE4, spB8

    f32 resamplingRate; // f12
    s32 temp;

    s32 s5Aligned;
    s32 resampledTempLen;                    // spD8, spAC
    u16 noteSamplesDmemAddrBeforeResampling = 0; // spD6, spAA
    u16 resamplingRateFixedPoint;            // sp5c, sp11A

    switch (bufLen) {
        case (128 * 2):
            currentRampingTableLeft = gVolRampingLhs128;
            currentRampingTableRight = gVolRampingRhs128;
            break;
        case (144 * 2):
            currentRampingTableLeft = gVolRampingLhs144;
            currentRampingTableRight = gVolRampingRhs144;
            break;
        case (136 * 2):
        default:
            currentRampingTableLeft = gVolRampingLhs136;
            currentRampingTableRight = gVolRampingRhs136;
            break;
    }

    for (noteIndex = 0; noteIndex < gMaxSimultaneousNotes; noteIndex++) {
        note = &gNotes[noteIndex];
        //! This function requires note->enabled to be volatile, but it breaks other functions like note_enable.
        //! Casting to a struct with just the volatile bitfield works, but there may be a better way to match.
        if (((struct vNote *)note)->enabled && !IS_BANK_LOAD_COMPLETE(note->bankId)) {
            gAudioErrorFlags = (note->bankId << 8) + noteIndex + 0x1000000;
        } else if (((struct vNote *)note)->enabled) {
            flags = 0;

            if (note->needsInit == TRUE) {
                flags = A_INIT;
                note->samplePosInt = 0;
                note->samplePosFrac = 0;
            }

            if (note->frequency < 2.0f) {
                nParts = 1;
                if (note->frequency > 1.99996f) {
                    note->frequency = 1.99996f;
                }
                resamplingRate = note->frequency;
            } else {
                // If frequency is > 2.0, the processing must be split into two parts
                nParts = 2;
                if (note->frequency >= 3.99993f) {
                    note->frequency = 3.99993f;
                }
                resamplingRate = note->frequency * 0.5f;
            }

            resamplingRateFixedPoint = (u16)(s32)(resamplingRate * 32768.0f);
            samplesLenFixedPoint = note->samplePosFrac + (resamplingRateFixedPoint * bufLen);
            note->samplePosFrac = samplesLenFixedPoint & 0xFFFF; // 16-bit store, can't reuse

            if (note->sound == NULL) {
                // A wave synthesis note (not ADPCM)

                cmd = load_wave_samples(cmd, note, samplesLenFixedPoint >> 16);
                noteSamplesDmemAddrBeforeResampling = DMEM_ADDR_UNCOMPRESSED_NOTE + note->samplePosInt * 2;
                note->samplePosInt += (samplesLenFixedPoint >> 16);
                flags = 0;
            }
            else {
                // ADPCM note
                audioBookSample = note->sound->sample;

                loopInfo = audioBookSample->loop;
                endPos = loopInfo->end;
                sampleAddr = audioBookSample->sampleAddr;
                resampledTempLen = 0;
                for (curPart = 0; curPart < nParts; curPart++) {
                    nAdpcmSamplesProcessed = 0; // s8
                    s5 = 0;                     // s4

                    if (nParts == 1) {
                        samplesLenAdjusted = samplesLenFixedPoint >> 16;
                    } else if ((samplesLenFixedPoint >> 16) & 1) {
                        samplesLenAdjusted = ((samplesLenFixedPoint >> 16) & ~1) + (curPart * 2);
                    }
                    else {
                        samplesLenAdjusted = (samplesLenFixedPoint >> 16);
                    }

                    if (curLoadedBook != audioBookSample->book->book) {
                        u32 nEntries; // v1
                        curLoadedBook = audioBookSample->book->book;
                        nEntries = audioBookSample->book->order * audioBookSample->book->npredictors * 16U;
                        aLoadADPCM(cmd++, nEntries, VIRTUAL_TO_PHYSICAL2(curLoadedBook));
                    }

                    while (nAdpcmSamplesProcessed != samplesLenAdjusted) {
                        s32 samplesRemaining; // v1
                        s32 s0;

                        noteFinished = FALSE;
                        restart = FALSE;
                        nSamplesToProcess = samplesLenAdjusted - nAdpcmSamplesProcessed;
                        s2 = note->samplePosInt & 0xf;
                        samplesRemaining = endPos - note->samplePosInt;

                        if (s2 == 0 && note->restart == FALSE) {
                            s2 = 16;
                        }

                        s6 = 16 - s2; // a1

                        if (nSamplesToProcess < samplesRemaining) {
                            t0 = (nSamplesToProcess - s6 + 0xf) / 16;
                            s0 = t0 * 16;
                            s3 = s6 + s0 - nSamplesToProcess;
                        } else {
                            s0 = samplesRemaining - s6;
                            s3 = 0;
                            if (s0 <= 0) {
                                s0 = 0;
                                s6 = samplesRemaining;
                            }
                            t0 = (s0 + 0xf) / 16;
                            if (loopInfo->count != 0) {
                                // Loop around and restart
                                restart = 1;
                            } else {
                                noteFinished = 1;
                            }
                        }

                        if (t0 != 0) {
                            temp = (note->samplePosInt - s2 + 16) / 16;
            
                            AUDIO_PROFILER_SWITCH(PROFILER_TIME_SUB_AUDIO_SYNTHESIS_PROCESSING, PROFILER_TIME_SUB_AUDIO_SYNTHESIS_DMA);

                            v0_2 = dma_sample_data(
                                (uintptr_t) (sampleAddr + temp * 9),
                                t0 * 9, flags, &note->sampleDmaIndex);

                            AUDIO_PROFILER_SWITCH(PROFILER_TIME_SUB_AUDIO_SYNTHESIS_DMA, PROFILER_TIME_SUB_AUDIO_SYNTHESIS_PROCESSING);

                            a3 = (u32)((uintptr_t) v0_2 & 0xf);
                            aSetBuffer(cmd++, 0, DMEM_ADDR_COMPRESSED_ADPCM_DATA, 0, t0 * 9 + a3);
                            aLoadBuffer(cmd++, VIRTUAL_TO_PHYSICAL2(v0_2 - a3));
                        } else {
                            s0 = 0;
                            a3 = 0;
                        }

                        if (note->restart != FALSE) {
                            aSetLoop(cmd++, VIRTUAL_TO_PHYSICAL2(audioBookSample->loop->state));
                            flags = A_LOOP; // = 2
                            note->restart = FALSE;
                        }

                        nSamplesInThisIteration = s0 + s6 - s3;
                        if (nAdpcmSamplesProcessed == 0) {
                            aSetBuffer(cmd++, 0, DMEM_ADDR_COMPRESSED_ADPCM_DATA + a3, DMEM_ADDR_UNCOMPRESSED_NOTE, s0 * 2);
                            aADPCMdec(cmd++, flags, VIRTUAL_TO_PHYSICAL2(note->synthesisBuffers->adpcmdecState));
                            sp130 = s2 * 2;
                        } else {
                            s5Aligned = ALIGN32(s5);
                            aSetBuffer(cmd++, 0, DMEM_ADDR_COMPRESSED_ADPCM_DATA + a3, DMEM_ADDR_UNCOMPRESSED_NOTE + s5Aligned, s0 * 2);
                            aADPCMdec(cmd++, flags, VIRTUAL_TO_PHYSICAL2(note->synthesisBuffers->adpcmdecState));
                            aDMEMMove(cmd++, DMEM_ADDR_UNCOMPRESSED_NOTE + s5Aligned + (s2 * 2), DMEM_ADDR_UNCOMPRESSED_NOTE + s5, (nSamplesInThisIteration) * 2);
                        }

                        nAdpcmSamplesProcessed += nSamplesInThisIteration;

                        switch (flags) {
                            case A_INIT: // = 1
                                /**
                                 * !NOTE: Removing this seems to produce a more accurate waveform, however I have no idea why Nintendo decided to add this originally.
                                 * I can only speculate (and hope) that this was just an oversight on their part and this has no reason to exist, given my testing.
                                 * I'm leaving it commented out here just in case though.
                                 */
                                // sp130 = 0;
                                s5 = s0 * 2 + s5;
                                break;

                            case A_LOOP: // = 2
                                s5 = nSamplesInThisIteration * 2 + s5;
                                break;

                            default:
                                if (s5 != 0) {
                                    s5 = nSamplesInThisIteration * 2 + s5;
                                } else {
                                    s5 = (s2 + nSamplesInThisIteration) * 2;
                                }
                                break;
                        }
                        flags = 0;

                        if (noteFinished) {
                            aClearBuffer(cmd++, DMEM_ADDR_UNCOMPRESSED_NOTE + s5,
                                         (samplesLenAdjusted - nAdpcmSamplesProcessed) * 2);
                            note->samplePosInt = 0;
                            note->finished = TRUE;
                            ((struct vNote *)note)->enabled = 0;
                            break;
                        }

                        if (restart) {
                            note->restart = TRUE;
                            note->samplePosInt = loopInfo->start;
                        } else {
                            note->samplePosInt += nSamplesToProcess;
                        }
                    }

                    switch (nParts) {
                        case 1:
                            noteSamplesDmemAddrBeforeResampling = DMEM_ADDR_UNCOMPRESSED_NOTE + sp130;
                            break;

                        case 2:
                            switch (curPart) {
                                case 0:
                                    aSetBuffer(cmd++, 0, DMEM_ADDR_UNCOMPRESSED_NOTE + sp130, DMEM_ADDR_RESAMPLED, samplesLenAdjusted + 4);
                                    aResample(cmd++, A_INIT, 0xff60, VIRTUAL_TO_PHYSICAL2(note->synthesisBuffers->dummyResampleState));
                                    resampledTempLen = samplesLenAdjusted + 4;
                                    noteSamplesDmemAddrBeforeResampling = DMEM_ADDR_RESAMPLED + 4;
                                    if (note->finished) {
                                        aClearBuffer(cmd++, DMEM_ADDR_RESAMPLED + resampledTempLen, samplesLenAdjusted + 16);
                                    }
                                    break;

                                case 1:
                                    aSetBuffer(cmd++, 0, DMEM_ADDR_UNCOMPRESSED_NOTE + sp130,
                                               DMEM_ADDR_RESAMPLED2,
                                               samplesLenAdjusted + 8);
                                    aResample(cmd++, A_INIT, 0xff60,
                                              VIRTUAL_TO_PHYSICAL2(
                                                  note->synthesisBuffers->dummyResampleState));
                                    aDMEMMove(cmd++, DMEM_ADDR_RESAMPLED2 + 4,
                                              DMEM_ADDR_RESAMPLED + resampledTempLen,
                                              samplesLenAdjusted + 4);
                                    break;
                            }
                    }

                    if (note->finished) {
                        break;
                    }
                }
            }

            flags = 0;
            if (note->needsInit == TRUE) {
                flags = A_INIT;
                note->needsInit = FALSE;
            }

            // final resample
            aSetBuffer(cmd++, /*flags*/ 0, noteSamplesDmemAddrBeforeResampling, /*dmemout*/ DMEM_ADDR_TEMP, bufLen);
            aResample(cmd++, flags, resamplingRateFixedPoint, VIRTUAL_TO_PHYSICAL2(note->synthesisBuffers->finalResampleState));

#ifdef ENABLE_STEREO_HEADSET_EFFECTS
            if (note->headsetPanRight != 0 || note->prevHeadsetPanRight != 0) {
                leftRight = 1;
            } else if (note->headsetPanLeft != 0 || note->prevHeadsetPanLeft != 0) {
                leftRight = 2;
            } else {
                leftRight = 0;
            }

            AUDIO_PROFILER_SWITCH(PROFILER_TIME_SUB_AUDIO_SYNTHESIS_PROCESSING, PROFILER_TIME_SUB_AUDIO_SYNTHESIS_ENVELOPE_REVERB);
            cmd = process_envelope(cmd, note, bufLen, 0, leftRight);
            AUDIO_PROFILER_SWITCH(PROFILER_TIME_SUB_AUDIO_SYNTHESIS_ENVELOPE_REVERB, PROFILER_TIME_SUB_AUDIO_SYNTHESIS_PROCESSING);

            if (note->usesHeadsetPanEffects) {
                cmd = note_apply_headset_pan_effects(cmd, note, bufLen, flags, leftRight);
            }
#else
            AUDIO_PROFILER_SWITCH(PROFILER_TIME_SUB_AUDIO_SYNTHESIS_PROCESSING, PROFILER_TIME_SUB_AUDIO_SYNTHESIS_ENVELOPE_REVERB);
            cmd = process_envelope(cmd, note, bufLen, 0);
            AUDIO_PROFILER_SWITCH(PROFILER_TIME_SUB_AUDIO_SYNTHESIS_ENVELOPE_REVERB, PROFILER_TIME_SUB_AUDIO_SYNTHESIS_PROCESSING);
#endif
        }
    }

    aSetBuffer(cmd++, 0, 0, DMEM_ADDR_TEMP, bufLen);
    aInterleave(cmd++, DMEM_ADDR_LEFT_CH, DMEM_ADDR_RIGHT_CH);
    aSetBuffer(cmd++, 0, 0, DMEM_ADDR_TEMP, bufLen * 2);
    aSaveBuffer(cmd++, VIRTUAL_TO_PHYSICAL2(aiBuf));

    return cmd;
}

u64 *load_wave_samples(u64 *cmd, struct Note *note, s32 nSamplesToLoad) {
    s32 a3;
    s32 repeats;
    s32 i;
    aSetBuffer(cmd++, /*flags*/ 0, /*dmemin*/ DMEM_ADDR_UNCOMPRESSED_NOTE, /*dmemout*/ 0,
               /*count*/ sizeof(note->synthesisBuffers->samples));
    aLoadBuffer(cmd++, VIRTUAL_TO_PHYSICAL2(note->synthesisBuffers->samples));

    note->samplePosInt &= (note->sampleCount - 1);
    a3 = 64 - note->samplePosInt;
    if (a3 < nSamplesToLoad) {
        repeats = (nSamplesToLoad - a3 + 63) / 64;
        for (i = 0; i < repeats; i++) {
            aDMEMMove(cmd++,
                      /*dmemin*/ DMEM_ADDR_UNCOMPRESSED_NOTE,
                      /*dmemout*/ DMEM_ADDR_UNCOMPRESSED_NOTE + (1 + i) * sizeof(note->synthesisBuffers->samples),
                      /*count*/ sizeof(note->synthesisBuffers->samples));
        }
    }
    return cmd;
}

#ifdef ENABLE_STEREO_HEADSET_EFFECTS
u64 *process_envelope(u64 *cmd, struct Note *note, s32 nSamples, u16 inBuf, s32 headsetPanSettings) {
#else
u64 *process_envelope(u64 *cmd, struct Note *note, s32 nSamples, u16 inBuf) {
#endif
    u8 mixerFlags;
    s32 rampLeft;
    s32 rampRight;
    struct VolumeChange vol;

    if (note->initFullVelocity) {
        note->initFullVelocity = FALSE;
        vol.sourceLeft = note->targetVolLeft;
        vol.sourceRight = note->targetVolRight;
    } else {
        vol.sourceLeft = note->curVolLeft;
        vol.sourceRight = note->curVolRight;
    }
    vol.targetLeft = note->targetVolLeft;
    vol.targetRight = note->targetVolRight;
    note->curVolLeft = vol.targetLeft;
    note->curVolRight = vol.targetRight;

    // For aEnvMixer, five buffers and count are set using aSetBuffer.
    // in, dry left, count without A_AUX flag.
    // dry right, wet left, wet right with A_AUX flag.

#ifdef ENABLE_STEREO_HEADSET_EFFECTS
    if (note->usesHeadsetPanEffects) {
        aClearBuffer(cmd++, DMEM_ADDR_NOTE_PAN_TEMP, DEFAULT_LEN_1CH);

        switch (headsetPanSettings) {
            case 1:
                aSetBuffer(cmd++, 0, inBuf, DMEM_ADDR_NOTE_PAN_TEMP, nSamples);
                aSetBuffer(cmd++, A_AUX, DMEM_ADDR_RIGHT_CH, DMEM_ADDR_WET_LEFT_CH,
                           DMEM_ADDR_WET_RIGHT_CH);
                break;
            case 2:
                aSetBuffer(cmd++, 0, inBuf, DMEM_ADDR_LEFT_CH, nSamples);
                aSetBuffer(cmd++, A_AUX, DMEM_ADDR_NOTE_PAN_TEMP, DMEM_ADDR_WET_LEFT_CH,
                           DMEM_ADDR_WET_RIGHT_CH);
                break;
            default:
                aSetBuffer(cmd++, 0, inBuf, DMEM_ADDR_LEFT_CH, nSamples);
                aSetBuffer(cmd++, A_AUX, DMEM_ADDR_RIGHT_CH, DMEM_ADDR_WET_LEFT_CH,
                           DMEM_ADDR_WET_RIGHT_CH);
                break;
        }
    } else {
        // It's a bit unclear what the "stereo strong" concept does.
        // Instead of mixing the opposite channel to the normal buffers, the sound is first
        // mixed into a temporary buffer and then subtracted from the normal buffer.
        if (note->stereoStrongRight) {
            aClearBuffer(cmd++, DMEM_ADDR_STEREO_STRONG_TEMP_DRY, DEFAULT_LEN_2CH);
            aSetBuffer(cmd++, 0, inBuf, DMEM_ADDR_STEREO_STRONG_TEMP_DRY, nSamples);
            aSetBuffer(cmd++, A_AUX, DMEM_ADDR_RIGHT_CH, DMEM_ADDR_STEREO_STRONG_TEMP_WET,
                       DMEM_ADDR_WET_RIGHT_CH);
        } else if (note->stereoStrongLeft) {
            aClearBuffer(cmd++, DMEM_ADDR_STEREO_STRONG_TEMP_DRY, DEFAULT_LEN_2CH);
            aSetBuffer(cmd++, 0, inBuf, DMEM_ADDR_LEFT_CH, nSamples);
            aSetBuffer(cmd++, A_AUX, DMEM_ADDR_STEREO_STRONG_TEMP_DRY, DMEM_ADDR_WET_LEFT_CH,
                       DMEM_ADDR_STEREO_STRONG_TEMP_WET);
        } else {
            aSetBuffer(cmd++, 0, inBuf, DMEM_ADDR_LEFT_CH, nSamples);
            aSetBuffer(cmd++, A_AUX, DMEM_ADDR_RIGHT_CH, DMEM_ADDR_WET_LEFT_CH, DMEM_ADDR_WET_RIGHT_CH);
        }
    }
#else
    aSetBuffer(cmd++, 0, inBuf, DMEM_ADDR_LEFT_CH, nSamples);
    aSetBuffer(cmd++, A_AUX, DMEM_ADDR_RIGHT_CH, DMEM_ADDR_WET_LEFT_CH, DMEM_ADDR_WET_RIGHT_CH);
#endif

    if (vol.targetLeft == vol.sourceLeft && vol.targetRight == vol.sourceRight
        && !note->envMixerNeedsInit) {
        mixerFlags = A_CONTINUE;
    } else {
        mixerFlags = A_INIT;

        // volume ramping
        // This roughly computes 2^16 * (targetVol / sourceVol) ^ (8 / arg2),
        // but with discretizations of targetVol, sourceVol and arg2.
        rampLeft = currentRampingTableLeft[vol.targetLeft >> (15 - VOL_RAMPING_EXPONENT)] * currentRampingTableRight[vol.sourceLeft >> (15 - VOL_RAMPING_EXPONENT)];
        rampRight = currentRampingTableLeft[vol.targetRight >> (15 - VOL_RAMPING_EXPONENT)] * currentRampingTableRight[vol.sourceRight >> (15 - VOL_RAMPING_EXPONENT)];

        // The operation's parameters change meanings depending on flags
        aSetVolume(cmd++, A_VOL | A_LEFT, vol.sourceLeft, 0, 0);
        aSetVolume(cmd++, A_VOL | A_RIGHT, vol.sourceRight, 0, 0);
        aSetVolume32(cmd++, A_RATE | A_LEFT, vol.targetLeft, rampLeft);
        aSetVolume32(cmd++, A_RATE | A_RIGHT, vol.targetRight, rampRight);
        aSetVolume(cmd++, A_AUX, gVolume, 0, note->reverbVol << 8);
    }

#ifdef ENABLE_STEREO_HEADSET_EFFECTS
    if (gSynthesisReverb.useReverb && note->reverbVol != 0) {
        aEnvMixer(cmd++, mixerFlags | A_AUX,
                  VIRTUAL_TO_PHYSICAL2(note->synthesisBuffers->mixEnvelopeState));
        if (note->stereoStrongRight) {
            aSetBuffer(cmd++, 0, 0, 0, nSamples);
            // 0x8000 is -100%, so subtract sound instead of adding...
            aMix(cmd++, 0, /*gain*/ 0x8000, /*in*/ DMEM_ADDR_STEREO_STRONG_TEMP_DRY, /*out*/ DMEM_ADDR_LEFT_CH);
            aMix(cmd++, 0, /*gain*/ 0x8000, /*in*/ DMEM_ADDR_STEREO_STRONG_TEMP_WET, /*out*/ DMEM_ADDR_WET_LEFT_CH);
        } else if (note->stereoStrongLeft) {
            aSetBuffer(cmd++, 0, 0, 0, nSamples);
            aMix(cmd++, 0, /*gain*/ 0x8000, /*in*/ DMEM_ADDR_STEREO_STRONG_TEMP_DRY, /*out*/ DMEM_ADDR_RIGHT_CH);
            aMix(cmd++, 0, /*gain*/ 0x8000, /*in*/ DMEM_ADDR_STEREO_STRONG_TEMP_WET, /*out*/ DMEM_ADDR_WET_RIGHT_CH);
        }
    } else {
        aEnvMixer(cmd++, mixerFlags, VIRTUAL_TO_PHYSICAL2(note->synthesisBuffers->mixEnvelopeState));
        if (note->stereoStrongRight) {
            aSetBuffer(cmd++, 0, 0, 0, nSamples);
            aMix(cmd++, 0, /*gain*/ 0x8000, /*in*/ DMEM_ADDR_STEREO_STRONG_TEMP_DRY,
                 /*out*/ DMEM_ADDR_LEFT_CH);
        } else if (note->stereoStrongLeft) {
            aSetBuffer(cmd++, 0, 0, 0, nSamples);
            aMix(cmd++, 0, /*gain*/ 0x8000, /*in*/ DMEM_ADDR_STEREO_STRONG_TEMP_DRY,
                 /*out*/ DMEM_ADDR_RIGHT_CH);
        }
    }
#else
    if (gSynthesisReverb.useReverb && note->reverbVol != 0) {
        mixerFlags |= A_AUX;
    }
    aEnvMixer(cmd++, mixerFlags, VIRTUAL_TO_PHYSICAL2(note->synthesisBuffers->mixEnvelopeState));
#endif
    return cmd;
}

#ifdef ENABLE_STEREO_HEADSET_EFFECTS
u64 *note_apply_headset_pan_effects(u64 *cmd, struct Note *note, s32 bufLen, s32 flags, s32 leftRight) {
    u16 dest;
    u16 pitch;
    u16 prevPanShift;
    u16 panShift;

    switch (leftRight) {
        case 1:
            dest = DMEM_ADDR_LEFT_CH;
            panShift = note->headsetPanRight;
            note->prevHeadsetPanLeft = 0;
            prevPanShift = note->prevHeadsetPanRight;
            note->prevHeadsetPanRight = panShift;
            break;
        case 2:
            dest = DMEM_ADDR_RIGHT_CH;
            panShift = note->headsetPanLeft;
            note->prevHeadsetPanRight = 0;

            prevPanShift = note->prevHeadsetPanLeft;
            note->prevHeadsetPanLeft = panShift;
            break;
        default:
            return cmd;
    }

    if (flags != 1) { // A_INIT?
        // Slightly adjust the sample rate in order to fit a change in pan shift
        if (prevPanShift == 0) {
            // Kind of a hack that moves the first samples into the resample state
            aDMEMMove(cmd++, DMEM_ADDR_NOTE_PAN_TEMP, DMEM_ADDR_TEMP, 8);
            aClearBuffer(cmd++, 8, 8); // Set pitch accumulator to 0 in the resample state
            aDMEMMove(cmd++, DMEM_ADDR_NOTE_PAN_TEMP, DMEM_ADDR_TEMP + 16,
                      16); // No idea, result seems to be overwritten later

            aSetBuffer(cmd++, 0, 0, DMEM_ADDR_TEMP, 32);
            aSaveBuffer(cmd++, VIRTUAL_TO_PHYSICAL2(note->synthesisBuffers->panResampleState));

            pitch = (bufLen << 0xf) / (bufLen + panShift - prevPanShift + 8);
            aSetBuffer(cmd++, 0, DMEM_ADDR_NOTE_PAN_TEMP + 8, DMEM_ADDR_TEMP, panShift + bufLen - prevPanShift);
            aResample(cmd++, 0, pitch, VIRTUAL_TO_PHYSICAL2(note->synthesisBuffers->panResampleState));
        } else {
            if (panShift == 0) {
                pitch = (bufLen << 0xf) / (bufLen - prevPanShift - 4);
            } else {
                pitch = (bufLen << 0xf) / (bufLen + panShift - prevPanShift);
            }

            aSetBuffer(cmd++, 0, DMEM_ADDR_NOTE_PAN_TEMP, DMEM_ADDR_TEMP, panShift + bufLen - prevPanShift);
            aResample(cmd++, 0, pitch, VIRTUAL_TO_PHYSICAL2(note->synthesisBuffers->panResampleState));
        }

        if (prevPanShift != 0) {
            aSetBuffer(cmd++, 0, DMEM_ADDR_NOTE_PAN_TEMP, 0, prevPanShift);
            aLoadBuffer(cmd++, VIRTUAL_TO_PHYSICAL2(note->synthesisBuffers->panSamplesBuffer));
            aDMEMMove(cmd++, DMEM_ADDR_TEMP, DMEM_ADDR_NOTE_PAN_TEMP + prevPanShift, panShift + bufLen - prevPanShift);
        } else {
            aDMEMMove(cmd++, DMEM_ADDR_TEMP, DMEM_ADDR_NOTE_PAN_TEMP, panShift + bufLen - prevPanShift);
        }
    } else {
        // Just shift right
        aDMEMMove(cmd++, DMEM_ADDR_NOTE_PAN_TEMP, DMEM_ADDR_TEMP, bufLen);
        aDMEMMove(cmd++, DMEM_ADDR_TEMP, DMEM_ADDR_NOTE_PAN_TEMP + panShift, bufLen);
        aClearBuffer(cmd++, DMEM_ADDR_NOTE_PAN_TEMP, panShift);
    }

    if (panShift) {
        // Save excessive samples for next iteration
        aSetBuffer(cmd++, 0, 0, DMEM_ADDR_NOTE_PAN_TEMP + bufLen, panShift);
        aSaveBuffer(cmd++, VIRTUAL_TO_PHYSICAL2(note->synthesisBuffers->panSamplesBuffer));
    }

    aSetBuffer(cmd++, 0, 0, 0, bufLen);
    aMix(cmd++, 0, /*gain*/ 0x7fff, /*in*/ DMEM_ADDR_NOTE_PAN_TEMP, /*out*/ dest);

    return cmd;
}
#endif

void note_init_volume(struct Note *note) {
    note->targetVolLeft = 0;
    note->targetVolRight = 0;
    note->reverbVol = 0;
    note->curVolLeft = 1;
    note->curVolRight = 1;
    note->frequency = 0.0f;
}

void note_set_vel_pan_reverb(struct Note *note, f32 velocity, f32 pan, u8 reverbVol) {
    f32 volLeft, volRight;
    s32 panIndex = (s32)(pan * 127.5f) & 127;

    if (gSoundMode == SOUND_MODE_MONO) {
        volLeft = 0.707f;
        volRight = 0.707f;
#ifdef ENABLE_STEREO_HEADSET_EFFECTS
    } else if (note->stereoHeadsetEffects && gSoundMode == SOUND_MODE_HEADSET) {
        s8 smallPanIndex;
        s8 temp = (s8)(pan * 10.0f);
        if (temp < 9) {
            smallPanIndex = temp;
        } else {
            smallPanIndex = 9;
        }
        note->headsetPanLeft = gHeadsetPanQuantization[smallPanIndex];
        note->headsetPanRight = gHeadsetPanQuantization[9 - smallPanIndex];
        note->stereoStrongRight = FALSE;
        note->stereoStrongLeft = FALSE;
        note->usesHeadsetPanEffects = TRUE;
        volLeft = gHeadsetPanVolume[panIndex];
        volRight = gHeadsetPanVolume[127 - panIndex];
    } else if (note->stereoHeadsetEffects && gSoundMode == SOUND_MODE_STEREO) {
        u8 strongLeft = FALSE;
        u8 strongRight = FALSE;
        note->headsetPanLeft = 0;
        note->headsetPanRight = 0;
        note->usesHeadsetPanEffects = FALSE;
        volLeft = gStereoPanVolume[panIndex];
        volRight = gStereoPanVolume[127 - panIndex];
        if (panIndex < 0x20) {
            strongLeft = TRUE;
        } else if (panIndex > 0x60) {
            strongRight = TRUE;
        }
        note->stereoStrongRight = strongRight;
        note->stereoStrongLeft = strongLeft;
#endif
    } else {
        volLeft = gDefaultPanVolume[panIndex];
        volRight = gDefaultPanVolume[127 - panIndex];
    }

    if (velocity < 0) {
        velocity = 0;
    }
    note->targetVolLeft = (u16)(s32)(velocity * volLeft) & VOLRAMPING_MASK;
    note->targetVolRight = (u16)(s32)(velocity * volRight) & VOLRAMPING_MASK;
    if (note->targetVolLeft == 0) {
        note->targetVolLeft++;
    }
    if (note->targetVolRight == 0) {
        note->targetVolRight++;
    }
    if (note->reverbVol != reverbVol) {
        note->reverbVol = reverbVol;
        note->envMixerNeedsInit = TRUE;
        return;
    }

    note->envMixerNeedsInit = note->needsInit;
}

void note_set_frequency(struct Note *note, f32 frequency) {
    note->frequency = frequency;
}

void note_enable(struct Note *note) {
    note->enabled = TRUE;
    note->needsInit = TRUE;
    note->restart = FALSE;
    note->finished = FALSE;
#ifdef ENABLE_STEREO_HEADSET_EFFECTS
    note->stereoStrongRight = FALSE;
    note->stereoStrongLeft = FALSE;
    note->usesHeadsetPanEffects = FALSE;
    note->initFullVelocity = FALSE;
    note->headsetPanLeft = 0;
    note->headsetPanRight = 0;
    note->prevHeadsetPanRight = 0;
    note->prevHeadsetPanLeft = 0;
#endif
}

void note_disable(struct Note *note) {
    if (note->needsInit) {
        note->needsInit = FALSE;
    } else {
        note_set_vel_pan_reverb(note, 0, 0.5f, 0);
    }
    note->priority = NOTE_PRIORITY_DISABLED;
    note->enabled = FALSE;
    note->finished = FALSE;
    note->parentLayer = NO_LAYER;
    note->prevParentLayer = NO_LAYER;
}
#endif
