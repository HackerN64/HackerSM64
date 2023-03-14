#include <ultra64.h>
#include <hvqm2dec.h>

#include "buffers.h"
#include "audio/data.h"
#ifdef HVQM
#include <hvqm/hvqm.h>
#endif
#include "config.h"
#include "audio/synthesis.h"

ALIGNED8 u8 gDecompressionHeap[0xD000];
ALIGNED16 u8 gAudioHeap[DOUBLE_SIZE_ON_64_BIT(AUDIO_HEAP_SIZE)];

ALIGNED8 u8 gIdleThreadStack[THREAD1_STACK];
ALIGNED8 u8 gThread3Stack[THREAD3_STACK];
ALIGNED8 u8 gThread4Stack[THREAD4_STACK];
ALIGNED8 u8 gThread5Stack[THREAD5_STACK];
#if ENABLE_RUMBLE
ALIGNED8 u8 gThread6Stack[THREAD6_STACK];
#endif
// 0x400 bytes
#if UNF
ALIGNED16 u8 gGfxSPTaskStack[SP_DRAM_STACK_SIZE8];
ALIGNED16 u8 gGfxSPTaskYieldBuffer[OS_YIELD_DATA_SIZE];
#else
// 0xc00 bytes for f3dex, 0x900 otherwise
ALIGNED8 u8 gGfxSPTaskStack[SP_DRAM_STACK_SIZE8];
ALIGNED8 u8 gGfxSPTaskYieldBuffer[OS_YIELD_DATA_SIZE];
#endif // UNF
// 0x200 bytes
ALIGNED8 struct SaveBuffer gSaveBuffer;
// 0x190a0 bytes
struct GfxPool gGfxPools[2];
