#include "emutest.h"

#include <PR/os_internal_reg.h>
#include <PR/os_internal_si.h>
#include <PR/os_message.h>
#include <PR/os_pi.h>
#include <PR/R4300.h>
#include <ultra64.h>
#include <string.h>
#include "vc_check.h"
#include "types.h"

extern u8 gCacheEmulated;
extern u8 gIsConsole;

extern OSMesgQueue gSIEventMesgQueue;
extern u8 __osContPifRam[];
extern u8 __osContLastCmd;
extern void __osSiGetAccess(void);
extern void __osSiRelAccess(void);

enum Emulator gEmulator = 0;

__attribute__((aligned(8)))
static const u32 check_count_factor[] = {
    0x40084800u, // MFC0 T0, COUNT
    0x40094800u, // MFC0 T1, COUNT
    0x03E00008u, // JR RA
    0x01281023u  // SUBU V0, T1, T0
};

static inline enum Emulator get_pj64_version() {
    // PJ64 4.0 dynarec core screws up the COUNT register.
    // The 4.0 interpreter core is already handled and never calls into this function
    if (((u32 (*)(void))check_count_factor)() == 0) {
        return EMU_PROJECT64_4;
    }

    // PJ64 1.6 test
    __osSiGetAccess();
    u32 *pifRam32 = (u32*)__osContPifRam;
    for (s32 i = 0; i < 15; i++) pifRam32[i] = 0;
    pifRam32[15] = 2;

    const u8 cicTest[] = {
        0x0F, 0x0F,
        0xEC, 0x3C, 0xB6, 0x76, 0xB8, 0x1D, 0xBB, 0x8F,
        0x6B, 0x3A, 0x80, 0xEC, 0xED, 0xEA, 0x5B
    };

    memcpy(&__osContPifRam[46], cicTest, 17);

    __osSiRawStartDma(OS_WRITE, __osContPifRam);
    osRecvMesg(&gSIEventMesgQueue, NULL, OS_MESG_BLOCK);
    __osContLastCmd = 254;

    __osSiRawStartDma(OS_READ, __osContPifRam);
    osRecvMesg(&gSIEventMesgQueue, NULL, OS_MESG_BLOCK);
    const u8 pifCheck = __osContPifRam[54];
    __osSiRelAccess();

    return (pifCheck == 0xB0) ? EMU_PROJECT64_1_OR_2 : EMU_PROJECT64_3;
}

static void check_cache_emulation() {
    // Disable interrupts to ensure that nothing evicts the variable from cache while we're using it.
    u32 saved = __osDisableInt();
    // Create a variable with an initial value of 1. This value will remain cached.
    volatile u8 sCachedValue = 1;
    // Overwrite the variable directly in RDRAM without going through cache.
    // This should preserve its value of 1 in dcache if dcache is emulated correctly.
    *(u8*)(K0_TO_K1(&sCachedValue)) = 0;
    // Read the variable back from dcache, if it's still 1 then cache is emulated correctly.
    // If it's zero, then dcache is not emulated correctly.
    gCacheEmulated = sCachedValue;
    // Restore interrupts
    __osRestoreInt(saved);
}

void detect_emulator() {
    if (IO_READ(DPC_CLOCK_REG) != 0) {
        gIsConsole = TRUE;
        gIsVC = FALSE;
        gEmulator = EMU_CONSOLE;
        return;
    }

    if (IS_VC()) {
        gIsVC = TRUE;
        gCacheEmulated = FALSE;
        gEmulator = EMU_WIIVC;
        return;
    }

    gIsVC = FALSE;
    check_cache_emulation();

    const u32 magic = *((volatile u32*)0xbfd00104u);
    if (magic == 0u) {
        gEmulator = EMU_MUPEN_OLD;
        return;
    } else if (magic != 0x01040104u) {
        gEmulator = EMU_CEN64;
        return;
    }

    switch (*((volatile u16*)0xbfd00106u)) {
        case 0x0106: {
            if (gCacheEmulated) {
                gEmulator = EMU_ARES;
                return;
            }

            osPiWriteIo(0x1FFB0000u, 0u);
            if (*((volatile u32*)0xbffb0000u) == 0x00500000u) {
                gEmulator = EMU_PARALLELN64;
                return;
            }

            gEmulator = EMU_PROJECT64_4; // interpreter core specifically
            return;
        }
        case 0x0104:
            gEmulator = gCacheEmulated ? EMU_SIMPLE64 : EMU_MUPEN64PLUS_NEXT;
            return;
        case 0x0000:
            gEmulator = get_pj64_version();
            return;
        default:
            gEmulator = 0; // Unknown. No tested emulator ever gets here
            return;
    }
} 
