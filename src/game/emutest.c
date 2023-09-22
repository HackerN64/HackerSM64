#include "emutest.h"

#include <PR/os_internal_reg.h>
#include <PR/os_internal_si.h>
#include <PR/os_message.h>
#include <PR/os_pi.h>
#include <PR/R4300.h>
#include <ultra64.h>
#include <string.h>
#include "emutest_vc.h"
#include "float.h"
#include "types.h"

extern OSMesgQueue gSIEventMesgQueue;
extern u8 __osContPifRam[];
extern u8 __osContLastCmd;
extern void __osSiGetAccess(void);
extern void __osSiRelAccess(void);
extern void __osPiGetAccess(void);
extern void __osPiRelAccess(void);

enum Emulator gEmulator = EMU_CONSOLE;

u32 pj64_get_count_factor_asm(void); // defined in asm/pj64_get_count_factor_asm.s
u32 emux_detect(void); // defined in asm/emux.s

static inline u32 check_count_factor() {
    const u32 saved = __osDisableInt();
    const u32 cf = pj64_get_count_factor_asm();
    __osRestoreInt(saved);
    return cf;
}

static inline enum Emulator get_pj64_version() {
    // When calling this function, we know that the emulator is some version of Project 64,
    // and it isn't using the PJ64 4.0 interpreter core. Figure out which version it is.
    
    // PJ64 4.0 dynarec core doesn't update the COUNT register correctly within recompiled functions
    if (check_count_factor() == 0) {
        return EMU_PROJECT64_4;
    }

    // Instead of implementing this PIF command correctly, PJ64 versions prior to 3.0 just have
    // a set of hardcoded values for some requests. At least one of these hardcoded values has a
    // typo in it, making it give an incorrect result.
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

    // check for the typo'd byte
    return (pifCheck == 0xB0) ? EMU_PROJECT64_1_OR_2 : EMU_PROJECT64_3;
}

static u8 check_cache_emulation() {
    // Disable interrupts to ensure that nothing evicts the variable from cache while we're using it.
    u32 saved = __osDisableInt();
    // Create a variable with an initial value of 1. This value will remain cached.
    volatile u8 sCachedValue = 1;
    // Overwrite the variable directly in RDRAM without going through cache.
    // This should preserve its value of 1 in dcache if dcache is emulated correctly.
    *(u8*)(K0_TO_K1(&sCachedValue)) = 0;
    // Read the variable back from dcache, if it's still 1 then cache is emulated correctly.
    // If it's zero, then dcache is not emulated correctly.
    const u8 cacheEmulated = sCachedValue;
    // Restore interrupts
    __osRestoreInt(saved);
    return cacheEmulated;
}

void detect_emulator() {
    if ((u32)IO_READ(DPC_PIPEBUSY_REG) | (u32)IO_READ(DPC_TMEM_REG) | (u32)IO_READ(DPC_BUFBUSY_REG)) {
        gEmulator = emux_detect() ? EMU_ARES : EMU_CONSOLE;
        return;
    }
    
    /*
     * This check forces RZ bug on vc
     * If console is N64/adequate Emu, the current rounding mode, which is initialized to round-to-nearest (RN), is used
     * If console is VC round-to-zero (RZ) mode is always used
     *
     * The double value 0.9999999999999999 used is 0x3FEFFFFFFFFFFFFF in binary
     * Exponent=01111111110, Mantissa=1111111111111111111111111111111111111111111111111111
     * RZ will output not 1.0f, RN will output exactly 1.0f
    */
    const FloatRoundingMode roundingMode = fcr_get_rounding_mode();
    fcr_set_rounding_mode(FRC_RM_ROUND_TO_NEAREST);
    if (1.0f != round_double_to_float(0.9999999999999999)) {
        gEmulator = EMU_WIIVC;
        fcr_set_rounding_mode(roundingMode);
        return;
    }
    fcr_set_rounding_mode(roundingMode);

    // Perform a read from unmapped PIF ram.
    // On console and well behaved emulators, this echos back the lower half of
    // the requested memory address, repeating it if a whole word is requested.
    // So in this case, it should result in 0x01040104
    u32 magic;
    osPiReadIo(0x1fd00104u, &magic);
    if (magic == 0u) {
        // Older versions of mupen (and pre-2.12 ParallelN64) just always read 0
        gEmulator = EMU_MUPEN_OLD;
        return;
    } else if (magic != 0x01040104u) {
        // cen64 does... something. The result is consistent, but not what it should be
        gEmulator = EMU_CEN64;
        return;
    }
    
    __osPiGetAccess();
    while (IO_READ(PI_STATUS_REG) & (PI_STATUS_DMA_BUSY|PI_STATUS_IO_BUSY));
    const u16 halfMagic = *((volatile u16*)0xbfd00106u);
    __osPiRelAccess();

    // Now do a halfword read instead.
    switch (halfMagic) {
        // This is the correct result (echo back the lower half of the requested address)
        case 0x0106: {
            // Test to see if the libpl emulator extension is present.
            osPiWriteIo(0x1ffb0000u, 0u);
            osPiReadIo(0x1ffb0000u, &magic);
            if (magic == 0x00500000u) {
                // libpl is supported. Must be ParallelN64
                gEmulator = EMU_PARALLELN64;
                return;
            }
            
            // If the cache is emulated, it's Ares
            if (check_cache_emulation()) {
                gEmulator = EMU_ARES;
                return;
            }

            // its the Project64 4.0 interpreter core
            gEmulator = EMU_PROJECT64_4;
            return;
        }
        // This looks like it should be the expected result considering what we got when we
        // requested the whole word, but that's actually wrong. Later versions of mupen
        // (and the Simple64 fork of it) get this wrong.
        case 0x0104:
            gEmulator = check_cache_emulation() ? EMU_SIMPLE64 : EMU_MUPEN64PLUS_NEXT;
            return;
        // If reading a word gives the correct response, but reading a halfword always gives 0,
        // then we are dealing with some version of Project 64. Call into this helper function
        // to find out which version we're dealing with.
        case 0x0000:
            gEmulator = get_pj64_version();
            return;
        // No known emulator gives any other value. If we somehow manage to get here, just return 0
        default:
            gEmulator = 0;
            return;
    }
} 
