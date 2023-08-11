#include <ultra64.h>
#include <PR/gs2dex.h>

#include "s2d_config.h"

#define s2d_text gspS2DEX2_fifoTextStart
#define s2d_data gspS2DEX2_fifoDataStart

#ifdef  L3DEX2_ALONE
    #define zex_text gspL3DEX2_fifoTextStart
    #define zex_data gspL3DEX2_fifoDataStart
#elif  F3DZEX_GBI_2
    #define zex_text gspF3DZEX2_PosLight_fifoTextStart
    #define zex_data gspF3DZEX2_PosLight_fifoDataStart
#elif  F3DZEX_NON_GBI_2
    #define zex_text gspF3DZEX2_NoN_PosLight_fifoTextStart
    #define zex_data gspF3DZEX2_NoN_PosLight_fifoDataStart
#elif   F3DEX2PL_GBI
    #define zex_text gspF3DEX2_PosLight_fifoTextStart
    #define zex_data gspF3DEX2_PosLight_fifoDataStart
#elif   F3DEX_GBI_2
    #define zex_text gspF3DEX2_fifoTextStart
    #define zex_data gspF3DEX2_fifoDataStart
#else
    #error S2DEX Text Engine only works with other EX2 series microcodes
#endif

extern void s2d_reset_defer_index();

extern void s2d_init(void);

extern void s2d_handle_deferred();

extern void s2d_stop(void);

