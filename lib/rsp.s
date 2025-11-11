#include "macros.inc"
#include "config.h"

// RSP TEXT SECTION

.section .text

.balign 16
glabel rspbootTextStart
.incbin "lib/PR/rspboot.bin"
.balign 16
glabel rspbootTextEnd

.balign 16
glabel aspMainTextStart
.incbin "lib/PR/aspMain.bin"
.balign 16
glabel aspMainTextEnd

#if F3DEX_VERSION == 3
    .balign 16
    glabel gspF3DEX3_BrWTextStart
    .incbin "lib/PR/f3dex3/gspF3DEX3_BrW.bin"
    glabel gspF3DEX3_BrWTextEnd
    #ifdef DEBUG_F3DEX3_PROFILER
        .balign 16
        glabel gspF3DEX3_BrW_PATextStart
        .incbin "lib/PR/f3dex3/gspF3DEX3_BrW_PA.bin"
        .balign 16
        glabel gspF3DEX3_BrW_PATextEnd

        .balign 16
        glabel gspF3DEX3_BrW_PBTextStart
        .incbin "lib/PR/f3dex3/gspF3DEX3_BrW_PB.bin"
        .balign 16
        glabel gspF3DEX3_BrW_PBTextEnd

        .balign 16
        glabel gspF3DEX3_BrW_PCTextStart
        .incbin "lib/PR/f3dex3/gspF3DEX3_BrW_PC.bin"
        .balign 16
        glabel gspF3DEX3_BrW_PCTextEnd
    #endif
#elif F3DEX_VERSION == 2
    .balign 16
    glabel gspF3DZEX2_NoN_fifoTextStart
    .incbin "lib/PR/f3dzex2/gspF3DZEX2.bin"
    .balign 16
    glabel gspF3DZEX2_NoN_fifoTextEnd
#else
    #error "Invalid F3DEX version."
#endif

#ifdef ENABLE_LINE_UCODE
    .balign 16
    glabel gspL3DZEX2_fifoTextStart
    .incbin "lib/f3dzex2/gspL3DZEX2.bin"
    .balign 16
    glabel gspL3DZEX2_fifoTextEnd
#endif

// RSP DATA SECTION

.section .data

.balign 16
glabel aspMainDataStart
.incbin "lib/PR/aspMain_data.bin"
.balign 16
glabel aspMainDataEnd


#if F3DEX_VERSION == 3
    .balign 16
    glabel gspF3DEX3_BrWDataStart
    .incbin "lib/PR/f3dex3/gspF3DEX3_BrW_data.bin"
    .balign 16
    glabel gspF3DEX3_BrWDataEnd
    #ifdef DEBUG_F3DEX3_PROFILER
        .balign 16
        glabel gspF3DEX3_BrW_PADataStart
        .incbin "lib/PR/f3dex3/gspF3DEX3_BrW_PA_data.bin"
        .balign 16
        glabel gspF3DEX3_BrW_PADataEnd

        .balign 16
        glabel gspF3DEX3_BrW_PBDataStart
        .incbin "lib/PR/f3dex3/gspF3DEX3_BrW_PB_data.bin"
        .balign 16
        glabel gspF3DEX3_BrW_PBDataEnd

        .balign 16
        glabel gspF3DEX3_BrW_PCDataStart
        .incbin "lib/PR/f3dex3/gspF3DEX3_BrW_PC_data.bin"
        .balign 16
        glabel gspF3DEX3_BrW_PCDataEnd
    #endif
#elif F3DEX_VERSION == 2
    .balign 16
    glabel gspF3DZEX2_NoN_fifoDataStart
    .incbin "lib/PR/f3dzex2/gspF3DZEX2_data.bin"
    .balign 16
    glabel gspF3DZEX2_NoN_fifoDataEnd
#else
    #error "Invalid F3DEX version."
#endif

#ifdef ENABLE_LINE_UCODE
    .balign 16
    glabel gspL3DZEX2_fifoDataStart
    .incbin "lib/f3dzex2/gspL3DZEX2_data.bin"
    .balign 16
    glabel gspL3DZEX2_fifoDataEnd
#endif
