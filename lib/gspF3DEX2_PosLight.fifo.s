#include "macros.inc"
.set UCODE_SIZE, 0x800

.section .text

.balign 16
glabel gspF3DEX2_PosLight_fifoTextStart
    .incbin "lib/PR/f3dex2pl/fifo/F3DEX2_PosLight.bin"
glabel gspF3DEX2_PosLight_fifoTextEnd

/* DATA SECTION START */

.section .data

.balign 16
glabel gspF3DEX2_PosLight_fifoDataStart
    .incbin "lib/PR/f3dex2pl/fifo/F3DEX2_PosLight_data.bin"
glabel gspF3DEX2_PosLight_fifoDataEnd
