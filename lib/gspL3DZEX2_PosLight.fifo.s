#include "macros.inc"
.set UCODE_SIZE, 0x800

.section .text

.balign 16
glabel gspL3DZEX2_PosLight_fifoTextStart
    .incbin "lib/PR/f3dzex/fifo/L3DZEX.bin"
glabel gspL3DZEX2_PosLight_fifoTextEnd

/* DATA SECTION START */

.section .data

.balign 16
glabel gspL3DZEX2_PosLight_fifoDataStart
    .incbin "lib/PR/f3dzex/fifo/L3DZEX_data.bin"
glabel gspL3DZEX2_PosLight_fifoDataEnd
