#include "macros.inc"
.set UCODE_SIZE, 0x800

.section .text

.balign 16
glabel gspF3DZEX2_NoN_PosLight_fifoTextStart
    .incbin "lib/PR/f3dzex/fifo/F3DZEX_NoN.bin"
glabel gspF3DZEX2_NoN_PosLight_fifoTextEnd

/* DATA SECTION START */

.section .data

.balign 16
glabel gspF3DZEX2_NoN_PosLight_fifoDataStart
    .incbin "lib/PR/f3dzex/fifo/F3DZEX_NoN_data.bin"
glabel gspF3DZEX2_NoN_PosLight_fifoDataEnd
