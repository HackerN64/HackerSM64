#include "macros.inc"
.set UCODE_SIZE, 0x800

.section .text


/* Audio Bins */

/*
 * "What the heck is this?"
 * Shindou is the only version to actually edit the audio microcode, so I had to do this sorry lol
 */

.balign 16
glabel aspMainTextStart
#if VERSION_SH == 1
    .incbin "rsp/audio.bin"
#else
    .incbin "lib/PR/audio/aspMain.bin"
#endif
glabel aspMainTextEnd

/* DATA SECTION START */

.section .data

/* Audio Data */

.balign 16
glabel aspMainDataStart
#if VERSION_SH == 1
    .incbin "rsp/audio_data.bin"
#else
    .incbin "lib/PR/audio/aspMain_data.bin"
#endif
glabel aspMainDataEnd
