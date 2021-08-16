.include "macros.inc"

.section .data

glabel gSoundDataADSR
.incbin "sound/sound_data.ctl"
.balign 16

glabel gSoundDataRaw
.incbin "sound/sound_data.tbl"
.balign 16

glabel gMusicData
.incbin "sound/sequences.bin"
.balign 16

#ifndef VERSION_SH
glabel gBankSetsData
.incbin "sound/bank_sets"
.balign 16
#endif