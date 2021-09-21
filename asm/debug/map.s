.include "macros.inc"
.section .data
.balign 16
glabel gMapEntries
.incbin "bin/addr.bin"
glabel gMapEntryEnd

.balign 16
glabel gMapStrings
.incbin "bin/name.bin"
glabel gMapStringsEnd

.balign 16
glabel gMapEntrySize
.word (gMapEntryEnd - gMapEntries) / 4
glabel gMapStringSize
.word (gMapStringsEnd - gMapStrings)
