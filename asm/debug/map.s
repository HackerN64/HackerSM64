.include "macros.inc"
.section .data
.balign 16
glabel gMapSymbols
.incbin "bin/addr.bin"
glabel gMapSymbolsEnd

.balign 16
glabel gMapStrings
.incbin "bin/name.bin"
glabel gMapStringsEnd
