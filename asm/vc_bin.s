// VC is using binary matching to stub functions - this hex was copied from SM64
// osEepromRead/Write are performing infinite cycle inside them which causes recompiler do go crazy
// _osEepStatus does not have any suspicious asm in it but it is just VC hacked anyways
//   If it is not replaced with a VC variant, EEP probe fails

.include "macros.inc"
.section .text

#if defined(EEP)
// 0x80329150 = 0xE4150
.balign 16
glabel osEepromReadVC
.incbin "bin/vc/osEepromReadVC.bin"

// 0x80328af0 = 0xE3AF0
.balign 16
glabel osEepromWriteVC
.incbin "bin/vc/osEepromWriteVC.bin"

.balign 16
glabel __osEepStatusVC
.incbin "bin/vc/osEepStatusVC.bin"
#endif
