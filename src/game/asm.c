#include <ultra64.h>

#include "config.h"
#include "types.h"
#include "asm.h"

#include "crash_screen/util/insn_disasm.h"


// Returns the address of its own call instruction.
NEVER_INLINE uintptr_t _asm_getaddr(void) {
    uintptr_t RAddr;
    ASM_GET_REG_CPU(RAddr, "$ra"); // Get $ra.
    RAddr -= (2 * sizeof(uintptr_t));
    return RAddr;
}

// Replaces its own call instruction with a custom MIPS assembly instruction.
NEVER_INLINE void _asm_setbits(uintptr_t bits) {
    uintptr_t RAddr;
    ASM_GET_REG_CPU(RAddr, "$ra"); // Get $ra.
    RAddr -= (2 * sizeof(uintptr_t));
    *((uintptr_t*)RAddr + 0) = bits;
    *((uintptr_t*)RAddr + 1) = OPS_NOP;
    asm volatile("jr %0":"=r"(RAddr):);
}
