#pragma once

#include "config.h"
#include "types.h"


// Inline asm macros:
#define ASM_SET_REG(dst, src) asm volatile("move "dst",%0"::"r"(src))
#define ASM_GET_REG(dst, src) asm volatile("move %0,"src:"=r"(dst):)

#define ASM_GET_RA(RAddr) ASM_GET_REG(RAddr, "$31")


uintptr_t _asm_getaddr(void);
void _asm_setbits(uintptr_t bits);


// Macros to trigger specific exceptions:
#define EXCEPTION_INT()     //! TODO: do { ; } while (0) // Interrupt.
#define EXCEPTION_MOD()     //! TODO: do { ; } while (0) // TLB modification exception.
#define EXCEPTION_RMISS()   do { *(vs8*)0 = *(vs8*)0;                                           } while (0) // TLB exception on load or instruction fetch.
#define EXCEPTION_WMISS()   do { *(vs8*)0 =        0;                                           } while (0) // TLB exception on store".
#define EXCEPTION_RADE()    do { vs8 x; *(uintptr_t*)(x + 1) = *(uintptr_t*)(x + 1);            } while (0) // Address error on load or instruction fetch.
#define EXCEPTION_WADE()    do { vs8 x; *(uintptr_t*)(x + 1) =                    0;            } while (0) // Address error on store.
#define EXCEPTION_IBE()     //! TODO: do { ; } while (0) // Bus error on instruction fetch.
#define EXCEPTION_DBE()     //! TODO: do { ; } while (0) // Bus error on data.
#define EXCEPTION_SYSCALL() do { asm volatile("syscall");                                       } while (0) // System call exception.
#define EXCEPTION_BREAK()   do { asm volatile("break");                                         } while (0) // Breakpoint exception.
#define EXCEPTION_II()      do { _asm_setbits(0x00000001);                                      } while (0) // Reserved instruction exception.
#define EXCEPTION_CPU()     //! TODO: do { ; } while (0) // Coprocessor unusable exception.
#define EXCEPTION_OV()      //! TODO: do { ; } while (0) // Arithmetic overflow exception.
#define EXCEPTION_TRAP()    do { __builtin_trap();                                              } while (0) // Trap exception. 
#define EXCEPTION_VCEI()    //! TODO: do { ; } while (0) // Virtual coherency exception on intruction fetch.
#define EXCEPTION_FPE()     //! TODO: do { ; } while (0) // Floating point exception (see fpcsr).
#define EXCEPTION_WATCH()   //! TODO: do { ; } while (0) // Watchpoint exception.
#define EXCEPTION_VCED()    //! TODO: do { ; } while (0) // Virtual coherency exception on data reference.
// Trigger specific floating-point exceptions:
#define EXCEPTION_CE()      do { vs32 x; asm volatile("add.s %0,%1,%2":"=f"(x):"f"(x),"f"(x));  } while (0) // Unimplemented operation.
#define EXCEPTION_CV()      //! TODO: do { ; } while (0) // Invalid operation.
#define EXCEPTION_CZ()      //! TODO: do { ; } while (0) // Division by zero.
#define EXCEPTION_CO()      //! TODO: do { ; } while (0) // Overflow.
#define EXCEPTION_CU()      //! TODO: do { ; } while (0) // Underflow.
#define EXCEPTION_CI()      //! TODO: do { ; } while (0) // Inexact operation.
