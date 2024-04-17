#pragma once

#include "config.h"
#include "types.h"


// Inline asm macros:
#define ASM_SET_REG_CPU(dst, src) asm volatile("move "dst",%0"::"r"(src))
#define ASM_GET_REG_CPU(dst, src) asm volatile("move %0,"src:"=r"(dst):)

#define ASM_SET_REG_COP0(dst, src) asm volatile("mtc0 "dst",%0"::"r"(src))
#define ASM_GET_REG_COP0(dst, src) asm volatile("mfc0 %0,"src:"=r"(dst):)

#define ASM_SET_REG_COP1(dst, src) asm volatile("mtc1 "dst",%0"::"r"(src))
#define ASM_GET_REG_COP1(dst, src) asm volatile("mfc1 %0,"src:"=r"(dst):)

#define ASM_SET_REG_FCR(dst, src) asm volatile("ctc1 "dst",%0"::"r"(src))
#define ASM_GET_REG_FCR(dst, src) asm volatile("cfc1 %0,"src:"=r"(dst):)


uintptr_t _asm_getaddr(void);
void _asm_setbits(uintptr_t bits);


// Macros to trigger specific exceptions:
#define EXCEPTION_INT()     //! TODO: do { ; } while (0) // Interrupt.
#define EXCEPTION_MOD()     //! TODO: do { ; } while (0) // TLB modification exception.
#define EXCEPTION_RMISS()   do { *(vs8*)0 = *(vs8*)0;                                                   } while (0) // TLB exception on load or instruction fetch.
#define EXCEPTION_WMISS()   do { *(vs8*)0 =        0;                                                   } while (0) // TLB exception on store".
#define EXCEPTION_RADE()    do { vs8 __x; *(uintptr_t*)(__x + 1) = *(uintptr_t*)(__x + 1);              } while (0) // Address error on load or instruction fetch.
#define EXCEPTION_WADE()    do { vs8 __x; *(uintptr_t*)(__x + 1) =                      0;              } while (0) // Address error on store.
#define EXCEPTION_IBE()     //! TODO: do { ; } while (0) // Bus error on instruction fetch.
#define EXCEPTION_DBE()     //! TODO: do { ; } while (0) // Bus error on data.
#define EXCEPTION_SYSCALL() do { asm volatile("syscall");                                               } while (0) // System call exception.
#define EXCEPTION_BREAK()   do { asm volatile("break");                                                 } while (0) // Breakpoint exception.
#define EXCEPTION_II()      do { _asm_setbits(0x00000001);                                              } while (0) // Reserved instruction exception.
#define EXCEPTION_CPU()     //! TODO: do { ; } while (0) // Coprocessor unusable exception.
#define EXCEPTION_OV()      //! TODO: do { ; } while (0) // Arithmetic overflow exception.
#define EXCEPTION_TRAP()    do { __builtin_trap();                                                      } while (0) // Trap exception. 
#define EXCEPTION_VCEI()    //! TODO: do { ; } while (0) // Virtual coherency exception on intruction fetch.
#define EXCEPTION_FPE()     do { volatile IEEE754_f32 __x = { .asU32 = 0x00000001, } asm volatile("add.s %0,%1,%2":"=f"(__x):"f"(__x),"f"(__x)); } while (0) // Floating point exception (see fpcsr).
#define EXCEPTION_WATCH()   //! TODO: do { ; } while (0) // Watchpoint exception.
#define EXCEPTION_VCED()    //! TODO: do { ; } while (0) // Virtual coherency exception on data reference.
// Trigger specific floating-point exceptions:
#define EXCEPTION_CE()      do { vs32 __x; asm volatile("add.s %0,%1,%2":"=f"(__x):"f"(__x),"f"(__x));  } while (0) // Unimplemented operation.
#define EXCEPTION_CV()      //! TODO: do { ; } while (0) // Invalid operation.
#define EXCEPTION_CZ()      //! TODO: do { ; } while (0) // Division by zero.
#define EXCEPTION_CO()      //! TODO: do { ; } while (0) // Overflow.
#define EXCEPTION_CU()      //! TODO: do { ; } while (0) // Underflow.
#define EXCEPTION_CI()      //! TODO: do { ; } while (0) // Inexact operation.


// // f32 -> s32
// ALWAYS_INLINE s32 roundf(f32 in) {
//     f32 tmp;
//     s32 out;
//     __asm__("round.w.s %0,%1" : "=f" (tmp) : "f" (in ));
//     __asm__("mfc1      %0,%1" : "=r" (out) : "f" (tmp));
//     return out;
// }
// ALWAYS_INLINE s32 truncf(f32 in) {
//     f32 tmp;
//     s32 out;
//     __asm__("trunc.w.s %0,%1" : "=f" (tmp) : "f" (in ));
//     __asm__("mfc1      %0,%1" : "=r" (out) : "f" (tmp));
//     return out;
// }
// ALWAYS_INLINE s32 ceilf(f32 in) {
//     f32 tmp;
//     s32 out;
//     __asm__("ceil.w.s  %0,%1" : "=f" (tmp) : "f" (in ));
//     __asm__("mfc1      %0,%1" : "=r" (out) : "f" (tmp));
//     return out;
// }
// ALWAYS_INLINE s32 floorf(f32 in) {
//     f32 tmp;
//     s32 out;
//     __asm__("floor.w.s %0,%1" : "=f" (tmp) : "f" (in ));
//     __asm__("mfc1      %0,%1" : "=r" (out) : "f" (tmp));
//     return out;
// }

// // f64 -> s64
// ALWAYS_INLINE s64 roundd(f64 in) {
//     f64 tmp;
//     s64 out;
//     __asm__("round.l.d %0,%1" : "=f" (tmp) : "f" (in ));
//     __asm__("dmfc1     %0,%1" : "=r" (out) : "f" (tmp));
//     return out;
// }
// ALWAYS_INLINE s64 truncd(f64 in) {
//     f64 tmp;
//     s64 out;
//     __asm__("trunc.l.d %0,%1" : "=f" (tmp) : "f" (in ));
//     __asm__("dmfc1     %0,%1" : "=r" (out) : "f" (tmp));
//     return out;
// }
// ALWAYS_INLINE s64 ceild(f64 in) {
//     f64 tmp;
//     s64 out;
//     __asm__("ceil.l.d  %0,%1" : "=f" (tmp) : "f" (in ));
//     __asm__("dmfc1     %0,%1" : "=r" (out) : "f" (tmp));
//     return out;
// }
// ALWAYS_INLINE s64 floord(f64 in) {
//     f64 tmp;
//     s64 out;
//     __asm__("floor.l.d %0,%1" : "=f" (tmp) : "f" (in ));
//     __asm__("dmfc1     %0,%1" : "=r" (out) : "f" (tmp));
//     return out;
// }
