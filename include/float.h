#ifndef GAME_FLOAT_H
#define GAME_FLOAT_H

#include "types.h"

typedef enum {
    FCR_RM_ROUND_TO_NEAREST = 0x0,
    FCR_RM_ROUND_TO_ZERO = 0x1,
    FCR_RM_ROUND_TO_POSITIVE_INFINITY = 0x2,
    FCR_RM_ROUND_TO_MINUS_INFINITY = 0x3
} FloatRoundingMode;

typedef enum {
    FCR_EXCEPTION_INEXACT_OPERATION = 0x1,
    FCR_EXCEPTION_UNDERFLOW = 0x2,
    FCR_EXCEPTION_OVERFLOW = 0x4,
    FCR_EXCEPTION_DIVISION_BY_ZERO = 0x8,
    FCR_EXCEPTION_INVALID_OPERATION = 0x10,
    FCR_EXCEPTION_UNIMPLEMENTED_OPERATION = 0x20 // only for Cause bits
} FloatExceptionBits;

extern FloatRoundingMode fcr_get_rounding_mode(void);
extern void fcr_set_rounding_mode(FloatRoundingMode);

// Get the Enable bits in FCR31
extern FloatExceptionBits fcr_get_enabled_exceptions(void);

// Set the Enable bits in FCR31 to the given bitset
// NOTE: Ares is currently the only emulator to actually respect the Enable bits.
//       Floating point exceptions will still not occur on other emulators.
extern void fcr_set_enabled_exceptions(FloatExceptionBits);

// Set the Enable bits to 1 for the given exception(s). Do not change any other exceptions.
// NOTE: Ares is currently the only emulator to actually respect the Enable bits.
//       Floating point exceptions will still not occur on other emulators.
extern void fcr_enable_exceptions(FloatExceptionBits);

// Set the Enable bits to 0 for the given exception(s). Do not change any other exceptions.
extern void fcr_disable_exceptions(FloatExceptionBits);

// Returns TRUE if all of the given exceptions are enabled
inline Bool32 fcr_check_exceptions_enabled(FloatExceptionBits exceptions) {
    return (fcr_get_enabled_exceptions() & exceptions) == exceptions;
}

// Gets the exception(s) raised by the most recently executed floating point instruction
extern FloatExceptionBits fcr_get_cause(void);

// Gets the current exception flags. These flags are set when an exception would be raised,
// but the exception is not currently enabled. They are only cleared by calling fcr_clear_flags.
// NOTE: This currently will only work on console and Ares. Other emulators will always return 0
extern FloatExceptionBits fcr_get_flags(void);

// Clear the current exception flags
extern void fcr_clear_flags(void);

#endif

