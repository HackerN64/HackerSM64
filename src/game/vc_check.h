#ifndef VC_CHECK_H
#define VC_CHECK_H

#include "sm64.h"

extern u8 gIsVC;

// This function must not be inlined by the compiler so I move it to a different C file
f32 round_double_to_float(f64);

/*
 * This check forces RTZ bug on vc
 * If console is N64/adequate Emu round-to-nearest (RTN) rounding mode is used
 * If console is VC round-to-zero (RTZ) mode is used
 *
 * The double value 0.9999999999999999 used is 0x3FEFFFFFFFFFFFFF in binary
 * Exponent=01111111110, Mantissa=1111111111111111111111111111111111111111111111111111
 * RTZ will output not 1.0f, RTN will output exactly 1.0f
 */
#define IS_VC() (1.0f != round_double_to_float(0.9999999999999999))

#endif
