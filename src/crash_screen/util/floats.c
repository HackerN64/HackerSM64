#include <ultra64.h>

#include <string.h>
#include <float.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/cs_main.h"

#include "floats.h"



FloatError validate_f32(IEEE754_f32 val) {
    if (val.mantissa != 0) {
        if (val.exponent == 0x00) {
            return FLT_ERR_DENORM; // Denormalized value.
        } else if (val.exponent == 0xFF) {
            return FLT_ERR_NAN; // NaN value.
        }
    }

    return FLT_ERR_NONE;
}

FloatError validate_f64(IEEE754_f64 val) {
    if (val.mantissa != 0) {
        if (val.exponent == 0x00) {
            return FLT_ERR_DENORM; // Denormalized value.
        } else if (val.exponent == 0xFF) {
            return FLT_ERR_NAN; // NaN value.
        }
    }

    return FLT_ERR_NONE;
}
