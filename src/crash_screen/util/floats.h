#pragma once

#include <ultra64.h>

#include "types.h"



enum FloatErrorType {
    FLT_ERR_NONE,
    FLT_ERR_DENORM,
    FLT_ERR_NAN,
    NUM_FLT_ERR,
};


enum FloatErrorType validate_f32(IEEE754_f32 val);
enum FloatErrorType validate_f64(IEEE754_f64 val);

ALWAYS_INLINE static f32 f32_from_word(Word x) {
    return ((IEEE754_f32){ .asU32 = x, }).asF32;
}

ALWAYS_INLINE static Word word_from_f32(f32 x) {
    return ((IEEE754_f32){ .asF32 = x, }).asU32;
}

ALWAYS_INLINE static f64 f64_from_doubleword(Doubleword x) {
    return ((IEEE754_f64){ .asU64 = x, }).asF64;
}

ALWAYS_INLINE static Doubleword doubleword_from_f64(f64 x) {
    return ((IEEE754_f64){ .asF64 = x, }).asU64;
}
