#pragma once

#include <PR/ultratypes.h>
#include <PR/gbi.h>

#include "types.h"
#include "math_util.h"

// I4
#define SIZ_I4      0x4
#define MSK_I4      BITMASK(SIZ_I4)

// I8
#define SIZ_I8      0x8
#define MSK_I8      BITMASK(SIZ_I8)

// IA4
#define SIZ_IA4_I   0x3
#define MSK_IA4_I   BITMASK(SIZ_IA4_I)
#define IDX_IA4_I   ((SIZ_IA4_I * 0) + 1)
#define IA4_I(c)    (((c) >> IDX_IA4_I) & MSK_IA4_I)
#define I_IA4(c)    (((c) & MSK_IA4_I) << IDX_IA4_I)
#define SIZ_IA4_A   0x1
#define MSK_IA4_A   BITMASK(SIZ_IA4_A)
#define IDX_IA4_A   ((SIZ_IA4_A * 0) + 0)
#define IA4_A(c)    (((c) >> IDX_IA4_A) & MSK_IA4_A)
#define A_IA4(c)    (((c) & MSK_IA4_A) << IDX_IA4_A)

// IA8
#define SIZ_IA8_C   0x4
#define MSK_IA8_C   BITMASK(SIZ_IA8_C)
#define IDX_IA8_I   ((SIZ_IA8_C * 1) + 0)
#define IDX_IA8_A   ((SIZ_IA8_C * 0) + 0)
#define IA8_I(c)    (((c) >> IDX_IA8_I) & MSK_IA8_C)
#define IA8_A(c)    (((c) >> IDX_IA8_A) & MSK_IA8_C)
#define I_IA8(c)    (((c) & MSK_IA8_C) << IDX_IA8_I)
#define A_IA8(c)    (((c) & MSK_IA8_C) << IDX_IA8_A)

// IA16
#define SIZ_IA16_C  0x8
#define MSK_IA16_C  BITMASK(SIZ_IA16_C)
#define IDX_IA16_I  ((SIZ_IA16_C * 1) + 0)
#define IDX_IA16_A  ((SIZ_IA16_C * 0) + 0)
#define IA16_I(c)   (((c) >> IDX_IA16_I) & MSK_IA16_C)
#define IA16_A(c)   (((c) >> IDX_IA16_A) & MSK_IA16_C)
#define I_IA16(c)   (((c) & MSK_IA16_C) << IDX_IA16_I)
#define A_IA16(c)   (((c) & MSK_IA16_C) << IDX_IA16_A)

// RGBA16
#define SIZ_RGBA16_C 0x5
#define MSK_RGBA16_C BITMASK(SIZ_RGBA16_C)
#define IDX_RGBA16_R ((SIZ_RGBA16_C * 2) + 1)
#define IDX_RGBA16_G ((SIZ_RGBA16_C * 1) + 1)
#define IDX_RGBA16_B ((SIZ_RGBA16_C * 0) + 1)
#define RGBA16_R(c)  (((c) >> IDX_RGBA16_R) & MSK_RGBA16_C)
#define RGBA16_G(c)  (((c) >> IDX_RGBA16_G) & MSK_RGBA16_C)
#define RGBA16_B(c)  (((c) >> IDX_RGBA16_B) & MSK_RGBA16_C)
#define R_RGBA16(c)  (((c) & MSK_RGBA16_C) << IDX_RGBA16_R)
#define G_RGBA16(c)  (((c) & MSK_RGBA16_C) << IDX_RGBA16_G)
#define B_RGBA16(c)  (((c) & MSK_RGBA16_C) << IDX_RGBA16_B)
#define SIZ_RGBA16_A 0x1
#define MSK_RGBA16_A BITMASK(SIZ_RGBA16_A)
#define IDX_RGBA16_A ((SIZ_RGBA16_A * 0) + 0)
#define RGBA16_A(c)  (((c) >> IDX_RGBA16_A) & MSK_RGBA16_A)
#define A_RGBA16(c)  (((c) & MSK_RGBA16_A) << IDX_RGBA16_A)

// RGBA32
#define SIZ_RGBA32_C 0x8
#define MSK_RGBA32_C BITMASK(SIZ_RGBA32_C)
#define IDX_RGBA32_R ((SIZ_RGBA32_C * 3) + 0)
#define IDX_RGBA32_G ((SIZ_RGBA32_C * 2) + 0)
#define IDX_RGBA32_B ((SIZ_RGBA32_C * 1) + 0)
#define IDX_RGBA32_A ((SIZ_RGBA32_C * 0) + 0)
#define RGBA32_R(c)  (((c) >> IDX_RGBA32_R) & MSK_RGBA32_C)
#define RGBA32_G(c)  (((c) >> IDX_RGBA32_G) & MSK_RGBA32_C)
#define RGBA32_B(c)  (((c) >> IDX_RGBA32_B) & MSK_RGBA32_C)
#define RGBA32_A(c)  (((c) >> IDX_RGBA32_A) & MSK_RGBA32_C)
#define R_RGBA32(c)  (((c) & MSK_RGBA32_C) << IDX_RGBA32_R)
#define G_RGBA32(c)  (((c) & MSK_RGBA32_C) << IDX_RGBA32_G)
#define B_RGBA32(c)  (((c) & MSK_RGBA32_C) << IDX_RGBA32_B)
#define A_RGBA32(c)  (((c) & MSK_RGBA32_A) << IDX_RGBA32_A)
#define SIZ_RGBA32_A SIZ_RGBA32_C
#define MSK_RGBA32_A MSK_RGBA32_C

// CI4
#define SIZ_CI4 0x4
#define MSK_CI4 BITMASK(SIZ_CI4)

// CI8
#define SIZ_CI8 0x8
#define MSK_CI8 BITMASK(SIZ_CI8)

// Direct format conversions:
#define I4_TO_RGBA16_C(c)   (((c) << (SIZ_RGBA16_C - SIZ_I4)) & MSK_RGBA16_C)
#define I8_TO_RGBA16_C(c)   (((c) >> (SIZ_I8 - SIZ_RGBA16_C)) & MSK_RGBA16_C)

#define RGBA16_COMPOSITE(r, g, b, a) (R_RGBA16(r) | G_RGBA16(g) | B_RGBA16(b) | A_RGBA16(a))
#define RGBA16_COMPOSITE_GRAYSCALE(val, alpha) RGBA16_COMPOSITE((val), (val), (val), (alpha))

#define COMPOSITE_TO_COLOR(src, bitmask, index)     (((((src) >> (index)) & (bitmask)) * 255.0f) / (bitmask))
#define COLOR_TO_COMPOSITE(src, bitmask, index)     (((CompositeColor)(((src) * (bitmask)) / 255.0f) & (bitmask)) << (index))

#define COMPOSITE_TO_COLORF(src, bitmask, index)    ((ColorF)(((src) >> (index)) & (bitmask)) / (bitmask))
#define COLORF_TO_COMPOSITE(src, bitmask, index)    (((CompositeColor)((src) * (bitmask)) & (bitmask)) << (index))

#define COLORRGB_TO_COLORRGBF(  dst, src) vec3_quot_val((dst), (src), 255.0f)
#define COLORRGBF_TO_COLORRGB(  dst, src) vec3_prod_val((dst), (src), 255.0f)
#define COLORRGBA_TO_COLORRGBAF(dst, src) vec4_quot_val((dst), (src), 255.0f)
#define COLORRGBAF_TO_COLORRGBA(dst, src) vec4_prod_val((dst), (src), 255.0f)

#define colorRGB_set(    dst, r, g, b) vec3_set( (dst), (r), (g), (b))
#define colorRGB_copy(   dst, src    ) vec3_copy((dst), (src)        )
#define colorRGB_to_vec3(dst, src    ) vec3_copy((dst), (src)        )
#define vec3_to_colorRGB(dst, src    ) vec3_copy((dst), (src)        )

#define RGBA32_TO_COLORRGB(dst, src) {  \
    (dst)[0] = RGBA32_R(src);           \
    (dst)[1] = RGBA32_G(src);           \
    (dst)[2] = RGBA32_B(src);           \
}
#define RGBA32_TO_COLORRGBA(dst, src) { \
    (dst)[0] = RGBA32_R(src);           \
    (dst)[1] = RGBA32_G(src);           \
    (dst)[2] = RGBA32_B(src);           \
    (dst)[3] = RGBA32_A(src);           \
}

#define COLORRGB0_TO_RGBA32(src) (R_RGBA32((src)[0]) | G_RGBA32((src)[1]) | B_RGBA32((src)[2]))
#define COLORRGB1_TO_RGBA32(src) (R_RGBA32((src)[0]) | G_RGBA32((src)[1]) | B_RGBA32((src)[2]) | MSK_RGBA32_A))
#define COLORRGBA_TO_RGBA32(src) (R_RGBA32((src)[0]) | G_RGBA32((src)[1]) | B_RGBA32((src)[2]) | A_RGBA32((src)[3]))

#include "color_presets.h"

Bool32 colorRGBA_average_2(ColorRGBA dst, ColorRGBA c1, ColorRGBA c2);
Bool32 colorRGBA_average_3(ColorRGBA dst, ColorRGBA c1, ColorRGBA c2, ColorRGBA c3);

void colorRGB_add_hue(ColorRGB color, Color hueAdd, Color s);
