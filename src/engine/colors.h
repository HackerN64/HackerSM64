#pragma once

#include <PR/ultratypes.h>
#include <PR/gbi.h>

#include "types.h"
#include "math_util.h"

#include "color_presets.h"

// Direct format conversion macros:

#define COLORRGB_TO_COLORRGBF(  dst, src) vec3_scale_dest((dst), (src), 1/255.0f)
#define COLORRGBF_TO_COLORRGB(  dst, src) vec3_scale_dest((dst), (src), 255.0f)
#define COLORRGBA_TO_COLORRGBAF(dst, src) vec4_scale_dest((dst), (src), 1/255.0f)
#define COLORRGBAF_TO_COLORRGBA(dst, src) vec4_scale_dest((dst), (src), 255.0f)

#define colorRGB_set(    dst, r, g, b) vec3_set( (dst), (r), (g), (b))
#define colorRGB_copy(   dst, src    ) vec3_copy((dst), (src)        )
#define colorRGB_to_vec3(dst, src    ) vec3_copy((dst), (src)        )
#define vec3_to_colorRGB(dst, src    ) vec3_copy((dst), (src)        )

#define COPY_RGBA32_TO_COLORRGB(dst, src) do {  \
    (dst)[0] = RGBA32_R(src);                   \
    (dst)[1] = RGBA32_G(src);                   \
    (dst)[2] = RGBA32_B(src);                   \
} while (0)
#define COPY_RGBA32_TO_COLORRGBA(dst, src) do { \
    (dst)[0] = RGBA32_R(src);                   \
    (dst)[1] = RGBA32_G(src);                   \
    (dst)[2] = RGBA32_B(src);                   \
    (dst)[3] = RGBA32_A(src);                   \
} while (0)

RGBA16 rgba16_blend(RGBA16 a, RGBA16 b, Alpha fac);

Bool32 colorRGBA_average_2(ColorRGBA dst, ColorRGBA c1, ColorRGBA c2);
Bool32 colorRGBA_average_3(ColorRGBA dst, ColorRGBA c1, ColorRGBA c2, ColorRGBA c3);

void colorRGB_add_hue(ColorRGB color, Color hueAdd, Color s);
